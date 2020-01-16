library(dplyr)
library(httr)
library(glue)
library(purrr)
library(readr)
library(stringr)
library(lubridate)

decompress <- function(x) {
    stopifnot(httr:::is.response(x))

    tmp <- tempfile()
    on.exit(unlink(tmp))

    writeBin(content(x), tmp)

    con <- file(tmp) # R automatically detects compression
    open(con, "rb")
    on.exit(close(con), TRUE)

    readBin(con, raw(), file.info(tmp)$size * 10)
}

# fxn to parse buoy FWF file format
# `rwstring`` should be the raw text file (as a string) downloaded from the NOAA
# servers
parse_buoy_data <- function(rwstring, na = c("", "NA")){
    splitcols <- rwstring %>%
        str_split("\\n") %>%
        .[[1]]

    colnames <- splitcols %>% .[1] %>%
        str_replace_all("#", "") %>%
        str_split(" +") %>%
        .[[1]]

    colwidths <- splitcols %>% .[3] %>%
        str_locate_all(" +") %>%
        .[[1]] %>% as_tibble() %>%
        mutate(width = start - lag(start, default = 1))

    cw <- colwidths$width
    if (length(cw) < length(colnames)){
        cw <- c(cw, NA)
    }

    out <- read_fwf(rwstring,
                    fwf_widths(cw, col_names = colnames),
                    comment = "#", na = na)
    modify_if(out, is.character, as.numeric)
}


get_buoy_data <- function(buoy_code, hist_years){
    hist_data <- hist_years %>%
        map_dfr(function(year){
            res <- GET(
                glue("https://www.ndbc.noaa.gov/view_text_file.php?filename={buoy_code}h{year}.txt.gz&dir=data/historical/stdmet/")
            )

            if(res$status_code == 200){
                parse_buoy_data(content(res))
            } else {
                return(NULL)
            }

        })

    jan_sept_19_data <- list(mnth = month.abb[1:9], mc = 1:9) %>%
        pmap_dfr(function(mnth, mc){
            "https://www.ndbc.noaa.gov/data/stdmet/{mnth}/{buoy_code}{mc}2019.txt.gz" %>%
                glue() %>%
                GET(add_headers(.headers = c('Accept-Encoding' = 'gzip, deflate'))) %>%
                decompress() %>% rawToChar() %>%
                parse_buoy_data()
        })

    oct19_data <- "https://www.ndbc.noaa.gov/view_text_file.php?filename={buoy_code}a2019.txt.gz&dir=data/stdmet/Oct/" %>%
        glue() %>% GET() %>% content() %>% parse_buoy_data()

    nov19_data <- "https://www.ndbc.noaa.gov/view_text_file.php?filename={buoy_code}b2019.txt.gz&dir=data/stdmet/Nov/" %>%
        glue() %>% GET() %>% content() %>% parse_buoy_data()

    current_data <- "https://www.ndbc.noaa.gov/data/realtime2/{buoy_code}.txt" %>%
        glue() %>% GET() %>% content() %>% parse_buoy_data(na = c("", "NA", "MM"))

    full_data <- bind_rows(hist_data, jan_sept_19_data, oct19_data, nov19_data,
                           current_data)

    # create date-time column
    full_data <- full_data %>%
        mutate(
            datetime = make_datetime(
                year = YY, month = MM, day = DD,
                hour = hh, min = mm
            )
        )

    # drop duplicates
    full_data <- full_data %>% distinct()

    # replace appropriate values with na
    full_data <- full_data %>%
        mutate_at(c("WSPD", "GST", "VIS", "TIDE", "DPD", "APD", "WVHT"), ~na_if(., 99)) %>%
        mutate_at(c("WDIR", "ATMP", "WTMP", "DEWP", "MWD"), ~na_if(., 999)) %>%
        mutate_at(c("PRES"), ~na_if(., 9999))

    summary(full_data)

    out <- full_data %>%
        select(-YY, -MM, -DD, -hh, -mm) %>%
        write_csv(glue("data/buoy/{buoy_code}.csv.gz"))
}

get_buoy_data(51101, hist_years = 2008:2018)
get_buoy_data(51001, hist_years = c(2008, 2009, 2015:2018))



