library(dplyr)
library(httr)
library(readr)
library(lubridate)
library(glue)


get_snotel_data <- function(site_code){
    raw_df <- "https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customSingleStationReport/daily/{site_code}:UT:SNTL|id=%22%22|name/POR_BEGIN,POR_END/WTEQ::value,PREC::value,TMAX::value,TMIN::value,TAVG::value,PRCP::value" %>%
        glue() %>%
        GET() %>% content() %>%
        read_csv(comment = "#", col_types = "Ddddddd")


    clean_df <- raw_df %>%
        mutate(Date = as_datetime(Date)) %>%
        rename(
            datetime = Date,
            SWE = "Snow Water Equivalent (in) Start of Day Values",
            ACC = "Precipitation Accumulation (in) Start of Day Values",
            TMAX = "Air Temperature Maximum (degF)",
            TMIN = "Air Temperature Minimum (degF)",
            TMEAN = "Air Temperature Average (degF)",
            PINC = "Precipitation Increment (in)"
        ) %>%
        write_csv(glue("data/snotel/{site_code}.csv.gz"))
}

get_snotel_data(366)
get_snotel_data(766)


