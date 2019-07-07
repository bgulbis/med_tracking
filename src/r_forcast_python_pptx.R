library(tidyverse)
library(lubridate)
library(forecast)
library(timetk)
library(sweep)

tz_locale <- locale(tz = "US/Central")
floor_unit <- "month"

campus <- c(
    "HC Childrens",
    "HH HERMANN",
    "HH Radiology",
    "HH Rehab",
    "HH Trans Care"
)

get_data <- function(path, pattern, col_types = NULL) {
    f <- list.files(path, pattern, full.names = TRUE)
    
    n <- f %>% 
        purrr::map_int(~ nrow(data.table::fread(.x, select = 1L))) 
    
    f[n > 0] %>%
        purrr::map_df(
            readr::read_csv,
            locale = tz_locale,
            col_types = col_types
        ) %>%
        rename_all(stringr::str_to_lower)
}

make_df <- function(df, ...) {
    cnt <- enquos(...)
    
    df %>%
        mutate(
            med_date = floor_date(clinical_event_datetime, unit = floor_unit),
            fiscal_year = year(med_date %m+% months(6)),
            month_plot = month(med_date, label = TRUE, abbr = TRUE)
        ) %>%
        count(fiscal_year, month_plot, med_date, !!!cnt) %>%
        mutate_at("fiscal_year", as_factor) %>%
        mutate_at(
            "month_plot", 
            factor, 
            ordered = TRUE, 
            levels = c(
                "Jul", 
                "Aug",
                "Sep",
                "Oct",
                "Nov",
                "Dec",
                "Jan",
                "Feb",
                "Mar",
                "Apr",
                "May",
                "Jun"
            )
        ) %>%
        arrange(med_date)
}

make_forecast <- function(ts, h = 12, lambda = "auto") {
    forecast::auto.arima(
        ts, 
        seasonal = FALSE,
        stepwise = FALSE,
        approximation = FALSE,
        lambda = lambda,
        biasadj = TRUE
    ) %>%
        forecast::forecast(h) %>%
        sweep::sw_sweep(timetk_idx = TRUE) %>%
        dplyr::mutate_at(
            "index",
            lubridate::floor_date, 
            unit = "day"
        ) %>%
        dplyr::mutate_at("key", stringr::str_to_title)
}

# acetaminophen ----------------------------------------
dir_data <- "data/tidy/acetaminophen"

data_apap_events <- get_data(dir_data, "apap_events") %>%
    distinct()
# data_apap_orders <- get_data(dir_data, "apap_orders")

df_apap <- data_apap_events %>%
    filter(
        facility_event %in% campus,
        # clinical_event_datetime >= mdy("4/1/2017"),
        clinical_event_datetime < rollback(now("US/Central"), TRUE, FALSE)
    ) %>%
    make_df()

ts_apap <- tk_ts(df_apap)
fcast_apap <- make_forecast(ts_apap)

ppt_apap <- fcast_apap %>%
    select(index, key, n) %>%
    spread(key, n) %>%
    mutate_at("index", floor_date, unit = "month") %>%
    mutate_at("Forecast", round, digits = 0)

# ivig -------------------------------------------------

dir_data <- "data/tidy/ivig"
data_ivig_events <- get_data(dir_data, "ivig_events")
# data_ivig_orders <- get_data(dir_data, "ivig_orders")

df_ivig <- data_ivig_events %>%
    filter(facility %in% campus) %>%
    make_df()

ts_ivig <- tk_ts(df_ivig)
fcast_ivig <- make_forecast(ts_ivig)

ppt_ivig <- fcast_ivig %>%
    select(index, key, n) %>%
    spread(key, n) %>%
    mutate_at("index", floor_date, unit = "month") %>%
    mutate_at("Forecast", round, digits = 0)


# python -----------------------------------------------

library(reticulate)
use_condaenv("med_tracking")
pptx <- import("pptx")
source_python("src/pptx_slides.py")

prs <- pptx$Presentation()
add_forecast_slide(prs, ppt_apap, "acetaminophen")
add_forecast_slide(prs, ppt_ivig, "IVIG")
prs$save("doc/py_from_r.pptx")


