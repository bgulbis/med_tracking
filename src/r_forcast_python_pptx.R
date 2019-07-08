library(tidyverse)
library(lubridate)
library(forecast)
library(timetk)
library(sweep)
library(prophet)

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
        dplyr::rename_all(stringr::str_to_lower) %>%
        dplyr::distinct()
}

prep_df <- function(df, ...) {
    cnt <- rlang::enquos(...)
    
    df %>%
        dplyr::select(
            event_datetime = tidyselect::matches(
                "clinical_event_datetime|order_datetime"
            ),
            facility = tidyselect::starts_with("facility")
        ) %>%
        dplyr::mutate(ds = lubridate::as_date(event_datetime)) %>%
        dplyr::filter(
            facility %in% campus,
            event_datetime < lubridate::rollback(
                lubridate::now("US/Central"), 
                TRUE, 
                FALSE
            )
        ) %>%
        dplyr::arrange(ds) %>%
        dplyr::count(ds, !!!cnt, name = "y")
}

fcast_arima <- function(ts, h = 12, lambda = "auto") {
    forecast::auto.arima(
        ts, 
        seasonal = FALSE,
        stepwise = FALSE,
        approximation = FALSE,
        lambda = lambda,
        biasadj = TRUE
    ) %>%
        forecast::forecast(h) %>%
        sweep::sw_sweep(timetk_idx = TRUE) 
}

run_forecast <- function(x, pattern, h) {
    fx <- function(df, h) {
        df %>%
            prep_df() %>%
            timetk::tk_ts(silent = TRUE) %>%
            fcast_arima(h) 
    }
    
    abx <- c(
        "ceftaroline", 
        "ceftazidime-avibactam", 
        "ceftolozane-tazobactam", 
        "DAPTOmycin", 
        "ertapenem",
        "meropenem-vaborbactam"
    )
    
    if (x %in% abx) {
        get_data("data/tidy/abx", pattern) %>%
            dplyr::filter(!!rlang::sym("med") == x) %>%
            fx(h)
    } else if (x %in% c("eculizumab_inpt", "eculizumab_outpt")) {
        get_data("data/tidy/eculizumab", pattern) %>%
            dplyr::filter(!!rlang::sym("encounter_type") == x) %>%
            fx(h)
    } else if (x == "pegfilgrastim") {
        get_data("data/tidy/eculizumab", pattern) %>%
            dplyr::filter(!!rlang::sym("encounter_type") == x) %>%
            fx(h)
        
    } else {
        get_data(paste0("data/tidy/", x), pattern) %>%
            fx(h)
    }
}

meds <- c(
    "acetaminophen" = "apap_events",
    "bupivacaine-liposome" = "bupivacaine-liposome_orders",
    "calcitonin" = "calcitonin_events",
    "eculizumab_inpt" = "eculizumab_events",
    "eculizumab_outpt" = "eculizumab_events",
    "ivig" = "ivig_events",
    "ceftaroline" = "antibiotics_events",
    "ceftazidime-avibactam" = "antibiotics_events",
    "ceftolozane-tazobactam" = "antibiotics_events",
    "DAPTOmycin" = "antibiotics_events",
    "ertapenem" = "antibiotics_events",
    "meropenem-vaborbactam" = "antibiotics_events"
)

l <- map2(names(meds), meds, run_forecast, h = 366)
names(l) <- names(meds)
openxlsx::write.xlsx(l, "data/final/forecast_daily.xlsx")

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

df_daily <- data_apap_events %>%
    prep_df()

ts_daily <- tk_ts(df_daily, silent = TRUE)
acf(ts_daily)
pacf(ts_daily)

fit <- auto.arima(
    ts_daily,
    seasonal = FALSE,
    stepwise = FALSE,
    approximation = FALSE,
    lambda = "auto",
    biasadj = TRUE
)
summary(fit)

f <- forecast(fit, h = 366)
plot(f)
s <- sw_sweep(f, timetk_idx = TRUE)

df_daily$cap <- max(df_daily$y) * 1.2
df_daily$floor <- min(df_daily$y) * 0.8

m <- prophet(df_daily, growth = "logistic")
future <- make_future_dataframe(m, periods = 366)
future$cap <- max(df_daily$y) * 1.2
future$floor <- min(df_daily$y) * 0.8
forecast <- predict(m, future)
plot(m, forecast)


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

pd <- import("pandas")

prs <- pptx$Presentation()
add_forecast_slide(prs, df, "acetaminophen")
add_forecast_slide(prs, ppt_ivig, "IVIG")
prs$save("doc/py_from_r.pptx")


