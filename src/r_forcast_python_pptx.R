library(mbohelpr)
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

prep_df <- function(df, unit = "day", ...) {
    cnt <- rlang::enquos(...)
    ds <- rlang::sym("ds")
    event_datetime <- rlang::sym("event_datetime")
    facility <- rlang::sym("facility")
    
    x <- df %>%
        dplyr::select(
            !!"event_datetime" := tidyselect::matches(
                "clinical_event_datetime|order_datetime"
            ),
            !!"facility" := tidyselect::starts_with("facility")
        ) %>%
        dplyr::mutate(!!"ds" := lubridate::as_date(!!event_datetime)) %>%
        dplyr::filter(
            !!facility %in% campus,
            !!event_datetime < lubridate::rollback(
                lubridate::now("US/Central"), 
                TRUE, 
                FALSE
            )
        ) %>%
        dplyr::arrange(!!ds)
    
    if (unit != "day") {
        x %>%
            mutate_at("ds", lubridate::floor_date, unit = unit) %>%
            dplyr::count(!!ds, !!!cnt, name = "y")
    } else {
        x %>%
            dplyr::count(!!ds, !!!cnt, name = "y")
    }
}

model_arima <- function(ts, lambda = "auto") {
    forecast::auto.arima(
        ts, 
        seasonal = FALSE,
        stepwise = FALSE,
        approximation = FALSE,
        lambda = lambda,
        biasadj = TRUE
    ) 
}

prep_pptx <- function(df) {
    index <- rlang::sym("index")
    key <- rlang::sym("key")
    y <- rlang::sym("y")
    
    df_ci <- df %>%
        dplyr::select(
            !!index,
            !!rlang::sym("hi.95"),
            !!rlang::sym("hi.80"),
            !!rlang::sym("lo.80"),
            !!rlang::sym("lo.95")
        )
        
    df %>%
        dplyr::select(!!index, !!key, !!y) %>%
        dplyr::mutate_at("key", stringr::str_to_sentence) %>%
        tidyr::spread(!!key, !!y) %>%
        dplyr::left_join(df_ci, by = "index")
}

run_forecast <- function(x, pattern, h, mod = "arima", unit = "month") {
    fx <- function(df, h) {
        if (mod == "arima") {
            
            df %>%
                prep_df(unit) %>%
                timetk::tk_ts(silent = TRUE) %>%
                model_arima() %>%
                forecast::forecast(h) %>%
                sweep::sw_sweep(timetk_idx = TRUE) %>%
                prep_pptx()
            
        } else if (mod == "prophet") {
            
            prep <- prep_df(df, unit = "day") 
            prep$cap <- max(prep$y) * 1.2
            prep$floor <- min(prep$y) * 0.8
            mod <- prophet(prep, growth = "logistic")
            future <- make_future_dataframe(mod, periods = h)
            future$cap <- max(prep$y) * 1.2
            future$floor <- min(prep$y) * 0.8
            predict(mod, future)
            
        }
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
        get_data(paste0("data/tidy/", x), pattern) %>%
            dplyr::filter(!!rlang::sym("encounter_type") == "Inpatient") %>%
            fx(h)
    } else if (x == "sugammadex") {
        get_data(paste0("data/tidy/", x), pattern) %>%
            dplyr::filter(!!rlang::sym("med") == x) %>%
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
    # "eculizumab_inpt" = "eculizumab_events",
    # "eculizumab_outpt" = "eculizumab_events",
    "isoproterenol" = "isoproterenol_events",
    "ivig" = "ivig_events",
    # "pegfilgrastim" = "pegfilgrastim_events",
    "sugammadex" = "sug-neo_events",
    "ceftaroline" = "antibiotics_events",
    "ceftazidime-avibactam" = "antibiotics_events",
    "ceftolozane-tazobactam" = "antibiotics_events",
    "DAPTOmycin" = "antibiotics_events",
    "ertapenem" = "antibiotics_events",
    "meropenem-vaborbactam" = "antibiotics_events"
)

l <- map2(names(meds), meds, run_forecast, h = 12, unit = "month")
names(l) <- names(meds)
openxlsx::write.xlsx(l, "data/final/forecast_monthly.xlsx")

# p <- map2(names(meds), meds, run_forecast, h = 366, mod = "prophet")
# names(p) <- names(meds)
# openxlsx::write.xlsx(p, "data/final/forecast_prophet.xlsx")


mod <- get_data("data/tidy/acetaminophen", "apap_events") %>%
    prep_df("month") %>%
    timetk::tk_ts(silent = TRUE) %>%
    model_arima() 

summary(mod)

f <- forecast(mod, h = 12) 
    # sw_sweep(timetk_idx = TRUE)

f$mean

# test -------------------------------------------------

df <- get_data("data/tidy", "acetaminophen-iv") %>%
    group_by(ds) %>%
    summarize_at("y", sum, na.rm = TRUE) %>%
    arrange(ds)

ts_df <- tk_ts(df, silent = TRUE)

mod <- auto.arima(
    ts_df, 
    # seasonal = FALSE,
    stepwise = FALSE,
    approximation = FALSE,
    lambda = "auto",
    biasadj = TRUE,
    # trace = TRUE,
    max.order = 10
) 
summary(mod)

f <- forecast(mod, h = 366)
plot(f)

m <- arima(ts_df, order = c(5, 1, 5))
summary(m)
f2 <- forecast(m, h = 366)
plot(f2)

abs(polyroot(c(1, -coef(m)[c("ar1", "ar2", "ar3", "ar4", "ar5")])))
abs(polyroot(c(1, -coef(m)[c("ma1", "ma2", "ma3", "ma4", "ma5")])))

df_month <- df %>%
    mutate(ds = floor_date(ds, unit = "months")) %>%
    group_by(ds) %>%
    summarize_at("y", sum, na.rm = TRUE)

ts_month <- tk_ts(df_month, silent = TRUE)

m_month <- auto.arima(
    ts_month, 
    stepwise = FALSE,
    approximation = FALSE,
    lambda = "auto",
    biasadj = TRUE,
    # trace = TRUE,
    max.order = 10
) 
summary(m_month)

f3 <- forecast(m_month, h = 12)

plot(f3)

# python -----------------------------------------------

library(reticulate)
use_condaenv("med_tracking")
pptx <- import("pptx")
source_python("src/pptx_slides.py")

prs <- pptx$Presentation()

walk2(l, names(l), function(x, y) add_forecast_slide(prs, x, y))

prs$save("doc/py_from_r.pptx")

pd <- import("pandas")
x <- l[[1]] %>% column_to_rownames("index")
df_pd <- pd$DataFrame(x)
add_forecast_slide(prs, df_pd, "apap")
