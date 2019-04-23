library(tidyverse)
library(lubridate)
library(officer)
library(themebg)
library(ggrepel)
library(rvg)
library(mschart)
library(forecast)
library(timetk)
library(sweep)

tz_locale <- locale(tz = "US/Central")
floor_unit <- "month"

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

make_forecast <- function(ts, h = 12) {
    forecast::auto.arima(ts) %>%
        forecast::forecast(h) %>%
        sweep::sw_sweep(timetk_idx = TRUE) %>%
        dplyr::mutate_at(
            "index",
            lubridate::floor_date, 
            unit = floor_unit
        ) %>%
        dplyr::mutate_at("key", stringr::str_to_title)
}

# acetaminophen ----------------------------------------

# albumin ----------------------------------------------

dir_data <- "../albumin/data/tidy/mbo"

data_albumin_events <- get_data(dir_data, "albumin_events")
data_albumin_orders <- get_data(dir_data, "albumin_orders")

# antibiotics ------------------------------------------

dir_data <- "data/tidy/abx"
data_abx_events <- get_data(dir_data, "antibiotics_events")
data_abx_orders <- get_data(dir_data, "antibiotics_orders")

# bupivacaine-liposome ---------------------------------

dir_data <- "data/tidy/bupivacaine-liposome"
data_bupiv_events <- get_data(dir_data, "bupivacaine-liposome_events")
data_bupiv_orders <- get_data(dir_data, "bupivacaine-liposome_orders")

# calcitonin -------------------------------------------

dir_data <- "data/tidy/calcitonin"
data_calcitonin_events <- get_data(dir_data, "calcitonin_events")
data_calcitonin_orders <- get_data(dir_data, "calcitonin_orders")

# pegfilgrastim ----------------------------------------

dir_data <- "data/tidy/pegfilgrastim"
data_pegfilgrastim_events <- get_data(dir_data, "pegfilgrastim_events")
data_pegfilgrastim_orders <- get_data(dir_data, "pegfilgrastim_orders")

ts_pegf <- data_pegfilgrastim_events %>%
    mutate(med_date = floor_date(clinical_event_datetime, unit = floor_unit)) %>%
    count(med_date) %>%
    tk_ts()

fcast_pegf <- make_forecast(ts_pegf)

g_fcast_pegf <- fcast_pegf %>%
    ggplot(aes(x = index, y = n)) +
    geom_ribbon(aes(ymin = lo.95, ymax = hi.95), fill = "grey85", alpha = 0.5) +
    geom_ribbon(aes(ymin = lo.80, ymax = hi.80), fill = "grey75", alpha = 0.5) +
    geom_line(aes(color = key), size = 2) +
    scale_color_manual(NULL, values = c("black", "blue")) +
    scale_x_datetime(NULL, date_breaks = "6 months", date_labels = "%b %Y") +
    ylab("Number of Doses") +
    # ggtitle(
    #     paste0(
    #         "Sugammadex usage predicted to increase by ", 
    #         chg$change, 
    #         "% over the next 12-months"
    #     )
    # ) +
    theme_bg() +
    theme(
        axis.text.x = element_text(vjust = 0.1),
        legend.position = "bottom"
    )
