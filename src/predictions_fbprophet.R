library(tidyverse) 
library(lubridate)
library(prophet)

tz_locale <- locale(tz = "US/Central")

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

data_apap <- get_data("data/tidy/acetaminophen", "apap_events") %>%
    distinct() 

df <- data_apap %>%
    mutate(ds = floor_date(clinical_event_datetime, unit = "day")) %>%
    count(ds) %>%
    # mutate_at("ds", as_date) %>%
    rename(y = n)

df$cap <- max(df$y) * 1.2
df$floor <- min(df$y) * 0.6

m <- prophet(df, growth = "logistic")
future <- make_future_dataframe(m, periods = 365)
future$cap <- max(df$y) * 1.2
future$floor <- min(df$y) * 0.6
forecast <- predict(m, future)
plot(m, forecast)

prophet_plot_components(m, forecast)

df_month <- data_apap %>%
    mutate(ds = floor_date(clinical_event_datetime, unit = "month")) %>%
    count(ds) %>%
    rename(y = n) %>%
    filter(ds < mdy("5/1/2019"))

m <- prophet(df_month)
future <- make_future_dataframe(m, periods = 12, freq = "month")
forecast <- predict(m, future)
plot(m, forecast)


