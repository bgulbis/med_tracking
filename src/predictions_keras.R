library(tidyverse) 
library(lubridate)
library(keras)

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
    mutate(event_day = floor_date(clinical_event_datetime, unit = "day")) %>%
    count(event_day) 

# keras ------------------------------------------------

nrml <- normalize(df$n)

gen <- timeseries_generator()

