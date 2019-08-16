library(tidyverse)
library(lubridate)
library(mbohelpr)
library(openxlsx)

data_dir <- "data/tidy/teresa"

data_raw <- get_data(data_dir, "monthly")

data_cut <- max(data_raw$event_date) - years(2)

df_7j <- data_raw %>%
    filter(
        nurse_unit == "HH 7J",
        event_date >= data_cut
    ) %>%
    select(event_date, medication, doses, patients) %>%
    group_by(event_date, medication) %>%
    summarize_at(c("doses", "patients"), sum, na.rm = TRUE) %>%
    gather(key, value, doses, patients) %>%
    spread(event_date, value, fill = 0L)

df_strk <- data_raw %>%
    filter(
        nurse_unit == "HH STRK",
        event_date >= data_cut
    ) %>%
    select(event_date, medication, doses, patients) %>%
    group_by(event_date, medication) %>%
    summarize_at(c("doses", "patients"), sum, na.rm = TRUE) %>%
    gather(key, value, doses, patients) %>%
    spread(event_date, value, fill = 0L)

sheets <- list(
    "HH 7J" = df_7j,
    "HH STRK" = df_strk
)

write.xlsx(sheets, "report/teresa/med_utilization_wide.xlsx")

df_7j <- data_raw %>%
    filter(
        nurse_unit == "HH 7J",
        event_date >= data_cut
    ) %>%
    select(medication, event_date, doses, patients) %>%
    group_by(medication, event_date) %>%
    summarize_at(c("doses", "patients"), sum, na.rm = TRUE) %>%
    arrange(medication, event_date)

df_strk <- data_raw %>%
    filter(
        nurse_unit == "HH STRK",
        event_date >= data_cut
    ) %>%
    select(medication, event_date, doses, patients) %>%
    group_by(medication, event_date) %>%
    summarize_at(c("doses", "patients"), sum, na.rm = TRUE) %>%
    arrange(medication, event_date)

sheets <- list(
    "HH 7J" = df_7j,
    "HH STRK" = df_strk
)

write.xlsx(sheets, "report/teresa/med_utilization_long.xlsx")
