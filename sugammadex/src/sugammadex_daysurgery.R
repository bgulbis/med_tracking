library(tidyverse)
library(readxl)
library(lubridate)
library(mbohelpr)
library(openxlsx)

options(scipen = 0)

f <- set_data_path("med_tracking", "sugammadex")

raw_pts <- get_xlsx_data(
    paste0(f, "raw"), "sugammadex_daysurgery_patients.xlsx", 
    sheet = 1, 
    skip = 9, 
    col_names = c("start", "end", "case_id", "surgery_date", "procedure", "service", "location")
) |> 
    select(-start, -end)

raw_op_surgeries <- get_xlsx_data(
    paste0(f, "raw"), "op_surg.xlsx", 
    sheet = 1, 
    skip = 10, 
    col_names = c("start", "end", "mrn", "case_id", "surg_csn", "admit_csn", "surgery_date", 
                  "procedure", "service", "location", "recovery_start", "recovery_stop", "recovery_los")
) |> 
    select(-start, -end)

df_op_surg <- semi_join(raw_op_surgeries, raw_pts, by = "case_id")

raw_sug <- get_xlsx_data(
    paste0(f, "raw"), "sugammadex_doses.xlsx", 
    sheet = 1, 
    skip = 10, 
    col_names = c("start", "end", "mrn", "encounter_csn", "order_id", "dose", "medication", 
                  "dose_datetime", "dose_unit", "nurse_unit")
) |> 
    select(-start, -end)

df_sug <- semi_join(raw_sug, df_op_surg, by = "mrn") |> 
    arrange(mrn, encounter_csn, dose_datetime) |> 
    summarize(
        num_sug_doses = n(),
        first_sug_dose = first(dose),
        total_sug_dose = sum(dose),
        first_sug_datetime = first(dose_datetime),
        .by = c(mrn, encounter_csn)
    ) |> 
    mutate(first_sug_date = floor_date(first_sug_datetime, unit = "day"))

raw_meds <- get_xlsx_data(
    paste0(f, "raw"), "meds.xlsx", 
    sheet = 1, 
    skip = 10, 
    col_names = c("start", "end", "mrn", "encounter_csn", "order_id", "dose_datetime", "medication", 
                  "dose", "dose_unit", "nurse_unit")
) |> 
    select(-start, -end)

df_roc <- semi_join(raw_meds, df_op_surg, by = "mrn") |> 
    arrange(mrn, encounter_csn, dose_datetime) |> 
    filter(str_detect(medication, regex("rocuronium", ignore_case = TRUE))) |> 
    summarize(
        num_roc_doses = n(),
        first_roc_dose = first(dose),
        total_roc_dose = sum(dose),
        first_roc_datetime = first(dose_datetime),
        .by = c(mrn, encounter_csn)
    ) |> 
    mutate(first_roc_date = floor_date(first_roc_datetime, unit = "day"))

df_roc_last <- semi_join(raw_meds, df_op_surg, by = "mrn") |> 
    arrange(mrn, encounter_csn, dose_datetime) |> 
    filter(str_detect(medication, regex("rocuronium", ignore_case = TRUE))) |> 
    summarize(
        last_roc_datetime = last(dose_datetime),
        .by = c(mrn, encounter_csn)
    ) |> 
    mutate(last_roc_date = floor_date(last_roc_datetime, unit = "day"))

df_neo <- semi_join(raw_meds, df_op_surg, by = "mrn") |> 
    arrange(mrn, encounter_csn, dose_datetime) |> 
    filter(str_detect(medication, regex("neostig", ignore_case = TRUE))) |> 
    summarize(
        num_neostig_doses = n(),
        first_neostig_dose = first(dose),
        total_neostig_dose = sum(dose),
        first_neostig_datetime = first(dose_datetime),
        .by = c(mrn, encounter_csn)
    ) |> 
    mutate(first_neostig_date = floor_date(first_neostig_datetime, unit = "day"))

df_reversal_times <- df_roc_last |> 
    left_join(df_sug, by = c("mrn", "encounter_csn", "last_roc_date" = "first_sug_date")) |> 
    left_join(df_neo, by = c("mrn", "encounter_csn", "last_roc_date" = "first_neostig_date")) |> 
    mutate(
        first_reversal_datetime = min(first_sug_datetime, first_neostig_datetime, na.rm = TRUE),
        .by = c(mrn, encounter_csn)
    ) |> 
    select(mrn, encounter_csn, last_roc_datetime, first_reversal_datetime)
    
raw_tof <- get_xlsx_data(
    paste0(f, "raw"), "tof.xlsx", 
    sheet = 1, 
    skip = 10, 
    col_names = c("start", "end", "mrn", "encounter_csn", "entry_date", "entry_time", "name", "tof", "nurse_unit")
) |> 
    mutate(entry_datetime = ymd_hms(paste(entry_date, entry_time))) |> 
    select(-start, -end, -entry_date, -entry_time)

df_tof <- semi_join(raw_tof, df_op_surg, by = "mrn") |> 
    arrange(mrn, encounter_csn, entry_datetime) |> 
    distinct(mrn, encounter_csn) |> 
    mutate(tof_monitoring = TRUE)

df_tof_prior <- semi_join(raw_tof, df_op_surg, by = "mrn") |> 
    arrange(mrn, encounter_csn, entry_datetime) |> 
    inner_join(df_reversal_times, by = c("mrn", "encounter_csn")) |> 
    filter(
        # entry_datetime > last_roc_datetime,
        entry_datetime < first_reversal_datetime
    ) |> 
    summarize(
        tof_before_reversal = last(tof),
        tof_datetime = last(entry_datetime),
        .by = c(mrn, encounter_csn)
    )

df_tof_last <- semi_join(raw_tof, df_op_surg, by = "mrn") |> 
    arrange(mrn, encounter_csn, entry_datetime) |> 
    summarize(
        last_tof = last(tof),
        last_tof_datetime = last(entry_datetime),
        .by = c(mrn, encounter_csn)
    )

raw_anesth <- get_xlsx_data(
    paste0(f, "raw"), "anesthesia.xlsx", 
    sheet = 1, 
    skip = 10, 
    col_names = c("start", "end", "mrn", "age", "sex", "weight", "bmi", "anesth_date", "procedure", "asa_score", 
                 "intubation_datetime", "extubation_datetime")
) |> 
    select(-start, -end)

df_anesth <- semi_join(raw_anesth, df_op_surg, by = "mrn") |> 
    select(-procedure) |> 
    distinct(mrn, anesth_date, .keep_all = TRUE)

data_patients <- df_op_surg |> 
    left_join(df_anesth, by = c("mrn", "surgery_date" = "anesth_date")) |> 
    left_join(df_roc, by = c("mrn", "surgery_date" = "first_roc_date")) |> 
    left_join(df_sug, by = c("mrn", "encounter_csn", "surgery_date" = "first_sug_date")) |> 
    left_join(df_neo, by = c("mrn", "encounter_csn", "surgery_date" = "first_neostig_date")) |> 
    left_join(df_tof, by = c("mrn", "encounter_csn")) |> 
    left_join(df_tof_prior, by = c("mrn", "encounter_csn")) |> 
    select(-contains("_csn"))

write.xlsx(data_patients, paste0(f, "final/op_sugammadex_data.xlsx"), overwrite = TRUE)
