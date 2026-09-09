library(tidyverse)
library(readxl)
library(lubridate)
library(mbohelpr)
library(openxlsx)

options(scipen = 0)

f <- set_data_path("med_tracking", "sugammadex_inpt_sophie")

raw_pts <- read_excel(
    paste0(f, "raw/patients.xlsx"), 
    sheet = 1, 
    skip = 10, 
    col_names = c("start", "end", "mrn", "age", "sex", "weight", "bmi", "anesth_md", "anesth_all", "service", "surgeon", "surgery",
                  "pt_class_case", "or_location", "asa_score", "anesth_start_datetime", "anesth_end_datetime", "induction_datetime",
                  "intubation_datetime", "extubation_datetime", "recovery_in_datetime", "recovery_out_datetime")
) |> 
    select(-start, -end)

raw_meds <- read_excel(
    paste0(f, "raw/meds.xlsx"), 
    sheet = 1, 
    skip = 10, 
    col_names = c("start", "end", "mrn", "encounter_csn", "order_id", "med_datetime", "medication", "order", "dose", "dose_unit",
                  "route", "freq", "admin_by", "admin_by_role", "nurse_unit")
) |> 
    mutate(across(medication, str_to_lower)) |> 
    select(-start, -end)

zzz_meds <- distinct(raw_meds, medication) |> arrange(medication)

raw_tof <- read_excel(
    paste0(f, "raw/tof.xlsx"), 
    sheet = 1, 
    skip = 10, 
    col_names = c("start", "end", "mrn", "encounter_csn", "entry_date", "entry_time", "row_name", "tof", "nurse_unit")
) |> 
    mutate(entry_datetime = ymd_hms(paste(entry_date, entry_time))) |> 
    select(-start, -end, -entry_date, -entry_time)

df_pts <- raw_pts |> 
    arrange(mrn, anesth_start_datetime) 

df_meds <- raw_meds |> 
    arrange(mrn, encounter_csn, med_datetime)

df_meds_summary <- df_meds |> 
    summarize(
        num_doses = n(),
        first_dose = first(dose),
        total_dose = sum(dose),
        first_datetime = first(med_datetime),
        last_datetime = last(med_datetime),
        .by = c(mrn, encounter_csn, medication)
    ) |> 
    mutate(
        across(
            medication, \(x) case_when(
                x == "neostigmine methylsulfate" ~ "neostig",
                x == "rocuronium bromide" ~ "roc",
                x == "sugammadex sodium" ~ "sug"
            )
        )
    ) |> 
    pivot_wider(names_from = medication, values_from = c(num_doses:last_datetime)) |> 
    select(mrn, encounter_csn, num_doses_roc, first_dose_roc, total_dose_roc, first_datetime_roc, last_datetime_roc, 
           num_doses_sug, first_dose_sug, total_dose_sug, first_datetime_sug, last_datetime_sug,
           num_doses_neostig, first_dose_neostig, total_dose_neostig, first_datetime_neostig, last_datetime_neostig)

df_tof <- raw_tof |>  
    arrange(mrn, encounter_csn, entry_datetime) |> 
    distinct(mrn, encounter_csn) |> 
    mutate(tof_monitoring = TRUE)

df_reversal_times <- df_meds_summary |> 
    mutate(first_reversal_datetime = min(first_datetime_sug, first_datetime_neostig, na.rm = TRUE), .by = c(mrn, encounter_csn)) |> 
    select(mrn, encounter_csn, first_reversal_datetime)
    
df_tof_prior <- raw_tof |> 
    inner_join(df_reversal_times, by = c("mrn", "encounter_csn")) |> 
    filter(entry_datetime <= first_reversal_datetime) |> 
    arrange(mrn, encounter_csn, entry_datetime) |> 
    summarize(
        tof_before_reversal = last(tof),
        tof_datetime = last(entry_datetime),
        .by = c(mrn, encounter_csn)
    )

df_tof_last <- raw_tof |> 
    arrange(mrn, encounter_csn, entry_datetime) |> 
    summarize(
        last_tof = last(tof),
        last_tof_datetime = last(entry_datetime),
        .by = c(mrn, encounter_csn)
    )

data_patients <- df_pts |> 
    inner_join(df_meds_summary, by = "mrn", relationship = "many-to-many") |> 
    filter(first_datetime_roc >= anesth_start_datetime, first_datetime_roc < anesth_end_datetime) |> 
    select(mrn, encounter_csn, everything()) |> 
    left_join(df_tof, by = c("mrn", "encounter_csn")) |>
    left_join(df_tof_prior, by = c("mrn", "encounter_csn")) |> 
    left_join(df_tof_last, by = c("mrn", "encounter_csn"))

l <- list(
    "data" = data_patients,
    "details" = df_meds
)

write.xlsx(l, paste0(f, "final/inpt_sugammadex_data.xlsx"), overwrite = TRUE)
