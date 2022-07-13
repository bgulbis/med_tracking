library(tidyverse)
library(readxl)
library(lubridate)
library(mbohelpr)

f <- set_data_path("med_tracking")

raw_heparin <- read_excel(paste0(f, "raw/heparin_drips.xlsx")) |> 
    rename_all(str_to_lower)

df_heparin <- raw_heparin |> 
    mutate(
        med_month = floor_date(med_datetime, unit = "month"),
        med_day = floor_date(med_datetime, unit = "day")
    )

data_patient_monthly <- df_heparin |> 
    distinct(encntr_id, med_month, nurse_unit) |> 
    count(med_month, nurse_unit, name = "num_patients")

data_patient_daily <- df_heparin |> 
    distinct(encntr_id, med_day, nurse_unit) |> 
    count(med_day, nurse_unit, name = "num_patients")

data_bags_monthly <- df_heparin |> 
    filter(iv_event == "Begin Bag") |> 
    count(med_month, nurse_unit, name = "num_bags_started")

data_bags_daily <- df_heparin |> 
    filter(iv_event == "Begin Bag") |> 
    count(med_day, nurse_unit, name = "num_bags_started")
