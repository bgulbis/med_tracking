library(tidyverse)
library(readxl)
library(lubridate)
library(mbohelpr)
library(openxlsx)

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

df_orders <- raw_heparin |> 
    distinct(encntr_id, orig_order_id, order_datetime, nurse_unit, order_from_mpp) |> 
    mutate(
        order_month = floor_date(order_datetime, unit = "month"),
        order_day = floor_date(order_datetime, unit = "day"),
        order_hour = floor_date(order_datetime, unit = "hour"),
        order_shift = if_else(hour(order_hour) >= 7 & hour(order_hour) < 19, "day", "night")
    )

data_orders_monthly <- df_orders |> 
    count(order_month, nurse_unit, order_shift, name = "num_orders") |> 
    filter(order_month >= mdy("07/01/2021")) |> 
    pivot_wider(names_from = order_month, values_from = num_orders)

data_orders_daily <- df_orders |> 
    count(order_day, nurse_unit, order_shift, name = "num_orders") |> 
    filter(order_day >= mdy("07/01/2021")) |> 
    pivot_wider(names_from = order_day, values_from = num_orders)

df_infusions <- raw_heparin |> 
    drip_runtime(orig_order_id, order_datetime) |> 
    filter(!is.na(rate)) |> 
    summarize_drips(orig_order_id, order_datetime)

df_infusions_shift <- df_infusions |> 
    mutate(
        order_month = floor_date(order_datetime, unit = "month"),
        order_day = floor_date(order_datetime, unit = "day"),
        order_hour = floor_date(order_datetime, unit = "hour"),
        order_shift = if_else(hour(order_hour) >= 7 & hour(order_hour) < 19, "day", "night")
    )

l <- list(
    "monthly" = data_orders_monthly,
    "daily" = data_orders_daily
)

write.xlsx(l, paste0(f, "final/heparin_drip_data.xlsx"), overwrite = TRUE)
