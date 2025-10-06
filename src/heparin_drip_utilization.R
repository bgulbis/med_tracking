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

mpp <- c(
    "Heparin Weight Based Orders Deep Vein Thrombosis Pulmonary Embolism MPP",
    "Heparin Weight Based Orders for Acute Coronary Syndromes MPP",
    "Heparin Weight Based Atrial Fibrillation and Stroke Prevention Orders MPP"
)

df_hep_mpp <- raw_heparin |> 
    filter(order_from_mpp %in% mpp) |> 
    mutate(
        med_month = floor_date(med_datetime, unit = "month"),
        med_day = floor_date(med_datetime, unit = "day"),
        med_shift = if_else(hour(med_datetime) >= 7 & hour(med_datetime) < 19, "day", "night")
    )

df_hep_pts_monthly <- df_hep_mpp |> 
    distinct(encntr_id, nurse_unit, med_month) |> 
    count(med_month, nurse_unit, name = "num_pts") |> 
    filter(med_month >= mdy("07/01/2021")) |> 
    pivot_wider(names_from = med_month, values_from = num_pts)

df_hep_pts_monthly_shift <- df_hep_mpp |> 
    distinct(encntr_id, nurse_unit, med_month, med_shift) |> 
    count(med_month, nurse_unit, med_shift, name = "num_pts") |> 
    filter(med_month >= mdy("07/01/2021")) |> 
    pivot_wider(names_from = med_month, values_from = num_pts)

df_hep_pts_daily <- df_hep_mpp |> 
    distinct(encntr_id, nurse_unit, med_day) |> 
    count(med_day, nurse_unit, name = "num_pts") |> 
    filter(med_day >= mdy("07/01/2021")) |> 
    pivot_wider(names_from = med_day, values_from = num_pts)

df_hep_pts_daily_shift <- df_hep_mpp |> 
    distinct(encntr_id, nurse_unit, med_day, med_shift) |> 
    count(med_day, nurse_unit, med_shift, name = "num_pts") |> 
    filter(med_day >= mdy("07/01/2021")) |> 
    pivot_wider(names_from = med_day, values_from = num_pts)

df_hep_pts_daily_avg <- df_hep_mpp |> 
    distinct(encntr_id, nurse_unit, med_day, med_month) |> 
    count(med_day, med_month, nurse_unit, name = "num_pts") |> 
    filter(med_day >= mdy("07/01/2021")) |> 
    group_by(med_month, nurse_unit) |> 
    summarize(across(num_pts, mean, na.rm = TRUE), .groups = "drop") |> 
    mutate(across(num_pts, round, digits = 1)) |> 
    pivot_wider(names_from = med_month, values_from = num_pts)

# l2 <- list(
#     "daily_avg" = df_hep_pts_daily_avg,
#     "daily_shift" = df_hep_pts_daily_shift
# )

write.xlsx(df_hep_pts_daily_avg, paste0(f, "final/heparin_pts_daily_avg.xlsx"), overwrite = TRUE)
