library(tidyverse)
library(readxl)
library(lubridate)
library(mbohelpr)
library(openxlsx)

f <- set_data_path("med_tracking", "overbudget")

raw_tpn <- read_excel(
    paste0(f, "raw/tpn_utilization.xlsx"), 
    skip = 10, 
    col_names = c("range_start", "range_end", "mrn", "encounter_csn", "order_id", "order_name", "med_datetime", "nurse_unit")
)

zz_prods <- distinct(raw_tpn, order_name) |> arrange(order_name)

df_tpn <- raw_tpn |> 
    mutate(
        tpn_product = case_when(
            str_detect(order_name, "Clinimix") ~ "clinimix",
            str_detect(order_name, "Central") ~ "tpn",
            str_detect(order_name, "Peripheral") ~ "ppn"
        ),
        med_month = floor_date(med_datetime, unit = "month")
    )

df_tpn_num <- df_tpn |> 
    distinct(encounter_csn)

df_tpn_num_loc <- df_tpn |> 
    count(nurse_unit, name = "num_tpn", sort = TRUE)

df_tpn_one_day_num <- df_tpn |> 
    count(encounter_csn, name = "num_tpn") |> 
    filter(num_tpn == 1) 

df_tpn_avg_days <- df_tpn |> 
    count(encounter_csn, name = "num_tpn") |> 
    summarize(
        avg_days = mean(num_tpn),
        sd_days = sd(num_tpn)
    )

df_tpn_one_day_loc <- df_tpn |> 
    semi_join(df_tpn_one_day_num, by = "encounter_csn") |> 
    count(nurse_unit, name = "num_one_day", sort = TRUE) 

df_tpn_one_day <- df_tpn_num_loc |> 
    left_join(df_tpn_one_day_loc, by = "nurse_unit")


df_tpn_one_day <- df_tpn |> 
    count(encounter_csn, nurse_unit, name = "num_tpn") |> 
    filter(num_tpn == 1) |> 
    distinct(encounter_csn, nurse_unit) |> 
    count(nurse_unit, name = "num_one_day")

df_tpn_one_day_percent <- df_tpn |> 
    distinct(encounter_csn, nurse_unit) |> 
    count(nurse_unit, name = "num_pts") |> 
    left_join(df_tpn_one_day, by = "nurse_unit") |> 
    mutate(
        across(num_one_day, \(x) coalesce(x, 0)),
        pct_one_day = num_one_day / num_pts
    )

df_tpn_one_day_summary <- df_tpn_one_day_percent |> 
    filter(num_pts > 1, num_one_day > 0)

write.xlsx(df_tpn_one_day_summary, paste0(f, "final/one_day_tpn.xlsx"), overwrite = TRUE)

df_tpn_num_monthly <- df_tpn |> 
    count(encounter_csn, tpn_product, nurse_unit, med_month, name = "num_tpn")
