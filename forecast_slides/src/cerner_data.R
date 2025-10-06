library(tidyverse)
library(readxl)
library(lubridate)
library(mbohelpr)
library(openxlsx)
# library(tsibble)
# library(fable)
# library(fasster)
# library(feasts)
# library(future)
# library(fcasthelpr)
# library(tictoc)
# library(themebg)
# library(plotly)

options(future.rng.onMisuse = "ignore")

f <- set_data_path("med_tracking", "fy23")

raw_df <- get_xlsx_data(paste0(f, "raw"), "tmc_target_meds")

df_meds <- raw_df |>
    mutate(
        across(medication, str_to_title),
        across(medication, \(x) str_replace_all(x, pattern = " Human", replacement = "")),
        route_group = case_when(
            route %in% c("DHT", "GT", "NG", "NJ", "PEG", "PO") ~ "PO",
            route %in% c("INJ", "IV", "IV Central", "IV Lock", "IVP", "IVPB", "DIALYSIS") ~ "IV",
            route %in% c("IM", "intra-ARTICULAR", "INTRAARTERIAL", "INTRADERM", "INTRATHECAL", "intraVENTRICular", "INTRAVESICULAR", "SUB-Q") ~ "INJ",
            TRUE ~ "Other"
        ),
        inpt = encntr_type %in% c("Inpatient", "Observation", "Emergency"),
        medication = case_when(
            medication == "Isavuconazonium" & route_group == "PO" ~ "Isavuconazonium (PO)",
            medication == "Isavuconazonium" ~ "Isavuconazonium (IV)",
            TRUE ~ medication
        )
    ) |>
    arrange(medication, dose_month)

meds <- distinct(df_meds, medication)

target_date <- df_meds |> 
    filter(medication == "Albumin") |>
    summarize(across(dose_month, max)) |> 
    pull()

add_end_date <- df_meds |> 
    group_by(medication) |> 
    summarize(across(dose_month, max)) |> 
    filter(dose_month < target_date) |> 
    mutate(
        dose_month = target_date,
        doses = 0L
    )

ts_doses <- df_meds |>
    # bind_rows(add_end_date) |> 
    mutate(across(dose_month, as.Date)) |>
    group_by(medication, dose_month) |>
    summarize(across(c(patients, doses), \(x) sum(x, na.rm = TRUE)), .groups = "drop") 
    # summarize(across(c(patients, doses, quantity), \(x) sum(x, na.rm = TRUE)), .groups = "drop") |>
    # summarize(across(c(patients, doses, quantity), sum, na.rm = TRUE)) |>
    # mutate(month = yearmonth(dose_month)) |>
    # as_tsibble(key = medication, index = month) |>
    # fill_gaps(doses = 0L) |>
    # mutate(across(dose_month, \(x) if_else(is.na(x), as.Date(month), x)))

# write_rds(ts_doses, paste0(f, "final/cerner_data.Rds"))
write.xlsx(ts_doses, paste0(f, "final/cerner_data.xlsx"), overwrite = TRUE)
