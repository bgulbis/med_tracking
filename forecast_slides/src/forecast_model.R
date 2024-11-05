library(tidyverse)
library(readxl)
library(lubridate)
library(mbohelpr)
library(tsibble)
library(fable)
library(fasster)
library(feasts)
library(future)
library(fcasthelpr)
library(tictoc)
# library(themebg)
# library(plotly)

options(future.rng.onMisuse = "ignore")

f <- set_data_path("med_tracking", "forecast_slides")

df_cerner <- read_excel(paste0(f, "final/cerner_data.xlsx"))

# raw_data <- get_xlsx_data(paste0(f, "raw"), "target_medications")

library(xlsx)
library(rstudioapi)
df_epic <- read.xlsx(paste0(f, "raw/target_medications_2024-10.xlsx"), sheetIndex = 1, startRow = 16, header = FALSE, password = rstudioapi::askForPassword("Input password for excel sheet."))

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
    bind_rows(add_end_date) |> 
    mutate(across(dose_month, as.Date)) |>
    group_by(medication, dose_month) |>
    summarize(across(c(patients, doses, quantity), \(x) sum(x, na.rm = TRUE)), .groups = "drop") |>
    # summarize(across(c(patients, doses, quantity), sum, na.rm = TRUE)) |>
    mutate(month = yearmonth(dose_month)) |>
    as_tsibble(key = medication, index = month) |>
    fill_gaps(doses = 0L) |>
    mutate(across(dose_month, \(x) if_else(is.na(x), as.Date(month), x)))

plan("multisession")
tic()

fit_doses <- ts_doses |> 
    model(
        ARIMA = ARIMA(doses, stepwise = FALSE, approximation = FALSE),
        ARIMA_D = decomposition_model(
            STL(log(doses + 1)),
            ARIMA(trend, stepwise = FALSE, approximation = FALSE),
            ARIMA(remainder, stepwise = FALSE, approximation = FALSE)
        ),
        ETS = ETS(doses),
        ETS_D = decomposition_model(
            STL(log(doses + 1) ~ season(window = Inf)),
            ETS(trend ~ season("N")),
            ETS(remainder ~ season("N"))
        ),        
        # NNAR = NNETAR(log(doses) ~ AR(), n_networks = 30),
        VAR = VAR(doses),
        VAR_D = decomposition_model(
            STL(log(doses + 1) ~ season(window = Inf)),
            VAR(trend),
            VAR(remainder)
        )
    ) |> 
    mutate(Forecast = (ARIMA + ETS + VAR + ARIMA_D + ETS_D + VAR_D) / 6)

toc()
plan("sequential")

# df_acc <- accuracy(fit_doses)
# 
# df_acc2 <- df_acc |> 
#     select(medication, .model, RMSE) |> 
#     filter(!is.nan(RMSE)) |> 
#     pivot_wider(names_from = .model, values_from = RMSE)

# plan("sequential")

write_rds(fit_doses, paste0(f, "final/fit_doses.Rds"))

# fit_doses <- read_rds("data/final/fit_doses.Rds")

tic()
plan("multisession")
fc_doses <- forecast(fit_doses, h = 12)
plan("sequential")
toc()

write_rds(fc_doses, paste0(f, "final/fc_doses.Rds"))

# fc_doses_fcast <- filter(fc_doses, .model == "Forecast")

df_doses <- ts_doses |> 
    as_tibble() |>
    # filter(medication %in% c("Albumin", "Amphotericin B Liposomal", "Daratumumab-Hyaluronidase")) |>
    mutate(.model = "Actual") |>
    rename(.mean = doses) |>
    bind_rows(fc_doses) |>
    mutate(.month = as.Date(month)) |>
    select(-doses, -dose_month)

# write_csv(df_doses, "data/final/df_doses.csv")

# df_doses |>
#     plot_ly(x = ~.month, y = ~.mean, color = ~.model) |>
#     add_lines()

df_fc_doses <- fc_doses |>
    filter(!is.na(.mean)) |>
    hilo() |>
    # unpack_hilo(c(`80%`, `95%`)) |>
    mutate(
        lo_80 = `80%`$lower,
        hi_80 = `80%`$upper,
        lo_95 = `95%`$lower,
        hi_95 = `95%`$upper
    ) |> 
    select(-doses, -`80%`, -`95%`)

df_fc_doses_ind <- df_fc_doses |>
    as_tibble() |>
    filter(!str_detect(.model, "Ensemble|Forecast")) |>
    mutate(
        across(c(.mean, starts_with(c("lo", "hi"))), round),
        date = as.Date(month)
    )

df_doses_hilo <- df_fc_doses_ind |>
    group_by(medication, month) |>
    summarize(across(c(lo_80, hi_80, lo_95, hi_95), \(x) mean(x, na.rm = TRUE)))

df_fc_doses_combo <- df_fc_doses |>
    as_tibble() |>
    filter(.model == "Forecast") |>
    select(-starts_with(c("hi", "lo"))) |>
    left_join(df_doses_hilo, by = c("medication", "month")) |>
    mutate(
        across(c(.mean, starts_with(c("lo", "hi"))), round),
        date = as.Date(month)
    )

write_rds(ts_doses, paste0(f, "final/ts_doses.Rds"))
write_rds(df_fc_doses_ind, paste0(f, "final/df_fc_doses_ind.Rds"))
write_rds(df_fc_doses_combo, paste0(f, "final/df_fc_doses_combo.Rds"))
