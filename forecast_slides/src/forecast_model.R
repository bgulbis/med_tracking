library(tidyverse)
library(readxl)
library(lubridate)
library(mbohelpr)
library(tsibble)
# library(xlsx)
# library(rstudioapi)
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

df_cerner <- read_excel(paste0(f, "final/cerner_data.xlsx")) |> 
    mutate(across(dose_month, as.Date))

zz_meds_cerner <- distinct(df_cerner, medication) |> arrange(medication)
# raw_data <- get_xlsx_data(paste0(f, "raw"), "target_medications")

# get_pwd_data <- function(path, pattern, colnm, coltype, sheetIndex = 1, startRow = 41, header = FALSE) {
#     f <- list.files(path, pattern, full.names = TRUE)
#     pwd <- rstudioapi::askForPassword("Input password")
    
#     df <- f |> 
#         purrr::map_df(
#             xlsx::read.xlsx,
#             sheetIndex = sheetIndex,
#             startRow = startRow,
#             colClasses = coltype,
#             header = header,
#             password = pwd
#         ) |> 
#         dplyr::distinct()
    
#     colnames(df) <- colnm
    
#     df
# }

colnm <- c("month_begin", "month_end", "medication", "route", "doses", "patients")
coltype <- c("date", "date", "text", "text", "numeric", "numeric")

# df_epic <- get_pwd_data(paste0(f, "raw/"), "target_medications", colnm, coltype)

df_epic <- get_xlsx_data(paste0(f, "raw/"), "target_medications", 1, colnm, coltype, skip = 41)

zz_meds_epic <- distinct(df_epic, medication) |> arrange(medication)
zz_route <- distinct(df_epic, route) |> arrange(route)

df_meds <- df_epic |>
    mutate(
        across(
            medication, \(x) case_when(
                medication == "Albumin Human" ~ "Albumin",
                medication == "Amphotericin B Liposome" ~ "Amphotericin B Liposomal",
                medication == "Anti-Thymocyte Glob (Rabbit)" ~ "Anti-Thymocyte Globulin (Rabbit)",
                medication == "Bictegravir-Emtricitab-Tenofov" ~ "Bictegravir/Emtricitabine/Tenofovir",
                medication == "Cangrelor Tetrasodium" ~ "Cangrelor",
                medication == "Cefiderocol Sulfate Tosylate" ~ "Cefiderocol",
                medication == "cefTAZidime-Avibactam" ~ "Ceftazidime-Avibactam",
                medication == "Daratumumab-Hyaluronidase-fihj" ~ "Daratumumab-Hyaluronidase",
                medication == "Isavuconazonium Sulfate" & route == "Oral" ~ "Isavuconazonium (PO)",
                medication == "Isavuconazonium Sulfate" ~ "Isavuconazonium (IV)",
                medication == "Sugammadex Sodium" ~ "Sugammadex",
                medication == "Ravulizumab-cwvz" ~ "Ravulizumab",
                str_detect(medication, "Thrombin") & route == "Apply externally" ~ "Thrombin Topical",
                .default = medication
            )
        )
    ) |> 
    filter(!medication %in% c("Thrombin", "Thrombin (Recombinant)")) |> 
    select(medication, dose_month = month_begin, patients, doses) |> 
    bind_rows(df_cerner) |> 
    summarize(
        across(c(patients, doses), sum),
        .by = c(medication, dose_month)
    ) |> 
    arrange(medication, dose_month)

meds <- distinct(df_meds, medication)

target_date <- df_meds |> 
    # filter(medication == "Albumin") |>
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
    summarize(
        across(c(patients, doses), \(x) sum(x, na.rm = TRUE)), 
        .by = c(medication, dose_month)
    ) |>
    mutate(month = yearmonth(dose_month)) |>
    as_tsibble(key = medication, index = month) |>
    fill_gaps(doses = 0L) |>
    mutate(across(dose_month, \(x) if_else(is.na(x), as.Date(month), x)))

# ts_doses_thrombin <- ts_doses |> 
#     filter(medication != "Thrombin Topical" | (medication == "Thrombin Topical" & dose_month >= as.Date(mdy("10/1/2024"))))

plan("multisession")
tic()

arima_approx <- TRUE

fit_doses <- ts_doses |> 
    filter(medication != "Thrombin Topical" | (medication == "Thrombin Topical" & dose_month >= as.Date(mdy("10/1/2024")))) |> 
    model(
        ARIMA = ARIMA(doses, stepwise = arima_approx, approximation = arima_approx),
        ARIMA_D = decomposition_model(
            STL(log(doses + 1)),
            ARIMA(trend, stepwise = arima_approx, approximation = arima_approx),
            ARIMA(remainder, stepwise = arima_approx, approximation = arima_approx)
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
    mutate(
        Forecast = if_else(
            medication == "Thrombin Topical", 
            (ARIMA + ETS + ARIMA_D + ETS_D) / 4, 
            (ARIMA + ETS + VAR + ARIMA_D + ETS_D + VAR_D) / 6)
    )

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
    # group_by(medication, month) |>
    summarize(
        across(c(lo_80, hi_80, lo_95, hi_95), \(x) mean(x, na.rm = TRUE)),
        .by = c(medication, month)
    )

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
