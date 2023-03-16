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
        # product = case_when(
        #     medication == "Epoetin Alfa" & str_detect(med_product, "epoetin alfa-epbx") ~ "Retacrit",
        #     # medication == "Epoetin Alfa" & (!str_detect(med_product, "epoetin alfa-epbx") | dose_month < mdy("5/1/2020")) ~ "Procrit",
        #     medication == "Epoetin Alfa" ~ "Procrit"
        # )
    ) |>
    arrange(medication, dose_month)

meds <- distinct(df_meds, medication)

df_monthly <- df_meds |>
    group_by(medication, dose_month) |>
    summarize(across(c(patients, doses, quantity), \(x) sum(x, na.rm = TRUE)), .groups = "drop") |>
    mutate(month = yearmonth(dose_month))

# df_monthly |>
#     ggplot(aes(x = month, y = doses)) +
#     geom_line() +
#     facet_wrap(~ medication, scales = "free_y") +
#     theme_bg()

ts_doses <- df_meds |>
    mutate(across(dose_month, as.Date)) |>
    group_by(medication, dose_month) |>
    summarize(across(c(patients, doses, quantity), \(x) sum(x, na.rm = TRUE)), .groups = "drop") |>
    # summarize(across(c(patients, doses, quantity), sum, na.rm = TRUE)) |>
    mutate(month = yearmonth(dose_month)) |>
    as_tsibble(key = medication, index = month) |>
    fill_gaps(doses = 0L) |>
    mutate(across(dose_month, \(x) if_else(is.na(x), as.Date(month), x)))

# ts_doses |> 
#     filter(medication == "Acetaminophen IV") |>
#     # gg_season(y = doses)
#     autoplot()

# df_feat <- ts_doses |>
#     features(doses, feat_stl)
# 
# df_feat2 <- ts_doses |>
#     features(log(doses + 1), feat_stl)
# 
# df_feat |>
#     plot_ly(x = ~trend_strength, y = ~seasonal_strength_year, color = ~medication) |>
#     add_markers() |>
#     layout(yaxis = list(range = c(0, 1)))
# 
# dcmp_stl <- ts_doses |>
#     model(STL(doses)) |>
#     components()
# 
# dcmp_stl2 <- ts_doses |>
#     model(STL(log(doses + 1))) |>
#     components()
# 
# dcmp_stl3 <- ts_doses |>
#     model(STL(log(doses + 1) ~ season(window = Inf))) |>
#     components()
# 
# fit_dcmp <- ts_doses |>
#     model(
#         NAIVE = decomposition_model(
#             STL(doses),
#             NAIVE(season_adjust)
#         ),
#         ETS = decomposition_model(
#             STL(log(doses + 1) ~ season(window = Inf)),
#             ETS(trend ~ season("N")),
#             ETS(remainder ~ season("N"))
#         ),
#         ARIMA = decomposition_model(
#             STL(log(doses + 1)),
#             ARIMA(trend, stepwise = FALSE, approximation = FALSE),
#             ARIMA(remainder, stepwise = FALSE, approximation = FALSE)
#         )
#     )
# 
# df_a <- accuracy(fit_dcmp)
# 
# fc_dcmp <- forecast(fit_dcmp, h = 12)

# ts_med <- filter(ts_doses, medication == "Acetaminophen IV")
# train <- slice(ts_med, 0:(n()-12))
# test <- slice(ts_med, n()-11:0)

dt_cut <- max(df_meds$dose_month) - months(3)

train <- ts_doses |> 
    filter(dose_month < dt_cut) |> 
    select(medication, month, doses)

test <- ts_doses |> 
    filter(dose_month >= dt_cut) |> 
    select(medication, month, doses)
# 
# df_train <- filter(train, medication == "Cangrelor")
# 
# 
# plan("multisession")
# tic()

# fit_train <- train |> 
#     filter(medication == "Amphotericin B Liposomal") |> 
#     model(
#         ARIMA = ARIMA(doses, stepwise = FALSE, approximation = FALSE),
#         ARIMA2 = ARIMA(log(doses + 1), stepwise = FALSE, approximation = FALSE),
#         ARIMA_D1 = decomposition_model(
#             STL(log(doses + 1)),
#             ARIMA(trend, stepwise = FALSE, approximation = FALSE),
#             ARIMA(remainder, stepwise = FALSE, approximation = FALSE)
#         ),
#         ETS = ETS(doses),
#         ETS_D1 = decomposition_model(
#             STL(log(doses + 1) ~ season(window = Inf)),
#             ETS(trend ~ season("N")),
#             ETS(remainder ~ season("N"))
#         ),
#         # FASSTER = FASSTER(doses ~ trend() + ARMA()),
#         # FASSTER2 = FASSTER(log(doses + 1) ~ trend() + ARMA()),
#         NAIVE = NAIVE(doses ~ drift()),
#         NAIVE_D1 = decomposition_model(
#             STL(doses),
#             NAIVE(season_adjust)
#         ),
#         NNAR = NNETAR(log(doses) ~ AR()),
#         NNAR2 = NNETAR(log(doses) ~ AR(), n_networks = 30),
#         NNAR_D1 = decomposition_model(
#             STL(log(doses + 1)),
#             NNETAR(trend),
#             NNETAR(remainder)
#         ),
#         # TSLM = TSLM(log(doses + 1) ~ trend(knots = yearmonth(c("2015 Dec", "2017 Mar", "2018 Oct", "2019 Feb")))),
#         VAR = VAR(doses),
#         VAR_D1 = decomposition_model(
#             STL(log(doses + 1) ~ season(window = Inf)),
#             VAR(trend),
#             VAR(remainder)
#         )
#     ) 
    # mutate(Forecast = (ARIMA_D1 + ETS_D1 + NNAR_D1 + VAR_D1) / 4)

plan("multisession")
tic()

fit_train <- train |> 
    # filter(medication %in% c("Albumin", "Amphotericin B Liposomal", "Daratumumab-Hyaluronidase")) |>
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
        # Forecast = combination_model(
        #     ARIMA(doses, stepwise = FALSE, approximation = FALSE),
        #     ETS(doses),
        #     VAR(doses)
        # )
        # Forecast2 = combination_model(
        #     ARIMA(doses, stepwise = FALSE, approximation = FALSE),
        #     ETS(doses),
        #     NNETAR(log(doses) ~ AR()), # n_networks = 30
        #     VAR(doses)
        # ) 
    ) 

fit_train_ens <- fit_train |> 
    mutate(
        Forecast = (ARIMA + ETS + VAR) / 3,
        Forecast_D = (ARIMA_D + ETS_D + VAR_D) / 3,
        Forecast_RD = (ARIMA + ETS + VAR + ARIMA_D + ETS_D + VAR_D) / 6
        # Forecast_N = (ARIMA + ETS + NNAR + VAR) / 4
    )

toc()
plan("sequential")

options(scipen=999)
# df_train_acc <- accuracy(fit_train)
df_train_acc_ens <- accuracy(fit_train_ens)

plan("multisession")
tic()
# fc_test <- forecast(fit_train, h = 3)
fc_test_ens <- forecast(fit_train_ens, h = 4)
toc()
plan("sequential")

# options(scipen=999)
# df_test_acc <- fc_test |> 
#     accuracy(test) |>
#     arrange(medication, .model)

df_test_acc_ens <- fc_test_ens |> 
    accuracy(test) |>
    arrange(medication, .model)

# fit_train_ensemble <- fit_train |> 
#     mutate(
#         Forecast = (ARIMA + ETS + NNAR + VAR) / 4,
#         Forecast_1 = (ARIMA_D1 + ETS_D1 + VAR_D1) / 3
#     ) |> 
#     select(medication, Forecast, Forecast_1)
# 
# df_train_acc_ens <- accuracy(fit_train_ensemble)
# 
# plan("multisession")
# tic()
# fc_test_ens <- fit_train_ensemble |> 
#     # select(medication, Forecast) |> 
#     forecast(h = 3)
# toc()
# plan("sequential")
# 
# df_test_acc_ens <- accuracy(fc_test_ens, test) |> 
#     arrange(medication, .model)

test_med = "Remdesivir"

fc_test_alb <- fc_test_ens |> 
    filter(
        medication == test_med,
        str_detect(.model, "Forecast")
    ) 

df_train <- ts_doses |>
    as_tibble() |>
    filter(medication == test_med) |>
    mutate(.model = "Actual") |>
    rename(.mean = doses) |>
    bind_rows(fc_test_alb) |>
    mutate(.month = as.Date(month)) |>
    arrange(medication, .model, .month) |>
    select(-doses)

df_train |> 
    filter(.month >= mdy("7/1/2022")) |> 
    ggplot(aes(x = month, y = .mean, color = .model)) +
    geom_line()
# 
# # df_train <- select(df_train, -doses)
# 
# write_rds(df_train, "data/final/df_train.Rds")
# write_csv(df_train, "data/final/df_train.csv")
# df_train <- read_rds("data/final/df_train.Rds")
# 
# df_train |>
#     plot_ly(x = ~.month, y = ~.mean, color = ~.model) |>
#     add_lines()

plan("multisession")
tic()

fit_doses <- ts_doses |> 
    # filter(medication %in% c("Albumin", "Amphotericin B Liposomal", "Daratumumab-Hyaluronidase")) |>
    model(
        Forecast = combination_model(
            ARIMA(doses, stepwise = FALSE, approximation = FALSE),
            ETS(doses),
            # NNETAR(log(doses) ~ AR()), # n_networks = 30
            VAR(doses)
        )
    )

toc()
plan("sequential")


# fit_test <- ts_doses |>
#     filter(medication == "Albumin") |> 
#     # filter(medication %in% c("Albumin", "Daratumumab-Hyaluronidase")) |> 
#     model(
#         Forecast = combination_model(
#             ARIMA(doses),
#             decomposition_model(
#                 STL(log(doses + 1)),
#                 ARIMA(trend),
#                 SNAIVE(season_year),
#                 ARIMA(remainder)
#             ),
#             ETS(doses),
#             VAR(doses)
#         )
#     )

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
    unpack_hilo(c(`80%`, `95%`)) |>
    select(-doses) |>
    rename(
        lo_80 = `80%_lower`,
        hi_80 = `80%_upper`,
        lo_95 = `95%_lower`,
        hi_95 = `95%_upper`
    )

df_fc_doses_ind <- df_fc_doses |>
    as_tibble() |>
    filter(!str_detect(.model, "Ensemble|Forecast")) |>
    mutate(
        across(c(.mean, starts_with(c("lo", "hi"))), round),
        date = as.Date(month)
    )

df_doses_hilo <- df_fc_doses_ind |>
    group_by(medication, month) |>
    summarize(across(c(lo_80, hi_80, lo_95, hi_95), mean, na.rm = TRUE))

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

# x <- ts_doses |>
#     # as_tibble() |>
#     mutate(date = as.Date(month))
# 
# y <- df_fc_dist_combo |>
#     mutate(.model = "A_Fcast")
# 
# plotly_fable(
#     x, 
#     y = doses, 
#     combo = y, 
#     mods = df_fc_doses_ind, 
#     title = "Acetaminophen IV", 
#     ytitle = "Doses",
#     width = NULL,
#     height = NULL
# )

# a <- distinct(df_doses, medication)
