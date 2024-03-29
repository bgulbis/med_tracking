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
library(themebg)
library(plotly)

# set path to data files
if (Sys.info()['sysname'] == "Windows") {
    p <- "U:/Data/med_tracking/"
} else if (Sys.info()['sysname'] == "Darwin") { # macOS
    p <- "/Volumes/brgulbis/Data/med_tracking/"
}

if (!dir.exists(p)) {
    stop("Network drive not available.")
}

source("src/target_meds_data.R", local = TRUE)

# raw_df <- get_data("data/raw", "tmc_target_meds") 

# f <- list.files("data/raw/fy21", "tmc_target_meds", full.names = TRUE)
# raw_df <- map_df(f, read_excel) %>%
#     rename_all(str_to_lower) %>%
#     distinct()
# 
# df_meds <- raw_df %>%
#     mutate(
#         across(medication, str_to_title),
#         across(
#             medication, 
#             str_replace_all, 
#             pattern = c(
#                 "Acetaminophen" = "Acetaminophen IV",
#                 "Levothyroxine" = "Levothyroxine IV",
#                 "Pantoprazole" = "Pantoprazole IV",
#                 " Human" = "",
#                 "Bupivacaine Liposome" = "Bupivacaine (liposomal)",
#                 "Immune Globulin Intravenous And Subcut" = "IVIG",
#                 "Immune Globulin Intravenous" = "IVIG"
#             )
#         ),
#         inpt = encntr_type %in% c("Inpatient", "Observation", "Emergency"),
#         medication = case_when(
#             medication == "IVIG" & inpt ~ "IVIG (inpatient)",
#             medication == "IVIG" & !inpt ~ "IVIG (outpatient)",
#             TRUE ~ medication
#         ),
#         product = case_when(
#             medication == "Epoetin Alfa" & (!str_detect(med_product, "epoetin alfa-epbx") | dose_month < mdy("5/1/2020")) ~ "Procrit",
#             medication == "Epoetin Alfa" & str_detect(med_product, "epoetin alfa-epbx") ~ "Retacrit",
#             medication == "Epoetin Alfa" ~ "Unknown"
#         )
#     ) %>%
#     arrange(medication, dose_month)

df_monthly <- df_meds %>%
    group_by(medication, product, dose_month) %>%
    summarize(across(c(patients, doses, quantity), sum, na.rm = TRUE)) %>%
    mutate(month = yearmonth(dose_month))

# df_monthly %>%
#     ggplot(aes(x = month, y = doses)) +
#     geom_line() +
#     facet_wrap(~ medication, scales = "free_y") +
#     theme_bg()

ts_doses <- df_meds %>%
    mutate(across(dose_month, as.Date)) %>%
    group_by(medication, dose_month) %>%
    summarize(across(c(patients, doses, quantity), sum, na.rm = TRUE)) %>%
    mutate(month = yearmonth(dose_month)) %>%
    as_tsibble(key = medication, index = month) %>%
    fill_gaps(doses = 0L) %>%
    mutate(across(dose_month, ~if_else(is.na(.), as.Date(month), .)))

# ts_doses %>% 
#     filter(medication == "Acetaminophen IV") %>%
#     # gg_season(y = doses)
#     autoplot()

df_feat <- ts_doses %>%
    features(doses, feat_stl)

df_feat2 <- ts_doses %>%
    features(log(doses + 1), feat_stl)
# 
# df_feat %>%
#     plot_ly(x = ~trend_strength, y = ~seasonal_strength_year, color = ~medication) %>%
#     add_markers() %>%
#     layout(yaxis = list(range = c(0, 1)))
# 
dcmp_stl <- ts_doses %>%
    model(STL(doses)) %>%
    components()

dcmp_stl2 <- ts_doses %>%
    model(STL(log(doses + 1))) %>%
    components()

dcmp_stl3 <- ts_doses %>%
    model(STL(log(doses + 1) ~ season(window = Inf))) %>%
    components()
# 
# fit_dcmp <- ts_doses %>%
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

# dt_cut <- max(df_meds$dose_month) - months(12)
# train <- ts_doses %>%
#     filter(dose_month < dt_cut) %>%
#     select(medication, month, doses)
# 
# test <- ts_doses %>%
#     filter(dose_month >= dt_cut) %>%
#     select(medication, month, doses)
# 
# df_train <- filter(train, medication == "Cangrelor")
# 
# 
# plan("multiprocess")
# tic()
# 
# fit_train <- train %>%
#     # filter(medication == "Cangrelor") %>%
#     model(
#         # ARIMA = ARIMA(doses, stepwise = FALSE, approximation = FALSE),
#         # ARIMA2 = ARIMA(log(doses + 1), stepwise = FALSE, approximation = FALSE),
#         ARIMA_D1 = decomposition_model(
#             STL(log(doses + 1)),
#             ARIMA(trend, stepwise = FALSE, approximation = FALSE),
#             ARIMA(remainder, stepwise = FALSE, approximation = FALSE)
#         ),
#         # ETS = ETS(doses),
#         ETS_D1 = decomposition_model(
#             STL(log(doses + 1) ~ season(window = Inf)),
#             ETS(trend ~ season("N")),
#             ETS(remainder ~ season("N"))
#         ),
#         # FASSTER = FASSTER(doses ~ trend() + ARMA()),
#         # FASSTER2 = FASSTER(log(doses + 1) ~ trend() + ARMA()),
#         # NAIVE = NAIVE(doses ~ drift()),
#         NAIVE_D1 = decomposition_model(
#             STL(doses),
#             NAIVE(season_adjust)
#         ),
#         # NNAR = NNETAR(log(doses) ~ AR(), n_networks = 30),
#         NNAR_D1 = decomposition_model(
#             STL(log(doses + 1)),
#             NNETAR(trend),
#             NNETAR(remainder)
#         ),
#         # TSLM = TSLM(log(doses + 1) ~ trend(knots = yearmonth(c("2015 Dec", "2017 Mar", "2018 Oct", "2019 Feb")))),
#         # VAR = VAR(doses),
#         VAR_D1 = decomposition_model(
#             STL(log(doses + 1) ~ season(window = Inf)),
#             VAR(trend),
#             VAR(remainder)
#         )
#     ) %>%
#     mutate(Forecast = (ARIMA_D1 + ETS_D1 + NNAR_D1 + VAR_D1) / 4)
# 
# toc()
# plan("sequential")
# 
# df_train_acc <- accuracy(fit_train)
# 
# fc_test <- forecast(fit_train, h = 12)
# 
# df_test_acc <- accuracy(fc_test, test) %>%
#     arrange(medication, .model)
# 
# df_train <- ts_doses %>% 
#     as_tibble() %>%
#     # filter(medication == "Acetaminophen IV") %>%
#     mutate(.model = "Actual") %>%
#     rename(.mean = doses) %>%
#     bind_rows(fc_test) %>%
#     mutate(.month = as.Date(month)) %>%
#     arrange(medication, .model, .month) %>%
#     select(-doses)
# 
# # ggplot(df_doses, aes(x = month, y = .mean, color = .model)) +
# #     geom_line()
# 
# # df_train <- select(df_train, -doses)
# 
# write_rds(df_train, "data/final/df_train.Rds")
# write_csv(df_train, "data/final/df_train.csv")
# df_train <- read_rds("data/final/df_train.Rds")
# 
# df_train %>%
#     plot_ly(x = ~.month, y = ~.mean, color = ~.model) %>%
#     add_lines()


# plan("multiprocess")
tic()

fit_doses <- ts_doses %>%
    # filter(medication %in% c("Acetaminophen IV", "Albumin", "Nicardipine", "Sugammadex")) %>%
    model(
        ARIMA = ARIMA(doses),
        ARIMA_D = decomposition_model(
            STL(log(doses + 1)),
            ARIMA(trend),
            ARIMA(season_year),
            ARIMA(remainder)
        ),
        # ARIMA_L = ARIMA(log(doses + 1), stepwise = FALSE, approximation = FALSE),
        ETS = ETS(doses),
        ETS_D = decomposition_model(
            STL(log(doses + 1) ~ season(window = Inf)),
            ETS(trend ~ season("N")),
            ETS(season_year ~ season("N")),
            ETS(remainder ~ season("N"))
        ),
        # ETS_L = ETS(log(doses + 1)),
        # NNAR = NNETAR(log(doses + 1) ~ AR(), n_networks = 30),
        # NNAR_D = decomposition_model(
        #     STL(log(doses + 1)),
        #     NNETAR(trend),
        #     NNETAR(remainder)
        # ),
        # NNAR_L = NNETAR(log(doses + 1) ~ AR(), n_networks = 30),
        VAR = VAR(doses),
        VAR_D = decomposition_model(
            STL(log(doses + 1) ~ season(window = Inf)),
            VAR(trend),
            VAR(season_year),
            VAR(remainder)
        )
        # VAR_L = VAR(log(doses + 1))
    ) %>%
    mutate(
        # Ensemble = (ARIMA + ETS + NNAR + VAR) / 4,
        # Ensemble_D = (ARIMA_D + ETS_D + NNAR_D + VAR_D) / 4,
        # Forecast_L = (ARIMA_L + ETS_L + NNAR_L + VAR_L) / 4,
        # Forecast = (ARIMA + ETS + NNAR + VAR + ARIMA_D + ETS_D + NNAR_D + VAR_D) / 8
        Forecast = (ARIMA + ETS + VAR + ARIMA_D + ETS_D + VAR_D) / 6
    )

toc()
# plan("sequential")

write_rds(fit_doses, paste0(p, "final/fit_doses.Rds"))
# df_acc <- accuracy(fit_doses)

# fit_doses <- read_rds("data/final/fit_doses.Rds")

tic()
# plan("multiprocess")
fc_doses <- forecast(fit_doses, h = 12)
# plan("sequential")
toc()

write_rds(fc_doses, paste0(p, "final/fc_doses.Rds"))

df_doses <- ts_doses %>% 
    as_tibble() %>%
    # filter(medication %in% c("Acetaminophen IV", "Albumin", "Nicardipine", "Sugammadex")) %>%
    mutate(.model = "Actual") %>%
    rename(.mean = doses) %>%
    bind_rows(fc_doses) %>%
    mutate(.month = as.Date(month)) %>%
    select(-doses, -dose_month)

# write_csv(df_doses, "data/final/df_doses.csv")

# df_doses %>%
#     plot_ly(x = ~.month, y = ~.mean, color = ~.model) %>%
#     add_lines()

df_fc_doses <- fc_doses %>%
    filter(!is.na(.mean)) %>%
    hilo() %>%
    unpack_hilo(c(`80%`, `95%`)) %>%
    select(-doses) %>%
    rename(
        lo_80 = `80%_lower`,
        hi_80 = `80%_upper`,
        lo_95 = `95%_lower`,
        hi_95 = `95%_upper`
    )

df_fc_doses_ind <- df_fc_doses %>%
    as_tibble() %>%
    filter(!str_detect(.model, "Ensemble|Forecast")) %>%
    mutate(
        across(c(.mean, starts_with(c("lo", "hi"))), round),
        date = as.Date(month)
    )

df_doses_hilo <- df_fc_doses_ind %>%
    group_by(medication, month) %>%
    summarize(across(c(lo_80, hi_80, lo_95, hi_95), mean, na.rm = TRUE))

df_fc_doses_combo <- df_fc_doses %>%
    as_tibble() %>%
    filter(.model == "Forecast") %>%
    select(-starts_with(c("hi", "lo"))) %>%
    left_join(df_doses_hilo, by = c("medication", "month")) %>%
    mutate(
        across(c(.mean, starts_with(c("lo", "hi"))), round),
        date = as.Date(month)
    )

write_rds(ts_doses, paste0(p, "final/ts_doses.Rds"))
write_rds(df_fc_doses_ind, paste0(p, "final/df_fc_doses_ind.Rds"))
write_rds(df_fc_doses_combo, paste0(p, "final/df_fc_doses_combo.Rds"))

# x <- ts_doses %>%
#     # as_tibble() %>%
#     mutate(date = as.Date(month))
# 
# y <- df_fc_dist_combo %>%
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
