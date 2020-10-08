library(tidyverse)
library(lubridate)
library(mbohelpr)
library(tsibble)
library(fable)
library(fasster)
library(feasts)
# library(future)
library(fcasthelpr)
library(tictoc)
library(themebg)
library(plotly)

raw_df <- get_data("data/raw", "tmc_target_meds") 

df_meds <- raw_df %>%
    mutate(
        across(
            dose_datetime, 
            parse_datetime, 
            format = "%Y/%m/%d %H:%M:%S", 
            locale = locale(tz = "US/Central")
        ),
        month = floor_date(dose_datetime, unit = "month"),
        across(c(encntr_id, event_id), as.character),
        across(medication, str_to_title),
        across(
            medication, 
            str_replace_all, 
            pattern = c(
                "Acetaminophen" = "Acetaminophen IV",
                "Levothyroxine" = "Levothyroxine IV",
                "Pantoprazole" = "Pantoprazole IV",
                " Human" = "",
                "Bupivacaine Liposome" = "Bupivacaine (liposomal)",
                "Immune Globulin Intravenous And Subcut" = "IVIG",
                "Immune Globulin Intravenous" = "IVIG"
            )
        ),
        inpt = encntr_type %in% c("Inpatient", "Observation", "Emergency"),
        medication = case_when(
            medication == "IVIG" & inpt ~ "IVIG (inpatient)",
            medication == "IVIG" & !inpt ~ "IVIG (outpatient)",
            TRUE ~ medication
        )
    ) %>%
    arrange(medication, dose_datetime)

df_monthly <- df_meds %>%
    count(medication, month, name = "doses")

# df_monthly %>%
#     ggplot(aes(x = month, y = doses)) +
#     geom_line() +
#     facet_wrap(~ medication, scales = "free_y") +
#     theme_bg()

ts_doses <- df_meds %>%
    mutate(month = yearmonth(month)) %>%
    count(medication, month, name = "doses") %>%
    as_tsibble(key = medication, index = month) %>%
    fill_gaps(doses = 0L)

# ts_doses %>% 
#     filter(medication == "Acetaminophen IV") %>%
#     # gg_season(y = doses)
#     autoplot()

df_feat <- ts_doses %>% 
    features(doses, feat_stl)

df_feat2 <- ts_doses %>%
    features(log(doses + 1), feat_stl)

df_feat %>%
    plot_ly(x = ~trend_strength, y = ~seasonal_strength_year, color = ~medication) %>%
    add_markers() %>%
    layout(yaxis = list(range = c(0, 1)))

dcmp_stl <- ts_doses %>%
    model(STL(doses)) %>%
    components()

dcmp_stl2 <- ts_doses %>%
    model(STL(log(doses + 1))) %>%
    components()

dcmp_stl3 <- ts_doses %>%
    model(STL(log(doses + 1) ~ season(window = Inf))) %>%
    components()

fit_dcmp <- ts_doses %>%
    model(
        NAIVE = decomposition_model(
            STL(doses),
            NAIVE(season_adjust)
        ),
        ETS = decomposition_model(
            STL(log(doses + 1) ~ season(window = Inf)),
            ETS(trend ~ season("N")),
            ETS(remainder ~ season("N"))
        ),
        ARIMA = decomposition_model(
            STL(log(doses + 1)),
            ARIMA(trend, stepwise = FALSE, approximation = FALSE),
            ARIMA(remainder, stepwise = FALSE, approximation = FALSE)
        )
    )

df_a <- accuracy(fit_dcmp)

fc_dcmp <- forecast(fit_dcmp, h = 12)

ts_med <- filter(ts_doses, medication == "Acetaminophen IV")
train <- slice(ts_med, 0:(n()-12))
test <- slice(ts_med, n()-11:0)

# plan("multiprocess")
tic()

fit_train <- train %>%
    # filter(medication == "Acetaminophen IV") %>%
    model(
        ARIMA = ARIMA(doses, stepwise = FALSE, approximation = FALSE),
        ARIMA_D1 = decomposition_model(
            STL(log(doses + 1)),
            ARIMA(trend, stepwise = FALSE, approximation = FALSE),
            ARIMA(remainder, stepwise = FALSE, approximation = FALSE)
        ),
        ARIMA_D2 = decomposition_model(
            STL(log(doses + 1)),
            ARIMA(season_adjust, stepwise = FALSE, approximation = FALSE)
        ),
        ETS = ETS(doses),
        ETS_D1 = decomposition_model(
            STL(log(doses + 1) ~ season(window = Inf)),
            ETS(trend ~ season("N")),
            ETS(remainder ~ season("N"))
        ),
        ETS_D2 = decomposition_model(
            STL(log(doses + 1) ~ season(window = Inf)),
            ETS(season_adjust ~ season("N"))
        ),
        FASSTER = FASSTER(doses ~ trend() + ARMA()),
        NAIVE = NAIVE(log(doses)),
        NNAR = NNETAR(log(doses) ~ AR(), n_networks = 30),
        # STLF = decomposition_model(
        #     STL(doses),
        #     ARIMA(season_adjust, stepwise = FALSE, approximation = FALSE)
        # ),
        STLF = decomposition_model(
            STL(doses),
            NAIVE(season_adjust)
        ),
        TSLM = TSLM(log(doses) ~ trend(knots = yearmonth(c("2015 Dec", "2017 Mar", "2018 Oct", "2019 Feb")))),
        VAR = VAR(doses)
    ) %>%
    mutate(Forecast = (ARIMA + ETS + FASSTER + NNAR + STLF + TSLM + VAR) / 7)

toc()
# plan("sequential")

# accuracy(fit_doses)

fc_test <- forecast(fit_train, h = 12)

accuracy(fc_test, test)

df_train <- ts_doses %>% 
    as_tibble() %>%
    filter(medication == "Acetaminophen IV") %>%
    mutate(.model = "Actual") %>%
    rename(.mean = doses) %>%
    bind_rows(fc_test) %>%
    mutate(.month = as.Date(month))

# ggplot(df_doses, aes(x = month, y = .mean, color = .model)) +
#     geom_line()

df_train %>%
    plot_ly(x = ~.month, y = ~.mean, color = ~.model) %>%
    add_lines()


fit_doses <- ts_med %>%
    model(
        # ARIMA = ARIMA(doses, stepwise = FALSE, approximation = FALSE),
        # ARIMA2 = ARIMA(log(doses + 1), stepwise = FALSE, approximation = FALSE),
        ARIMA_D1 = decomposition_model(
            STL(log(doses + 1)),
            ARIMA(trend, stepwise = FALSE, approximation = FALSE),
            ARIMA(remainder, stepwise = FALSE, approximation = FALSE)
        ),
        # ETS = ETS(doses),
        ETS_D1 = decomposition_model(
            STL(log(doses + 1) ~ season(window = Inf)),
            ETS(trend ~ season("N")),
            ETS(remainder ~ season("N"))
        ),
        # FASSTER = FASSTER(doses ~ trend() + ARMA()),
        # FASSTER2 = FASSTER(log(doses + 1) ~ trend() + ARMA()),
        # NNAR = NNETAR(log(doses) ~ AR(), n_networks = 30),
        NNAR_D1 = decomposition_model(
            STL(log(doses + 1)),
            NNETAR(trend),
            NNETAR(remainder)
        ),
        # STLF = decomposition_model(
        #     STL(doses),
        #     NAIVE(season_adjust)
        # ),
        # TSLM = TSLM(log(doses + 1) ~ trend(knots = yearmonth(c("2015 Dec", "2017 Mar", "2018 Oct", "2019 Feb")))),
        # VAR = VAR(doses),
        VAR_D1 = decomposition_model(
            STL(log(doses + 1) ~ season(window = Inf)),
            VAR(trend),
            VAR(remainder)
        )
    ) %>%
    mutate(Forecast = (ARIMA_D1 + ETS_D1 + NNAR_D1 + VAR_D1) / 4)

accuracy(fit_doses)

fc_doses <- forecast(fit_doses, h = 12)

df_doses <- ts_doses %>% 
    as_tibble() %>%
    filter(medication == "Acetaminophen IV") %>%
    mutate(.model = "Actual") %>%
    rename(.mean = doses) %>%
    bind_rows(fc_doses) %>%
    mutate(.month = as.Date(month))

df_doses %>%
    plot_ly(x = ~.month, y = ~.mean, color = ~.model) %>%
    add_lines()

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
    filter(.model != "Forecast") %>%
    mutate(
        across(c(.mean, starts_with(c("lo", "hi"))), round),
        date = as.Date(month)
    )

df_doses_hilo <- df_fc_doses_ind %>%
    group_by(medication, month) %>%
    summarize(across(c(lo_80, hi_80, lo_95, hi_95), mean, na.rm = TRUE))

df_fc_dist_combo <- df_fc_doses %>%
    as_tibble() %>%
    filter(.model == "Forecast") %>%
    select(-starts_with(c("hi", "lo"))) %>%
    left_join(df_doses_hilo, by = c("medication", "month")) %>%
    mutate(
        across(c(.mean, starts_with(c("lo", "hi"))), round),
        date = as.Date(month)
    )

x <- ts_med %>%
    # as_tibble() %>%
    mutate(date = as.Date(month))

y <- df_fc_dist_combo %>%
    mutate(.model = "A_Fcast")

plotly_fable(
    x, 
    y = doses, 
    combo = y, 
    mods = df_fc_doses_ind, 
    title = "Acetaminophen IV", 
    ytitle = "Doses",
    width = NULL,
    height = NULL
)
