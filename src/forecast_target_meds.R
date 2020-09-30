library(tidyverse)
library(lubridate)
library(mbohelpr)
library(tsibble)
library(fable)
library(future)
library(tictoc)

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

ts_doses <- df_meds %>%
    mutate(month = yearmonth(month)) %>%
    count(medication, month, name = "doses") %>%
    as_tsibble(key = medication, index = month)

plan("multiprocess")
tic()

fit_doses <- ts_doses %>%
    filter(medication == "Acetaminophen IV") %>%
    model(
        ARIMA = ARIMA(doses, stepwise = FALSE, approximation = FALSE),
        ARIMA2 = ARIMA(log(doses) ~ PDQ(0, 0, 0), stepwise = FALSE, approximation = FALSE),
        ETS = ETS(doses),
        ETS2 = ETS(log(doses)),
        NNAR = NNETAR(log(doses) ~ AR(), n_networks = 30),
        VAR = VAR(doses)
    )

toc()
plan("sequential")

accuracy(fit_doses)
