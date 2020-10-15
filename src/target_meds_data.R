f <- list.files("data/raw/fy21", "tmc_target_meds", full.names = TRUE)
raw_df <- map_df(f, read_excel) %>%
    rename_all(str_to_lower) %>%
    distinct()

df_meds <- raw_df %>%
    mutate(
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
        ),
        product = case_when(
            medication == "Epoetin Alfa" & str_detect(med_product, "epoetin alfa-epbx") ~ "Retacrit",
            # medication == "Epoetin Alfa" & (!str_detect(med_product, "epoetin alfa-epbx") | dose_month < mdy("5/1/2020")) ~ "Procrit",
            medication == "Epoetin Alfa" ~ "Procrit"
        )
    ) %>%
    arrange(medication, dose_month)
