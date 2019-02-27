library(tidyverse)
library(readxl)
library(lubridate)

purch <- read_excel(
    "data/external/raw_purchasing.xlsx", 
    col_names = c(
        "drug",
        "2018-06-01",
        "2018-07-01",
        "2018-08-01",
        "2018-09-01",
        "2018-10-01",
        "2018-11-01",
        "2018-12-01",
        "2019-01-01"
    ),
    skip = 1
) %>%
    gather(month, purchase, -drug) %>%
    mutate_at(
        "drug",
        str_replace_all,
        pattern = c(
            "ACETAMINOPHEN/OFIRMEV" = "acetaminophen",
            "ALBUMIN/ALBUMINAR/ALBUTEIN" = "albumin human",
            "CATHFLO/ACTIVASE/ALTEPLASE" = "alteplase",
            "CERVIDIL/DINOPROSTONE" = "dinoprostone topical",
            "EXPAREL/BUPIVACAINE/LIPOSOMAL" = "bupivacaine liposome",
            "NEULASTA/PEGFILGRASTIM" = "pegfilgrastim",
            "SANTYL/COLLAGENASE" = "collagenase topical",
            "VASSOPRESIN/VASOSTRICT/PRESSYN" = "vasopressin",
            "IVIG/IMMUNE GLOBULIN" = "immune globulin intravenous",
            "DAPTOMYCIN/CUBICIN" = "DAPTOmycin",
            "CEFTAROLINE/TEFLARO" = "ceftaroline",
            "ERTAPENEM/INVANZ" = "ertapenem",
            "ZERBAXA/CEFTOLOZANE" = "ceftolozane-tazobactam",
            "AVYCAZ/CEFTAZIDIME" = "ceftazidime-avibactam",
            "NOREPINEPHRINE SYRINGE/LEVOPHED" = "norepinephrine",
            "SOLIRIS/ECULIZUMAB" = "eculizumab",
            "SPINRAZA/NUSINERSEN" = "nusinersen",
            "SUGAMMADEX/BRIDION" = "sugammadex",
            "CALCITONIN/MIACALCIN" = "calcitonin",
            "ISOPROTERENOL/ISUPREL" = "isoproterenol"
        )
    ) %>%
    mutate_at("month", ymd, tz = "UTC") %>%
    mutate_at("purchase", round, digits = 2)

write_csv(purch, "data/external/tidy_purchasing.csv")
