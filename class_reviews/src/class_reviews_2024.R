library(tidyverse)
library(readxl)
library(lubridate)
library(mbohelpr)
library(openxlsx)

options(scipen = 0)

f <- set_data_path("med_tracking", "class_reviews")

raw_df <- get_xlsx_data(paste0(f, "raw"), "mhhs_cardiovascular_purchases_03-2023_02-2024", sheet = 1) |> 
    rename_all(str_to_lower) |> 
    rename(
        prod_desc = `product description`,
        account_name = `account name`,
        shipped_qty = `shipped qty`,
        unit_size_qty = `unit size qty`,
        units_shipped = `total shipped units, tabs, ml`
    )

df_prod_sum <- raw_df |> 
    mutate(across(units_shipped, \(x) coalesce(x, shipped_qty * unit_size_qty))) |> 
    # group_by(prod_desc, account_name) |> 
    summarize(
        shipments = sum(shipped_qty, na.rm = TRUE),
        quantity = sum(units_shipped, na.rm = TRUE),
        cost = sum(`invoice price`),
        # across(c(shipped_qty, units_shipped, `invoice price`), \(x) sum(x, na.rm = TRUE)),
        # .groups = "drop_last"
        .by = c(prod_desc, account_name)
    ) |> 
    summarize(
        across(c(shipments, quantity, cost), \(x) sum(x, na.rm = TRUE)),
        # .groups = "drop"
        .by = prod_desc
    ) |> 
    mutate(
        dosage_form = case_when(
            str_detect(prod_desc, "SDV|MDV|FTV|VL|CJ|AMP|BAG|INJ|LIDOCAINE|MILRINONE") ~ "iv",  
            str_detect(prod_desc, "NITRO") & str_detect(prod_desc, "SOL") ~ "iv",  
            str_detect(prod_desc, "TAB|CAP|SGC") ~ "po",  
            str_detect(prod_desc, "ORAL|SUS|PWD|SOL") ~ "liq",  
            # str_detect(prod_desc, "PROPRANOLOL|DIGOXIN") & str_detect(prod_desc, "SOL") ~ "iv",  
            str_detect(prod_desc, "PAT|SYSTEM|ONT|SPY") ~ "top",  
            TRUE ~ ""
        ),
        multi_unit = str_detect(prod_desc, "[0-9]{1,2}X[0-9]{1,3}"),
        pkg_qty = if_else(multi_unit, str_extract(prod_desc, "([0-9]{1,2})X"), "1"),
        across(pkg_qty, \(x) str_replace_all(x, "X", "")),
        across(pkg_qty, as.numeric),
        across(
            pkg_qty,
            \(x) case_when(
                str_detect(prod_desc, "PAT 4") ~ 4, # clonidine patch
                str_detect(prod_desc, "PAT 30") ~ 30, # ntg patch
                # str_detect(prod_desc, "SILDENAFIL 10MG-ML SUS 112 ML") ~ 112,
                TRUE ~ x
            )
        )
    ) |> 
    filter(shipments > 0)

x <- raw_df |> 
    filter(str_detect(prod_desc, "ENTRESTO")) |> 
    distinct(prod_desc)

df_totals <- df_prod_sum |> 
    mutate(
        qty = if_else(dosage_form %in% c("iv", "liq", "top"), shipments * pkg_qty, quantity),
        # across(
        #     quantity,
        #     \(x) case_when(
        #         str_detect(prod_desc, "ESMOLOL.*100( )?MG") ~ x / 10,
        #         str_detect(prod_desc, "ESMOLOL.*2500") ~ x / 250,
        #         str_detect(prod_desc, "METOPROLOL.*(SDV|FTV|VL)") ~ x / 5,
        #         # str_detect(prod_desc, "ESMOLOL") ~ x / 25,
        #         str_detect(prod_desc, "PROPRANOLOL 20 MG/5ML SOL 500 ML") ~ x / 500,
        #         str_detect(prod_desc, "PROPRANOLOL 40 MG SOL 500 ML") ~ x / 500,
        #         str_detect(prod_desc, "LABETALOL HCL 100") ~ x / 20,
        #         str_detect(prod_desc, "LABETALOL HCL 20 MG") ~ x / 40,
        #         TRUE ~ x
        #     )
        # ),
        prod_lower = str_to_lower(prod_desc),
        product = case_when(
            str_detect(prod_lower, "metoprol") & str_detect(prod_lower, "tart") ~ "metoprolol_tartrate",
            str_detect(prod_lower, "metoprol") & str_detect(prod_lower, "succ|er tab") ~ "metoprolol_succinate",
            str_detect(prod_lower, "labetalol hcl 100") ~ "labetalol_iv",
            str_detect(prod_lower, "isosorbide d") ~ "isosorbide_dinitrate",
            str_detect(prod_lower, "isoso(rb|br)ide m") ~ "isosorbide_mononitrate",
            str_detect(prod_lower, "^nitro") ~ "nitroglycerin",
            str_detect(prod_lower, "nitroprus") ~ "nitroprusside",
            TRUE ~ word(prod_lower, 1)
        ),
        strength = case_when(
            str_detect(prod_desc, "DILTIAZEM HCL 5 MG-ML SDV 10X10 ML") ~ "50mg",
            str_detect(prod_desc, "DILTIAZEM HCL 5 MG-ML SDV 10X25 ML|DILTIAZEM 5 MG-ML SDV 10X25 ML NVP") ~ "125mg",
            str_detect(prod_desc, "DILTIAZEM HCL 5 MG-ML SDV 10X5 ML|DILTIAZEM 5 MG-ML SDV 10X5 ML NVP") ~ "25mg",
            str_detect(prod_desc, "NITRO-BID") ~ "2%",
            str_detect(prod_desc, "NITRO-BID") ~ "2%",
            str_detect(prod_desc, "24MG/26MG|24/ 26MG") ~ "24/26mg",
            str_detect(prod_desc, "49MG/51MG|49/ 51MG") ~ "49/51mg",
            str_detect(prod_desc, "97MG/103MG|97/ 103MG") ~ "97/103mg",
            TRUE ~ str_extract(prod_lower, "[0-9].*[ ]?m[c]?g")
        ),
        across(strength, \(x) str_replace_all(x, pattern = " ", replacement = ""))
    ) |> 
    group_by(product, strength, dosage_form) |> 
    summarize(
        across(c(qty, cost), sum),
        .groups = "drop"
    ) |> 
    mutate(
        unit_cost = cost / qty,
        across(unit_cost, \(x) round(x, 2))
    )

write.xlsx(df_totals, paste0(f, "final/class_review_cost_data_2024.xlsx"), overwrite = TRUE)
