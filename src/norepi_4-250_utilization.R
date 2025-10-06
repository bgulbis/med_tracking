library(tidyverse)
library(lubridate)
library(mbohelpr)
library(openxlsx)

f <- set_data_path("med_tracking")

raw_ne <- read_csv(paste0(f, "raw/norepi_4-250_utilization.csv")) |> 
    rename_all(str_to_lower) |> 
    mutate(
        across(fin, as.character),
        across(c(med_datetime, med_month), ymd_hms)
    ) 
    
df_ne <- raw_ne |> 
    filter(
        item_id == 2866373,
        dose_quantity == 1
    ) |> 
    select(-encntr_id, -item_id)

write.xlsx(df_ne, paste0(f, "final/norepi_4mg-250ml_utilization.xlsx"), overwrite = TRUE)
