library(tidyverse)
library(lubridate)
library(mbohelpr)

data_apap <- get_data("data/tidy/sidney", "med-utilization") %>%
    filter(
        medication == "acetaminophen",
        event_date >= mdy("6/1/2019")
    ) %>%
    select(facility, event_date, doses) %>%
    group_by(event_date, facility) %>%
    summarize_at("doses", sum, na.rm = TRUE) %>%
    spread(event_date, doses) %>%
    mutate(pct = 1 - (`2019-07-01` / `2019-06-01`))
    
