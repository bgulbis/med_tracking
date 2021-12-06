library(tidyverse)
library(lubridate)
library(officer)
library(mschart)
library(fabletools)

p <- "/Volumes/brgulbis/Data/med_tracking/"

ts_doses <- read_rds(paste0(p, "final/ts_doses.Rds"))
# df_fc_doses_ind <- read_rds(paset0(p, "final/df_fc_doses_ind.Rds"))
df_fc_doses_combo <- read_rds(paste0(p, "final/df_fc_doses_combo.Rds"))

x <- df_fc_doses_combo %>%
    mutate(across(date, ~format(., "%b %Y")))

m <- unique(ts_doses$medication)
date_cut <- (floor_date(max(df_fc_doses_combo$date), unit = "year") + months(6)) - years(4)

add_chart <- function(pptx, m, slide_layout = "Blank", slide_master = "Office Theme") {
    df_fcast <- df_fc_doses_combo %>%
        filter(medication == m) %>%
        select(medication, date, Forecast = .mean, lo_80, hi_80) %>%
        pivot_longer(cols = c(Forecast, lo_80, hi_80), names_to = "model")
    
    df_all <- ts_doses %>%
        as_tibble() %>%
        filter(medication == m) %>%
        mutate(model = "Actual") %>%
        select(medication, date = dose_month, model, value = doses) %>%
        bind_rows(df_fcast) %>%
        filter(date >= date_cut)
    
    lc <- ms_linechart(df_all, x = "date", y = "value", group = "model") %>%
        chart_ax_x(num_fmt = "[$-en-US]mmm yy;@") %>%
        chart_ax_y(num_fmt = "#,##0") %>%
        chart_labels(title = paste(m, "forecast"), ylab = "Doses") 
    
    pptx %>%
        add_slide(layout = slide_layout, master = slide_master) %>%
        ph_with(value = lc, location = ph_location(left = 0.5, top = 1, width = 9, height = 6))
}

slide_layout <- "Blank"
slide_master <- "Office Theme"
my_theme <- mschart_theme(
    grid_major_line = fp_border(width = 0),
    date_fmt = "[$-en-US]mmm yyyy;@",
    legend_position = "n"
)

pptx <- read_pptx("doc/template.pptx") %>%
    set_theme(my_theme) %>%
    add_slide(layout = "Title Slide", master = slide_master) %>%
    ph_with("Forecast for Target Medications", location = ph_location_label("Title 1")) %>%
    ph_with(
        paste(
            format(min(df_fc_doses_combo$date), "%B, %Y"),
            "to",
            format(max(df_fc_doses_combo$date), "%B, %Y"),
            "\nBrian Gulbis, PharmD, BCPS"
        ),
        location = ph_location_label("Subtitle 2")
    )

for (i in 1:length(m)) {
    pptx <- add_chart(pptx, m[i])
}

print(pptx, target = paste0(p, "report/tmc_target_meds/forecast_slides.pptx"))
    
    
