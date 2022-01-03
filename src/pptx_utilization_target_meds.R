library(tidyverse)
library(readxl)
library(lubridate)
library(officer)
library(mschart)
library(tsibble)
# library(fabletools)

# get data from source
if (Sys.info()['sysname'] == "Windows") {
    p <- "U:/Data/med_tracking/"
    if (!dir.exists(p)) {
        p <- ""
    }
} else if (Sys.info()['sysname'] == "macOS") {
    p <- "/Volumes/brgulbis/Data/med_tracking/"
}

source("src/target_meds_data.R", local = TRUE)

my_theme <- mschart_theme(
    grid_major_line = fp_border(width = 0),
    date_fmt = "[$-en-US]mmm yy;@",
    legend_position = "n"
)

ts_doses <- df_meds %>%
    mutate(across(dose_month, as.Date)) %>%
    group_by(medication, product, dose_month) %>%
    summarize(across(c(patients, doses, quantity), sum, na.rm = TRUE)) %>%
    mutate(month = yearmonth(dose_month)) %>%
    as_tsibble(key = c(medication, product), index = month) %>%
    fill_gaps(doses = 0L) %>%
    as_tibble() %>%
    mutate(
        across(dose_month, ~if_else(is.na(.), as.Date(month), .)),
        fiscal_qtr = yearquarter(dose_month, 7),
        fiscal_year = fiscal_year(fiscal_qtr),
        across(month, ~format(., "%m")),
        fiscal_date = ymd(paste(if_else(as.numeric(month) >= 7, "2019", "2020"), month, "1", sep = "/"))
    )

curr_fy <- max(ts_doses$fiscal_year)
m <- unique(ts_doses$medication)

df_data <- ts_doses %>%
    filter(fiscal_year >= curr_fy - 3) %>%
    mutate(
        across(fiscal_qtr, ~format(., "FY%y")),
        across(fiscal_qtr, ~if_else(!is.na(product), paste(., product, sep = "-"), .))
    ) %>%
    select(medication, dose_month, doses, fiscal_year, fy = fiscal_qtr, fiscal_date)

add_chart <- function(pptx, m, slide_layout = "Blank", slide_master = "Office Theme") {
    lc <- df_data %>%
        filter(medication == m) %>%
        ms_linechart(x = "fiscal_date", y = "doses", group = "fy") %>%
        chart_data_smooth(0) %>%
        chart_ax_x(num_fmt = "[$-en-US]mmm;@") %>%
        chart_ax_y(num_fmt = "#,##0") %>%
        chart_labels(title = paste(m, "utilization"), ylab = "Doses") %>%
        set_theme(my_theme)
    
    pptx %>%
        add_slide(layout = slide_layout, master = slide_master) %>%
        ph_with(value = lc, location = ph_location(left = 0.5, top = 1, width = 9, height = 6))
}


# epo use by product ------------------------------------------------------

p_epo <- ts_doses %>%
    filter(
        medication == "Epoetin Alfa",
        dose_month >= mdy("5/1/2020")
    ) %>%
    ms_barchart(x = "dose_month", y = "doses", group = "product") %>%
    as_bar_stack(percent = TRUE) %>%
    chart_ax_x(num_fmt = "[$-en-US]mmm yy;@") %>%
    chart_ax_y(num_fmt = "#,##0") %>%
    chart_labels(title = "Percent of epoetin doses by product", ylab = "Doses") %>%
    set_theme(my_theme)

# sugammadex breakdown ----------------------------------------------------

title_1m <- format(max(ts_doses$dose_month), "%b, %Y")
title_6m <- format(max(ts_doses$dose_month) - months(5), "%b, %Y")

df_sug <- df_meds %>%
    filter(
        medication == "Sugammadex",
        dose_month > max(ts_doses$dose_month) - months(6)
    )

p_sug_service_1m <- df_sug %>%
    filter(dose_month == max(ts_doses$dose_month)) %>%
    group_by(med_service) %>%
    summarize(across(doses, sum, na.rm = TRUE)) %>%
    arrange(doses) %>%
    top_n(20, doses) %>%
    mutate(across(med_service, str_replace_all, pattern = "&", replacement = "and")) %>%
    mutate(across(med_service, fct_inorder)) %>%
    ms_barchart(x = "med_service", y = "doses") %>%
    chart_settings(var_colors = TRUE, dir = "horizontal", grouping = "standard") %>%
    chart_ax_y(num_fmt = "#,##0") %>%
    chart_labels(title = paste("Top 20 service lines utilizing sugammadex in", title_1m), ylab = "Doses") %>%
    set_theme(my_theme) %>%
    chart_theme(title_y_rot = 0)

p_sug_service_6m <- df_sug %>%
    group_by(med_service) %>%
    summarize(across(doses, sum, na.rm = TRUE)) %>%
    arrange(doses) %>%
    top_n(20, doses) %>%
    mutate(across(med_service, str_replace_all, pattern = "&", replacement = "and")) %>%
    mutate(across(med_service, fct_inorder)) %>%
    ms_barchart(x = "med_service", y = "doses") %>%
    chart_settings(var_colors = TRUE, dir = "horizontal", grouping = "standard") %>%
    chart_ax_y(num_fmt = "#,##0") %>%
    chart_labels(title = paste("Top 20 service lines utilizing sugammadex since", title_6m), ylab = "Doses") %>%
    set_theme(my_theme) %>%
    chart_theme(title_y_rot = 0)

p_sug_loc_1m <- df_sug %>%
    filter(dose_month == max(ts_doses$dose_month)) %>%
    group_by(nurse_unit) %>%
    summarize(across(doses, sum, na.rm = TRUE)) %>%
    arrange(doses) %>%
    top_n(20, doses) %>%
    mutate(across(nurse_unit, fct_inorder)) %>%
    ms_barchart(x = "nurse_unit", y = "doses") %>%
    chart_settings(var_colors = TRUE, dir = "horizontal", grouping = "standard") %>%
    chart_ax_y(num_fmt = "#,##0") %>%
    chart_labels(title = paste("Top 20 nurse units utilizing sugammadex in", title_1m), ylab = "Doses") %>%
    set_theme(my_theme) %>%
    chart_theme(title_y_rot = 0)

p_sug_loc_6m <- df_sug %>%
    group_by(nurse_unit) %>%
    summarize(across(doses, sum, na.rm = TRUE)) %>%
    arrange(doses) %>%
    top_n(20, doses) %>%
    mutate(across(nurse_unit, fct_inorder)) %>%
    ms_barchart(x = "nurse_unit", y = "doses") %>%
    chart_settings(var_colors = TRUE, dir = "horizontal", grouping = "standard") %>%
    chart_ax_y(num_fmt = "#,##0") %>%
    chart_labels(title = paste("Top 20 nurse units utilizing sugammadex since", title_6m), ylab = "Doses") %>%
    set_theme(my_theme) %>%
    chart_theme(title_y_rot = 0)

p_sug_encntr_1m <- df_sug %>%
    filter(dose_month == max(ts_doses$dose_month)) %>%
    group_by(encntr_type) %>%
    summarize(across(doses, sum, na.rm = TRUE)) %>%
    arrange(doses) %>%
    top_n(20, doses) %>%
    mutate(across(encntr_type, fct_inorder)) %>%
    ms_barchart(x = "encntr_type", y = "doses") %>%
    chart_settings(var_colors = TRUE, dir = "horizontal", grouping = "standard") %>%
    chart_ax_y(num_fmt = "#,##0") %>%
    chart_labels(title = paste("Sugammadex use by encounter type in", title_1m), ylab = "Doses") %>%
    set_theme(my_theme) %>%
    chart_theme(title_y_rot = 0)

p_sug_encntr_6m <- df_sug %>%
    group_by(encntr_type) %>%
    summarize(across(doses, sum, na.rm = TRUE)) %>%
    arrange(doses) %>%
    top_n(20, doses) %>%
    mutate(across(encntr_type, fct_inorder)) %>%
    ms_barchart(x = "encntr_type", y = "doses") %>%
    chart_settings(var_colors = TRUE, dir = "horizontal", grouping = "standard") %>%
    chart_ax_y(num_fmt = "#,##0") %>%
    chart_labels(title = paste("Sugammadex use by encounter type since", title_6m), ylab = "Doses") %>%
    set_theme(my_theme) %>%
    chart_theme(title_y_rot = 0)

# powerpoint slides -------------------------------------------------------

slide_layout <- "Blank"
slide_master <- "Office Theme"


add_bars <- function(pptx, value) {
    pptx %>%
        ph_with(value = value, location = ph_location(left = 0.5, top = 1, width = 9, height = 6))
}

pptx <- read_pptx("doc/template.pptx") %>%
    add_slide(layout = "Title Slide", master = slide_master) %>%
    ph_with("Utilization of Target Medications", location = ph_location_label("Title 1")) %>%
    ph_with(
        paste(
            "Data through:",
            format(max(df_meds$dose_month), "%B, %Y"),
            "\nBrian Gulbis, PharmD, BCPS"
        ),
        location = ph_location_label("Subtitle 2")
    )

for (i in 1:length(m)) {
    pptx <- add_chart(pptx, m[i])
}

pptx <- pptx %>%
    add_slide(layout = slide_layout, master = slide_master) %>%
    add_bars(p_epo) %>%
    add_slide(layout = slide_layout, master = slide_master) %>%
    add_bars(p_sug_service_1m) %>%
    add_slide(layout = slide_layout, master = slide_master) %>%
    add_bars(p_sug_service_6m) %>%
    add_slide(layout = slide_layout, master = slide_master) %>%
    add_bars(p_sug_loc_1m) %>%
    add_slide(layout = slide_layout, master = slide_master) %>%
    add_bars(p_sug_loc_6m) %>%
    add_slide(layout = slide_layout, master = slide_master) %>%
    add_bars(p_sug_encntr_1m) %>%
    add_slide(layout = slide_layout, master = slide_master) %>%
    add_bars(p_sug_encntr_6m) 


print(pptx, target = paste0(p, "report/tmc_target_meds/utilization_slides.pptx"))
