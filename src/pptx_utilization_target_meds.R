library(tidyverse)
library(readxl)
library(lubridate)
library(officer)
library(mschart)
library(tsibble)
# library(fabletools)

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

my_theme <- mschart_theme(
    grid_major_line = fp_border(style = "none"),
    double_fmt = "#,##0",
    date_fmt = "[$-en-US]mmm yyyy;@",
    # main_title = fp_text(color = "#404040", font.size = 24, bold = FALSE, font.family = "Calibri"),
    main_title = fp_text(color = "#595959", font.size = 16, bold = FALSE, font.family = "Calibri"),
    axis_title = fp_text(color = "#595959", font.size = 16, bold = FALSE, font.family = "Calibri"),
    axis_text = fp_text(color = "#7F7F7F", font.size = 14, bold = FALSE, font.family = "Calibri"),
    legend_position = "n",
    legend_text = fp_text(color = "#7F7F7F", font.size = 14, bold = FALSE, font.family = "Calibri")
)

slide_title_format <- fp_text(color = "#404040", font.size = 24, bold = FALSE, font.family = "Calibri")

ts_doses <- df_meds |>
    mutate(across(dose_month, as.Date)) |>
    group_by(medication, product, dose_month) |>
    summarize(across(c(patients, doses, quantity), sum, na.rm = TRUE), .groups = "drop_last") |>
    mutate(month = yearmonth(dose_month)) |>
    as_tsibble(key = c(medication, product), index = month) |>
    fill_gaps(doses = 0L) |>
    as_tibble() |>
    mutate(
        across(dose_month, ~if_else(is.na(.), as.Date(month), .)),
        fiscal_qtr = yearquarter(dose_month, 7),
        fiscal_year = fiscal_year(fiscal_qtr),
        across(month, ~format(., "%m")),
        fiscal_date = ymd(paste(if_else(as.numeric(month) >= 7, "2019", "2020"), month, "1", sep = "/"))
    )

curr_fy <- max(ts_doses$fiscal_year)
m <- unique(ts_doses$medication)

df_data <- ts_doses |>
    filter(fiscal_year >= curr_fy - 3) |>
    mutate(
        across(fiscal_qtr, ~format(., "FY%y")),
        across(fiscal_qtr, ~if_else(!is.na(product), paste0(., " (", str_sub(product, 1, 1), ")"), .)),
        group_color = case_when(
            fiscal_year == curr_fy ~ "#000000",
            fiscal_year == curr_fy - 1 ~ "#A6CEE3",
            fiscal_year == curr_fy - 2 ~ "#FDBF6F",
            fiscal_year == curr_fy - 3 ~ "#B2DF8A"
        ),
        line_style = if_else(product == "Procrit", "dashed", "solid", "solid"),
        line_width = if_else(fiscal_year == curr_fy, 3.5, 2.25)
    ) |>
    select(medication, dose_month, doses, fiscal_year, fy = fiscal_qtr, fiscal_date, group_color, line_style, line_width) |> 
    add_row(medication = "Spacer", doses = 0, fy = "FY", fiscal_date = mdy("6/1/2019"), group_color = "#FFFFFF", line_style = "none", line_width = 0) |> 
    add_row(medication = "Spacer", doses = 0, fy = "FY", fiscal_date = mdy("7/1/2020"), group_color = "#FFFFFF", line_style = "none", line_width = 0)

# x <- df_data |>
#     filter(medication == "Acetaminophen IV") 
# 
# y <- x |>
#     distinct(fy, line_style) |>
#     deframe()
# 
# l <- map(y, ~fp_text(color = .x, font.size = 14, font.family = "Calibri"))
# 

add_chart <- function(pptx, m, 
                      slide_layout = "Title and Chart", 
                      slide_master = "Office Theme", 
                      title_loc = ph_location_label("Title 1"),
                      chart_loc = ph_location_label("Chart Placeholder 7")) {
    # ph_location(left = 0.5, top = 1, width = 9, height = 6)
    
    df <- filter(df_data, medication %in% c(m, "Spacer")) 

    group_colors <- df |> 
        distinct(fy, group_color) |> 
        deframe()
    
    line_styles <- df |> 
        distinct(fy, line_style) |> 
        deframe()

    line_widths <- df |> 
        distinct(fy, line_width) |> 
        deframe()

    data_labels <- map(group_colors, ~fp_text(color = .x, font.size = 14, font.family = "Calibri"))
    
    lc <- df |>
        ms_linechart(x = "fiscal_date", y = "doses", group = "fy") |>
        chart_data_smooth(0) |>
        chart_settings(style = "line") |>
        chart_ax_x(num_fmt = "[$-en-US]mmm;@", major_tick_mark = "none") |> # cross_between = "midCat", major_tick_mark = "in"
        chart_ax_y(num_fmt = "#,##0", major_tick_mark = "none") |>
        chart_labels(title = "Doses") |>
        chart_data_fill(values = group_colors) |>
        chart_data_stroke(values = group_colors) |>
        chart_data_line_style(values = line_styles) |>
        chart_data_line_width(values = line_widths) |>
        chart_labels_text(values = data_labels) |>
        set_theme(my_theme) |> 
        chart_theme(axis_ticks_y = fp_border(color = "#FFFFFF"))
    
    cover_top <- 6.7
    
    if (m == "Epoetin Alfa") {
        lc <- chart_labels(lc, title = "Doses", xlab = "(P) = Procrit, (R) = Retacrit")
        cover_top <- 6.4
    }
    
    slide_title <- fpar(ftext(paste(m, "utilization"), slide_title_format))
    
    g <- ggplot() +
        annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = "white", color = "white") +
        theme_void()
    
    pptx |>
        add_slide(layout = slide_layout, master = slide_master) |>
        ph_with(value = slide_title, location = title_loc) |>
        ph_with(value = lc, location = chart_loc) |> 
        ph_with(value = g, location = ph_location(left = 1, top = cover_top, width = 0.6, height = 0.3)) |> 
        ph_with(value = g, location = ph_location(left = 8.8, top = cover_top, width = 0.5, height = 0.3))
    
}


# epo use by product ------------------------------------------------------

fill_color <- "#1F78B4"

p_epo <- ts_doses |>
    filter(
        medication == "Epoetin Alfa",
        dose_month >= mdy("5/1/2020")
    ) |>
    ms_barchart(x = "dose_month", y = "doses", group = "product") |>
    as_bar_stack(percent = TRUE) |>
    chart_ax_x(num_fmt = "[$-en-US]mmm yy;@", major_tick_mark = "none") |>
    chart_ax_y(num_fmt = "0%%", major_tick_mark = "none") |>
    chart_labels(title = "Doses (%)") |>
    chart_data_fill(values = c(Retacrit = "#FFFFFF", Procrit = fill_color)) |> 
    chart_data_stroke(values = c(Retacrit = "#FFFFFF", Procrit = fill_color)) |> 
    set_theme(my_theme) |> 
    chart_theme(axis_ticks_y = fp_border(color = "#FFFFFF"))

# sugammadex breakdown ----------------------------------------------------

title_1m <- format(max(ts_doses$dose_month), "%B, %Y")
title_6m <- format(max(ts_doses$dose_month) - months(5), "%B, %Y")

df_sug <- df_meds |>
    filter(
        medication == "Sugammadex",
        dose_month > max(ts_doses$dose_month) - months(6)
    )

add_bars <- function(df, x, .title, filter = TRUE) {
    x <- enquo(x)
    
    if (filter) {
        df <- filter(df, dose_month == max(ts_doses$dose_month))
    }
    
    if (rlang::as_name(x) == "encntr_type") {
        axis_font <- 14
    } else {
        axis_font <- 12
    }
    
    df |> 
        group_by(!!x) |> 
        summarize(across(doses, sum, na.rm = TRUE)) |>
        arrange(doses) |>
        top_n(20, doses) |>
        mutate(across(!!x, str_replace_all, pattern = "&", replacement = "and")) |>
        mutate(across(!!x, fct_inorder)) |>
        ms_barchart(x = rlang::as_name(x), y = "doses") |>
        chart_settings(var_colors = TRUE, dir = "horizontal", grouping = "standard") |>
        chart_ax_x(major_tick_mark = "none") |> 
        chart_ax_y(num_fmt = "#,##0", major_tick_mark = "none") |>
        chart_labels(title = .title, ylab = "Doses") |>
        chart_data_fill(fill_color) |> 
        chart_data_stroke(fill_color) |> 
        set_theme(my_theme) |> 
        chart_theme(
            axis_text_x = fp_text(color = "#7F7F7F", font.size = axis_font, bold = FALSE, font.family = "Calibri"),
            axis_ticks_x = fp_border(color = "#FFFFFF"),
            title_y_rot = 0
        )
} 

p_sug_service_1m <- add_bars(df_sug, med_service, title_1m)
    
p_sug_service_6m <- add_bars(df_sug, med_service, paste("Since", title_6m), FALSE)
    
p_sug_loc_1m <- add_bars(df_sug, nurse_unit, title_1m)
    
p_sug_loc_6m <- add_bars(df_sug, nurse_unit, paste("Since", title_6m), FALSE)
    
p_sug_encntr_1m <- add_bars(df_sug, encntr_type, title_1m)
    
p_sug_encntr_6m <- add_bars(df_sug, encntr_type, paste("Since", title_6m), FALSE)

# powerpoint slides -------------------------------------------------------


# slide_layout <- "Blank"
slide_layout <- "Title and Chart"
slide_master <- "Office Theme"

title_loc <- ph_location_label("Title 1")
chart_loc <- ph_location_label("Chart Placeholder 7")

# test_pptx <- read_pptx("doc/template.pptx")
# layout_properties(test_pptx, layout = slide_layout)
# layout_properties(test_pptx, layout = "Title Slide")


pptx <- read_pptx("doc/template.pptx") |>
    add_slide(layout = "Title Slide", master = slide_master) |>
    ph_with("Utilization of Target Medications", location = ph_location_label("Title 1")) |>
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

pptx <- pptx |>
    add_slide(layout = slide_layout, master = slide_master) |>
    ph_with(value = fpar(ftext("Percent of epoetin doses administered with Procrit", slide_title_format)), location = title_loc) |>
    ph_with(value = p_epo, location = chart_loc) |> 
    add_slide(layout = slide_layout, master = slide_master) |>
    ph_with(value = fpar(ftext("Top 20 service lines at time of sugammadex administration", slide_title_format)), location = title_loc) |>
    ph_with(value = p_sug_service_1m, location = chart_loc) |> 
    add_slide(layout = slide_layout, master = slide_master) |>
    ph_with(value = fpar(ftext("Top 20 service lines at time of sugammadex administration", slide_title_format)), location = title_loc) |>
    ph_with(value = p_sug_service_6m, location = chart_loc) |> 
    add_slide(layout = slide_layout, master = slide_master) |>
    ph_with(value = fpar(ftext("Top 20 nursing units at time of sugammadex administration", slide_title_format)), location = title_loc) |>
    ph_with(value = p_sug_loc_1m, location = chart_loc) |> 
    add_slide(layout = slide_layout, master = slide_master) |>
    ph_with(value = fpar(ftext("Top 20 nursing units at time of sugammadex administration", slide_title_format)), location = title_loc) |>
    ph_with(value = p_sug_loc_6m, location = chart_loc) |> 
    add_slide(layout = slide_layout, master = slide_master) |>
    ph_with(value = fpar(ftext("Encounter type at time of sugammadex administration", slide_title_format)), location = title_loc) |>
    ph_with(value = p_sug_encntr_1m, location = chart_loc) |> 
    add_slide(layout = slide_layout, master = slide_master) |>
    ph_with(value = fpar(ftext("Encounter type at time of sugammadex administration", slide_title_format)), location = title_loc) |>
    ph_with(value = p_sug_encntr_6m, location = chart_loc) |> 
    move_slide(21, 9) |> 
    move_slide(21, 20)

print(pptx, target = paste0(p, "report/tmc_target_meds/utilization_slides.pptx"))
