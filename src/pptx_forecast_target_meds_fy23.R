library(tidyverse)
library(lubridate)
library(officer)
library(mschart)
library(fabletools)

# set path to data files
f <- set_data_path("med_tracking", "fy23")

if (!dir.exists(f)) {
    stop("Network drive not available.")
}

my_theme <- mschart_theme(
    grid_major_line = fp_border(style = "none"),
    double_fmt = "#,##0",
    date_fmt = "[$-en-US]mmm yyyy;@",
    # main_title = fp_text(color = "#404040", font.size = 24, bold = FALSE, font.family = "Calibri"),
    main_title = fp_text(color = "#595959", font.size = 16, bold = FALSE, font.family = "Calibri"),
    axis_title = fp_text(color = "#595959", font.size = 16, bold = FALSE, font.family = "Calibri"),
    axis_text_x = fp_text(color = "#7F7F7F", font.size = 12, bold = FALSE, font.family = "Calibri"),
    axis_text_y = fp_text(color = "#7F7F7F", font.size = 12, bold = FALSE, font.family = "Calibri"),
    legend_position = "n",
    legend_text = fp_text(color = "#7F7F7F", font.size = 14, bold = FALSE, font.family = "Calibri")
)

slide_title_format <- fp_text(color = "#404040", font.size = 24, bold = FALSE, font.family = "Calibri")

ts_doses <- read_rds(paste0(f, "final/ts_doses.Rds"))
# df_fc_doses_ind <- read_rds(paset0(f, "final/df_fc_doses_ind.Rds"))
df_fc_doses_combo <- read_rds(paste0(f, "final/df_fc_doses_combo.Rds"))

x <- df_fc_doses_combo |>
    mutate(across(date, ~format(., "%b %Y")))

m <- unique(ts_doses$medication)
date_cut <- (floor_date(max(df_fc_doses_combo$date), unit = "year") + months(6)) - years(4)

# m <- "Remdesivir"

add_chart <- function(pptx, m, slide_layout = "Title and Chart", 
                      slide_master = "Office Theme", 
                      title_loc = ph_location_label("Title 1"),
                      chart_loc = ph_location_label("Chart Placeholder 7")) {
    
    df_fcast <- df_fc_doses_combo |>
        filter(medication == m) |>
        select(medication, date, Forecast = .mean, lo_80, hi_80) |>
        pivot_longer(cols = c(Forecast, lo_80, hi_80), names_to = "model")
    
    label_dates <- c(max(df_fc_doses_combo$date) - years(2), max(df_fc_doses_combo$date) - years(1), max(df_fc_doses_combo$date))
    
    df_all <- ts_doses |>
        fill_gaps(doses = 0L, .full = start()) |> 
        as_tibble() |>
        filter(medication == m) |>
        mutate(
            model = "Actual",
            across(dose_month, \(x) coalesce(x, ym(month)))
        ) |>
        select(medication, date = dose_month, model, value = doses) |>
        bind_rows(df_fcast) |>
        filter(date >= date_cut) |> 
        add_row(medication = m, date = max(df_fc_doses_combo$date) + months(1), model = "Forecast") |> 
        mutate(
            across(value, ~if_else(. < 0, 0, .)),
            label = case_when(
                model %in% c("Actual", "Forecast") & date %in% label_dates ~ str_c(format(date, "%b %Y"), format(value, big.mark = ","), sep = "\n"),
                TRUE ~ " "
            ),
            date_chr = format(date, "%b %Y"),
            across(date_chr, as_factor)
        ) 

    slide_title <- fpar(ftext(paste(m, "forecast"), slide_title_format))
    
    group_colors <- c(Actual = "#4472C4", Forecast = "#8FAADC", lo_80 = "#F2F2F2", hi_80 = "#F2F2F2")
    line_styles <- c(Actual = "solid", Forecast = "dashed", lo_80 = "solid", hi_80 = "solid")
    line_widths <- c(Actual = 3.5, Forecast = 2.25, lo_80 = 2.25, hi_80 = 2.25)
    data_labels <- list(
        Actual = fp_text(color = "#4472C4", font.size = 10, font.family = "Calibri"),
        Forecast = fp_text(color = "#8FAADC", font.size = 10, font.family = "Calibri"),
        lo_80 = fp_text(color = "#F2F2F2", font.size = 10, font.family = "Calibri"),
        hi_80 = fp_text(color = "#F2F2F2", font.size = 10, font.family = "Calibri")
    )
    
    lc <- ms_linechart(df_all, x = "date_chr", y = "value", group = "model") |> #, labels = "label"
        chart_settings(style = "line") |>
        # chart_ax_x(num_fmt = "[$-en-US]mmm yy;@", major_tick_mark = "none") |>
        chart_ax_x(major_tick_mark = "none") |> 
        chart_ax_y(num_fmt = "#,##0", major_tick_mark = "none", limit_min = 0) |>
        chart_labels(title = "Doses") |> 
        chart_data_fill(values = group_colors) |>
        chart_data_stroke(values = group_colors) |>
        chart_data_line_style(values = line_styles) |>
        chart_data_line_width(values = line_widths) |>
        chart_data_labels(position = "t", num_fmt = "#,##0", separator = "\n") |> 
        chart_labels_text(values = data_labels) |>
        set_theme(my_theme) 
    
    pptx |>
        add_slide(layout = slide_layout, master = slide_master) |>
        ph_with(value = slide_title, location = title_loc) |>
        ph_with(value = lc, location = chart_loc) 
}

date_range <- filter(df_fc_doses_combo, medication == "Albumin") 
    
pptx <- read_pptx("doc/template.pptx") |>
    add_slide(layout = "Title Slide", master = "Office Theme") |>
    ph_with("Forecast for Target Medications", location = ph_location_label("Title 1")) |>
    ph_with(
        paste(
            format(min(date_range$date), "%B, %Y"),
            "to",
            format(max(date_range$date), "%B, %Y"),
            "\nBrian Gulbis, PharmD, BCPS"
        ),
        location = ph_location_label("Subtitle 2")
    )

for (i in 1:length(m)) {
    pptx <- add_chart(pptx, m[i])
}

print(pptx, target = paste0(f, "report/forecast_slides.pptx"))
    
    
