library(mbohelpr)
library(tidyverse)
library(lubridate)
library(officer)
library(mschart)
library(forecast)
library(timetk)
library(sweep)

tz_locale <- locale(tz = "US/Central")
floor_unit <- "month"

campus <- c(
    "HC Childrens",
    "HH HERMANN",
    "HH Radiology",
    "HH Rehab",
    "HH Trans Care"
)

make_df <- function(df, ...) {
    cnt <- enquos(...)
    
    df %>%
        mutate(
            med_date = floor_date(clinical_event_datetime, unit = floor_unit),
            fiscal_year = year(med_date %m+% months(6)),
            month_plot = month(med_date, label = TRUE, abbr = TRUE)
        ) %>%
        count(fiscal_year, month_plot, med_date, !!!cnt) %>%
        mutate_at("fiscal_year", as_factor) %>%
        mutate_at(
            "month_plot", 
            factor, 
            ordered = TRUE, 
            levels = c(
                "Jul", 
                "Aug",
                "Sep",
                "Oct",
                "Nov",
                "Dec",
                "Jan",
                "Feb",
                "Mar",
                "Apr",
                "May",
                "Jun"
            )
        ) %>%
        arrange(med_date)
}

make_forecast <- function(ts, h = 12) {
    forecast::auto.arima(ts, lambda = "auto") %>%
        forecast::forecast(h) %>%
        sweep::sw_sweep(timetk_idx = TRUE) %>%
        dplyr::mutate_at(
            "index",
            lubridate::floor_date, 
            unit = floor_unit
        ) %>%
        dplyr::mutate_at("key", stringr::str_to_title)
}

plot_utilization <- function(df) {
    df %>%
        ggplot(aes(x = month_plot, y = n, group = fiscal_year)) +
        geom_line(
            aes(color = fiscal_year, alpha = fiscal_year), 
            size = 2
        ) +
        xlab(NULL) +
        ylab("Monthly Doses") +
        scale_color_manual(
            "Fiscal Year", 
            values = c("#377eb8", "#4daf4a", "black")
        ) +
        scale_alpha_manual(
            "Fiscal Year", 
            values = c(0.4, 0.4, 1)
        ) +
        expand_limits(y = 0) +
        theme_bg() +
        theme(
            axis.text.x = element_text(vjust = 0.1),
            legend.position = "bottom"
        )
}

plot_forecast <- function(df) {
    df %>%
        group_by(key) %>%
        mutate(
            label = if_else(
                index == last(index), 
                as.integer(round(n, 0)), 
                NA_integer_)
        ) %>%
        ggplot(aes(x = index, y = n)) +
        geom_ribbon(
            aes(ymin = lo.95, ymax = hi.95), 
            fill = "grey85",
            alpha = 0.5
        ) +
        geom_ribbon(
            aes(ymin = lo.80, ymax = hi.80),
            fill = "grey75", 
            alpha = 0.5
        ) +
        geom_line(aes(linetype = key, size = key)) +
        geom_point(
            aes(y = label, size = key, fill = key), 
            shape = 21, 
            color = "black", 
            show.legend = FALSE
        ) +
        geom_text_repel(
            aes(label = label), 
            direction = "y",
            point.padding = 1
        ) +
        scale_linetype_manual(NULL, values = c("solid", "dashed")) +
        scale_size_manual(NULL, values = c(1.75, 1)) +
        scale_fill_manual(NULL, values = c("white", "white")) +
        scale_x_datetime(
            NULL, 
            breaks = seq(
                mdy("1/1/2016", tz = "US/Central"), 
                now() + years(1), 
                by = "6 months"
            ),
            date_labels = "%b %Y"
        ) +
        ylab("Monthly Doses") +
        expand_limits(y = 0) +
        theme_bg() +
        theme(
            axis.text.x = element_text(vjust = 0.1),
            legend.position = "bottom"
        )
}

# albumin ----------------------------------------------

dir_data <- "data/tidy/albumin"

data_albumin <- get_data(dir_data, "albumin") %>%
    mutate(event_month = floor_date(event_date, "month")) %>%
    group_by(event_month) %>%
    summarize_at("doses", sum, na.rm = TRUE)

ts_albumin <- tk_ts(data_albumin, silent = TRUE)
mod_albumin <- auto.arima(ts_albumin, lambda = "auto")
fcast_albumin <- forecast(mod_albumin, 12) 
swp_albumin <- sw_sweep(fcast_albumin, timetk_idx = TRUE)
# fcast_albumin <- make_forecast(ts_albumin) 

# g_albumin <- plot_utilization(df_albumin)
# g_albumin_fcast <- plot_forecast(fcast_albumin)
# g_albumin_fcast

# l <- df_albumin %>%
#     mutate_at("med_date", as_date) %>%
#     ms_linechart(x= "med_date", y = "n") %>%
#     chart_ax_x(num_fmt = "mmm") %>%
#     chart_data_labels(num_fmt = "0", position = "t", show_val = TRUE) %>% 
#     chart_labels(title = "Albumin utilization", xlab = "Month", ylab = "Doses") %>% 
#     chart_theme(
#         grid_major_line_x = fp_border(style = "none"),
#         grid_major_line_y = fp_border(style = "none"),
#         legend_position = "n"
#     )

l_fcast <- swp_albumin %>%
    mutate_at("index", as_date) %>%
    # gather("key", "doses", lo.80:hi.95)
    # mutate_at("n", round, digits = 0) %>%
    ms_linechart(x= "index", y = "doses", group = "key") 
    # chart_ax_x(num_fmt = "mmm") %>%
    # chart_data_labels(num_fmt = "0", position = "t", show_val = TRUE) %>% 
    # chart_labels(title = "Albumin forecast", xlab = "Month", ylab = "Doses per month") %>% 
    # chart_data_symbol(values = c(Actual = "none", Forecast = "circle")) %>%
    # chart_data_line_width(values = c(Actual = 3.5, Forecast = 2.25)) %>%
    # chart_theme(
    #     grid_major_line_x = fp_border(style = "none"),
    #     grid_major_line_y = fp_border(style = "none"),
    #     legend_position = "n"
    # )


# powerpoint -------------------------------------------

slide_layout <- "Title and Content"
slide_master <- "Office Theme"
title_size <- fp_text(font.size = 32)

# layout_properties(read_pptx(), layout = "Title Slide", master = slide_master)

read_pptx(path = "doc/template.pptx") %>%
    # add_slide(layout = slide_layout, master = slide_master) %>%
    # ph_with("Albumin", location = ph_location_type("title")) %>%
    # ph_with_chart(l, type = "body") %>%
    add_slide(layout = "Blank", master = slide_master) %>%
    # ph_with("Albumin forecast", location = ph_location_type("title")) %>%
    ph_with_chart_at(l_fcast, 1, 1, 8, 6) %>%
    print(target = "doc/mscharts.pptx")

# read_pptx() %>%
#     add_slide(layout = slide_layout, master = slide_master) %>%
#     ph_with("Albumin", location = ph_location_type("title")) %>%
#     ph_with_vg(ggobj = g_stacked, type = "body") %>%
#     print(target = "figs/stacked_fy.pptx")

