library(tidyverse)
library(lubridate)
library(officer)
library(themebg)
library(ggrepel)
library(rvg)
library(mschart)
library(forecast)
library(timetk)
library(sweep)

tz_locale <- locale(tz = "US/Central")
floor_unit <- "month"

data_month <- floor_date(rollback(now(), FALSE, FALSE), unit = "month")
fy <- year(data_month %m+% months(6))

col_pal <- c("#377eb8", "#4daf4a")

campus <- c(
    "HC Childrens",
    "HH Clinics",
    "HH HERMANN",
    "HH Radiology",
    "HH Rehab",
    "HH Trans Care"
)

get_data <- function(path, pattern, col_types = NULL) {
    f <- list.files(path, pattern, full.names = TRUE)
    
    n <- f %>% 
        purrr::map_int(~ nrow(data.table::fread(.x, select = 1L))) 
    
    f[n > 0] %>%
        purrr::map_df(
            readr::read_csv,
            locale = tz_locale,
            col_types = col_types
        ) %>%
        rename_all(stringr::str_to_lower)
}

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
    forecast::auto.arima(ts, lambda = 0) %>%
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
        geom_line(aes(color = key, linetype = key), size = 2) +
        scale_color_manual(NULL, values = c("black", "blue")) +
        scale_linetype_manual(NULL, values = c("solid", "dashed")) +
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

# acetaminophen ----------------------------------------

dir_data <- "data/tidy/acetaminophen"

fy_months <- c(
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

data_apap_events <- get_data(dir_data, "apap_events") %>%
    distinct() 

data_apap_orders <- get_data(dir_data, "apap_orders")

month_end <- floor_date(
    max(data_apap_events$clinical_event_datetime), 
    unit = "month"
)

df_apap <- data_apap_events %>%
    filter(
        clinical_event_datetime <= month_end,
        facility_event %in% campus,
        (is.na(admin_event) | admin_event == "Administered")
    ) %>%
    mutate(
        med_month = floor_date(clinical_event_datetime, unit = floor_unit),
        fiscal_year = year(med_month %m+% months(6)),
        month_plot = month(med_month, label = TRUE, abbr = TRUE)
    ) 
    
n_apap_doses <- df_apap %>%
    count(fiscal_year, month_plot, med_month, name = "doses") %>%
    mutate_at("fiscal_year", as_factor) %>%
    mutate_at(
        "month_plot", 
        factor, 
        ordered = TRUE, 
        levels = fy_months
    ) %>%
    arrange(med_month)

n_apap_pts <- df_apap %>%
    distinct(encounter_id, med_month) %>%
    count(med_month, name = "patients") 

df_apap_n <- n_apap_doses %>%
    left_join(n_apap_pts, by = "med_month") %>%
    gather("key", "value", doses, patients) %>%
    group_by(key) %>%
    mutate(
        label = if_else(
            med_month == last(med_month),
            str_to_sentence(key),
            NA_character_
        )
    )

ts_apap <- tk_ts(df_apap)

fcast_apap <- make_forecast(ts_apap)
g_apap <- plot_utilization(df_apap)
g_apap_fcast <- plot_forecast(fcast_apap)

# graphs -----------------------------------------------

cutoff <- 15L

g_utilization_fy <- df_apap_n %>%
    filter(fiscal_year == fy) %>%
    ggplot(aes(x = med_month, y = value, color = key)) +
    geom_line(size = 1) +
    geom_smooth(method = "lm", size = 0.5, linetype = "dashed", se = FALSE) +
    geom_text_repel(aes(label = label), nudge_y = -1) +
    scale_x_datetime(NULL, date_breaks = "1 month", date_labels = "%b %y") +
    ylab("Number") +
    scale_color_manual(NULL, values = col_pal) +
    theme_bg() +
    theme(legend.position = "None")

g_utilization_all <- df_apap_n %>%
    ggplot(aes(x = med_month, y = value, color = key)) +
    geom_line(size = 1) +
    geom_smooth(method = "lm", size = 0.5, linetype = "dashed", se = FALSE) +
    geom_text_repel(aes(label = label), nudge_y = -1) +
    scale_x_datetime(NULL, date_breaks = "3 months", date_labels = "%b %y") +
    ylab("Number") +
    scale_color_manual(NULL, values = col_pal) +
    theme_bg() +
    theme(legend.position = "None")

g_units <- df_apap %>%
    filter(med_month == data_month) %>%
    add_count(nurse_unit, prn_dose, name = "dose_type") %>%
    add_count(nurse_unit, name = "doses") %>%
    arrange(doses) %>%
    mutate_at("nurse_unit", fct_inorder) %>%
    # mutate_at("nurse_unit", fct_rev) %>%
    distinct(nurse_unit, prn_dose, dose_type, doses) %>%
    filter(doses >= cutoff) %>%
    ggplot(aes(x = nurse_unit, y = dose_type, fill = prn_dose)) +
    geom_col() +
    xlab(NULL) +
    ylab("Number of doses") +
    scale_fill_manual(NULL, values = col_pal, labels = c("Scheduled", "PRN")) +
    coord_flip() +
    theme_bg() 

g_median <- df_apap %>%
    filter(med_month == data_month) %>%
    count(nurse_unit, encounter_id) %>%
    group_by(nurse_unit) %>%
    summarize_at("n", median, na.rm = TRUE) %>%
    arrange(desc(n)) %>%
    mutate_at("nurse_unit", fct_inorder) %>%
    mutate_at("nurse_unit", fct_rev) %>%
    filter(n > 1) %>%
    ggplot(aes(x = nurse_unit, y = n)) +
    geom_col() +
    xlab(NULL) +
    ylab("Median doses per patient") +
    coord_flip() +
    theme_bg() 

g_po_trend <- df_apap %>%
    filter(fiscal_year == fy) %>%
    group_by(med_month) %>%
    summarize(
        doses = n(),
        po_pct = sum(po_med, na.rm = TRUE) / doses
    ) %>%
    ggplot(aes(x = med_month, y = po_pct)) +
    geom_line(size = 1) +
    geom_smooth(method = "lm", size = 0.5, linetype = "dashed", se = FALSE) +
    scale_x_datetime(NULL, date_breaks = "1 month", date_labels = "%b %y") +
    scale_y_continuous("Doses (%)", labels = scales::percent) +
    coord_cartesian(ylim = c(0, 1)) +
    theme_bg() +
    theme(legend.position = "None")


# powerpoint -------------------------------------------

slide_layout <- "Title and Content"
slide_master <- "Office Theme"
title_size <- fp_text(font.size = 32)

# layout_properties(read_pptx(), layout = "Title Slide", master = slide_master)

read_pptx() %>%
    add_slide(layout = "Title Slide", master = slide_master) %>%
    ph_with_text(type = "ctrTitle", str = "Utilization of Target Medications") %>%
    ph_with_text(
        type = "subTitle", 
        str = paste0(
            "Data through: ", 
            format(month_end, "%B %Y"),
            "\nBrian Gulbis, PharmD, BCPS"
        )
    ) %>%
    add_slide(layout = slide_layout, master = slide_master) %>%
    ph_with("IV Acetaminophen", location = ph_location_type("title")) %>%
    ph_with_vg(ggobj = g_apap, type = "body") %>%
    add_slide(layout = slide_layout, master = slide_master) %>%
    ph_with("Albumin", location = ph_location_type("title")) %>%
    ph_with_vg(ggobj = g_albumin, type = "body") %>%
    add_slide(layout = slide_layout, master = slide_master) %>%
    ph_with("Bupivacaine Liposome", location = ph_location_type("title")) %>%
    ph_with_vg(ggobj = g_bupiv, type = "body") %>%
    add_slide(layout = slide_layout, master = slide_master) %>%
    ph_with("Calcitonin", location = ph_location_type("title")) %>%
    ph_with_vg(ggobj = g_calctn, type = "body") %>%
    add_slide(layout = slide_layout, master = slide_master) %>%
    ph_with("Eculizumab (Outpatient)", location = ph_location_type("title")) %>%
    ph_with_vg(ggobj = g_eculiz_out, type = "body") %>%
    add_slide(layout = slide_layout, master = slide_master) %>%
    ph_with("Eculizumab (Inpatient)", location = ph_location_type("title")) %>%
    ph_with_vg(ggobj = g_eculiz_in, type = "body") %>%
    add_slide(layout = slide_layout, master = slide_master) %>%
    ph_with("Isoproterenol", location = ph_location_type("title")) %>%
    ph_with_vg(ggobj = g_isoprot, type = "body") %>%
    add_slide(layout = slide_layout, master = slide_master) %>%
    ph_with("IVIG", location = ph_location_type("title")) %>%
    ph_with_vg(ggobj = g_ivig, type = "body") %>%
    add_slide(layout = slide_layout, master = slide_master) %>%
    ph_with("Pegfilgrastim (Outpatient)", location = ph_location_type("title")) %>%
    ph_with_vg(ggobj = g_pegf_out, type = "body") %>%
    add_slide(layout = slide_layout, master = slide_master) %>%
    ph_with("Sugammadex", location = ph_location_type("title")) %>%
    ph_with_vg(ggobj = g_sug, type = "body") %>%
    add_slide(layout = slide_layout, master = slide_master) %>%
    ph_with("Ceftaroline", location = ph_location_type("title")) %>%
    ph_with_vg(ggobj = g_ceftar, type = "body") %>%
    add_slide(layout = slide_layout, master = slide_master) %>%
    ph_with("Ceftazidime-Avibactam", location = ph_location_type("title")) %>%
    ph_with_vg(ggobj = g_avycaz, type = "body") %>%
    add_slide(layout = slide_layout, master = slide_master) %>%
    ph_with("Ceftolozane-Tazobactam", location = ph_location_type("title")) %>%
    ph_with_vg(ggobj = g_zerbaxa, type = "body") %>%
    add_slide(layout = slide_layout, master = slide_master) %>%
    ph_with("Daptomycin", location = ph_location_type("title")) %>%
    ph_with_vg(ggobj = g_dapto, type = "body") %>%
    add_slide(layout = slide_layout, master = slide_master) %>%
    ph_with("Ertapenem", location = ph_location_type("title")) %>%
    ph_with_vg(ggobj = g_ertap, type = "body") %>%
    print(
        target = paste0(
            "figs/target_med_utilization_",
            format(month_end, "%Y-%m"),
            ".pptx"
        )
    )

read_pptx() %>%
    add_slide(layout = "Title Slide", master = slide_master) %>%
    ph_with_text(type = "ctrTitle", str = "Forecast for Target Medications") %>%
    ph_with_text(
        type = "subTitle", 
        str = paste0(
            format(month_end + months(1), "%B %Y"),
            " to ",
            format(month_end + months(12), "%B %Y"),
            "\nBrian Gulbis, PharmD, BCPS"
        )
    ) %>%
    add_slide(layout = slide_layout, master = slide_master) %>%
    ph_with("IV Acetaminophen", location = ph_location_type("title")) %>%
    ph_with_vg(ggobj = g_apap_fcast, type = "body") %>%
    add_slide(layout = slide_layout, master = slide_master) %>%
    ph_with("Albumin", location = ph_location_type("title")) %>%
    ph_with_vg(ggobj = g_albumin_fcast, type = "body") %>%
    add_slide(layout = slide_layout, master = slide_master) %>%
    ph_with("Bupivacaine Liposome", location = ph_location_type("title")) %>%
    ph_with_vg(ggobj = g_bupiv_fcast, type = "body") %>%
    add_slide(layout = slide_layout, master = slide_master) %>%
    ph_with("Calcitonin", location = ph_location_type("title")) %>%
    ph_with_vg(ggobj = g_calctn_fcast, type = "body") %>%
    add_slide(layout = slide_layout, master = slide_master) %>%
    ph_with("Eculizumab (Outpatient)", location = ph_location_type("title")) %>%
    ph_with_vg(ggobj = g_eculiz_fcast_out, type = "body") %>%
    add_slide(layout = slide_layout, master = slide_master) %>%
    ph_with("Eculizumab (Inpatient)", location = ph_location_type("title")) %>%
    ph_with_vg(ggobj = g_eculiz_fcast_in, type = "body") %>%
    add_slide(layout = slide_layout, master = slide_master) %>%
    ph_with("Isoproterenol", location = ph_location_type("title")) %>%
    ph_with_vg(ggobj = g_isoprot_fcast, type = "body") %>%
    add_slide(layout = slide_layout, master = slide_master) %>%
    ph_with("IVIG", location = ph_location_type("title")) %>%
    ph_with_vg(ggobj = g_ivig_fcast, type = "body") %>%
    add_slide(layout = slide_layout, master = slide_master) %>%
    ph_with("Pegfilgrastim (Outpatient)", location = ph_location_type("title")) %>%
    ph_with_vg(ggobj = g_pegf_fcast_out, type = "body") %>%
    add_slide(layout = slide_layout, master = slide_master) %>%
    ph_with("Sugammadex", location = ph_location_type("title")) %>%
    ph_with_vg(ggobj = g_sug_fcast, type = "body") %>%
    add_slide(layout = slide_layout, master = slide_master) %>%
    ph_with("Ceftaroline", location = ph_location_type("title")) %>%
    ph_with_vg(ggobj = g_ceftar_fcast, type = "body") %>%
    add_slide(layout = slide_layout, master = slide_master) %>%
    ph_with("Ceftazidime-Avibactam", location = ph_location_type("title")) %>%
    ph_with_vg(ggobj = g_avycaz_fcast, type = "body") %>%
    add_slide(layout = slide_layout, master = slide_master) %>%
    ph_with("Ceftolozane-Tazobactam", location = ph_location_type("title")) %>%
    ph_with_vg(ggobj = g_zerbaxa_fcast, type = "body") %>%
    add_slide(layout = slide_layout, master = slide_master) %>%
    ph_with("Daptomycin", location = ph_location_type("title")) %>%
    ph_with_vg(ggobj = g_dapto_fcast, type = "body") %>%
    add_slide(layout = slide_layout, master = slide_master) %>%
    ph_with("Ertapenem", location = ph_location_type("title")) %>%
    ph_with_vg(ggobj = g_ertap_fcast, type = "body") %>%
    print(
        target = paste0(
            "figs/target_med_forecast_",
            format(month_end, "%Y-%m"),
            ".pptx"
        )
    )

# read_pptx() %>%
#     add_slide(layout = slide_layout, master = slide_master) %>%
#     ph_with("Albumin", location = ph_location_type("title")) %>%
#     ph_with_vg(ggobj = g_stacked, type = "body") %>%
#     print(target = "figs/stacked_fy.pptx")

