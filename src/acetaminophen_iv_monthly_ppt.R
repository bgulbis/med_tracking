library(tidyverse)
library(lubridate)
library(officer)
library(themebg)
library(ggrepel)
library(rvg)
# library(mschart)
# library(forecast)
# library(timetk)
# library(sweep)

tz_locale <- locale(tz = "US/Central")
floor_unit <- "month"

data_month <- floor_date(rollback(now(), FALSE, FALSE), unit = "month")
fy <- year(data_month %m+% months(6))

# col_pal <- c("#377eb8", "#4daf4a", "#ff7f00", "#999999")

col_pal <- c("#4E79A7", "#F28E2B", "#E15759", "#76B7B2")
# col_pal <- c(
#     rgb(77, 77, 77, maxColorValue = 255),
#     rgb(93, 165, 218, maxColorValue = 255),
#     rgb(250, 164, 58, maxColorValue = 255),
#     rgb(96, 189, 104, maxColorValue = 255)
# )

# col_pal <- c(
#     rgb(255, 86, 87, maxColorValue = 255),
#     rgb(55, 108, 138, maxColorValue = 255),
#     rgb(242, 217, 187, maxColorValue = 255),
#     rgb(99, 143, 169, maxColorValue = 255)
# )

# col_pal <- "Dark2"

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

df_apap_orders <- data_apap_orders %>%
    filter(
        order_datetime <= month_end,
        facility_order %in% campus,
        (verified_status != "Rejected" | is.na(verified_status))
    ) %>%
    mutate(
        order_month = floor_date(order_datetime, unit = floor_unit),
        fiscal_year = year(order_month %m+% months(6)),
        month_plot = month(order_month, label = TRUE, abbr = TRUE),
        freq_type = case_when(
            prn ~ "PRN",
            freq == "ONCE" ~ "Once",
            is.na(freq) ~ "Unknown",
            TRUE ~ "Scheduled"
        )
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

# ts_apap <- tk_ts(df_apap)
# 
# fcast_apap <- make_forecast(ts_apap)
# g_apap <- plot_utilization(df_apap)
# g_apap_fcast <- plot_forecast(fcast_apap)

# graphs -----------------------------------------------

cutoff <- 15L
family <- "Calibri"

g_utilization_fy <- df_apap_n %>%
    filter(fiscal_year == fy) %>%
    ggplot(aes(x = med_month, y = value, color = key)) +
    geom_line(size = 1) +
    geom_smooth(method = "lm", size = 0.5, linetype = "dashed", se = FALSE) +
    geom_text_repel(
        aes(label = label), 
        nudge_y = -1, 
        color = "Grey35", 
        family = "Calibri"
    ) +
    ggtitle("Monthly utilization of IV acetaminophen") +
    scale_x_datetime(
        paste("Fiscal Year", fy), 
        date_breaks = "1 month", 
        date_labels = "%b"
    ) +
    ylab("Number") +
    # scale_color_brewer(NULL, palette = col_pal) +
    scale_color_manual(NULL, values = col_pal) +
    expand_limits(y = 0) +
    theme_bg() +
    theme(
        legend.position = "None", 
        axis.text.x = element_text(vjust = 0.1),
        axis.title.x = element_text(vjust = 0.5),
        axis.text = element_text(family = family, size = 14),
        axis.title = element_text(family = family, size = 16),
        plot.title = element_text(family = family, size = 22, hjust = 0.5)
    )

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

g_po_unit <- df_apap %>%
    filter(
        med_month == data_month,
        po_med
    ) %>%
    count(nurse_unit, sort = TRUE) %>%
    mutate_at("nurse_unit", fct_inorder) %>%
    mutate_at("nurse_unit", fct_rev) %>%
    filter(n >= 5) %>%
    ggplot(aes(x = nurse_unit, y = n)) +
    geom_col() +
    xlab(NULL) +
    ylab("Number of doses") +
    coord_flip() +
    theme_bg() 

g_orders_unit <- df_apap_orders %>%
    filter(order_month == data_month) %>%
    add_count(nurse_unit_order, freq_type, name = "freq_type_n") %>%
    add_count(nurse_unit_order, name = "orders") %>%
    arrange(orders) %>%
    mutate_at("nurse_unit_order", fct_inorder) %>%
    # mutate_at("nurse_unit_order", fct_rev) %>%
    distinct(nurse_unit_order, freq_type, freq_type_n, orders) %>%
    filter(orders >= cutoff) %>%
    ggplot(aes(x = nurse_unit_order, y = freq_type_n, fill = freq_type)) +
    geom_col() +
    xlab(NULL) +
    ylab("Number of orders") +
    # scale_fill_brewer(NULL, palette = "Paired") +
    scale_fill_manual(NULL, values = col_pal) +
    coord_flip() +
    theme_bg() 

g_orders_service <- df_apap_orders %>%
    filter(order_month == data_month) %>%
    add_count(med_service_order, freq_type, name = "freq_type_n") %>%
    add_count(med_service_order) %>%
    arrange(n) %>%
    mutate_at("med_service_order", fct_inorder) %>%
    # mutate_at("nurse_unit_order", fct_rev) %>%
    distinct(med_service_order, freq_type, freq_type_n, n) %>%
    filter(n >= cutoff) %>%
    ggplot(aes(x = med_service_order, y = freq_type_n, fill = freq_type)) +
    geom_col() +
    xlab(NULL) +
    ylab("Number of orders") +
    scale_fill_manual(NULL, values = col_pal) +
    coord_flip() +
    theme_bg() 

g_orders_provider <- df_apap_orders %>%
    mutate_at(
        "provider_position", 
        str_replace_all, 
        pattern = " eOrder", 
        replacement = ""
    ) %>%
    filter(order_month == data_month) %>%
    add_count(provider_position, freq_type, name = "freq_type_n") %>%
    add_count(provider_position) %>%
    arrange(n) %>%
    mutate_at("provider_position", fct_inorder) %>%
    distinct(provider_position, freq_type, freq_type_n, n) %>%
    filter(n >= cutoff) %>%
    ggplot(aes(x = provider_position, y = freq_type_n, fill = freq_type)) +
    geom_col() +
    xlab(NULL) +
    ylab("Number of orders") +
    scale_fill_manual(NULL, values = col_pal) +
    coord_flip() +
    theme_bg() 

g_orders_fy <- df_apap_orders %>%
    count(fiscal_year, month_plot, order_month, freq_type) %>%
    filter(fiscal_year == fy) %>%
    arrange(order_month) %>%
    group_by(freq_type) %>%
    mutate(
        label = if_else(
            order_month == last(order_month),
            freq_type,
            NA_character_
        )
    ) %>%
    ggplot(aes(x = order_month, y = n, color = freq_type)) +
    geom_line(size = 1) +
    geom_text_repel(
        aes(label = label), 
        nudge_y = -1, 
        color = "Grey35", 
        family = "Calibri"
    ) +
    ggtitle("Monthly utilization of IV acetaminophen") +
    scale_x_datetime(
        paste("Fiscal Year", fy), 
        date_breaks = "1 month", 
        date_labels = "%b"
    ) +
    ylab("Number") +
    scale_color_manual(NULL, values = col_pal) +
    theme_bg() +
    theme(legend.position = "None")


# powerpoint -------------------------------------------

slide_layout <- "Title and Content"
slide_master <- "Office Theme"
title_size <- fp_text(font.size = 32)

# layout_summary(read_pptx())
# layout_properties(read_pptx(), layout = "Title Slide", master = slide_master)
# layout_properties(read_pptx(), layout = slide_layout, master = slide_master)
# layout_properties(read_pptx(), layout = "Blank", master = slide_master)

cur_month <- format(data_month, "%B %Y")
l <- 0.5
w <- 9
h <- 6.5

read_pptx() %>%
    add_slide(layout = "Title Slide", master = slide_master) %>%
    ph_with_text(type = "ctrTitle", str = "Utilization of Acetaminophen IV") %>%
    ph_with_text(
        type = "subTitle", 
        str = paste0(
            "Data through: ", 
            cur_month,
            "\nBrian Gulbis, PharmD, BCPS"
        )
    ) %>%
    add_slide(layout = "Blank", master = slide_master) %>%
    ph_with_vg_at(
        ggobj = g_utilization_fy, 
        left = l, 
        top = l, 
        width = w, 
        height = h
    ) %>%
    
    add_slide(layout = slide_layout, master = slide_master) %>%
    ph_with(
        "Monthly orders for IV acetaminophen",
        location = ph_location_type("title")
    ) %>%
    ph_with_vg(ggobj = g_orders_fy, type = "body") %>%
    add_slide(layout = slide_layout, master = slide_master) %>%
    ph_with(
        paste("Doses by nursing unit in", cur_month),
        location = ph_location_type("title")
    ) %>%
    ph_with_vg(ggobj = g_units, type = "body") %>%
    add_slide(layout = slide_layout, master = slide_master) %>%
    ph_with(
        paste("Median doses per patient by nursing unit in", cur_month),
        location = ph_location_type("title")
    ) %>%
    ph_with_vg(ggobj = g_median, type = "body") %>%
    add_slide(layout = slide_layout, master = slide_master) %>%
    ph_with(
        "Acetaminophen IV doses given within 2 hours of oral medications",
        location = ph_location_type("title")
    ) %>%
    ph_with_vg(ggobj = g_po_trend, type = "body") %>%
    add_slide(layout = slide_layout, master = slide_master) %>%
    ph_with(
        paste("IV doses given within 2 hours of oral medications by nursing unit in", cur_month),
        location = ph_location_type("title")
    ) %>%
    ph_with_vg(ggobj = g_po_unit, type = "body") %>%
    add_slide(layout = slide_layout, master = slide_master) %>%
    ph_with(
        paste("Orders by nursing unit in", cur_month),
        location = ph_location_type("title")
    ) %>%
    ph_with_vg(ggobj = g_orders_unit, type = "body") %>%
    add_slide(layout = slide_layout, master = slide_master) %>%
    ph_with(
        paste("Orders by primary service in", cur_month),
        location = ph_location_type("title")
    ) %>%
    ph_with_vg(ggobj = g_orders_service, type = "body") %>%
    add_slide(layout = slide_layout, master = slide_master) %>%
    ph_with(
        paste("Orders by provider role in", cur_month),
        location = ph_location_type("title")
    ) %>%
    ph_with_vg(ggobj = g_orders_provider, type = "body") %>%
    print(
        target = paste0(
            "report/ppt/apap_iv_utilization_",
            format(data_month, "%Y-%m"),
            ".pptx"
        )
    )
