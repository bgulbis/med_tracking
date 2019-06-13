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

# constants --------------------------------------------

tz_locale <- locale(tz = "US/Central")
floor_unit <- "month"

data_month <- floor_date(rollback(now(), FALSE, FALSE), unit = "month")
fy <- year(data_month %m+% months(6))
cur_month <- format(data_month, "%B %Y")

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

col_pal <- c("#4E79A7", "#F28E2B", "#E15759", "#76B7B2")
text_col <- "Grey35"

cutoff <- 15L

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

gr_trend <- function(df, x, y, color, label, smooth = FALSE, title = waiver(), 
                     subtitle = waiver(), caption = waiver()) {
    x <- enquo(x)
    y <- enquo(y)
    color <- enquo(color)
    label <- enquo(label)
    
    g <- df %>%
        ggplot(aes(x = !!x, y = !!y, color = !!color)) +
        geom_line(size = 1)
    
    if (smooth) {
        g <- g +
            geom_smooth(
                method = "lm", 
                size = 0.5, 
                linetype = "dashed", 
                se = FALSE
            )
    }
    
    g <- g +
        geom_text_repel(
            aes(label = !!label), 
            nudge_y = -1, 
            color = text_col
        ) +
        labs(title = title, subtitle = subtitle, caption = caption) +
        scale_x_datetime(
            paste("Fiscal Year", fy), 
            date_breaks = "1 month", 
            date_labels = "%b"
        ) +
        ylab("Number") +
        scale_color_manual(NULL, values = col_pal) +
        expand_limits(y = 0) +
        theme_bg_ppt() +
        theme(legend.position = "None")
} 

gr_count_orders <- function(df, x, title, subtitle, cutoff = 15) {
    x <- enquo(x)
    
    df %>%
        filter(order_month == data_month) %>%
        add_count(!!x, freq_type, name = "freq_type_n") %>%
        add_count(!!x, name = "orders") %>%
        arrange(orders) %>%
        mutate_at(dplyr::vars(!!x), fct_inorder) %>%
        distinct(!!x, freq_type, freq_type_n, orders) %>%
        filter(orders >= cutoff) %>%
        ggplot(aes(x = !!x, y = freq_type_n, fill = freq_type)) +
        geom_col() +
        labs(title = title, subtitle = subtitle) +
        xlab(NULL) +
        ylab("Number of orders") +
        scale_fill_manual(NULL, values = col_pal) +
        coord_flip() +
        theme_bg_ppt() +
        theme(legend.position = "top")
}

# acetaminophen ----------------------------------------

dir_data <- "data/tidy/acetaminophen"

data_apap_events <- get_data(dir_data, "apap_events") %>%
    distinct() %>%
    mutate(
        med_month = floor_date(clinical_event_datetime, unit = floor_unit),
        fiscal_year = year(med_month %m+% months(6)),
        month_plot = month(med_month, label = TRUE, abbr = TRUE)
    ) %>%
    filter(
        med_month <= data_month,
        facility_event %in% campus,
        (is.na(admin_event) | admin_event == "Administered")
    ) 

data_apap_orders <- get_data(dir_data, "apap_orders") %>%
    mutate_at(
        "med_service_order",
        str_replace_all,
        pattern = " Service",
        replacement = ""
    ) %>%
    mutate_at(
        "med_service_order",
        str_replace_all,
        pattern = "Respiratory Therapy",
        replacement = "Resp Ther"
    ) %>%
    mutate_at(
        "provider_position", 
        str_replace_all, 
        pattern = " eOrder", 
        replacement = ""
    ) %>%
    mutate_at(
        "provider_position", 
        str_replace_all, 
        pattern = "Fellow/Resident", 
        replacement = "Fel/Res"
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
    ) %>%
    filter(
        order_month <= data_month,
        facility_order %in% campus,
        (verified_status != "Rejected" | is.na(verified_status))
    ) 

# n_apap_doses <- data_apap_events %>%
#     count(fiscal_year, month_plot, med_month, name = "doses") %>%
#     mutate_at("fiscal_year", as_factor) %>%
#     mutate_at(
#         "month_plot", 
#         factor, 
#         ordered = TRUE, 
#         levels = fy_months
#     ) %>%
#     arrange(med_month)

# ts_apap <- tk_ts(data_apap_events)
# 
# fcast_apap <- make_forecast(ts_apap)
# g_apap <- plot_utilization(data_apap_events)
# g_apap_fcast <- plot_forecast(fcast_apap)

# graphs -----------------------------------------------

n_apap_pts <- data_apap_events %>%
    distinct(encounter_id, med_month) %>%
    count(med_month, name = "patients") 

g_utilization_fy <- data_apap_events %>%
    filter(fiscal_year == fy) %>%
    count(med_month, name = "doses") %>%
    left_join(n_apap_pts, by = "med_month") %>%
    gather("key", "value", doses, patients) %>%
    group_by(key) %>%
    mutate(
        label = if_else(
            med_month == last(med_month),
            str_to_sentence(key),
            NA_character_
        )
    ) %>%
    gr_trend(
        x = med_month,
        y = value,
        color = key,
        label = label,
        smooth = TRUE,
        title = "Monthly utilization of IV acetaminophen"
    )

g_orders_fy <- data_apap_orders %>%
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
    gr_trend(
        x = order_month,
        y = n,
        color = freq_type,
        label = label,
        title = "Monthly orders of IV acetaminophen"
    )

# g_utilization_all <- df_apap_n %>%
#     ggplot(aes(x = med_month, y = value, color = key)) +
#     geom_line(size = 1) +
#     geom_smooth(method = "lm", size = 0.5, linetype = "dashed", se = FALSE) +
#     geom_text_repel(aes(label = label), nudge_y = -1) +
#     scale_x_datetime(NULL, date_breaks = "3 months", date_labels = "%b %y") +
#     ylab("Number") +
#     scale_color_manual(NULL, values = col_pal) +
#     theme_bg() +
#     theme(legend.position = "None")

g_units <- data_apap_events %>%
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
    labs(title = "Doses by nursing unit", subtitle = cur_month) +
    xlab(NULL) +
    ylab("Number of doses") +
    scale_fill_manual(NULL, values = col_pal, labels = c("Scheduled", "PRN")) +
    coord_flip() +
    theme_bg_ppt() +
    theme(legend.position = "top")

g_median <- data_apap_events %>%
    filter(med_month == data_month) %>%
    count(nurse_unit, encounter_id) %>%
    group_by(nurse_unit) %>%
    summarize_at("n", median, na.rm = TRUE) %>%
    arrange(desc(n)) %>%
    mutate_at("nurse_unit", fct_inorder) %>%
    mutate_at("nurse_unit", fct_rev) %>%
    filter(n > 1) %>%
    ggplot(aes(x = nurse_unit, y = n)) +
    geom_col(fill = col_pal[1]) +
    labs(title = "Median doses per patient by nursing unit", subtitle = cur_month) +
    xlab(NULL) +
    ylab("Median doses per patient") +
    coord_flip() +
    theme_bg_ppt() 

g_po_trend <- data_apap_events %>%
    filter(fiscal_year == fy) %>%
    group_by(med_month) %>%
    summarize(
        doses = n(),
        po_pct = sum(po_med, na.rm = TRUE) / doses
    ) %>%
    ggplot(aes(x = med_month, y = po_pct)) +
    geom_line(size = 1, color = col_pal[1]) +
    geom_smooth(
        method = "lm", 
        se = FALSE, 
        size = 0.5, 
        linetype = "dashed", 
        color = col_pal[1]
    ) +
    ggtitle("IV doses given within 2 hours of oral medications") +
    scale_x_datetime(
        paste("Fiscal Year", fy), 
        date_breaks = "1 month", 
        date_labels = "%b"
    ) +
    scale_y_continuous("Doses (%)", labels = scales::percent) +
    coord_cartesian(ylim = c(0, 1)) +
    theme_bg_ppt() +
    theme(legend.position = "None")

g_po_unit <- data_apap_events %>%
    filter(
        med_month == data_month,
        po_med
    ) %>%
    count(nurse_unit, sort = TRUE) %>%
    mutate_at("nurse_unit", fct_inorder) %>%
    mutate_at("nurse_unit", fct_rev) %>%
    filter(n >= 5) %>%
    ggplot(aes(x = nurse_unit, y = n)) +
    geom_col(fill = col_pal[1]) +
    labs(
        title = "Opportunity for conversion to oral by nursing unit", 
        subtitle = cur_month
    ) +
    xlab(NULL) +
    ylab("Number of doses") +
    coord_flip() +
    theme_bg_ppt() 

g_orders_unit <- data_apap_orders %>%
    gr_count_orders(
        nurse_unit_order,
        title = "Orders by nursing unit", 
        subtitle = cur_month
    )

g_orders_service <- data_apap_orders %>%
    gr_count_orders(
        med_service_order,
        title = "Orders by primary service",
        subtitle = cur_month
    )

g_orders_provider <- data_apap_orders %>%
    gr_count_orders(
        provider_position,
        title = "Orders by provider role",
        subtitle = cur_month
    )

# powerpoint -------------------------------------------

slide_layout <- "Title and Content"
slide_master <- "Office Theme"
# title_size <- fp_text(font.size = 32)

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
        # fonts = list("Arial" = "Arial")
    ) %>%
    add_slide(layout = "Blank", master = slide_master) %>%
    ph_with_vg_at(
        ggobj = g_orders_fy, 
        left = l, 
        top = l, 
        width = w, 
        height = h
    ) %>%
    add_slide(layout = "Blank", master = slide_master) %>%
    ph_with_vg_at(
        ggobj = g_units, 
        left = l, 
        top = l, 
        width = w, 
        height = h
    ) %>%
    add_slide(layout = "Blank", master = slide_master) %>%
    ph_with_vg_at(
        ggobj = g_median, 
        left = l, 
        top = l, 
        width = w, 
        height = h
    ) %>%
    add_slide(layout = "Blank", master = slide_master) %>%
    ph_with_vg_at(
        ggobj = g_po_trend, 
        left = l, 
        top = l, 
        width = w, 
        height = h
    ) %>%
    add_slide(layout = "Blank", master = slide_master) %>%
    ph_with_vg_at(
        ggobj = g_po_unit, 
        left = l, 
        top = l, 
        width = w, 
        height = h
    ) %>%
    add_slide(layout = "Blank", master = slide_master) %>%
    ph_with_vg_at(
        ggobj = g_orders_unit, 
        left = l, 
        top = l, 
        width = w, 
        height = h
    ) %>%
    add_slide(layout = "Blank", master = slide_master) %>%
    ph_with_vg_at(
        ggobj = g_orders_service, 
        left = l, 
        top = l, 
        width = w, 
        height = h
    ) %>%
    add_slide(layout = "Blank", master = slide_master) %>%
    ph_with_vg_at(
        ggobj = g_orders_provider, 
        left = l, 
        top = l, 
        width = w, 
        height = h
    ) %>%
    print(
        target = paste0(
            "report/ppt/apap_iv_utilization_",
            format(data_month, "%Y-%m"),
            ".pptx"
        )
    )
