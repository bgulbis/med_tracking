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

campus <- c(
    "HC Childrens",
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

# albumin ----------------------------------------------

dir_data <- "data/tidy/albumin"

data_albumin_events <- get_data(dir_data, "albumin_events")
# data_albumin_orders <- get_data(dir_data, "albumin_orders")

df_albumin <- data_albumin_events %>%
    filter(facility_event %in% campus) %>%
    make_df()

month_end <- max(df_albumin$med_date)

ts_albumin <- tk_ts(df_albumin)

fcast_albumin <- make_forecast(ts_albumin)
g_albumin <- plot_utilization(df_albumin)
g_albumin_fcast <- plot_forecast(fcast_albumin)

# acetaminophen ----------------------------------------

dir_data <- "data/tidy/acetaminophen"

data_apap_events <- get_data(dir_data, "apap_events") %>%
    distinct()
# data_apap_orders <- get_data(dir_data, "apap_orders")

df_apap <- data_apap_events %>%
    filter(
        facility_event %in% campus,
        # clinical_event_datetime >= mdy("4/1/2017"),
        clinical_event_datetime <= month_end %m+% months(1)
    ) %>%
    make_df()
    
ts_apap <- tk_ts(df_apap)

fcast_apap <- make_forecast(ts_apap)
g_apap <- plot_utilization(df_apap)
g_apap_fcast <- plot_forecast(fcast_apap)

# antibiotics ------------------------------------------

dir_data <- "data/tidy/abx"
data_abx_events <- get_data(dir_data, "antibiotics_events")
# data_abx_orders <- get_data(dir_data, "antibiotics_orders")

df_abx <- data_abx_events %>%
    filter(facility %in% campus) %>%
    make_df(med)

ts_ceftar <- df_abx %>%
    filter(med == "ceftaroline") %>%
    tk_ts()

fcast_ceftar <- make_forecast(ts_ceftar)
g_ceftar <-  df_abx %>%
    filter(med == "ceftaroline") %>%
    plot_utilization()
g_ceftar_fcast <- plot_forecast(fcast_ceftar)

ts_avycaz <- df_abx %>%
    filter(med == "ceftazidime-avibactam") %>%
    tk_ts()

fcast_avycaz <- make_forecast(ts_avycaz)
g_avycaz <- df_abx %>%
    filter(med == "ceftazidime-avibactam") %>%
    plot_utilization()
g_avycaz_fcast <- plot_forecast(fcast_avycaz)

ts_zerbaxa <- df_abx %>%
    filter(med == "ceftolozane-tazobactam") %>%
    tk_ts()

fcast_zerbaxa <- make_forecast(ts_zerbaxa)
g_zerbaxa <- df_abx %>%
    filter(med == "ceftolozane-tazobactam") %>%
    plot_utilization()
g_zerbaxa_fcast <- plot_forecast(fcast_zerbaxa)

ts_dapto <- df_abx %>%
    filter(med == "DAPTOmycin") %>%
    tk_ts()

fcast_dapto <- make_forecast(ts_dapto)
g_dapto <- df_abx %>%
    filter(med == "DAPTOmycin") %>%
    plot_utilization()
g_dapto_fcast <- plot_forecast(fcast_dapto)

ts_ertap <- df_abx %>%
    filter(med == "ertapenem") %>%
    tk_ts()

fcast_ertap <- make_forecast(ts_ertap)
g_ertap <- df_abx %>%
    filter(med == "ertapenem") %>%
    plot_utilization()
g_ertap_fcast <- plot_forecast(fcast_ertap)

# bupivacaine-liposome ---------------------------------

dir_data <- "data/tidy/bupivacaine-liposome"
# data_bupiv_events <- get_data(dir_data, "bupivacaine-liposome_events")
data_bupiv_orders <- get_data(dir_data, "bupivacaine-liposome_orders")

df_bupiv <- data_bupiv_orders %>%
    filter(facility %in% campus) %>%
    rename(clinical_event_datetime = order_datetime) %>%
    make_df()

ts_bupiv <- tk_ts(df_bupiv)

fcast_bupiv <- make_forecast(ts_bupiv)
g_bupiv <- plot_utilization(df_bupiv)
g_bupiv_fcast <- plot_forecast(fcast_bupiv)

# calcitonin -------------------------------------------

dir_data <- "data/tidy/calcitonin"
data_calcitonin_events <- get_data(dir_data, "calcitonin_events")
# data_calcitonin_orders <- get_data(dir_data, "calcitonin_orders")

df_calctn <- data_calcitonin_events %>%
    filter(facility %in% campus) %>%
    make_df()

ts_calctn <- tk_ts(df_calctn)

fcast_calctn <- make_forecast(ts_calctn)
g_calctn <- plot_utilization(df_calctn)
g_calctn_fcast <- plot_forecast(fcast_calctn)

# isoproterenol ----------------------------------------

dir_data <- "data/tidy/isoproterenol"
data_isoprot_events <- get_data(dir_data, "isoproterenol_events")
# data_isoprot_orders <- get_data(dir_data, "isoproterenol_orders")

df_isoprot <- data_isoprot_events %>%
    filter(
        facility %in% campus,
        iv_event == "Begin Bag"
    ) %>%
    make_df()

ts_isoprot <- tk_ts(df_isoprot)

fcast_isoprot <- make_forecast(ts_isoprot)
g_isoprot <- plot_utilization(df_isoprot)
g_isoprot_fcast <- plot_forecast(fcast_isoprot)

# ivig -------------------------------------------------

dir_data <- "data/tidy/ivig"
data_ivig_events <- get_data(dir_data, "ivig_events")
# data_ivig_orders <- get_data(dir_data, "ivig_orders")

df_ivig <- data_ivig_events %>%
    filter(facility %in% campus) %>%
    make_df()

ts_ivig <- tk_ts(df_ivig)

fcast_ivig <- make_forecast(ts_ivig)
g_ivig <- plot_utilization(df_ivig)
g_ivig_fcast <- plot_forecast(fcast_ivig) +
    coord_cartesian(ylim = c(0, 100))

# pegfilgrastim ----------------------------------------

dir_data <- "data/tidy/pegfilgrastim"
data_pegfilgrastim_events <- get_data(dir_data, "pegfilgrastim_events")
# data_pegfilgrastim_orders <- get_data(dir_data, "pegfilgrastim_orders")

df_pegf <- data_pegfilgrastim_events %>%
    mutate(
        visit_type = if_else(
            encounter_type %in% c("Inpatient", "Observation"),
            "Inpatient",
            "Outpatient"
        )
    ) %>%
    # filter(!(encounter_type %in% c("Inpatient", "Observation"))) %>%
    make_df(visit_type)

ts_pegf <- df_pegf %>%
    filter(visit_type == "Outpatient") %>%
    tk_ts()

fcast_pegf <- make_forecast(ts_pegf)
g_pegf <- df_pegf %>%
    filter(visit_type == "Outpatient") %>%
    plot_utilization()
g_pegf_fcast <- plot_forecast(fcast_pegf)

# g_pegf_inpt <- df_pegf %>%
#     filter(visit_type == "Inpatient") %>%
#     plot_utilization()

# sugammadex -------------------------------------------

dir_data <- "data/tidy/sugammadex"
data_sug_neo_events <- get_data(dir_data, "sug-neo_events")
# data_pegfilgrastim_orders <- get_data(dir_data, "pegfilgrastim_orders")

df_sug <- data_sug_neo_events %>%
    filter(
        facility %in% campus,
        clinical_event_datetime >= mdy("4/1/2017"),
        med == "sugammadex"
    ) %>%
    make_df()

ts_sug <- tk_ts(df_sug)

fcast_sug <- make_forecast(ts_sug)
g_sug <- plot_utilization(df_sug)
g_sug_fcast <- plot_forecast(fcast_sug) +
    coord_cartesian(ylim = c(0, 700))

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
    ph_with("Isoproterenol", location = ph_location_type("title")) %>%
    ph_with_vg(ggobj = g_isoprot, type = "body") %>%
    add_slide(layout = slide_layout, master = slide_master) %>%
    ph_with("IVIG", location = ph_location_type("title")) %>%
    ph_with_vg(ggobj = g_ivig, type = "body") %>%
    add_slide(layout = slide_layout, master = slide_master) %>%
    ph_with("Outpatient Pegfilgrastim", location = ph_location_type("title")) %>%
    ph_with_vg(ggobj = g_pegf, type = "body") %>%
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
    ph_with("Isoproterenol", location = ph_location_type("title")) %>%
    ph_with_vg(ggobj = g_isoprot_fcast, type = "body") %>%
    add_slide(layout = slide_layout, master = slide_master) %>%
    ph_with("IVIG", location = ph_location_type("title")) %>%
    ph_with_vg(ggobj = g_ivig_fcast, type = "body") %>%
    add_slide(layout = slide_layout, master = slide_master) %>%
    ph_with("Outpatient Pegfilgrastim", location = ph_location_type("title")) %>%
    ph_with_vg(ggobj = g_pegf_fcast, type = "body") %>%
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

