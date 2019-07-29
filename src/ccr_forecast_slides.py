import glob
import numpy as np
import pandas as pd

from datetime import date, datetime
from pptx import Presentation
from pptx.chart.data import CategoryChartData
from pptx.enum.chart import XL_CHART_TYPE, XL_LEGEND_POSITION, XL_DATA_LABEL_POSITION, XL_MARKER_STYLE, XL_TICK_MARK
from pptx.enum.dml import MSO_LINE_DASH_STYLE, MSO_THEME_COLOR
from pptx.util import Inches, Pt
from rpy2.robjects import pandas2ri
from rpy2.robjects.packages import importr
from rpy2.robjects.functions import SignatureTranslatedFunction

pandas2ri.activate()
forecast = importr('forecast')
forecast.auto_arima = SignatureTranslatedFunction(forecast.auto_arima, init_prm_translate = {'lambda_': 'lambda'})

def read_data(file):
    filepaths = glob.glob("../data/tidy/" + file + "/" + file + "_daily_doses*.csv")
    df = pd.concat(map(lambda x: pd.read_csv(x, index_col=0, parse_dates=True), filepaths), sort=False)
    df.sort_index(inplace=True)
    return df

def make_forecast(df, h=12):
    f_m = forecast.auto_arima(
        df["DOSES"],
        max_order=10,
        stepwise=False,
        approximation=False,
        lambda_="auto",
        biasadj=True
    )

    fc_m = forecast.forecast(f_m, h=h)
    return fc_m

def make_forecast_df(med, h=12, resamp="MS"):
    df = read_data(med)
    df_encntr = df.copy()

    if "ENCOUNTER_TYPE" in df_encntr.columns:
        df_encntr["Inpatient"] = df["ENCOUNTER_TYPE"].str.contains("inpatient|observation", case=False)
        df_pvt = df_encntr.pivot_table(values="DOSES", index="EVENT_DATE", columns="Inpatient", aggfunc=np.sum)
        df_pvt = df_pvt.resample("MS").sum()
        df_pvt.columns = ["Outpatient", "Inpatient"]

    df = df.resample(resamp).sum()
    fc = make_forecast(df, h)
    idx_m = pd.date_range(df.index[-1] + pd.DateOffset(months=1), periods=h, freq=resamp)
    yhat = pd.Series(fc.rx2("mean"), name="Forecast", index=idx_m)
    lwr = pd.Series(fc.rx2("lower")[:, :1].reshape(12, ), name="Lower", index=idx_m)
    upr = pd.Series(fc.rx2("upper")[:, :1].reshape(12, ), name="Upper", index=idx_m)
    df_fc = pd.concat([yhat, lwr, upr], axis=1, sort=False)
    df.columns = ["Actual"]

    if "ENCOUNTER_TYPE" in df_encntr.columns:
        x = [df, df_fc, df_pvt]
    else:
        x = [df, df_fc]

    df_combined = pd.concat(x, axis=1).replace({pd.np.nan: None})

    return df_combined

def add_forecast_slide(p, df, med):
    blank_slide_layout = p.slide_layouts[6]
    slide = p.slides.add_slide(blank_slide_layout)

    num_fmt = "#,##0"
    chart_data = CategoryChartData()
    chart_data.categories = df.index
    chart_data.categories.number_format = "mmmm yyyy"

    chart_data.add_series("Actual", df["Actual"], num_fmt)
    chart_data.add_series("Forecast", df["Forecast"], num_fmt)
    chart_data.add_series("Upper", df["Upper"], num_fmt)
    chart_data.add_series("Lower", df["Lower"], num_fmt)

    # use this if want to show separate use on inpt/outpt basis
    #     if "Outpatient" in df.columns:
    #         chart_data.add_series("Outpatient", df["Outpatient"])
    #         chart_data.add_series("Inpatient", df["Inpatient"])

    x, y, cx, cy = Inches(1), Inches(1), Inches(8), Inches(6)
    chart = slide.shapes.add_chart(XL_CHART_TYPE.LINE, x, y, cx, cy, chart_data).chart
    chart = format_graph(chart, med)
    return p

def format_graph(chart, med, font_nm=None, ax_bright=0.5):
    category_axis = format_axis_title(chart.category_axis, "Month", font_nm, ax_bright)
    category_axis = format_axis_tick_labels(category_axis, font_nm, ax_bright, "mmm yy")
    category_axis = format_axis(category_axis, ax_bright)

    ax_min = date(2016, 7, 1) - date(1899, 12, 30)
    category_axis.minimum_scale = ax_min.days  # 7/1/2016

    ax_max = date(2020, 7, 1) - date(1899, 12, 30)
    category_axis.maximum_scale = ax_max.days  # 7/1/2020

    value_axis = format_axis_title(chart.value_axis, "Doses per month", font_nm, ax_bright)
    value_axis = format_axis_tick_labels(value_axis, font_nm, ax_bright, "#,##0")
    value_axis = format_axis(value_axis, ax_bright)

    if med == "ivig":
        med_title = med.upper()
    elif med == "acetaminophen-iv":
        med_title = "Acetaminophen IV"
    elif med == "bupivacaine-liposomal":
        med_title = "Bupivacaine (liposomal)"
    elif med == "levothyroxine-iv":
        med_title = "Levothyroxine IV"
    else:
        med_title = med.title()

    chart.has_title = True
    chart.chart_title.text_frame.text = med_title + " forecast"
    chart.chart_title.text_frame.paragraphs[0].font.name = font_nm
    chart.chart_title.text_frame.paragraphs[0].font.size = Pt(24)
    chart.chart_title.text_frame.paragraphs[0].font.bold = False
    chart.chart_title.text_frame.paragraphs[0].font.color.theme_color = MSO_THEME_COLOR.TEXT_1
    chart.chart_title.text_frame.paragraphs[0].font.color.brightness = 0.25

    chart.has_legend = False
    # chart.legend.include_in_layout = False
    # chart.legend.position = XL_LEGEND_POSITION.TOP

    dlbl_pos = XL_DATA_LABEL_POSITION.ABOVE
    # set default formatting for data labels
    for i in range(0, 2):
        chart.series[i].data_labels.number_format = "#,##0"
        chart.series[i].data_labels.font.name = font_nm
        chart.series[i].data_labels.font.size = Pt(16)
        chart.series[i].data_labels.font.bold = False
        chart.series[i].data_labels.position = dlbl_pos

    # format Actual line
    col_actual = MSO_THEME_COLOR.ACCENT_1
    chart.series[0].format.line.width = Pt(3.5)
    chart.series[0].format.line.color.theme_color = col_actual

    size_mrk_actual = 6
    chart.series[0].marker.size = size_mrk_actual
    chart.series[0].marker.format.fill.solid()
    chart.series[0].marker.format.fill.fore_color.theme_color = MSO_THEME_COLOR.BACKGROUND_1
    chart.series[0].marker.format.line.width = Pt(2)
    chart.series[0].marker.format.line.color.theme_color = col_actual
    i = len(chart.series[0].points) - 13
    chart.series[0].points[i].marker.style = XL_MARKER_STYLE.CIRCLE
    chart.series[0].points[i].marker.size = size_mrk_actual
    #     chart.series[0].points[i].data_label.position = dlbl_pos

    #     i = len(chart.series[0].points) - 13
    #     chart = format_marker(chart, 0, i, font_nm, MSO_THEME_COLOR.BACKGROUND_1, col_actual)

    # format Forecast line
    brght_forecast = 0.4
    size_mrk_forecast = 4

    chart.series[1].format.line.dash_style = MSO_LINE_DASH_STYLE.DASH
    chart.series[1].format.line.width = Pt(2.5)
    chart.series[1].format.line.color.theme_color = col_actual
    chart.series[1].format.line.color.brightness = brght_forecast

    chart.series[1].marker.size = size_mrk_forecast
    chart.series[1].marker.format.fill.solid()
    chart.series[1].marker.format.fill.fore_color.theme_color = col_actual
    chart.series[1].marker.format.fill.fore_color.brightness = brght_forecast
    chart.series[1].marker.format.line.width = Pt(2)
    chart.series[1].marker.format.line.color.theme_color = col_actual
    chart.series[1].marker.format.line.color.brightness = brght_forecast
    i = len(chart.series[1].points) - 1
    chart.series[1].points[i].marker.style = XL_MARKER_STYLE.CIRCLE
    chart.series[1].points[i].marker.size = size_mrk_forecast
    #     chart.series[1].points[i].data_label.position = dlbl_pos

    #     j = len(chart.series[1].points) - 1
    #     chart = format_marker(chart, 1, j, font_nm, col_actual, col_actual, brght_forecast)

    # format confidence interval lines
    col_ci = MSO_THEME_COLOR.BACKGROUND_1
    brght_ci = -0.05
    line_ci = Pt(1.5)
    chart.series[2].format.line.width = line_ci
    chart.series[2].format.line.color.theme_color = col_ci
    chart.series[2].format.line.color.brightness = brght_ci
    chart.series[3].format.line.width = line_ci
    chart.series[3].format.line.color.theme_color = col_ci
    chart.series[3].format.line.color.brightness = brght_ci

    # would only be used if inpatient/outpatient lines are displayed
    if len(chart.series) > 4:
        chart.series[4].format.line.width = line_ci
        chart.series[4].format.line.color.theme_color = MSO_THEME_COLOR.ACCENT_3
        chart.series[4].format.line.color.brightness = brght_forecast

        chart.series[5].format.line.width = line_ci
        chart.series[5].format.line.color.theme_color = MSO_THEME_COLOR.ACCENT_2
        chart.series[5].format.line.color.brightness = brght_forecast

    return chart

def format_axis(axis, ax_bright):
    axis.format.line.color.theme_color = MSO_THEME_COLOR.TEXT_1
    axis.format.line.color.brightness = ax_bright
    axis.has_major_gridlines = False
    axis.has_minor_gridlines = False
    return axis

def format_axis_title(axis, title_text, font_nm, ax_bright, has_title=True):
    axis.has_title = has_title
    axis.axis_title.text_frame.text = title_text
    axis.axis_title.text_frame.paragraphs[0].font.name = font_nm
    axis.axis_title.text_frame.paragraphs[0].font.size = Pt(18)
    axis.axis_title.text_frame.paragraphs[0].font.bold = False
    axis.axis_title.text_frame.paragraphs[0].font.color.theme_color = MSO_THEME_COLOR.TEXT_1
    axis.axis_title.text_frame.paragraphs[0].font.color.brightness = ax_bright
    return axis

def format_axis_tick_labels(axis, font_nm, ax_bright, num_fmt):
    axis.tick_labels.font.name = font_nm
    axis.tick_labels.font.size = Pt(16)
    axis.tick_labels.font_bold = False
    axis.tick_labels.font.color.theme_color = MSO_THEME_COLOR.TEXT_1
    axis.tick_labels.font.color.brightness = ax_bright
    axis.tick_labels.number_format = num_fmt
    return axis

def make_slides(meds, n=12):
    prs = Presentation()
    # title slide
    title_slide_layout = prs.slide_layouts[0]
    slide = prs.slides.add_slide(title_slide_layout)
    title = slide.shapes.title
    subtitle = slide.placeholders[1]
    title.text = "Forecast for Target Medications"
    fcast_start = datetime.now().strftime('%B %Y')
    fcast_end = (datetime.now() + pd.DateOffset(months=11)).strftime('%B %Y')
    subtitle.text = fcast_start + " to " + fcast_end + "\nBrian Gulbis, PharmD, BCPS"

    for i in meds:
        df = make_forecast_df(i)
        add_forecast_slide(prs, df, i)

    return prs

meds = ["acetaminophen-iv",
        "albumin",
        "sugammadex",
        "bupivacaine-liposomal",
        "isoproterenol",
        "nicardipine",
        "levothyroxine-iv",
        "ivig",
        "calcitonin",
        "pegfilgrastim",
        "eculizumab",
        "ceftaroline",
        "ceftazidime-avibactam",
        "ceftolozane-tazobactam",
        "daptomycin",
        "ertapenem",
        "meropenem-vaborbactam"]

prs = make_slides(meds)
prs.save('../report/utilization/python_forecast_slides.pptx')