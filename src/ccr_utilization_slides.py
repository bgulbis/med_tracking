import glob
import numpy as np
import pandas as pd

from datetime import date, datetime
from os import listdir
from pptx import Presentation
from pptx.chart.data import CategoryChartData
# from pptx.dml.color import RGBColor
from pptx.enum.chart import XL_CHART_TYPE, XL_LEGEND_POSITION, XL_DATA_LABEL_POSITION, XL_MARKER_STYLE, XL_TICK_MARK
from pptx.enum.dml import MSO_LINE_DASH_STYLE, MSO_THEME_COLOR
from pptx.util import Inches, Pt

def read_data(file):
    filepaths = glob.glob("../data/tidy/" + file + "/" + file + "_daily_doses*.csv")
    df = pd.concat(map(lambda x: pd.read_csv(x, index_col=0, parse_dates=True), filepaths), sort=False)
    df.sort_index(inplace=True)
    return df

def prep_df(df):
    df = df.resample("MS").sum()
    df["FY"] = "FY" + (df.index + pd.DateOffset(months=6)).strftime('%Y')
    df["idx"] = pd.to_datetime("2019-" + (df.index + pd.DateOffset(months=-6)).strftime('%m-%d')) + pd.DateOffset(months=6)
    df_pvt = df['2016-07-01':].pivot(index="idx", columns="FY", values="DOSES").replace({pd.np.nan: None})

    return df_pvt

def add_utilization_slide(p, df, med):
    blank_slide_layout = p.slide_layouts[6]
    slide = p.slides.add_slide(blank_slide_layout)

    chart_data = CategoryChartData()
    chart_data.categories = df.index

    for i in range(0, len(df.columns)):
        chart_data.add_series(df.columns[i], df.iloc[:, i])

    x, y, cx, cy = Inches(1), Inches(1), Inches(8), Inches(6)
    chart = slide.shapes.add_chart(XL_CHART_TYPE.LINE, x, y, cx, cy, chart_data).chart
    chart = format_graph(chart, med)
    return p

def format_graph(chart, med, font_nm=None, ax_bright=0.5):
    category_axis = format_axis_title(chart.category_axis, "Month", font_nm, ax_bright)
    category_axis = format_axis_tick_labels(category_axis, font_nm, ax_bright, "mmm")
    category_axis = format_axis(category_axis, ax_bright)

    ax_max = date(2020, 6, 30) - date(1899, 12, 30)
    category_axis.maximum_scale = ax_max.days  # 6/30/2020

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
    chart.chart_title.text_frame.text = med_title + " utilization"
    chart.chart_title.text_frame.paragraphs[0].font.name = font_nm
    chart.chart_title.text_frame.paragraphs[0].font.size = Pt(24)
    chart.chart_title.text_frame.paragraphs[0].font.bold = False
    chart.chart_title.text_frame.paragraphs[0].font.color.theme_color = MSO_THEME_COLOR.TEXT_1
    chart.chart_title.text_frame.paragraphs[0].font.color.brightness = 0.25

    chart.has_legend = False
    # chart.legend.include_in_layout = False
    # chart.legend.position = XL_LEGEND_POSITION.TOP

    # format line for current fiscal year
    col_cur = MSO_THEME_COLOR.TEXT_1
    chart.series[-1].format.line.width = Pt(3.5)
    chart.series[-1].format.line.color.theme_color = col_cur
    chart.series[-1].data_labels.font.name = font_nm
    chart.series[-1].data_labels.font.size = Pt(16)
    chart.series[-1].data_labels.font.bold = False
    chart.series[-1].data_labels.font.color.theme_color = col_cur

    col_prev = [MSO_THEME_COLOR.ACCENT_1,
                MSO_THEME_COLOR.ACCENT_2,
                MSO_THEME_COLOR.ACCENT_3,
                MSO_THEME_COLOR.ACCENT_4]
    brght_prev = 0.8

    if len(chart.series) > 1:
        for i in range(0, len(chart.series) - 1):
            chart.series[i].format.line.width = Pt(2.5)
            chart.series[i].format.line.color.theme_color = col_prev[i]
            chart.series[i].format.line.color.brightness = brght_prev
            chart.series[i].data_labels.font.name = font_nm
            chart.series[i].data_labels.font.size = Pt(16)
            chart.series[i].data_labels.font.bold = False
            chart.series[i].data_labels.font.color.theme_color = col_prev[i]
            chart.series[i].data_labels.font.color.brightness = brght_prev
            chart.series[i].data_labels.position = XL_DATA_LABEL_POSITION.RIGHT

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
    title.text = "Utilization of Target Medications"
    data_end = read_data(meds[0]).index[-1].strftime('%B %Y')
    subtitle.text = "Data through: " + data_end + "\nBrian Gulbis, PharmD, BCPS"

    for i in meds:
        df = read_data(i)
        df = prep_df(df)
        add_utilization_slide(prs, df, i)

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
# save_month = (datetime.now() + pd.DateOffset(months=-1)).strftime('%Y-%m')
prs.save('../report/utilization/python_utilization_slides.pptx')