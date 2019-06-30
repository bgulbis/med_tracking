import numpy as np 
import pandas as pd 
import datetime as datetime 

from pptx import Presentation 
from pptx.chart.data import CategoryChartData
from pptx.dml.color import RGBColor
from pptx.enum.chart import XL_CHART_TYPE, XL_LEGEND_POSITION, XL_DATA_LABEL_POSITION, XL_MARKER_STYLE, XL_TICK_MARK
from pptx.enum.dml import MSO_LINE_DASH_STYLE
from pptx.util import Inches, Pt

prs = Presentation()
# title slide
title_slide_layout = prs.slide_layouts[0]
slide = prs.slides.add_slide(title_slide_layout)
title = slide.shapes.title
subtitle = slide.placeholders[1]

title.text = "Test Forecast Slides"
subtitle.text = "Updated: Some Date"

# forecast slide
blank_slide_layout = prs.slide_layouts[6]
slide = prs.slides.add_slide(blank_slide_layout)

chart_data = CategoryChartData()
#chart_data.categories = ['Q1 Sales', 'Q2 Sales', 'Q3 Sales']

date1 = '2017-07-01'
date2 = '2017-12-01'
chart_data.categories = pd.date_range(date1, date2, freq="MS").tolist()

chart_data.add_series('West',    (32.2, 28.4, 34.7))
chart_data.add_series('East',    (24.3, 30.6, 20.2))
chart_data.add_series('Midwest', (None, None, None, 20.4, 18.3, 26.2))

x, y, cx, cy = Inches(1), Inches(1), Inches(8), Inches(6)
chart = slide.shapes.add_chart(XL_CHART_TYPE.LINE, x, y, cx, cy, chart_data).chart

category_axis = chart.category_axis
category_axis.has_title = True
category_axis.axis_title.text_frame.text = "Month"

value_axis = chart.value_axis
value_axis.has_major_gridlines = False
value_axis.has_minor_gridlines = False

chart.has_legend = True
chart.legend.include_in_layout = False
chart.legend.position = XL_LEGEND_POSITION.TOP

chart.series[0].smooth = True

chart.series[1].format.line.dash_style = MSO_LINE_DASH_STYLE.DASH
chart.series[1].format.line.width = Pt(1.5)

i = len(chart.series[-1].points) - 1
chart.series[-1].points[i]

chart.series[-1].points[i].marker.style = XL_MARKER_STYLE.CIRCLE
chart.series[-1].points[i].marker.format.fill.solid()
chart.series[-1].points[i].marker.format.fill.fore_color.rgb = RGBColor.from_string("FFFFFF")
chart.series[-1].points[i].marker.format.line.width = Pt(1.5)
chart.series[-1].points[i].data_label.has_text_frame = True
chart.series[-1].points[i].data_label.position = XL_DATA_LABEL_POSITION.BELOW
chart.series[-1].points[i].data_label.text_frame.text = str(round(chart.series[-1].values[-1], None))

prs.save('doc/forecast.pptx')
