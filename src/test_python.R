library(reticulate)
use_condaenv("med_tracking")
pptx <- import("pptx")

prs <- pptx$Presentation()

title_slide_layout <- prs$slide_layouts[0]
slide <- prs$slides$add_slide(title_slide_layout)
title <- slide$shapes$title
title$text = "Forecast for Target Medications"

prs$save("doc/py_from_r.pptx")
