# in general this srequires no modification
library(shiny)
library(shinyjs)

source("R/submit/global.R")

fluidPage(
  tags$head(includeCSS(file.path('R/submit/www', 'style.css'))),
  shinyjs::useShinyjs(),
  
  inputPanel(
    textInput("neptun", label = "Neptun", value = "")
  ),
  
  uiOutput("submit_ui"),
  
  hr(),
  actionButton("submit", submitbtn_label, class = "btn-primary", icon = icon("cloud-upload")),
  shinyjs::hidden(
    span(id = "submit_msg", submit_msg, style = "margin-left: 15px;")
  )
  
  # shinyjs::hidden(div(img(id = "success_img", src ="image/sucess.jpg", alt="sucess")))
  
)