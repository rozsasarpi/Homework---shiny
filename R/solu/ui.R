# in general this srequires no modification
library(shiny)

# if the shinyapp is called from markdown document then it cannot find the global.R automatically
source("R/solu/global.R")

fluidPage(
  fluidRow(
    inputPanel(
      column(width = 12,
        textInput("neptun", label = "Neptun", value = "")
      ),
      column(width = 12,
        uiOutput("select_problem")
      )
    )
  ),
  
  actionButton("show_solu", solubtn_label, class = "btn-primary", icon = icon("calculator")),
  
  hr(),
  withMathJax(),
  htmlOutput("solu_ui")
)