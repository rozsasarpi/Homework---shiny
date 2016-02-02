library(dplyr, warn.conflicts = F)
library(DT, warn.conflicts = F)
source("R/get_compound_par.R")

## Load prepared input and corresponding solution data /initialize.R/
load(file = "data/input_data.RData")
load(file = "data/solu_data.RData")



## Get compound type representation of both datasets
compound         = get_compound_par(input_data)
comp_input_names = compound$names
comp_input_data  = compound$data

solu_par = filter(solu_data, Neptun == solu_data$Neptun[1])[,c(-1,-5)]

compound         = get_compound_par(solu_data)
comp_solu_names  = compound$names



## Datatable options
DT_control  = vector("list", 2)
# for input parameters
DT_options  = options = list(searching = F, ordering = F, paging = F, info = F, autowidth = F)

## Language ---------------------------------------------
language = readRDS(file = 'data/language.RData')

if (language == 'hun'){
  DT_rownames = c('részfeladat', 'paraméter', 'érték', 'dimenzió')
  DT_caption  = 'Kiindulási adatok:'
} else if (language == 'eng') {
  DT_rownames = c('subtask', 'parameter', 'value', 'dimension')
  DT_caption  = 'Input values:'
}
## ------------------------------------------------------

DT_class    = 'display'

DT_control$input$options  = DT_options
DT_control$input$rownames = DT_rownames
DT_control$input$caption  = DT_caption
DT_control$input$class    = DT_class

# for required solution
DT_options   = DT_options
## Language --------------------------------------------- 
if (language == 'hun'){
  DT_rownames  = c('részfeladat', 'paraméter', 'dimenzió', 'pont')
  DT_caption   = 'Feltöltendő eredmények:'
} else if (language == 'eng') {
  DT_rownames = c('subtask', 'parameter', 'dimension', 'point')
  DT_caption  = 'Required results:'
}
## ------------------------------------------------------

DT_class     = "display"

DT_control$solu$options  = DT_options
DT_control$solu$rownames = DT_rownames
DT_control$solu$caption  = DT_caption
DT_control$solu$class    = DT_class

### FUNCTIONS ---------------------------------------------------------------
## 
# Required output
req_datatable <- function(prob_n){
  
  if (any(solu_par$problem_n == prob_n)) {
    dt = filter(solu_par, problem_n == prob_n)
    
    # omit subproblem_l row if there is no subproblem for the given problem_n
    if (all(dt$subproblem_l == "")){
      req_dt = t(dt[,c('parameter', 'dim', 'point')])
      
      datatable(req_dt, 
                options = DT_control$solu$options, class = DT_control$solu$class, caption = DT_control$solu$caption,
                colnames = rep('',sum(comp_solu_names$problem_n == prob_n)), rownames = DT_control$solu$rownames[-1])
    } else {
      req_dt = t(dt[,c('subproblem_l','parameter', 'dim', 'point')])
      
      datatable(req_dt, 
                options = DT_control$solu$options, class = DT_control$solu$class, caption = DT_control$solu$caption,
                colnames = rep('',sum(comp_solu_names$problem_n == prob_n)), rownames = DT_control$solu$rownames)
    }
    
  } else NULL
}

# Personalized user input
in_datatable <- function(prob_n, user_neptun){
  
  if (any(input_data$Neptun == user_neptun)){
    
    dt = filter(input_data, Neptun == user_neptun & problem_n == prob_n)
    
    # omit subproblem_l row if there is no subproblem for the given problem_n
    if (all(dt$subproblem_l == "")){
      in_dt = t(dt[,c('parameter', 'value', 'dim')])
      
      datatable(in_dt, 
                options = DT_control$input$options, class = DT_control$input$class, caption = DT_control$input$caption,
                colnames = rep('',sum(comp_input_names$problem_n == prob_n)), rownames = DT_control$input$rownames[-1])
    } else {
      in_dt = t(dt[,c('subproblem_l', 'parameter', 'value', 'dim')])
      
      datatable(in_dt, 
                options = DT_control$input$options, class = DT_control$input$class, caption = DT_control$input$caption,
                colnames = rep('',sum(comp_input_names$problem_n == prob_n)), rownames = DT_control$input$rownames)
    }
    
  } else NULL
}



