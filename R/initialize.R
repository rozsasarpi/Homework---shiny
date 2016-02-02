
setwd("/srv/shiny-server/TMA-BoD/BoD/BoD_hw3")
library(dplyr)
library(googlesheets)
source("R/get_compound_par.R")
source("R/generate_input.R")
source("R/solver.R")

language         = 'eng' #'hun' or 'eng'
saveRDS(language, file = 'data/language.RData')
  
comp_input_data  = generate_input()

solu_data        = solver(comp_input_data)

compound         = get_compound_par(solu_data)
comp_solu_names  = compound$names
comp_solu_data   = compound$data
save(comp_solu_names, file = 'data/comp_solu_names.RData')
write.csv2(comp_solu_data, file = 'data/comp_solu_data.csv', row.names = FALSE)

# initialize, update google sheet for submission storage
# a header and at least one data row is needed for gs_add_row()
## Language ---------------------------------------------
if (language == 'hun'){
   sheet_key = "1OXWXQnjyNTxhZ0rzAvGRAoffrWENF9XhrCcoavfr2mA" #same key should be used in 'R/submit/global.R'
 } else if (language == 'eng') {
   sheet_key = "1GIZggl8omi5cekHeb7gPrGSY-srKToH2NOQzTQCRFAc"
 }
 ## ------------------------------------------------------
 
 sheet     = gs_key(sheet_key, lookup = T)
 
 col_names = c('Neptun', as.character(comp_solu_names$comp_parameter), 'timestamp', 'sum_points')
 gs_edit_cells(sheet, input = col_names, byrow = T)
 gs_edit_cells(sheet, input = c("DUMMY", rep(pi, length(col_names)-1)), anchor = "A2", byrow = T)

