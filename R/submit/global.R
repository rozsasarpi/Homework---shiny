# in general this requires no modification
#
# warn.conflicts = F
# because sometimes the warning message is printed to the htm site even with echo = F


library(shiny, warn.conflicts = F)
library(shinyjs, warn.conflicts = F)
library(dplyr, warn.conflicts = F)

# ugly
wd = getwd()
# wd = dirname(dirname(wd))

source(file.path(wd, "R/get_compound_par.R"))
source(file.path(wd, "R/submit/save_input.R"))
source(file.path(wd, "R/submit/check_input_type.R"))
source(file.path(wd, "R/submit/clean_input.R"))

load(file = file.path(wd, "data/solu_data.RData"))

## Language ---------------------------------------------
language = readRDS(file = 'data/language.RData')

if (language == 'hun'){
  problem_txt      = ". Feladat: "
  subproblem_txt   = ", reszfeladat: "
  point_txt        = " pont)"

  submitbtn_label  = "Beküldés"
  submit_msg       = "Feltöltés..."
  
  unknownneptun_msg = ":'( Sikertelen beküldés! A megadott neptun kód nem szerepel az adatbázisban."
  notnumber_msg    = ":'( Sikertelen beküldés! Egy vagy több megadott megoldás nem számformátumú."
  success_msg      = "Helyes formátumú adatok! Feltöltés..."
  save_msg         = "Mentettük az adatokat! :) Ellenőrizze a megoldásait, zöld = helyes, piros = hibás. A megoldások határidőig felülírhatók."
  
  sheet_key        = "1OXWXQnjyNTxhZ0rzAvGRAoffrWENF9XhrCcoavfr2mA"
  
} else if (language == 'eng') {
  problem_txt      = ". Task: "
  subproblem_txt   = ", subtask: "
  point_txt        = " point(s))"
  
  submitbtn_label  = "Submit"
  submit_msg       = "Uploading..."
  
  unknownneptun_msg = ":'( Submission failed! Unknown Neptun code."
  notnumber_msg    = ":'( Submission failed! One or more provided answers are not numbers."
  success_msg      = "Valid data format and neptun code! Uploading..."
  save_msg         = "Your answers have been saved! :) Check them: green = correct, red = wrong. The answers can be overwritten until the deadline."
  
  sheet_key        = "1GIZggl8omi5cekHeb7gPrGSY-srKToH2NOQzTQCRFAc"
}
## ------------------------------------------------------

compound = get_compound_par(solu_data)
comp_solu_names = compound$names
comp_solu_data = compound$data

# create textInput() element for the jj_th input variable
user_input <- function(ss, comp_solu_names) {
  c_par     = comp_solu_names$comp_parameter[ss]
  par       = comp_solu_names$parameter[ss]
  subprob_l = comp_solu_names$subproblem_l[ss]
  dim       = comp_solu_names$dim[ss]
  point     = comp_solu_names$point[ss]
  
  if (subprob_l != ""){
    textInput(c_par, label = paste(par, " ", dim, subproblem_txt, subprob_l, ", (",  point, point_txt, sep = ""))
  } else {
    textInput(c_par, label = paste(par,  " ", dim, ", (",  point, point_txt, sep = ""))
  }
}

