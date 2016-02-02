# in general this requires no modification
#
# warn.conflicts = F
# because sometimes the warning message is printed to the html site even with echo = F


library(shiny, warn.conflicts = F)
library(markdown, warn.conflicts = F)

comp_input_names = readRDS('data/comp_input_names.RData')
load('data/input_data.RData')

## Language --------------------------------------------- 
language = readRDS(file = 'data/language.RData')

if (language == 'hun'){
  Problem_txt      = "Feladat"
  problem_txt      = ". feladat"
  solubtn_label    = "Megoldás"
  
  submitbtn_label  = "Beküldés"
  submit_msg       = "Feltöltés..."
  
  unknownneptun_msg = ":'( A megadott neptun kód nem szerepel az adatbázisban."
} else if (language == 'eng') {
  Problem_txt      = "Task"
  problem_txt      = ". task"
  solubtn_label    = "Solution"
  
  submitbtn_label  = "Submit"
  submit_msg       = "Uploading..."
  
  unknownneptun_msg = ":'( Unknown Neptun code."
}
## ------------------------------------------------------

# choices based on the number of problems
n_prob = max(comp_input_names$problem_n)

prob_choices = sapply(1:n_prob, function(x) paste("p", x, sep = ""))

attributes(prob_choices)$names = sapply(1:n_prob, function(x) paste(x, problem_txt, sep = ""))
