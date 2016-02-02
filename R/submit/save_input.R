save_input = function(input, comp_names, sheet_key, sum_points){
 ## Fill and save submitted data into database
  
  library(googlesheets, warn.conflicts = F)
  # library(magrittr, warn.conflicts = F)
  
  n_par = dim(comp_names)[1]
  par   = as.character(comp_names$comp_parameter)
  
  # fill the student_solu dataframe with submitted values
  # using compound parameter naming
  student_solu = data.frame(Neptun = input$neptun)
  
  val = lapply(par, function(i) input[[i]])
  # str(val)
  # convert input number with ',' to one with '.'
  idx = is.na(as.numeric(val))
  val[!idx] = as.numeric(val[!idx])
  val[idx] = as.numeric(gsub(",", ".", gsub("\\.", "", val[idx])))
  val = data.frame(t(val))
  names(val) = par
  # str(val)
  student_solu = cbind(student_solu, val)
  student_solu$timestamp = as.character(Sys.time())
  student_solu$sum_points = sum_points
  
  # add to submit_data collector dataframe
#   if (exists('submit_data')){
#     submit_data <<- rbind(submit_data, student_solu)
#   } else {
#     submit_data <<- student_solu
#   }
  save(student_solu, file = 'data/submit_data.RData')
  
  # print(student_solu)
  # print(sheet_key)
  # add user input to data (google sheet in the cloud)
  # print(student_solu)
  # str(student_solu)
  sheet = gs_key(sheet_key)
  gs_add_row(sheet, input = student_solu)
  # sheet_key %>% gs_key %>% gs_add_row(input = student_solu)
}