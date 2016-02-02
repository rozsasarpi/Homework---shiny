clean_input = function(input, comp_names){
 ## Clean submitted data
  
  n_par = dim(comp_names)[1]
  par   = as.character(comp_names$comp_parameter)
  
  # fill the student_solu dataframe with submitted values
  # using compound parameter naming
  student_solu = data.frame(Neptun = input$neptun)
  
  val = lapply(par, function(i) input[[i]])
  
  # convert input number with ',' to one with '.'
  idx        = is.na(as.numeric(val))
  val[!idx]  = as.numeric(val[!idx])
  val[idx]   = as.numeric(gsub(",", ".", gsub("\\.", "", val[idx])))
  val        = as.data.frame(val)
  names(val) = par
  
  student_solu = cbind(student_solu, val)
  
  return(student_solu)
}