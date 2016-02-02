check_input_type = function(input, comp_names){
  ## Validate the input values, e.g. variable type check
  ##
  ##NOTES:
  ## * all input is expected to be numeric
  ## * accept '.' and ',' as decimal symbol as well
  ## * if the input is not numerical 'NA' value is attached to it
  
  n_par = dim(comp_names)[1]
  par   = as.character(comp_names$comp_parameter)
  
  # get the 'input' values using compound parameter naming
  user_input = lapply(par, function(i) input[[i]])

  idx = user_input == ''
  # convert ',' decimal symbol to '.'
  # check only the inputs which are provided (original 'NA's are left alone)
  user_input = as.numeric(gsub(",", ".", gsub("\\.", "",user_input[!idx])))

  return(!any(is.na(user_input)))
}