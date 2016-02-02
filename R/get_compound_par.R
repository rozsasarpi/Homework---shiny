get_compound_par = function(data){
  ## Generate compound variable naming for data
  ##
  ##NOTES:
  ## * compound = the problem number and variable name are also indicated in the compound naming
  ##   This is required to get unique name for shiny input variables even if problems have variable(s)
  ##   with the same name
  ## * naming convention:
  ##      'p1.a.F' where 'p1' is refering to problem number, 'a' is to subproblem letter, and 'F' is to variable, '.' is the separator
  
  library(dplyr)
  
  probs  = unique(levels(data$problem_n))
  n_prob = length(probs)
  n_par  = length(unique(paste(data$parameter, data$problem_n, data$subproblem_l)))
  
  comp_parameter = vector(length = n_par)
  parameter      = vector(length = n_par)
  point          = vector(length = n_par)
  dim            = vector(length = n_par)
  prob_n         = vector(length = n_par)
  subprob_l      = vector(length = n_par)
  pp = 1
  
  # loop over problems
  for (ii in 1:n_prob){
    prob_ii  = probs[ii]
    
    subprob_ii = as.character(unique(filter(data, problem_n == prob_ii)$subproblem_l))
    n_subprob_ii = length(subprob_ii)
    
    # loop over subproblems
    for (jj in 1:n_subprob_ii) {
      
      data_jj = filter(data, problem_n == prob_ii & subproblem_l == subprob_ii[jj])
      
      par_jj   = as.character(unique(data_jj$parameter))
      subprob_jj   = sapply(par_jj, function(x) as.character(data_jj[data_jj$parameter == x, ]$subproblem_l[1]))
      
      point_jj = sapply(par_jj, function(x) as.character(data_jj[data_jj$parameter == x, ]$point[1]))
      dim_jj   = sapply(par_jj, function(x) as.character(data_jj[data_jj$parameter == x, ]$dim[1]))
      n_par_jj = length(par_jj)
    
      # loop over parameters
      for (kk in 1:n_par_jj) {
        
        # !! shinyjs is not working with 'id' containing '.' thus they are replaced by '-'
        comp_parameter[pp]  = paste('p',prob_ii, '_', subprob_jj[kk], '_', par_jj[kk], sep = "")
        parameter[pp]       = par_jj[kk]
        
        point[pp]           = point_jj[kk]
        dim[pp]             = paste('[' , dim_jj[kk], ']', sep = "")
        subprob_l[pp]       = subprob_ii[jj]
        prob_n[pp]          = prob_ii
        
        pp = pp + 1
      }
    }
  }
  
  compound_names = data.frame(comp_parameter = comp_parameter, parameter = parameter, 
                              problem_n = prob_n, subproblem_l = subprob_l,  
                              dim = dim, point = point)
  
  # Restructure the dataframe using the compound parameter names (one row is corresponding to one neptun code)
  neptun = as.character(unique(data$Neptun))
  
  M = sapply(neptun, function(x) filter(data, Neptun == x)$value)
  # M = vapply(neptun, function(x) filter(data, Neptun == x)$value, FUN.VALUE = double(lengt(comp_parameter)))
  
  compound_dataframe = as.data.frame(t(M))
  names(compound_dataframe) = compound_names$comp_parameter
  compound_dataframe = cbind(data.frame(Neptun = neptun), compound_dataframe)
  
  # OUTPUT
  compound = list(names = compound_names, data = compound_dataframe)
}
