add2dataframe = function(parameter, dim, problem_n, subproblem_l, dataframe, point = NA){
  ## Add new elements to dataframe
  ##
  ##SYNOPSYS
  ##
  ## df_bind = add2input_data(parameter, dim, problem_n, subprob_n, dataframe)
  ##
  ##
  ##INPUT
  ## parameter    vector of elements to be sampled
  ## dim          dimension of parameter value /character/
  ## problem_n    problem number
  ## subproblem_l sub-problem letter
  ## dataframe    dataframe (for first step use only the dataframe with neptun codes)
  ## 
  ##
  ##OUTPUT
  ## df_bind      dataframe, parameter and input dataframe row binded
  ##
  
  n  = length(parameter)
  df = data.frame(Neptun = unique(dataframe$Neptun), problem_n = as.factor(problem_n), subproblem_l = as.factor(subproblem_l),
                  parameter = deparse(substitute(parameter)), value = parameter, dim = dim, point = point)

 
  if (ncol(dataframe) > 1){ # WARNING!
    df_bind = rbind(dataframe, df)
  } else {
    df_bind = df
  }
  
  return(df_bind)
   
}