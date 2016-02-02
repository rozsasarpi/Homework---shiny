generate_input = function(){
  ## Generate random input data for homework problems as specified inside the $$$$$ block
  ##
  ## * neptun_codes.csv with neptun codes in first column should be copied to data folder
  ## * sufficent to run once
  ## * creates input_data.RData file to be used as a source by the html-shiny page
  
  # rm(list=ls(all=TRUE))
  source('R/randomize_int.R', local = T)
  source('R/randomize_set.R', local = T)
  source('R/add2dataframe.R', local = T)
  
  # set the random generator for reproducibility
  set.seed(111)
  
  # check if neptuns already exist and keep the original input 
  if (file.exists('data/previous/neptun_codes.csv')){
    # load neptun codes
    neptun_prev = as.vector(read.csv2(file = 'data/previous/neptun_codes.csv')[,1])
    neptun      = as.vector(read.csv2(file = 'data/neptun_codes.csv')[,1])
    
    load(file = 'data/input_data.RData')
    input_data_prev = input_data
     
    # position/index of the new neptun codes in the previous one (not to overwrite the previously assigned!)
    matching_neptun_idx = sapply(neptun, function(x) match(x, neptun_prev))
    
    old_neptun = neptun_prev[matching_neptun_idx[!is.na(matching_neptun_idx)]]
    new_neptun = as.data.frame(neptun[is.na(matching_neptun_idx)])
    
    input_data_new_old = filter(input_data_prev, sapply(input_data_prev$Neptun, "%in%", old_neptun))
  } else {
    new_neptun = as.data.frame(read.csv2(file = 'data/neptun_codes.csv')[,1])
  }
  
  if (length(new_neptun) != 0){
    names(new_neptun) = 'Neptun'
  }
  input_data   = new_neptun
  n_new_neptun = dim(new_neptun)[1]

  if (n_new_neptun != 0){
  ## MODIFY THIS PART $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  #
  # NOTES:
  # The input variables are stored in dataframe using the following structure:
  #   | Neptun | problem number | sub-problem letter | parameter name | parameter value | parameter dimension |
  #
  # * thus it is essential to provide the correct prob_n value and use the desired variable naming
  #   since those will be used in the dataframe
  # * make sure that add2dataframe() has the right variable argument!
  #
  
  ## Problem 1 ----------------------
  prob_n = 1
  subprob_l = 'a'
  
  F = randomize_int(min = 3, max = 15, n_out = n_new_neptun, n_signif = 2)
  l = randomize_int(min = 3, max = 4, n_out = n_new_neptun, n_signif = 2)
  L_0 = randomize_int(min = 4.1, max = 5, n_out = n_new_neptun, n_signif = 2)
  A = randomize_int(min = 100, max = 200, n_out = n_new_neptun, n_signif = 2)

  input_data = add2dataframe(F, dim = 'kN', prob_n, subprob_l, input_data)
  input_data = add2dataframe(l, dim = 'm', prob_n, subprob_l, input_data)
  input_data = add2dataframe(L_0, dim = 'm', prob_n, subprob_l, input_data)
  input_data = add2dataframe(A, dim = 'mm^2', prob_n, subprob_l, input_data) 
  
  subprob_l = 'b'
  
  
  ## Problem 2 ----------------------
  prob_n = 2
  subprob_l = ''
  
  
  a = randomize_int(min = 200, max = 300, n_out = n_new_neptun, n_signif = 2)
  b = randomize_int(min = 200, max = 300, n_out = n_new_neptun, n_signif = 2)
  
  phi_set = c(14, 16, 18, 20, 22)
  phi = randomize_set(phi_set, n_out = n_new_neptun)
  
  input_data = add2dataframe(a, dim = 'mm', prob_n, subprob_l, input_data)
  input_data = add2dataframe(b, dim = 'mm', prob_n, subprob_l, input_data)
  input_data = add2dataframe(phi, dim = 'mm', prob_n, subprob_l, input_data)
  
  
  ## Problem 3 ----------------------
  prob_n = 3
 
  h = randomize_int(min = 2.5, max = 3.5, n_out = n_new_neptun, n_signif = 2)
  p_1 = randomize_int(min = 0.5, max = 2, n_out = n_new_neptun, n_signif = 2)
  p_2 = randomize_int(min = 3, max = 5, n_out = n_new_neptun, n_signif = 2)
  y = randomize_int(min = 0.9, max = 1.5, n_out = n_new_neptun, n_signif = 2)
  
  input_data = add2dataframe(h, dim = 'm', prob_n, subprob_l, input_data)
  input_data = add2dataframe(p_1, dim = 'kN/m^2', prob_n, subprob_l, input_data)
  input_data = add2dataframe(p_2, dim = 'kN/m^2', prob_n, subprob_l, input_data)
  input_data = add2dataframe(y, dim = 'm', prob_n, subprob_l, input_data)
  
  
  ## Problem 4 ----------------------
  prob_n = 4
 
  
  ## Problem 5 ----------------------
  prob_n = 5
  subprob_l = ''
  
  F_w = randomize_int(min = 20, max = 30, n_out = n_new_neptun, n_signif = 2)
  l = randomize_int(min = 15, max = 20, n_out = n_new_neptun, n_signif = 2)
  a = randomize_int(min = 2, max = 4, n_out = n_new_neptun, n_signif = 2)
  d = randomize_int(min = 8, max = 10, n_out = n_new_neptun, n_signif = 2)
  h = randomize_int(min = 4, max = 5, n_out = n_new_neptun, n_signif = 2)
  
  input_data = add2dataframe(F_w, dim = 'kN', prob_n, subprob_l, input_data)
  input_data = add2dataframe(l, dim = 'm', prob_n, subprob_l, input_data)
  input_data = add2dataframe(a, dim = 'm', prob_n, subprob_l, input_data)
  input_data = add2dataframe(d, dim = 'm', prob_n, subprob_l, input_data)
  input_data = add2dataframe(h, dim = 'm', prob_n, subprob_l, input_data)
  
  ## Problem 6 ----------------------
  prob_n = 6
  subprob_l = ''
  
  Q = randomize_int(min = 20, max = 30, n_out = n_new_neptun, n_signif = 2)
  
  input_data = add2dataframe(Q, dim = 'kN', prob_n, subprob_l, input_data)
  
  ## Problem 7 ----------------------
  prob_n = 7
  subprob_l = ''
  
  
  
  ## Problem 8 ----------------------
  prob_n = 8
  subprob_l = ''
  
  R = randomize_int(min = 4, max = 5, n_out = n_new_neptun, n_signif = 2)
  varrho = randomize_int(min = 6, max = 8, n_out = n_new_neptun, n_signif = 2)
  t = randomize_int(min = 0.4, max = 0.8, n_out = n_new_neptun, n_signif = 2)
  f = randomize_int(min = 15, max = 25, n_out = n_new_neptun, n_signif = 2)
  p = randomize_int(min = 1200, max = 1800, n_out = n_new_neptun, n_signif = 2)
  
  input_data = add2dataframe(R, dim = 'm', prob_n, subprob_l, input_data)
  input_data = add2dataframe(varrho, dim = 'm', prob_n, subprob_l, input_data)
  input_data = add2dataframe(t, dim = 'mm', prob_n, subprob_l, input_data)
  input_data = add2dataframe(f, dim = 'N/mm^2', prob_n, subprob_l, input_data)
  input_data = add2dataframe(p, dim = 'N/m^2', prob_n, subprob_l, input_data)
  
  ## Problem 9 ----------------------
  prob_n = 9
  subprob_l = ''
  
  
  
  # Problem 10 ----------------------
  prob_n = 10
  subprob_l = ''
  
  D = randomize_int(min = 8, max = 12, n_out = n_new_neptun, n_signif = 2)
  rho = randomize_int(min = 7, max = 8, n_out = n_new_neptun, n_signif = 2)
  t = randomize_int(min = 0.4, max = 0.8, n_out = n_new_neptun, n_signif = 2)
  p = randomize_int(min = 1200, max = 1800, n_out = n_new_neptun, n_signif = 2)
  a = randomize_int(min = 700, max = 1000, n_out = n_new_neptun, n_signif = 2)
  b = randomize_int(min = 700, max = 1000, n_out = n_new_neptun, n_signif = 2)
  
  input_data = add2dataframe(D, dim = 'm', prob_n, subprob_l, input_data)
  input_data = add2dataframe(rho, dim = 'm', prob_n, subprob_l, input_data)
  input_data = add2dataframe(t, dim = 'mm', prob_n, subprob_l, input_data)
  input_data = add2dataframe(p, dim = 'N/m^2', prob_n, subprob_l, input_data)
  input_data = add2dataframe(a, dim = 'mm', prob_n, subprob_l, input_data)
  input_data = add2dataframe(b, dim = 'mm', prob_n, subprob_l, input_data)
  
  ## MODIFY THIS PART $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  }
  
  # bind the new input to the old
  if (exists('input_data_new_old')){
    input_data = rbind(input_data_new_old, input_data)
  }
  
  compound         = get_compound_par(input_data)
  comp_input_names = compound$names
  comp_input_data  = compound$data
  
  # reorder input data - alphabetical order of neptun codes
  # comp_input_data = comp_input_names[match(levels(comp_input_names$id), comp_input_names$id),]
  
  # save RData for furher use
  save(input_data, file = 'data/input_data.RData')
  message('input_data.RData has been succesfully generated!')
  
  saveRDS(comp_input_names, file = 'data/comp_input_names.RData')
  write.csv2(comp_input_data, file = 'data/comp_input_data.csv', row.names = FALSE)
  
  # 
  file.copy(from = 'data/neptun_codes.csv', to = 'data/previous/neptun_codes.csv', overwrite = TRUE)

  return(comp_input_data)
}

