randomize_set = function(set, n_out){
  ## Generate random elements from a set
  ##
  ##SYNOPSYS
  ##
  ## r = randomize_set(set, n_out)
  ##
  ##
  ##INPUT
  ## set      vector of elements to be sampled
  ## n_out    number of elements to be generated
  ##
  ##OUTPUT
  ## r        vector of generated random elements
  ##
  
  r = sample(set, n_out, replace = T)
  return(r)
}