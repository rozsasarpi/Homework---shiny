randomize_int = function(min, max, n_out, n_signif = 3){
  ## Generate random elements from a contunuous interval
  ##
  ##SYNOPSYS
  ##
  ## r = randomize_int(min, max, n_out)
  ##
  ##
  ##INPUT
  ## min      lower bound of interval
  ## max      upper bound of interval
  ## n_out    number of elements to be generated
  ##OPTIONAL
  ## n_signif number of significant digits
  ##
  ##OUTPUT
  ## r        vector of generated random elements
  ##
  
  r = signif(min + runif(n_out)*(max-min), n_signif)
  return(r)
}