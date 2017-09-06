momentumCalculator <- function(vec, fun.name = ""){
  
  len <- length(vec)
  
  if(nchar(fun.name) == 0 | fun.name == "halving"){
    
    w <- 1/2^(0:(len-1))
    
    mom <- sapply(1:length(vec), function(i){
      sum(w[1:i] * vec[1:i]) / sum(w[1:i])
    })
  }
  
  
  
  return(mom)
}