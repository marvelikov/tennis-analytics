momentumCalculator <- function(vec, fun.name = NULL){
  
  len <- length(vec)
  
  if(is.null(fun.name) | fun.name == "halving"){
    
    w <- 1/2^(0:(len-1))
    
    mom <- sapply(1:length(vec), function(i){
      sum(w[1:i] * vec[1:i]) / sum(w[1:i])
    })
  }
  
  
  
  return(mom)
}