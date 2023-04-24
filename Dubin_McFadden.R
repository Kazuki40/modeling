# library -----------------------------------------------------------------

source("estimate_parameter_function.R")

# preparation -------------------------------------------------------------

data <- read.csv("file.csv", header = TRUE)

para_0 <- numeric(4)

data_length <- length(data[, 1])

# function ----------------------------------------------------------------

logit_func <- function(para) {
  ans <- 0
  i <- 0
  
  for (i in 1:data_length) {
    y1 <-
      (para[1] * data$aaa[i] + para[2] * data$bbb[i] + para[3] * data$ccc[i])*exp(-data$price[i]*para[4])
    y2 <-
      (para[1] * data$aaa[i] + para[2] * data$bbb[i])*exp(-data$price[i]*para[4])
    
    
    p1_molecule <- exp(y1)
    p2_molecule <- exp(y2)
    denominator <- p1_molecule + p2_molecule
    
    p1 <- p1_molecule / denominator
    p2 <- p2_molecule / denominator
    
    likelihood <- (p1 ^ data$y[i]) * (p2 ^ (1 - data$y[i]))
    
    
    ans <- ans + log(likelihood)
    
  }
  
  return(ans)
  
}

g<-function(para){
  
  x <- -(para[2]/para[1])+(para[4]/para[1]) (para[1] * data$aaa[i] + para[2] * data$bbb[i] + para[3] * data$ccc[i])
  
}

# optim -------------------------------------------------------------------

result <- estimate_parameter_function(logit_func, para_0)
result

