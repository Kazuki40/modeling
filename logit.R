# library -----------------------------------------------------------------

source("estimate_parameter_function.R")

# preparation -------------------------------------------------------------

data <- read.csv("file.csv", header = TRUE)

para_0 <- numeric(3)

data_length <- length(data[, 1])

judge <- 0

# function ----------------------------------------------------------------

logit_func <- function(para) {
  ans <- 0
  i <- 0
  
  for (i in 1:data_length) {
    v1 <-
      para[1] * data$aaa[i] + para[2] * data$bbb[i] + para[3] * data$ccc[i]
    v2 <-
      para[1] * data$aa2[i] + para[2] * data$bb2[i] + para[3] * data$cc2[i]
    
    
    p1_molecule <- exp(v1)
    p2_molecule <- exp(v2)
    denominator <- p1_molecule + p2_molecule
    
    p1 <- p1_molecule / denominator
    p2 <- p2_molecule / denominator
    
    likelihood <- (p1 ^ data$y[i]) * (p2 ^ (1 - data$y[i]))
    
    if (judge == 0) {
      ans <- ans + log(likelihood)
      
      
    } else{
      if (p1 >= p2) {
    
        #p1‚ª‘å‚«‚¢
        if (data$y[i] == 1) {
          ans <- ans + 1
        }
        
        
      } else{
        #p2‚ª‘å‚«‚¢
        if (data$y[i] == 0) {
          ans <- ans + 1
        }
        
      }
    }
    
  }
  
  return(ans)
  
}

# optim -------------------------------------------------------------------

result <- estimate_parameter_function(logit_func, para_0)
result

#hitratio
judge <- 1
hit_ratio <- logit_func(result$all$par) / data_length
hit_ratio