# library -----------------------------------------------------------------

source("estimate_parameter_function.R")

# preparation -------------------------------------------------------------

data <- read.csv("file.csv", header = TRUE)

para_0 <- numeric(7)

data_length <- length(data[, 1])

# function ----------------------------------------------------------------

NL_func <- function(para) {
  ans <- 0
  i <- 0
  
  for (i in 1:data_length) {
    #second step
    V_1a <- para[1]
    V_1b <- para[2]
    
    V_2c <- para[3]
    V_2d <- para[4]
    
    #first step
    #log-sum
    log_sum_func <- function(z) {
      ans <- exp(z) / (1 + exp(z))
      return (ans)
    }
    
    #ログサム変数はパラドックスを防ぐために導入
    #値は0-1の範囲であること
    V_1_indect <- log_sum_func(para[7]) * log(exp(V_1a) + exp(V_1b))
    V_2_indect <- log_sum_func(para[7]) * log(exp(V_1a) + exp(V_1b))
    
    V_1 <- para[5] + V_1_indect
    V_2 <- para[6] + V_2_indect
    
    #probablity
    P_1 <- exp(V_1) / (exp(V_1) + exp(V_2))
    P_2 <- exp(V_2) / (exp(V_1) + exp(V_2))
    
    P_a_1 <- exp(V_1a) / (exp(V_1a) + exp(V_1b))
    P_b_1 <- exp(V_1b) / (exp(V_1a) + exp(V_1b))
    
    P_c_2 <- exp(V_2c) / (exp(V_2c) + exp(V_2d))
    P_d_2 <- exp(V_2d) / (exp(V_2c) + exp(V_2d))
    
    #joint probability
    P_a <- P_a_1 * P_1
    P_b <- P_b_1 * P_1
    P_c <- P_c_2 * P_2
    P_d <- P_d_2 * P_2
    
    likelihood <- P_a * data +
      
      
      
      ans <- ans + log(likelihood)
    
    
    
  }
  
  
  return(ans)
  
}

# optim -------------------------------------------------------------------

result <- estimate_parameter_function(NL_func, para_0)
result
