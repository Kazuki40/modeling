# library -----------------------------------------------------------------

source("estimate_parameter_function.R")

# preparation -------------------------------------------------------------

data <- read.csv("file.csv", header = TRUE)

para_0 <- numeric(3)

data_length <- length(data[, 1])

# model -------------------------------------------------------------------

tobit <- function(para) {
  ans <- 0
  i <- 0
  for (i in 1:data_length) {
    temp_y <- para[1] * data$x[i] + para[2]
    temp_sd <- exp(para[3])
    lnlikelihood <- 0
    
    if (data$y[i] > 0) {
      #ämó¶ñßìxä÷êî
      lnlikelihood <- dnorm((data$y[i] - temp_y),
                            mean = 0,
                            sd = temp_sd,
                            log = TRUE)
      
    } else if (data$y[i] <= 0) {
      #ó›êœï™ïzä÷êî
      lnlikelihood <- log(1 - pnorm(temp_y, mean = 0, sd = temp_sd))
      
    }
    
    ans <- ans + lnlikelihood
    
  }
  return (ans)
}