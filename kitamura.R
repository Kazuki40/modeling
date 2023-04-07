# read_data ---------------------------------------------------------------

read_data <- function(filename) {
  data <- read.csv(filename,
                   header = T)
  
  return(data)
}

# mu ----------------------------------------------------------------------
mu_func <- function(max_time) {
  mu_func1 <- function(para) {
    para <- exp(para)
    d <- (para * log(max_time * para) - (1 + para) * log(1 + para))
    return(d)
  }
  mu_para <- 0
  return(uniroot(mu_func1, c(-500, 500.0))$root)
}

# para --------------------------------------------------------------------
#iter

param <- numeric(16)
max_time_h <- 24 * 60
mu <- exp(mu_func(max_time_h))
use_data <- read_data("data.csv")
data_y <- use_data$cartime_day
optim_tf <- TRUE
rand_tf <- FALSE

#data
x1 <- use_data$man
x2 <- use_data$X30year
x3 <- use_data$X40year
x4 <- use_data$X50year
x5 <- use_data$than60year
x6 <- use_data$less6million
x7 <- use_data$less10million
x8 <- use_data$less15million
x9 <- use_data$housewife
x10 <- use_data$kurumamoti
x11 <- use_data$bicyclemoti
x12 <- use_data$holiday
x13 <- use_data$Friday
x14 <- use_data$sougei

# utility_func ------------------------------------------------------------

in_v_func <- function(i, para) {
  v <-
    x1[i] * para[1] + x2[i] * para[2] + x3[i] * para[3] + x4[i] * para[4] +
    x5[i] * para[5] + x6[i] * para[6] + x7[i] * para[7] + x8[i] * para[8] +
    x9[i] * para[9] + x10[i] * para[10] + x11[i] * para[11] + x12[i] * para[12] +
    x13[i] * para[13] + x14[i] * para[14] + para[15]
  return(v)
}


# main_func ---------------------------------------------------------------

kitamura_func <- function(para) {
  #data_arrangement
  sigma <- exp(para[length(para)])
  
  #utility
  V_func <- function(i) {
    v <- in_v_func(i, para)
    return(v)
  }
  
  
  #choce_t
  choce_e <- function(i) {
    time_error <-
      (log(data_y[i] / (max_time_h - data_y[i])) - V_func(i))
    
    #probability density
    pd <- dnorm(time_error, sd = sigma) 
    
    return(pd)
    
  }
  
  #no_choce_t
  no_choce_e <- function(i) {
    time_error <- (log(mu) - V_func(i))
    #probability density
    pd <- pnorm(time_error, sd = sigma)
    
    return(pd)
  }
  
  
  #logl-ilkelihood
  choce_ll <- function(i) {
    return(log(choce_e(i)))
  }
  no_choce_ll <- function(i) {
    return(log(no_choce_e(i)))
  }
  
  ll_func <- function() {
    #iter
    ll_para <- 0
    for (i in 1:length(data_y)) {
      #use
      if (data_y[i] > 0) {
        ll_para <- choce_ll(i) + ll_para
        
      } else{
        #no
        ll_para <- no_choce_ll(i) + ll_para
        
      }
    }
    
    return(ll_para)
    
  }
  
  
  sim_func <- function() {
    result_matrix <- matrix(data = 0,
                            nrow = length(data_y),
                            ncol = 2)
    
    for (i in 1:length(data_y)) {
      if (rand_tf == TRUE) {
        ra_v <- rnorm(1, 0, sd = sigma)
      } else{
        ra_v <- 0
      }
      
      est_v <- V_func(i) + ra_v
      
      ride_time <- (-log(mu) + est_v)
      
      if (ride_time <= 0) {
        ans <- 0
      }  else{
        ans <- (exp(est_v) / (1 + exp(est_v))) * max_time_h
        
        
      }
      result_matrix[i, 1] <- ride_time
      result_matrix[i, 2] <- ans

    }
    return(result_matrix)
  }
  
  
  
  if (optim_tf == TRUE) {
    return(ll_func())
  } else{
    return(sim_func())
  }
  
}


# optim -------------------------------------------------------------------

estimate_parameter_function <-
  function(estimate_para_func, para_0) {
    #maximization algorithm
    estimate_result <-
      optim(
        par = para_0,
        fn = estimate_para_func,
        gr = NULL,
        method = "BFGS",
        lower = -Inf,
        upper = Inf,
        control = list(
          trace = 1,
          fnscale = -1,
          maxit = 10 ^ 9
        ),
        hessian = TRUE
      )
    
    #convergence test
    if (estimate_result$convergence == 0) {
      print("T")
      print(estimate_result)
    } else{
      print("F")
    }
    
    
    #t-test
    #solve :inverse matrix
    #diag :diagonal component
    #sqrt : root
    t_valuse <-
      estimate_result$par / (sqrt(-diag(solve(
        estimate_result$hessian
      ))))
    
    #Initial log-likelihood
    ln_likelihood_0 <- estimate_para_func(para_0)
    
    #Final log-likelihood
    ln_likelihood_last <- estimate_result$value
    
    #likelihood ratio
    likelihood_ratio <-
      ((ln_likelihood_0 - ln_likelihood_last) / ln_likelihood_0)
    
    #modified likelihood ratio
    modified_likelihood_ratio <-
      ((ln_likelihood_0 - (ln_likelihood_last - length(para_0))) / ln_likelihood_0)
    
    #summary
    answer <-
      list(
        estimate_result,
        t_valuse,
        ln_likelihood_0,
        ln_likelihood_last,
        likelihood_ratio,
        modified_likelihood_ratio
      )
    
    #name
    names(answer) <-
      c("estimate", "t", "LL_0", "LL_L", "L_ratio", "M_L_ratio")
    
    return (answer)
    
  }


# estimate ----------------------------------------------------------------

ans <- estimate_parameter_function(kitamura_func, param)
ans

optim_tf <- FALSE
use_data <- read_data("data.csv")
write.csv(kitamura_func(ans$estimate$par), "ans1.csv")
