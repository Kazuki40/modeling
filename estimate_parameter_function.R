estimate_parameter_function <- function(estimate_para_func, para_0) {
  #maximization algorithm
  estimate_result <-
    optim(
      par = para_0,
      fn = estimate_para_func,
      gr = NULL,
      method = "Nelder-Mead",
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
  
  
  
  #solve :inverse matrix
  #diag :diagonal component
  #sqrt : root
  #sd
  sd_valuse <- (sqrt(-diag(solve(
    estimate_result$hessian
  ))))
  
  #t-test
  t_valuse <-
    estimate_result$par / sd_valuse
  
  #Initial log-likelihood
  ln_likelihood_0 <- estimate_para_func(para_0)
  
  #Final log-likelihood
  ln_likelihood_last <- estimate_result$value
  
  #likelihood ratio
  #McFadden
  likelihood_ratio <-
    ((ln_likelihood_0 - ln_likelihood_last) / ln_likelihood_0)
  
  #modified likelihood ratio
  modified_likelihood_ratio <-
    ((ln_likelihood_0 - (ln_likelihood_last - length(para_0))) / ln_likelihood_0)
  
  #cox and snell
  cox_snell <-
    1 - (exp(ln_likelihood_0) / exp(ln_likelihood_last)) ^ (2 / data_length)
  
  #Nagelkerke
  nagelkerke <-
    cox_snell / (1 - (exp(ln_likelihood_0)) ^ (2 / data_length))
  
  #deviance
  D <- -2 * (ln_likelihood_0 - ln_likelihood_last)
  D_p <- 1 - pchisq(D, length(para_0))
  
  #AIC
  aic <- -2 * ln_likelihood_last + 2 * length(para_0)
  
  #BIC
  bic <-
    -2 * ln_likelihood_last + length(para_0) * log(data_length)
  
  #summary
  answer <-
    list(
      estimate_result,
      data_length,
      ln_likelihood_0,
      ln_likelihood_last,
      estimate_result$par,
      t_valuse,
      sd_valuse,
      D,
      D_p,
      likelihood_ratio,
      modified_likelihood_ratio,
      cox_snell,
      nagelkerke,
      aic,
      bic
    )
  
  #name
  names(answer) <-
    c(
      "all",
      "smple_size",
      "LL_0",
      "LL_L",
      "estimate",
      "t",
      "sd",
      "D",
      "D_p",
      "McFadden",
      "M_L_ratio",
      "cox_snell",
      "nagelkerke",
      "aic",
      "bic"
    )
  
  return (answer)
  
}