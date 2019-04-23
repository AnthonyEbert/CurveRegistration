
#' @export
loss_sG <- function(param, inp){

  obs <- inp$obs

  sim <- simulator_sGaussian(inp$Time, param = param, alpha_norm = inp$alpha_norm, alpha_cauchy = inp$alpha_cauchy, mean_global = inp$mean_global, sigma_a = inp$sigma_a)

  out <- distance_fun(obs, sim, registration = inp$registration, distance = inp$distance, method = inp$method, y_kmmd = inp$y_kmmd, var = inp$var, threshold = 6)

  return(as.numeric(out))
}

#' @export
simulator_sGaussian <- function(Time, param = c(1, 0.7, 0.01), alpha_norm = c(20, 60), alpha_cauchy = c(40, 80), mean_global = 0, sigma_a = 5){

  sigma_global  <- param[1]
  scale_global  <- param[2]
  sigma_e       <- param[3]

  mean_global   <- ifelse(is.na(param[4]), mean_global, param[4])
  sigma_a       <- ifelse(is.na(param[5]), sigma_a, param[5])

  a_norm        <- rnorm(length(alpha_norm), mean_global, sigma_a)
  a_cauchy      <- rnorm(length(alpha_cauchy), mean_global, sigma_a)

  y <- rep(NA, length(Time))

  for(i in 1:length(Time)){
    y[i] <- sum(dnorm(Time[i], mean = a_norm + alpha_norm, sd = sigma_global)) +
      sum(dcauchy(Time[i], location = a_cauchy + alpha_cauchy, scale = scale_global)) +
      rnorm(1, 0, sigma_e)
  }

  output <- as.matrix(data.frame(Time = Time, Output = y))

  return(output)
}











