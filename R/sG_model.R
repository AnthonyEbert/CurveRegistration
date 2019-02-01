
#' @export
loss_sG <- function(param, inp){

  obs <- inp$obs

  sim <- simulator_sGaussian(inp$Time, param = param, alpha = inp$alpha, mean_global = inp$mean_global, sigma_a = inp$sigma_a)

  out <- distance_fun(obs, sim, registration = inp$registration, distance = inp$distance, method = inp$method, y_kmmd = inp$y_kmmd, var = inp$var, threshold = 6)

  return(as.numeric(out))
}

#' @export
simulator_sGaussian <- function(Time, param = c(1, 0.01, 0.7), alpha = c(10, 21, 25, 40, 50), basel = 0, mean_global = 0, sigma_a = 5){

  sigma_global  <- param[1]
  sigma_e       <- param[2]
  scale_global  <- param[3]

  mean_global   <- ifelse(is.na(param[4]), mean_global, param[4])
  sigma_a       <- ifelse(is.na(param[5]), sigma_a, param[5])

  a       <- rnorm(length(alpha), mean_global, sigma_a)

  y <- rep(NA, length(Time))

  for(i in 1:length(Time)){
    y[i] <- sum(density_fun(Time[i], mean = a + alpha, scale_param = c(sigma_global, scale_global), i = i)) + rnorm(1, 0, sigma_e) + basel * Time[i]
  }

  output <- as.matrix(data.frame(Time = Time, Output = y))

  return(output)
}

density_fun <- function(x, mean, scale_param, i){

  output <-
    dnorm(x, mean = mean, sd = scale_param[1]) +
    dcauchy(x, location = mean + 20, scale = scale_param[2])

  return(output)

}










