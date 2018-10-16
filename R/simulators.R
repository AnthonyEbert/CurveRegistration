

#' @export
simulator_1 <- function(t, param = c(1, -4, 2, 1, 0.1)){

  a     <- param[1]
  b     <- param[2]
  c     <- param[3]
  d     <- param[4]
  sigma <- param[5]

  y <- a * exp(-exp(b) * t) * sin(c + d * t) + rnorm(length(t), 0, sigma)

  return(y)
}


#' @export
simulator_sGaussian <- function(t, param = c(2, 5, 1, 0.01), alpha = c(10, 21, 25, 40, 50), basel = 0){

  mean_global   <- param[1]
  sigma_a       <- param[2]
  sigma_global  <- param[3]
  sigma_e       <- param[4]

  a       <- rnorm(length(alpha), mean_global, sigma_a)

  y <- rep(NA, length(t))

  for(i in 1:length(t)){
    y[i] <- sum(dnorm(t[i], mean = a + alpha, sd = sigma_global)) + rnorm(1, 0, sigma_e) + basel * t[i]
  }

  return(y)
}




#' @export
sim_sGaussian <- function(theta, distance_args){

  x <- paper3depfromP::simulator_sGaussian(distance_args$t, param = c(distance_args$theta, theta), alpha = distance_args$alpha)

  x_df   <- data.frame(t = distance_args$t, y = x)

  y_df   <- data.frame(t = distance_args$t, y = distance_args$y)

  if(distance_args$registration == 1){
    out <- paper3depfromP::RegisteredDistance_4(y_df, x_df, var = distance_args$var, threshold = 6, method = distance_args$method, y_kmmd = distance_args$y_kmmd)$func_list$elast_dist
  }

  if(distance_args$registration == 2){
    out <- EasyMMD::MMD(distance_args$y_df, as.matrix(x_df), y_kmmd = distance_args$y_kmmd, bias = TRUE, var = distance_args$var, threshold = 6)
  }

  if(distance_args$registration == 3){
    out <- paper3depfromP::RegisteredDistance_4(y_df, x_df, var = distance_args$var, threshold = 6, method = distance_args$method, y_kmmd = distance_args$y_kmmd)[[1]]
  }

  return(as.numeric(out))
}
