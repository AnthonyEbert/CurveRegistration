
#' @export
RegisteredDistance_4 <- function(y, x, method = "DP", ...){

  func_list <- register_functions(y, x, method = method)
  x_r <- func_list$x_r

  MMD_reg <- EasyMMD::MMD(as.matrix(y[which(!is.na(y$y)),]), as.matrix(x_r), bias = TRUE, ...)

  #dist_elastic <- fdasrvf::elastic.distance(y$y, x_r$y, y$t)

  return(list(dist = MMD_reg, func_list = func_list))
}

#' @export
RegisteredDistance_5 <- function(y, x, ...){

  reg_dist <- fdasrvf::elastic.distance(y$y, x$y, y$t)[[1]]

  return(reg_dist)
}


#' @export
register_functions <- function(y, x, ...){

  reg_f <- fdasrvf::pair_align_functions(y$y, x$y, x$t, ...)

  x_r <- data.frame(t = x$t, y = reg_f$f2tilde)

  elast_dist <- sqrt(pracma::trapz(x$t, (fdasrvf::f_to_srvf(y$y, y$t) - fdasrvf::f_to_srvf(x_r$y, x$t))^2))

  func_list <- list(x = x, y = y, x_r = x_r, gam = reg_f$gam, elast_dist = elast_dist)

  return(func_list)
}

#' @export
warper2 <- function(x, gam){
  ord = order(x)
  t = x[ord]
  gam = gam[ord]

  return(
    function(t_eval){

      gam2 <- t[1] + gam * (t[length(t)] - t[1])
      x <- approx(gam2, t, t_eval)$y
      x[which(is.na(x))] <- t_eval[which(is.na(x))]
      return(x)
    })
}


L2_dist <- function(x, y, tfine){

  xfine <- fda::eval.fd(tfine, x, 0)
  yfine <- fda::eval.fd(tfine, y, 0)

  output <- pracma::trapz((xfine - yfine)^2)

  return(output)
}


res_f <- function(x, x_smoothed, tfine){

  xfine <- fda::eval.fd(tfine, x_smoothed, 0)

  output <- sqrt(sum((xfine - x)^2))
  return(output)
}

#' @export
plot.regdist <- function(input, ...){

  plot(input$x)
  points(input$y, col = "red")
  lines(input$x_smoothed)
  lines(input$y_smoothed, col = "red")
  lines(input$z_smoothed, col = "blue")

}
