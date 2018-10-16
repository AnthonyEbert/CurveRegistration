
#' @export
simulator_hydro <- function(param, InputsModel, RunOptions, one_d = FALSE, lambda_bc = 0.5, A_bc = 5){

  OutputsModel <-
    airGR::RunModel_GR4J(InputsModel = InputsModel,
                         RunOptions = RunOptions,
                         Param = param[1:4])

  n_days <- length(OutputsModel$Qsim)

  day_num <- seq(1, n_days)

  tvalue = box_cox(OutputsModel$Qsim, lambda_bc, A_bc) + rnorm(n_days, 0, param[5])

  output <- as.matrix(data.frame(day_num = day_num, value = inv_box_cox(tvalue, lambda_bc, A_bc)))

  if(one_d){
    output <- output[,2]
  }

  return(output)
}

#' @export
distance_hydro <- function(obs, sim, var, y_kmmd, reg = FALSE, phase_coef = 0){

  phase_dist <- 0

  if(reg){

    reg_output <- reg_hydro(obs, sim)
    sim[,2] <- reg_output$value
    phase_dist <- reg_output$phase

  }

  output <- EasyMMD::MMD(obs, sim, y_kmmd = y_kmmd, bias = TRUE, threshold = 6,  var = var) + phase_coef * phase_dist

  return(output)

}

#' @export
distance_hydro_simple <- function(obs, sim, var, y_kmmd, reg = FALSE, phase_coef = 0){

  phase_dist <- 0

  if(reg){

    reg_output <- reg_hydro(obs, sim)
    sim[,2] <- reg_output$value
    phase_dist <- reg_output$phase

  }

  output <- sum((obs - sim)^2)

  return(output)

}

#' @export
loss_hydro <- function(param, distance_args){

  sim <- simulator_hydro(param, distance_args$InputsModel, distance_args$RunOptions)

  output <- distance_hydro(distance_args$obs, sim, var = distance_args$var, y_kmmd = distance_args$y_kmmd, reg = distance_args$reg)

  return(output)
}

#' @export
reg_hydro <- function(obs, sim){

  reg_output <- fdasrvf::pair_align_functions(obs[,2], sim[,2], time = obs[,1], method = "DP2")

  values <- reg_output$f2tilde

  time1 <- seq(0, 1, length.out =  length(obs[,2]))
  phase <- acos(pracma::trapz(time1[-1], sqrt(diff(reg_output$gam) / diff(time1))))




  return(list(values = values, phase = phase))

}


#' @export
box_cox <- function(x, lambda, A){

  output <- ((x + A)^lambda - 1)/lambda

  return(output)
}

#' @export
inv_box_cox <- function(x, lambda, A){

  output <- (x * lambda + 1)^(1/lambda) - A

  return(output)
}






