


#' @export
distance_MMD <- function(params, distance_args, debug = FALSE, sim = FALSE){

  global_input <- AirportSim::global_fun_1(mu = params[1], vm2 = params[2])

  nat_input <- AirportSim::airport_list_1$nat_level
  nat_input$rate_imm <- params[3:4]

  flight_input <- distance_args$flight_level

  if(distance_args$flight_effect){
    flight_input$arrive <- runif(length(flight_input$arrive), pmax.int(0, flight_input$arrive -20), flight_input$arrive + 20)
  }

  input <- AirportSim::airport_fun_1(global_level = global_input, nat_level = nat_input, flight_level = flight_input)

  Passenger_df <- do.call(AirportSim::AirportSimulate1, input)

  xy_obs_syn <- AirportSim::post_process_1(Passenger_df)

  x_full <- AirportSim::convert_stamps_to_hist(xy_obs_syn, breaks = distance_args$breaks)

  x_ac <- x_full[which(x_full$key == "x_obs"),]
  x_imm <- x_full[which(x_full$key == "y_obs"),]

  x_ac <- x_ac[,-1]
  x_imm <- x_imm[,-1]

  if(sim){
    return(x_full)
  }

  if(distance_args$registration == 0){

    y_ac  <- distance_args$y_full[which(distance_args$y_full$key == "x_obs"), ]
    y_ac  <- y_ac[,-1]

    output_ac <- EasyMMD::MMD(as.matrix(y_ac[which(y_ac$y != 0)]), as.matrix(x_ac), var = distance_args$var, bias = TRUE, threshold = distance_args$threshold)

    y_imm <- distance_args$y_full[which(distance_args$y_full$key == "y_obs"), ]
    y_imm <- y_imm[,-1]

    output_imm <- EasyMMD::MMD(as.matrix(y_imm), as.matrix(x_imm), var = distance_args$var, bias = TRUE, threshold = distance_args$threshold)

    output <- output_ac + output_imm

    if(debug){
      output <- list(
        dist = output,
        y_df = distance_args$y_ac,
        x_df = x_ac
      )
    }





  } else if(distance_args$registration %in% c(1, 3)) {

    y_ac  <- distance_args$y_full[which(distance_args$y_full$key == "x_obs"), ]
    y_ac  <- y_ac[,-1]
    y_ac  <- y_ac[which(y_ac$t %in% distance_args$breaks),]
    x_ac  <- x_ac[which(x_ac$t %in% distance_args$breaks),]

    reg_output <- RegisteredDistance_4(y = y_ac, x = x_ac, var = distance_args$var, threshold = distance_args$threshold, method = distance_args$method)

    if(distance_args$registration == 3){
      output_ac <- reg_output$dist
    } else if(distance_args$registration == 1){
      output_ac <- reg_output$func_list$elast_dist
    }

    y_imm <- distance_args$y_full[which(distance_args$y_full$key == "y_obs"), ]
    y_imm <- y_imm[,-1]

    sim_out <- NA
    warper <- NA

    warper <- paper3depfromP::warper2(x_ac$t, reg_output$func_list$gam)

    sim_out <- Passenger_df %>%
      group_by(nat) %>%
      mutate(
        depart_imm = queuecomputer::queue(warper(arrive_imm), service_imm, server_imm[[1]])
      ) %>%
      AirportSim::post_process_1() %>%
      AirportSim::convert_stamps_to_hist(breaks = distance_args$breaks)

    x_imm <- sim_out %>%
      filter(key == "y_obs") %>%
      select(-key)


    if(distance_args$registration == 3){
      output_imm <- EasyMMD::MMD(as.matrix(y_imm), as.matrix(x_imm), var = distance_args$var, bias = TRUE, threshold = distance_args$threshold)
    } else if(distance_args$registration == 1){
      output_imm <- sqrt(pracma::trapz(x_imm$t, (fdasrvf::f_to_srvf(y_imm$y, y_imm$t) - fdasrvf::f_to_srvf(x_imm$y, x_imm$t))^2))
    }

    output <- output_ac + output_imm



    if(debug){
      output <- list(
        dist = output,
        y_df = y_ac,
        x_df = x_ac,
        x_imm = x_imm,
        y_imm = y_imm,
        reg_output = reg_output,
        sim_out = sim_out,
        warper = warper
      )
    }

  } else if(distance_args$registration == 2) {

    output <- fdasrvf::elastic.distance(x_df$y, distance_args$y_df$y, distance_args$y_df$t)[[1]]

  }


  return(output)
}








