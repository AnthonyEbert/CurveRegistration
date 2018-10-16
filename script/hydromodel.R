

library(airGR)
data(L0123001)

InputsModel <-
  CreateInputsModel(
    FUN_MOD = RunModel_GR4J,
    DatesR = BasinObs$DatesR,
    Precip = BasinObs$P,
    PotEvap = BasinObs$E
  )

Ind_Run <-
  seq(which(format(BasinObs$DatesR, format = "%d/%m/%Y") == "01/01/1997"),
      which(format(BasinObs$DatesR, format = "%d/%m/%Y") == "01/01/1998"))

RunOptions <- CreateRunOptions(
  FUN_MOD = RunModel_GR4J,
  InputsModel = InputsModel,
  IndPeriod_Run = Ind_Run,
  IniStates = NULL,
  IniResLevels = NULL,
  IndPeriod_WarmUp = NULL
)

Param <- c(257, 1, 88, 2.2, 0.05)

#Param <- c(115, 1.07, 128, 2.8)

OutputsModel <-
  RunModel_GR4J(InputsModel = InputsModel,
                RunOptions = RunOptions,
                Param = Param[1:4])

plot(OutputsModel, Qobs = BasinObs$Qmm[Ind_Run], which = c("Precip", "Flows"))

# InputsCrit <- CreateInputsCrit(FUN_CRIT = ErrorCrit_NSE, InputsModel = InputsModel,


# protoABC

library(protoABC)
library(parallel)
library(paper3depfromP)

## Run with MMD

prior_obj <- protoABC::prior_unif(lhs_lim = c(100, -5, 20, 1.1, 0.01), rhs_lim = c(1200, 3, 300, 2.9, 0.08), var_names = c("x1", "x2", "x3", "x4", "sigma"))

prior_eval <- protoABC::prior_unif(lhs_lim = c(100, -5, 20, 1.1, 0.01), rhs_lim = c(1200, 3, 300, 2.9, 0.08), var_names = c("x1", "x2", "x3", "x4", "sigma"), eval = TRUE)

var_mat <- diag(c(100, 1e-2))

obs_syn <- simulator_hydro(Param, InputsModel, RunOptions)


distance_args <- list(
  InputsModel = InputsModel,
  RunOptions = RunOptions,
  obs = obs_syn,
  var = var_mat,
  y_kmmd = EasyMMD::kmmd(obs_syn, var = var_mat),
  reg = FALSE
)

loss_hydro(Param, distance_args)

cov_func <- function(x){
  robust::covRob(x)$cov + 1e-7
}


n <- 5

output_sim <- protoABC::abc_start(prior_obj, loss_hydro, distance_args, method = "RABC", control = list(n = n, prior_eval = prior_eval, d_eps_final = 1e-2), cl = "mclapply")

distance_args$reg <- TRUE

output_sim_reg <- protoABC::abc_start(prior_obj, loss_hydro, distance_args, method = "RABC", control = list(n = n, prior_eval = prior_eval, d_eps_final = 1e-2), cl = "mclapply")


n_days <- length(Ind_Run)
day_num <- seq(1, n_days)

obs_true <- as.matrix(data.frame(day_num = day_num, value = BasinObs$Qmm[Ind_Run]))

distance_args$obs <- obs_true
distance_args$reg <- FALSE
distance_args$y_kmmd <- EasyMMD::kmmd(obs_true, var = var_mat)

loss_hydro(Param, distance_args)

output_true <- protoABC::abc_start(prior_obj, loss_hydro, distance_args, method = "RABC", control = list(n = n, prior_eval = prior_eval, d_eps_final = 1e-2), cl = "mclapply")

distance_args$reg <- TRUE

output_true_reg <- protoABC::abc_start(prior_obj, loss_hydro, distance_args, method = "RABC", control = list(n = n, prior_eval = prior_eval, d_eps_final = 1e-2), cl = "mclapply")



predictions_noreg <- apply(output_true, 1, simulator_hydro, InputsModel = InputsModel, RunOptions = RunOptions, one_d = TRUE)

predictions_reg <- apply(output_true_reg, 1, simulator_hydro, InputsModel = InputsModel, RunOptions = RunOptions, one_d = TRUE)

predictions_prior <- apply(prior_obj(500), 1, simulator_hydro, InputsModel = InputsModel, RunOptions = RunOptions, one_d = TRUE)

library(dplyr)


quantiles_noreg <- apply(predictions_noreg, 1, quantile, probs = c(0.25, 0.5, 0.75)) %>% t()

quantiles_reg <- apply(predictions_reg, 1, quantile, probs = c(0.25, 0.5, 0.75)) %>% t()

quantiles_prior <- apply(predictions_prior, 1, quantile, probs = c(0.01, 0.5, 0.99)) %>% t()


tidy_noreg <- quantiles_noreg %>% as_data_frame() %>% mutate(day_num = day_num) %>% mutate(reg = FALSE)

tidy_reg <- quantiles_reg %>% as_data_frame() %>% mutate(day_num = day_num) %>% mutate(reg = TRUE)

tidy_prior <- quantiles_prior %>% as_data_frame() %>% mutate(day_num = day_num)


tidy_df <- dplyr::bind_rows(tidy_noreg, tidy_reg)

library(ggplot2)

ggplot(tidy_df) + aes(x = day_num, ymin = `25%`, ymax = `75%`) + geom_ribbon(alpha = 0.5, fill = "red") + facet_wrap(~reg) + geom_line(data = as_data_frame(obs_true), mapping = aes(x = day_num, y = value), inherit.aes = FALSE)

save.image(file = "output_4.RData")

sessionInfo()
