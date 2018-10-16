# Simulator 2

library(CurveRegistration)
library(protoABC)
library(parallel)
library(plyr)
library(dplyr)

# Settings

run_number = 6

set.seed(1)

t <- seq(0, 300, by = 0.5)
alpha = seq(25, 300, by = 20)
theta = c(1, 0.01)

n_runs = 10
pacc_final = 0.05

y <- simulator_sGaussian(t, param = c(0, 20, theta), alpha = alpha)

cov_func <- function(x){
  robust::covRob(x)$cov
}

distance_args <- list(
  t = t,
  alpha = alpha,
  y = y,
  y_df = as.matrix(data.frame(t = t, y = y)),
  var = diag(c(9, 0.01^2)),
  threshold = 6,
  method = "DP2"
)

distance_args$y_kmmd <- EasyMMD::kmmd(distance_args$y_df, var = distance_args$var)

cl <- makeCluster(detectCores() - 1)










prior_sGaussian <- prior_unif(c(0, 0), c(3, 0.02), var_names = c("sigma_phi", "sigma_e"), eval = FALSE)

prior_sGaussian_eval <- prior_unif(c(0, 0), c(3, 0.02), var_names = c("sigma_phi", "sigma_e"), eval = TRUE)

abc_control <- list(prior_eval = prior_sGaussian_eval, n = n_runs, pacc_final = pacc_final, cov_func = cov_func)









fixed_params <- c(0, 5)

y <- simulator_sGaussian(t, param = c(fixed_params, theta), alpha = alpha)

distance_args <- list(
  t = t,
  alpha = alpha,
  y = y,
  y_df = as.matrix(data.frame(t = t, y = y)),
  var = diag(c(9, 0.01^2)),
  threshold = 6,
  method = "DP2",
  theta = fixed_params
)

distance_args$y_kmmd <- EasyMMD::kmmd(distance_args$y_df, var = distance_args$var)



distance_args$registration <- 1

ptm = proc.time()

n_runs = 10

output_sG_ed_5 <- abc_start(prior_sGaussian, sim_sGaussian, distance_args = distance_args, method = "RABC", control = abc_control, cl = cl) %>%
  mutate(
    n            = n_runs,
    registration = distance_args$registration,
    sigma_a      = distance_args$theta[2]
  )

#save(output_sG_ed_5, file = paste0(run_number, "_sG_ed_5.RData"))

proc.time() - ptm
ptm = proc.time()

distance_args$registration <- 2

output_sG_MMD_5 <- abc_start(prior_sGaussian, sim_sGaussian, distance_args = distance_args, method = "RABC", control = abc_control, cl = cl) %>%
  mutate(
    registration = distance_args$registration,
    sigma_a      = distance_args$theta[2]
  )

#save(output_sG_MMD_5, file = paste0(run_number, "_sG_MMD_5.RData"))

proc.time() - ptm
ptm = proc.time()

distance_args$registration <- 3

output_sG_rMMD_5 <- abc_start(prior_sGaussian, sim_sGaussian, distance_args = distance_args, method = "RABC", control = abc_control, cl = cl) %>%
  mutate(
    registration = distance_args$registration,
    sigma_a      = distance_args$theta[2]
  )

proc.time() - ptm
ptm = proc.time()

save(output_sG_rMMD_5, file = paste0(run_number, "_sG_rMMD_5.RData"))







fixed_params <- c(0, 20)

y <- simulator_sGaussian(t, param = c(fixed_params, theta), alpha = alpha)

distance_args <- list(
  t = t,
  alpha = alpha,
  y = y,
  y_df = as.matrix(data.frame(t = t, y = y)),
  var = diag(c(9, 0.01^2)),
  threshold = 6,
  method = "DP2",
  theta = fixed_params
)

distance_args$y_kmmd <- EasyMMD::kmmd(distance_args$y_df, var = distance_args$var)


distance_args$registration <- 1

output_sG_ed_20 <- abc_start(prior_sGaussian, sim_sGaussian, distance_args = distance_args, method = "RABC", control = abc_control, cl = cl) %>%
  mutate(
    registration = distance_args$registration,
    sigma_a      = distance_args$theta[2]
  )

#save(output_sG_ed_20, file = paste0(run_number, "_sG_ed_20.RData"))

proc.time() - ptm
ptm = proc.time()

distance_args$registration <- 2

output_sG_MMD_20 <- abc_start(prior_sGaussian, sim_sGaussian, distance_args = distance_args, method = "RABC", control = abc_control, cl = cl) %>%
  mutate(
    registration = distance_args$registration,
    sigma_a      = distance_args$theta[2]
  )

#save(output_sG_MMD_20, file = paste0(run_number, "_sG_MMD_20.RData"))

proc.time() - ptm
ptm = proc.time()

distance_args$registration <- 3

output_sG_rMMD_20 <- abc_start(prior_sGaussian, sim_sGaussian, distance_args = distance_args, method = "RABC", control = abc_control, cl = cl) %>%
  mutate(
    registration = distance_args$registration,
    sigma_a      = distance_args$theta[2]
  )

#save(output_sG_rMMD_20, file = paste0(run_number, "_sG_rMMD_20.RData"))

proc.time() - ptm
ptm = proc.time()


output_sG <- bind_rows(output_sG_ed_5, output_sG_MMD_5, output_sG_rMMD_5, output_sG_ed_20, output_sG_MMD_20, output_sG_rMMD_20)

#save(output_sG, file = paste0(run_number, "_sG.RData"))

