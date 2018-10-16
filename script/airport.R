
library(AirportSim)
library(dplyr)
library(paper3depfromP)
library(ggplot2)

cov_func <- function(x){
  robust::covRob(x)$cov
}

set.seed(3)

flight_level <- AirportSim::generate_flightlevel(5, 1000)

distance_args <- airport_control(
  registration = 0,
  flight_effect = FALSE,
  var = diag(c(2, 2)),
  method = "DP",
  threshold = 6,
  breaks = seq(0, 1000, by = 5),
  flight_level = flight_level
)

# ABC -----------

param_names <- c("mu", "vm2", "lambda_f", "lambda_l")

prior <- protoABC::prior_unif(c(0, 0, 0, 0), c(0.05, 1, 1, 1), var_names = param_names, eval = FALSE)

prior_eval <- protoABC::prior_unif(c(0, 0, 0, 0), c(0.05, 1, 1, 1), var_names = param_names, eval = TRUE)

cl <- parallel::makeCluster(parallel::detectCores() - 1)
parallel::clusterEvalQ(cl = cl, expr = library(dplyr))

abc_control <- list(
  n          = 1000,
  pacc_final = 0.025,
  prior_eval = prior_eval,
  cov_func = cov_func
)

## Synthetic data

#set.seed(1)

distance_args$sim <- TRUE
distance_args$flight_effect <- TRUE

true_params <- c(0.02, 0.64, 0.4, 0.5)

output <- distance_MMD(true_params, distance_args, sim = TRUE)

distance_args$y_full <- output

### Synthetic data, MMD -----------

# distance_args$registration <- 0
#
# syn_output_0 <- protoABC::abc_start(prior, distance_MMD, distance_args = distance_args, method = "RABC", control = abc_control, cl = cl) %>%
#   mutate(registration = 0)
#
# save(syn_output_0, file = "AirportSim_4-0.RData")

### Registration 3

# distance_args$registration <- 3
#
# syn_output_3 <- protoABC::abc_start(prior, distance_MMD, distance_args = distance_args, method = "RABC", control = abc_control, cl = cl) %>%
#   mutate(registration = 3)
#
# save(syn_output_3, file = "AirportSim_4-3.RData")

distance_args$registration <- 1

syn_output_1 <- protoABC::abc_start(prior, distance_MMD, distance_args = distance_args, method = "RABC", control = abc_control, cl = cl) %>%
  mutate(registration = 1)

save(syn_output_1, file = "AirportSim_4-1.RData")

vline_df = data.frame(parameter = param_names, input = true_params, value = rep(1, 4))





ggplot2::ggsave(file = "AirportSim_4.pdf")

save.image(file = "AirportSim_4.RData")

syn_output <- rbind(syn_output_0, syn_output_1, syn_output_3) %>%
  tidyr::gather("parameter", , -registration)


syn_output$parameter <- factor(syn_output$parameter, labels = c("lambda[foreign]", "lambda[local]", "mu", "psi"))

syn_output <- syn_output %>%
  mutate(registration = forcats::fct_relevel(factor(registration), "0", "3", "1")) %>%
  mutate(registration = forcats::fct_recode(registration, "MMD" = "0", "Registered MMD" = "3", "Elastic distance" = "1"))

vline_df$parameter <- factor(vline_df$parameter, labels = c("lambda[foreign]", "lambda[local]", "mu", "psi"))

vline_df$legend_key = "True parameter values"

blank_df <- vline_df
blank_df$input <- c(0.05, 1, 1, 1)

registration_names <- c("0" = "MMD", "3" = "Registered MMD")

ggplot(syn_output) +
  aes(x = value, col = factor(registration)) +
  stat_density(position = "identity", geom = "line") +
  facet_wrap(~parameter, scales = "free", labeller = label_parsed) +
  scale_linetype_manual(values = 1) +
  geom_vline(data = vline_df , mapping = aes(xintercept = input, lty = legend_key), col = "black") +
  labs(colour = "Registration status") +
  scale_y_continuous(expand = c(0.02, 0), limits = c(0, NA)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) +
  ggthemes::theme_few() +
  ylab(latex2exp::TeX('$\\pi_{ABC} (\\theta | y)$')) +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(), panel.spacing.x = unit(2, "lines"), legend.title = element_blank(), axis.title.x = element_blank() , legend.position = "bottom" , plot.margin = unit(c(0, 2, 0, 0),  "lines")) +
  ggthemes::scale_color_wsj() +
  geom_blank(data = blank_df, aes(x = input), inherit.aes = FALSE) +
  guides(
    color = guide_legend(order = 0),
    lty = guide_legend(order = 1)
  )

ggsave(file = "Densities_airport_4.pdf", width = 5, height = 5)



