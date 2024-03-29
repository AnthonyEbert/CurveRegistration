library(CurveRegistration)
library(ggplot2)
library(dplyr)
library(tidyr)

load("../../runs/hydrology/3/ABC_hydro_syn.RData")
true_color <- "black"

true_params <- c(257, 1, 88, 10, 0.05)
param_names <- names(ABC_hydro_syn)[1:5]

hydro_syn_tidy <- ABC_hydro_syn %>%
  tidyr::gather("parameter", ,-data, -registration, -distance) %>%
  mutate(registration = factor(ifelse(registration, "Registered", "Unregistered"))) %>%
  mutate(registration = factor(registration, levels(registration)[c(2,1)]))

hydro_syn_tidy$parameter <- factor(hydro_syn_tidy$parameter, labels = c("sigma", "theta[1]", "theta[2]", "theta[3]", "theta[4]"))

vline_df = data.frame(parameter = param_names, input = true_params, value = rep(1, 5))

vline_df$parameter <- factor(vline_df$parameter, labels = c("sigma", "theta[1]", "theta[2]", "theta[3]", "theta[4]"))

blank_df <- data.frame(parameter = rep(levels(hydro_syn_tidy$parameter), each = 2), value = c(0.01, 0.08, 100, 1200, -5, 3, 20, 3, 1.1, 30), registration = rep(hydro_syn_tidy$registration[1], 10))

out <- ggplot(hydro_syn_tidy %>% filter(distance == "MMD")) +
  aes(x = value, col = registration, linetype = registration) +
  stat_density(position = "identity", geom = "line") +
  facet_wrap(~parameter, scales = "free", labeller = label_parsed) +
  scale_y_continuous(expand = c(0.02, 0), limits = c(0, NA)) +
  scale_x_continuous(expand = c(0, 0), limits = c(NA, NA)) +
  geom_vline(data = vline_df , mapping = aes(xintercept = input), col = true_color, linetype = 1) +
  ggthemes::theme_few() +
  ylab(latex2exp::TeX('$\\pi_{ABC} (\\theta | y)$')) +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(), panel.spacing.x = unit(2, "lines"), legend.title = element_blank(), axis.title.x = element_blank() , legend.position = "bottom" , plot.margin = unit(c(0, 2, 0, 0),  "lines"), aspect.ratio = 1) +
  geom_blank(data = blank_df) +
  scale_color_manual(values = c("grey", "black")) +
  scale_linetype_manual(values = c(2, 4)) +
  guides(color=guide_legend(), linetype = guide_legend())

ggsave(filename = "HydroPost-2.pdf", width = 13, height = 10, units = "cm", plot = out, scale = 1.5)
