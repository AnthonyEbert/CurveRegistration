library(CurveRegistration)
library(ggplot2)
library(dplyr)
library(tidyr)

load("../../runs/airport/2/ABC_airport.RData")
true_color <- "black"

# data file ABC_airport

param_names <- c("rho", "nu", "lambda[f]", "lambda[l]")
true_params <- c(0.02, 0.64, 0.4, 0.5)

vline_df = data.frame(parameter = param_names, input = true_params, value = rep(1, 4))

airport_tidy <- ABC_airport %>% tidyr::gather("parameter", , -registration, -distance, -correction) %>%
  filter(!(!registration & correction)) %>%
  mutate(out = factor(ifelse(!registration, "Unregistered", ifelse(!correction, "Registered", "Corrected")))) %>%
  mutate(out = factor(out, levels(out)[c(3,2,1)]))

# %>% mutate(distance = paste0(distance, ifelse(registration, "", " (reg)")))

airport_tidy$parameter <- factor(airport_tidy$parameter, labels = c("lambda[f]", "lambda[l]", "rho", "nu"))

blank_df <- data.frame(parameter = rep(levels(airport_tidy$parameter), each = 2), value = c(0, 1, 0, 1, 0, 0.05, 0, 1), out = rep(airport_tidy$out[1], 4))

out <- ggplot(airport_tidy %>% filter(distance == "MMD")) +
  aes(x = value, col = out, linetype = out) +
  stat_density(position = "identity", geom = "line") +
  facet_wrap(~parameter, scales = "free", labeller = label_parsed) +
  scale_y_continuous(expand = c(0.02, 0), limits = c(0, NA)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) +
  geom_vline(data = vline_df , mapping = aes(xintercept = input), col = true_color, linetype = 1) +
  ggthemes::theme_few() +
  ylab(latex2exp::TeX('$\\pi_{ABC} (\\theta | y)$')) +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(), panel.spacing.x = unit(2, "lines"), legend.title = element_blank(), axis.title.x = element_blank() , legend.position = "bottom" , plot.margin = unit(c(0, 2, 0, 0),  "lines"), aspect.ratio = 1, axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
  geom_blank(data = blank_df) +
  scale_color_manual(values = c("grey", "black", "grey30")) +
  scale_linetype_manual(values = c(2, 4, 3)) +
  guides(color=guide_legend(), linetype = guide_legend())

ggsave(filename = "AirportPost-2.pdf", width = 13, height = 12, units = "cm", plot = out, scale = 1.5)

