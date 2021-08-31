library(CurveRegistration)
library(ggplot2)
library(dplyr)
library(tidyr)

load("../../runs/skew/14/ABC_skew.RData")
true_color <- "black"

output_tidy <- tidyr::gather(ABC_skew, "parameter", , -registration, -sigma_a, -distance)

output_tidy$parameter[which(output_tidy$parameter == "sigma[phi]")] <- "sigma[SN]"

# output_tidy$parameter <- factor(output_tidy$parameter, labels = c("rho[phi]", "sigma[epsilon]", "sigma[phi]"))

true_params = c(1, 0.01, 0.5)

vline_df = data.frame(parameter = c("sigma[SN]", "sigma[epsilon]","eta"), input = true_params, value = c(1, 1, 1))

blank_df <- data.frame(parameter = c("sigma[SN]", "sigma[epsilon]","eta", "eta"), value = c(3,0.005,1,-1))

output_tidy <- output_tidy %>%
  mutate(distance = factor(distance)) %>%
  mutate(distance = factor(distance, levels(distance)[c(2,1)])) %>%
  mutate(registration = factor(ifelse(registration, "Registered", "Unregistered"))) %>%
  mutate(registration = factor(registration, levels(registration)[c(2,1)]))

output_tidy$parameter <- factor(output_tidy$parameter, levels = c("sigma[SN]", "eta", "sigma[epsilon]"))

output_tidy2 <- tidyr::unite(output_tidy, "united", c("registration", "distance"))

output_tidy2$united <- factor(output_tidy2$united, levels = c("Unregistered_FR", "Unregistered_MMD", "Registered_FR", "Registered_MMD"), labels = c("Unregistered Fisher-Rao", "Unregistered MMD", "Registered Fisher-Rao", "Registered MMD"))

#blank_df <- data.frame(parameter = rep(levels(output_tidy$parameter), each = 2), value = c(0, 0.02, 0, 3), registration = rep(output_tidy$registration[1], 4), distance = rep(output_tidy$distance[1], 4))

out <- ggplot(output_tidy2) +
  aes(x = value, col = united, linetype = united) +
  stat_density(position = "identity", geom = "line", adjust = 2) +
  facet_wrap(~parameter, scales = "free", labeller = label_parsed) +
  geom_vline(data = vline_df , mapping = aes(xintercept = input), col = true_color, lty = 1) +
  scale_y_continuous(expand = c(0.02, 0), limits = c(0, NA)) +
  scale_x_continuous(expand = c(0, 0), limits = c(NA, NA)) +
  ggthemes::theme_few() +
  ylab(latex2exp::TeX('$\\pi_{ABC} (\\theta | y)$')) +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(), panel.spacing.x = unit(2, "lines"), legend.title = element_blank(), axis.title.x = element_blank() , legend.position = "bottom" , plot.margin = unit(c(0, 2, 0, 0),  "lines"), aspect.ratio = 1) +
  geom_blank(data = blank_df, mapping = aes(x = value), inherit.aes = FALSE) +
  scale_color_manual(values = c("grey", "grey", "black", "black")) +
  scale_linetype_manual(values = c(2, 4, 2, 4)) +
  guides(color=guide_legend(), linetype = guide_legend())

ggsave(filename = "skewPost-2.pdf", width = 13, height = 6, units = "cm", plot = out, scale = 1.5)
