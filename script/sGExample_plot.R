library(CurveRegistration)
library(dplyr)
library(ggplot2)


pdf(file = "sGExample-2.pdf", width = 1.5*11/2.54, height = 1.5*6/2.54)

set.seed(1)

t <- seq(0, 200, by = 0.5)
alpha = seq(20, 180, by = 20)
theta = c(1, 0.8, 0.01)

alpha_norm   <- alpha[seq(1,length(alpha), by = 2)]
alpha_cauchy <- alpha[seq(2,length(alpha), by = 2)]


y <- simulator_sGaussian(t, param = theta,
                         alpha_norm = alpha_norm, alpha_cauchy = alpha_cauchy)
z <- simulator_sGaussian(t, param = theta,
                         alpha_norm = alpha_norm, alpha_cauchy = alpha_cauchy)

plot(y, type = "l", lwd = 1, xaxs = "i", yaxs = "i", col = "grey")
lines(z, col = "black")
abline(v = alpha, lty = 2)

dev.off()
