library(airGR)
library(ggplot2)
library(dplyr)
data(L0123001)

InputsModel <-
  CreateInputsModel(
    FUN_MOD = RunModel_GR4J,
    DatesR = BasinObs$DatesR,
    Precip = BasinObs$P,
    PotEvap = BasinObs$E
  )

Ind_Run <-
  seq(which(format(BasinObs$DatesR, format = "%d/%m/%Y") == "01/03/1997"),
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

OutputsModel <-
  RunModel_GR4J(InputsModel = InputsModel,
                RunOptions = RunOptions,
                Param = Param[1:4])

#plot(OutputsModel, Qobs = BasinObs$Qmm[Ind_Run], which = c("Precip", "Flows"))

Hydrodata <- BasinObs[Ind_Run,] %>% mutate(Flowrate = as.factor("Flow rate"))

out <- ggplot(Hydrodata) +
  geom_line(aes(x = DatesR, y = P/4, group = "Rainfall", col = E), size = 0.5) +
  geom_line(aes(x = DatesR, y = Qmm, linetype = Flowrate), col = "black") +
  scale_linetype_manual(values = "dashed") +
  ggthemes::theme_few() +
  scale_x_datetime(breaks = scales::pretty_breaks(12), date_labels = "%b") +
  scale_y_continuous(sec.axis = sec_axis(~.*4, name = "Rainfall (mm/day)")) +
  labs(y = "Flow rate (mm/day)") +
  theme(legend.position = "top", axis.title.x = element_blank()) +
  scale_color_gradient2(midpoint = 2, low = "white", mid = "red", high = "darkred") +
  guides(col = guide_colourbar(title = "Evaporation"), linetype = guide_legend(title = element_blank()))

ggsave(filename = "HydroExample-2.pdf", width = 13, height = 9, units = "cm", plot = out, scale = 1.5)

