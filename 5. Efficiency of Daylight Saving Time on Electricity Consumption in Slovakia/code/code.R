# CODE
library('dplyr')
library(stargazer)
library(readxl)

data <- read_excel("~/IES/First semester/ECOX/data.xlsx")

# summary
summary(data)

# plot
library(ggplot2)
newdata <- data[which(data$Time==12),]
theme_set(theme_bw())
ggplot(newdata, aes(x=Average_temperature)) + 
  geom_line(aes(y=Energy_consumption), size=0.1) +
    labs(y="Energy consumption",
         x="Average temperature",
         caption="Energy consumption in MW")

# transformation of variables
lnconsump <- log(data$'Energy_consumption')
data$lnconsump <- lnconsump

# regression
dcidreg = lm(lnconcump ~ DST + treatment + DSTtreatment + Average_temperature + Temperature_squared + Pressure + Relative_humidity + Weekend + Public_holiday, data = data)
summary(dcidreg)

# robustness check
didreg_rch = lm(lnconcump ~ DST + treatment + DSTtreatment + Cooling_degrees + Heating_degrees + Pressure + Relative_humidity + Weekend + Public_holiday, data = data)
summary(didreg_rch)

stargazer(dcidreg, didreg_rch, title="Regression Results', dep.var.labels=c("Electricity consumption", "Electricity consumption"), ci=TRUE, ci.level=0.90, single.row=TRUE)
