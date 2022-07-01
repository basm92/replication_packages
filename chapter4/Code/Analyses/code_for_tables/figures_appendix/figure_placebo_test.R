#fig_placebo_test
library(readxl); library(tidyverse); library(hrbrthemes); library(rdrobust); library(modelsummary)
library(ggtext); library(lubridate)
# calculate att etc. 
source("./Code/Analyses/function_calculate_itt_and_att.R")
source("./Code/Analyses/functions_for_tables.R")
source("./Code/Analyses/new_data_analysis.R")

# Regression figure: placebo tests with false cutoff point - Make a figure
fig_data <- data.frame(cutoff = seq(from = -0.15, to = 0.15, by = 0.01), 
                       coef = vector(length = 31), 
                       lb = vector(length = 31), 
                       ub = vector(length = 31))

for(i in 1:length(seq(from = -0.15, to = 0.15, by = 0.01))){
  regression <- rdrobust(y = firstrents_pooled$defw, x = firstrents_pooled$margin, c = fig_data[['cutoff']][i])
  fig_data[['lb']][i] <- regression[['ci']][3,][1]
  fig_data[['ub']][i] <- regression[['ci']][3,][2]
  fig_data[['coef']][i] <- regression[['coef']][1]
}

good <- subset(fig_data, cutoff == 0)

placebo <- fig_data %>%
  ggplot(aes(x = cutoff, y = coef)) + geom_point(color = 'blue') + 
  theme_bw() +
  xlab("Cut-off point") + ylab("RD Estimate") +
  ylim(-2, 2.5) + 
  geom_errorbar(aes(x = cutoff, ymin = lb, ymax = ub), size = 0.2, color = 'black', width=0.003) +
  geom_point(data = good, color = "red", size = 2) +
  geom_text(data = good, label = "Actual Estimate", vjust =c(-7.5), hjust = c(-0.1)) +
  geom_segment(aes(x = 0.025, y = 2.3, xend = 0.005, yend = 1.3), arrow = arrow(length = unit(0.2, "cm")))


ggplot2::ggsave("./figures_appendix/placebo_test.pdf", placebo, width = 10, height = 3.8)
