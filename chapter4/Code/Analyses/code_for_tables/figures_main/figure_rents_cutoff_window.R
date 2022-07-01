#figure rents cutoff window
# regression_tables
library(readxl); library(tidyverse); library(hrbrthemes); library(rdrobust); library(modelsummary)
library(ggtext); library(lubridate)
# calculate att etc. 
source("./Code/Analyses/function_calculate_itt_and_att.R")
source("./Code/Analyses/functions_for_tables.R")
source("./Code/Analyses/new_data_analysis.R")

make_covariates <- function(dataset){
  cbind(dataset$yoe, 
        log(1+dataset$birthplace_pop_1859), 
        dataset$birthplace_agri, 
        dataset$birthplace_indus, 
        dataset$lifespan,
        dataset$rec_soc,
        dataset$rec_lib,
        dataset$rec_kath,
        dataset$rec_prot)
}

make_covariates2 <- function(dataset){
  
  cbind(dataset$yoe, 
        log(1+dataset$birthplace_pop_1859), 
        dataset$birthplace_agri, 
        dataset$birthplace_indus, 
        dataset$lifespan,
        dataset$rec_soc)
}

data_for_graph <- data.frame(date = seq(from = dmy("15-06-1880"), to = dmy("15-06-1901"), by = "years"),
                             estimate = vector(length = length(date)),
                             ci_up = vector(length = length(date)),
                             ci_down = vector(length = length(date)))

for(i in seq(from = dmy("15-06-1880"), to = dmy("15-06-1902"), by = "years")){
  
  current_data <- firstrents_pooled %>% filter(verkiezingdatum > i)
  
  estimates <- rdrobust(current_data$defw, 
                         current_data$margin, 
                         covs = make_covariates2(current_data),
                         bwselect='msetwo')
  
  data_for_graph[data_for_graph['date'] == i, 'estimate'] <- estimates$coef[1]
  data_for_graph[data_for_graph['date'] == i, 'ci_up'] <- estimates$ci[4]
  data_for_graph[data_for_graph['date'] == i, 'ci_down'] <- estimates$ci[1]
  
}

graph <- data_for_graph %>%
  ggplot(aes(x = date, y = estimate)) + geom_line() + geom_point() +
  theme_bw() +
  geom_ribbon(aes(ymin=ci_down, ymax = ci_up), alpha = 0.2) +
  geom_line(aes(x = date, y = ci_up)) +
  geom_line(aes(x = date, y = ci_down)) +
  geom_vline(aes(xintercept = dmy("01-09-1887")), lty = 2) +
  geom_vline(aes(xintercept = dmy("15-06-1897")), lty = 2) +
  ylab("Coefficient Estimate (ITT)") +
  xlab("Observations Starting from Date")


ggsave("./figures_main/figure_rents_cutoff_window.pdf", width = 10, height = 4)
