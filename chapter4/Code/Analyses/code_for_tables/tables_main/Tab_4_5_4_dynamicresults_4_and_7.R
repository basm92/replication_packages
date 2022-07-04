# maindynamicresults t*=4, t*=7
library(readxl); library(tidyverse); library(hrbrthemes); library(rdrobust); library(modelsummary)
library(ggtext); library(lubridate)
# calculate att etc. 
source("./Code/Analyses/function_calculate_itt_and_att.R")
source("./Code/Analyses/functions_for_tables.R")
source("./Code/Analyses/new_data_analysis.R")
source("./Code/Analyses/helper_calculate_itt_att_extensive.R")
#dataset <- readRDS("./Data/analysis/dataset.RDS")


# table
tabledata <- bind_rows(
  get_info_dynamic(dataset, 4, covs = c("yoe", "totaal_aantal_stemmen","hoeveelste_keer_prob_alg", "birthplace_agri", 
                                                "birthplace_indus", "rec_soc",
                                                "rec_lib"), bwselect = "msetwo"),
  get_info_dynamic(dataset, 7, covs = c("yoe", "totaal_aantal_stemmen","hoeveelste_keer_prob_alg", "birthplace_agri", 
                                                "birthplace_indus", "rec_soc", 
                                                "rec_lib"), bwselect = "msetwo"))
notitie <- "Table showing coefficients effects of stints \\\\{1, \\\\dots, $t^*$\\\\} under different $t^* \\\\in \\\\{4,7\\\\}$. All the ATT coefficients are derived and recursively computed from ITT coefficients, which are in turn estimated using the methodology in \\\\citep{cattaneo2019practical} using MSE-optimal bandwidth. Standard errors are calculated using the delta method. The estimates in both panels control for birthplace population, birthplace characteristics, age at election, newspaper recommendations (party) and politicians' lifespan. *: p < 0.10, **: p < 0.05, ***: p < 0.01. "
knitr::opts_current$set(label = "attresults")
datasummary_df(tabledata %>%
                 rename("t=1" = ...2, 
                        "t=2" = ...3,
                        "t=3" = ...4,
                        "t=4" = ...5,
                        "t=5" = ...6,
                        "t=6" = ...7,
                        "t=7" = ...8,
                        " " = names) %>%
                 replace_na(list("t=7"="", "t=6" = "", "t=5"= "")),
               out = "kableExtra",
               output = "latex",
               title = "ATT estimates for different $t^*$") %>%
  kableExtra::group_rows("Panel A: t* = 4", 1, 8) %>%
  kableExtra::group_rows("Panel B: t* = 7", 9, 16) %>%
  kableExtra::kable_styling(latex_options = c("hold_position", "scale_down")) %>% #, full_width = F, font_size = 10) %>%
  kableExtra::footnote(general = notitie, footnote_as_chunk = T, threeparttable = T, escape = F)  %>%
  kableExtra::save_kable("./tables_main/att_mainresults.tex")
