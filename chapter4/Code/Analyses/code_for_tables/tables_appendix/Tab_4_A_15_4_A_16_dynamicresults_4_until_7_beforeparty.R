# dynamicresults_4_until_7_beforeparty

#dynamicresults_4_until_7.R
library(readxl); library(tidyverse); library(hrbrthemes); library(rdrobust); library(modelsummary)
library(ggtext); library(lubridate)
# calculate att etc. 
source("./Code/Analyses/function_calculate_itt_and_att.R")
source("./Code/Analyses/functions_for_tables.R")
source("./Code/Analyses/new_data_analysis.R")
source("./Code/Analyses/helper_calculate_itt_att_extensive.R")

# read dataset
#dataset <- readRDS("./Data/analysis/dataset.RDS")

# make a better proxy of party
help_allelections <- read_csv("./Data/elections/election_results_details.csv") %>%
  janitor::clean_names()

find_recommendations <- function(row){
  
  help_allelections %>%
    filter(naam == row$naam) %>%
    select(aanbevolen_door) %>%
    filter(!is.na(.)) %>%
    pull() %>%
    paste(collapse = "|")
  
}

# find the across time recommendations
dataset <- dataset %>%
  rowwise() %>%
  do(row = as.data.frame(.)) %>%
  mutate(recommendations = find_recommendations(row)) %>%
  unnest(cols = c(row))

# divide them into cath, prot, lib (and soc)
dataset <- dataset %>%
  mutate(party_category = if_else(
    party_category == "none", case_when(
      stringr::str_detect(recommendations, "AR|VA|NC|NH") ~ "protestant", 
      stringr::str_detect(recommendations, "Ka|KD|DT") ~ "catholic",
      stringr::str_detect(recommendations, "Lib|VL|AH") ~ "liberal",
      stringr::str_detect(recommendations, "Rad|Soc|SDAP|SDP") ~ "socialist",
      TRUE ~ party_category
    ), party_category))

# make the within variable
dataset_wp <- dataset %>%
  mutate(within_party = case_when(party_category == "catholic" & election_after_rk == 1 ~ 1,
                                  party_category == "protestant" & verkiezingdatum > dmy("03-04-1879") ~ 1,
                                  party_category == "liberal" & election_after_lib == 1 ~ 1,
                                  party_category == "socialist" & verkiezingdatum > dmy("26-08-1894") ~ 1,
                                  TRUE ~ 0),
         party_none = if_else(party_category == "none", 1, 0),
         party_lib = if_else(party_category == "liberal", 1, 0),
         party_prot = if_else(party_category == "protestant", 1, 0),
         party_cath = if_else(party_category == "catholic", 1, 0),
         party_soc = if_else(party_category == "socialist", 1, 0))



dataset_wp1 <- dataset_wp %>%
  filter(within_party == 1)

dataset_wp0 <- dataset_wp %>%
  filter(within_party == 0)

# tables
tabledata0 <- bind_rows(
  get_info_dynamic(dataset_wp0, 4, 
                   covs=c("totaal_aantal_stemmen","hoeveelste_keer_prob_alg", "yoe", "age_at_election","turnout", "district_share_prot", "rec_soc", "rec_ar", "rec_kath", "rec_lib"),
                   bwselect = "mserd"),
  get_info_dynamic(dataset_wp0, 5, 
                   covs=c("totaal_aantal_stemmen","hoeveelste_keer_prob_alg", "yoe", "age_at_election", "turnout", "district_share_prot", "rec_soc", "rec_ar", "rec_kath", "rec_lib"),
                   bwselect = "mserd"),
  get_info_dynamic(dataset_wp0, 6,
                   covs=c("totaal_aantal_stemmen","hoeveelste_keer_prob_alg", "yoe", "age_at_election", "turnout", "district_share_prot", "rec_soc", "rec_ar", "rec_kath", "rec_lib"),
                   bwselect = "mserd"),
  get_info_dynamic(dataset_wp0, 7,
                   covs=c("totaal_aantal_stemmen","hoeveelste_keer_prob_alg", "yoe", "age_at_election", "turnout", "district_share_prot", "rec_soc", "rec_ar", "rec_kath", "rec_lib"),
                   bwselect = "mserd"))

tabledata1 <- bind_rows(
  get_info_dynamic(dataset_wp1, 4, 
                   covs=c("totaal_aantal_stemmen","hoeveelste_keer_prob_alg", "yoe", "age_at_election", "turnout", "district_share_prot", "rec_soc", "rec_ar", "rec_kath", "rec_lib"),
                   bwselect = "mserd"),
  get_info_dynamic(dataset_wp1, 5, 
                   covs=c("totaal_aantal_stemmen","hoeveelste_keer_prob_alg", "yoe", "age_at_election", "turnout", "district_share_prot", "rec_soc", "rec_ar", "rec_kath", "rec_lib"),
                   bwselect = "mserd"),
  get_info_dynamic(dataset_wp1, 6,
                   covs=c("totaal_aantal_stemmen","hoeveelste_keer_prob_alg", "yoe", "age_at_election", "turnout", "district_share_prot", "rec_soc", "rec_ar", "rec_kath", "rec_lib"),
                   bwselect = "mserd"),
  get_info_dynamic(dataset_wp1, 7,
                   covs=c("totaal_aantal_stemmen","hoeveelste_keer_prob_alg", "yoe", "age_at_election", "turnout", "district_share_prot", "rec_soc", "rec_ar", "rec_kath", "rec_lib"),
                   bwselect = "mserd"))


notitie <- "Table showing coefficients effects of stints \\\\{1, \\\\dots, $t^*$\\\\} under different $t^* \\\\in \\\\{4,5,6,7\\\\}$ before party formation. All the ATT coefficients are derived and recursively computed from ITT coefficients, which are in turn estimated using the methodology in \\\\citep{cattaneo2019practical} using MSE-optimal bandwidth. Standard errors are calculated using the delta method. The estimates in both panels control for birthplace population, birthplace characteristics, age at election, newspaper recommendations (party) and politicians' lifespan. *: p < 0.10, **: p < 0.05, ***: p < 0.01. "
knitr::opts_current$set(label = "attresults_before")
datasummary_df(tabledata0 %>%
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
               title = "ATT estimates for different $t^*$ - Before Party Formation") %>%
  kableExtra::group_rows("Panel A: t* = 4", 1, 8) %>%
  kableExtra::group_rows("Panel B: t* = 5", 9, 16) %>%
  kableExtra::group_rows("Panel C: t* = 6", 17, 24) %>%
  kableExtra::group_rows("Panel D: t* = 7", 25, 32) %>%
  kableExtra::kable_styling(latex_options = c("hold_position"), full_width = F, font_size = 10) %>%
  kableExtra::footnote(general = notitie, footnote_as_chunk = T, threeparttable = T, escape = F)  %>%
  kableExtra::save_kable("./tables_appendix/attresults_beforeparty.tex")

notitie <- "Table showing coefficients effects of stints \\\\{1, \\\\dots, $t^*$\\\\} under different $t^* \\\\in \\\\{4,5,6,7\\\\}$ after party formation. All the ATT coefficients are derived and recursively computed from ITT coefficients, which are in turn estimated using the methodology in \\\\citep{cattaneo2019practical} using MSE-optimal bandwidth. Standard errors are calculated using the delta method. The estimates in both panels control for birthplace population, birthplace characteristics, age at election, newspaper recommendations (party) and politicians' lifespan. *: p < 0.10, **: p < 0.05, ***: p < 0.01. "
knitr::opts_current$set(label = "attresults_after")
datasummary_df(tabledata1 %>%
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
               title = "ATT estimates for different $t^*$ - After Party Formation") %>%
  kableExtra::group_rows("Panel A: t* = 4", 1, 8) %>%
  kableExtra::group_rows("Panel B: t* = 5", 9, 16) %>%
  kableExtra::group_rows("Panel C: t* = 6", 17, 24) %>%
  kableExtra::group_rows("Panel D: t* = 7", 25, 32) %>%
  kableExtra::kable_styling(latex_options = c("hold_position"), full_width = F, font_size = 10) %>%
  kableExtra::footnote(general = notitie, footnote_as_chunk = T, threeparttable = T, escape = F)  %>%
  kableExtra::save_kable("./tables_appendix/attresults_afterparty.tex")
