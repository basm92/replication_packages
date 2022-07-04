# regression_tables_careerpaths
library(readxl); library(tidyverse); library(hrbrthemes); library(rdrobust); library(modelsummary)
library(lubridate)
# calculate att etc. 
source("./Code/Analyses/function_calculate_itt_and_att.R")
source("./Code/Analyses/functions_for_tables.R")
source("./Code/Analyses/new_data_analysis.R")


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
# first stint (pooled)
dataset_wp <- dataset %>%
  filter(hoevaak_gewonnen_verleden == 0) %>%
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

make_covariates <- function(dataset){
  cbind(
    dataset$totaal_aantal_stemmen,
    dataset$hoeveelste_keer_prob_alg,
    dataset$age_at_election,
    dataset$no_candidates,
    dataset$district_cath,      
    dataset$district_serv,
    dataset$rec_ar,
    dataset$rec_lib,
    dataset$rec_soc,
    dataset$rec_kath
  )
}

make_covariates2 <- function(dataset){
  cbind(
    dataset$totaal_aantal_stemmen,
    dataset$hoeveelste_keer_prob_alg,
    dataset$age_at_election,
    dataset$no_candidates,         
    dataset$district_indus
  )
}


panel_a <- data.frame(names = c(
  "Coefficient", 
    "SE (BC)",
    "Mean DV Treated (1%)",
    "Mean DV Control (1%)",
    "N (Politicians)",
    "N (Non-Politicians)",
  "Bandwidth"),
  get_info(dataset_wp, 'prof_business', covs = make_covariates(dataset_wp)),
  get_info(dataset_wp, 'prof_business', covs = make_covariates2(dataset_wp)),
  get_info(dataset_wp, 'prof_colonial', covs = make_covariates(dataset_wp)),
  get_info(dataset_wp, 'prof_colonial', covs = make_covariates2(dataset_wp)),
  get_info(dataset_wp, 'prof_politics', covs = make_covariates(dataset_wp)),
  get_info(dataset_wp, 'prof_politics', covs = make_covariates2(dataset_wp))) 


panel_b <- data.frame(names = c(
  "Coefficient (Before Party)", 
  "SE (Before Party)",
  "Coefficient (After Party)",
  "SE (After Party)",
  "Mean DV Treated (1%)",
  "Mean DV Control (1%)",
  "N (Politicians)",
  "N (Non-Politicians)",
  "Bandwidth"),
  out = c(get_info(dataset_wp, 'prof_business', covs = make_covariates(dataset_wp), 
             weights = if_else(dataset_wp$within_party == 1, 0, 1),
             bwselect = 'mserd')[c(1,2),],
    get_info(dataset_wp, 'prof_business', covs = make_covariates(dataset_wp), 
             weights = if_else(dataset_wp$within_party == 1, 1, 0),
             bwselect = 'mserd')[c(1,2),],
    get_info(dataset_wp, 'prof_business', covs = make_covariates(dataset_wp), 
             weights = if_else(dataset_wp$within_party == 1, 1, 0),
             bwselect = 'mserd')[3:7,]),
  out.1 = c(get_info(dataset_wp, 'prof_business', covs = make_covariates2(dataset_wp), 
                    weights = if_else(dataset_wp$within_party == 1, 0, 1),
                    bwselect = 'mserd')[c(1,2),],
           get_info(dataset_wp, 'prof_business', covs = make_covariates2(dataset_wp), 
                    weights = if_else(dataset_wp$within_party == 1, 1, 0),
                    bwselect = 'mserd')[c(1,2),],
           get_info(dataset_wp, 'prof_business', covs = make_covariates2(dataset_wp), 
                    weights = if_else(dataset_wp$within_party == 1, 1, 0),
                    bwselect = 'mserd')[3:7,]),
  out.2 = c(get_info(dataset_wp, 'prof_colonial', covs = make_covariates(dataset_wp), 
                    weights = if_else(dataset_wp$within_party == 1, 0, 1),
                    bwselect = 'mserd')[c(1,2),],
           get_info(dataset_wp, 'prof_colonial', covs = make_covariates(dataset_wp), 
                    weights = if_else(dataset_wp$within_party == 1, 1, 0),
                    bwselect = 'mserd')[c(1,2),],
           get_info(dataset_wp, 'prof_colonial', covs = make_covariates(dataset_wp), 
                    weights = if_else(dataset_wp$within_party == 1, 1, 0),
                    bwselect = 'mserd')[3:7,]),
  out.3 = c(get_info(dataset_wp, 'prof_colonial', covs = make_covariates2(dataset_wp), 
                    weights = if_else(dataset_wp$within_party == 1, 0, 1),
                    bwselect = 'mserd')[c(1,2),],
           get_info(dataset_wp, 'prof_colonial', covs = make_covariates2(dataset_wp), 
                    weights = if_else(dataset_wp$within_party == 1, 1, 0),
                    bwselect = 'mserd')[c(1,2),],
           get_info(dataset_wp, 'prof_colonial', covs = make_covariates2(dataset_wp), 
                    weights = if_else(dataset_wp$within_party == 1, 1, 0),
                    bwselect = 'mserd')[3:7,]),
  out.4 = c(get_info(dataset_wp, 'prof_politics', covs = make_covariates(dataset_wp), 
                    weights = if_else(dataset_wp$within_party == 1, 0, 1),
                    bwselect = 'mserd')[c(1,2),],
           get_info(dataset_wp, 'prof_politics', covs = make_covariates(dataset_wp), 
                    weights = if_else(dataset_wp$within_party == 1, 1, 0),
                    bwselect = 'mserd')[c(1,2),],
           get_info(dataset_wp, 'prof_politics', covs = make_covariates(dataset_wp), 
                    weights = if_else(dataset_wp$within_party == 1, 1, 0),
                    bwselect = 'mserd')[3:7,]),
  out.5 = c(get_info(dataset_wp, 'prof_politics', covs = make_covariates2(dataset_wp), 
                    weights = if_else(dataset_wp$within_party == 1, 0, 1),
                    bwselect = 'mserd')[c(1,2),],
           get_info(dataset_wp, 'prof_politics', covs = make_covariates2(dataset_wp), 
                    weights = if_else(dataset_wp$within_party == 1, 1, 0),
                    bwselect = 'mserd')[c(1,2),],
           get_info(dataset_wp, 'prof_politics', covs = make_covariates2(dataset_wp), 
                    weights = if_else(dataset_wp$within_party == 1, 1, 0),
                    bwselect = 'mserd')[3:7,]))
    
# make table
notitie <- "Table showing the effect of being elected into politics on three future career paths: taking up a position in finance (business), continuing in non-lower house politics (as a mayor), and taking up a career in the colonies. Bias-corrected and Robust standard errors clustered at the individual-level. All effects are estimated under the MSE-optimal bandwidth. I use two sets of covariates: first, I control for total amount of votes, age, newspaper recommendations and economic and demographic composition of the district. Second, I control for newspaper recommendations, the number of tries, and the economic and demographic composition of the candidate's birthplace. *: p < 0.10, **: p < 0.05, ***: p < 0.01."

knitr::opts_current$set(label = "rdd_results_careerpaths")
datasummary_df(bind_rows(panel_a, panel_b) %>%
                 rename(` ` = names, 
                        "(1)" = out,
                        "(2)" = out.1,
                        "(3)" = out.2,
                        "(4)" = out.3,
                        "(5)" = out.4,
                        "(6)" = out.5), 
               out = "kableExtra",
               output = "latex",
               title = "RD Estimates of Being Elected on Career Paths") %>%
  kableExtra::add_header_above(c(" " = 1, "Finance" = 2, "Colonial" = 2, "Mayor" = 2)) %>%
  kableExtra::group_rows("Panel A: Unconditional Estimates", 1, 7) %>%
  kableExtra::group_rows("Panel B: Before and After Party Establishment", 8, 16) %>%
  kableExtra::kable_styling(latex_options = c("hold_position", "scale_down")) %>%
  kableExtra::footnote(general = notitie, footnote_as_chunk = T, threeparttable = T, escape = F)  %>%
  kableExtra::save_kable("./tables_main/rdd_results_careerpaths.tex")
