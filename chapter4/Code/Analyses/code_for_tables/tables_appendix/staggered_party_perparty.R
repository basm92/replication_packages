# regression staggered party perparty
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

dataset_wp_cath <- dataset_wp %>% filter(party_category == "catholic")
dataset_wp_lib <- dataset_wp %>% filter(party_category == "liberal")
dataset_wp_prot <- dataset_wp %>% filter(party_category == "protestant")

# two sets of covariates
make_covariates <- function(dataset){
  cbind(
    dataset$totaal_aantal_stemmen,
    dataset$hoeveelste_keer_prob_alg,
    dataset$age_at_election,
    dataset$no_candidates,
    dataset$district_cath,      
    dataset$district_serv
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

# need these stupid regressions to calculate p-value
high1 <- rdrobust(dataset_wp_cath$defw, dataset_wp_cath$margin, 
                 weights = if_else(dataset_wp_cath$within_party == 1,0,1),
                 covs = make_covariates(dataset_wp_cath),
                 bwselect = 'msetwo')
low1 <- rdrobust(dataset_wp_cath$defw, dataset_wp_cath$margin, 
                weights = if_else(dataset_wp_cath$within_party == 1,1,0),
                covs = make_covariates(dataset_wp_cath),
                bwselect = 'msetwo')
high2 <- rdrobust(dataset_wp_cath$defw, dataset_wp_cath$margin, 
                  weights = if_else(dataset_wp_cath$within_party == 1,0,1),
                  covs = make_covariates2(dataset_wp_cath),
                  bwselect = 'msetwo')
low2 <- rdrobust(dataset_wp_cath$defw, dataset_wp_cath$margin, 
                 weights = if_else(dataset_wp_cath$within_party == 1,1,0),
                 covs = make_covariates2(dataset_wp_cath),
                 bwselect = 'msetwo')
high3 <- rdrobust(dataset_wp_lib$defw, dataset_wp_lib$margin, 
                  weights = if_else(dataset_wp_lib$within_party == 1,0,1),
                  covs = make_covariates(dataset_wp_lib),
                  bwselect = 'msetwo')
low3 <- rdrobust(dataset_wp_lib$defw, dataset_wp_lib$margin, 
                 weights = if_else(dataset_wp_lib$within_party == 1,1,0),
                 covs = make_covariates(dataset_wp_lib),
                 bwselect = 'msetwo')
high4 <- rdrobust(dataset_wp_lib$defw, dataset_wp_lib$margin, 
                  weights = if_else(dataset_wp_lib$within_party == 1,0,1),
                  covs = make_covariates2(dataset_wp_lib),
                  bwselect = 'msetwo')
low4 <- rdrobust(dataset_wp_lib$defw, dataset_wp_lib$margin, 
                 weights = if_else(dataset_wp_lib$within_party == 1,1,0),
                 covs = make_covariates2(dataset_wp_lib),
                 bwselect = 'msetwo')
high5 <- rdrobust(dataset_wp_prot$defw, dataset_wp_prot$margin, 
                  weights = if_else(dataset_wp_prot$within_party == 1,0,1),
                  covs = make_covariates(dataset_wp_prot),
                  bwselect = 'msetwo')
low5 <- rdrobust(dataset_wp_prot$defw, dataset_wp_prot$margin, 
                 weights = if_else(dataset_wp_prot$within_party == 1,1,0),
                 covs = make_covariates(dataset_wp_prot),
                 bwselect = 'msetwo')
high6 <- rdrobust(dataset_wp_prot$defw, dataset_wp_prot$margin, 
                  weights = if_else(dataset_wp_prot$within_party == 1,0,1),
                  covs = make_covariates2(dataset_wp_prot),
                  bwselect = 'msetwo')
low6 <- rdrobust(dataset_wp_prot$defw, dataset_wp_prot$margin, 
                 weights = if_else(dataset_wp_prot$within_party == 1,1,0),
                 covs = make_covariates2(dataset_wp_prot),
                 bwselect = 'msetwo')


# now, make the table
panel_a <- data.frame(
  names = c('Coefficient (Without Party)',
            "SE (Without Party)",
            'Coefficient (Within Party)',
            "SE (Within Party)",
            "p-value Difference",
            "Mean DV Treated",
            "Mean DV Control",
            "N Treated",
            "N Control",
            "Bandwidth"),
  cath_one = c(get_info(dataset_wp_cath, 'defw', covs = make_covariates(dataset_wp_cath), 
                   weights = if_else(dataset_wp_cath$within_party == 1, 0, 1),
                   bwselect = 'msetwo')[c(1,2),],
          get_info(dataset_wp_cath, 'defw', covs = make_covariates(dataset_wp_cath), 
                   weights = if_else(dataset_wp_cath$within_party == 1, 1, 0),
                   bwselect = 'msetwo')[c(1,2),],
          2*(pnorm((high1$coef[1] - low1$coef[1]), mean = 0, sd = sqrt(high1$se[1]^2 + low1$se[1]^2))) %>% round(3),
          get_info(dataset_wp_cath, 'defw', covs = make_covariates(dataset_wp_cath), 
                   weights = if_else(dataset_wp_cath$within_party == 1, 1, 0),
                   bwselect = 'msetwo')[3:7,]),
  cath_two = c(get_info(dataset_wp_cath, 'defw', covs = make_covariates2(dataset_wp_cath), 
                   weights = if_else(dataset_wp_cath$within_party == 1, 0, 1),
                   bwselect = 'msetwo')[c(1,2),],
          get_info(dataset_wp_cath, 'defw', covs = make_covariates2(dataset_wp_cath), 
                   weights = if_else(dataset_wp_cath$within_party == 1, 1, 0),
                   bwselect = 'msetwo')[c(1,2),],
          2*(1-pnorm((high2$coef[1] - low2$coef[1]), mean = 0, sd = sqrt(high2$se[1]^2 + low2$se[1]^2))) %>% round(3),
          get_info(dataset_wp_cath, 'defw', covs = make_covariates2(dataset_wp_cath), 
                   weights = if_else(dataset_wp_cath$within_party == 1, 1, 0),
                   bwselect = 'msetwo')[3:7,]),
  lib_one = c(get_info(dataset_wp_lib, 'defw', covs = make_covariates(dataset_wp_lib), 
                       weights = if_else(dataset_wp_lib$within_party == 1, 0, 1),
                       bwselect = 'msetwo')[c(1,2),],
              get_info(dataset_wp_lib, 'defw', covs = make_covariates(dataset_wp_lib), 
                       weights = if_else(dataset_wp_lib$within_party == 1, 1, 0),
                       bwselect = 'msetwo')[c(1,2),],
              2*(1-pnorm((high3$coef[1] - low3$coef[1]), mean = 0, sd = sqrt(high3$se[1]^2 + low3$se[1]^2))) %>% round(3),
              get_info(dataset_wp_lib, 'defw', covs = make_covariates(dataset_wp_lib), 
                       weights = if_else(dataset_wp_lib$within_party == 1, 1, 0),
                       bwselect = 'msetwo')[3:7,]),
  lib_two = c(get_info(dataset_wp_lib, 'defw', covs = make_covariates2(dataset_wp_lib), 
                       weights = if_else(dataset_wp_lib$within_party == 1, 0, 1),
                       bwselect = 'msetwo')[c(1,2),],
              get_info(dataset_wp_lib, 'defw', covs = make_covariates2(dataset_wp_lib), 
                       weights = if_else(dataset_wp_lib$within_party == 1, 1, 0),
                       bwselect = 'msetwo')[c(1,2),],
              2*(1-pnorm((high4$coef[1] - low4$coef[1]), mean = 0, sd = sqrt(high4$se[1]^2 + low4$se[1]^2))) %>% round(3),
              get_info(dataset_wp_lib, 'defw', covs = make_covariates2(dataset_wp_lib), 
                       weights = if_else(dataset_wp_lib$within_party == 1, 1, 0),
                       bwselect = 'msetwo')[3:7,]),
  prot_one = c(get_info(dataset_wp_prot, 'defw', covs = make_covariates(dataset_wp_prot), 
                        weights = if_else(dataset_wp_prot$within_party == 1, 0, 1),
                        bwselect = 'msetwo')[c(1,2),],
               get_info(dataset_wp_prot, 'defw', covs = make_covariates(dataset_wp_prot), 
                        weights = if_else(dataset_wp_prot$within_party == 1, 1, 0),
                        bwselect = 'msetwo')[c(1,2),],
               2*(1-pnorm((high5$coef[1] - low5$coef[1]), mean = 0, sd = sqrt(high5$se[1]^2 + low5$se[1]^2))) %>% round(3),
               get_info(dataset_wp_prot, 'defw', covs = make_covariates(dataset_wp_prot), 
                        weights = if_else(dataset_wp_prot$within_party == 1, 1, 0),
                        bwselect = 'msetwo')[3:7,]),
  prot_two = c(get_info(dataset_wp_prot, 'defw', covs = make_covariates2(dataset_wp_prot), 
                        weights = if_else(dataset_wp_prot$within_party == 1, 0, 1),
                        bwselect = 'msetwo')[c(1,2),],
               get_info(dataset_wp_prot, 'defw', covs = make_covariates2(dataset_wp_prot), 
                        weights = if_else(dataset_wp_prot$within_party == 1, 1, 0),
                        bwselect = 'msetwo')[c(1,2),],
               2*(1-pnorm((high6$coef[1] - low6$coef[1]), mean = 0, sd = sqrt(high6$se[1]^2 + low6$se[1]^2))) %>% round(3),
               get_info(dataset_wp_prot, 'defw', covs = make_covariates2(dataset_wp_prot), 
                        weights = if_else(dataset_wp_prot$within_party == 1, 1, 0),
                        bwselect = 'msetwo')[3:7,]))


# tabel
# now, make the tables
knitr::opts_current$set(label = "rdd_party_interaction_perparty")
notitie <- "The table shows RD estimates using the MSE-optimal bandwidth \\\\citep{cattaneo2019practical}. The Dependent Variable is Log(1+Personal Wealth). I report bias-corrected standard errors. The first two columns show estimates of the returns for the first-triers for the first stint, the second two estimates the returns for the second stint, and the third pair shows the results for all triers. Columns (1), (3) and (5) contain estimates with covariates including party, lifespan, number of votes, age, and number of candidates. Columns (2), (4) and (6) control for number of tries, party, district economic composition and total amount of votes. *: p < 0.1, **: p < 0.05, ***: p < 0.01."
datasummary_df(panel_a %>%
                 rename(` ` = names, 
                        "(1)" = cath_one,
                        "(2)" = cath_two,
                        "(3)" = lib_one,
                        "(4)" = lib_two,
                        "(5)" = prot_one,
                        "(6)" = prot_two), 
               out = "kableExtra",
               output = "latex",
               title = "Estimates In and Out-Party, Per Party") %>%
  kableExtra::add_header_above(c(" " = 1, "Catholic" = 2, "Liberal" = 2, "Protestant" = 2)) %>%
  kableExtra::kable_styling(latex_options = c("hold_position", "scale_down")) %>% #, full_width = F, font_size = 10) %>%
  kableExtra::footnote(general = notitie, footnote_as_chunk = T, threeparttable = T, escape = F)  %>%
  kableExtra::save_kable("./tables_appendix/rdd_party_interaction_perparty.tex")
