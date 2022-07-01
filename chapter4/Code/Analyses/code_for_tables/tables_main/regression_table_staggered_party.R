# regression table staggered adoption
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

# first stint (first try)
dataset_wp_ft <- dataset %>%
  filter(hoevaak_gewonnen_verleden == 0, howmany_before_alg == 0) %>%
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

# first stint (second try)
dataset_wp_st <- dataset %>%
  filter(hoevaak_gewonnen_verleden == 0, howmany_before_alg > 0) %>%
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
    dataset$party_lib,
    dataset$party_prot,
    dataset$party_cath
  )
}

make_covariates2 <- function(dataset){
  cbind(
    dataset$totaal_aantal_stemmen,
    dataset$hoeveelste_keer_prob_alg,
    dataset$age_at_election,
    dataset$no_candidates,         
    dataset$district_indus,
    dataset$party_lib,
    dataset$party_prot,
    dataset$party_cath
  )
}

# need to estimate these things to calculate p-value
high <- rdrobust(dataset_wp$defw, dataset_wp$margin, 
                 weights = if_else(dataset_wp$within_party == 1,0,1),
                 covs = make_covariates(dataset_wp),
                 bwselect = 'msetwo')
low <- rdrobust(dataset_wp$defw, dataset_wp$margin, 
                weights = if_else(dataset_wp$within_party == 1,1,0),
                covs = make_covariates(dataset_wp),
                bwselect = 'msetwo')

high2 <- rdrobust(dataset_wp$defw, dataset_wp$margin, 
                  weights = if_else(dataset_wp$within_party == 1,0,1),
                  covs = make_covariates2(dataset_wp),
                  bwselect = 'msetwo')
low2 <- rdrobust(dataset_wp$defw, dataset_wp$margin, 
                 weights = if_else(dataset_wp$within_party == 1,1,0),
                 covs = make_covariates2(dataset_wp),
                 bwselect = 'msetwo')
high3 <- rdrobust(dataset_wp_ft$defw, dataset_wp_ft$margin, 
                  weights = if_else(dataset_wp_ft$within_party == 1,0,1),
                  covs = make_covariates(dataset_wp_ft),
                  bwselect = 'msetwo')
low3 <- rdrobust(dataset_wp_ft$defw, dataset_wp_ft$margin, 
                 weights = if_else(dataset_wp_ft$within_party == 1,1,0),
                 covs = make_covariates(dataset_wp_ft),
                 bwselect = 'msetwo')
high4 <- rdrobust(dataset_wp_ft$defw, dataset_wp_ft$margin, 
                  weights = if_else(dataset_wp_ft$within_party == 1,0,1),
                  covs = make_covariates2(dataset_wp_ft),
                  bwselect = 'msetwo')
low4 <- rdrobust(dataset_wp_ft$defw, dataset_wp_ft$margin, 
                 weights = if_else(dataset_wp_ft$within_party == 1,1,0),
                 covs = make_covariates2(dataset_wp_ft),
                 bwselect = 'msetwo')
high5 <- rdrobust(dataset_wp_st$defw, dataset_wp_st$margin, 
                  weights = if_else(dataset_wp_st$within_party == 1,0,1),
                  covs = make_covariates(dataset_wp_st),
                  bwselect = 'msetwo')
low5 <- rdrobust(dataset_wp_st$defw, dataset_wp_st$margin, 
                 weights = if_else(dataset_wp_st$within_party == 1,1,0),
                 covs = make_covariates(dataset_wp_st),
                 bwselect = 'msetwo')
high6 <- rdrobust(dataset_wp_st$defw, dataset_wp_st$margin, 
                  weights = if_else(dataset_wp_st$within_party == 1,0,1),
                  covs = make_covariates2(dataset_wp_st),
                  bwselect = 'msetwo')
low6 <- rdrobust(dataset_wp_st$defw, dataset_wp_st$margin, 
                 weights = if_else(dataset_wp_st$within_party == 1,1,0),
                 covs = make_covariates2(dataset_wp_st),
                 bwselect = 'msetwo')

# now, finally for the table:

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
  one = c(get_info2(dataset_wp_ft, 'defw', covs = make_covariates(dataset_wp_ft), 
                   weights = if_else(dataset_wp_ft$within_party == 1, 0, 1),
                   bwselect = 'msetwo')[c(1,2),],
          get_info2(dataset_wp_ft, 'defw', covs = make_covariates(dataset_wp_ft), 
                   weights = if_else(dataset_wp_ft$within_party == 1, 1, 0),
                   bwselect = 'msetwo')[c(1,2),],
          2*(1-pnorm((high3$coef[1] - low3$coef[1]), mean = 0, sd = sqrt(high3$se[1]^2 + low3$se[1]^2))) %>% round(3),
          get_info2(dataset_wp_ft, 'defw', covs = make_covariates(dataset_wp_ft), 
                   weights = if_else(dataset_wp_ft$within_party == 1, 1, 0),
                   bwselect = 'msetwo')[3:7,]),
  two = c(get_info2(dataset_wp_ft, 'defw', covs = make_covariates2(dataset_wp_ft), 
                   weights = if_else(dataset_wp_ft$within_party == 1, 0, 1),
                   bwselect = 'msetwo')[c(1,2),],
          get_info2(dataset_wp_ft, 'defw', covs = make_covariates2(dataset_wp_ft), 
                   weights = if_else(dataset_wp_ft$within_party == 1, 1, 0),
                   bwselect = 'msetwo')[c(1,2),],
          2*(1-pnorm((high4$coef[1] - low4$coef[1]), mean = 0, sd = sqrt(high4$se[1]^2 + low4$se[1]^2))) %>% round(3),
          get_info2(dataset_wp_ft, 'defw', covs = make_covariates2(dataset_wp_ft), 
                   weights = if_else(dataset_wp_ft$within_party == 1, 1, 0),
                   bwselect = 'msetwo')[3:7,]),
  three = c(get_info2(dataset_wp_st, 'defw', covs = make_covariates(dataset_wp_st), 
                     weights = if_else(dataset_wp_st$within_party == 1, 0, 1),
                     bwselect = 'msetwo')[c(1,2),],
            get_info2(dataset_wp_st, 'defw', covs = make_covariates(dataset_wp_st), 
                     weights = if_else(dataset_wp_st$within_party == 1, 1, 0),
                     bwselect = 'msetwo')[c(1,2),],
            2*(1-pnorm((high5$coef[1] - low5$coef[1]), mean = 0, sd = sqrt(high5$se[1]^2 + low5$se[1]^2))) %>% round(3),
            get_info2(dataset_wp_st, 'defw', covs = make_covariates(dataset_wp_st), 
                     weights = if_else(dataset_wp_st$within_party == 1, 1, 0),
                     bwselect = 'msetwo')[3:7,]),
  four = c(get_info2(dataset_wp_st, 'defw', covs = make_covariates2(dataset_wp_st), 
                    weights = if_else(dataset_wp_st$within_party == 1, 0, 1),
                    bwselect = 'msetwo')[c(1,2),],
           get_info2(dataset_wp_st, 'defw', covs = make_covariates2(dataset_wp_st), 
                    weights = if_else(dataset_wp_st$within_party == 1, 1, 0),
                    bwselect = 'msetwo')[c(1,2),],
           2*(1-pnorm((high6$coef[1] - low6$coef[1]), mean = 0, sd = sqrt(high6$se[1]^2 + low6$se[1]^2))) %>% round(3),
           get_info2(dataset_wp_st, 'defw', covs = make_covariates2(dataset_wp_st), 
                    weights = if_else(dataset_wp_st$within_party == 1, 1, 0),
                    bwselect = 'msetwo')[3:7,]),
  five = c(get_info2(dataset_wp, 'defw', covs = make_covariates(dataset_wp), 
                    weights = if_else(dataset_wp$within_party == 1, 0, 1),
                    bwselect = 'msetwo')[c(1,2),],
           get_info2(dataset_wp, 'defw', covs = make_covariates(dataset_wp), 
                    weights = if_else(dataset_wp$within_party == 1, 1, 0),
                    bwselect = 'msetwo')[c(1,2),],
           2*(1-pnorm((high$coef[1] - low$coef[1]), mean = 0, sd = sqrt(high$se[1]^2 + low$se[1]^2))) %>% round(3),
           get_info2(dataset_wp, 'defw', covs = make_covariates(dataset_wp), 
                    weights = if_else(dataset_wp$within_party == 1, 1, 0),
                    bwselect = 'msetwo')[3:7,]),
  six = c(get_info2(dataset_wp, 'defw', covs = make_covariates2(dataset_wp), 
                   weights = if_else(dataset_wp$within_party == 1, 0, 1),
                   bwselect = 'msetwo')[c(1,2),],
          get_info2(dataset_wp, 'defw', covs = make_covariates2(dataset_wp), 
                   weights = if_else(dataset_wp$within_party == 1, 1, 0),
                   bwselect = 'msetwo')[c(1,2),],
          2*(1-pnorm((high2$coef[1] - low2$coef[1]), mean = 0, sd = sqrt(high2$se[1]^2 + low2$se[1]^2))) %>% round(3),
          get_info2(dataset_wp, 'defw', covs = make_covariates2(dataset_wp), 
                   weights = if_else(dataset_wp$within_party == 1, 1, 0),
                   bwselect = 'msetwo')[3:7,]))

# now, make the tables
knitr::opts_current$set(label = "rdd_party_interaction")
notitie <- "The table shows RD estimates using the MSE-optimal bandwidth \\\\citep{cattaneo2019practical}. The Dependent Variable is Log(1+Personal Wealth). I report bias-corrected standard errors. The first two columns show estimates of the returns for the first-triers for the first stint, the second two estimates the returns for the second stint, and the third pair shows the results for all triers. Columns (1), (3) and (5) contain estimates with covariates including party, lifespan, number of votes, age, and number of candidates. Columns (2), (4) and (6) control for number of tries, party, district economic composition and total amount of votes. *: p < 0.1, **: p < 0.05, ***: p < 0.01."
datasummary_df(panel_a %>%
                 rename(` ` = names, 
                        "(1)" = one,
                        "(2)" = two,
                        "(3)" = three,
                        "(4)" = four,
                        "(5)" = five,
                        "(6)" = six), 
               out = "kableExtra",
               output = "latex",
               title = "Estimates In and Out-Party") %>%
  kableExtra::add_header_above(c(" " = 1, "First Triers" = 2, "Other Triers" = 2, "All Triers" = 2)) %>%
  kableExtra::kable_styling(latex_options = c("hold_position", "scale_down")) %>% #, full_width = F, font_size = 10) %>%
  kableExtra::footnote(general = notitie, footnote_as_chunk = T, threeparttable = T, escape = F)  %>%
  kableExtra::save_kable("./tables_main/rdd_party_interaction.tex")
