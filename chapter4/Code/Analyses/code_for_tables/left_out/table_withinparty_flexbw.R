# table_withinparty_flexbandwidth
library(readxl); library(tidyverse); library(hrbrthemes); 
library(rdrobust); library(modelsummary); library(ggtext); library(RATest); library(lubridate); library(extraDistr)

source("./Code/Analyses/functions_for_tables.R")
# Parameters

ihs <- function(x) {log(x + sqrt(x^2 + 1))}
## Few mutations with the data set
dataset <- read_delim("./Data/analysis/unmatched_sample_with_vars.csv", delim=",") %>% # %>% #"./Data/analysis/unmatched_sample_with_vars.csv") %>%
  select(-1) %>%
  mutate(defw = log(1+Vermogen_deflated),
         defw2 = ihs(Vermogen_deflated),
         distrverk = str_c(District, "-", Verkiezingdatum),
         lifespan = lifespan/365,
         politician_dummy = if_else(!is.na(`b1-nummer`), 1, 0),
         politician_indic = if_else(!is.na(`b1-nummer`), "Politician", "Non-Politician"),
         taxespercap_1859 = if_else(is.na(taxespercap_1859), 0.5, taxespercap_1859),
         taxespercap_1889 = if_else(is.na(taxespercap_1889), 0.5, taxespercap_1889),
         district_share_prot = district_prot / (district_prot + district_cath + district_ov),
         district_share_cath = district_cath / (district_prot + district_cath + district_ov),
         rec_ar = case_when(stringr::str_detect(party_election, "AR|VA|NC|NH") ~ 1, 
                            is.na(party_election) ~ 0,
                            TRUE~  0),
         rec_kath = case_when(stringr::str_detect(party_election, "Ka|KD|DT") ~ 1,
                              is.na(party_election) ~ 0,
                              TRUE~ 0),
         rec_lib = case_when(stringr::str_detect(party_election, "Lib|VL|AH") ~ 1,
                             is.na(party_election) ~ 0,
                             TRUE~ 0),
         rec_soc = case_when(stringr::str_detect(party_election, "Rad|Soc|SDAP|SDP") ~ 1,
                             is.na(party_election) ~ 0,
                             TRUE~ 0),
         elec_type_alg = if_else(election_type == "algemeen", 1, 0),
         elec_type_else = if_else(election_type != "algemeen", 1, 0),
         yod = as.numeric(stringr::str_extract(Sterfdatum,"\\d{4}$")),
         yoe = as.numeric(stringr::str_extract(Verkiezingdatum, "\\d{4}$"))
  ) %>%
  filter(!is.na(defw2)) 


# show the rd coefficient of variable on margin
far <- 0.2
close <- 0.05 

# Within and without party variation
dataset_wp <- dataset %>%
  mutate(within_party = case_when(party_category == "catholic" & election_after_rk == 1 ~ 1,
                                  party_category == "protestant" & election_after_arp == 1 ~ 1,
                                  party_category == "liberal" & election_after_lib == 1 ~ 1,
                                  TRUE ~ 0))

in_party <- dataset_wp %>%
  filter(within_party == 1 | politician_dummy == 0)

out_party <- dataset_wp %>%
  filter(within_party == 0 | politician_dummy == 0)

make_covariates <- function(dataset){
  cbind(
    log(1+dataset$birthplace_pop_1859), 
    dataset$yoe,
    dataset$birthplace_agri, 
    dataset$age_at_election, 
    dataset$taxespercap_1859,
    dataset$district_prot,
    dataset$lifespan)
}

make_covariates <- function(dataset){
  cbind(
    dataset$age_at_election, 
    dataset$rec_soc,
    dataset$rec_ar,
    dataset$rec_kath,
    dataset$rec_lib,
    dataset$lifespan)
  
}

covs_ip <- make_covariates(in_party)
covs_op <- make_covariates(out_party)

# Full control group
panel_a <- data.frame(names = c("Coefficient",
                                "SE (BC)",
                                "SE (Rob.)",
                                "N Treated",
                                "N Control", 
                                "Covariates"),
                      inparty1 = c(get_stats_withinparty(in_party, dv = 'defw', bw = 'msetwo')[[1]],
                                   get_stats_withinparty(in_party, dv = 'defw', bw = 'msetwo')[[2]],
                                   get_stats_withinparty(in_party, dv = 'defw', bw = 'msetwo')[[3]],
                                   get_stats_withinparty(in_party, dv = 'defw', bw = 'msetwo')[[4]],
                                   get_stats_withinparty(in_party, dv = 'defw', bw = 'msetwo')[[5]],
                                   "No"),
                      outparty1 = c(get_stats_withinparty(out_party, dv = 'defw', bw = 'msetwo')[[1]],
                                    get_stats_withinparty(out_party, dv = 'defw', bw = 'msetwo')[[2]],
                                    get_stats_withinparty(out_party, dv = 'defw', bw = 'msetwo')[[3]],
                                    get_stats_withinparty(out_party, dv = 'defw', bw = 'msetwo')[[4]],
                                    get_stats_withinparty(out_party, dv = 'defw', bw = 'msetwo')[[5]],
                                    "No"),
                      diff1 = c(as.numeric(get_stats_withinparty(in_party, dv = 'defw')[[1]]) - as.numeric(get_stats_withinparty(out_party, dv = 'defw')[[1]]),
                                round(calc_pv(-0.143), 3), 
                                " ",
                                " ", 
                                " ",
                                " "),
                      inparty2 = c(get_stats_withinparty(in_party, dv = 'defw', bw = 'msetwo', covs = covs_ip)[[1]],
                                   get_stats_withinparty(in_party, dv = 'defw', bw = 'msetwo', covs = covs_ip)[[2]],
                                   get_stats_withinparty(in_party, dv = 'defw', bw = 'msetwo', covs = covs_ip)[[3]],
                                   get_stats_withinparty(in_party, dv = 'defw', bw = 'msetwo', covs = covs_ip)[[4]],
                                   get_stats_withinparty(in_party, dv = 'defw', bw = 'msetwo', covs = covs_ip)[[5]],
                                   "Yes"),
                      outparty2 = c(get_stats_withinparty(out_party, dv = 'defw', bw = 'msetwo', covs = covs_op)[[1]],
                                    get_stats_withinparty(out_party, dv = 'defw', bw = 'msetwo', covs = covs_op)[[2]],
                                    get_stats_withinparty(out_party, dv = 'defw', bw = 'msetwo', covs = covs_op)[[3]],
                                    get_stats_withinparty(out_party, dv = 'defw', bw = 'msetwo', covs = covs_op)[[4]],
                                    get_stats_withinparty(out_party, dv = 'defw', bw = 'msetwo', covs = covs_op)[[5]],
                                    "Yes"),
                      diff2 = c(as.numeric(get_stats_withinparty(in_party, dv = 'defw', bw = 'msetwo', covs = covs_ip)[[1]]) - as.numeric(get_stats_withinparty(out_party, dv = 'defw', bw = 'msetwo', covs = covs_op)[[1]]),
                                round(calc_pv(-1.256), 3), 
                                " ",
                                " ", 
                                " ",
                                " "))

in_party <- dataset_wp %>%
  filter(within_party == 1 | (politician_dummy == 0 & election_after_arp == 1))
out_party <- dataset_wp %>%
  filter(within_party == 0 & yoe < 1879)

covs_ip <- make_covariates(in_party)
covs_op <- make_covariates(out_party)

# Contemporaneous control group
panel_b <- data.frame(names = c("Coefficient",
                                "SE (BC)",
                                "SE (Rob.)",
                                "N Treated",
                                "N Control", 
                                "Covariates"),
                      inparty1 = c(get_stats_withinparty(in_party, dv = 'defw', bw = 'msetwo')[[1]],
                                   get_stats_withinparty(in_party, dv = 'defw', bw = 'msetwo')[[2]],
                                   get_stats_withinparty(in_party, dv = 'defw', bw = 'msetwo')[[3]],
                                   get_stats_withinparty(in_party, dv = 'defw', bw = 'msetwo')[[4]],
                                   get_stats_withinparty(in_party, dv = 'defw', bw = 'msetwo')[[5]],
                                   "No"),
                      outparty1 = c(get_stats_withinparty(out_party, dv = 'defw', bw = 'msetwo')[[1]],
                                    get_stats_withinparty(out_party, dv = 'defw', bw = 'msetwo')[[2]],
                                    get_stats_withinparty(out_party, dv = 'defw', bw = 'msetwo')[[3]],
                                    get_stats_withinparty(out_party, dv = 'defw', bw = 'msetwo')[[4]],
                                    get_stats_withinparty(out_party, dv = 'defw', bw = 'msetwo')[[5]],
                                    "No"),
                      diff1 = c(as.numeric(get_stats_withinparty(in_party, dv = 'defw', bw = 'msetwo')[[1]]) - as.numeric(get_stats_withinparty(out_party, dv = 'defw', bw = 'msetwo')[[1]]),
                                round(calc_pv(-1.504), 3), 
                                " ",
                                " ", 
                                " ",
                                " "),
                      inparty2 = c(get_stats_withinparty(in_party, dv = 'defw', bw = 'msetwo', covs = covs_ip)[[1]],
                                   get_stats_withinparty(in_party, dv = 'defw', bw = 'msetwo', covs = covs_ip)[[2]],
                                   get_stats_withinparty(in_party, dv = 'defw', bw = 'msetwo', covs = covs_ip)[[3]],
                                   get_stats_withinparty(in_party, dv = 'defw', bw = 'msetwo', covs = covs_ip)[[4]],
                                   get_stats_withinparty(in_party, dv = 'defw', bw = 'msetwo', covs = covs_ip)[[5]],
                                   "Yes"),
                      outparty2 = c(get_stats_withinparty(out_party, dv = 'defw', bw = 'msetwo', covs = covs_op)[[1]],
                                    get_stats_withinparty(out_party, dv = 'defw', bw = 'msetwo', covs = covs_op)[[2]],
                                    get_stats_withinparty(out_party, dv = 'defw', bw = 'msetwo', covs = covs_op)[[3]],
                                    get_stats_withinparty(out_party, dv = 'defw', bw = 'msetwo', covs = covs_op)[[4]],
                                    get_stats_withinparty(out_party, dv = 'defw', bw = 'msetwo', covs = covs_op)[[5]],
                                    "Yes"),
                      diff2 = c(as.numeric(get_stats_withinparty(in_party, dv = 'defw', bw = 'msetwo', covs=covs_ip)[[1]]) - as.numeric(get_stats_withinparty(out_party, dv = 'defw', bw = 'msetwo', covs = covs_op)[[1]]),
                                round(calc_pv(-2.937), 3), 
                                " ",
                                " ", 
                                " ",
                                " "))

# Now, create the table
notitie <- "The table shows RD estimates using the MSE-optimal bandwidth \\\\citep{cattaneo2019practical} separately for both sides. The Dependent Variable is Log(Personal Wealth). I report bias-corrected and robust standard errors. Panel A uses the entire control group, whereas panel B opts for control-observations from the pre-and post-party periods respectively. Columns (1) and (2) contain estimates with no covariates, and columns (3) and (4) control for potential imbalances in lifespan, age and newspaper recommendations. *: p < 0.1, **: p < 0.05, ***: p < 0.01."
knitr::opts_current$set(label = "results_within_party_flexbw")
datasummary_df(bind_rows(panel_a, panel_b) %>%
                 rename(` ` = names,
                        "(1)" = inparty1,
                        "(2)" = outparty1,
                        `  ` = diff1,
                        "(3)" = inparty2,
                        "(4)" = outparty2,
                        `   ` = diff2),
               align = c("lllrllr"),
               out = "kableExtra",
               output = "latex",
               title = "RD Estimates of Political Rents according to Party Establishment") %>%
  kableExtra::group_rows("Panel A: All control observations", 1, 6) %>%
  kableExtra::group_rows("Panel B: Contemporaneous control observations", 7, 12) %>%
  kableExtra::add_header_above(c(" " = 1, "After" = 1, "Before" = 1, "Diff. (p-value)" = 1, "After" = 1, "Before" = 1,  "Diff. (p-value)" = 1)) %>%
  kableExtra::add_header_above(c(" " = 1, "No Covariates" = 3, "With Covariates" = 3)) %>%
  kableExtra::kable_styling(latex_options = c("hold_position"), font_size=10) %>%
  kableExtra::footnote(general = notitie, footnote_as_chunk = T, threeparttable = T, escape = F)  %>%
  kableExtra::save_kable("./Tables/in_out_party_effect_flexbw.tex")
