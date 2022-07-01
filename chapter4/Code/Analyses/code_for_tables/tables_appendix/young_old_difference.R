## table showing no different effects for young and old
library(readxl); library(tidyverse); library(hrbrthemes); library(rdrobust); library(modelsummary)
library(lubridate)
# calculate att etc. 
source("./Code/Analyses/function_calculate_itt_and_att.R")
source("./Code/Analyses/functions_for_tables.R")
source("./Code/Analyses/new_data_analysis.R")

# covariates
make_covariates <- function(dataset){
  cbind(
    dataset$totaal_aantal_stemmen,
    dataset$hoeveelste_keer_prob_alg,
    #dataset$lifespan,
    #dataset$age_at_election,
    #dataset$age_of_death,
    dataset$district_cath,
    dataset$birthplace_cath,
    dataset$district_agri
  )
  
}

make_covariates2 <- function(dataset){
  cbind(
    dataset$rec_ar,
    dataset$rec_kath,
    dataset$rec_lib,
    dataset$rec_soc,
    dataset$age_at_election,
    dataset$district_pop_1859
   # dataset$taxespercap_1859
  )
}

base <- firstrents_pooled %>%
  mutate(
    youngold_median = case_when(
    age_at_election > quantile(age_at_election, 0.50, na.rm =T) ~ "1",
    age_at_election < quantile(age_at_election, 0.50, na.rm =T) ~ "0",
    TRUE ~ NA_character_),
    youngold_20 = case_when(
           age_at_election > quantile(age_at_election, 0.80, na.rm =T) ~ "1",
           age_at_election < quantile(age_at_election, 0.20, na.rm =T) ~ "0",
           TRUE ~ NA_character_),
         youngold_30 = case_when(
           age_at_election > quantile(age_at_election, 0.70, na.rm =T) ~ "1",
           age_at_election < quantile(age_at_election, 0.30, na.rm =T) ~ "0",
           TRUE ~ NA_character_))

frp_med <- base %>%
  filter(!is.na(youngold_median))
frp_20 <- base %>%
  filter(!is.na(youngold_20))
frp_30 <- base %>%
  filter(!is.na(youngold_30))

panel_a <- data.frame(
  names = c('Coefficient (Young)',
            "SE (Young)",
            'Coefficient (Old)',
            "SE (Old)",
            "Mean DV Treated",
            "Mean DV Control",
            "N Treated",
            "N Control",
            "Bandwidth"),
  median1 = c(
    get_info(frp_med, 'defw', covs = make_covariates(frp_med), 
             weights = if_else(frp_med$youngold_median == 0, 1, 0))[c(1,2),],
    get_info(frp_med, 'defw', covs = make_covariates(frp_med), 
             weights = if_else(frp_med$youngold_median == 1, 1, 0))[1:7,]
    ),
  median2 = c(
    get_info(frp_med, 'defw', covs = make_covariates2(frp_med), 
             weights = if_else(frp_med$youngold_median == 0, 1, 0))[c(1,2),],
    get_info(frp_med, 'defw', covs = make_covariates2(frp_med), 
             weights = if_else(frp_med$youngold_median == 1, 1, 0))[1:7,]
  ),
  thirty1 = c(
    get_info(frp_30, 'defw', covs = make_covariates(frp_30), 
                     weights = if_else(frp_30$youngold_median == 0, 1, 0))[c(1,2),],
    get_info(frp_30, 'defw', covs = make_covariates(frp_30), 
           weights = if_else(frp_30$youngold_median == 1, 1, 0))[1:7,]
  ),
  thirty2 = c(
    get_info(frp_30, 'defw', covs = make_covariates2(frp_30), 
             weights = if_else(frp_30$youngold_median == 0, 1, 0))[c(1,2),],
    get_info(frp_30, 'defw', covs = make_covariates2(frp_30), 
             weights = if_else(frp_30$youngold_median == 1, 1, 0))[1:7,]
    ),
  twenty1 = c(
    get_info(frp_20, 'defw', covs = make_covariates(frp_20), 
             weights = if_else(frp_20$youngold_median == 0, 1, 0))[c(1,2),],
    get_info(frp_20, 'defw', covs = make_covariates(frp_20), 
             weights = if_else(frp_20$youngold_median == 1, 1, 0))[1:7,]
    ),
  twenty2 = c(
    get_info(frp_20, 'defw', covs = make_covariates2(frp_20), 
             weights = if_else(frp_20$youngold_median == 0, 1, 0))[c(1,2),],
    get_info(frp_20, 'defw', covs = make_covariates2(frp_20), 
             weights = if_else(frp_20$youngold_median == 1, 1, 0))[1:7,]
  ))

## now make the table

knitr::opts_current$set(label = "young_old_difference")
notitie <- "The table shows RD estimates using the MSE-optimal bandwidth \\\\citep{cattaneo2019practical}. The Dependent Variable is Log(1+Personal Wealth). I report bias-corrected standard errors clustered at the individual level. The first two columns show estimates of the returns for individuals aged above and below the median age, the second two estimates the results for individuals aged above the 70th quantile and below the 30th quantile, and the third pair shows the results for individuals aged above the 80th quantile and below the 20th quantile. Columns (1), (3) and (5) contain estimates with covariates including district characteristics, number of tries, number of votes, nd number of candidates. Columns (2), (4) and (6) control for number of tries, party, and district population. *: p < 0.1, **: p < 0.05, ***: p < 0.01."
datasummary_df(panel_a %>%
                 rename(` ` = names, 
                        "(1)" = median1,
                        "(2)" = median2,
                        "(3)" = thirty1,
                        "(4)" = thirty2,
                        "(5)" = twenty1,
                        "(6)" = twenty2), 
               out = "kableExtra",
               output = "latex",
               title = "RD Estimates For Young & Old Politicians") %>%
  kableExtra::add_header_above(c(" " = 1, "Median" = 2, "30 vs. 70" = 2, "20 vs. 80" = 2)) %>%
  kableExtra::kable_styling(latex_options = c("hold_position", "scale_down")) %>% #, full_width = F, font_size = 10) %>%
  kableExtra::footnote(general = notitie, footnote_as_chunk = T, threeparttable = T, escape = F)  %>%
  kableExtra::save_kable("./tables_appendix/young_old_difference.tex")

