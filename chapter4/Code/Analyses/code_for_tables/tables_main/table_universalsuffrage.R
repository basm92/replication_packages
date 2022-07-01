#table_universalsuffrage

# regression_tables
library(readxl); library(tidyverse); library(hrbrthemes); library(rdrobust); library(modelsummary)
library(ggtext); library(lubridate)
# calculate att etc. 
source("./Code/Analyses/function_calculate_itt_and_att.R")
source("./Code/Analyses/functions_for_tables.R")
source("./Code/Analyses/new_data_analysis.R")

make_covariates <- function(dataset){
  cbind(log(1+dataset$birthplace_pop_1859), 
        dataset$birthplace_agri, 
        dataset$birthplace_indus, 
        dataset$lifespan,
        dataset$rec_soc,
        dataset$taxespercap_1859,
        dataset$rec_ar,
        dataset$rec_lib,
        dataset$rec_kath)
}

make_covariates2 <- function(dataset){
  
  cbind(
        dataset$hoeveelste_keer_prob_alg,
        dataset$birthplace_agri,
        dataset$birthplace_indus,
        dataset$lifespan,
        dataset$rec_kath,
        dataset$rec_ar,
        dataset$rec_soc,
        dataset$rec_lib)
}

panel_a <- data.frame(
  names = c(
    "Coefficient (ITT)",
    "SE (BC)",
    "Mean DV Treated (1%)",
    "Mean DV Control (1%)",
    "N Treated",
    "N Control",
    "Bandwidth"
  ),
  get_info(firstrents_pooled %>% filter(verkiezingdatum < dmy("01-09-1887")), 
         'defw', 
         covs = make_covariates(firstrents_pooled %>% filter(verkiezingdatum < dmy("01-09-1887")))),
  get_info(firstrents_pooled %>% filter(verkiezingdatum < dmy("01-09-1887")), 
           'defw', 
           covs = make_covariates2(firstrents_pooled %>% filter(verkiezingdatum < dmy("01-09-1887")))),
  get_info(firstrents_pooled %>% filter(between(verkiezingdatum, dmy("01-09'1887"), dmy("15-06-1897"))), 
         'defw',
         covs = make_covariates(firstrents_pooled %>% filter(between(verkiezingdatum, dmy("01-09'1887"), dmy("15-06-1897"))))),
  get_info(firstrents_pooled %>% filter(between(verkiezingdatum, dmy("01-09'1887"), dmy("15-06-1897"))), 
           'defw',
           covs = make_covariates2(firstrents_pooled %>% filter(between(verkiezingdatum, dmy("01-09'1887"), dmy("15-06-1897"))))),
  get_info(firstrents_pooled %>% filter(verkiezingdatum > dmy("15-06-1897")), 
         'defw',
         covs = make_covariates(firstrents_pooled %>% filter(verkiezingdatum > dmy("15-06-1897")))),
  get_info(firstrents_pooled %>% filter(verkiezingdatum > dmy("15-06-1897")), 
           'defw',
           covs = make_covariates2(firstrents_pooled %>% filter(verkiezingdatum > dmy("15-06-1897")))))


# make table
notitie <- "Table showing the effect of being elected into politics on personal end-of-life wealth. The dependent variable is Log(1+Wealth at Death). The estimates show Bias-corrected and Robust standard errors clustered at the individual-level. All effects are estimated under the MSE-optimal bandwidth. I use two sets of covariates: in columns (1), (3) and (5) I control for birtplace population, and demographics, and newspaper recommendations (party). In columns (2), (4) and (6) I control for number of tries, birthplace demographics, district demographics and number of tries. *: p < 0.10, **: p < 0.05, ***: p < 0.01."

knitr::opts_current$set(label = "rdd_results_universalsuffrage")
datasummary_df(panel_a %>%
                 rename(` ` = names, 
                        "(1)" = out,
                        "(2)" = out.1,
                        "(3)" = out.2,
                        "(4)" = out.3,
                        "(5)" = out.4,
                        "(6)" = out.5), 
               out = "kableExtra",
               output = "latex",
               title = "RD Estimates of Being Elected on Personal Wealth Before/After Suffrage Extensions") %>%
  kableExtra::add_header_above(c(" " = 1, "Before 1887" = 2, "Between 1887-1897" = 2, "After 1897" = 2)) %>%
  kableExtra::kable_styling(latex_options = c("hold_position", "scale_down")) %>%
  kableExtra::footnote(general = notitie, footnote_as_chunk = T, threeparttable = T, escape = F)  %>%
  kableExtra::save_kable("./tables_main/rdd_results_universalsuffrage.tex")
