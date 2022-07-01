#robustnesscheck_suffrage_and_turnout

#regression_tables_suffrage_and_turnout
library(readxl); library(tidyverse); library(hrbrthemes); library(rdrobust); library(modelsummary)
library(ggtext); library(lubridate)
# calculate att etc. 
source("./Code/Analyses/function_calculate_itt_and_att.R")
source("./Code/Analyses/functions_for_tables.R")
source("./Code/Analyses/code_for_tables/new_data_analysis.R")

make_covariates <- function(dataset){
  cbind(dataset$district_share_cath,
        log(1+dataset$birthplace_pop_1859), 
        dataset$birthplace_cath,
        dataset$birthplace_agri, 
        dataset$birthplace_indus, 
        dataset$lifespan,
        dataset$taxespercap_1889)
}

# before and after suffrage extensions
# panel A: before and after suffrage extension - election 1887
before <- firstrents_pooled %>%
  filter(verkiezingdatum < dmy("27-03-1888"))

after <- firstrents_pooled %>%
  filter(verkiezingdatum > dmy("26-03-1888"))

sec_before <- secondrents %>%
  filter(verkiezingdatum < dmy("27-03-1888"))
sec_after <- secondrents %>%
  filter(verkiezingdatum > dmy("26-03-1888"))

covs_before <- make_covariates(before)
covs_after <- make_covariates(after)
panel_a <- data.frame(names = c("Coefficient (ITT)", 
                                "SE (BC)",
                                "Mean DV Treated (1%)",
                                "Mean DV Control (1%)",
                                "N (Politicians)",
                                "N (Non-Politicians)",
                                "Period"),
                      # pooled, no covs
                      an_defw=c(get_coef(before, 'defw', bwselect='msecomb2'),
                                get_se_bc(before, 'defw', bwselect='msecomb2'),
                                mean_wealth_pols(before, 'defw'),
                                mean_wealth_nonpols(before, 'defw'),
                                n_pols(before, 'defw'),
                                n_nonpols(before, 'defw'),
                                "Before"),
                      an_defw_a = c(get_coef(after, 'defw', bwselect='msecomb2'),
                                    get_se_bc(after, 'defw', bwselect='msecomb2'),
                                    mean_wealth_pols(after, 'defw'),
                                    mean_wealth_nonpols(after, 'defw'),
                                    n_pols(after, 'defw'),
                                    n_nonpols(after, 'defw'),
                                    "After"),
                      # pooled, covs
                      an_defw_covs = c(get_coef(before, 'defw', covs = covs_before, bwselect='msecomb2'),
                                       get_se_bc(before, 'defw', covs = covs_before, bwselect='msecomb2'),
                                       mean_wealth_pols(before %>% filter(!is.na(covs_before)), 'defw'),
                                       mean_wealth_nonpols(before %>% filter(!is.na(covs_before)), 'defw'),
                                       n_pols(before, 'defw', covs = covs_before),
                                       n_nonpols(before, 'defw', covs = covs_before),
                                       "Before"),
                      an_defw_a_covs = c(get_coef(after, 'defw', covs = covs_after, bwselect='msecomb2'),
                                         get_se_bc(after, 'defw', covs = covs_after, bwselect='msecomb2'),
                                         mean_wealth_pols(after %>% filter(!is.na(covs_after)), 'defw'),
                                         mean_wealth_nonpols(after %>% filter(!is.na(covs_after)), 'defw'),
                                         n_pols(after, 'defw', covs = covs_after),
                                         n_nonpols(after, 'defw', covs = covs_after),
                                         "After"),
                      secr_b = c(get_coef(sec_before, 'defw', bwselect='msecomb2'),
                                 get_se_bc(sec_before, 'defw', bwselect='msecomb2'),
                                 mean_wealth_pols(sec_before, 'defw'),
                                 mean_wealth_nonpols(sec_before, 'defw'),
                                 n_pols(sec_before, 'defw'),
                                 n_nonpols(sec_before, 'defw'),
                                 "Before"),
                      secr_a = c(get_coef(sec_after, 'defw', bwselect='msecomb2'),
                                 get_se_bc(sec_after, 'defw', bwselect='msecomb2'),
                                 mean_wealth_pols(sec_after, 'defw'),
                                 mean_wealth_nonpols(sec_after, 'defw'),
                                 n_pols(sec_after, 'defw'),
                                 n_nonpols(sec_after, 'defw'),
                                 "After"))

# panel B: before and after suffrage extension- ex1896
before2 <- firstrents_pooled %>%
  filter(verkiezingdatum < dmy("15-06-1897"))
after2 <- firstrents_pooled %>%
  filter(verkiezingdatum > dmy("14-06-1897"))

make_covariates <- function(dataset){
  cbind(dataset$age_at_election, 
        dataset$rec_soc,
        dataset$lifespan)
}

covs_before2 <- make_covariates(before2)
covs_after2 <- make_covariates(after2)

sec_before2 <- secondrents %>%
  filter(verkiezingdatum < dmy("15-06-1897"))
sec_after2 <- secondrents %>%
  filter(verkiezingdatum > dmy("14-06-1897"))

panel_b <- data.frame(names = c("Coefficient (ITT)", 
                                "SE (BC)",
                                "Mean DV Treated (1%)",
                                "Mean DV Control (1%)",
                                "N (Politicians)",
                                "N (Non-Politicians)",
                                "Period"),
                      # pooled, no covs
                      an_defw=c(get_coef(before2, 'defw', bwselect='msecomb2'),
                                get_se_bc(before2, 'defw', bwselect='msecomb2'),
                                mean_wealth_pols(before2, 'defw'),
                                mean_wealth_nonpols(before2, 'defw'),
                                n_pols(before2, 'defw'),
                                n_nonpols(before2, 'defw'),
                                "Before"),
                      an_defw_a = c(get_coef(after2, 'defw', bwselect='msecomb2'),
                                    get_se_bc(after2, 'defw', bwselect='msecomb2'),
                                    mean_wealth_pols(after2, 'defw'),
                                    mean_wealth_nonpols(after2, 'defw'),
                                    n_pols(after2, 'defw'),
                                    n_nonpols(after2, 'defw'),
                                    "After"),
                      # pooled, covs
                      an_defw_covs = c(get_coef(before2, 'defw', covs = covs_before2, bwselect='msecomb2'),
                                       get_se_bc(before2, 'defw', covs = covs_before2, bwselect='msecomb2'),
                                       mean_wealth_pols(before2 %>% filter(!is.na(covs_before2)), 'defw'),
                                       mean_wealth_nonpols(before2 %>% filter(!is.na(covs_before2)), 'defw'),
                                       n_pols(before2, 'defw', covs = covs_before2),
                                       n_nonpols(before2, 'defw', covs = covs_before2),
                                       "Before"),
                      an_defw_a_covs = c(get_coef(after2, 'defw', covs = covs_after2, bwselect='msecomb2'),
                                         get_se_bc(after2, 'defw', covs = covs_after2, bwselect='msecomb2'),
                                         mean_wealth_pols(after2 %>% filter(!is.na(covs_after2)), 'defw'),
                                         mean_wealth_nonpols(after2 %>% filter(!is.na(covs_after2)), 'defw'),
                                         n_pols(after2, 'defw', covs = covs_after2),
                                         n_nonpols(after2, 'defw', covs = covs_after2),
                                         "After"),
                      secr_b = c(get_coef(sec_before2, 'defw', bwselect='msecomb2'),
                                 get_se_bc(sec_before2, 'defw', bwselect='msecomb2'),
                                 mean_wealth_pols(sec_before2, 'defw'),
                                 mean_wealth_nonpols(sec_before2, 'defw'),
                                 n_pols(sec_before2, 'defw'),
                                 n_nonpols(sec_before2, 'defw'),
                                 "Before"),
                      secr_a = c(get_coef(sec_after2, 'defw', bwselect='msecomb2'),
                                 get_se_bc(sec_after2, 'defw', bwselect='msecomb2'),
                                 mean_wealth_pols(sec_after2, 'defw'),
                                 mean_wealth_nonpols(sec_after2, 'defw'),
                                 n_pols(sec_after2, 'defw'),
                                 n_nonpols(sec_after2, 'defw'),
                                 "After"))


# panel C: high versus low turnout

make_covariates <- function(dataset){
  cbind(dataset$district_share_cath,
              log(1+dataset$birthplace_pop_1859), 
              dataset$birthplace_cath,
              dataset$birthplace_agri, 
              dataset$birthplace_indus, 
              dataset$lifespan,
              dataset$taxespercap_1889)
}

firstrents_pooled <- firstrents_pooled %>%
  mutate(perc_diff_turn = diff_turn/district_pop_1889,
         perc_turn = aantal_stemmen_geldig / district_pop_1889)

secondrents <- secondrents %>%
  mutate(perc_diff_turn = diff_turn/district_pop_1889,
         perc_turn = aantal_stemmen_geldig / district_pop_1889)

dataset_high <- firstrents_pooled %>% filter(perc_diff_turn > 0)
dataset_low <- firstrents_pooled %>% filter(perc_diff_turn < 0)

secr_high <- secondrents %>% filter(perc_diff_turn > 0)
secr_low <- secondrents %>% filter(perc_diff_turn < 0)

covs_dataset_high <- make_covariates(dataset_high)
covs_dataset_low <- make_covariates(dataset_low)

panel_c <- data.frame(names = c("Coefficient (ITT)", 
                                "SE (BC)",
                                "Mean DV Treated (1%)",
                                "Mean DV Control (1%)",
                                "N (Politicians)",
                                "N (Non-Politicians)",
                                "Turnout"),
                      # pooled, no covs
                      an_defw=c(get_coef(dataset_high, 'defw', bwselect='msecomb2'),
                                get_se_bc(dataset_high, 'defw', bwselect='msecomb2'),
                                mean_wealth_pols(dataset_high, 'defw'),
                                mean_wealth_nonpols(dataset_high, 'defw'),
                                n_pols(dataset_high, 'defw'),
                                n_nonpols(dataset_high, 'defw'),
                                "High"),
                      an_defw_a = c(get_coef(dataset_low, 'defw', bwselect='msecomb2'),
                                    get_se_bc(dataset_low, 'defw', bwselect='msecomb2'),
                                    mean_wealth_pols(dataset_low, 'defw'),
                                    mean_wealth_nonpols(dataset_low, 'defw'),
                                    n_pols(dataset_low, 'defw'),
                                    n_nonpols(dataset_low, 'defw'),
                                    "Low"),
                      # pooled, covs
                      an_defw_covs = c(get_coef(dataset_high, 'defw', covs = covs_dataset_high, bwselect='msecomb2'),
                                       get_se_bc(dataset_high, 'defw', covs = covs_dataset_high, bwselect='msecomb2'),
                                       mean_wealth_pols(dataset_high %>% filter(!is.na(covs_dataset_high)), 'defw'),
                                       mean_wealth_nonpols(dataset_high %>% filter(!is.na(covs_dataset_high)), 'defw'),
                                       n_pols(dataset_high, 'defw', covs = covs_dataset_high),
                                       n_nonpols(dataset_high, 'defw', covs = covs_dataset_high),
                                       "High"),
                      an_defw_a_covs = c(get_coef(dataset_low, 'defw', covs = covs_dataset_low, bwselect='msecomb2'),
                                         get_se_bc(dataset_low, 'defw', covs = covs_dataset_low, bwselect='msecomb2'),
                                         mean_wealth_pols(dataset_low %>% filter(!is.na(covs_dataset_low)), 'defw'),
                                         mean_wealth_nonpols(dataset_low %>% filter(!is.na(covs_dataset_low)), 'defw'),
                                         n_pols(dataset_low, 'defw', covs = covs_dataset_low),
                                         n_nonpols(dataset_low, 'defw', covs = covs_dataset_low),
                                         "Low"),
                      secr_b = c(get_coef(secr_high, 'defw', bwselect='msecomb2'),
                                 get_se_bc(secr_high, 'defw', bwselect='msecomb2'),
                                 mean_wealth_pols(secr_high, 'defw'),
                                 mean_wealth_nonpols(secr_high, 'defw'),
                                 n_pols(secr_high, 'defw'),
                                 n_nonpols(secr_high, 'defw'),
                                 "High"),
                      secr_a = c(get_coef(secr_low, 'defw', bwselect='msecomb2'),
                                 get_se_bc(secr_low, 'defw', bwselect='msecomb2'),
                                 mean_wealth_pols(secr_low, 'defw'),
                                 mean_wealth_nonpols(secr_low, 'defw'),
                                 n_pols(secr_low, 'defw'),
                                 n_nonpols(secr_low, 'defw'),
                                 "Low"))

# make table
notitie <- "Table showing Bias-corrected and Robust standard errors clustered at the Birthplace-level. The dependent variable in all cases is Log(1+Wealth). All effects are estimated under the MSE-optimal bandwidth under the option \\\\textit{msecomb2}. Panel A shows the differences in political rents before and after the suffrage extension in 1887. Panel B shows the differences in returns to politics before and after the suffrage extension in 1896. Panel C shows the differences in returns between elections with a positive turnout difference vis-a-vis elections with a negative turnout differences in comparison to the preceding general election. In panels A and C, I control for economic and religious composition of the politicians' district and the politicians' birthplace, as well as for lifespan. In panel B, due to data constraints, I control for age, lifespan and newspaper recommendations. *: p < 0.10, **: p < 0.05, ***: p < 0.01."

knitr::opts_current$set(label = "robustness_rdd_results_suffrage_ext_turnout")
datasummary_df(bind_rows(panel_a, panel_b, panel_c) %>%
                 rename(` ` = names, 
                        "(1)" = an_defw,
                        "(2)" = an_defw_a,
                        "(3)" = an_defw_covs,
                        "(4)"  = an_defw_a_covs,
                        "(5)" = secr_b,
                        "(6)" = secr_a), 
               out = "kableExtra",
               output = "latex",
               title = "Robustness to RD Estimates - Suffrage Extension and Turnout") %>%
  kableExtra::add_header_above(c(" " = 1, "Without Covariates" = 2, "With Covariates" = 2, "Second Rents" = 2)) %>%
  kableExtra::group_rows("Before and After Suffrage Extension - 1887", 1, 7)  %>%
  kableExtra::group_rows("Before and After Suffrage Extension - 1896", 8, 14) %>%
  kableExtra::group_rows("High and Low Turnout", 15, 21) %>%
  kableExtra::kable_styling(latex_options = c("hold_position", "scale_down")) %>%
  kableExtra::footnote(general = notitie, footnote_as_chunk = T, threeparttable = T, escape = F)  %>%
  kableExtra::save_kable("./Tables/robustness_rdd_results_suffrage_ext_turnout.tex")
