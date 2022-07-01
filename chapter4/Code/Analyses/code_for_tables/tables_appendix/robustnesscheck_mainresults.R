# robustness check
library(readxl); library(tidyverse); library(hrbrthemes); library(rdrobust); library(modelsummary)
library(ggtext); library(lubridate)
# calculate att etc. 
source("./Code/Analyses/function_calculate_itt_and_att.R")
source("./Code/Analyses/functions_for_tables.R")
source("./Code/Analyses/new_data_analysis.R")

# Regression table with the main results
# Panel A: ITT
covariates <- cbind(firstrents_firsttry$district_share_cath,
                    log(1+firstrents_firsttry$birthplace_pop_1859), 
                    firstrents_firsttry$birthplace_cath,
                    firstrents_firsttry$birthplace_agri, 
                    firstrents_firsttry$birthplace_indus, 
                    firstrents_firsttry$lifespan,
                    firstrents_firsttry$taxespercap_1889)

covariates2 <- cbind(firstrents_pooled$district_share_cath,
                     log(1+firstrents_pooled$birthplace_pop_1859), 
                     firstrents_pooled$birthplace_cath,
                     firstrents_pooled$birthplace_agri, 
                     firstrents_pooled$birthplace_indus, 
                     firstrents_pooled$lifespan,
                     firstrents_pooled$taxespercap_1889)

covariates3 <- cbind(firstrents_secondtry$district_share_cath,
                      log(1+firstrents_secondtry$birthplace_pop_1859), 
                      firstrents_secondtry$birthplace_cath,
                      firstrents_secondtry$birthplace_agri, 
                      firstrents_secondtry$birthplace_indus, 
                      firstrents_secondtry$lifespan,
                      firstrents_secondtry$taxespercap_1889)

panel_a <- data.frame(names = c("Coefficient (ITT)", 
                                "SE (BC)",
                                "Mean DV Treated (1%)",
                                "Mean DV Control (1%)",
                                "N (Politicians)",
                                "N (Non-Politicians)",
                                "Bandwidth"),
                      an_defw = get_info(firstrents_firsttry, 'defw', covs = NULL, bwselect = 'msecomb2'),
                      an_defw_w = get_info(firstrents_firsttry, 'defw', bw_mult = 2, bwselect = 'msecomb2'),
                      an_defw_cov = get_info(firstrents_firsttry, 'defw', covs = covariates, bwselect = 'msecomb2'),
                      an_defw2_cov = get_info(firstrents_firsttry, 'defw', covs = covariates, bw_mult = 2, bwselect = 'msecomb2'),
                      an_defw_sec_cov = get_info(firstrents_secondtry, 'defw', covs = covariates3, bwselect = 'msecomb2'),
                      an_defw2_sec_cov = get_info(firstrents_secondtry, 'defw', covs = covariates3, bw_mult = 2, bwselect = 'msecomb2'),
                      an_defw_pooled_cov = get_info(firstrents_pooled, 'defw', covs = covariates2, bwselect = 'msecomb2'),
                      an_defw2_pooled_cov = get_info(firstrents_pooled, 'defw', covs = covariates2, bw_mult = 2, bwselect = 'msecomb2'))

notitie <- "Table showing Bias-corrected standard errors clustered at the Birthplace-level. The first two columns show univariate regressions under the optimal MSE bandwidth with the option \\\\textit{msecomb2}, and twice the optimal bandwidth. In columns 3 and 4, selected covariates are added, an alternative selection to the covariates in the main results. In particular, the regression controls for district religious share, birthplace population, birthplace religious share, district GDP, lifespan and birthplace labor force composition. Columns 5 and 6 focus on second-triers and columns 7 and 8 pool all attempts. *: p < 0.10, **: p < 0.05, ***: p < 0.01. "

knitr::opts_current$set(label = "mainresults_robust")
datasummary_df(panel_a %>%
                 rename(` ` = names, 
                        "(1)" = out,
                        "(2)" = out.1,
                        "(3)" = out.2,
                        "(4)" = out.3,
                        "(5)" = out.4,
                        "(6)" = out.5,
                        "(7)" = out.6,
                        "(8)" = out.7), 
               out = "kableExtra",
               output = "latex",
               title = "Robustness to Main RD Estimates - 1st Period: BW Selector") %>%
  kableExtra::add_header_above(c(" " = 1, "First Triers" = 4, "Second Triers" = 2, "All Triers" = 2)) %>%
  kableExtra::kable_styling(latex_options = c("hold_position", "scale_down")) %>% #, full_width = F, font_size = 10) %>%
  kableExtra::footnote(general = notitie, footnote_as_chunk = T, threeparttable = T, escape = F)  %>%
  kableExtra::save_kable("./tables_appendix/robustness_rdd_mainresults_firststint.tex")
