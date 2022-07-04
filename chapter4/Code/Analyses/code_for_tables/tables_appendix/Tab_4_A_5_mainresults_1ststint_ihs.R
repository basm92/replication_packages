# regression_tables
library(readxl); library(tidyverse); library(hrbrthemes); library(rdrobust); library(modelsummary)
library(ggtext); library(lubridate)
# calculate att etc. 
source("./Code/Analyses/function_calculate_itt_and_att.R")
source("./Code/Analyses/functions_for_tables.R")
source("./Code/Analyses/new_data_analysis.R")

# Panel A: ITT
covariates <- cbind(log(1+firstrents_firsttry$birthplace_pop_1859), 
                    firstrents_firsttry$birthplace_agri, 
                    firstrents_firsttry$birthplace_indus, 
                    #firstrents_firsttry$lifespan,
                    firstrents_firsttry$rec_soc,
                    as.factor(firstrents_firsttry$suffrage_period))

covariates2 <- cbind(log(1+firstrents_pooled$birthplace_pop_1859), 
                     firstrents_pooled$birthplace_agri, 
                     firstrents_pooled$birthplace_indus, 
                     #firstrents_pooled$lifespan,
                     firstrents_pooled$rec_soc)

covariates3 <- cbind(log(1+firstrents_secondtry$birthplace_pop_1859), 
                     firstrents_secondtry$birthplace_agri, 
                     firstrents_secondtry$birthplace_indus, 
                     #firstrents_secondtry$lifespan,
                     firstrents_secondtry$rec_soc)
ihs <- function(x) { if_else(log(x + sqrt(x^2 -1)) > 0, log(x + sqrt(x^2 - 1)), 0)}

firstrents_pooled <- firstrents_pooled %>%
  mutate(ihs_wealth = ihs(deflated_wealth))
firstrents_firsttry <- firstrents_firsttry %>%
  mutate(ihs_wealth = ihs(deflated_wealth))
firstrents_secondtry <- firstrents_secondtry %>%
  mutate(ihs_wealth = ihs(deflated_wealth))


panel_a <- data.frame(names = c("Coefficient (ITT)", 
                                "SE (BC)",
                                "Mean DV Treated (1%)",
                                "Mean DV Control (1%)",
                                "N (Politicians)",
                                "N (Non-Politicians)",
                                "Bandwidth"),
                      an_defw = get_info(firstrents_firsttry, 'ihs_wealth', covs = NULL),
                      an_defw_w = get_info(firstrents_firsttry, 'ihs_wealth', bw_mult = 2),
                      an_defw_cov = get_info(firstrents_firsttry, 'ihs_wealth', covs = covariates),
                      an_defw2_cov = get_info(firstrents_firsttry, 'ihs_wealth', covs = covariates, bw_mult = 2),
                      an_defw_sec_cov = get_info(firstrents_secondtry, 'ihs_wealth', covs = covariates3),
                      an_defw2_sec_cov = get_info(firstrents_secondtry, 'ihs_wealth', covs = covariates3, bw_mult = 2),
                      an_defw_pooled_cov = get_info(firstrents_pooled, 'ihs_wealth', covs = covariates2),
                      an_defw2_pooled_cov = get_info(firstrents_pooled, 'ihs_wealth', covs = covariates2, bw_mult = 2))

notitie <- "Table showing Bias-corrected standard errors clustered at the individual-level. The dependent variable is ihs(Personal Wealth). The first two columns show univariate regressions under the optimal MSE bandwidth, and twice the optimal bandwidth. In columns 3 and 4, selected covariates are added, in particular, covariates that seemed to be unbalanced at the 2\\\\% cutoff. In particular, the regression controls for birthplace population, birthplace characteristics, age at election, and socialist recommendations. In addition, I control for politicians' lifespan. Columns 5 and 6 focus on second-triers and columns 7 and 8 pool all attempts. *: p < 0.10, **: p < 0.05, ***: p < 0.01. "

knitr::opts_current$set(label = "mainresults_ihsversion")
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
               title = "Robustness to Main RD Estimates - 1st Period: Ihs") %>%
  kableExtra::add_header_above(c(" " = 1, "First Triers" = 4, "Second Triers" = 2, "All Triers" = 2)) %>%
  kableExtra::kable_styling(latex_options = c("hold_position", "scale_down")) %>% #, full_width = F, font_size = 10) %>%
  kableExtra::footnote(general = notitie, footnote_as_chunk = T, threeparttable = T, escape = F)  %>%
  kableExtra::save_kable("./tables_appendix/rdd_mainresults_firststint_ihs_version.tex")
