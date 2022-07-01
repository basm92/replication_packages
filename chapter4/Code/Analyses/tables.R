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


# see ?datasummary for new columns to find out how to specify where the new column should be

notes <- c("The table contains means for various sets of variables conditioned on the absolute margin being < 0.2 (left panel) and <0.05 (right panel). The first two columns represent the means for politicians and non-politicians respectively, and the third column shows the p-value of a Welch two-sample t-test. The last column shows the local non-parametric RD estimate, estimated by the procedure in \\\\cite{cattaneo2019practical}. HC-Robust standard errors are shown between brackets. Significance is indicated by *: p < 0.1, **: p < 0.05, ***: p < 0.01.")
knitr::opts_current$set(label = "covbal")
datasummary(data = dataset,
            align = c("llllllll"),
            formula = 
              rec_ar + 
              rec_lib + 
              rec_soc + 
              rec_kath +
              lifespan +
              age_at_election +
              yod +
            #  age_of_death + 
              yoe + 
              howmany_before_alg +
              log(turnout) +
              log(turnout_previous_el) +
              log(1+birthplace_pop_1859) +
              birthplace_share_cath +
              birthplace_share_prot +
              birthplace_agri +
              birthplace_indus +
              taxespercap_1859 +
              taxespercap_1889 +
              distance_bp_hag +
              district_share_prot +
              district_share_cath +
              district_agri +
              district_indus ~ (`Politicians`=mean_treatment_far)  + 
              (`Non-Politicians`=mean_control_far) + 
              (`p-val.`=p_val_far) + 
              (`Politicians`=mean_treatment_close) + (`Non-Politicians`=mean_control_close) +
              (`p-val.` = p_val_close) + (`RD Estimate (SD)`=get_coef_and_se2), 
            out = "kableExtra",
            output = "latex",
            title = "Covariate Balance") %>%
  kableExtra::group_rows("Panel A: Newspaper Recommendations", 1,4) %>%
  kableExtra::group_rows("Panel B: Pre-Election Demographic Characteristics", 5, 7) %>%
  kableExtra::group_rows("Panel C: Election Characteristics", 8, 11) %>%
  kableExtra::group_rows("Panel D: Birthplace Characteristics", 12, 19)  %>%
  kableExtra::group_rows("Panel E: District Characteristics", 20, 23) %>% 
  kableExtra::add_header_above(c(" " = 1, "Margin < 0.2" = 3, "Margin < 0.05" = 3, " " = 1)) %>%
  kableExtra::kable_styling(latex_options = c("hold_position", "scale_down")) %>%
  kableExtra::footnote(general = notes, footnote_as_chunk = T, threeparttable = T, escape = F)  %>%
  kableExtra::save_kable("./Tables/covariate_balance.tex")

# Canay and Kanat (2018) equality of dist. test:
#RDperm(W = c("rec_ar", 
#             "rec_lib", 
#             "rec_soc", 
#             "rec_kath", 
#             "lifespan", 
#             "age_at_election",
#             "yod",
#             "yoe",
#             "howmany_before_alg"), 
#       z = "margin", 
#       q_type = 25,
#       data = dataset)

# Still another, more classical descr. stat table here:
knitr::opts_current$set(label = "descriptivestats")
notes <- "This table shows descriptive statistics for politicians (left panel) and non-politicians (right panel). In panel A, I show newspaper recommendations for each major political faction. Panel B discusses demographic characteristics, and panel C discusses characteristics related to elections. Panels D and E contain birthplace and district characteristics. Panel F contains ex-post variables and Panel G contains several variables related to party and career characteristics."
datasummary(data = dataset %>%
              mutate(politician_indic = factor(politician_indic, levels=c('Politician','Non-Politician')),
                     lib = if_else(party_category=="liberal", 1, 0),
                     prot = if_else(party_category=="protestant", 1, 0),
                     cath = if_else(party_category=="catholic", 1, 0)),
            formula = 
              rec_ar + 
              rec_lib + 
              rec_soc + 
              rec_kath +
              lifespan +
              age_at_election +
              yod +
              yoe + 
              howmany_before_alg +
              log(turnout) +
              log(turnout_previous_el) +
              log(1+birthplace_pop_1859) +
              birthplace_share_cath +
              birthplace_share_prot +
              birthplace_agri +
              birthplace_indus +
              taxespercap_1859 +
              taxespercap_1889 +
              distance_bp_hag +
              district_share_prot +
              district_share_cath +
              district_agri +
              district_indus +
              defw +
              age_of_death + 
              election_after_arp +
              election_after_rk +
              election_after_lib + 
              lib + 
              prot +
              cath +
              prof_business +
              prof_politics +
              prof_colonial 
              ~ politician_indic*(Mean + SD + Min + Max + N),
            out = "kableExtra",
            output="latex",
            title = "Descriptive Statistics") %>%
  kableExtra::group_rows("Panel A: Newspaper Recommendations", 1,4) %>%
  kableExtra::group_rows("Panel B: Demographic Characteristics Politicians", 5, 8) %>%
  kableExtra::group_rows("Panel C: Election Characteristics", 9, 12) %>%
  kableExtra::group_rows("Panel D: Birthplace Characteristics", 13, 19)  %>%
  kableExtra::group_rows("Panel E: District Characteristics", 20, 23) %>% 
  kableExtra::group_rows("Panel F: Ex-Post Characteristics", 24, 25) %>%
  kableExtra::group_rows("Panel G: Party and Career Characteristics", 26, 34) %>%
  kableExtra::kable_styling(latex_options = c("hold_position", "scale_down")) %>%
  kableExtra::footnote(general = notes, footnote_as_chunk = T, threeparttable = T, escape = F)  %>%
  kableExtra::save_kable("./Tables/descriptivestats_trad.tex")

# Density plot of margin, defw, and defw2
p1 <- dataset %>%
  ggplot(aes(x = margin)) + geom_density() +
  theme_classic() + ylab("Density") + xlab("Margin")
p2 <- dataset %>%
  ggplot(aes(x = defw)) + geom_density() +
  theme_classic() + ylab("Density") + xlab("Log(Wealth)")
p3 <- dataset %>%
  ggplot(aes(x = defw2)) + geom_density() +
  theme_classic() + ylab("Density") + xlab("Ihs(Wealth)")

plottie <- cowplot::plot_grid(p1, p2, p3, ncol = 3)
cowplot::save_plot("./Tables/Density_Plot_MarginWealth.pdf", 
                   plottie,
                   base_height = 3.5, 
                   base_width = 10)

# Regression table with the main results
# Panel A: Without Covariates
panel_a <- data.frame(names = c("Coefficient", 
                     "SE (BC)",
                     "SE (Rob.)",
                     "Mean DV Politicians (1%)",
                     "Mean DV Non-Politicians (1%)",
                     "N (Politicians)",
                     "N (Non-Politicians)",
                     "Bandwidth"),
           an_defw=c(get_coef(dataset$defw),
                      get_se_bc(dataset$defw),
                      get_se_rob(dataset$defw),
                      mean_wealth_pols(dataset$defw),
                      mean_wealth_nonpols(dataset$defw),
                      n_pols(dataset$defw),
                      n_nonpols(dataset$defw),
                      "Optimal"),
           an_defw_w = c(get_coef_w(dataset$defw),
                         get_se_bc_w(dataset$defw),
                         get_se_rob_w(dataset$defw),
                         mean_wealth_pols(dataset$defw),
                         mean_wealth_nonpols(dataset$defw),
                        n_pols(dataset$defw),
                        n_nonpols(dataset$defw),
                         "2 x Optimal"),
           an_defw2 = c(get_coef(dataset$defw2),
                        get_se_bc(dataset$defw2),
                        get_se_rob(dataset$defw2),
                        mean_wealth_pols(dataset$defw2),
                        mean_wealth_nonpols(dataset$defw2),
                        n_pols(dataset$defw2),
                        n_nonpols(dataset$defw2),
                        "Optimal"),
           an_defw2_w = c(get_coef_w(dataset$defw2),
                          get_se_bc_w(dataset$defw2),
                          get_se_rob_w(dataset$defw2),
                          mean_wealth_pols(dataset$defw2),
                          mean_wealth_nonpols(dataset$defw2),
                          n_pols(dataset$defw2),
                          n_nonpols(dataset$defw2),
                          "2 x Optimal")
)


# Panel B: With Covariates which are significant in Panel A at 0.05 cutoff point
# yoe, howmany_before_alg, log(1+birthplace_pop_1859), birthplace_agri, 
# birthplace_indus, age_at_election, yod, rec_soc
# rdrobust(y=dataset$defw, x = dataset$margin, 

covariates <- cbind(dataset$yoe, 
                   dataset$howmany_before_alg,
                   log(1+dataset$birthplace_pop_1859), 
                   dataset$birthplace_agri, 
                   dataset$birthplace_indus, 
                   dataset$age_at_election, 
                   dataset$yod, 
                   dataset$rec_soc,
                   dataset$lifespan)


panel_b <- data.frame(names = c("Coefficient", 
                                "SE (BC)",
                                "SE (Rob.)",
                                "Mean DV Politicians (1%)",
                                "Mean DV Non-Politicians (1%)",
                                "N (Politicians)",
                                "N (Non-Politicians)",
                                "Bandwidth"),
                      an_defw=c(get_coef_cov(dataset$defw, covs = covariates),
                                get_se_bc_cov(dataset$defw, covs = covariates),
                                get_se_rob_cov(dataset$defw, covs = covariates),
                                mean_wealth_pols(dataset$defw),
                                mean_wealth_nonpols(dataset$defw),
                                n_pols_cov(dataset$defw, covs = covariates),
                                n_nonpols_cov(dataset$defw, covs = covariates),
                                "Optimal"),
                      an_defw_w = c(get_coef_cov(dataset$defw, bw_mult = 2, covs = covariates),
                                    get_se_bc_cov(dataset$defw, bw_mult = 2, covs = covariates),
                                    get_se_rob_cov(dataset$defw, bw_mult = 2, covs = covariates),
                                    mean_wealth_pols(dataset$defw),
                                    mean_wealth_nonpols(dataset$defw),
                                    n_pols_cov(dataset$defw, covs = covariates),
                                    n_nonpols_cov(dataset$defw, covs = covariates),
                                    "2 x Optimal"),
                      an_defw2 = c(get_coef_cov(dataset$defw2, covs = covariates),
                                   get_se_bc_cov(dataset$defw2, covs = covariates),
                                   get_se_rob_cov(dataset$defw2, covs = covariates),
                                   mean_wealth_pols(dataset$defw2),
                                   mean_wealth_nonpols(dataset$defw2),
                                   n_pols_cov(dataset$defw2, covs = covariates),
                                   n_nonpols_cov(dataset$defw2, covs = covariates),
                                   "Optimal"),
                      an_defw2_w = c(get_coef_cov(dataset$defw2, bw_mult = 2, covs = covariates),
                                     get_se_bc_cov(dataset$defw2, bw_mult = 2, covs = covariates),
                                     get_se_rob_cov(dataset$defw2, bw_mult = 2, covs = covariates),
                                     mean_wealth_pols(dataset$defw2),
                                     mean_wealth_nonpols(dataset$defw2),
                                     n_pols_cov(dataset$defw2, covs = covariates),
                                     n_nonpols_cov(dataset$defw2, covs = covariates),
                                     "2 x Optimal"))

notitie <- "Table showing Bias-corrected and Robust standard errors clustered at the Birthplace-level. Panel A shows univariate regressions under the optimal MSE bandwidth, and twice the optimal bandwidth. In panel B, selected covariates are added, in particular, covariates that seemed to be unbalanced at the 2\\\\% cutoff. In particular, the regression controls for lifespan, times participated in election, birthplace population, birthplace characteristics, age at election, and socialist recommendations. In addition, I control for politicians' lifespan. *: p < 0.10, **: p < 0.05, ***: p < 0.01."
knitr::opts_current$set(label = "mainresults")
datasummary_df(bind_rows(panel_a, panel_b) %>%
                 rename(` ` = names, 
                        "(1)" = an_defw,
                        "(2)" = an_defw_w,
                        "(3)" = an_defw2,
                        "(4)"  = an_defw2_w), 
               out = "kableExtra",
               output = "latex",
               title = "Main RD Estimates") %>%
  kableExtra::add_header_above(c(" " = 1, "Log(Wealth)" = 2, "Ihs(Wealth)" = 2)) %>%
  kableExtra::group_rows("Panel A: Baseline Estimates", 1, 8)  %>%
  kableExtra::group_rows("Panel B: Estimates With Selected Covariates", 9, 16) %>%
  kableExtra::kable_styling(latex_options = c("hold_position"), full_width = F, font_size = 10) %>%
  kableExtra::footnote(general = notitie, footnote_as_chunk = T, threeparttable = T, escape = F)  %>%
  kableExtra::save_kable("./Tables/rdd_mainresults.tex")



# Regression figure: placebo tests with false cutoff point - Make a figure
fig_data <- data.frame(cutoff = seq(from = -0.15, to = 0.15, by = 0.01), 
                       coef = vector(length = 31), 
                       lb = vector(length = 31), 
                       ub = vector(length = 31))

for(i in 1:length(seq(from = -0.15, to = 0.15, by = 0.01))){
  regression <- rdrobust(y = dataset$defw, x = dataset$margin, c = fig_data[['cutoff']][i])
  fig_data[['lb']][i] <- regression[['ci']][3,][1]
  fig_data[['ub']][i] <- regression[['ci']][3,][2]
  fig_data[['coef']][i] <- regression[['coef']][1]
}

good <- subset(fig_data, cutoff == 0)

placebo <- fig_data %>%
  ggplot(aes(x = cutoff, y = coef)) + geom_point(color = 'blue') + 
  theme_bw() +
  xlab("Cut-off point") + ylab("RD Estimate") +
  geom_errorbar(aes(x = cutoff, ymin = lb, ymax = ub), size = 0.2, color = 'black', width=0.003) +
  geom_point(data = good, color = "red", size = 2) +
  geom_text(data = good, label = "Actual Estimate", vjust =c(-5), hjust = c(-0.1)) +
  geom_segment(aes(x = 0.025, y = 3.3, xend = 0.005, yend = 2.3), arrow = arrow(length = unit(0.2, "cm")))


ggplot2::ggsave("./Tables/placebo_test.pdf", placebo, width = 10, height = 5)

# Create a bandwidth robuustness graph
fig_data <- data.frame(bw_mult = seq(from = 0.3, to = 5, by = 0.1), 
                       coef = vector(length = 48), 
                       cil = vector(length = 48), 
                       ciu = vector(length = 48))

for(i in 1:length(seq(from = 0.3, to = 5, by = 0.1))){
  
  fig_data[['coef']][i] <- get_coef_and_ci(dataset$defw, bw_mult = fig_data[['bw_mult']][i])$coef
  fig_data[['cil']][i] <- get_coef_and_ci(dataset$defw, bw_mult = fig_data[['bw_mult']][i])$cil
  fig_data[['ciu']][i] <- get_coef_and_ci(dataset$defw, bw_mult = fig_data[['bw_mult']][i])$ciu
  }


bandwidthje <- fig_data %>%
  ggplot(aes(x = bw_mult, y = coef)) + geom_line(color ='blue') +
  geom_ribbon(aes(x = bw_mult, 
                    ymin = cil, 
                    ymax = ciu), 
              alpha = 0.2,
              size = 0.2, 
              color = 'black') +
  theme_bw() +
  scale_x_continuous(breaks = scales::extended_breaks(n=15)) +
  xlab("Optimal Bandwidth Multiplier") + ylab("Coefficient Estimate (95% CI)")

ggplot2::ggsave("./Tables/bandwidth_test.pdf", bandwidthje, width = 10, height = 5)


# Now create two plots, one with and one without covariate adjustment for log wealth
# And combine them in one plot

step1 <- rdplot(y = dataset$defw, x = dataset$margin, covs = covariates, col.lines = 'red', ci = 90)
step2 <- step1$rdplot
step2$layers <- step2$layers[c(1:4)]
data_for_errorbar <- step2$layers[[4]]$data
step2$layers <- step2$layers[c(1:3)]
p1 <- step2 + xlim(-0.2, 0.2)  +  
  geom_vline(aes(xintercept = 0), lty = 2) +
  geom_errorbar(data= data_for_errorbar, aes(x = rdplot_mean_bin, 
                                             ymin = rdplot_cil_bin,
                                             ymax = rdplot_cir_bin), 
                color = 'dark grey', 
                width = 0.004) +
  ylab("Log(Wealth)") + xlab("Margin") + ggtitle(" ") 

step1 <- rdplot(y = dataset$defw2, x = dataset$margin, covs = covariates, col.lines = 'red', ci = 90)
step2 <- step1$rdplot
step2$layers <- step2$layers[c(1:4)]
data_for_errorbar <- step2$layers[[4]]$data
step2$layers <- step2$layers[c(1:3)]
p2 <- step2 + xlim(-0.2, 0.2) + ylim(2,16) +  
  geom_vline(aes(xintercept = 0), lty = 2) +
  geom_errorbar(data= data_for_errorbar, aes(x = rdplot_mean_bin, 
                                             ymin = rdplot_cil_bin,
                                             ymax = rdplot_cir_bin), 
                color = 'dark grey', 
                width = 0.004) +
  ylab("Ihs(Wealth)") + xlab("Margin") + ggtitle(" ") 

plot <- cowplot::plot_grid(p1, p2, nrow = 1)
cowplot::save_plot("./Tables/RDD_Plot.pdf", plot, base_width = 10, base_height = 4)


## Mechanism 1: electoral competition (before/after expansion)
make_covariates <- function(dataset){
  cbind(dataset$yoe, 
        dataset$howmany_before_alg,
        log(1+dataset$birthplace_pop_1859), 
        dataset$birthplace_agri, 
        dataset$birthplace_indus, 
        dataset$age_at_election, 
        dataset$yod, 
        dataset$rec_soc,
        dataset$lifespan,
        dataset$turnout)
  
}

fig_data <- data.frame(cutoff =  seq(from = dmy("01-01-1880"), to = dmy("01-01-1900"), by = "6 months"),
           coef_after = vector(length = 41),
           coef_before = vector(length = 41),
           se_after = vector(length = 41),
           se_before = vector(length = 41))
## calculate difference in rents by cutoff point 
j <- 1
for(i in seq(from = dmy("01-01-1880"), to = dmy("01-01-1900"), by = "6 months")){
  
  dataset2 <- dataset %>%
    filter(lubridate::dmy(Verkiezingdatum) > i)
  
  covs <- NULL #make_covariates(dataset2)
  regression_output <- rdrobust(y=dataset2[['defw']], x = dataset2[['margin']], covs = covs)
  
  coef_after <- regression_output$coef[1]
  se_after <- regression_output$se[2]
  
  dataset3 <- dataset %>%
    filter(lubridate::dmy(Verkiezingdatum) < i | politician_dummy == 0)
  
  #covs <- make_covariates(dataset3)
  regression_output <- rdrobust(y=dataset3[['defw']], x = dataset3[['margin']], covs = covs)
  
  coef_before <- regression_output$coef[1]
  se_before <- regression_output$se[2]
  
  fig_data[['coef_after']][j] <- coef_after
  fig_data[['coef_before']][j] <- coef_before
  fig_data[['se_after']][j] <- se_after
  fig_data[['se_before']][j] <- se_before
  
  j <- j+1
  
}

fig_data <- fig_data %>%
  mutate(diff = coef_after - coef_before, 
         var_diff = se_after^2 + se_before^2, 
         z_val = pnorm(diff, mean = 0, sd = sqrt(var_diff)),
         cil = diff - 1.65*sqrt(var_diff),
         ciu = diff + 1.65*sqrt(var_diff))

electoral_comp <- fig_data %>%
  ggplot(aes(x = cutoff, y = diff)) + 
  geom_errorbar(aes(x = cutoff, ymin = cil, ymax = ciu), width = 50, color = 'grey') +
  geom_point(color='blue') +
  theme_bw() +
  geom_vline(xintercept = dmy("01-01-1887"), lty=2) +
  geom_vline(xintercept = dmy("01-01-1896"), lty=2) + 
  ylab("Difference in Rents Estimate After - Before") + xlab("Cutoff Election Date")

ggsave("./Tables/electoral_competition.pdf", electoral_comp, width = 10, height = 5)

## 2. Exploit differences in turnout

## Turnout quantile graph

fig_data <- data.frame(quantile = seq(from = 0.4, to = 0.7, by = 0.01),
                       coef_higher = vector(length = 31),
                       coef_lower = vector(length = 31),
                       se_after = vector(length = 31),
                       se_before = vector(length = 31))

j <- 1

for(i in seq(from = 0.4, to = 0.7, by = 0.01)){
  dataset2 <- dataset %>%
    filter(!is.na(defw)) %>%
    mutate(effective_turnout = if_else(turnout/amount_electors > 1, 1, turnout/amount_electors)) %>%
    filter(effective_turnout > quantile(effective_turnout, i, na.rm = T))
  
  covs <- make_covariates(dataset2)
  regression_output <- rdrobust(y=dataset2[['defw']], x = dataset2[['margin']], covs = covs)
  
  coef_after <- regression_output$coef[1]
  se_after <- regression_output$se[2]
  
 
  dataset3 <- dataset %>%
    mutate(effective_turnout = if_else(turnout/amount_electors > 1, 1, turnout/amount_electors)) %>%
    filter(effective_turnout < quantile(effective_turnout, i, na.rm = T))
  
  covs <- make_covariates(dataset3)
  regression_output <- rdrobust(y=dataset3[['defw']], x = dataset3[['margin']], covs = covs)
  
  coef_before <- regression_output$coef[1]
  se_before <- regression_output$se[2]
  
  fig_data[['coef_higher']][j] <- coef_after
  fig_data[['coef_lower']][j] <- coef_before
  fig_data[['se_after']][j] <- se_after
  fig_data[['se_before']][j] <- se_before
  
  j <- j+1
  
}

fig_data <- fig_data %>%
  mutate(diff = coef_higher - coef_lower, 
         var_diff = se_after^2 + se_before^2, 
         z_val = pnorm(diff, mean = 0, sd = sqrt(var_diff)),
         cil = diff - 1.65*sqrt(var_diff),
         ciu = diff + 1.65*sqrt(var_diff))

turnout_competition <- fig_data %>%
  ggplot(aes(x = quantile, y = diff)) + 
  geom_errorbar(aes(x = quantile, ymin = cil, ymax = ciu), width = 0.005, color = 'grey') +
  geom_point(color='blue') +
  theme_bw() + ylab("Difference in Rents Estimate High - Low Quantile") + xlab("Turnout Quantile")

ggsave("./Tables/turnout_competition.pdf", turnout_competition, width = 10, height = 5)

## second attempt
j <- 1

for(i in seq(from = 0.4, to = 0.7, by = 0.01)){
  dataset2 <- dataset %>%
    filter(!is.na(defw)) %>%
    mutate(effective_turnout = turnout/district_pop_1889) %>%
    filter(effective_turnout > quantile(effective_turnout, i, na.rm = T))
  
  covs <- make_covariates(dataset2)
  regression_output <- rdrobust(y=dataset2[['defw']], x = dataset2[['margin']], covs = covs)
  
  coef_after <- regression_output$coef[1]
  se_after <- regression_output$se[2]
  
  
  dataset3 <- dataset %>%
    mutate(effective_turnout = if_else(turnout/amount_electors > 1, 1, turnout/amount_electors)) %>%
    filter(effective_turnout < quantile(effective_turnout, i, na.rm = T))
  
  covs <- make_covariates(dataset3)
  regression_output <- rdrobust(y=dataset3[['defw']], x = dataset3[['margin']], covs = covs)
  
  coef_before <- regression_output$coef[1]
  se_before <- regression_output$se[2]
  
  fig_data[['coef_higher']][j] <- coef_after
  fig_data[['coef_lower']][j] <- coef_before
  fig_data[['se_after']][j] <- se_after
  fig_data[['se_before']][j] <- se_before
  
  j <- j+1
  
}

fig_data <- fig_data %>%
  mutate(diff = coef_higher - coef_lower, 
         var_diff = se_after^2 + se_before^2, 
         z_val = pnorm(diff, mean = 0, sd = sqrt(var_diff)),
         cil = diff - 1.65*sqrt(var_diff),
         ciu = diff + 1.65*sqrt(var_diff))

turnout_competition <- fig_data %>%
  ggplot(aes(x = quantile, y = diff)) + 
  geom_errorbar(aes(x = quantile, ymin = cil, ymax = ciu), width = 0.005, color = 'grey') +
  geom_point(color='blue') +
  theme_bw() + ylab("Difference in Rents Estimate High - Low Quantile") + xlab("Turnout Quantile")

ggsave("./Tables/turnout_competition.pdf", turnout_competition, width = 10, height = 5)
## Mechanism 2: Party organization (before/after party establishment)
### Make a table for this - BETWEEN party with and without covariates
make_covariates <- function(dataset){
  cbind(
        dataset$age_at_election, 
        dataset$rec_soc,
        dataset$rec_ar,
        dataset$rec_kath,
        dataset$rec_lib,
        dataset$lifespan)
  
}

prot <- dataset %>% filter(party_category == "protestant" | politician_dummy == 0)
covs_prot <- make_covariates(prot)

cath <- dataset %>% filter(party_category == "catholic" | politician_dummy == 0)
covs_cath <- make_covariates(cath)

lib <- dataset %>% filter(party_category == "liberal" | politician_dummy == 0)
covs_lib <- make_covariates(lib)

## Make a table on the basis of the above ^ 
panel_a <- data.frame(names = c("Coefficient",
                     "SE (BC)",
                     "SE (Rob.)",
                     "N Treatment",
                     "Covariates"),
           prot_one = c(get_stats_for_partytable(prot, 'defw')[[1]],
                        get_stats_for_partytable(prot, 'defw')[[2]],
                        get_stats_for_partytable(prot, 'defw')[[3]],
                        get_stats_for_partytable(prot, 'defw')[[4]],
                        "No"),
           prot_two = c(get_stats_for_partytable(prot, 'defw', covs = covs_prot)[[1]],
                        get_stats_for_partytable(prot, 'defw', covs = covs_prot)[[2]],
                        get_stats_for_partytable(prot, 'defw', covs = covs_prot)[[3]],
                        get_stats_for_partytable(prot, 'defw', covs = covs_prot)[[4]],
                        "Yes"),
           cath_one = c(get_stats_for_partytable(cath, 'defw')[[1]],
                        get_stats_for_partytable(cath, 'defw')[[2]],
                        get_stats_for_partytable(cath, 'defw')[[3]],
                        get_stats_for_partytable(cath, 'defw')[[4]],
                        "No"),
           cath_two = c(get_stats_for_partytable(cath, 'defw', covs = covs_cath)[[1]],
                        get_stats_for_partytable(cath, 'defw', covs = covs_cath)[[2]],
                        get_stats_for_partytable(cath, 'defw', covs = covs_cath)[[3]],
                        get_stats_for_partytable(cath, 'defw', covs = covs_cath)[[4]],
                        "Yes"),
           lib_one = c(get_stats_for_partytable(lib, 'defw')[[1]],
                        get_stats_for_partytable(lib, 'defw')[[2]],
                        get_stats_for_partytable(lib, 'defw')[[3]],
                        get_stats_for_partytable(lib, 'defw')[[4]],
                        "No"),
           lib_two = c(get_stats_for_partytable(lib, 'defw', covs = covs_lib)[[1]],
                       get_stats_for_partytable(lib, 'defw', covs = covs_lib)[[2]],
                       get_stats_for_partytable(lib, 'defw', covs = covs_lib)[[3]],
                       get_stats_for_partytable(lib, 'defw', covs = covs_lib)[[4]],
                       "Yes"))

panel_b <- data.frame(names = c("Coefficient",
                                "SE (BC)",
                                "SE (Rob.)",
                                "N Treatment",
                                "Covariates"),
                      prot_one = c(get_stats_for_partytable(prot, 'defw2')[[1]],
                                   get_stats_for_partytable(prot, 'defw2')[[2]],
                                   get_stats_for_partytable(prot, 'defw2')[[3]],
                                   get_stats_for_partytable(prot, 'defw2')[[4]],
                                   "No"),
                      prot_two = c(get_stats_for_partytable(prot, 'defw2', covs = covs_prot)[[1]],
                                   get_stats_for_partytable(prot, 'defw2', covs = covs_prot)[[2]],
                                   get_stats_for_partytable(prot, 'defw2', covs = covs_prot)[[3]],
                                   get_stats_for_partytable(prot, 'defw2', covs = covs_prot)[[4]],
                                   "Yes"),
                      cath_one = c(get_stats_for_partytable(cath, 'defw2')[[1]],
                                   get_stats_for_partytable(cath, 'defw2')[[2]],
                                   get_stats_for_partytable(cath, 'defw2')[[3]],
                                   get_stats_for_partytable(cath, 'defw2')[[4]],
                                   "No"),
                      cath_two = c(get_stats_for_partytable(cath, 'defw2', covs = covs_cath)[[1]],
                                   get_stats_for_partytable(cath, 'defw2', covs = covs_cath)[[2]],
                                   get_stats_for_partytable(cath, 'defw2', covs = covs_cath)[[3]],
                                   get_stats_for_partytable(cath, 'defw2', covs = covs_cath)[[4]],
                                   "Yes"),
                      lib_one = c(get_stats_for_partytable(lib, 'defw2')[[1]],
                                  get_stats_for_partytable(lib, 'defw2')[[2]],
                                  get_stats_for_partytable(lib, 'defw2')[[3]],
                                  get_stats_for_partytable(lib, 'defw2')[[4]],
                                  "No"),
                      lib_two = c(get_stats_for_partytable(lib, 'defw2', covs = covs_lib)[[1]],
                                  get_stats_for_partytable(lib, 'defw2', covs = covs_lib)[[2]],
                                  get_stats_for_partytable(lib, 'defw2', covs = covs_lib)[[3]],
                                  get_stats_for_partytable(lib, 'defw2', covs = covs_lib)[[4]],
                                  "Yes"))

notitie <- "Table showing Bias-corrected and Robust standard errors clustered at the Birthplace-level. Panel A shows RD estimates of Log(Wealth) under the optimal MSE bandwidth per party. In panel B, I  show RD estimates of Ihs(Wealth) per party. I control for age, lifespan and newspaper recommendations. *: p < 0.10, **: p < 0.05, ***: p < 0.01."
knitr::opts_current$set(label = "results_per_party")
datasummary_df(bind_rows(panel_a, panel_b) %>%
                 rename(` ` = names, 
                        "(1)" = prot_one,
                        "(2)" = prot_two,
                        "(3)" = cath_one,
                        "(4)"  = cath_two,
                        "(5)" = lib_one,
                        "(6)" = lib_two), 
               out = "kableExtra",
               output = "latex",
               title = "RD Estimates by Party") %>%
  kableExtra::add_header_above(c(" " = 1, "Protestants" = 2, "Catholics" = 2, "Liberals" = 2)) %>%
  kableExtra::group_rows("Panel A: Log(Wealth)", 1, 5)  %>%
  kableExtra::group_rows("Panel B: Ihs(Wealth)", 6, 10) %>%
  kableExtra::kable_styling(latex_options = c("hold_position"), font_size=10) %>%
  kableExtra::footnote(general = notitie, footnote_as_chunk = T, threeparttable = T, escape = F)  %>%
  kableExtra::save_kable("./Tables/rdd_resultsperparty.tex")

# plots
p1 <- rdplot(prot$defw, prot$margin, 
       ci=90, 
       x.lim = c(-0.2, 0.2), 
       title = "Protestants", 
       y.label = "Log(Wealth)",
       x.label = "Margin")
p1 <- p1$rdplot

p2 <- rdplot(y=cath$defw, x= cath$margin, 
       ci = 90,
       x.lim = c(-0.2, 0.2),
       title = "Catholics",
       y.label = "Log(Wealth)",
       x.label = "Margin")
p2 <- p2$rdplot

p3 <- rdplot(lib$defw, lib$margin, 
       ci=90, 
       x.lim = c(-0.2, 0.2),
       title = "Liberals", 
       y.label = "Log(Wealth)",
       x.label = "Margin")
p3 <- p3$rdplot

plotteke <- cowplot::plot_grid(p1, p2, p3, ncol = 3)
cowplot::save_plot("./Tables/rdplot_per_party.pdf", 
                   plotteke, 
                   base_height = 4.8, 
                   base_width =12)

## Second thing: WITHIN party - make use of timing of party formation
### Set up a bootstrap to compute the distribution

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
           inparty1 = c(get_stats_withinparty(in_party, dv = 'defw')[[1]],
                        get_stats_withinparty(in_party, dv = 'defw')[[2]],
                        get_stats_withinparty(in_party, dv = 'defw')[[3]],
                        get_stats_withinparty(in_party, dv = 'defw')[[4]],
                        get_stats_withinparty(in_party, dv = 'defw')[[5]],
                        "No"),
           outparty1 = c(get_stats_withinparty(out_party, dv = 'defw')[[1]],
                         get_stats_withinparty(out_party, dv = 'defw')[[2]],
                         get_stats_withinparty(out_party, dv = 'defw')[[3]],
                         get_stats_withinparty(out_party, dv = 'defw')[[4]],
                         get_stats_withinparty(out_party, dv = 'defw')[[5]],
                         "No"),
           diff1 = c(as.numeric(get_stats_withinparty(in_party, dv = 'defw')[[1]]) - as.numeric(get_stats_withinparty(out_party, dv = 'defw')[[1]]),
                    round(calc_pv(-0.143), 3), 
                    " ",
                    " ", 
                    " ",
                    " "),
           inparty2 = c(get_stats_withinparty(in_party, dv = 'defw', covs = covs_ip)[[1]],
                        get_stats_withinparty(in_party, dv = 'defw', covs = covs_ip)[[2]],
                        get_stats_withinparty(in_party, dv = 'defw', covs = covs_ip)[[3]],
                        get_stats_withinparty(in_party, dv = 'defw', covs = covs_ip)[[4]],
                        get_stats_withinparty(in_party, dv = 'defw', covs = covs_ip)[[5]],
                        "Yes"),
           outparty2 = c(get_stats_withinparty(out_party, dv = 'defw', covs = covs_op)[[1]],
                         get_stats_withinparty(out_party, dv = 'defw', covs = covs_op)[[2]],
                         get_stats_withinparty(out_party, dv = 'defw', covs = covs_op)[[3]],
                         get_stats_withinparty(out_party, dv = 'defw', covs = covs_op)[[4]],
                         get_stats_withinparty(out_party, dv = 'defw', covs = covs_op)[[5]],
                         "Yes"),
           diff2 = c(as.numeric(get_stats_withinparty(in_party, dv = 'defw', covs = covs_ip)[[1]]) - as.numeric(get_stats_withinparty(out_party, dv = 'defw', covs = covs_op)[[1]]),
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
                      inparty1 = c(get_stats_withinparty(in_party, dv = 'defw')[[1]],
                                   get_stats_withinparty(in_party, dv = 'defw')[[2]],
                                   get_stats_withinparty(in_party, dv = 'defw')[[3]],
                                   get_stats_withinparty(in_party, dv = 'defw')[[4]],
                                   get_stats_withinparty(in_party, dv = 'defw')[[5]],
                                   "No"),
                      outparty1 = c(get_stats_withinparty(out_party, dv = 'defw')[[1]],
                                    get_stats_withinparty(out_party, dv = 'defw')[[2]],
                                    get_stats_withinparty(out_party, dv = 'defw')[[3]],
                                    get_stats_withinparty(out_party, dv = 'defw')[[4]],
                                    get_stats_withinparty(out_party, dv = 'defw')[[5]],
                                    "No"),
                      diff1 = c(as.numeric(get_stats_withinparty(in_party, dv = 'defw')[[1]]) - as.numeric(get_stats_withinparty(out_party, dv = 'defw')[[1]]),
                               round(calc_pv(-1.07), 3), 
                               " ",
                               " ", 
                               " ",
                               " "),
                      inparty2 = c(get_stats_withinparty(in_party, dv = 'defw', covs = covs_ip)[[1]],
                                   get_stats_withinparty(in_party, dv = 'defw', covs = covs_ip)[[2]],
                                   get_stats_withinparty(in_party, dv = 'defw', covs = covs_ip)[[3]],
                                   get_stats_withinparty(in_party, dv = 'defw', covs = covs_ip)[[4]],
                                   get_stats_withinparty(in_party, dv = 'defw', covs = covs_ip)[[5]],
                                   "Yes"),
                      outparty2 = c(get_stats_withinparty(out_party, dv = 'defw', covs = covs_op)[[1]],
                                    get_stats_withinparty(out_party, dv = 'defw', covs = covs_op)[[2]],
                                    get_stats_withinparty(out_party, dv = 'defw', covs = covs_op)[[3]],
                                    get_stats_withinparty(out_party, dv = 'defw', covs = covs_op)[[4]],
                                    get_stats_withinparty(out_party, dv = 'defw', covs = covs_op)[[5]],
                                    "Yes"),
                      diff2 = c(as.numeric(get_stats_withinparty(in_party, dv = 'defw', covs=covs_ip)[[1]]) - as.numeric(get_stats_withinparty(out_party, dv = 'defw', covs = covs_op)[[1]]),
                               round(calc_pv(-2.796), 3), 
                               " ",
                               " ", 
                               " ",
                               " "))

# Now, create the table
notitie <- "The table shows RD estimates using the MSE-optimal bandwidth \\\\citep{cattaneo2019practical}. The Dependent Variable is Log(Personal Wealth). I report bias-corrected and robust standard errors. Panel A uses the entire control group, whereas panel B opts for control-observations from the pre-and post-party periods respectively. Columns (1) and (2) contain estimates with no covariates, and columns (3) and (4) control for potential imbalances in lifespan, age and newspaper recommendations. *: p < 0.1, **: p < 0.05, ***: p < 0.01."
knitr::opts_current$set(label = "results_within_party")
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
  kableExtra::save_kable("./Tables/in_out_party_effect.tex")

## Mechanism 3: Career Paths

make_covariates <- function(dataset){
  cbind(
    dataset$age_at_election, 
    dataset$rec_soc,
    dataset$rec_ar,
    dataset$rec_kath,
    dataset$rec_lib,
    dataset$lifespan)
  
}

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

## Business: 
business <- dataset %>%
  filter(prof_business == 1 | politician_dummy == 0)
nonbusiness <- dataset %>%
  filter(prof_business == 0 | politician_dummy == 0)
## Colonial:
colonial <- dataset %>%
  filter(prof_colonial == 1 | politician_dummy == 0)
noncolonial <- dataset %>%
  filter(prof_colonial == 0 | politician_dummy == 0)
## Politics:
politics <- dataset %>%
  filter(prof_politics == 1 | politician_dummy == 0) 
nonpolitics <- dataset %>%
  filter(prof_politics == 0 | politician_dummy == 0) 

tablitsa <- data.frame('names' = c(
  "Coefficient",
  "SE (BC)", 
  "SE (Rob.)",
  "N Treated", 
  "N Control"),
  col1 = c(get_stats_withinparty(colonial, 'defw')[[1]],
           get_stats_withinparty(colonial, 'defw')[[2]],
           get_stats_withinparty(colonial, 'defw')[[3]],
           get_stats_withinparty(colonial, 'defw')[[4]],
           get_stats_withinparty(colonial, 'defw')[[5]]),
  noncol1 = c(
    get_stats_withinparty(noncolonial, 'defw')[[1]],
    get_stats_withinparty(noncolonial, 'defw')[[2]],
    get_stats_withinparty(noncolonial, 'defw')[[3]],
    get_stats_withinparty(noncolonial, 'defw')[[4]],
    get_stats_withinparty(noncolonial, 'defw')[[5]]),
  bus1 = c(
    get_stats_withinparty(business, 'defw')[[1]],
    get_stats_withinparty(business, 'defw')[[2]],
    get_stats_withinparty(business, 'defw')[[3]],
    get_stats_withinparty(business, 'defw')[[4]],
    get_stats_withinparty(business, 'defw')[[5]]
  ),
  nonbus1 = c(
    get_stats_withinparty(nonbusiness, 'defw')[[1]],
    get_stats_withinparty(nonbusiness, 'defw')[[2]],
    get_stats_withinparty(nonbusiness, 'defw')[[3]],
    get_stats_withinparty(nonbusiness, 'defw')[[4]],
    get_stats_withinparty(nonbusiness, 'defw')[[5]]
  ),
  pol1 = c(
    get_stats_withinparty(politics, 'defw')[[1]],
    get_stats_withinparty(politics, 'defw')[[2]],
    get_stats_withinparty(politics, 'defw')[[3]],
    get_stats_withinparty(politics, 'defw')[[4]],
    get_stats_withinparty(politics, 'defw')[[5]]
  ),
  nonpol1 = c(
    get_stats_withinparty(nonpolitics, 'defw')[[1]],
    get_stats_withinparty(nonpolitics, 'defw')[[2]],
    get_stats_withinparty(nonpolitics, 'defw')[[3]],
    get_stats_withinparty(nonpolitics, 'defw')[[4]],
    get_stats_withinparty(nonpolitics, 'defw')[[5]]
  ))
           

tablitsa2 <- data.frame('names' = c(
  "Coefficient",
  "SE (BC)", 
  "SE (Rob.)",
  "N Treated", 
  "N Control"),
  col1 = c(get_stats_withinparty(colonial, 'defw', covs = make_covariates(colonial))[[1]],
           get_stats_withinparty(colonial, 'defw', covs = make_covariates(colonial))[[2]],
           get_stats_withinparty(colonial, 'defw', covs = make_covariates(colonial))[[3]],
           get_stats_withinparty(colonial, 'defw', covs = make_covariates(colonial))[[4]],
           get_stats_withinparty(colonial, 'defw', covs = make_covariates(colonial))[[5]]),
  noncol1 = c(
    get_stats_withinparty(noncolonial, 'defw', covs = make_covariates(noncolonial))[[1]],
    get_stats_withinparty(noncolonial, 'defw', covs = make_covariates(noncolonial))[[2]],
    get_stats_withinparty(noncolonial, 'defw', covs = make_covariates(noncolonial))[[3]],
    get_stats_withinparty(noncolonial, 'defw', covs = make_covariates(noncolonial))[[4]],
    get_stats_withinparty(noncolonial, 'defw', covs = make_covariates(noncolonial))[[5]]),
  bus1 = c(
    get_stats_withinparty(business, 'defw', covs = make_covariates(business))[[1]],
    get_stats_withinparty(business, 'defw', covs = make_covariates(business))[[2]],
    get_stats_withinparty(business, 'defw', covs = make_covariates(business))[[3]],
    get_stats_withinparty(business, 'defw', covs = make_covariates(business))[[4]],
    get_stats_withinparty(business, 'defw', covs = make_covariates(business))[[5]]
  ),
  nonbus1 = c(
    get_stats_withinparty(nonbusiness, 'defw', covs = make_covariates(nonbusiness))[[1]],
    get_stats_withinparty(nonbusiness, 'defw', covs = make_covariates(nonbusiness))[[2]],
    get_stats_withinparty(nonbusiness, 'defw', covs = make_covariates(nonbusiness))[[3]],
    get_stats_withinparty(nonbusiness, 'defw', covs = make_covariates(nonbusiness))[[4]],
    get_stats_withinparty(nonbusiness, 'defw', covs = make_covariates(nonbusiness))[[5]]
  ),
  pol1 = c(
    get_stats_withinparty(politics, 'defw', covs = make_covariates(politics))[[1]],
    get_stats_withinparty(politics, 'defw', covs = make_covariates(politics))[[2]],
    get_stats_withinparty(politics, 'defw', covs = make_covariates(politics))[[3]],
    get_stats_withinparty(politics, 'defw', covs = make_covariates(politics))[[4]],
    get_stats_withinparty(politics, 'defw', covs = make_covariates(politics))[[5]]
  ),
  nonpol1 = c(
    get_stats_withinparty(nonpolitics, 'defw', covs = make_covariates(nonpolitics))[[1]],
    get_stats_withinparty(nonpolitics, 'defw', covs = make_covariates(nonpolitics))[[2]],
    get_stats_withinparty(nonpolitics, 'defw', covs = make_covariates(nonpolitics))[[3]],
    get_stats_withinparty(nonpolitics, 'defw', covs = make_covariates(nonpolitics))[[4]],
    get_stats_withinparty(nonpolitics, 'defw', covs = make_covariates(nonpolitics))[[5]]
  ))
  
notitie <- "The table shows RD estimates using the MSE-optimal bandwidth \\\\citep{cattaneo2019practical}. The Dependent Variable is Log(Personal Wealth). I report bias-corrected and robust standard errors. Panel A uses no covariates, whereas Panel B controls for several possible imbalances. The first equation in the panel estimates the rents for the politicians who have taken a given career paths, and the second equation estimates the rents for those who did not. *: p < 0.1, **: p < 0.05, ***: p < 0.01."
knitr::opts_current$set(label = "results_careerpaths")
modelsummary::datasummary_df(bind_rows(tablitsa, tablitsa2)
                             %>% rename(" " = names,
                               "(1)"= col1,
                                        "(2)" = noncol1,
                                        "(3)" = bus1,
                                        "(4)" = nonbus1,
                                        "(5)" = pol1,
                                        "(6)" = nonpol1),
                             title = "Political Rents and Career Paths",
                             out = "kableExtra",
                             output = "latex") %>%
  kableExtra::add_header_above(c(" " = 1, rep(c("Yes", "No"), 3))) %>%
  kableExtra::add_header_above(c(" " = 1, "Colonial" =  2, "Business" = 2, "Politics" = 2)) %>%
  kableExtra::group_rows("Without Covariates", 1, 5)%>%
  kableExtra::group_rows("With Covariates", 6, 10) %>%
  kableExtra::kable_styling(latex_options = c("hold_position"), font_size=10) %>%
  kableExtra::footnote(general = notitie, footnote_as_chunk = T, threeparttable = T, escape = F)  %>%
  kableExtra::save_kable("./Tables/results_careerpaths.tex")
  

## Other way of testing politics with tenure
## Create dataset
fig_data <- data.frame('quantile' = seq(0.5, 0.8, by = 0.05),
                      'est_hightenure' = vector(length = 7),
                      'se_hightenure' = vector(length =7),
                       'est_lowtenure' = vector(length = 7),
                      'se_lowtenure' = vector(length = 7))

for(i in 1:7){
  sequence <- seq(0.5, 0.8, by = 0.05)
  
  hightenure <- dataset %>%
    filter(tenure > quantile(tenure, sequence[i], na.rm = TRUE) | politician_dummy == 0)
  lowtenure <- dataset %>%
    filter(tenure < quantile(tenure, sequence[i], na.rm = TRUE) | politician_dummy == 0)  
  
  high <- rdrobust(y=hightenure$defw2, x=hightenure$margin, covs = make_covariates(hightenure))
  low <- rdrobust(y=lowtenure$defw2, x = lowtenure$margin, covs = make_covariates(lowtenure))
  
  fig_data[['est_hightenure']][i] <- high$coef[1]
  fig_data[['est_lowtenure']][i] <- low$coef[1]
  fig_data[['se_hightenure']][i] <- high$se[1]
  fig_data[['se_lowtenure']][i] <- low$se[1]
  
}

figgetje <- fig_data %>%
  ggplot(aes(x = quantile)) + 
  geom_line(aes(y = est_hightenure, color = 'Above Cut-Off')) +
  theme_bw() + 
  geom_errorbar(aes(x = quantile - 0.0025, 
                    ymin = est_hightenure - 1.65*se_hightenure, 
                    ymax = est_hightenure + 1.65*se_hightenure), 
                width = 0.005,
                color = 'brown',
                lty = 1) + 
  geom_point(aes(y = est_hightenure), color = 'brown') +
  geom_line(aes(y = est_lowtenure, color = 'Below Cut-Off')) + 
  geom_errorbar(aes(x = quantile + 0.0025,
                    ymin = est_lowtenure - 1.65*se_lowtenure,
                    ymax = est_lowtenure + 1.65*se_hightenure),
                width = 0.005,
                color = 'blue', 
                lty = 3) + 
  geom_point(aes(y = est_lowtenure), color = 'blue') + 
  labs(color = "Tenure Status", x = "Quantile Cut-Off", y = "RD Estimate") +
  scale_color_manual(values=c('brown', 'blue'))

ggsave("./Tables/fig_tenure_politics.pdf", figgetje, width = 10, height = 4)

## Randstad / Non-Randstad
make_covariates <- function(dataset){
  cbind(dataset$yoe, 
        dataset$howmany_before_alg,
        log(1+dataset$birthplace_pop_1859), 
        dataset$birthplace_agri, 
        dataset$age_at_election, 
        dataset$yod, 
        dataset$rec_soc,
        dataset$lifespan)
}


randstad <- dataset %>% 
  filter(distance_bp_hag < 65)

nonrandstad <- dataset %>%
  filter(distance_bp_hag > 65)

covs_randstad <- make_covariates(randstad)
rdrobust(randstad$defw, randstad$margin, covs = covs_randstad) %>% summary()
covs_nonrandstad <- make_covariates(nonrandstad)
rdrobust(nonrandstad$defw, nonrandstad$margin, covs = covs_nonrandstad) %>% summary()

tabelletje <- data.frame(
  names = c("Coefficient", 
            "SE (BC)",
            "SE (Rob.)",
            "N Treated",
            "N Control",
            "Covariates"),
  randstad = c(
    get_stats_withinparty(randstad, 'defw')[[1]],
    get_stats_withinparty(randstad, 'defw')[[2]],
    get_stats_withinparty(randstad, 'defw')[[3]],
    get_stats_withinparty(randstad, 'defw')[[4]],
    get_stats_withinparty(randstad, 'defw')[[5]],
    "No"),
  nonrandstad = c(
    get_stats_withinparty(nonrandstad, 'defw')[[1]],
    get_stats_withinparty(nonrandstad, 'defw')[[2]],
    get_stats_withinparty(nonrandstad, 'defw')[[3]],
    get_stats_withinparty(nonrandstad, 'defw')[[4]],
    get_stats_withinparty(nonrandstad, 'defw')[[5]],
    "No"),
    randstad2 = c(
      get_stats_withinparty(randstad, 'defw', covs = make_covariates(randstad))[[1]],
      get_stats_withinparty(randstad, 'defw', covs = make_covariates(randstad))[[2]],
      get_stats_withinparty(randstad, 'defw', covs = make_covariates(randstad))[[3]],
      get_stats_withinparty(randstad, 'defw', covs = make_covariates(randstad))[[4]],
      get_stats_withinparty(randstad, 'defw', covs = make_covariates(randstad))[[5]],
      "Yes"
  ),
  nonrandstad2 = c(
  get_stats_withinparty(nonrandstad, 'defw', covs = make_covariates(nonrandstad))[[1]],
  get_stats_withinparty(nonrandstad, 'defw', covs = make_covariates(nonrandstad))[[2]],
  get_stats_withinparty(nonrandstad, 'defw', covs = make_covariates(nonrandstad))[[3]],
  get_stats_withinparty(nonrandstad, 'defw', covs = make_covariates(nonrandstad))[[4]],
  get_stats_withinparty(nonrandstad, 'defw', covs = make_covariates(nonrandstad))[[5]],
  "Yes"))

# make a table out of this
notitie <- "The table shows RD estimates using the MSE-optimal bandwidth \\\\citep{cattaneo2019practical}. The Dependent Variable is Log(Personal Wealth). I report bias-corrected and robust standard errors. The table shows estimates for political rents in- and outside of the Randstad area, without (1 and 2) and with (3 and 4) selected covariates. *: p < 0.1, **: p < 0.05, ***: p < 0.01."
knitr::opts_current$set(label = "results_randstad")
modelsummary::datasummary_df(tabelletje %>% rename(" " = names,
                                        "(1)"= randstad,
                                        "(2)" = nonrandstad,
                                        "(3)" = randstad2,
                                        "(4)" = nonrandstad2),
                             title = "Political Rents and Geography",
                             out = "kableExtra",
                             output = "latex") %>%
  kableExtra::add_header_above(c(" " = 1, rep(c("Randstad", "Non-Randstad"), 2))) %>%
  kableExtra::kable_styling(latex_options = c("hold_position"), font_size=10) %>%
  kableExtra::footnote(general = notitie, footnote_as_chunk = T, threeparttable = T, escape = F)  %>%
  kableExtra::save_kable("./Tables/results_randstad.tex")


## figure distance to the hague and rents
fig_data <- data.frame("cutoff" = seq(10, 120, by = 5),
                       "rents_within" = vector(length = 23),
                       "se_within" = vector(length = 23),
                       "rents_outside" = vector(length = 23),
                       "se_outside" = vector(length = 23))

for(i in 1:23){
  
  sequence <- seq(10, 120, by = 5)
  
  randstad <- dataset %>% 
    filter(distance_bp_hag < sequence[i])
  
  nonrandstad <- dataset %>% 
    filter(distance_bp_hag > sequence[i])
  
  est1 <- rdrobust(randstad[['defw']], randstad[['margin']], covs = make_covariates(randstad))
  est2 <- rdrobust(nonrandstad[['defw']], nonrandstad[['margin']], covs = make_covariates(nonrandstad))
  
  fig_data[['rents_within']][i] <- est1$coef[1]
  fig_data[['se_within']][i] <- est1$se[2]
  
  fig_data[['rents_outside']][i] <- est2$coef[1]
  fig_data[['se_outside']][i] <- est2$se[2]
  
  
  
}

plotinho <- fig_data %>%
  ggplot(aes(x = cutoff)) + 
  theme_bw() + 
  geom_line(aes(y = rents_within, color = "Within Distance")) +
  geom_line(aes(y = rents_outside, color = "Outside of Distance"), lty = 2) +
  geom_errorbar(aes(x = cutoff + 0.5, ymin = rents_within - 1.65*se_within, ymax = rents_within + 1.65*se_within), 
                color = 'brown', width = 1) +
  geom_errorbar(aes(x = cutoff - 0.5, ymin = rents_outside - 1.65*se_outside, ymax = rents_outside + 1.65*se_outside), 
                color = 'blue', width = 1, lty = 2) +
  labs(color = "Distance to the Hague", x = "Cut-Off (km)", y = "RD Estimate") +
  scale_color_manual(values=c('blue', 'brown'))

ggsave("./Tables/randstad_distance.pdf", plotinho, width = 10, height= 4)



### Robustness check - main analysis - compute the opt. bandwidth below and above the cutoff point and estimate regression
# Panel A: Without Covariates
panel_a <- data.frame(names = c("Coefficient", 
                                "SE (BC)",
                                "SE (Rob.)",
                                "Mean DV Politicians (1%)",
                                "Mean DV Non-Politicians (1%)",
                                "N (Politicians)",
                                "N (Non-Politicians)",
                                "Bandwidth"),
                      an_defw=c(get_coef2(dataset$defw),
                                get_se_bc2(dataset$defw),
                                get_se_rob2(dataset$defw),
                                mean_wealth_pols(dataset$defw),
                                mean_wealth_nonpols(dataset$defw),
                                n_pols(dataset$defw),
                                n_nonpols(dataset$defw),
                                "Optimal"),
                      an_defw_w = c(get_coef_w2(dataset$defw),
                                    get_se_bc_w2(dataset$defw),
                                    get_se_rob_w2(dataset$defw),
                                    mean_wealth_pols(dataset$defw),
                                    mean_wealth_nonpols(dataset$defw),
                                    n_pols(dataset$defw),
                                    n_nonpols(dataset$defw),
                                    "2 x Optimal"),
                      an_defw2 = c(get_coef2(dataset$defw2),
                                   get_se_bc2(dataset$defw2),
                                   get_se_rob2(dataset$defw2),
                                   mean_wealth_pols(dataset$defw2),
                                   mean_wealth_nonpols(dataset$defw2),
                                   n_pols(dataset$defw2),
                                   n_nonpols(dataset$defw2),
                                   "Optimal"),
                      an_defw2_w = c(get_coef_w2(dataset$defw2),
                                     get_se_bc_w2(dataset$defw2),
                                     get_se_rob_w2(dataset$defw2),
                                     mean_wealth_pols(dataset$defw2),
                                     mean_wealth_nonpols(dataset$defw2),
                                     n_pols(dataset$defw2),
                                     n_nonpols(dataset$defw2),
                                     "2 x Optimal")
)


# Panel B: With Covariates which are significant in Panel A at 0.05 cutoff point
# yoe, howmany_before_alg, log(1+birthplace_pop_1859), birthplace_agri, 
# birthplace_indus, age_at_election, yod, rec_soc
# rdrobust(y=dataset$defw, x = dataset$margin, 

covariates <- cbind(dataset$yoe, 
                    dataset$howmany_before_alg,
                    log(1+dataset$birthplace_pop_1859), 
                    dataset$birthplace_agri, 
                    dataset$birthplace_indus, 
                    dataset$age_at_election, 
                    dataset$yod, 
                    dataset$rec_soc,
                    dataset$lifespan)


panel_b <- data.frame(names = c("Coefficient", 
                                "SE (BC)",
                                "SE (Rob.)",
                                "Mean DV Politicians (1%)",
                                "Mean DV Non-Politicians (1%)",
                                "N (Politicians)",
                                "N (Non-Politicians)",
                                "Bandwidth"),
                      an_defw=c(get_coef_cov2(dataset$defw, covs = covariates),
                                get_se_bc_cov2(dataset$defw, covs = covariates),
                                get_se_rob_cov2(dataset$defw, covs = covariates),
                                mean_wealth_pols(dataset$defw),
                                mean_wealth_nonpols(dataset$defw),
                                n_pols_cov(dataset$defw, covs = covariates),
                                n_nonpols_cov(dataset$defw, covs = covariates),
                                "Optimal"),
                      an_defw_w = c(get_coef_cov2(dataset$defw, bw_mult = 2, covs = covariates),
                                    get_se_bc_cov2(dataset$defw, bw_mult = 2, covs = covariates),
                                    get_se_rob_cov2(dataset$defw, bw_mult = 2, covs = covariates),
                                    mean_wealth_pols(dataset$defw),
                                    mean_wealth_nonpols(dataset$defw),
                                    n_pols_cov(dataset$defw, covs = covariates),
                                    n_nonpols_cov(dataset$defw, covs = covariates),
                                    "2 x Optimal"),
                      an_defw2 = c(get_coef_cov2(dataset$defw2, covs = covariates),
                                   get_se_bc_cov2(dataset$defw2, covs = covariates),
                                   get_se_rob_cov2(dataset$defw2, covs = covariates),
                                   mean_wealth_pols(dataset$defw2),
                                   mean_wealth_nonpols(dataset$defw2),
                                   n_pols_cov(dataset$defw2, covs = covariates),
                                   n_nonpols_cov(dataset$defw2, covs = covariates),
                                   "Optimal"),
                      an_defw2_w = c(get_coef_cov2(dataset$defw2, bw_mult = 2, covs = covariates),
                                     get_se_bc_cov2(dataset$defw2, bw_mult = 2, covs = covariates),
                                     get_se_rob_cov2(dataset$defw2, bw_mult = 2, covs = covariates),
                                     mean_wealth_pols(dataset$defw2),
                                     mean_wealth_nonpols(dataset$defw2),
                                     n_pols_cov(dataset$defw2, covs = covariates),
                                     n_nonpols_cov(dataset$defw2, covs = covariates),
                                     "2 x Optimal"))

notitie <- "Table showing Bias-corrected and Robust standard errors clustered at the Birthplace-level. Panel A shows univariate regressions under the optimal MSE bandwidth computed separately for both sides from the cut-off point, and twice the optimal bandwidth. In panel B, selected covariates are added, in particular, covariates that seemed to be unbalanced at the 2\\\\% cutoff. In particular, the regression controls for lifespan, times participated in election, birthplace population, birthplace characteristics, age at election, and socialist recommendations. In addition, I control for politicians' lifespan. *: p < 0.10, **: p < 0.05, ***: p < 0.01."
knitr::opts_current$set(label = "mainresults")
datasummary_df(bind_rows(panel_a, panel_b) %>%
                 rename(` ` = names, 
                        "(1)" = an_defw,
                        "(2)" = an_defw_w,
                        "(3)" = an_defw2,
                        "(4)"  = an_defw2_w), 
               out = "kableExtra",
               output = "latex",
               title = "Main RD Estimates") %>%
  kableExtra::add_header_above(c(" " = 1, "Log(Wealth)" = 2, "Ihs(Wealth)" = 2)) %>%
  kableExtra::group_rows("Panel A: Baseline Estimates", 1, 8)  %>%
  kableExtra::group_rows("Panel B: Estimates With Selected Covariates", 9, 16) %>%
  kableExtra::kable_styling(latex_options = c("hold_position"), full_width = F, font_size = 10) %>%
  kableExtra::footnote(general = notitie, footnote_as_chunk = T, threeparttable = T, escape = F)  %>%
  kableExtra::save_kable("./Tables/rdd_mainresults_twobandwidths.tex")


## Table with analysis split by tenure
longtenure1 <- dataset %>%
  filter(politician_dummy == 0 | tenure > 25)

shorttenure1 <- dataset %>%
  filter(politician_dummy == 0 | tenure < 10)

# Build these specifications into a table:
# 1 with defw, long tenure, short tenure, 1 with defw2, long tenure, short tenure
rdrobust(longtenure1$defw, longtenure1$margin, covs = make_covariates(longtenure1)) %>% summary()
rdrobust(shorttenure1$defw, shorttenure1$margin, covs = make_covariates(shorttenure1)) %>% summary()

tenure_table <- data.frame(names = c("Coefficient", 
                                "SE (BC)",
                                "SE (Rob.)",
                                "N (Politicians)",
                                "N (Non-Politicians)",
                                "Bandwidth"),
           first = c(get_coef_t('defw', longtenure1),
                     get_se_bc_t('defw', longtenure1),
                     get_se_rob_t('defw', longtenure1),
                     get_n_treated_t(longtenure1, 'defw'),
                     get_n_untreated_t(longtenure1, 'defw'),
                     "Optimal"),
           second = c(get_coef_t('defw', shorttenure1),
                      get_se_bc_t('defw', shorttenure1),
                      get_se_rob_t('defw', shorttenure1),
                      get_n_treated_t(shorttenure1, 'defw'),
                      get_n_untreated_t(shorttenure1, 'defw'),
                      "Optimal"),
           third = c(get_coef_t('defw2', longtenure1),
                     get_se_bc_t('defw2', longtenure1),
                     get_se_rob_t('defw2', longtenure1),
                     get_n_treated_t(longtenure1, 'defw2'),
                     get_n_untreated_t(longtenure1, 'defw2'),
                     "Optimal"),
           fourth = c(get_coef_t('defw2', shorttenure1),
                      get_se_bc_t('defw2', shorttenure1),
                      get_se_rob_t('defw2', shorttenure1),
                      get_n_treated_t(shorttenure1, 'defw2'),
                      get_n_untreated_t(shorttenure1, 'defw2'),
                      "Optimal"))

notitie <- "Table showing Bias-corrected and Robust standard errors clustered at the Birthplace-level. The table shows regressions under the optimal MSE bandwidth with the addition of several covariates (lifespan, newspaper recommendations and birthplace differences).  *: p < 0.10, **: p < 0.05, ***: p < 0.01."

knitr::opts_current$set(label = "tenure_results")
datasummary_df(tenure_table %>%
                 rename(` ` = names, 
                        "(1)" = first,
                        "(2)" = second,
                        "(3)" = third,
                        "(4)"  = fourth), 
               out = "kableExtra",
               output = "latex",
               title = "RD Estimates According to Tenure") %>%
  kableExtra::add_header_above(c(" " = 1, "Tenure > 20" = 1, "Tenure < 5" = 1, "Tenure > 20" = 1, "Tenure < 5" = 1)) %>%
  kableExtra::add_header_above(c(" " = 1, "Log(Wealth)" = 2, "Ihs(Wealth)" = 2)) %>%
  kableExtra::kable_styling(latex_options = c("scale_down", "hold_position"), full_width = F, font_size = 10) %>%
  kableExtra::footnote(general = notitie, footnote_as_chunk = T, threeparttable = T, escape = F)  %>%
  kableExtra::save_kable("./Tables/rdd_results_tenure.tex")

## tenure table with flexible bandwidth

tenure_table_flexband <- data.frame(names = c("Coefficient", 
                                     "SE (BC)",
                                     "SE (Rob.)",
                                     "N (Politicians)",
                                     "N (Non-Politicians)",
                                     "Bandwidth"),
                           first = c(get_coef_t('defw', longtenure1, 'msetwo'),
                                     get_se_bc_t('defw', longtenure1, 'msetwo'),
                                     get_se_rob_t('defw', longtenure1, 'msetwo'),
                                     get_n_treated_t(longtenure1, 'defw'),
                                     get_n_untreated_t(longtenure1, 'defw'),
                                     "Optimal"),
                           second = c(get_coef_t('defw', shorttenure1, 'msetwo'),
                                      get_se_bc_t('defw', shorttenure1, 'msetwo'),
                                      get_se_rob_t('defw', shorttenure1, 'msetwo'),
                                      get_n_treated_t(shorttenure1, 'defw'),
                                      get_n_untreated_t(shorttenure1, 'defw'),
                                      "Optimal"),
                           third = c(get_coef_t('defw2', longtenure1, 'msetwo'),
                                     get_se_bc_t('defw2', longtenure1, 'msetwo'),
                                     get_se_rob_t('defw2', longtenure1, 'msetwo'),
                                     get_n_treated_t(longtenure1, 'defw2'),
                                     get_n_untreated_t(longtenure1, 'defw2'),
                                     "Optimal"),
                           fourth = c(get_coef_t('defw2', shorttenure1, 'msetwo'),
                                      get_se_bc_t('defw2', shorttenure1, 'msetwo'),
                                      get_se_rob_t('defw2', shorttenure1, 'msetwo'),
                                      get_n_treated_t(shorttenure1, 'defw2'),
                                      get_n_untreated_t(shorttenure1, 'defw2'),
                                      "Optimal"))

notitie <- "Table showing Bias-corrected and Robust standard errors clustered at the Birthplace-level. The table shows regressions under the optimal MSE bandwidth at each side of the cut-off, under the addition of several covariates (lifespan, newspaper recommendations and birthplace differences).  *: p < 0.10, **: p < 0.05, ***: p < 0.01."

knitr::opts_current$set(label = "tenure_results_flexbw")
datasummary_df(tenure_table_flexband %>%
                 rename(` ` = names, 
                        "(1)" = first,
                        "(2)" = second,
                        "(3)" = third,
                        "(4)"  = fourth), 
               out = "kableExtra",
               output = "latex",
               title = "RD Estimates According to Tenure") %>%
  kableExtra::add_header_above(c(" " = 1, "Tenure > 20" = 1, "Tenure < 5" = 1, "Tenure > 20" = 1, "Tenure < 5" = 1)) %>%
  kableExtra::add_header_above(c(" " = 1, "Log(Wealth)" = 2, "Ihs(Wealth)" = 2)) %>%
  kableExtra::kable_styling(latex_options = c("scale_down", "hold_position"), full_width = F, font_size = 10) %>%
  kableExtra::footnote(general = notitie, footnote_as_chunk = T, threeparttable = T, escape = F)  %>%
  kableExtra::save_kable("./Tables/rdd_results_tenure_flexbw.tex")
