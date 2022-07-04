#figure_placebo_staggeredparty
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
    dataset$lifespan,
    dataset$age_at_election,
    dataset$age_of_death,
    dataset$district_cath,
    dataset$district_cath,
    dataset$birthplace_cath
  )
  
}

# reload the settings of previous analysis
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
output <- data.frame(
  period = seq(-8, 8, 1),
  magnitude_high = vector(length=length(seq(-8,8,1))),
  magnitude_low = vector(length=length(seq(-8,8,1))),
  ci_high_up = vector(length=length(seq(-8,8,1))),
  ci_high_down = vector(length=length(seq(-8,8,1))),
  ci_low_up = vector(length=length(seq(-8,8,1))),
  ci_low_down = vector(length=length(seq(-8,8,1))),
  diff = vector(length=length(seq(-8,8,1))),
  diff_se = vector(length=length(seq(-8,8,1)))
  )

for(k in seq(-8, 8, 1)){
  dataset_wp <- dataset %>%
  filter(hoevaak_gewonnen_verleden == 0) %>%
  mutate(within_party = case_when(party_category == "catholic" & verkiezingdatum > dmy("15-10-1904") + years(k) ~ 1,
                                  party_category == "protestant" & verkiezingdatum > dmy("03-04-1879") + years(k) ~ 1,
                                  party_category == "liberal" & verkiezingdatum > dmy("04-03-1885") + years(k) ~ 1,
                                  party_category == "socialist" & verkiezingdatum > dmy("26-08-1894") + years(k) ~ 1,
                                  TRUE ~ 0),
         party_none = if_else(party_category == "none", 1, 0),
         party_lib = if_else(party_category == "liberal", 1, 0),
         party_prot = if_else(party_category == "protestant", 1, 0),
         party_cath = if_else(party_category == "catholic", 1, 0),
         party_soc = if_else(party_category == "socialist", 1, 0))
  
  
  high <- rdrobust(dataset_wp$defw, dataset_wp$margin, 
                   weights = if_else(dataset_wp$within_party == 1,0,1),
                   covs = make_covariates(dataset_wp),
                   bwselect = 'msecomb2')
  low <- rdrobust(dataset_wp$defw, dataset_wp$margin, 
                  weights = if_else(dataset_wp$within_party == 1,1,0),
                  covs = make_covariates(dataset_wp),
                  bwselect = 'msecomb2')
  
  output[output['period'] == k,'magnitude_high'] <- high$coef[1]
  output[output['period'] == k,'magnitude_low'] <- low$coef[1]
  
  output[output['period'] == k,'ci_high_up'] <- high$ci[4]
  output[output['period'] == k,'ci_high_down'] <- high$ci[1]
  
  output[output['period'] == k,'ci_low_up'] <- low$ci[4]
  output[output['period'] == k,'ci_low_down'] <- low$ci[1]
  
  output[output['period'] == k, 'diff'] <- high$coef[1] - low$coef[1]
  output[output['period'] == k, 'diff_se'] <- sqrt(high$se[1]^2 + low$se[1]^2)
  
  #print(2*(1-pnorm((high$coef[1] - low$coef[1]), mean = 0, sd = sqrt(high$se[1]^2 + low$se[1]^2))) %>% round(3))
  
}

placebo <- output %>%
  ggplot(aes(x = period)) +
  theme_bw() +
  xlab("Fictitious Party Establishment Date Relative to True Date (0)") +
  ylab("Magnitude of difference before-after") +
  geom_errorbar(aes(
    ymin = diff -1.65*diff_se, 
    ymax = diff + 1.65*diff_se), size = 0.2, color = 'black', width = 0.2) +
  geom_point(aes(y = diff), color = 'blue') +
  geom_point(aes(x = 0, y = 1.41507796), color = 'red') +
  geom_text(aes(x = 0.5, y = 1.41), label = "Actual Estimate", vjust=c(-7.5), hjust = c(-0.1)) +
  geom_segment(aes(x = 1.7, y = 2.4, xend = 0.2, yend = 1.45), arrow = arrow(length = unit(0.2, "cm")))


ggplot2::ggsave("./figures_appendix/figure_placebo_staggeredparty.pdf", placebo, width = 10, height = 4)
