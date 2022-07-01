## table showing that wealth does not predict margin
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

fifthrents <- dataset %>%
  filter(hoevaak_gewonnen_verleden == 4)
sixthrents <- dataset %>%
  filter(hoevaak_gewonnen_verleden == 5)

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



## make the regression table
model1 <- lm(data = firstrents_pooled, gewonnen ~ defw + party_category + age_at_election   + no_candidates + as.numeric(opkomst) + district)
model2 <- lm(data = secondrents, gewonnen ~ defw + party_category + age_at_election   + no_candidates + as.numeric(opkomst) + district)
model3 <- lm(data = thirdrents, gewonnen ~ defw + party_category + age_at_election  + no_candidates + as.numeric(opkomst) + district)
model4 <- lm(data = fourthrents, gewonnen ~ defw + party_category + age_at_election + no_candidates + as.numeric(opkomst) + district)
model5 <- lm(data = fifthrents, gewonnen ~ defw + party_category + age_at_election  + no_candidates + as.numeric(opkomst) + district)
model6 <- lm(data = sixthrents, gewonnen ~ defw + party_category + age_at_election  + no_candidates + as.numeric(opkomst) + district)



models <- list("(1)"=model1, "(2)"=model2, "(3)"=model3, "(4)"=model4, "(5)"=model5, "(6)"=model6)

gm <- tibble::tribble(
  ~raw,        ~clean,          ~fmt,
  "nobs",      "N",             0,
  "adj.r.squared","Adj. R2", 2)

coefconvert <- c(
  "defw" = "Personal Wealth")

knitr::opts_current$set(label = "wealth_prob_election")
notes <- "Robust standard errors in parentheses. Analysis show the correlation between end-of-life wealth and probability of election in the 1st election in (1). Then, in the second election given that the first election was won, in (2), etc. Estimates are conditional on party controls, electoral controls, and district fixed effects. *: p<0.1, **: p<0.05, ***:p<0.01."
modelsummary(models,
             stars = c("*" = .1, "**" = 0.05, "***" = 0.01),
             vcov = "HC0",
             gof_map = gm,
             coef_map = coefconvert,
             title="Correlation between Wealth and Probability of Election",
             add_rows = tribble(~raz, ~dva, ~tri, ~chetire, ~pjat, ~shest, ~sem,
                               "Party Controls", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes",
                               "Electoral Controls", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes",
                               "District FE","Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
             out="kableExtra",
             output = "./tables_appendix/wealth_prob_election.tex"
) %>%
  kableExtra::kable_styling(latex_options = c("hold_position", "scale_down"))  %>%
  kableExtra::footnote(general = notes, footnote_as_chunk = T, threeparttable = T, escape = F)  %>%
  kableExtra::save_kable("./tables_appendix/wealth_prob_election.tex")
