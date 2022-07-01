#candidacy_recommendation_predict_wealth

#candidate_predict_wealth
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
  janitor::clean_names() %>%
  mutate(verkiezingdatum = lubridate::dmy(verkiezingdatum))


find_candidatenextelection_rec <- function(row){
  
  ok <- c("algemeen", "periodiek")
  nextelection <- help_allelections %>%
    filter(
      is.element(type, ok),
      verkiezingdatum > row$verkiezingdatum) %>%
    pull(verkiezingdatum) %>%
    min()
  
  dataset %>%
    filter(naam == row$naam, 
           verkiezingdatum == nextelection,
           !is.na(aanbevolen_door)) %>%
    nrow() > 0
  
}

dataset <- dataset %>%
  rowwise() %>%
  do(row = as.data.frame(.)) %>%
  mutate(candidacy_nextelection_rec = find_candidatenextelection_rec(row)) %>%
  unnest(cols = c(row))

# create the datasets again
firstrents_pooled <- dataset %>%
  filter(hoevaak_gewonnen_verleden == 0)
secondrents <- dataset %>%
  filter(hoevaak_gewonnen_verleden == 1)
thirdrents <- dataset %>%
  filter(hoevaak_gewonnen_verleden == 2)
fourthrents <- dataset %>%
  filter(hoevaak_gewonnen_verleden == 3) 
fifthrents <- dataset %>%
  filter(hoevaak_gewonnen_verleden == 4)
sixthrents <- dataset %>%
  filter(hoevaak_gewonnen_verleden == 5)

## make the regression table
model1 <- lm(data = firstrents_pooled, candidacy_nextelection_rec ~ defw + party_category + age_at_election   + no_candidates + as.numeric(opkomst) + district)
model2 <- lm(data = secondrents, candidacy_nextelection_rec ~ defw + party_category + age_at_election   + no_candidates + as.numeric(opkomst) + district)
model3 <- lm(data = thirdrents, candidacy_nextelection_rec ~ defw + party_category + age_at_election  + no_candidates + as.numeric(opkomst) + district)
model4 <- lm(data = fourthrents, candidacy_nextelection_rec ~ defw + party_category + age_at_election + no_candidates + as.numeric(opkomst) + district)
model5 <- lm(data = fifthrents, candidacy_nextelection_rec ~ defw + party_category + age_at_election  + no_candidates + as.numeric(opkomst) + district)
model6 <- lm(data = sixthrents, candidacy_nextelection_rec ~ defw + party_category + age_at_election  + no_candidates + as.numeric(opkomst) + district)

models <- list("(1)"=model1, "(2)"=model2, "(3)"=model3, "(4)"=model4, "(5)"=model5, "(6)"=model6)

gm <- tibble::tribble(
  ~raw,        ~clean,          ~fmt,
  "nobs",      "N",             0,
  "adj.r.squared","Adj. R2", 2)

coefconvert <- c(
  "defw" = "Personal Wealth")

knitr::opts_current$set(label = "wealth_prob_candidacy_rec")
notes <- "Robust standard errors in parentheses. Analysis show the correlation between end-of-life wealth and probability of candidacy and recommendation in the 1st election in (1). Then, in the second election given that the first election was won, in (2), etc. Estimates are conditional on party controls, electoral controls, and district fixed effects. *: p<0.1, **: p<0.05, ***:p<0.01."
modelsummary(models,
             stars = c("*" = .1, "**" = 0.05, "***" = 0.01),
             vcov = "HC0",
             gof_map = gm,
             coef_map = coefconvert,
             title="Correlation between Wealth and Probability of Candidacy and Recommendation",
             add_rows = tribble(~raz, ~dva, ~tri, ~chetire, ~pjat, ~shest, ~sem,
                                "Party Controls", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes",
                                "Electoral Controls", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes",
                                "District FE","Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
             out="kableExtra",
             output = "./tables_appendix/wealth_prob_candidacy_rec.tex"
) %>%
  kableExtra::kable_styling(latex_options = c("hold_position", "scale_down"))  %>%
  kableExtra::footnote(general = notes, footnote_as_chunk = T, threeparttable = T, escape = F)  %>%
  kableExtra::save_kable("./tables_appendix/wealth_prob_candidacy_rec.tex")

