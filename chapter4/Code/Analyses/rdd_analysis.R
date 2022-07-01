library(readxl); library(tidyverse); library(hrbrthemes); 
library(rdrobust); library(modelsummary)

# Parameters
target_margin_neg <- -0.18 #0.18
target_margin_pos <- 0.20 #0.20
maxdate <- '01-01-1928'

## Few mutations with the data set
dataset <- read_csv("./Data/analysis/unmatched_sample_with_vars.csv") %>%
  select(-1) %>%
  mutate(defw = log(1+Vermogen_deflated),
         defw2 = asinh(Vermogen_deflated),
         distrverk = str_c(District, "-", Verkiezingdatum),
         lifespan = lifespan/365,
         tenure = as.numeric(str_extract(tenure, "\\d+"))/365,
         politician_dummy = if_else(!is.na(`b1-nummer`), 1, 0),
         politician_indic = if_else(!is.na(`b1-nummer`), "Politician", "Non-Politician"),
         rec_ar = case_when(stringr::str_detect(party_election, "AR|VA|NC|NH") ~ 1, 
                            is.na(party_election) ~ 0,
                                   TRUE~  0),
         rec_kath = case_when(stringr::str_detect(party_election, "Ka|KD") ~ 1,
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
         )

## Aply the filter
dataset <- dataset %>%
  filter(!is.na(defw)) %>%
  filter(margin > target_margin_neg, margin < target_margin_pos) %>% 
  filter(lubridate::dmy(Sterfdatum) < lubridate::dmy(maxdate))
  ## Changing the last two lines helps alleviate the density asymmetry
  ## Making the max. margin equal
  ## Making the circumstances equal (last line)

## Match on election
elections_for_ctrl_group <- dataset %>%
  filter(is.na(`b1-nummer`)) %>%
  select(distrverk) %>%
  pull()

dataset_matched <- dataset %>%
  filter(is.element(distrverk, elections_for_ctrl_group))

#elections_for_treat_group <- dataset_matched %>%
#  filter(!is.na(`b1-nummer`)) %>%
#  select(distrverk) %>%
#  pull()


#dataset_matched <- dataset_matched %>%
#  filter(is.element(distrverk, elections_for_treat_group))

## Create a covariate balance table
caption <- "Covariate Balance in Politicians and Non-Politicians"

tabledata <- dataset %>%
  select(Margin = margin, Turnout = turnout, 
         `Amount Votes` = amount_votes,
         `Amount Seats in District` = amount_seats,
         `Election: General` = elec_type_alg,
         `Election: Other` = elec_type_else,
         Kiesdrempel = kiesdrempel,
         `Rec: AR` = rec_ar,
         `Rec: RC` = rec_kath,
         `Rec: Lib` = rec_lib,
         `Rec: Soc` = rec_soc,
         `# Elections Participated Before` = before,
         `# Elections Participated After` = after,
         `Yrs Lived After Close Election` = lifespan,
         `Year of Death` = yod,
         `Year of Election` = yoe,
         politician_indic) 

knitr::opts_current$set(label = "covariate_balance_standard")
datasummary_balance(~politician_indic,
                    data = tabledata,
                    output = "./Tables/covariate_balance_standard.tex",
                    out = "kableExtra",
                    caption = caption,
                    fmt=2, 
                    dinm = TRUE,
                    dinm_statistic = "p.value")  %>%
  kableExtra::kable_styling(latex_options = "hold_position",
                            font_size = 9)


rdplot(y=dataset$defw, x=dataset$margin, ci=90, 
       title="RD Plot: U.S. Senate Election Data", 
       y.label="Vote Share in Election at time t+2",
       x.label="Vote Share in Election at time t") 

model1 <- rdrobust(y=dataset$defw, x=dataset$margin, covs = cbind(dataset$lifespan,
                                                                dataset$before,
                                                                dataset$after,
                                                                dataset$amount_votes,
                                                                dataset$rec_lib,
                                                                dataset$rec_kath,
                                                                dataset$yod),
        all = TRUE)

summary(model1)

# [Plot of E[Y|X] to also see the discontinuity in outcomes]
p1 <- dataset %>%
  ggplot(aes(x = margin, y = defw)) + 
  geom_point(alpha=0.5) +
  xlab("Margin") + 
  ylab("Log(Wealth)") +
  theme_minimal()

# [Plot of E[W|X] to not see a discontinuity in covariates]
# Still to do
p2 <- dataset %>%
  ggplot(aes(x = margin, y = politician_dummy)) +
  geom_point() +
  xlab("Margin") +
  ylab("Prob(Politician)") +
  theme_minimal()

# [Density of X (check for manipulation, McGreary test]
dataset_analysis <- rdd_data(
        y = defw,
         x = margin,
         data = dataset_matched,
         cutpoint = 0, covar = data.frame(lifespan, before, after, amount_votes, rec_lib, rec_kath, yod))

bw_ik <- rdd_bw_ik(dataset_analysis)

model1 <- rdd_reg_np(
  rdd_object=dataset_analysis, bw=bw_ik)

model2<- rdd_reg_lm(rdd_object = dataset_analysis)

summary(model1);summary(model2)
dens_test(model1)
dens_test(model2)


model3 <- rdrobust(y=dataset_matched$defw, x=dataset_matched$margin, covs = cbind(dataset_matched$lifespan,
                                                                  dataset_matched$before,
                                                                  dataset_matched$after,
                                                                  dataset_matched$amount_votes,
                                                                  dataset_matched$rec_lib,
                                                                  dataset_matched$rec_kath,
                                                                  dataset_matched$yod),
                   all = TRUE)

summary(model3)


plot(dataset_analysis, h = c(0.005, 0.05, 0.1), nplot = 3)

plotSensi(model1, from = 0, to = 0.25, by = 0.002)

