# Table with descriptives just before and just after party formation
#covariate balance tables
library(readxl); library(tidyverse); library(hrbrthemes); library(rdrobust); library(modelsummary)
library(ggtext); library(lubridate)
# calculate att etc. 
source("./Code/Analyses/function_calculate_itt_and_att.R")
source("./Code/Analyses/functions_for_tables.R")
source("./Code/Analyses/new_data_analysis.R")

# parameter for how many years
k <- 5

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

dataset_wp <- dataset %>%
  filter(hoevaak_gewonnen_verleden == 0) %>%
  mutate(party_cutoff = case_when(party_category == "catholic" & between(verkiezingdatum, dmy("15-10-1904"), dmy("15-10-1904") + years(k)) ~ "(0,5]",
                                  party_category == "protestant" & between(verkiezingdatum, dmy("03-04-1879"), dmy("03-04-1879") + years(k)) ~ "(0,5]",
                                  party_category == "liberal" & between(verkiezingdatum, dmy("04-03-1885"), dmy("04-03-1885") + years(k)) ~ "(0,5]",
                                  party_category == "socialist" & between(verkiezingdatum, dmy("26-08-1894"), dmy("26-08-1894") + years(k)) ~ "(0,5]",
                                  party_category == "catholic" & between(verkiezingdatum, dmy("15-10-1904") - years(k), dmy("15-10-1904")) ~ "[-5,0)",
                                  party_category == "protestant" & between(verkiezingdatum, dmy("03-04-1879") - years(k), dmy("03-04-1879")) ~ "[-5,0)",
                                  party_category == "liberal" & between(verkiezingdatum, dmy("04-03-1885") - years(k), dmy("04-03-1885")) ~ "[-5,0)",
                                  party_category == "socialist" & between(verkiezingdatum, dmy("26-08-1894") - years(k), dmy("26-08-1894")) ~ "[-5,0)",
                                  TRUE ~ NA_character_)
  )

# function to calculate the p-value & difference between these groups
t_val <- function(x) {
  out <- t.test(x[!is.na(dataset_wp$party_cutoff)]~ dataset_wp$party_cutoff[!is.na(dataset_wp$party_cutoff)])
  stat <- out$statistic %>%
    round(3) %>%
    format(nsmall=3) 
  
  return(stat)

}

p_val <- function(x) {
  out <- t.test(x[!is.na(dataset_wp$party_cutoff)]~ dataset_wp$party_cutoff[!is.na(dataset_wp$party_cutoff)])
  if(out$p.value > 0.1){
    pv <- out$p.value %>%
      round(3) %>%
      format(nsmall=3)
    return(pv)
  }
  else if(between(out$p.value, 0.05, 0.10)){
    pv <- out$p.value %>%
      round(3) %>%
      format(nsmall=3)
    paste(pv, "*", sep = "")
  }
  else if(between(out$p.value, 0.01, 0.05)){
    pv <- out$p.value %>%
      round(3) %>%
      format(nsmall=3)
    paste(pv, "**", sep = "")
  }
  else if(out$p.value < 0.01){
    pv <- out$p.value %>%
      round(3) %>%
      format(nsmall=3)
    paste(pv, "***", sep = "")
  }
}



# now, make a table like the descriptives table according to this status
notes <- "Table shows means and standard deviations for candidates who have not been elected before in two groups: from 0 to 5 years after party formation, and from 5 to 0 years before party formation. I then conduct Welch t-test and show the p-value. Significance is indicated as follows: *: p<0.1, **: p<0.05, ***: p<0.01."
knitr::opts_current$set(label = "change_composition_candidatepool")
datasummary(data = dataset_wp,
            align = c("lllllll"),
            formula = 
              (`Rec.: Protestant`=rec_ar) + 
              (`Rec.: Liberal`=rec_lib) + 
              (`Rec.: Socialist`=rec_soc) + 
              (`Rec: Catholic` = rec_kath) +
           (`Lifespan`=lifespan) +
             (`Age at Election`=age_at_election) +
             (`Year of Death`=yod) +
             (`Year of Election`=yoe) + 
             (`Log Turnout`=log(aantal_stemmen_geldig) )+
             (`Log Turnout Previous`=log(turnout_previous_el)) +
             (`Log Population 1859`=log(1+birthplace_pop_1859)) +
             (`Share Protestant`=birthplace_share_prot) +
             (`Share Catholic`=birthplace_share_cath) +
             (`Labor Force Share Agricul.`=birthplace_agri) +
             (`Labor Force Share Industry`=birthplace_indus )+
             (`Taxes Per Capita 1859`=taxespercap_1859) +
             (`Taxes Per Capita 1889`=taxespercap_1889) +
             (`Distance to the Hague`=distance_bp_hag) +
             (`Share Protestant`=district_share_prot) +
             (`Share Catholic`=district_share_cath) +
             (`Labor Force Share Agricul.`=district_agri) +
             (`Labor Force Share Industry`=district_indus)~ party_cutoff*(Mean + SD) + (`t-stat.`=t_val) + (`p-value`=p_val),
           out = "kableExtra",
           output = "latex",
           title = "Change in Candidate Composition After/Before Party Formation") %>%
  kableExtra::group_rows("Panel A: Newspaper Recommendations", 1,4) %>%
  kableExtra::group_rows("Panel B: Demographic Characteristics", 5, 8) %>%
  kableExtra::group_rows("Panel C: Election Characteristics", 9, 10) %>%
  kableExtra::group_rows("Panel D: Birthplace Characteristics", 11, 18)  %>%
  kableExtra::group_rows("Panel E: District Characteristics", 19, 22) %>%
  kableExtra::kable_styling(latex_options = c("hold_position", "scale_down"))  %>%
  kableExtra::footnote(general = notes, footnote_as_chunk = T, threeparttable = T, escape = F)  %>%
  kableExtra::save_kable("./tables_appendix/change_candidatepool_after_before_party.tex")
           