# appendix_to_regression_table_staggered_party.R

# Explore the parameter space

# regression table staggered adoption
library(readxl); library(tidyverse); library(hrbrthemes); library(rdrobust); library(modelsummary)
library(lubridate)
# calculate att etc. 
source("./Code/Analyses/function_calculate_itt_and_att.R")
source("./Code/Analyses/functions_for_tables.R")
source("./Code/Analyses/new_data_analysis.R")

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
dataset_wp <- dataset %>%
  filter(hoevaak_gewonnen_verleden == 0) %>%
  mutate(within_party = case_when(party_category == "catholic" & election_after_rk == 1 ~ 1,
                                  party_category == "protestant" & verkiezingdatum > dmy("03-04-1879") ~ 1,
                                  party_category == "liberal" & election_after_lib == 1 ~ 1,
                                  party_category == "socialist" & verkiezingdatum > dmy("26-08-1894") ~ 1,
                                  TRUE ~ 0),
         party_none = if_else(party_category == "none", 1, 0),
         party_lib = if_else(party_category == "liberal", 1, 0),
         party_prot = if_else(party_category == "protestant", 1, 0),
         party_cath = if_else(party_category == "catholic", 1, 0),
         party_soc = if_else(party_category == "socialist", 1, 0))



## now the algorithm to explore the param space


all_vars <- dataset_wp %>% names() %>%
  .[c(17, 28, 50, 62, 63, 64, 65, 66, 67:86, 91, 99:102, 105)]

combinaytion <- combn(all_vars, 7)

make_vars <- function(vector){
  
  cbind(dataset_wp[vector[1]],
        dataset_wp[vector[2]],
        dataset_wp[vector[3]],
        dataset_wp[vector[4]],
        dataset_wp[vector[5]],
        dataset_wp[vector[6]],
        dataset_wp[vector[7]]) %>%
    as.matrix()
}


which_col <- vector(length = 7)
j <- 1
for(i in 1:length(ncol(combinaytion))){
  
  minimum_p_value <- 1
  
  while(minimum_p_value > 0.01){
    
    print(i) 
    
    high <- rdrobust(dataset_wp$defw, dataset_wp$margin, 
                     weights = if_else(dataset_wp$within_party == 1,0,1),
                     covs = make_vars(combinaytion[,i]),
                     bwselect = 'msetwo')
    low <- rdrobust(dataset_wp$defw, dataset_wp$margin, 
                    weights = if_else(dataset_wp$within_party == 1,1,0),
                    covs = make_vars(combinaytion[,i]),
                    bwselect = 'msetwo')
    
    p_value <- 2*(1-pnorm((high$coef[1] - low$coef[1]), mean = 0, sd = sqrt(high$se[1]^2 + low$se[1]^2)))
    
    if(p_value < minimum_p_value){
      print(p_value)
      print(combinaytion[,i])
      minimum_p_value <- p_value
      j <- i
      which_col <- combinaytion[,i]
    }
    
    i <- i + 1
  }
  
}



#descriptives
dataset_wp %>% 
  group_by(party_category) %>% 
  summarize(count = sum(defw >= 0, na.rm = T), 
            after_prot = sum(verkiezingdatum > dmy("03-04-1879") & defw >= 0, na.rm = T),
            after_lib =sum(verkiezingdatum > dmy("04-03-1885") & defw >= 0, na.rm = T),
            after_soc = sum(verkiezingdatum > dmy("26-08-1894") & defw >= 0, na.rm = T),
            after_cath = sum(verkiezingdatum > dmy("05-05-1897") & defw >= 0, na.rm = T))

