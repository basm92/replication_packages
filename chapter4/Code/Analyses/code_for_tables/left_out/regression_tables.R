# regression_tables
library(readxl); library(tidyverse); library(hrbrthemes); library(rdrobust); library(modelsummary)
library(ggtext); library(lubridate)
# calculate att etc. 
source("./Code/Analyses/function_calculate_itt_and_att.R")
source("./Code/Analyses/functions_for_tables.R")
source("./Code/Analyses/code_for_tables/new_data_analysis.R")

# package for rdd tables

# Table first period rents ATT and ITT
# Next table: ATTS without and with covariates

panel_a <- data.frame(names = c("ΑΤΤ_1", "ATT_2", "ATT_3", "ATT_4", "ATT_5", "ATT_6", "ATT_7"),
           `t_star4` = rep(0, 7),
           `t_star5` = rep(0,7),
           `t_star6` = rep(0,7),
           `t_star7` = rep(0,7))

# fill this matrix

for(j in 2:5){
  
  data <- compute_itt_and_att(dataset, j+2)[[3]] %>%
    round(3)
  
  for(i in 1:7){
    
    if(i > nrow(data)){
      break
    } else{
      
      out <- dplyr::case_when(data[i, 1] / data[i, 2] < 1.64 ~ paste(format(data[i,1], nsmall = 3)),
                              between(data[i,1]/data[i,2], 1.64, 1.96) ~ paste(format(data[i,1], nsmall=3), "*", sep = ""),
                              between(data[i, 1]/data[i,2], 1.96, 2.58) ~ paste(format(data[i,1],nsmall=3), "**", sep = ""),
                              data[i, 1] / data[i,2] > 2.58 ~ paste(format(data[i,1],nsmall=3), "***", sep = ""))
                              
      panel_a[i, j] <- out
    }
  }
  
}

panel_b <- data.frame(names = c("ATT_1", "ATT_2", "ATT_3", "ATT_4", "ATT_5", "ATT_6", "ATT_7"),
                      `t_star4` = rep(0, 7),
                      `t_star5` = rep(0,7),
                      `t_star6` = rep(0,7),
                      `t_star7` = rep(0,7))


covariates <- c("yoe", "birthplace_agri", "birthplace_indus", "lifespan", "rec_soc")

for(j in 2:5){
  
  data <- compute_itt_and_att(dataset, j+2, covs = covariates, bwselect='msetwo')[[3]] %>%
    round(3)
  
  for(i in 1:7){
    
    if(i > nrow(data)){
      break
    } else{
      
      out <- dplyr::case_when(data[i, 1] / data[i, 2] < 1.64 ~ paste(format(data[i,1], nsmall = 3)),
                              between(data[i,1]/data[i,2], 1.64, 1.96) ~ paste(format(data[i,1], nsmall=3), "*", sep = ""),
                              between(data[i, 1]/data[i,2], 1.96, 2.58) ~ paste(format(data[i,1],nsmall=3), "**", sep = ""),
                              data[i, 1] / data[i,2] > 2.58 ~ paste(format(data[i,1],nsmall=3), "***", sep = ""))
      
      panel_b[i, j] <- out
    }
  }
  
}

notitie <- "Table showing coefficients effects of stints \\\\{1, \\\\dots, 7\\\\} under different t*. All the ATT coefficients are derived and recursively computed from ITT coefficients, which are in turn estimated using the methodology in \\\\citep{cattaneo2019practical} using MSE-optimal bandwidth. Standard errors are calculated using the delta method. The estimates in panel A are without control variables and the estimates in panel B control for birthplace population, birthplace characteristics, age at election, socialist newspaper recommendations and politicians' lifespan. *: p < 0.10, **: p < 0.05, ***: p < 0.01. "

knitr::opts_current$set(label = "attresults")
datasummary_df(bind_rows(panel_a, panel_b) %>%
                 rename("t* = 4" = t_star4,
                        "t* = 5" = t_star5,
                        "t* = 6" = t_star6,
                        "t* = 7" = t_star7,
                        " " = names),
               out = "kableExtra",
               output = "latex",
               title = "ATT estimates for different t*") %>%
  kableExtra::group_rows("Panel A: Without Control Variables", 1, 7) %>%
  kableExtra::group_rows("Panel B: With Control Variables", 8, 14) %>%
  kableExtra::kable_styling(latex_options = c("hold_position"), full_width = F, font_size = 10) %>%
  kableExtra::footnote(general = notitie, footnote_as_chunk = T, threeparttable = T, escape = F)  %>%
  kableExtra::save_kable("./Tables/att_mainresults.tex")

