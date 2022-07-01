#step7_fig_wealth_table
#welath naar affil: eerste en tweede kamer
library(readxl)
library(tidyverse)
library(xtable)
library(gridExtra)
library(cowplot)   
library(janitor)
source("./Code/classify.R")
# Read data
polparty <- read_csv("Data/key_politicalparty_category.csv") %>%
  select(-1)

wealth <- read_xlsx("./Data/AnalysisFile.xlsx", sheet = "Analysis") %>%
  clean_names()

#LH and clean
lh_parliaments <- read_csv("./Data/lh_parliaments.csv") %>%
  clean_names() %>%
  select(-1)

lh_parliaments <- left_join(lh_parliaments, wealth,
                            by = c("b1_nummer" = "indexnummer"))

lh_parliaments <- classify(lh_parliaments)

#UH and clean
uh_parliaments <- read_csv("./Data/uh_parliaments.csv") %>%
  clean_names() %>%
  select(-1)

uh_parliaments <- left_join(uh_parliaments, wealth,
                            by = c("b1_nummer" = "indexnummer"))

uh_parliaments <- classify(uh_parliaments)

#Min and clean
ministers <- read_xlsx("./Data/bewindslieden_1815tot1950_uu.xlsx") %>%
  clean_names()

ministers <- merge(ministers, wealth, 
                   by.x = "b1_nummer", 
                   by.y = "indexnummer") %>%
  as_tibble()

ministers <- classify(ministers)

#Dep and clean
gedeputeerden <- wealth %>%
  filter(grepl("G|C", indexnummer))

#write a standard summarize function
make_sum <- function(df, variable) {
  
  
  temp <- df %>%
    dplyr::distinct(.[1], .keep_all = TRUE) 
  
  if("class" %in% colnames(df)){
    out <- temp %>%
    dplyr::group_by(class) %>%
    dplyr::summarize(Mean = mean({{variable}}, na.rm = T), 
                     Median = median({{variable}}, na.rm = T),
                     StdDev = sd({{variable}}, na.rm = T),
                     p25 = quantile({{variable}}, probs = 0.25, na.rm = TRUE),
                     p75 = quantile({{variable}}, probs = 0.75, na.rm = TRUE),
                     n = sum(!is.na({{variable}}))) %>%
    filter(class != "")
      
  }
  
  if(!"class" %in% colnames(df)){
    out <- temp %>%
      dplyr::summarize(Mean = mean({{variable}}, na.rm = T), 
                     Median = median({{variable}}, na.rm = T),
                     StdDev = sd({{variable}}, na.rm = T),
                     p25 = quantile({{variable}}, probs = 0.25, na.rm = TRUE),
                     p75 = quantile({{variable}}, probs = 0.75, na.rm = TRUE),
                     n = sum(!is.na({{variable}}))) %>%
      mutate(class = "-") %>%
      relocate(class, Mean, Median, StdDev, p25, p75, n)
    
  }
  
  out %>%
    rename("Political Affiliation" = class)
}


kinds <- list(lh_parliaments, uh_parliaments, ministers, gedeputeerden)

kinds <- lapply(kinds, make_sum, w_deflated)

kinds <- kinds %>%
  lapply(relocate, `Political Affiliation`, n, Mean, StdDev, p25, Median, p75) %>%
  lapply(rename, p50 = Median, N = n)

for(i in 1:length(kinds)){          
  write_csv(kinds[[i]], path = paste("./Data/comp_with_pop_",i,".csv", sep = ""))
}

#Try to make it in thousand guilders
kinds <- lapply(kinds, mutate, across(Mean:p75, ~ .x / 1000))


names(kinds) <- c("Lower House", "Upper House", "Ministers", "Regional Executives")
attr(kinds, "subheadings") <- paste0("Panel ", c("A","B","C","D"),": ", names(kinds))
kinds <- xtableList(kinds, 
                    caption = "Wealth according to political affiliation (1000 guilders)",
                    digits = c(0,0,1,1,1,1,1,0),
                    label = "tab:wealthfunction")
print.xtableList(kinds, 
                 colnames.format = "multiple", 
                 include.rownames = F)


#footnote
#\floatfoot{This table shows various statistics (mean, standard deviation, 25th, 50th (median), and 75th quantile) of net wealth at the age of death (in 1000 guilders, in values of 1900) for politicians according to representative body, and political affiliation. These numbers give a good idea of how wealthy the bulk of representatives were across the period 1870-1920. Note that the sample sizes does not add up to the unique amount of \textit{Memories van Successie}, since some politicians were members of more than one representative body, and hence, have been taken into account more than once.}
