#welath naar affil: eerste en tweede kamer
library(readxl)
library(tidyverse)
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

## Summarize function
get_shares <- function(df) {
  df %>%
    rowwise() %>%
    mutate(total = sum(across(re:misc), na.rm = T),
           re_share = re/total,
           shares_share = (fosh+dush)/total,
           bonds_share = (foprbo + duprbo + fogobo + dugobo)/total, 
           misc = (cash + misc)/total)  %>%
    ungroup() %>%
    summarize(re_mean = mean(re_share, na.rm = T),
              re_sd = sd(re_share, na.rm = T), 
              shares_mean = mean(shares_share, na.rm = T),
              shares_sd = sd(shares_share, na.rm = T),
              bonds_mean = mean(bonds_share, na.rm = T),
              bonds_sd = sd(bonds_share, na.rm = T),
              misc_mean = mean(misc, na.rm = T),
              misc_sd = sd(misc, na.rm = T))
  
}


data <- list(lh_parliaments, uh_parliaments, ministers, gedeputeerden)

data <- lapply(data, get_shares) 
names(data) <- c("Lower House", "Upper House", "Ministers", "Provincial Executives")

ans <- map_df(data, ~as.data.frame(.x), .id="id") 

p1 <- ans %>%
  pivot_longer(re_mean:misc_sd) %>%
  separate(col = name, into = c("asset", "stat")) %>%
  filter(stat == "mean") %>%
  ggplot(aes(x = id, y = value, fill = asset)) + geom_bar(position = "dodge",
                                                                     stat="identity") +
  theme_classic() +
  ylab("Portfolio Share") +
  xlab("Politician") + 
  ggtitle("Portfolio Share per Political Function") 

ggsave("./Figures/portfolio_composition_function.png", p1)

         