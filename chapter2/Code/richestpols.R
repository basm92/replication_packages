# richestpols table

library(readxl)
library(tidyverse)
library(xtable)
library(janitor)
source("./Code/classify.R")

#import wealth
wealth <- read_xlsx("./Data/AnalysisFile.xlsx", sheet = "Analysis") %>%
  clean_names()

#### First lower house
lh <- read_csv("./Data/lh_parliaments.csv") %>%
  select(-1) %>%
  distinct(b1_nummer, .keep_all = TRUE)

aod <- read_xlsx("./Data/tk_1815tot1950uu.xlsx", sheet = 2) %>%
  clean_names() %>%
  filter(rubriek == "3020") %>%
  rename(died = datum) %>%
  select(b1_nummer, died)

richest_lh <- left_join(lh, wealth, 
          by = c("b1_nummer" = "indexnummer")) %>%
  filter(w_deflated >= 0) %>%
  .[order(.$w_deflated, decreasing = TRUE),] %>%
  slice(1:5) %>%
  left_join(aod, by = c("b1_nummer" = "b1_nummer")) %>%
  select(Name, begin_periode, einde_periode, died, w_deflated)

### Now upper house
uh <- read_csv("./Data/uh_parliaments.csv") %>%
  select(-1) %>%
  distinct(b1_nummer, .keep_all = TRUE)

aod <- read_xlsx("./Data/ek_1815tot1950_uu.xlsx", sheet = 2) %>%
  clean_names() %>%
  filter(rubriek == "3020") %>%
  rename(died = datum) %>%
  select(b1_nummer, died)

richest_uh <- left_join(uh, wealth,
          by = c("b1_nummer" = "indexnummer")) %>%
  filter(w_deflated >=0) %>%
  .[order(.$w_deflated, decreasing = TRUE),] %>%
  slice(1:5) %>%
  left_join(aod, by = c("b1_nummer" = "b1_nummer")) %>%
  select(Name, begin_periode, einde_periode, died, w_deflated)

### Now ministers
min <- read_xlsx("./Data/bewindslieden_1815tot1950_uu.xlsx") %>%
  clean_names() %>%
  distinct()

aod <- read_xlsx("./Data/bewindslieden_1815tot1950_uu.xlsx", sheet = 2) %>%
  clean_names() %>%
  filter(rubriek == "3020") %>%
  rename(died = datum) %>%
  select(b1_nummer, died)

richest_min <- left_join(min, wealth,
          by = c("b1_nummer" = "indexnummer")) %>%
  filter(w_deflated >= 0) %>%
  .[order(.$w_deflated, decreasing = TRUE),] %>%
  slice(2:6) %>%
  left_join(aod, by = c("b1_nummer" = "b1_nummer")) %>%
  select(voorna_a_m_en, achternaam, begin_periode, einde_periode, died, w_deflated) %>%
  unite("Name", c(voorna_a_m_en, achternaam), 
        sep = " ", 
        na.rm = TRUE)

### Now provincials
dep <- read_xlsx("./Data/AnalysisFile.xlsx", sheet = "RegistrationFile") %>%
  janitor::clean_names() %>%
  filter(grepl("G|C", index_nummer) | grepl("gedeputeerde|commissaris", functie))

richest_dep <- left_join(dep, wealth,
          by = c("index_nummer" = "indexnummer")) %>%
  filter(w_deflated >= 0) %>%
  .[order(.$w_deflated, decreasing = TRUE),] %>%
  slice(1:5) %>%
  select(voorletters, achternaam, beginyr, endyr, yearofdeath, w_deflated) %>%
  unite("Name", c(voorletters, achternaam), sep = " ", na.rm = TRUE)


## Make a table
kinds <- list(richest_lh, richest_uh, richest_min, richest_dep) %>%
  lapply(rename, Wealth = w_deflated)

kinds[1:3] <- kinds[-4] %>%
  lapply(mutate, across(begin_periode:died, ~ stringr::str_extract(.x, "\\d{4}"))) %>%
  lapply(rename, Begin = begin_periode, End = einde_periode, Death = died)

kinds[4] <- kinds[4] %>%
  lapply(rename, Begin = beginyr, End = endyr, Death = yearofdeath)

kinds <- kinds %>%
  lapply(mutate, Wealth = Wealth / 1000)

attr(kinds, "subheadings") <- paste0("Panel ", 
                                     c("A", "B", "C", "D"), 
                                     ": ", 
                                     c("Lower House", "Upper House", "Ministers", "Provincial Executives"))


kinds <- xtableList(kinds, 
                    caption = "5 Richest Politicians in each Function (1000 guilders)",
                    label = "tab:richestpols",
                    digits = c(0,0,0,0,0,1))

print.xtableList(kinds,
                 #size = "footnotesize",
                 file = "./Tables/richestpols.tex",
                 colnames.format = "multiple", 
                 include.rownames = F,
                 format.args = list(big.mark = ","))
