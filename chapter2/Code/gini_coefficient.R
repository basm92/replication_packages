#Gini coeff
# read wealth and lh
library(readxl)
library(tidyverse)
library(janitor)
library(xtable)
library(cowplot)

wealth <- read_xlsx("./Data/AnalysisFile.xlsx", sheet = "Analysis") %>%
  clean_names()

lh_parliaments <- read_csv("./Data/lh_parliaments.csv") %>%
  clean_names() %>%
  select(-1)

lh_parliaments <- left_join(lh_parliaments, wealth,
                            by = c("b1_nummer" = "indexnummer"))

#Create gini
gini_lh <- lh_parliaments %>%
  group_by(parliament) %>%
  filter(w_deflated > 0) %>%
  summarize(min = min(w_deflated, na.rm = TRUE),
            p25 = quantile(w_deflated, 0.25, na.rm = TRUE),
            p75 = quantile(w_deflated, 0.75, na.rm = TRUE),
            max = max(w_deflated, na.rm = TRUE),
            gini = DescTools::Gini(w_deflated, na.rm = TRUE))

# create gini while removing the most extreme observations
test <- lh_parliaments %>%
  group_by(parliament) %>%
  slice_max(w_deflated, n = 1)

giniwoext_lh <- setdiff(lh_parliaments, test) %>%
  group_by(parliament) %>%
  filter(w_deflated > 0) %>%
  summarize(gini = DescTools::Gini(w_deflated, na.rm = TRUE))

gini_lh <- gini_lh %>%
  left_join(giniwoext_lh, by = c("parliament" = "parliament"))

#now, read uh
uh_parliaments <- read_csv("./Data/uh_parliaments.csv") %>%
  clean_names() %>%
  select(-1)

uh_parliaments <- left_join(uh_parliaments, wealth,
                            by = c("b1_nummer" = "indexnummer"))

#Create gini
gini_uh <- uh_parliaments %>%
  group_by(parliament) %>%
  filter(w_deflated > 0) %>%
  summarize(min = min(w_deflated, na.rm = TRUE),
            p25 = quantile(w_deflated, 0.25, na.rm = TRUE),
            p75 = quantile(w_deflated, 0.75, na.rm = TRUE),
            max = max(w_deflated, na.rm = TRUE),
            gini = DescTools::Gini(w_deflated, na.rm = TRUE))

# create gini while removing the most extreme observations
test <- uh_parliaments %>%
  group_by(parliament) %>%
  slice_max(w_deflated, n = 1)

giniwoext_uh <- setdiff(uh_parliaments, test) %>%
  group_by(parliament) %>%
  filter(w_deflated > 0) %>%
  summarize(gini = DescTools::Gini(w_deflated, na.rm = TRUE))

gini_uh <- gini_uh %>%
  left_join(giniwoext_uh, by = c("parliament" = "parliament"))

gini_lh <- gini_lh %>%
  rename(gini = gini.x, `gini2` = gini.y)
gini_uh <- gini_uh %>%
  rename(gini = gini.x, `gini2` = gini.y)

#Make the tables
kinds <- list(gini_lh, gini_uh) %>%
  lapply(mutate, across(min:max, ~ .x / 1000))

attr(kinds, "subheadings") <- paste0("Panel ", c("A", "B"), ":", c("Lower House", "Upper House"))

kinds <- xtableList(kinds, 
                    caption = "Wealth Distribution over Time (1000 Guilders)",
                    label = "tab:ginicoef",
                    digits = c(0,0,2,1,1,1,3,3))

print.xtableList(kinds, 
                 file = "./Tables/ginicoef.tex",
                 colnames.format = "multiple", 
                 size = "footnotesize",
                 include.rownames = F)

