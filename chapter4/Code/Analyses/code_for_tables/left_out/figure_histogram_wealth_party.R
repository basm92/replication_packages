# table_withinparty_flexbandwidth
library(readxl); library(tidyverse); library(hrbrthemes); 
library(rdrobust); library(modelsummary); library(ggtext); library(RATest); library(lubridate); library(extraDistr)

source("./Code/Analyses/functions_for_tables.R")
# Parameters

ihs <- function(x) {log(x + sqrt(x^2 + 1))}
## Few mutations with the data set
dataset <- read_delim("./Data/analysis/unmatched_sample_with_vars.csv", delim=",") %>% # %>% #"./Data/analysis/unmatched_sample_with_vars.csv") %>%
  select(-1) %>%
  mutate(defw = log(1+Vermogen_deflated),
         defw2 = ihs(Vermogen_deflated),
         distrverk = str_c(District, "-", Verkiezingdatum),
         lifespan = lifespan/365,
         politician_dummy = if_else(!is.na(`b1-nummer`), 1, 0),
         politician_indic = if_else(!is.na(`b1-nummer`), "Politician", "Non-Politician"),
         taxespercap_1859 = if_else(is.na(taxespercap_1859), 0.5, taxespercap_1859),
         taxespercap_1889 = if_else(is.na(taxespercap_1889), 0.5, taxespercap_1889),
         district_share_prot = district_prot / (district_prot + district_cath + district_ov),
         district_share_cath = district_cath / (district_prot + district_cath + district_ov),
         rec_ar = case_when(stringr::str_detect(party_election, "AR|VA|NC|NH") ~ 1, 
                            is.na(party_election) ~ 0,
                            TRUE~  0),
         rec_kath = case_when(stringr::str_detect(party_election, "Ka|KD|DT") ~ 1,
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
  ) %>%
  filter(!is.na(defw2)) 


# show the rd coefficient of variable on margin
far <- 0.2
close <- 0.05 

prot <- dataset %>%
  filter(rec_ar == 1) %>%
  mutate(politician_dummy = factor(politician_dummy)) %>%
  ggplot(aes(x = defw, group = politician_dummy, color = politician_dummy)) + 
  geom_density(alpha = 0.2)  +
  theme_bw() +
  theme(legend.position="none")+
  labs(title = "Protestant", color = "Status", x = "Log (Wealth)", y = "Density") +
  scale_color_manual(labels=c("Non-Politicians", "Politicians"), values=c("blue", "darkred"))

cath <- dataset %>%
  filter(rec_kath == 1) %>%
  mutate(politician_dummy = factor(politician_dummy)) %>%
  ggplot(aes(x = defw, group = politician_dummy, color = politician_dummy)) + 
  geom_density(alpha = 0.2) +
  theme_bw() +
  theme(legend.position="none") + 
  labs(title = "Catholic", color = "Status", x = "Log (Wealth)", y = "Density") +
  scale_color_manual(labels=c("Non-Politicians", "Politicians"), values=c("blue", "darkred"))

lib <- dataset %>%
  filter(rec_lib == 1) %>%
  mutate(politician_dummy = factor(politician_dummy)) %>%
  ggplot(aes(x = defw, group = politician_dummy, color = politician_dummy)) + 
  geom_density(alpha = 0.2) +
  theme_bw() +
  labs(title = "Liberal", color = "Status", x = "Log (Wealth)", y = "Density") +
  scale_color_manual(labels=c("Non-Politicians", "Politicians"), values=c("blue", "darkred"))

out <- cowplot::plot_grid(prot, cath, lib, nrow = 1, rel_widths = c(1,1,1.25))

cowplot::save_plot("./Tables/histogram_wealth_party.pdf", out, base_width = 15, base_height = 5)
