# table to compare harnas with non-harnas politicians
wealth <- read_xlsx("./Data/AnalysisFile.xlsx", sheet = "Analysis") %>%
  clean_names()

#lh 
sterfdata_lh <- read_xlsx("./Data/tk_1815tot1950uu.xlsx", sheet = 2) %>%
  filter(rubriek == "3020" | rubriek == "3010") %>%
  pivot_wider(id_cols = `b1-nummer`, 
              names_from = rubriek, 
              values_from = datum,
              values_fn = last) %>%
  rename(gebdat = `3010`, stedat = `3020`) %>%
  mutate(aod = as.numeric(dmy(stedat) - dmy(gebdat)) /365)

harnas_lh <- read_xlsx("./Data/tk_1815tot1950uu.xlsx") %>%
  merge(sterfdata_lh) %>%
  as_tibble() %>%
  left_join(wealth, by = c("b1-nummer" = "indexnummer")) %>%
  mutate(`einde periode` = ymd(`einde periode`)) %>%
  mutate(harnas = dmy(stedat) - `einde periode` < 2) %>%
  select(achternaam, `begin periode`, `einde periode`, `aod`, `w_deflated`, `harnas`)
  

#uh
sterfdata_uh <- read_xlsx("./Data/ek_1815tot1950_uu.xlsx", sheet = 2) %>%
  filter(rubriek == "3020" | rubriek == "3010") %>%
  pivot_wider(id_cols = `b1-nummer`, 
              names_from = rubriek, 
              values_from = datum,
              values_fn = last) %>%
  rename(gebdat = `3010`, stedat = `3020`) %>%
  mutate(aod = as.numeric(dmy(stedat) - dmy(gebdat)) /365)

harnas_uh <- read_xlsx("./Data/ek_1815tot1950_uu.xlsx") %>%
  merge(sterfdata_uh) %>%
  as_tibble() %>%
  left_join(wealth, by = c("b1-nummer" = "indexnummer")) %>%
  mutate(`einde periode` = ymd(`einde periode`)) %>%
  mutate(harnas = dmy(stedat) - `einde periode` < 2) %>%
  select(achternaam, `begin periode`, `einde periode`, `aod`, `w_deflated`, `harnas`)


#min
sterfdata_ministers <- read_xlsx("./Data/bewindslieden_1815tot1950_uu.xlsx", sheet = 2) %>%
  filter(rubriek == "3020" | rubriek == "3010") %>%
  pivot_wider(id_cols = `b1-nummer`, 
              names_from = rubriek, 
              values_from = datum,
              values_fn = last) %>%
  rename(gebdat = `3010`, stedat = `3020`) %>%
  mutate(aod = as.numeric(dmy(stedat) - dmy(gebdat)) /365)

harnas_min <- read_xlsx("./Data/bewindslieden_1815tot1950_uu.xlsx") %>%
  merge(sterfdata_ministers) %>%
  as_tibble() %>%
  left_join(wealth, by = c("b1-nummer" = "indexnummer")) %>%
  mutate(`einde periode` = ymd(`einde periode`)) %>%
  mutate(harnas = dmy(stedat) - `einde periode` < 2) %>%
  select(achternaam, `begin periode`, `einde periode`, `aod`, `w_deflated`, `harnas`)

#Dep and clean
gedeputeerden <- wealth %>%
  filter(grepl("G|C", indexnummer))

deps <- read_xlsx("./Data/AnalysisFile.xlsx") %>%
  filter(grepl("G|C", `Index-nummer`))

harnas_dep <- left_join(deps, gedeputeerden, by = c("Index-nummer" = "indexnummer")) %>%
  mutate(`jaar van overlijden` = as.numeric(`jaar van overlijden`),
         endyr = as.numeric(endyr), 
          harnas = `jaar van overlijden` - endyr < 2) %>%
  select(achternaam, beginyr, endyr, `jaar van overlijden`, `w_deflated`, harnas)

# summarize and report
harnas_lh_sum <- harnas_lh %>%
  group_by(harnas) %>%
  filter(!is.na(harnas)) %>%
  summarize(Mean = mean(w_deflated, na.rm = T), 
            Median = median(w_deflated, na.rm = T), 
            SD = sd(w_deflated, na.rm = T),
            aod = round(mean(aod, na.rm = T), 1)) %>%
  mutate(harnas = if_else(harnas == FALSE, "> 2 Year", "< 2 Year"))

harnas_uh_sum <- harnas_uh %>%
  group_by(harnas) %>%
  filter(!is.na(harnas)) %>%
  summarize(Mean = mean(w_deflated, na.rm = T), 
            Median = median(w_deflated, na.rm = T), 
            SD = sd(w_deflated, na.rm = T),
            aod = round(mean(aod, na.rm = T), 1)) %>%
  mutate(harnas = if_else(harnas == FALSE, "> 2 Year", "< 2 Year"))

harnas_min_sum <- harnas_min %>%
  group_by(harnas) %>%
  filter(!is.na(harnas)) %>%
  summarize(Mean = mean(w_deflated, na.rm = T), 
            Median = median(w_deflated, na.rm = T), 
            SD = sd(w_deflated, na.rm = T),
            aod = round(mean(aod, na.rm = T), 1)) %>%
  mutate(harnas = if_else(harnas == FALSE, "> 2 Year", "< 2 Year"))

harnas_dep_sum <- harnas_dep %>%
  group_by(harnas) %>%
  filter(!is.na(harnas)) %>%
  summarize(Mean = mean(w_deflated, na.rm = T), 
            Median = median(w_deflated, na.rm = T), 
            SD = sd(w_deflated, na.rm = T)) %>%
mutate(harnas = if_else(harnas == FALSE, "> 2 Year", "< 2 Year"), aod = "") %>%
  relocate(harnas, Mean, Median, SD, aod)

kinds <- list(harnas_lh_sum, 
              harnas_uh_sum, 
              harnas_min_sum, 
              harnas_dep_sum)

kinds <- kinds %>%
  lapply(rename, Harnas = harnas, AoD = aod) %>%
  lapply(mutate, across(Mean:SD, ~ .x / 1000))


names(kinds) <- c("Lower House", "Upper House", "Ministers", "Regional Executives")
attr(kinds, "subheadings") <- paste0("Panel ", c("A","B","C","D"),": ", names(kinds))

kinds <- xtableList(kinds, 
                    caption = "Wealth according to having died shortly after leaving office",
                    digits = c(0,0,1,1,1,0),
                    label = "tab:harnas")

print.xtableList(kinds, 
                 colnames.format = "multiple", 
                 include.rownames = F)

#\floatfoot{This table shows the average wealth (in 1000 guilders) of politicians conditional on having died two years after leaving office (indicated by \textit{Harnas}), as well as its standard deviation, median, and the average age of death (AoD), to compare whether politicians that died recently after leaving office died on average earlier than politicians who died later after leaving office. The age of death of regional executives is not generally known.}