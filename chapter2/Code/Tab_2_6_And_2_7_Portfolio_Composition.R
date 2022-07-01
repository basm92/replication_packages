###Portfolio_composition_table
#in both panel 1, panel 2, according to political FUNCTION
#welath naar affil: eerste en tweede kamer
library(readxl)
library(tidyverse)
source("./Code/classify.R")

## Functions
get_shares <- function(df) {
  df %>%
    rowwise() %>%
    mutate(total = sum(across(re:misc), na.rm = T),
           re_share = re/total,
           shares_share = (fosh+dush)/total,
           bonds_share = (foprbo + duprbo + fogobo + dugobo)/total, 
           misc_share = (cash + misc)/total)
}

get_summary <- function(df) {
  
  if (!all(c("re_share", "shares_share") %in% names(df))) {
    stop("`df` must contain re_share and shares_share columns")
  }
  
  df %>%
    dplyr::summarize(mean_re = mean(re_share,na.rm = T),
                     mean_shares = mean(shares_share, na.rm = T),
                     mean_bonds = mean(bonds_share, na.rm =T),
                     mean_misc = mean(misc_share, na.rm = T),
                     n = sum(!is.na(re_share)))
}

append <- function(df, text) {
  if(!is.character(text)){
    stop("text must be text.")
  }

  if("class" %in% names(df)){
  df %>%
    mutate(House = text) %>%
    relocate(House, class, before_after)
  } else{
    df %>%
      mutate(House = text, class = "-") %>%
      relocate(House, class, before_after) %>%
      na.omit()
  }
  
}

append2 <- function(df, text) {
  
  if(!is.character(text)){
    stop("text must be text.")
  }
  
  df %>%
    mutate(House = text) %>%
    relocate(House, class)
  
}

      
## Data cleaning
wealth <- read_xlsx("./Data/AnalysisFile.xlsx", sheet = "Analysis") %>%
  clean_names()

wealth <- get_shares(wealth) %>%
  select(indexnummer, re_share, shares_share, bonds_share, misc_share)

#functie year
## lh
lh <- read_csv("./Data/lh_parliaments.csv") %>%
  clean_names() %>%
  classify() %>%
  distinct(b1_nummer, .keep_all = TRUE) %>%
  select(b1_nummer, begin_periode, einde_periode, class)

lh <- lh %>%
  left_join(wealth, 
            by = c("b1_nummer"="indexnummer")) %>%
  mutate(before_after = ifelse(begin_periode > "1900-01-01", "After 1900", "Before 1900"))

lh <- lh %>%
  group_by(before_after) %>%
  get_summary() %>%
  append("Lower House")

##uh
uh <- read_csv("./Data/uh_parliaments.csv") %>%
  clean_names() %>%
  classify() %>%
  distinct(b1_nummer, .keep_all = TRUE) %>%
  select(b1_nummer, begin_periode, einde_periode, class)

uh <- uh %>%
  left_join(wealth, 
            by = c("b1_nummer"="indexnummer")) %>%
  mutate(before_after = ifelse(begin_periode > "1900-01-01", "After 1900", "Before 1900"))

uh <- uh %>%
  group_by(before_after) %>%
  get_summary() %>%
  append("Upper House")

##min
min <- read_xlsx("./Data/bewindslieden_1815tot1950_uu.xlsx") %>%
  clean_names() %>%
  classify() %>%
  filter(class != "") %>%
  distinct(b1_nummer, .keep_all = TRUE) %>%
  select(b1_nummer, begin_periode, einde_periode, class)

min <- merge(min, wealth, 
      by.x = "b1_nummer",
      by.y = "indexnummer") %>%
  mutate(before_after = ifelse(begin_periode > "1900-01-01", "After 1900", "Before 1900"))

min <- min %>%
  group_by(before_after) %>%
  get_summary() %>%
  append("Ministers")

##dep
dep <- read_xlsx("./Data/AnalysisFile.xlsx", sheet = "RegistrationFile") %>%
  clean_names() %>%
  filter(grepl("C|G", index_nummer)) %>%
  select(index_nummer, beginyr, endyr) %>%
  left_join(wealth, by = c("index_nummer" = "indexnummer")) %>%
  mutate(before_after = ifelse(beginyr > "1900-01-01", "After 1900", "Before 1900"))

dep <- dep %>%
  group_by(before_after) %>%
  get_summary() %>%
  append("Provincial Executives")

table <- bind_rows(lh, uh, min, dep) 

maybetable <- table %>%
  select(-class, -n) %>% #-n is necessary for this table, otherwise no pivot
  pivot_longer(mean_re:mean_misc) %>%
  pivot_wider(names_from = c(before_after, name), values_from = value) %>%
  relocate(House, contains("Before"), contains("After"))


#functie party
lh <- read_csv("./Data/lh_parliaments.csv") %>%
  clean_names() %>%
  classify() %>%
  distinct(b1_nummer, .keep_all = TRUE) %>%
  select(b1_nummer, begin_periode, einde_periode, class)

lh <- lh %>%
  left_join(wealth, 
            by = c("b1_nummer"="indexnummer")) %>%
  mutate(before_after = ifelse(begin_periode > "1900-01-01", "After 1900", "Before 1900"))

lh <- lh %>%
  group_by(class) %>%
  get_summary() %>%
  append2("Lower House")

uh <- read_csv("./Data/uh_parliaments.csv") %>%
  clean_names() %>%
  classify() %>%
  distinct(b1_nummer, .keep_all = TRUE) %>%
  select(b1_nummer, begin_periode, einde_periode, class)

uh <- uh %>%
  left_join(wealth, 
            by = c("b1_nummer"="indexnummer")) %>%
  mutate(before_after = ifelse(begin_periode > "1900-01-01", "After 1900", "Before 1900"))

uh <- uh %>%
  group_by(class) %>%
  get_summary() %>%
  append2("Upper House")

min <- read_xlsx("./Data/bewindslieden_1815tot1950_uu.xlsx") %>%
  clean_names() %>%
  classify() %>%
  filter(class != "") %>%
  distinct(b1_nummer, .keep_all = TRUE) %>%
  select(b1_nummer, begin_periode, einde_periode, class)

min <- merge(min, wealth, 
             by.x = "b1_nummer",
             by.y = "indexnummer") %>%
  mutate(before_after = ifelse(begin_periode > "1900-01-01", "After 1900", "Before 1900"))

min <- min %>%
  group_by(class) %>%
  get_summary() %>%
  append2("Ministers")


#Now make the tables
#make a function to rewrite a variable
convert_pct <- function(vector){
  scales::label_percent(accuracy = 0.1)(vector)
}

table1_1 <- table %>%
  select(-class) %>%
  filter(before_after == "Before 1900") %>%
  select(-before_after) %>%
  mutate(across(mean_re:mean_misc, ~ convert_pct(.x)))

table1_2 <- table %>%
  select(-class) %>%
  filter(before_after == "After 1900") %>%
  select(-before_after) %>%
  mutate(across(mean_re:mean_misc, ~ convert_pct(.x)))
  
kinds <- list(table1_1, table1_2) %>%
  lapply(rename, 
         RealEstate = mean_re, 
         Stocks = mean_shares,
         Bonds = mean_bonds,
         Misc = mean_misc,
         N = n)

attr(kinds, "subheadings") <- paste0("Panel ", 
                                     c("A","B"),
                                     ": ",
                                     c("Before 1900", "After 1900")
                                     )

kinds <- xtable::xtableList(kinds, 
                   caption = "Mean Portfolio Shares Before and After 1900",
                   label = "fig:portcomp1")

print.xtableList(kinds,
                 colnames.format = "multiple", 
                 include.rownames = F)


table2 <- bind_rows(lh, uh, min)

table2 <- table2 %>%
  rename(Party = class,
    RealEstate = mean_re, 
         Stocks = mean_shares,
         Bonds = mean_bonds,
         Misc = mean_misc,
         N = n) %>%
  mutate(across(RealEstate:Misc, ~ convert_pct(.x)))

table2 <- xtable(table2, 
                 caption = "Portfolio Share according to Political Color and Organ",
                 label = "fig:portcomp2") 


print.xtable(table2, file = "./Tables/portcomp_2.tex", 
             size = "small",
             include.rownames = FALSE)

#kable(table2, format = "latex", booktabs = T) %>%
#  kable_styling() %>%
#  add_footnote("Footnote 1")

#library(kableExtra)
