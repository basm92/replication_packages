# Table between wealth according to pol affil and the parliamentary composition
# Goal: compare politicians to the rest of the population
library(readxl)
library(tidyverse)

wealthdistr <- read_xlsx("./Data/WealthTax (in Numbers) 20200606.xlsx", sheet = 1) %>%
  select(1:20) %>%
  mutate(across(everything(), ~ as.character(.x))) %>%
  pivot_longer(-1) %>%
  rename("year" = "...1")

where_am_i_in_dist <- function(wealthnumber, arg_year){
  
  distr <- vector(length = length(wealthnumber))
  
  for(i in 1:length(wealthnumber)){
    
    if(wealthnumber[i] < 13000){
      
      distr[i] <- 0
      
    } else{
      
  #get the correct distribution
  distribution <- wealthdistr %>%
    filter(year == arg_year) %>%
    mutate(max_wealth = stringr::str_extract(name, "/(\\d+)"), 
           max_wealth = as.numeric(str_extract(max_wealth, "\\d+")),
           value = as.numeric(value))
  
  #get the wealth bracket in which it belongs
  row <- which.min(abs(wealthnumber[i] - distribution$max_wealth))
  
  #separate procedure for if row == 1, continue with the rest
  if(row == 1) {
    total <- distribution %>%
      filter(name == "TOTAL") %>%
      pull(value)
    
    distr[i] <- ((wealthnumber[i]/15000)*15000)/total
    
    next
  }
  
  if((wealthnumber[i] - distribution$max_wealth[row]) > 0){
    row <- row
  }  else {
    row <- row-1
  }
  
  #get all the people who are at least as rich
  howmany <- sum(distribution$value[1:row])
  
  #linearly interpolate the bracket
  percent<- (wealthnumber[i] - distribution$max_wealth[row]) / 
    (distribution$max_wealth[row+1] - distribution$max_wealth[row])
  
  #quantile_in_distr 
  total <- distribution %>%
    filter(name == "TOTAL") %>%
    pull(value)
  
  distr[i] <- (howmany + percent*distribution$value[row+1]) / total

    }
    
  }
  
  distr
  
}

# It works
test <- c(47640, 13010, 17010, 800001)

where_am_i_in_dist(test, 1900)

# Now, get the data from step7_table_wealth_function.R 
# And 'convert' the numbers to a wealth distribution
lh <- read_csv("./Data/comp_with_pop_1.csv") %>%
  group_by(`Political Affiliation`) %>%
  summarize(
    across(
      c(Mean, p50, p25, p75), 
      ~ where_am_i_in_dist(.x, 1900), 
      .names = "{.col}"),
    n = N)

uh <- read_csv("./Data/comp_with_pop_2.csv") %>%
  group_by(`Political Affiliation`) %>%
  summarize(
    across(
      c(Mean, p50, p25, p75), 
      ~ where_am_i_in_dist(.x, 1900), 
      .names = "{.col}"),
    n = N)

min <- read_csv("./Data/comp_with_pop_3.csv") %>%
  group_by(`Political Affiliation`) %>%
  summarize(
    across(
      c(Mean, p50, p25, p75), 
      ~ where_am_i_in_dist(.x, 1900), 
      .names = "{.col}"),
    n = N)

dep <- read_csv("./Data/comp_with_pop_4.csv") %>%
  group_by(`Political Affiliation`) %>%
  summarize(
    across(
      c(Mean, p50, p25, p75), 
      ~ where_am_i_in_dist(.x, 1900), 
      .names = "{.col}"),
    n = N)

kinds <- list(lh, uh, min, dep)

names(kinds) <- c("Lower House", "Upper House", "Ministers", "Regional Executives")

attr(kinds, "subheadings") <- paste0("Panel ", c("A","B","C","D"),": ", names(kinds))

kinds <- xtableList(kinds, 
                    caption = "Estimates of the Place of Politicians in the Population Wealth Distribution",
                    digits = c(0,0,3,3,3,3,0),
                    label = "tab:comp_population")

print.xtableList(kinds, 
                 colnames.format = "multiple", 
                 include.rownames = F,
                 file = "/Tables/comparison_with_pop.tex")

