# Matching the names from parliament with the names from pdc
library(tidyverse)
library(stringr)
library(readxl)
library(stringdist)
library(janitor)

upperhouse <- read.csv("./Data/upperhouse_clean.csv")
lowerhouse <- read.csv("./Data/lowerhouse_clean.csv")

lowerhouse_pdc <- read_xlsx("./Data/tk_1815tot1950uu.xlsx") %>%
  clean_names()

upperhouse_pdc <- read_xlsx("./Data/ek_1815tot1950_uu.xlsx") %>%
  clean_names()

merge_names <- function(uplow_clean, pdc_data){
  
  pdc_data <- pdc_data %>%
    mutate(begin_periode = lubridate::ymd(begin_periode),
           einde_periode = lubridate::ymd(einde_periode))
  
  matched <- list()
  z <- 0
  
  for(i in unique(uplow_clean$Parliament)){
    z <- z+1
    # Here do everything
    #Split the numbers
    numbers <- stringr::str_split(i, "-") %>%
      magrittr::extract2(1)
    
    #Make them into dates
    numbers <- lubridate::ymd(paste0(numbers, c("-01-01", "-12-31")))
    
    #Filter the pdc data conditioned on these limit dates
    query <- pdc_data %>%
      filter(begin_periode < numbers[2], einde_periode > numbers[1])
    
    #Filter the uplow_clean data on i
    cand <- uplow_clean %>%
      filter(Parliament == i)
    
    #Match both of them together
    
      # Step 1: Bold Levenshtein match
    matches <- stringdist::amatch(cand$Name, 
                                  str_c(
                                    query$voorna_a_m_en, 
                                    " ", 
                                    query$achternaam),
                                  maxDist = 5)
    
    step1 <- cbind(cand, query[matches,]) %>%
      as_tibble()
    
      # Step 2: Use a last name + colloq. first name only for the NA's
    for (j in 1:nrow(step1)) {
      if(!is.na(step1[j,]$b1_nummer)) {
        next } else {
          
          match <- amatch(step1[j,]$Name, 
                 str_c(query$roepnaam, " ", query$achternaam),
                 maxDist = 5)
          
          step1[j,] <- cbind(cand[j,], query[match,])
          
        }
      
    }
    
    # Step 3: Use only the last name and the first of full name of 
    # the cand string to match
    for (k in 1:nrow(step1)) {
      if(!is.na(step1[k,]$b1_nummer)) {
        next } else {
          
          voornaam <- stringr::str_split(query$voorna_a_m_en, " ") %>%
            lapply(magrittr::extract, 1) %>%
            map_chr(rbind)
          
          achternaam <- query$achternaam
          
          voorachternaam <- stringr::str_c(voornaam, " ", achternaam)
          
          match <- amatch(step1[k,]$Name, 
                          voorachternaam,
                          maxDist = 10)
          
          step1[k,] <- cbind(cand[k,], query[match,])
          
        }
      
    }
    
    # Step 4: remove "van", "van der", "van den", "van de"
    for (m in 1:nrow(step1)) {
      if(!is.na(step1[m,]$b1_nummer)) {
        next } else {
          
          corname <- stringr::str_replace_all(step1[m,]$Name, 
                                          "van |van der |van den |van de |der ",
                                          "")
          
          match <- stringdist::amatch(
            corname, 
            str_c(query$voorna_a_m_en, " ", query$achternaam),
            maxDist = 10)
          
          step1[m,] <- cbind(cand[m,], query[match,])
          
        }
      
    }
    
    # Put everything in a list
    
    matched[[z]] <- step1
    
    }
    
  #Now concatenate everything and remove NA's (mistake in wiki)
  purrr::reduce(matched, rbind) %>%
    filter(!is.na(b1_nummer))
    
  }


# write csv's
merge_names(lowerhouse, lowerhouse_pdc) -> lhparliaments
merge_names(upperhouse, upperhouse_pdc) -> uhparliaments
  
write_csv(lhparliaments, "./Data/lh_parliaments.csv")
write_csv(uhparliaments, "./Data/uh_parliaments.csv")
