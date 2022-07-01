# Function for ministers
## Function is self-sufficient, imports everything
get_wealth_kabinet <- function(precision = 50) {
#For Loop over different cabinets with:
    kabinetten <- read_csv("./Data/kabinetten.csv")
    ministers <- read_xlsx("./Data/bewindslieden_1815tot1950_uu.xlsx", sheet = 2)%>%
        filter(grepl("^minister van", waarde) | grepl("voorzitter van de ministerraad", waarde)) %>%
        separate(col = datum,into = c("begin", "end"), sep = "/") %>%
        mutate(across(c(begin, end), lubridate::dmy))
    
    #make a list to store results
    results <- vector("list", length(kabinetten$namegovt))
    
    for(i in 1:length(kabinetten$namegovt)){
        
        begindate <- lubridate::dmy(kabinetten$arrival[i])
        enddate <- lubridate::dmy(kabinetten$resign[i])
    # Start with professions (sheet 2)
        #Filter on date
        #Extract the b1_nummers
        query <- ministers %>%
            filter(end >= begindate+precision, begin <= enddate-precision)
        #Filter wealth dataset on indexnummer %in% b1 nummer
        wealth <- read_xlsx("./Data/AnalysisFile.xlsx", sheet = "Analysis") %>%
            filter(Indexnummer %in% query$`b1-nummer`)
        
        query <- left_join(query, wealth, by = c("b1-nummer" = "Indexnummer"))
        
        results[[i]] <- query %>%
            mutate(govt = kabinetten$namegovt[i]) %>%
            distinct(`b1-nummer`, .keep_all = T)

    # Summarize and compute avg. and median wealth
    }
    
    #combine everything
    purrr::reduce(results, rbind) %>%
        mutate(govt = as.factor(govt)) %>%
        janitor::clean_names()
    
}

#Get the data
get_wealth_kabinet() -> wealthreg

#Preserve factor level
wealthreg$govt <- factor(wealthreg$govt, levels = unique(wealthreg$govt))

#Summarize and clean
wealthreg <- wealthreg %>%
    group_by(govt) %>%
    summarize(mean = mean(w_deflated, na.rm = T), 
              median = median(w_deflated, na.rm = T),
              sd = sd(w_deflated, na.rm = T),
              count = sum(!is.na(w_deflated))) %>%
    pivot_longer(mean:sd, 
                 names_to = "Statistic", 
                 values_to = "Wealth") 

## Figure
p1 <- wealthreg %>%
    filter(Statistic != "sd") %>%
    ggplot(aes(x = govt, 
               y = Wealth, 
               group = Statistic, 
               fill = Statistic)) + 
    geom_bar(position = "dodge", stat = "identity") + 
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, 
                                     hjust = 1,
                                     size = 7)) +
    xlab("Government") + 
    ggtitle("Average Wealth per Government") +
    scale_y_continuous(labels = scales::number_format(accuracy = 1),
                       limits = c(0, 8e5))

p1
ggsave("./Figures/grafiekwealthmin.png", p1)
