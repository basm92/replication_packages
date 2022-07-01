kabinetten <- data.frame(namegovt = c(
    "Thorbecke III",
    "De Vries/Fransen van de Putte", 
    "Heemskerk/Van Lynden van Sandenburg",
    "Kappeyne van de Coppello",
    "Van Lynden van Sandenburg",
    "Heemskerk Azn.",
    "Mackay",
    "Van Tienhoven",
    "Röell",
    "Pierson",
    "Kuyper",
    "De Meester",
    "Heemskerk",
    "Cort van der Linden",
    "Ruijs de Beerenbrouck I"
    ), 
    
    arrival = c(
        "24 november 1870",
        "25 juni 1872",  
        "27 augustus 1874",
        "3 november 1877",
        "20 augustus 1879",
        "23 april 1883",
        "21 april 1888",
        "21 augustus 1891",
        "9 mei 1894",
        "27 juli 1897",
        "1 augustus 1901",
        "17 augustus 1905",
        "12 februari 1908",
        "29 augustus 1913",
        "9 september 1918"
        ),
    
    resign = c(
        "24 juni 1872",
        "26 augustus 1874",
        "2 november 1877",
        "19 augustus 1879",
        "22 april 1883",
        "20 april 1888",
        "20 augustus 1891",
        "8 mei 1894",
        "26 juli 1897",
        "31 juli 1901",
        "16 augustus 1905",
        "11 februari 1908",
        "28 augustus 1913",
        "8 september 1918",
        "18 juli 1922"
        )
) %>%
    mutate(arrival = str_replace_all(arrival, 
                                     c("juni" ="june", 
                                       "augustus" = "august",
                                       "mei" = "may",
                                       "juli" = "july",
                                       "februari"="february")),
           resign = str_replace_all(resign, 
                                    c("juni" ="june", 
                                      "augustus" = "august",
                                      "mei" = "may",
                                      "juli" = "july",
                                      "februari"="february")))

write_csv(kabinetten,"./Data/kabinetten.csv")
