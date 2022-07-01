#election dates
electiondates <- data.frame(date = c(
"13 june 1871",
"8 june 1875",
"10 june 1879",
"12 june 1883",
"28 october 1884",
"15 june 1886",
"1 september 1887",
"6 march 1888",
"9 june 1891",
"10 april 1894",
"15 june 1897",
"27 june 1901",
"16 june 1905",
"11 june 1909",
"17 june 1913",
"5 june 1917",
"3 july 1918"),
parl = c("1871-1875",
            "1875-1879",
            "1879-1883",
            "1883-1884",
            "1884-1886",
            "1886-1887",
            "1887-1888",
            "1888-1891",
            "1891-1894",
            "1894-1897",
            "1897-1901",
            "1901-1905",
            "1905-1909",
            "1909-1913",
            "1913-1917",
            "1917-1918",
            "1918-1922")
)

electiondates <- electiondates %>%
  mutate(date = dmy(date))

write_csv(electiondates, "./Data/electiondates.csv")
