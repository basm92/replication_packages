#function classify
classify <- function(df){
  df %>%
    mutate(class = ifelse(
  str_detect(partij_en_fractie_s, ".*?iber*.?|.*?vrijzin.*?"),
  "liberal",
  ""),
  class = ifelse(
    str_detect(partij_en_fractie_s,".*?athol.*?"),
    "confessional",
    class
  ),
  class = ifelse(
    str_detect(partij_en_fractie_s, ".*?onserva.*?"),
    "confessional",
    class
  ),
  class = ifelse(
    str_detect(partij_en_fractie_s, ".*?ntirevol.*?"),
    "confessional",
    class
  ),
  class = ifelse(
    str_detect(partij_en_fractie_s, ".*?AR.*?"),
    "confessional",
    class
  ),
  class = ifelse(
    str_detect(partij_en_fractie_s, ".*?CH.*?|.*?c\\.h\\..*?|.*?a\\.r\\..*?"),
    "confessional",
    class
  ),
  class = ifelse(
    str_detect(partij_en_fractie_s, "Schaepman.*?|Putt.*?|Kappey.*?|Bahlman.*?|KVP"),
    "confessional",
    class
  ),
  class = ifelse(
    str_detect(partij_en_fractie_s, "Thorb.*?|.*?VVD.*?"),
    "liberal",
    class
  ),
  class = ifelse(
    str_detect(partij_en_fractie_s, ".*?RK.*?"),
    "confessional",
    class
  ),
  class = ifelse(
    str_detect(partij_en_fractie_s, ".*?VDB.*?|.*?SDAP.*?|.*?SDB.*?|.*?Sociali.*?|Vrije Socialist|Radicale Bond"),
    "socialist",
    class
  ),
  class = ifelse(
    str_detect(partij_en_fractie_s, ".*?eutr.*?|.*?lattelan.*?|.*?partijloo.*?|MP S&L|NBTMP|VNH"),
    "neutral",
    class
  ),
  class = ifelse(
    str_detect(partij_en_fractie_s, "CSP|CPH|.*?CPN.*?|CDU|.*?PvdA.*?|RSP;RSAP|SP \\(voor 1940\\)"),
    "socialist",
    class
  ),
  class = ifelse(
    str_detect(partij_en_fractie_s, "SGP|HGSP"),
    "confessional",
    class
  ),
  class = ifelse(
    str_detect(partij_en_fractie_s, "NSB"),
    "national-socialist",
    class
  ),
  class = ifelse(
    str_detect(partij_en_fractie_s, "Volkspartij|Vrijheidsbond.*?"),
    "liberal",
    class
  ),
  class = ifelse(
    str_detect(partij_en_fractie_s, "technocraat"),
    "neutral",
    class
  )
)
}
