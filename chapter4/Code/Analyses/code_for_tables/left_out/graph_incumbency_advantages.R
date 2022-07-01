library(readxl); library(tidyverse); library(hrbrthemes); library(rdrobust); library(modelsummary)
library(ggtext); library(RATest); library(lubridate); library(extraDistr)

# inverse hyperbolic sine transform
ihs <- function(x) {log(x + sqrt(x^2 + 1))}
## Few mutations with the data set
dataset <- read_delim("./Data/analysis/full_sample_analysis_novars.csv", delim=",") %>%
  select(-1) %>%
  janitor::clean_names() %>%
  mutate(defw = log(1+deflated_wealth), defw2 = ihs(deflated_wealth)) %>%
  mutate(across(starts_with("verk_"), ~ if_else(.x > 1, 1, .x))) %>% 
  filter(consequential_election == 1)

## Incumbency advantages
create_graph <- function(howmany){
  out <- data.frame(incumbency_advantage = 1:howmany,
                        ci_low = vector(length = howmany),
                        ci_high = vector(length = howmany),
                        coef = vector(length = howmany),
                    n = vector(length = howmany))
  
  for(i in 1:howmany){
    depvar <- paste("verk_", i+1, "_gewonnen", sep = "")
    reg <- rdrobust::rdrobust(y = dataset[[depvar]], x = dataset[['margin']])
    
    out[i, 'coef'] <- reg$coef[1]
    out[i, 'ci_low'] <- reg$ci[3]
    out[i, 'ci_high'] <- reg$ci[6]
    out[i, 'n'] <- sum(reg$N)
    
  }
  
  graph <- ggplot(data = out, aes(x = incumbency_advantage)) + 
    theme_bw() +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high), color='dark blue', width = 0.1) +
    geom_hline(aes(yintercept = 0), lty=3) +
    geom_point(aes(y = coef), color = 'red') + 
    xlab("Incumbency Advantage for x Elections") +
    ylab("Coefficient") +
    scale_x_continuous(breaks=seq(1, howmany, 1)) +
    geom_text(aes(x = incumbency_advantage + 0.1, y = 0.25, label = paste("N =", n)))
  
  return(list(out, graph))
  
}

figure <- create_graph(9)[[2]]
