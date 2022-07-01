# read wealth and lh
library(readxl)
library(tidyverse)
library(janitor)
library(cowplot)
source("./Code/classify.R")


wealth <- read_xlsx("./Data/AnalysisFile.xlsx", sheet = "Analysis") %>%
  clean_names()

lh_parliaments <- read_csv("./Data/lh_parliaments.csv") %>%
  clean_names() %>%
  select(-1)

lh_parliaments <- left_join(lh_parliaments, wealth,
                            by = c("b1_nummer" = "indexnummer"))

lh_parliaments <- classify(lh_parliaments)

meanmedw_lh <- lh_parliaments %>%
  group_by(parliament, class) %>%
  summarize(median = median(w_deflated, na.rm = T))

p1 <- meanmedw_lh %>%
  filter(class != "neutral") %>%
  pivot_longer(median,
               names_to = "Statistic", 
               values_to = "Wealth") %>%
  ggplot(aes(x = parliament, 
             y = Wealth, 
             group = class, 
             linetype = class)) + 
  geom_line() + 
  theme_minimal() + 
  xlab("Parliament") +
  ylab("Wealth (guilders)") +
  theme(axis.text.x = element_text(angle = 45),
        text = element_text(size = 13),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  ggtitle("Panel A: Lower House") + 
  scale_y_continuous(labels = scales::number_format(accuracy = 1),
                     limits=c(0,8e5)) +
  scale_color_manual(values = c("blue", "orange", "red")) 

p1 <- p1 + theme(legend.position = c(0.9, 0.85)) + 
  guides(linetype=guide_legend(title="Political Color"))

#now, read uh
uh_parliaments <- read_csv("./Data/uh_parliaments.csv") %>%
  clean_names() %>%
  select(-1)

uh_parliaments <- left_join(uh_parliaments, wealth,
                            by = c("b1_nummer" = "indexnummer"))


uh_parliaments <- classify(uh_parliaments)


meanmedw_uh <- uh_parliaments %>%
  group_by(parliament, class) %>%
  summarize(median = median(w_deflated, na.rm = T))

p2 <- meanmedw_uh %>%
  pivot_longer(median,
               names_to = "Statistic", 
               values_to = "Wealth") %>%
  ggplot(aes(x = parliament, 
             y = Wealth, 
             group = class, 
             linetype = class)) + 
  geom_line() + 
  theme_minimal() + 
  xlab("Parliament") +
  ylab("Wealth (guilders)") +
  theme(axis.text.x = element_text(angle = 45)) +
  ggtitle("Panel B: Upper House") + 
  scale_y_continuous(labels = scales::number_format(accuracy = 1),
                     limits = c(0, 8e5)) +
  theme(legend.position = c(0.9, 0.9),
        text = element_text(size = 13),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) + 
  scale_color_manual(values = c("blue", "orange", "red")) +
  guides(linetype=guide_legend(title="Political Color"))


fig <- cowplot::plot_grid(p1, p2, nrow = 2, rel_heights = c(45,55))

ggsave("./Figures/step8fig2wealthperparlperparty.png", fig, width = 12, height = 8)

