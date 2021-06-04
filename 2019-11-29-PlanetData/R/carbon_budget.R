# Carbon budget #

# Source: BEIS
# https://www.gov.uk/government/publications/updated-energy-and-emissions-projections-2018
# Licence: Open Government Licence 3.0

library(tidyverse) ; library(lubridate) ; library(scales)
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

df <- tibble(
  period = c("2018-22\n(3rd Carbon Budget)", "2023-27\n(4th Carbon Budget)", 
             "2028-32\n(5th Carbon Budget)"),
  Emissions = c(2456, 2059, 1890),
  Budget = c(2544,1950,1725)
) %>% 
  gather(group, value, -period) %>% 
  mutate(group = fct_relevel(factor(group), "Emissions", "Budget"))

ggplot(df, aes(x = period, y = value, fill = as.factor(group))) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = 0, size = 1, colour = "#333333") +
  scale_fill_manual(values = c("Emissions" = "#dddddd", "Budget" = "#5C65B7")) +
  scale_x_discrete(expand = c(0.005, 0.005)) +
  scale_y_continuous(expand = c(0.005, 0.005), labels = comma) +
  labs(x = NULL, 
       y = expression(paste("Carbon (", MtC0[2], "e)")),
       title = "Projected performance against carbon budgets",
       subtitle = "United Kingdom, 2008 - 2032",
       caption = "Source: BEIS",
       fill = NULL) +
  theme_lab() +
  theme(plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 12)) 

ggsave("outputs/carbon_budget.png", scale = 1, dpi = 300)  
