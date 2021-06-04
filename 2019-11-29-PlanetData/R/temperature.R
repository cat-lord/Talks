# Temperature #

# Source: Met Office Hadley Centre and Climatic Research Unit
# https://www.metoffice.gov.uk/hadobs/hadcrut4

library(tidyverse) ; library(lubridate)
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

df <- read_csv("https://www.eea.europa.eu/data-and-maps/daviz/global-average-air-temperature-anomalies-5/download.csv") %>% 
  filter(`Type:text` == "Global annual") %>% 
  select(year = 1, median = 4, lower = 3, upper = 2) %>%
  mutate(year = ymd(str_c(year, "01-01", sep = "-"))) %>% 
  arrange(year)

ggplot(data = df, aes(x = year, y = median)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 2, alpha = 0.3) +
  geom_line() +
  geom_hline(yintercept = 0, size = 1, colour = "#333333") +
  scale_fill_manual( values = c("#771711", "#316C8D")) + 
  scale_x_date(expand = c(0,0)) +
  scale_y_continuous(name = NULL, breaks = c(0, 1.5, 1, 2),
                     labels = c("0°C", "1.5°C", "1.0°C", "2.0°C"), 
                     limits = c(-0.5, 2.25),
                     position = "right",
                     expand = c(0,0)) +
  labs(x = NULL, 
       title = "Global temperature change, 1850-2018",
       subtitle = "Deviation from 1850-1900 average",
       caption = "Source: Met Office Hadley Centre and Climatic Research Unit") +
  theme_lab() +
  theme(plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(angle = 90, hjust = 1))

ggsave("outputs/temperature.png", scale = 1, dpi = 300) 
