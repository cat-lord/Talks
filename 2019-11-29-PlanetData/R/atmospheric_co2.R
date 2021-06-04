# Atmospheric CO2 #

# National Oceanic and Atmospheric Administration's Earth System Research Laboratory
# https://www.esrl.noaa.gov/gmd/ccgg/trends

library(tidyverse) ; library(lubridate)
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

df <- read.table("ftp://aftp.cmdl.noaa.gov/products/trends/co2/co2_mm_mlo.txt") %>% 
  select(year = 1, month = 2, co2ppm = 5) %>% 
  mutate(date = ymd(str_c(year, month, "-01", sep = "-"))) %>% 
  select(date, co2ppm)

ggplot(df, aes(x = date, y = co2ppm)) +
  geom_line() +
  labs(x = NULL, y = NULL,
       title = expression(bold(paste("Monthly average atmospheric ", CO[2], " levels (ppm)"))),
       subtitle = "Mauna Loa observatory (Hawaii), 1958-2019",
       caption = "Source: National Oceanic and Atmospheric Agency ESRL") +
  scale_y_continuous(limits = c(300, max(df$co2ppm)), position = "right") +
  theme_lab() +
  theme(panel.grid.major.y = element_blank(), 
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(angle = 90, hjust = 1))

ggsave("outputs/atmospheric_co2.png", scale = 1, dpi = 300) 
