# Recycling #

# Source: Department for Environment, Food & Rural Affairs
# URL: https://www.gov.uk/government/statistical-data-sets/env23-uk-waste-data-and-management
# Licence: Open Government Licence v.3.0

library(tidyverse) ; library(readxl) ; library(httr) ; library(lubridate) ; library(scales)
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

tmp <- tempfile(fileext = ".xlsx")
GET(url = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/784273/UK_Statistics_on_Waste_dataset_Mar_2019_rev_FINAL.xlsx",
    write_disk(tmp))

df <- read_xlsx(tmp, sheet = 4, range = "A8:C38") %>% 
  filter(Measure == "Recycling rate (excl. IBAm)") %>% 
  mutate(year = ymd(str_c(Year, "01-01", sep = "-"))) %>% 
  select(year, value = UK)

ggplot(df, aes(x = year, y = value)) +
  geom_col(fill = "#A2516A") +
  geom_hline(yintercept = 0, size = 1, colour = "#333333") +
  geom_hline(yintercept = 0.1, size = 0.5, colour = "#FFFFFF") +
  geom_hline(yintercept = 0.2, size = 0.5, colour = "#FFFFFF") +
  geom_hline(yintercept = 0.3, size = 0.5, colour = "#FFFFFF") +
  geom_hline(yintercept = 0.4, size = 0.5, colour = "#FFFFFF") +
  geom_hline(aes(yintercept = 0.5), linetype = "dotted", size = 0.5) +
  annotate(geom = "text", x = ymd(20180201), y = filter(df, year == ymd(20170101))$value-0.01,
           label = "atop(bold('45%')~recycling~rate~'in'~bold('2017'))",
           parse = TRUE, hjust = 0, size = 3) +
  geom_segment(aes(x = ymd(20180101), y = filter(df, year == ymd(20170101))$value, 
                   xend = ymd(20170701), yend = filter(df, year == ymd(20170101))$value), 
               colour = "#212121", size = 0.5,
               arrow = arrow(length = unit(0.02, "npc"))) +
  annotate(geom = "text", x = ymd(20170301), y = 0.5, 
           label = "EU Waste Framework Directive 2020 target", hjust = 0, vjust = -1, fontface = "italic") +
  scale_x_date(expand = c(0.005, 0.005)) +
  scale_y_continuous(expand = c(0.005, 0.005), labels = percent_format(1), position = "right") +
  labs(x = NULL, y = NULL,
       title = "Recycling rates",
       subtitle = "United Kingdom, 2010-2017",
       caption = "Source: DEFRA") +
  theme_lab() +
  theme(panel.grid.major.y = element_blank(), 
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 12)) +
  expand_limits(x = ymd(20200601), y = c(0,0.55))

ggsave("outputs/recycling.png", scale = 1, dpi = 300)  

