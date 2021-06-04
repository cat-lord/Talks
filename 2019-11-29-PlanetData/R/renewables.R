# Renewables #

# Source: BEIS
# https://www.gov.uk/government/statistics/renewable-sources-of-energy-chapter-6-digest-of-united-kingdom-energy-statistics-dukes
# Licence: Open Governmenty Licence 3.0

library(tidyverse) ; library(readxl) ; library(httr) ; library(lubridate) ; library(scales)
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

tmp <- tempfile(fileext = ".xls")
GET(url = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/826256/DUKES_6.7.xls",
    write_disk(tmp))

df <- read_xls(tmp, sheet = 2, skip = 4, range = "A5:P53") %>% 
  slice(48) %>% 
  gather(year, value, -1) %>% 
  select(year, value) %>% 
  mutate(year = ymd(str_c(year, "01-01", sep = "-")))

ggplot(df, aes(year, value)) +
  geom_col(fill = "#175FAF") +
  geom_hline(yintercept = 0.05, size = 0.5, colour = "#FFFFFF") +
  geom_hline(yintercept = 0.10, size = 0.5, colour = "#FFFFFF") +
  annotate(geom = "text", x = ymd(20130301), y = filter(df, year == "2018-01-01")$value,
           label = "atop(bold('11%')~of~total~energy~consumption~came~from,'renewable sources in 2018')",
           parse = TRUE, hjust = 0, vjust = -2.1, size = 3) +
  geom_curve(aes(x = ymd(20160801), y = 0.13, 
                 xend = ymd(20180101), yend = 0.11),
             colour = "#212121", curvature = -0.2, size = 0.5,
             arrow = arrow(length = unit(0.02, "npc"))) +
  geom_hline(aes(yintercept = 0.15), linetype = "dotted", size = 0.5) +
  annotate(geom = "text", x = ymd(20180101), y = 0.15, 
           label = "EU Renewable Directive 2020 target", vjust = -1, fontface = "italic") +
  geom_hline(yintercept = 0, size = 1, colour = "#333333") +
  scale_x_date(breaks = "1 year", date_labels = "%Y", expand = c(0.005, 0.005)) +
  scale_y_continuous(limits = c(0, 0.16), labels = percent_format(1), expand = c(0.005, 0.005), position = "right") +
  labs(x = NULL, y = NULL,
       title = "Share of energy from renewable sources",
       subtitle = "United Kingdom, 2004-2018",
       caption = "Source: BEIS") +
  theme_lab() +
  theme(panel.grid.major.y = element_blank(), 
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 12)) +
  expand_limits(x = ymd(20200601))

ggsave("outputs/renewables.png", scale = 1, dpi = 300)  
