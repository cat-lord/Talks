# PM 2.5 #

# Source: European Environment Agency
# https://www.eea.europa.eu/data-and-maps/dashboards/necd-directive-data-viewer-2

library(tidyverse) ; library(lubridate)
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

actual <- read_csv("data/PM2_5.csv") %>% 
  mutate(year = ymd(str_c(Year, "01-01", sep = "-")),
         group = "actual") %>% 
  select(year, value = Emissions, group) %>% 
  filter(year >= "2005-01-01")

projected <- tibble(
  year = c(ymd("20200101"), ymd("20250101"), ymd("20300101")),
  value = c(98.2100, 94.5800, 93.2000),
  group = c(rep("projected",3))
)

df <- bind_rows(actual, projected) %>%
  mutate(index = round(100 * value / value[1], 0),
         change = index-100)

# 2020 NECD commitment
round(100 * 87.04 / filter(df, year == "2005-01-01")$value[1], 0)
# 2030 NECD commitment
round(100 * 67.14 / filter(df, year == "2005-01-01")$value[1], 0)

ggplot(df, aes(year, index)) +
  geom_col(fill = ifelse(df$group == "actual", "#E25E6A", "#dddddd")) +
  geom_hline(yintercept = 25, size = 0.5, colour = "#FFFFFF") +
  geom_hline(yintercept = 50, size = 0.5, colour = "#FFFFFF") +
  geom_hline(yintercept = 75, size = 0.5, colour = "#FFFFFF") +
  geom_hline(yintercept = 69, size = 0.5, linetype = "dotted") +
  geom_hline(yintercept = 53, size = 0.5, linetype = "dotted") +
  annotate(geom = "text", x = ymd(20260101), y = 69, label = "2020 NECD emission reduction commitment", vjust = -0.6, fontface = "italic") +
  annotate(geom = "text", x = ymd(20260101), y = 53, label = "2030 NECD emission reduction commitment", vjust = -0.6, fontface = "italic") +
  geom_hline(yintercept = 0, size = 1, colour = "#212121", linetype = "solid") +
  scale_x_date(breaks = "1 year", date_labels = "%Y", expand = c(0.005, 0.005)) +
  scale_y_continuous(limits = c(0, 110), expand = c(0.005, 0.005),
                     position = "right",
                     breaks = c(54, 70),
                     labels = c("-46%", "-30%")) +
  labs(x = NULL, y = NULL,
       title = expression(bold(paste("Change in total ", PM[2.5], " emissions (Gg) since 2005 with projections"))),
       subtitle = "United Kingdom, 2005-2030",
       caption = "Source: European Environment Agency") +
  theme_lab() +
  theme(panel.grid.major.y = element_blank(), 
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  expand_limits(y = 110)

ggsave("outputs/pm2_5.png", scale = 1, dpi = 300) 
