# UK greenhouse gas emissions #

# Source: BEIS
# https://www.gov.uk/government/statistics/provisional-uk-greenhouse-gas-emissions-national-statistics-2018
# Licence: Open Government Licence v.3.0

library(tidyverse) ; library(readxl) ; library(httr) ; library(lubridate) ; library(scales)
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

tmp <- tempfile(fileext = ".xlsx")
GET(url = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/789950/2018-provisional-emissions-data-tables.xlsx",
    write_disk(tmp))

df <- read_xlsx(tmp, sheet = 2, range = "A2:AD16") %>% 
  filter(Sector == "Total CO2") %>% 
  gather(year, value, -Sector) %>% 
  select(year, value) %>% 
  mutate(year = ymd(str_c(year, "01-01", sep = "-"))) %>% 
  mutate(index = round(100 * value / value[1], 0),
         change = index-100)

ggplot(df, aes(x = year, y = index)) +
  geom_line(colour = "#a31800", size = 1) +
  geom_point(data = filter(df, year == "1990-01-01"), pch = 21, size = 4, stroke = 1, fill = "#FFFFFF", colour = "#a31800") +
  geom_point(data = filter(df, year == "2018-01-01"), pch = 21, size = 4, stroke = 1, fill = "#FFFFFF", colour = "#a31800") +
  geom_hline(yintercept = 0, size = 1, colour = "#333333") +
  scale_x_date(expand = c(0.005, 70), date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(limits = c(0, 110), expand = c(0.005, 0.005),
                     breaks = c(0, 25, 50, 75, 100, 125),
                     labels = c("-100%", "-75%", "-50%", "-25%", "0%", "+25%"),
                     sec.axis = sec_axis(~ ., breaks = pull(filter(df, year == "2018-01-01"), index),
                                         labels = paste0(pull(filter(df, year == "2018-01-01"), change), "%"))) +
  labs(x = NULL, y = NULL,
       title = expression(bold(paste("Change in ", CO[2], " emissions since 1990"))),
       subtitle = "United Kingdom, 1990-2018",
       caption = "Source: BEIS") +
  theme_lab() +
  theme(plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(angle = 90, hjust = 1))

ggsave("outputs/uk_carbon_emissions.png", scale = 1, dpi = 300) 

