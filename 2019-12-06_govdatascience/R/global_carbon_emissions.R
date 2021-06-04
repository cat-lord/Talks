# Global carbon emissions #

# Inspiration: https://blog.datawrapper.de/weekly-chart-greenhouse-gas-emissions-climate-crisis/
# Data: http://www.globalcarbonatlas.org/en/CO2-emissions

# load libraries ------------------------------
library(tidyverse) ; library(lubridate) ; library(countrycode)

# load data ------------------------------
df <- read_delim("data/global_carbon_emissions.txt", skip = 1, delim = ";") %>% 
  gather(country, emissions, -X1) %>%
  rename(year = X1) %>%
  filter(!grepl("Sources|Territorial", year)) %>%
  mutate(year = ymd(str_c(year, "01-01", sep = "-")),
         emissions = as.numeric(emissions),
         eu28 = countrycode(country, "country.name", destination = "eu28"),
         country = case_when(
           eu28 == "EU" ~ "EU",
           country == "China" ~ "China",
           country == "India" ~ "India",
           country == "United States of America" ~ "USA",
           TRUE ~ "Other countries"
         )) %>% 
  group_by(year, country) %>% 
  summarise(emissions = sum(emissions, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(country = fct_relevel(factor(country), "Other countries", "EU", "USA", "India", "China"))

# plot data ------------------------------
ggplot(df, aes(x = year, y = emissions/1000)) +
  geom_area(aes(fill = country, alpha = 0.5)) +
  geom_label(aes(x = as.Date("1992-01-01"), y = 24.5, label = "Rio\nEarth summit"), 
             hjust = 1, size = 3, fontface = "bold", colour = "#212121", fill = "#FFFFFF", label.size = NA) +
  geom_segment(aes(x = as.Date("1992-01-01"), y = sum(filter(df, year == "1992-01-01")$emissions/1000), 
                   xend = as.Date("1992-01-01"), yend = 24), 
               colour = "#555555",  size = 0.5, linetype = "dotted") +
  geom_label(aes(x = as.Date("1997-01-01"), y = 26.5, label = "Kyoto\nprotocol"), 
             hjust = 1, size = 3, fontface = "bold", colour = "#212121", fill = "#FFFFFF", label.size = NA) +
  geom_segment(aes(x = as.Date("1997-01-01"), y = sum(filter(df, year == "1997-01-01")$emissions/1000), 
                   xend = as.Date("1997-01-01"), yend = 26), 
               colour = "#555555",  size = 0.5, linetype = "dotted") +
  geom_label(aes(x = as.Date("2015-01-01"), y = 37.5, label = "Paris\nagreement"), 
             hjust = 1, size = 3, fontface = "bold", colour = "#212121", fill = "#FFFFFF", label.size = NA) +
  geom_segment(aes(x = as.Date("2015-01-01"), y = sum(filter(df, year == "2015-01-01")$emissions/1000), 
                   xend = as.Date("2015-01-01"), yend = 37), 
               colour = "#555555",  size = 0.5, linetype = "dotted") +
  geom_line(data = group_by(df, year) %>% 
              summarise(emissions = sum(emissions)) %>% 
              ungroup(),
            aes(x = year, y = emissions/1000), 
            colour = "#000000", size = 1) +
  geom_point(aes(x = as.Date("2017-01-01"), y = sum(filter(df, year == "2017-01-01")$emissions/1000)), 
             size = 4, colour = "#000000") +
  geom_text(aes(x = as.Date("2013-01-01"), y = 27, 
                label = "Other\ncountries"), 
            hjust = 0, colour = "#777777", size = 4) +
  geom_text(aes(x = as.Date("2013-01-01"), y = 19, 
                label = "EU"), 
            hjust = 0, colour = "#a83834", size = 4) +
  geom_text(aes(x = as.Date("2013-01-01"), y = 15, 
                label = "USA"), 
            hjust = 0, colour = "#ca7a15", size = 4) +
  geom_text(aes(x = as.Date("2013-01-01"), y = 11, 
                label = "India"), 
            hjust = 0, colour = "#27b592", size = 4) +
  geom_text(aes(x = as.Date("2013-01-01"), y = 5, 
                label = "China"), 
            hjust = 0, colour = "#517783", size = 4) +
  geom_hline(yintercept = 0, size = 1, colour  ="#333333") +
  scale_fill_manual(values = c("Other countries" = "#EFEFEF",
                               "EU" = "#EFCDCC",
                               "USA" = "#F9E3C7",
                               "India" = "#C7F3E8",
                               "China" = "#CBDADF")) +
  scale_x_date(breaks = seq(as.Date("1960-01-01"), as.Date("2017-01-01"), by = "10 years"),
               date_labels = "%Y", expand = c(0.01, 0.01)) +
  scale_y_continuous(position = "right", breaks = seq(0,40,5), expand = c(0.005, 0.005)) +
  labs(x = NULL, y = expression(paste("Gigatonnes of ", CO[2])),
       title = NULL, subtitle = NULL,
       caption = "Source: Global Carbon Project | @traffordDataLab") +
  theme_minimal() +
  theme(plot.margin = unit(rep(0.5, 4), "cm"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 25, face = "bold"),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 10, color = "grey50", hjust = 1, margin = margin(t = 20)),
        axis.title.y = element_text(size = 10, colour = "#757575", hjust = 1),
        legend.position = "none") +
  expand_limits(y = 39)

# write output ------------------------------
ggsave("outputs/global_carbon_emissions.png", dpi = 300, scale = 1)
