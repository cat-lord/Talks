# Trees #

# Source: Forest Research
# https://www.forestresearch.gov.uk/tools-and-resources/statistics/data-downloads/
# Licence: Government Open Licence 3.0

library(tidyverse) ; library(readxl) ; library(httr) ; library(janitor) ; library(lubridate) 
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

tmp <- tempfile(fileext = ".xlsx")
GET(url = "https://www.forestresearch.gov.uk/documents/7322/Ch1_Woodland_FS2019.xlsx",
    write_disk(tmp))

df <- read_xlsx(tmp, sheet = 21, skip = 4, range = "A5:F49") %>% 
  clean_names() %>% 
  mutate(year = ymd(str_c(year_ending_31_3, "01-01", sep = "-"))) %>% 
  select(year, value = uk)

ggplot(df, aes(year, value)) +
  geom_col(fill = "#C4D39B") +
  geom_col(fill = ifelse(df$year %in% c(ymd(19890101), ymd(20190101)), "#394D11", "#C4D39B")) +
  geom_hline(yintercept = 10, size = 0.5, colour = "#FFFFFF") +
  geom_hline(yintercept = 20, size = 0.5, colour = "#FFFFFF") +
  geom_hline(yintercept = 30, size = 0.5, colour = "#212121", linetype = "dotted") + 
  annotate(geom = "text", x = ymd(19900101), y = 30, 
           label = "atop(bold('30,170')~hectares~planted~'in'~bold('1988/1989'))",
           parse = TRUE, hjust = 0, vjust = 1.2, size = 3) +
  annotate(geom = "text", x = ymd(20200101), y = 12, 
           label = "atop(bold('13,400')~hectares~planted~'in'~bold('2018/19'))",
           parse = TRUE, hjust = 0, size = 3) +
  annotate(geom = "text", x = ymd(20220901), y = 32.5, 
           label = "The Committee on Climate Change recommend\nplanting 30,000 hectares of woodland each year", 
           hjust = 0, vjust = 1.2, fontface = "italic") +
  geom_hline(yintercept = 0, size = 1, colour = "#333333") +
  scale_x_date(breaks = "5 years", date_labels = "%Y", expand = c(0.005, 0.005)) +
  scale_y_continuous(expand = c(0.005, 0.005),
                     position = "right",
                     breaks = c(0, 10, 20, 30),
                     labels = c("0 kha", "10 kha", "20 kha", "30 kha")) +
  labs(x = NULL, y = NULL,
       title = "Tree planting rates",
       subtitle = "United Kingdom, 1976-2019",
       caption = "Source: Forestry Research") +
  theme_lab() +
  theme(panel.grid.major.y = element_blank(), 
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  expand_limits(x = ymd(20500101), y = 32)

ggsave("outputs/trees.png", scale = 1, dpi = 300) 

