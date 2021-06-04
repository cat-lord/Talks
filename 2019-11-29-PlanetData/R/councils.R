# Councils declaring a climate emergency #

# Source: Declare a Climate Emergency
# Publisher URL: https://www.climateemergency.uk/blog/list-of-councils/
# Licence: data derived from open sources

# load libraries ------------------------------
library(tidyverse) ; library(rvest) ; library(sf) ; library(ggthemes) ; library(grid)
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

# load data ------------------------------
uk <- st_read("https://opendata.arcgis.com/datasets/bbb0e58b0be64cc1a1460aa69e33678f_0.geojson") %>% 
  select(area_code = lad19cd, area_name = lad19nm)

df <- read_html("https://www.climateemergency.uk/blog/list-of-councils/") %>% 
  html_node("table") %>%
  html_table() %>% 
  select(council = Council, region = Region, type = Type, url = 8) %>% 
  distinct(council, region, type, url) %>% 
  filter(!type %in% c("City Region", "Combined Authority", "County")) %>% 
  mutate(area_name = str_trim(council),
         area_name = str_replace_all(area_name, "&", "and"),
         area_name = case_when(
           council == "Bath & NES" ~ "Bath and North East Somerset",
           council == "Blackburn-with-Darwen" ~ "Blackburn with Darwen",
           council == "Bristol" ~ "Bristol, City of",
           council == "City of York" ~ "York",
           council == "Derry & Strabane" ~ "Derry City and Strabane",
           council == "Dundee" ~ "Dundee City",
           council == "Durham" ~ "County Durham",
           council == "Edinburgh" ~ "City of Edinburgh",
           council == "Glasgow" ~ "Glasgow City",
           council == "Herefordshire" ~ "Herefordshire, County of",
           council == "Hull" ~ "Kingston upon Hull, City of",
           council == "Kingston-upon-Thames" ~ "Kingston upon Thames",
           council == "Liverpool City" ~ "Liverpool",
           council == "Orkney" ~ "Orkney Islands",
           council == "Richmond-upon-Thames" ~ "Richmond upon Thames",
           council == "Scilly Isles" ~ "Isles of Scilly",
           council == "St Alban's" ~ "St Albans",
           council == "St. Helen's" ~ "St. Helens",
           council == "Uttesford" ~ "Uttlesford",
           TRUE ~ area_name)) %>% 
  left_join(st_set_geometry(uk, NULL), df, by = "area_name")

sf <- left_join(uk, select(df, -area_name), by = "area_code") %>% 
  mutate(status = case_when(
    !is.na(council) ~ "Declared", TRUE ~ "Undeclared"
  )) %>% 
  select(area_code, area_name, everything())

# plot data ------------------------------
map <- ggplot() + 
  geom_sf(data = sf, aes(fill = status), 
          color = "#FFFFFF", size = 0.1) +
  labs(x = NULL, y = NULL,
       title = "UK councils declaring a climate emergency",
       subtitle = "as of 21 October 2019",
       caption = "Source: climateemergency.uk | @traffordDataLab\nContains Ordnance Survey data © Crown copyright and database right 2019",
       fill = NULL) +
  scale_fill_manual(values = c("Declared" = "#7fbf7b", "Undeclared" = "#af8dc3"), 
                    labels = c("Declared", "Undeclared")) +
  coord_sf(datum = NA) +
  theme_lab() +
  theme(
    plot.margin = unit(rep(0.5, 4), "cm"),
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(size = 8, color = "grey50", hjust = 0),
    legend.position = c(0.1,0.9),
    legend.direction = "vertical",
    legend.text = element_text(size = 8)
  ) 

inset <- ggplot() + 
  geom_sf(data = filter(sf, area_name %in% c("Barking and Dagenham", "Barnet", "Bexley",
                                             "Brent", "Bromley", "Camden", "City of London", "Croydon",
                                             "Ealing", "Enfield", "Greenwich", "Hackney",
                                             "Hammersmith and Fulham", "Haringey", "Harrow",
                                             "Havering", "Hillingdon", "Hounslow", "Islington",
                                             "Kensington and Chelsea", "Kingston upon Thames", "Lambeth", 
                                             "Lewisham", "Merton", "Newham", "Redbridge", "Richmond upon Thames",
                                             "Southwark", "Sutton", "Tower Hamlets", "Waltham Forest",
                                             "Wandsworth", "Westminster")),
          aes(fill = status), 
          color = "#FFFFFF", size = 0.1) +
  labs(x = NULL, y = NULL, title = "London", subtitle = NULL, caption = NULL, fill = NULL) +
  scale_fill_manual(values = c("Declared" = "#7fbf7b", "Undeclared" = "#af8dc3"), 
                    labels = c("Declared", "Undeclared")) +
  coord_sf(datum = NA) +
  theme_map() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")

map
print(inset, vp = viewport(0.8, 0.22, width = 0.2, height = 0.2))
