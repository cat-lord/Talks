## Dentists and GPs in Greater Manchester ##

# Source: Care Quality Commission
# Publisher URL: http://www.cqc.org.uk/about-us/transparency/using-cqc-data
# Licence: Open Government Licence 3.0

# load libraries ---------------------------
library(tidyverse) ;  library(sf)

# import data ---------------------------
raw <- read_csv("https://www.cqc.org.uk/sites/default/files/13_March_2019_CQC_directory.csv", 
               skip = 4, col_names = TRUE) %>% 
  setNames(tolower(names(.)))

# postcode centroids derive from ONS Open Geography Portal under OGL 3.0
area_code <- paste0("E0", seq(8000001, 8000010, by = 1))
query <- paste0("https://ons-inspire.esriuk.com/arcgis/rest/services/Postcodes/ONS_Postcode_Directory_Latest_Centroids/MapServer/0/query?where=UPPER(oslaua)=%27", area_code, "%27&outFields=pcds,oslaua,lat,long&outSR=4326&f=geojson")
postcodes <- map_df(query, function(i) {
  sf <- st_read(i) %>% select(postcode = pcds, area_code = oslaua, lon = long, lat)
  df <- rbind(sf) %>% st_set_geometry(NULL)
})

# tidy data ---------------------------
df <- raw %>%
  filter(str_detect(`service types`, "Dentist|GPs") & 
           `local authority` %in% c("Bolton", "Bury", "Manchester", "Oldham",
                                    "Rochdale", "Salford", "Stockport", 
                                    "Tameside", "Trafford", "Wigan")) %>%
  select(name, 
         type = `service types`, 
         address, postcode, 
         telephone = `phone number`, 
         website = `service's website (if available)`, 
         area_name = `local authority`) %>% 
  mutate(type = 
           case_when(
             str_detect(type, "Dentist") ~ "Dentist",
             TRUE ~ "General Practice"
           ),
         telephone = 
           case_when(
             is.na(telephone) ~ "",
             TRUE ~ paste0("0", telephone)
           ),
         website = fct_explicit_na(website, na_level = "")) %>% 
  left_join(., postcodes, by = "postcode")

# write data ---------------------------
write_csv(df, "gm_health_services.csv")
