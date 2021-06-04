library(tidyverse)

starwars %>%
  select(name, height, mass) %>%
  drop_na() %>%
  mutate(bmi = mass / (height/100)^2) %>%
  ggplot(aes(x = bmi)) +
  geom_histogram(fill = "#3182bd", colour = "#ffffff", alpha = 0.8, binwidth = 10) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = expression(paste("Body Mass Index (kg/m"^2, ")"), sep = ""),
       y = NULL,
       title = "BMI distribution for Star Wars characters",
       caption = "Source:http://swapi.co") +
  theme_minimal() +
  theme(
    plot.margin = unit(rep(30, 4), "pt"),
    plot.title = element_text(size = 16, face = "bold")
  )

ggsave("starwars_BMI.png", dpi = 300, scale = 1)