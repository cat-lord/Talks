# Rosling's bubble chart #

library(tidyverse) ; library(gapminder) ; library(scales)

ggplot(filter(gapminder, year == 2007), aes(x = gdpPercap, y = lifeExp)) + 
  scale_x_log10(labels = scales::dollar) + 
  geom_point(aes(size = pop, fill = continent), shape = 21, colour = "white", alpha = 0.6) + 
  scale_fill_brewer(palette = "Set2") + 
  scale_size_continuous(range = c(1, 20)) + 
  labs(title = "Life expectancy increases with income", 
       subtitle = "Relationship between life expectancy and income, 2007", 
       caption = "Source: Gapminder.org  |  @traffordDataLab", 
       x = "GDP per capita ($)", 
       y = "Age (years)") + 
  guides(size = FALSE) + 
  theme_minimal() + 
  theme(panel.grid.major.x = element_blank(),
        plot.caption = element_text(size = 10, hjust = 1, margin = margin(t = 15)),
        axis.title = element_text(size = 10, hjust = 1),
        legend.position = "right", 
        legend.title = element_blank())

ggsave(file = "figures/rosling_bubble_chart.png", width = 6, height = 6)

