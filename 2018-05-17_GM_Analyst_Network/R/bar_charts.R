# Macworld 2008 keynote: US SmartPhone Marketshare #

## Source: https://www.engadget.com/2008/01/15/live-from-macworld-2008-steve-jobs-keynote/

# load libraries ---------------------------
library(tidyverse); library(ggplot2) ; library(ggthemes) ; library(extrafont)

df <- tibble = c(
  brand = c("Nokia", "Motorola", "Palm", "Apple", "Other", "RIM"),
  percent = c(3.1, 7.4, 9.8, 19.5, 21.2, 39)
)

# create data ---------------------------
df <- data.frame (
  brand = c("Nokia", "Motorola", "Palm", "Apple", "Other", "RIM"),
  percent = c(3.1, 7.4, 9.8, 19.5, 21.2, 39)
)

# tidy data ---------------------------
df <- df %>% 
  arrange(desc(percent)) %>%
  mutate(brand = factor(brand, levels = as.character(brand)))

# create plot ---------------------------
standard <- 
  ggplot(data = df, aes(x = brand, y = percent)) + 
  geom_col(fill = "grey", width = 0.6) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(labels = function(x){ paste0(x, "%") }, expand = c(0, 0), limits = c(0, 40)) +
  labs(title = NULL,
       subtitle = NULL,
       x = NULL, 
       y = NULL) +
  labs(title = NULL,
       subtitle = NULL,
       x = NULL, 
       y = NULL) +
  geom_text(x = 3.8, y = 22, adj = 1, family = "serif", color = "#757575",
            label = c("The 1st generation iPhone\nrecorded 19.5% of the\nsmartphone market share\nin Q1 2008")) +
  theme_bw(base_size = 14, base_family = "serif") +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_blank())

tufte <- 
  ggplot(data = df, aes(x = brand, y = percent)) + 
  geom_col(fill = "grey", width = 0.6) +
  geom_hline(yintercept = seq(0, max(df$percent), 10), colour = "white", lwd = 1) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(labels = function(x){ paste0(x, "%") }, expand = c(0, 0), limits = c(0, 40)) +
  labs(title = NULL,
       subtitle = NULL,
       x = NULL, 
       y = NULL) +
  geom_text(x = 3.8, y = 22, adj = 1, family = "serif", color = "#757575",
            label = c("The 1st generation iPhone\nrecorded 19.5% of the\nsmartphone market share\nin Q1 2008")) +
  theme_tufte(base_size = 14, ticks = F) +
    theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
          axis.line = element_blank(),
          axis.title = element_blank())

# save plot ---------------------------
ggsave("figures/bar_chart.png", plot = standard, dpi = 300, scale = 1)
ggsave("figures/tufte_bar_chart.png", plot = tufte, dpi = 300, scale = 1)
