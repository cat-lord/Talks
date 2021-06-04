# How many @traffordDataLab followers are bots? #

library(tidyverse) ; library(rtweet) ; library(tweetbotornot)

# retrieve followers of @OpenGovInt
followers <- get_followers("traffordDataLab", n = "all") 
followers_info <- lookup_users(followers$user_id) %>% 
  select(screen_name, name, followers = followers_count, following = friends_count)

# how many followers are bots?
bot <- tweetbotornot(followers_info$screen_name[1:50], fast = FALSE) %>%
  arrange(prob_bot)

# arrange by probability estimates
bot[order(bot$prob_bot), ]

# plot probability estimates
bot %>% 
  select(screen_name, prob_bot) %>% 
  arrange(prob_bot) %>% 
  ggplot()  + 
  geom_col(aes(x = reorder(screen_name, -prob_bot), y = prob_bot), fill = "#E44690") + 
  scale_y_continuous(limits = c(0, 1), expand = c(0,0)) +
  coord_flip() + 
  labs(title = "Probability of @traffordDataLab followers being bots",
       x = NULL, y = NULL) + 
  theme_minimal() +
  theme(plot.margin=unit(c(1,1,1,1),"cm"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.y = element_text(hjust = 0))

ggsave("bot_or_not.png", dpi = 300, scale = 1)


