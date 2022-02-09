library(tidyverse)
library(RColorBrewer)
data_olm <- read.csv('./../data/data_olm.csv')

# Proportion of liking ratings for classical
p_classical_olm <- data_olm %>% filter(genre == "classical") %>%
  ggplot(aes(x = as.factor(exp_inf), fill = factor(mean_rating))) +
  geom_bar(position = "fill") +
  facet_grid(. ~ wm, 
             labeller = as_labeller(c(`0` = "no cognitive load", `1` = "cognitive load",                                                   'rock' = "rock", 'classical' = "classical"))) +
  scale_fill_brewer(palette = "PuBuGn") + 
  scale_x_discrete(name = "Explicit information condition", labels = c("low","high","none")) + 
  scale_y_continuous(name = "Proportion of liking ratings") + 
  labs(fill = "Liking rating") + 
  theme_classic() +
  ggtitle("Proportion of liking ratings for classical music") +
  theme(text = element_text(size = 16),
        axis.text = element_text(size = 14), 
        title = element_text(face = "bold")) 


p_classical_olm
ggsave("./../results/classical_proportion_liking.png", plot = p_classical_olm, width = 7, height = 5, units = "in")

# Proportion of liking ratings for rock
p_rock_olm <- data_olm %>% filter(genre == "rock") %>%
  ggplot(aes(x = exp_inf, fill = factor(mean_rating))) +
  geom_bar(position = "fill") +
  facet_grid(. ~ wm, 
             labeller = as_labeller(c(`0` = "no cognitive load", `1` = "cognitive load",                                                   'rock' = "rock", 'classical' = "classical"))) +
  scale_fill_brewer(palette = "YlOrRd") + 
  scale_x_discrete(name = "Explicit information condition", labels = c("low","high","none")) + 
  scale_y_continuous(name = "Proportion of liking ratings") + 
  labs(fill = "Liking rating") + 
  theme_classic() +
  ggtitle("Proportion of liking ratings for classical music") +
  theme(text = element_text(size = 16),
        axis.text = element_text(size = 14), 
        title = element_text(face = "bold")) 

p_rock_olm
ggsave("./../results/rock_proportion_liking.png", plot = p_rock_olm, width = 7, height = 5, units = "in")






