# wrangle for analysis
library(tidyverse)
library(stringr)
GSIraw <- read.csv('../data/demographic_data.csv')
GSIdf <- as.data.frame(GSIraw)

GSIdf2 <- GSIdf %>% rename(uid = ID) %>%
  dplyr::select(-question_en,-resp_scale) %>%
  pivot_wider(names_from = c("scale", "q_n"), values_from = c("response")) %>%
  rename_at(vars(starts_with("perceptual abilities_")), 
            funs(str_replace(., "perceptual abilities_", "PA_0"))) %>%
  rename_at(vars(starts_with("musical training_")), 
            funs(str_replace(., "musical training_", "MT_0")))

# You have to manually correct the PA_0X and MT_0X labels, because they do not
# correspond to the right item number in DE (they did in EN).

write.csv(GSIdf2,"../data/GSIanalysis_data.csv")
