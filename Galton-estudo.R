# Estudo de caso: A altura é hereditária ?

library(tidyverse)
library(broom)
library(modelr)
library(HistData)


data("GaltonFamilies")

set.seed(1, sample.kind = "Rounding") # if you are using R 3.6 or later

male_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

glimpse(male_heights)

female_heights <- GaltonFamilies %>%
  filter(gender == "female") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(mother, childHeight) %>%
  rename(daughter = childHeight)

glimpse(female_heights)

galton <- GaltonFamilies %>%
  group_by(family, gender) %>%
  sample_n(1) %>%
  ungroup() %>% 
  gather(parent, parentHeight, father:mother) %>%
  mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
  unite(pair, c("parent", "child"))

glimpse(galton)

pares <- galton     %>%
         group_by(pair) %>%
         summarize(count = n())

pares 

glimpse(galton)

galton %>%
  group_by(pair) %>%
  summarize(cor = cor(parentHeight, childHeight)) %>%
  filter(cor == min(cor))

galton %>%
  group_by(pair) %>%
  summarize(cor = cor(parentHeight, childHeight)) %>%
  filter(cor == max(cor))

#################################################################

galton <- GaltonFamilies %>%
  group_by(family, gender) %>%
  sample_n(1) %>%
  ungroup() %>% 
  gather(parent, parentHeight, father:mother) %>%
  mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
  unite(pair, c("parent", "child"))

glimpse(galton)

sol10 <- galton %>%
       group_by(pair) %>%
       do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE)) %>%
       filter(term == "parentHeight") %>%
       select(pair, estimate, std.error, conf.low, conf.high)
sol10       

galton %>%
  group_by(pair) %>%
  do(glance(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE)) %>%
  select(pair, r.squared, adj.r.squared, p.value, sigma)

# another solution should be 
galton %>%
  group_by(pair) %>%
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE)) %>%
  filter(term == "parentHeight", pair == "father_daughter") %>%
  pull(estimate)


galton %>%
  group_by(pair) %>%
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE)) %>%
  filter(term == "parentHeight", pair == "mother_son") %>%
  pull(estimate)





