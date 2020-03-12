library(babynames)
library(tidyverse)

#1. Common 2017 names
most_popular_names <- babynames %>%
  filter(year == 2017) %>%
  arrange(desc(n))

color_scale <- c("#003399","#FF2B4F", "#fcab27", "#3686d3","#419AA4")

babynames %>%
  filter(name %in% most_popular_names$name[1:5]) %>%
  group_by(name, year) %>%
  summarise(n = mean(n)) %>%
  ggplot(aes(year, n, color = name)) +
  geom_line() +
  scale_color_manual(values = color_scale) +
  labs(x = "", y = "Number of Babies", title = "Most Popular 2017 Names Over Time",
       color = "Name")

## Splitting by Sex
babynames %>%
  filter(name %in% most_popular_names$name[1:8]) %>%
  filter(n > 100) %>% #dealing with names that are used to name bothm & f babies but only popular w/ one
  ggplot(aes(year, n, color = name)) +
  geom_line() +
  facet_grid(~ sex) +
  labs(x = "", y = "Number of Babies", title = "Most Popular 2017 Names Over Time",
       color = "Name")


#2. Popular Unisex Names
# Name Used As Unisex at Least Once
unisex_names <- babynames %>%
  distinct(name, sex) %>%
  group_by(name) %>%
  count() %>%
  filter(n > 1)

freq_unisex_names <- babynames %>%
  filter(name %in% unisex_names$name) %>%
  group_by(name, year) %>% #how many times name appears per year -> 2 if unisex
  count() %>%
  group_by(name) %>% # counts the above, gives idea of how often name is used s unisex
  summarise(n = sum(n)) %>%
  arrange(desc(n)) %>%
  filter(n > quantile(.$n, 0.9)) #arbitrary cutoff

babynames %>%
  filter(name %in% freq_unisex_names$name) %>%
  group_by(name) %>%
  summarise(sd_n = sd(n)) %>%
  arrange(desc(sd_n))


#3. Patterns
#share of top 5 names at a point in time
top10_share <- babynames %>%
  split(.$year) %>%
  map(top_n, 10, n) %>%
  map(group_by, year, sex) %>%
  map(summarize, sum(prop)) %>%
  do.call("rbind", .) %>%
  rename(prop_sum = `sum(prop)`)

top10_share %>%
  ggplot(aes(year, prop_sum, color = sex)) + 
  geom_path() +
  scale_color_manual(values = color_scale) +
  scale_y_continuous(labels = scales::percent) + 
  labs(title = "Share of Babies with Top 10 Most Popular Name",
       subtitle = "Babies Now Have More Unique Names",
       x = "", y = "", color = "Sex")

#number of names recorded by sex by year
babynames %>%
  group_by(year, sex) %>%
  count() %>%
  ggplot(aes(year, n, color = sex)) +
  geom_path() +
  scale_color_manual(values = color_scale) +
  labs(title = "Number of Recorded Baby Names",
       subtitle = "Babies Now Have More Unique Names",
       x = "", y = "", color = "Sex")
