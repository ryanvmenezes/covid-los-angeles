library(tidyverse)
library(lubridate)

data = read_csv('latdata/latimes-county-totals.csv')

counts.21 = data %>% 
  mutate(week = floor_date(date, 'weeks', week_start = 1)) %>% 
  group_by(week) %>% 
  summarise(
    cases = sum(new_confirmed_cases, na.rm = TRUE),
    deaths = sum(new_deaths, na.rm = TRUE)
  ) %>% 
  arrange(desc(week)) %>% 
  filter(year(week) == 2021) %>% 
  mutate(week = lag(week) - 1) %>% 
  tail(-1)

counts.21

counts.21 %>% 
  pivot_longer(-week) %>% 
  ggplot(aes(week, value)) +
  geom_bar(stat = 'identity') +
  facet_wrap(. ~ name, scale = 'free_y')

counts.21 %>% write_csv('latest-counts.csv', na = '')
