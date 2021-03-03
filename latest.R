library(tidyverse)
library(lubridate)
library(zoo)

counts = read_csv('latdata/latimes-county-totals.csv')

counts

hosp = read_csv('latdata/cdph-hospital-patient-county-totals.csv')

hosp

case.7day = counts %>% 
  group_by(date) %>% 
  summarise(cases = sum(new_confirmed_cases, na.rm = TRUE)) %>% 
  mutate(cases = rollmean(cases, k = 7, fill = NA, na.pad = TRUE, align = 'right')) %>% 
  filter(date >= '2020-03-01')

case.7day

case.7day %>% write_csv('case-7day.csv', na = '')

death.7day = counts %>% 
  group_by(date) %>% 
  summarise(deaths = sum(new_deaths, na.rm = TRUE)) %>% 
  mutate(deaths = rollmean(deaths, k = 7, fill = NA, na.pad = TRUE, align = 'right')) %>% 
  filter(date >= '2020-03-01')

death.7day

death.7day %>% write_csv('death-7day.csv', na = '')

hosp.totals = hosp %>% 
  group_by(date) %>% 
  summarise(hosp = sum(positive_patients, na.rm = TRUE))

hosp.totals %>% write_csv('hospital-totals.csv', na = '')

# 
# counts.by.week = data %>% 
#   mutate(week = floor_date(date, 'weeks', week_start = 1)) %>% 
#   group_by(week) %>% 
#   summarise(
#     cases = sum(new_confirmed_cases, na.rm = TRUE),
#     deaths = sum(new_deaths, na.rm = TRUE)
#   ) %>% 
#   arrange(desc(week)) %>% 
#   mutate(week = lag(week) - 1) %>%
#   drop_na(week)
# 
# counts.by.week
# 
# counts.21 = counts.by.week %>% 
#   filter(year(week) == 2021)
# 
# counts.21
# 
# counts.21 %>% write_csv('latest-counts.csv', na = '')
# 
# 
# 
# icu.by.week = icu %>% 
#   group_by(date) %>% 
#   summarise(
#     hosp = sum(positive_patients) + sum(icu_positive_patients)
#   ) %>% 
#   mutate(week = floor_date(date, 'weeks', week_start = 7)) %>% 
#   arrange(date) %>% 
#   group_by(week) %>% 
#   summarise(hosp = last(hosp)) %>% 
#   arrange(desc(week))
# 
# icu.by.week
# 
# icu.21 = icu.by.week %>% 
#   filter(year(week) == 2021)
# 
# icu.21
# 
# icu.21 %>% write_csv('latest-hospitalizations.csv',  na = '')  
# 
# counts.21 %>% 
#   left_join(icu.21) %>% 
#   pivot_longer(-week) %>% 
#   ggplot(aes(week, value)) +
#   geom_bar(stat = 'identity') +
#   facet_wrap(. ~ name, scale = 'free_y')
# 
# counts.by.week %>% 
#   left_join(icu.by.week) %>% 
#   pivot_longer(-week) %>% 
#   ggplot(aes(week, value)) +
#   geom_line() +
#   geom_vline(xintercept = ymd('2020-11-01'), color = 'red') +
#   facet_wrap(. ~ name, scale = 'free_y')
# 
# counts.by.week %>% 
#   left_join(icu.by.week) %>% 
#   pivot_longer(-week) %>% 
#   filter(week > ymd('2020-09-01')) %>% 
#   group_by(name) %>% 
#   filter(value == min(value))
