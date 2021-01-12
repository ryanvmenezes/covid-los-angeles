library(tidyverse)

# facilities = read_csv('latdata/cdph-skilled-nursing-facilities.csv')
# 
# facilities %>%
#   filter(fips == '037') %>% 
#   distinct(id, slug, name) %>% 
#   write_csv('snf/la-county-facility-list.csv', na = '')

snf = read_csv('latdata/cdph-skilled-nursing-facilities.csv')

snf

facilities = read_csv('snf/la-county-facility-list-marked.csv')

all.snf = facilities %>% 
  mutate(overflow = !is.na(overflow)) %>% 
  mutate(id = as.character(id)) %>% 
  right_join(snf)  %>%
  arrange(id, slug, name, date) %>% 
  group_by(id) %>% 
  mutate(
    new_cases = patients_confirmed_cases - lag(patients_confirmed_cases),
    new_deaths = patients_deaths - lag(patients_deaths)
  ) %>% 
  ungroup()

all.snf

la.snf = all.snf %>% 
  filter(fips == '037')

la.snf

la.snf.overflow.monthly = la.snf %>% 
  group_by(overflow, month = lubridate::floor_date(date, 'month')) %>% 
  summarise(
    new.cases = sum(new_cases, na.rm = TRUE),
    new.deaths = sum(new_deaths, na.rm = TRUE),
    facilities = n_distinct(id), 
    avg.cases = new.cases / facilities,
    avg.deaths = new.deaths / facilities,
  )

la.snf.overflow.monthly

la.snf.overflow.monthly %>% 
  ggplot(aes(month, new.deaths, color = overflow)) +
  geom_line() +
  labs(
    x = 'Month',
    y = '',
    title = 'Total deaths by facility by month'
  ) +
  theme_minimal()

la.snf.overflow.monthly %>% 
  ggplot(aes(month, avg.deaths, color = overflow)) +
  geom_line() +
  labs(
    x = 'Month',
    y = '',
    title = 'Average deaths per facility by month'
  ) +
  theme_minimal()

la.snf.by.month = la.snf %>% 
  group_by(id, slug, name, overflow, month = lubridate::floor_date(date, 'month')) %>% 
  summarise(
    new.cases = sum(new_cases, na.rm = TRUE),
    new.deaths = sum(new_deaths, na.rm = TRUE),
  )

la.snf.by.month  

la.snf.by.month %>% 
  ggplot(aes(month, new.cases, group = id)) +
  geom_line(alpha = 0.1) +
  geom_line(data = . %>% filter(overflow), color = 'coral') +
  theme_minimal()

