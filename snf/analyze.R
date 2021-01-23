library(tidyverse)
library(lubridate)

# facilities = read_csv('latdata/cdph-skilled-nursing-facilities.csv')
# 
# facilities %>%
#   filter(fips == '037') %>% 
#   distinct(id, slug, name) %>% 
#   write_csv('snf/la-county-facility-list.csv', na = '')

snf = read_csv('latdata/cdph-skilled-nursing-facilities.csv')

snf

master = readxl::read_excel('snf/healthcare_facility_locations.xlsx')

master

facilities = read_csv('snf/la-county-facility-list-marked.csv')

facilities

snf.capacity = master %>% 
  filter(FAC_FDR == 'SKILLED NURSING FACILITY') %>% 
  select(FACID, FAC_FDR, FACNAME, CAPACITY, ENTITY_TYPE_DESCRIPTION)

snf.capacity

all.snf = facilities %>% 
  mutate(overflow = !is.na(overflow)) %>% 
  mutate(id = str_pad(id, width = 9, side = 'left', pad = '0')) %>% 
  right_join(snf) %>%
  full_join(snf.capacity, by = c('id' = 'FACID')) %>% 
  arrange(id, slug, name, date) %>% 
  group_by(id) %>% 
  mutate(
    new_cases = patients_confirmed_cases - replace_na(lag(patients_confirmed_cases), 0),
    new_deaths = patients_deaths - replace_na(lag(patients_deaths), 0)
  ) %>% 
  ungroup() %>% 
  filter(date != lubridate::today())

all.snf

la.snf = all.snf %>% 
  filter(fips == '037')

la.snf

# what percentage of deaths compared to beds/facilities? 

la.snf.list = la.snf %>% 
  group_by(id, slug, name, overflow, ENTITY_TYPE_DESCRIPTION, CAPACITY) %>% 
  summarise(cases = sum(new_cases), deaths = sum(new_deaths)) %>% 
  ungroup()

la.snf.list

la.snf.list %>% 
  group_by(overflow) %>% 
  summarise(
    facilities = n_distinct(id),
    capacity = sum(CAPACITY, na.rm = TRUE),
    deaths = sum(deaths)
  ) %>% 
  mutate_if(is.numeric, ~ .x / sum(.x))

la.snf.list %>% 
  count(overflow, ENTITY_TYPE_DESCRIPTION) %>% 
  pivot_wider(names_from = overflow, values_from = n)

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

la.snf.overflow.monthly %>% 
  ggplot(aes(month, avg.cases, color = overflow)) +
  geom_line() +
  labs(
    x = 'Month',
    y = '',
    title = 'Average cases per facility by month'
  ) +
  theme_minimal()

all.snf.monthly = all.snf %>% 
  group_by(id, slug, name, county, overflow, month = lubridate::floor_date(date, 'month')) %>% 
  summarise(
    new.cases = sum(new_cases, na.rm = TRUE),
    new.deaths = sum(new_deaths, na.rm = TRUE),
  )

all.snf.monthly

all.snf.monthly %>% 
  # filter(month == '2020-12-01') %>% 
  arrange(-new.deaths)

# checking kai-ai 

all.snf.monthly %>% 
  filter(id == '970000111') %>% 
  mutate(
    cum.cases = cumsum(new.cases),
    cum.deaths = cumsum(new.deaths)
  )

all.snf %>% 
  filter(id == '970000111') %>% 
  group_by(month = lubridate::floor_date(date, 'month')) %>% 
  filter(date == max(date)) %>% 
  select(name, county, overflow, date, starts_with('patients'))

all.snf.monthly %>% 
  filter(county == 'Los Angeles') %>% 
  ggplot(aes(month, new.deaths, group = id)) +
  geom_line(alpha = 0.1) +
  geom_line(data = . %>% filter(id == '970000111'), color = 'coral') +
  theme_minimal()

all.snf.latest = all.snf %>% 
  group_by(id, slug, name, overflow) %>% 
  filter(date == max(date)) %>% 
  ungroup()

all.snf.latest

la.snf.latest = all.snf.latest %>% 
  filter(county == "Los Angeles")

la.snf.latest

la.snf.latest %>% 
  mutate(
    loc = case_when(
      id == '970000111' ~ 'Kei-Ai',
      overflow ~ 'Other overflow',
      TRUE ~ 'Non-overflow'
    )
  ) %>% 
  ggplot(aes(patients_confirmed_cases, patients_deaths, size = CAPACITY)) +
  geom_point(color = 'gray') +
  geom_point(data = . %>% filter(overflow), aes(color = loc)) +
  theme_minimal()

la.snf.latest %>% 
  arrange(-patients_deaths) %>% 
  head(10) %>% 
  distinct(id) %>% 
  left_join(la.snf) %>% 
  arrange(id, date) %>% 
  # filter(patients_deaths > 10) %>% 
  mutate(
    loc = case_when(
      id == '970000111' ~ 'Kei-Ai',
      overflow ~ 'Other overflow',
      TRUE ~ 'Non-overflow'
    )
  ) %>% 
  ggplot(aes(date, patients_deaths, group = id)) +
  geom_line(color = 'grey', alpha = 0.5) +
  geom_line(data = . %>% filter(overflow), aes(color = loc)) +
  theme_minimal()

# monthly cumulative

la.snf.latest %>% 
  arrange(-patients_deaths) %>% 
  head(10) %>% 
  distinct(id) %>% 
  left_join(la.snf) %>% 
  arrange(id, date) %>% 
  # filter(patients_deaths > 10) %>% 
  mutate(
    loc = case_when(
      id == '970000111' ~ 'Kei-Ai',
      overflow ~ 'Other overflow',
      TRUE ~ 'Non-overflow'
    ),
    month = floor_date(date, 'month')
  ) %>% 
  group_by(id, month) %>% 
  filter(date == max(date)) %>% 
  ggplot(aes(date, patients_deaths, group = id)) +
  geom_step(color = 'grey', alpha = 0.5) +
  geom_step(data = . %>% filter(overflow), aes(color = loc)) +
  theme_minimal()

la.snf %>% 
  filter(patients_deaths > 10) %>% 
  arrange(id, date) %>% 
  ggplot(aes(date, patients_deaths, group = id)) +
  geom_line(color = 'grey', alpha = 0.5) +
  geom_line(data = . %>% filter(overflow)) +
  geom_line(data = . %>% filter(id == '970000111'), color = 'coral') +
  labs(
    title = 'Skilled nursing facilities in L.A. County',
    subtitle = 'Total deaths by day',
    x = '',
    y = ''
  ) +
  theme_minimal()

la.snf %>% 
  arrange(id, date) %>% 
  ggplot(aes(date, patients_confirmed_cases, group = id)) +
  geom_line(color = 'grey', alpha = 0.5) +
  # geom_line(data = . %>% filter(overflow)) +
  geom_line(data = . %>% filter(id == '970000111'), color = 'coral') +
  labs(
    title = 'Skilled nursing facilities in L.A. County',
    subtitle = 'Total cases by day',
    x = '',
    y = ''
  ) +
  theme_minimal()

# export for chart

la.snf %>% # maybe all snf?
  anti_join(
    la.snf %>% 
      filter(new_deaths < 0) %>%
      distinct(id)    
  ) %>% 
  select(id, date, patients_deaths) %>% 
  filter(patients_deaths > 10) %>% 
  pivot_wider(names_from = 'id', values_from = 'patients_deaths') %>% 
  arrange(date) %>% 
  # group_by(id) %>% 
  # filter(date == min(date) | patients_deaths != lag(patients_deaths) | date == max(date)) %>% 
  select(-`970000111`, `970000111`) %>% 
  write_csv('snf/la-death-counts.csv', na = '')

la.snf.latest %>%
  ungroup() %>% 
  arrange(-patients_deaths) %>% 
  head(10) %>% 
  distinct(id) %>% 
  left_join(la.snf) %>% 
  arrange(id, date) %>% 
  # filter(patients_deaths > 10) %>% 
  select(id, date, patients_deaths) %>% 
  pivot_wider(names_from = 'id', values_from = 'patients_deaths') %>% 
  arrange(date) %>% 
  select(-`970000111`, `970000111`) %>% 
  write_csv('snf/la-top10-death-counts.csv', na = '')

la.snf %>% 
  group_by(overflow, date) %>%
  summarise(deaths = mean(patients_deaths, na.rm = TRUE)) %>% 
  ggplot(aes(date, deaths, color = overflow)) +
  geom_line()


kei.ai = la.snf %>% 
  filter(id == '970000111') %>% 
  select(date, cases = patients_confirmed_cases, deaths = patients_deaths)

kei.ai

kei.ai %>% write_csv('snf/kei-ai-deaths-cases.csv', na = '')

kei.ai %>% 
  ggplot(aes(date)) +
  geom_line(aes(y = cases)) +
  geom_line(aes(y = deaths)) 

la.snf.latest %>% 
  select(name, overflow, cases = patients_confirmed_cases, deaths = patients_deaths) %>% 
  arrange(-overflow) %>% 
  write_csv('snf/la-latest.csv')
