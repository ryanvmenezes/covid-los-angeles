library(tidyverse)

csa.list = read_csv('los-angeles/countywide-statistical-areas-cleaned-list.csv')

csa.list

csa.mapla.xwalk = read_csv('los-angeles/mapla-region-csa-crosswalk.csv')

csa.mapla.xwalk

CSA.COUNTS = read_csv('data/processed/covid/lat-la-county-cities.csv')

csa.daily.counts = CSA.COUNTS %>% 
  filter(date >= '2020-03-27') %>% # data gets better on this date
  filter(city != 'Los Angeles') %>% 
  filter(city != 'City of Los Angeles') %>% 
  filter(!str_detect(str_to_lower(city), 'under investigation')) %>% 
  mutate(
    hood.name = str_replace(city, 'City of ', ''),
    hood.name = str_replace(hood.name, 'Unincorporated - ', ''),
    hood.name = str_replace(hood.name, 'Los Angeles - ', ''),
    hood.name = if_else(str_detect(hood.name, 'Covina \\(Charter Oak\\)'), 'Covina (Charter Oak)', hood.name),
  ) %>% 
  group_by(hood.name, date) %>% 
  summarise(confirmed.cases = sum(confirmed_cases, na.rm = TRUE)) %>% 
  left_join(
    csa.list %>% 
      select(hood.name, population)
  ) %>% 
  left_join(
    csa.mapla.xwalk %>% 
      select(hood.name = csa.hood.name, mapla.region.slug)
  ) %>% 
  mutate(case.rate = confirmed.cases / population) %>% 
  select(hood.name, mapla.region.slug, everything())

csa.daily.counts

# check for match
csa.daily.counts %>% 
  count(hood.name) %>% 
  full_join(csa.list) %>% 
  filter(is.na(n) | is.na(count.original))

csa.latest = csa.daily.counts %>% 
  arrange(hood.name, date) %>% 
  group_by(hood.name) %>% 
  summarise(
    count.updates = n(),
    first.update = min(date),
    latest.update = max(date),
    population = last(population),
    latest.confirmed.cases = last(confirmed.cases),
    latest.case.rate = last(case.rate)
  ) %>% 
  arrange(-latest.confirmed.cases)

csa.latest

case.cutoff = 50

csa.latest %>% 
  ggplot(aes(x = latest.confirmed.cases)) +
  geom_histogram(bins = 100) +
  geom_vline(xintercept = case.cutoff, color = 'red') +
  theme_minimal()

csa.latest %>% 
  filter(latest.confirmed.cases >= case.cutoff) %>% 
  nrow()

case.rate.cutoff = 0.002

csa.latest %>% 
  ggplot(aes(x = latest.case.rate)) +
  geom_histogram(bins = 100) +
  geom_vline(xintercept = case.rate.cutoff, color = 'red') +
  theme_minimal()

csa.latest %>% 
  filter(latest.case.rate >= case.rate.cutoff) %>% 
  nrow()

csa.daily.calcs = csa.daily.counts %>% 
  left_join(
    csa.daily.counts %>% 
      filter(confirmed.cases >= case.cutoff) %>% 
      group_by(hood.name) %>% 
      filter(date == min(date)) %>% 
      select(hood.name, date.of.case.cutoff = date)
  ) %>% 
  left_join(
    csa.daily.counts %>% 
      filter(case.rate >= case.rate.cutoff) %>% 
      group_by(hood.name) %>% 
      filter(date == min(date)) %>% 
      select(hood.name, date.of.case.rate.cutoff = date)
  ) %>% 
  mutate(
    days.since.case.cutoff = as.integer(date - date.of.case.cutoff),
    days.since.case.rate.cutoff = as.integer(date - date.of.case.rate.cutoff)
  ) %>% 
  select(-date.of.case.cutoff, -date.of.case.rate.cutoff) %>% 
  ungroup()
  
csa.daily.calcs

csa.daily.calcs.latest = csa.daily.calcs %>% 
  group_by(hood.name) %>% 
  filter(date == max(date))

csa.daily.calcs.latest

region.daily.counts = csa.daily.counts %>% 
  filter(!is.na(mapla.region.slug)) %>% 
  group_by(mapla.region.slug, date) %>% 
  summarise(
    confirmed.cases = sum(confirmed.cases),
    population = sum(population),
    case.rate = confirmed.cases / population
  )

region.daily.calcs = region.daily.counts %>% 
  left_join(
    region.daily.counts %>% 
      filter(confirmed.cases >= case.cutoff * 2) %>% 
      group_by(mapla.region.slug) %>% 
      filter(date == min(date)) %>% 
      select(mapla.region.slug, date.of.case.cutoff = date)
  ) %>% 
  left_join(
    region.daily.counts %>% 
      filter(case.rate >= case.rate.cutoff / 2) %>% 
      group_by(mapla.region.slug) %>% 
      filter(date == min(date)) %>% 
      select(mapla.region.slug, date.of.case.rate.cutoff = date)
  ) %>% 
  mutate(
    days.since.case.cutoff = as.integer(date - date.of.case.cutoff),
    days.since.case.rate.cutoff = as.integer(date - date.of.case.rate.cutoff)
  ) %>% 
  select(-date.of.case.cutoff, -date.of.case.rate.cutoff) %>% 
  ungroup()

region.daily.calcs

region.daily.calcs.latest = region.daily.calcs %>% 
  group_by(mapla.region.slug) %>% 
  filter(date == max(date))

region.daily.calcs.latest

csa.daily.calcs %>% write_csv('los-angeles/tables/csa-daily-calcs.csv', na = '')
csa.daily.calcs.latest %>% write_csv('los-angeles/tables/csa-daily-calcs-latest.csv', na = '')

region.daily.calcs %>% write_csv('los-angeles/tables/region-daily-calcs.csv', na = '')
region.daily.calcs.latest %>% write_csv('los-angeles/tables/region-daily-calcs-latest.csv', na = '')
