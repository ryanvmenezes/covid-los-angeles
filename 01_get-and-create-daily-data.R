library(glue)
library(tidyverse)

# download raw copy of hand-entered data from LAT spreadsheet 

download.file(
  'https://docs.google.com/spreadsheets/d/e/2PACX-1vTpZB-OMT3gU9Rs7ZqU0ZFJT8oZXHiv6xRT79fDQU7dmbISu6rciNrCmoNw7R-oH4DqrGdzHMDqtPFC/pub?output=csv&gid=254031884',
  'raw/lat-la-csa-daily.csv'
)

case.cutoff = 50
case.rate.cutoff = 100

csa.list = read_csv('processed/countywide-statistical-areas-cleaned-list.csv')

csa.list

CSA.COUNTS = read_csv(
  'raw/lat-la-csa-daily.csv',
  skip = 4,
  col_types = cols(
    city = col_character(),
    date = col_date(format = ""),
    confirmed_cases = col_double(),
    X4 = col_logical(),
    X5 = col_logical(),
    X6 = col_logical()
  )
)

CSA.COUNTS

csa.daily.counts = CSA.COUNTS %>% 
  select(-X4) %>% 
  filter(date >= '2020-03-27') %>% # data gets better on this date
  filter(city != 'Los Angeles') %>% 
  filter(city != 'City of Los Angeles') %>% 
  filter(!str_detect(str_to_lower(city), 'under investigation')) %>% 
  mutate(
    csa.hood.name = str_replace(city, 'City of ', ''),
    csa.hood.name = str_replace(csa.hood.name, 'Unincorporated - ', ''),
    csa.hood.name = str_replace(csa.hood.name, 'Los Angeles - ', ''),
    csa.hood.name = if_else(str_detect(csa.hood.name, 'Covina \\(Charter Oak\\)'), 'Covina (Charter Oak)', csa.hood.name),
  ) %>% 
  group_by(csa.hood.name, date) %>% 
  summarise(confirmed.cases = sum(confirmed_cases, na.rm = TRUE)) %>% 
  left_join(
    csa.list %>% 
      select(csa.hood.name, mapla.region.slug, population)
  ) %>% 
  mutate(case.rate.100k = confirmed.cases / population * 100000) %>% 
  select(csa.hood.name, mapla.region.slug, everything())

csa.daily.counts

# check for match
csa.daily.counts %>% 
  count(csa.hood.name) %>% 
  full_join(csa.list) %>% 
  filter(is.na(n) | is.na(count.original))

csa.latest = csa.daily.counts %>% 
  arrange(csa.hood.name, date) %>% 
  group_by(csa.hood.name) %>% 
  summarise(
    count.updates = n(),
    first.update = min(date),
    latest.update = max(date),
    population = last(population),
    latest.confirmed.cases = last(confirmed.cases),
    latest.case.rate.100k = last(case.rate.100k)
  ) %>% 
  arrange(-latest.confirmed.cases)

csa.latest


csa.latest %>% 
  ggplot(aes(x = latest.confirmed.cases)) +
  geom_histogram(bins = 100) +
  geom_vline(xintercept = case.cutoff, color = 'red') +
  theme_minimal()

csa.latest %>% 
  filter(latest.confirmed.cases >= case.cutoff) %>% 
  nrow()

csa.latest %>% 
  ggplot(aes(x = latest.case.rate.100k)) +
  geom_histogram(bins = 100) +
  geom_vline(xintercept = case.rate.cutoff, color = 'red') +
  theme_minimal()

csa.latest %>% 
  filter(latest.case.rate.100k >= case.rate.cutoff) %>% 
  nrow()

csa.daily.calcs = csa.daily.counts %>% 
  left_join(
    csa.daily.counts %>% 
      filter(confirmed.cases >= case.cutoff) %>% 
      group_by(csa.hood.name) %>% 
      filter(date == min(date)) %>% 
      select(csa.hood.name, date.of.case.cutoff = date)
  ) %>% 
  left_join(
    csa.daily.counts %>% 
      filter(case.rate.100k >= case.rate.cutoff) %>% 
      group_by(csa.hood.name) %>% 
      filter(date == min(date)) %>% 
      select(csa.hood.name, date.of.case.rate.cutoff = date)
  ) %>% 
  mutate(
    days.since.case.cutoff = as.integer(date - date.of.case.cutoff),
    days.since.case.rate.cutoff = as.integer(date - date.of.case.rate.cutoff)
  ) %>% 
  select(-date.of.case.cutoff, -date.of.case.rate.cutoff) %>% 
  rename_at(vars(days.since.case.cutoff), ~glue('days.since.{case.cutoff}.cases')) %>% 
  rename_at(vars(days.since.case.rate.cutoff), ~glue('days.since.{case.rate.cutoff}.cases.per.100k')) %>% 
  ungroup()
  
csa.daily.calcs

csa.daily.calcs.latest = csa.daily.calcs %>% 
  group_by(csa.hood.name) %>% 
  filter(date == max(date))

csa.daily.calcs.latest

region.daily.counts = csa.daily.counts %>% 
  filter(!is.na(mapla.region.slug)) %>% 
  group_by(mapla.region.slug, date) %>% 
  summarise(
    confirmed.cases = sum(confirmed.cases),
    population = sum(population),
    case.rate.100k = confirmed.cases / population * 100000
  )

region.daily.counts

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
      filter(case.rate.100k >= case.rate.cutoff) %>% 
      group_by(mapla.region.slug) %>% 
      filter(date == min(date)) %>% 
      select(mapla.region.slug, date.of.case.rate.cutoff = date)
  ) %>% 
  mutate(
    days.since.case.cutoff = as.integer(date - date.of.case.cutoff),
    days.since.case.rate.cutoff = as.integer(date - date.of.case.rate.cutoff)
  ) %>% 
  select(-date.of.case.cutoff, -date.of.case.rate.cutoff) %>% 
  rename_at(vars(days.since.case.cutoff), ~glue('days.since.{case.cutoff * 2}.cases')) %>% 
  rename_at(vars(days.since.case.rate.cutoff), ~glue('days.since.{case.rate.cutoff}.cases.per.100k')) %>% 
  ungroup()

region.daily.calcs

region.daily.calcs.latest = region.daily.calcs %>% 
  group_by(mapla.region.slug) %>% 
  filter(date == max(date))

region.daily.calcs.latest

csa.daily.calcs %>% write_csv('processed/csa-daily-calcs.csv', na = '')
csa.daily.calcs.latest %>% write_csv('processed/csa-daily-calcs-latest.csv', na = '')

region.daily.calcs %>% write_csv('processed/region-daily-calcs.csv', na = '')
region.daily.calcs.latest %>% write_csv('processed/region-daily-calcs-latest.csv', na = '')
