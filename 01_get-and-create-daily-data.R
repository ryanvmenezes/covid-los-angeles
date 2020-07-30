library(tidyverse)
library(lubridate)
library(zoo)

# download raw copy of hand-entered data from LAT spreadsheet 

download.file(
  'https://docs.google.com/spreadsheets/d/e/2PACX-1vTpZB-OMT3gU9Rs7ZqU0ZFJT8oZXHiv6xRT79fDQU7dmbISu6rciNrCmoNw7R-oH4DqrGdzHMDqtPFC/pub?output=csv&gid=254031884',
  'raw/lat-la-csa-daily.csv'
)

# cleaned up CSA list based on GIS work
# has population, all component parts of the whole CSA, and mapping la region it maps to 
csa.list = read_csv('processed/countywide-statistical-areas-cleaned-list.csv')

csa.list

CSA.COUNTS = read_csv(
  'raw/lat-la-csa-daily.csv',
  skip = 4
)

CSA.COUNTS

# clean up the csa names 
csa.daily = CSA.COUNTS %>% 
  filter(date >= '2020-03-27') %>% # data gets better on this date
  filter(city != 'Los Angeles') %>% 
  filter(city != 'City of Los Angeles') %>% 
  filter(!str_detect(str_to_lower(city), 'under investigation')) %>% 
  mutate(
    csa.hood.name = str_replace(city, 'City of ', ''),
    csa.hood.name = str_replace(csa.hood.name, '\\*', ''),
    csa.hood.name = str_replace(csa.hood.name, 'Unincorporated - ', ''),
    csa.hood.name = str_replace(csa.hood.name, 'Los Angeles - ', ''),
    csa.hood.name = if_else(str_detect(csa.hood.name, 'Covina \\(Charter Oak\\)'), 'Covina (Charter Oak)', csa.hood.name),
  ) %>% 
  # group and sum across these new CSA named
  group_by(csa.hood.name, date) %>% 
  summarise(cases = sum(confirmed_cases, na.rm = TRUE)) %>% 
  # join to the cleaned CSA list to get population totals
  left_join(
    csa.list %>% 
      select(csa.hood.name, mapla.region.slug, population)
  ) %>% 
  # rate per 100,000 population
  mutate(
    rate.cases = cases / population * 100000
  ) %>% 
  select(csa.hood.name, mapla.region.slug, everything()) %>% 
  arrange(csa.hood.name, date) %>% 
  group_by(csa.hood.name) %>%
  mutate(
    new.cases = cases - lag(cases),
    new.cases.7dayavg = rollmean(new.cases, 7, na.pad = TRUE, align = 'right'),
    rate.new.cases.7dayavg = new.cases.7dayavg / population * 100000
  ) %>% 
  ungroup()

csa.daily

# check to make sure everything matched
csa.list %>% 
  anti_join(csa.daily %>% distinct(csa.hood.name))

# get summary that includes latest data
csa.latest = csa.daily %>% 
  group_by(csa.hood.name) %>% 
  filter(date == max(date)) %>% 
  arrange(-cases)

csa.latest

# look at just the new cases in the last two weeks

# on a daily basis
csa.recent.daily = csa.daily %>% 
  filter(date >= today() - 14) %>%
  left_join(
    csa.daily %>% 
      group_by(csa.hood.name) %>% 
      filter(date == today() - 14) %>% 
      select(csa.hood.name, cases.two.weeks.ago = cases)
  ) %>% 
  mutate(
    cases = cases - cases.two.weeks.ago,
    rate.cases = cases / population * 100000
  ) %>% 
  select(
    csa.hood.name, mapla.region.slug, date,
    recent.cases = cases, population, recent.rate.cases = rate.cases
  )

csa.recent.daily

# the summary of the last two weeks 
csa.recent.latest = csa.recent.daily %>% 
  group_by(csa.hood.name) %>% 
  filter(date == max(date)) %>% 
  arrange(-recent.cases) %>% 
  ungroup()

csa.recent.latest

# same for regions
region.daily = csa.daily %>% 
  filter(!is.na(mapla.region.slug)) %>% 
  group_by(mapla.region.slug, date) %>% 
  summarise(
    cases = sum(cases),
    population = sum(population),
    rate.cases = cases / population * 100000
  ) %>% 
  group_by(mapla.region.slug) %>% 
  mutate(
    new.cases = cases - lag(cases),
    new.cases.7dayavg = rollmean(new.cases, 7, na.pad = TRUE, align = 'right'),
    rate.new.cases.7dayavg = new.cases.7dayavg / population * 100000
  ) %>% 
  ungroup()

region.daily

region.latest = region.daily %>% 
  group_by(mapla.region.slug) %>% 
  filter(date == max(date)) %>% 
  arrange(-rate.cases)

region.latest

region.recent.daily = region.daily %>% 
  filter(date >= today() - 14) %>%
  left_join(
    region.daily %>% 
      group_by(mapla.region.slug) %>% 
      filter(date == today() - 14) %>% 
      select(mapla.region.slug, cases.two.weeks.ago = cases)
  ) %>% 
  mutate(
    cases = cases - cases.two.weeks.ago,
    rate.cases = cases / population * 100000
  ) %>% 
  select(
    mapla.region.slug, date,
    recent.cases = cases, population, recent.rate.cases = rate.cases
  )

region.recent.daily

region.recent.latest = region.recent.daily %>% 
  group_by(mapla.region.slug) %>% 
  filter(date == max(date)) %>% 
  arrange(-recent.rate.cases) %>% 
  ungroup()

region.recent.latest

csa.daily %>% write_csv('processed/csa-daily.csv', na = '')
# csa.latest %>% write_csv('processed/csa-latest.csv', na = '')

csa.recent.daily %>% write_csv('processed/csa-recent-daily.csv', na = '')
csa.recent.latest %>% write_csv('processed/csa-recent-latest.csv', na = '')

region.daily %>% write_csv('processed/region-daily.csv', na = '')
# region.latest %>% write_csv('processed/region-latest.csv', na = '')

region.recent.daily %>% write_csv('processed/region-recent-daily.csv', na = '')
region.recent.latest %>% write_csv('processed/region-recent-latest.csv', na = '')

# death data

# file manually downlaoded from LACDPH shiny dashboard

lacdph.table = read_csv('raw/LA_County_Covid19_CSA_case_death_table.csv')

lacdph.table

deaths.latest = lacdph.table %>% 
  mutate(
    csa.hood.name = str_replace(geo_merge, 'City of ', ''),
    csa.hood.name = str_replace(csa.hood.name, 'Unincorporated - ', ''),
    csa.hood.name = str_replace(csa.hood.name, 'Los Angeles - ', ''),
  ) %>% 
  group_by(csa.hood.name) %>% 
  summarise(deaths = sum(deaths_final)) %>% 
  ungroup() %>% 
  bind_rows(
    tribble(
      ~csa.hood.name, ~deaths,
      'Pasadena', 105,
      'Long Beach', 170,
    )
  ) %>% 
  full_join(csa.list %>% select(csa.hood.name, population)) %>% 
  mutate(
    deaths = replace_na(deaths, 0),
    rate.deaths = deaths / population * 100000
  )

deaths.latest

deaths.latest %>% write_csv('processed/deaths-latest.csv', na = '')

csa.latest = csa.latest %>% 
  left_join(deaths.latest %>% select(-population))

csa.latest

region.latest = region.latest %>% 
  left_join(
    csa.latest %>% 
      group_by(mapla.region.slug) %>% 
      summarise(deaths = sum(deaths)) %>% 
      ungroup() %>% 
      drop_na()
  ) %>% 
  mutate(rate.deaths = deaths / population * 100000)

csa.latest %>% write_csv('processed/csa-latest.csv', na = '')
region.latest %>% write_csv('processed/region-latest.csv', na = '')
