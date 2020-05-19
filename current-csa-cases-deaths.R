library(tidyverse)

county.summary = read_csv('raw/city_community_table.csv')

county.summary

hood.list = read_csv('processed/countywide-statistical-areas-cleaned-list.csv')

hood.list

csa.race = read_csv('gis-census/csa-race.csv')

csa.race

csa.poverty = read_csv('gis-census/csa-poverty.csv')

csa.poverty


hood.list %>% 
  select(csa.hood.name, original.label = original.labels) %>% 
  separate_rows(original.label, sep = ' \\| ')

csa.hoods.now = county.summary %>% 
  rename(original.label = geo_merge) %>% 
  left_join(
    hood.list %>% 
      select(csa.hood.name, original.label = original.labels) %>% 
      separate_rows(original.label, sep = ' \\| ')
  ) %>% 
  group_by(csa.hood.name) %>% 
  summarise(
    cases = sum(cases_final),
    deaths = sum(deaths_final),
    persons.tested = sum(persons_tested_final),
    population = sum(population)
  ) %>% 
  bind_rows(
    tribble(
      ~csa.hood.name, ~cases, ~deaths, ~persons.tested, ~population,
      'Long Beach', 1157, 51, NA_real_, 467354,
      'Pasadena', 607, 69, NA_real_, 138101,
    )
  ) %>% 
  mutate(
    case.rate.100k = cases / population * 100000,
    death.rate.100k = deaths / population * 100000,
  ) %>% 
  left_join(
    csa.race %>% 
      select(csa.hood.name = hood.name, top.group, is.majority)
  ) %>% 
  left_join(
    csa.poverty %>%
      select(csa.hood.name = hood.name, pct.below.poverty) %>% 
      mutate(
        poverty.bin = case_when(
          pct.below.poverty >= 0.15 ~ 'high',
          pct.below.poverty >= 0.05 & pct.below.poverty < 0.15 ~ 'medium',
          pct.below.poverty < 0.05 ~ 'low'
        )
      )
  )

csa.hoods.now

csa.hoods.now %>% 
  ggplot(aes(cases, deaths, size = population)) +
  geom_point()

csa.hoods.now %>% 
  # filter(cases > 0) %>% 
  # filter(deaths > 0) %>% 
  ggplot(aes(cases, deaths, size = population)) +
  facet_wrap(. ~ top.group) +
  # coord_trans(x = 'log10', y = 'log10') +
  geom_point() +
  theme_minimal()


csa.hoods.now %>% 
  # filter(case.rate.100k > 0) %>% 
  # filter(death.rate.100k > 0) %>% 
  ggplot(aes(case.rate.100k, death.rate.100k, size = population)) +
  geom_point() +
  facet_wrap(. ~ top.group) +
  scale_x_continuous(limits = c(0, 750)) +
  scale_y_continuous(limits = c(0, 100)) +
  # coord_trans(x = 'log10', y = 'log10') +
  theme_minimal()
