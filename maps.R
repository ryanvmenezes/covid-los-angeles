library(sf)
library(glue)
library(tidyverse)

csa = read_sf('processed/countywide-statistical-areas-consolidated.geojson') %>% 
  filter(!hood.name %in% c('San Clemente Island', 'Santa Catalina Island', 'Avalon'))

race = read_csv('gis-census/csa-race.csv')

race

plot.plain.race = csa %>% 
  left_join(race %>% select(hood.name, top.group)) %>% 
  ggplot(aes(fill = top.group)) +
  geom_sf() +
  scale_fill_brewer(palette = 'Dark2')

plot.plain.race

poverty = read_csv('gis-census/csa-poverty.csv')

poverty

daily = read_csv('processed/csa-daily-calcs.csv') %>% 
  filter(!csa.hood.name %in% c('San Clemente Island', 'Santa Catalina Island', 'Avalon')) %>% 
  left_join(
    race %>% 
      select(csa.hood.name = hood.name, top.group)
  )

daily

ntop = 50

freeze.date.1 = '2020-04-15'
freeze.date.2 = '2020-05-15'

top.date.1 = daily %>% 
  filter(date == freeze.date.1) %>% 
  arrange(-case.rate.100k) %>% 
  head(ntop)

top.date.1

top.date.2 = daily %>% 
  filter(date == freeze.date.2) %>% 
  arrange(-case.rate.100k) %>% 
  head(ntop)

top.date.2

top.both.dates = top.date.1 %>% 
  transmute(hood.name = csa.hood.name, top.date = freeze.date.1, top.group) %>% 
  bind_rows(
    top.date.2 %>% 
      transmute(hood.name = csa.hood.name, top.date = freeze.date.2, top.group)
  )

top.both.dates


plot.side.by.side.top.on.date = csa %>% 
  mutate(top.date = freeze.date.1) %>% 
  rbind(
    csa %>% 
      mutate(top.date = freeze.date.2)
  ) %>% 
  left_join(top.both.dates) %>% 
  ggplot() +
  geom_sf(color = 'grey', fill = 'grey') +
  geom_sf(
    data = . %>% 
      filter(!is.na(top.group)),
    aes(fill = top.group),
    color = 'black'
  ) +
  facet_wrap(. ~ top.date) +
  scale_fill_brewer(palette = 'Dark2') +
  ggtitle(glue('Top {ntop} neighborhoods by case rate on date')) +
  theme_minimal()
  

plot.side.by.side.top.on.date

ggsave('plots/map-comparison-top-neighborhoods.png', plot = plot.side.by.side.top.on.date, width = 11, height = 8)
