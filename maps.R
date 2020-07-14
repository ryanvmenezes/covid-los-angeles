library(sf)
library(glue)
library(tidyverse)

csa = read_sf('processed/countywide-statistical-areas-consolidated.geojson') %>% 
  filter(!hood.name %in% c('San Clemente Island', 'Santa Catalina Island', 'Avalon'))

csa

race = read_csv('gis-census/csa-race.csv')

race

plot.plain.race = csa %>% 
  left_join(race %>% select(hood.name, top.group)) %>% 
  ggplot(aes(fill = top.group)) +
  geom_sf() +
  scale_fill_brewer(palette = 'Dark2') +
  theme_minimal()

plot.plain.race

poverty = read_csv('gis-census/csa-poverty.csv')

poverty

daily = read_csv('processed/csa-daily.csv') %>% 
  filter(!csa.hood.name %in% c('San Clemente Island', 'Santa Catalina Island', 'Avalon')) %>% 
  left_join(
    race %>% 
      select(csa.hood.name = hood.name, top.group)
  )

daily

ntop = 50

top.both.dates = daily %>% 
  filter(
    date == '2020-04-15'|
      date == '2020-05-15' |
      date == '2020-06-15'
  ) %>% 
  group_by(date) %>% 
  nest() %>% 
  mutate(data = map(data, ~.x %>% arrange(-case.rate.100k) %>% head(ntop))) %>% 
  unnest(data)

top.both.dates

plot.top.hood.comparison = csa %>%
  right_join(top.both.dates) %>% 
  ggplot() +
  geom_sf(data = csa, color = 'grey', fill = 'grey') +
  geom_sf(fill = 'red', color = NA) +
  facet_wrap(. ~ date) +
  theme_minimal() +
  labs(
    title = glue('Top {ntop} neighborhoods by case rate on date')
  )

plot.top.hood.comparison

ggsave('plots/map-top-hoods-time.png', plot = plot.top.hood.comparison, width = 12, height = 8)

csa %>%
  right_join(top.both.dates) %>% 
  st_transform(3311) %>%
  st_centroid() %>% 
  mutate(
    x = st_coordinates(.)[,1],
    y = st_coordinates(.)[,2]
  ) %>% 
  ggplot() +
  geom_sf(data = st_transform(csa, 3311), color = 'grey', fill = 'grey') +
  geom_point(aes(x, y, size = population), color = 'black', fill = NA, shape = 21) +
  facet_wrap(. ~ date) +
  theme_minimal()
  
