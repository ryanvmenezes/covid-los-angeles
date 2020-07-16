library(tidyverse)
library(sf)
library(glue)
library(lubridate)

csa = read_sf('processed/countywide-statistical-areas-consolidated.geojson') %>% 
  filter(!hood.name %in% c('San Clemente Island', 'Santa Catalina Island', 'Avalon'))

csa

csa.df = csa %>%
  st_set_geometry(NULL) %>% 
  arrange(-population)

csa.df

race = read_csv('gis-census/csa-race.csv')

race

plot.plain.race = csa %>% 
  left_join(race %>% select(hood.name, top.group)) %>% 
  mutate(
    alpha = population / 220000,
    alpha = if_else(alpha > 1, 1, alpha),
  ) %>% 
  ggplot(aes(fill = top.group, color = top.group, alpha = alpha)) +
  geom_sf(
    size = 0.3
  ) +
  scale_fill_brewer(palette = 'Dark2') +
  scale_color_brewer(palette = 'Dark2') +
  theme_minimal()

plot.plain.race

daily = read_csv('processed/csa-daily.csv') %>% 
  filter(!csa.hood.name %in% c('San Clemente Island', 'Santa Catalina Island', 'Avalon'))

daily

freeze.dates = ymd(c('2020-04-01', '2020-05-01', '2020-06-01', '2020-07-14'))

top.on.date = daily %>% 
  filter(date %in% freeze.dates)

top.on.date

top.n.on.date = top.on.date %>%
  group_by(date) %>%
  nest() %>%
  mutate(data = map(data, ~.x %>% arrange(-rate.cases) %>% head(50))) %>%
  unnest(data) %>% 
  ungroup()

top.n.on.date

plot.top.hood.comparison = csa %>%
  right_join(top.n.on.date) %>%
  ggplot() +
  geom_sf(data = csa, color = 'grey', fill = 'grey') +
  geom_sf(fill = 'dodgerblue', size = 0.1) +
  facet_wrap(. ~ date) +
  theme_minimal() +
  labs(
    title = glue('Top 50 neighborhoods by case rate on date')
  )

ggsave('plots/map-top-hoods-time.png', plot = plot.top.hood.comparison, width = 12, height = 8)

plot.top.hood.comparison



# time.plots = tibble(freeze.dates) %>% 
#   mutate(
#     data = map(
#       freeze.dates,
#       ~daily %>%
#         filter(date == .x) %>% 
#         mutate(
#           rate.new.cases.7dayavg = case_when(
#             rate.new.cases.7dayavg < 0 ~ 0,
#             rate.new.cases.7dayavg > 100 ~ 100,
#             TRUE ~ rate.new.cases.7dayavg
#           )
#         )
#     ),
#     plot = map(
#       data,
#       ~csa %>% 
#         left_join(.x) %>% 
#         filter(rate.new.cases.7dayavg >= 0) %>% 
#         ggplot() +
#         geom_sf(aes(fill = rate.new.cases.7dayavg), size = 0) +
#         scale_fill_gradient(low = 'dodgerblue', high = 'red') +
#         theme_minimal()
#     )
#   )
# 
# time.plots
# 
# time.plots$plot
# 




# csa %>%
#   right_join(top.both.dates) %>% 
#   st_transform(3311) %>%
#   st_centroid() %>% 
#   mutate(
#     x = st_coordinates(.)[,1],
#     y = st_coordinates(.)[,2]
#   ) %>% 
#   ggplot() +
#   geom_sf(data = st_transform(csa, 3311), color = 'grey', fill = 'grey') +
#   geom_point(aes(x, y, size = population), color = 'black', fill = NA, shape = 21) +
#   facet_wrap(. ~ date) +
#   theme_minimal()
  

wt.average.lt.ln = csa %>% 
  # st_transform(3311) %>% 
  st_centroid() %>% 
  mutate(
    x = st_coordinates(.)[,1],
    y = st_coordinates(.)[,2],
  ) %>% 
  st_set_geometry(NULL) %>% 
  right_join(daily) %>% 
  mutate(
    wt.x = x * cases,
    wt.y = y * cases,
  ) %>% 
  group_by(date) %>% 
  summarise(
    cases = sum(cases),
    wt.avg.x = sum(wt.x) / sum(cases),
    wt.avg.y = sum(wt.y) / sum(cases),
  )

wt.average.lt.ln

csa.df

wt.average.lt.ln %>% 
  ggplot() +
  geom_sf(data = csa, color = 'grey', fill = 'grey') +
  geom_line(aes(wt.avg.x, wt.avg.y)) +
  geom_point(aes(wt.avg.x, wt.avg.y)) +
  theme_minimal()
