library(tidyverse)
library(sf)

stats = read_csv('vax/la-county-vaccinated-hoods.csv')

stats

age = read_csv('gis-census/csa-age-65.csv')

age

shapes = read_sf('processed/countywide-statistical-areas-consolidated.geojson')

shapes

stats.cleaned = stats %>% 
  rename(hood = 1, vax = 2, pct.vax = 3) %>% 
  mutate(
    hood = str_remove(hood, 'City of '),
    hood = str_remove(hood, 'Los Angeles - '),
    hood = str_remove(hood, 'Unincorporated - '),
  ) %>% 
  mutate(vax = replace_na(as.numeric(vax), 0)) %>% 
  group_by(hood) %>% 
  summarise(vax = sum(vax))

stats.cleaned

stats.joined = stats.cleaned %>% 
  rename(hood.name = hood) %>% 
  full_join(age) %>%
  drop_na(hood.name) %>% 
  mutate(pct.vax = vax / total)

stats.joined

stats.joined %>% 
  filter(pct.vax < 0.5) %>%
  ggplot(aes(pct.gte65, pct.vax)) +
  geom_point() +
  geom_smooth(method = 'lm')

model = lm(pct.vax ~ pct.gte65, data = stats.joined)

model

stats.joined %>% 
  mutate(res = resid(model)) %>% 
  arrange(res) %>% 
  filter(pct.vax < 0.5) %>%
  ggplot(aes(pct.gte65, pct.vax)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  geom_text(
    data = . %>% filter(abs(res) > .05, total > 5000),
    aes(label = hood.name)
  )

shapes %>% 
  left_join(stats.joined) %>% 
  mutate(pct.vax = if_else(pct.vax > .4, .4, pct.vax)) %>% 
  ggplot() +
  geom_sf(aes(fill = pct.vax), lwd = 0) +
  labs(
    title = 'pct vaccinated'
  )

shapes %>% 
  left_join(stats.joined) %>% 
  # mutate(pct.vax = if_else(pct.vax > .4, .4, pct.vax)) %>% 
  ggplot() +
  geom_sf(aes(fill = pct.gte65), lwd = 0) +
  labs(
    title = 'pct 65+'
  )



shapes %>% 
  left_join(
    stats.joined %>% 
      select(hood.name, pct.gte65, pct.vax) %>% 
      pivot_longer(-hood.name)
  ) %>% 
  ggplot() + 
  geom_sf(aes(fill = value), lwd = 0) +
  facet_wrap(. ~ name)
  


