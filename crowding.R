library(tidyverse)
library(ggrepel)
library(broom)

save.plot = function (ggobj, filename) {
  ggsave(filename, plot = ggobj, width = 12, height = 8)
}

csa.latest = read_csv('processed/csa-latest.csv')

csa.latest

deaths.latest = read_csv('processed/deaths-latest.csv')

deaths.latest

crowding = read_csv('gis-census/csa-crowded.csv')

crowding

area = read_csv('gis-census/area.csv')

area

prison.hoods = c('Commerce','Lancaster','Lynwood','San Dimas','Boyle Heights','Downtown',
                 'San Pedro','Sylmar','Wholesale District','Castaic','La Verne','Santa Monica Mountains')

countywide.case.rate = csa.latest %>% 
  summarise(cases = sum(cases), population = sum(population)) %>% 
  mutate(rate.cases = cases / population * 100000) %>% 
  pull(rate.cases)

csa.crowding.data = csa.latest %>% 
  left_join(crowding %>% rename(csa.hood.name = hood.name, total.homes = total)) %>% 
  left_join(deaths.latest) %>% 
  left_join(area %>% select(csa.hood.name = hood.name, area, density)) %>% 
  mutate(pct.crowded = pct.crowded * 100)

csa.crowding.data

countywide.crowding.rate = csa.crowding.data %>% 
  summarise(
    crowded = sum(crowded),
    total.homes = sum(total.homes),
  ) %>% 
  mutate(pct.crowded = crowded / total.homes * 100) %>% 
  pull(pct.crowded)

crowding.model = lm(
  rate.cases ~ pct.crowded,
  data = csa.crowding.data %>% filter(is.finite(rate.cases)) %>% filter(!csa.hood.name %in% prison.hoods) %>% filter(population >= 1000)
)

summary(crowding.model)

tidy(crowding.model) %>% 
  write_csv('processed/crowding-model-csa.csv')

plot.crowding.csa = csa.crowding.data %>%
  filter(population > 1000) %>%
  filter(!csa.hood.name %in% c('Castaic', 'Wholesale District')) %>%
  ggplot(aes(pct.crowded, rate.cases, size = population)) +
  geom_hline(yintercept = countywide.case.rate, linetype = 2) +
  geom_vline(xintercept = countywide.crowding.rate, linetype = 2) +
  annotate('text', x = 37.5, y = countywide.case.rate + 200, label = 'Countywide rate') +
  geom_smooth(method = 'lm', se = FALSE, color = 'salmon') +
  geom_point(color = '#B0B0B0') +
  geom_point(
    data = . %>% 
      filter(mapla.region.slug == 'san-fernando-valley'),
    color = '#D55E00'
  ) +
  theme_minimal() +
  theme(legend.position = 'none') +
  labs(
    title = 'Crowding and COVID relationship for L.A. CSAs',
    subtitle = 'San Fernando Valley highlighted\nCircles sized by population',
    x = 'Percent of homes crowded in neighborhood',
    y = 'Cases per 1,000 people',
    caption = 'Neighborhoods with population >1,000 only\nExcluding Wholesale District and Castaic which include prison counts'
  )

plot.crowding.csa

plot.crowding.csa %>% save.plot('plots/csa-crowding.png')

countywide.density = csa.crowding.data %>% 
  summarise(
    area = sum(area),
    population = sum(population),
  ) %>% 
  mutate(density = population / area) %>% 
  pull(density)

plot.density.csa = csa.crowding.data %>%
  filter(population > 1000) %>%
  filter(!csa.hood.name %in% c('Castaic', 'Wholesale District')) %>%
  ggplot(aes(density, rate.cases, size = population)) +
  geom_hline(yintercept = countywide.case.rate, linetype = 2) +
  geom_vline(xintercept = countywide.density, linetype = 2) +
  annotate('text', x = 30000, y = countywide.case.rate + 200, label = 'Countywide rate') +
  geom_smooth(method = 'lm', se = FALSE, color = 'salmon') +
  geom_point(color = '#B0B0B0') +
  geom_point(
    data = . %>% 
      filter(mapla.region.slug == 'san-fernando-valley'),
    color = '#D55E00'
  ) +
  theme_minimal() +
  theme(legend.position = 'none') +
  labs(
    title = 'Crowding and COVID relationship for L.A. CSAs',
    subtitle = 'Southeast L.A. highlighted\nCircles sized by population',
    x = 'Population density',
    y = 'Cases per 1,000 people',
    caption = 'Neighborhoods with population >1,000 only\nExcluding Wholesale District and Castaic which include prison counts'
  )

plot.density.csa


region.crowding.data = csa.crowding.data %>% 
  # filter(!csa.hood.name %in% c('Castaic', 'Wholesale District')) %>%
  group_by(mapla.region.slug) %>% 
  summarise(
    cases = sum(cases),
    population = sum(population),
    rate.cases = cases / population * 100000,
    total.homes = sum(total.homes),
    crowded = sum(crowded),
    pct.crowded = crowded / total.homes * 100,
    area = sum(area),
    density = population / area
  ) %>% 
  filter(!mapla.region.slug %in% c('angeles-forest', 'santa-monica-mountains')) %>%
  drop_na(mapla.region.slug)

region.crowding.data

crowding.model.region = lm(
  rate.cases ~ pct.crowded,
  data = region.crowding.data
)

summary(crowding.model.region)

tidy(crowding.model.region) %>% 
  write_csv('processed/crowding-model-region.csv')

plot.crowding.region = region.crowding.data %>%
  ggplot(aes(pct.crowded, rate.cases)) +
  geom_hline(yintercept = countywide.case.rate, linetype = 2) +
  geom_vline(xintercept = countywide.crowding.rate, linetype = 2) +
  annotate('text', x = 20, y = countywide.case.rate + 100, label = 'Countywide rate') +
  geom_smooth(method = 'lm', se = FALSE, color = 'salmon') +
  geom_point() +
  geom_text_repel(aes(label = mapla.region.slug)) +
  # geom_point(color = '#B0B0B0') +
  # geom_point(
  #   data = . %>% 
  #     filter(mapla.region.slug == 'southeast'),
  #   color = '#D55E00'
  # ) +
  theme_minimal() +
  theme(legend.position = 'none') +
  labs(
    title = 'Crowding and COVID relationship for L.A. County regions',
    # subtitle = 'Southeast L.A. highlighted\nCircles sized by population',
    x = 'Percent of homes crowded in neighborhood',
    y = 'Cases per 1,000 people'
  )

plot.crowding.region

plot.crowding.region %>% save.plot('plots/region-crowding.png')

region.crowding.data %>% write_csv('processed/region-crowding.csv')

csa.crowding.data %>% write_csv('processed/csa-crowding.csv')

# ggplot(mapping = aes(pct.crowded, rate.cases)) +
#   geom_point(
#     data = csa.crowding.data %>% filter(is.finite(rate.cases)) %>% filter(!csa.hood.name %in% prison.hoods) %>% filter(population >= 1000)
#   ) +
#   geom_smooth(
#     data = csa.crowding.data %>% filter(is.finite(rate.cases)) %>% filter(!csa.hood.name %in% prison.hoods) %>% filter(population >= 1000),
#     method = 'lm',se = FALSE, color = 'black'
#   ) + 
#   geom_point(data = region.crowding.data, color = 'salmon') +
#   geom_smooth(data = region.crowding.data, method = 'lm',se = FALSE, color = 'salmon') +
#   theme_minimal()


