library(tidyverse)
library(ggrepel)

save.plot = function (ggobj, filename) {
  ggsave(filename, plot = ggobj, width = 12, height = 8)
}

csa.latest = read_csv('processed/csa-latest.csv')

csa.latest

deaths.latest = read_csv('processed/deaths-latest.csv')

deaths.latest

crowding = read_csv('gis-census/csa-crowded.csv')

crowding

prison.hoods = c('Commerce','Lancaster','Lynwood','San Dimas','Boyle Heights','Downtown',
                 'San Pedro','Sylmar','Wholesale District','Castaic','La Verne','Santa Monica Mountains')

countywide.case.rate = csa.latest %>% 
  summarise(cases = sum(cases), population = sum(population)) %>% 
  mutate(rate.cases = cases / population * 100000) %>% 
  pull(rate.cases)

csa.crowding.data = csa.latest %>% 
  left_join(crowding %>% rename(csa.hood.name = hood.name)) %>% 
  left_join(deaths.latest) %>% 
  mutate(pct.crowded = pct.crowded * 100)

csa.crowding.data

countywide.crowding.rate = csa.crowding.data %>% 
  summarise(crowded = sum(crowded), total = sum(total)) %>% 
  mutate(pct.crowded = crowded / total * 100) %>% 
  pull(pct.crowded)

crowding.model = lm(
  rate.cases ~ pct.crowded,
  data = csa.crowding.data %>% filter(is.finite(rate.cases))
)

summary(crowding.model)

plot.crowding.csa = csa.crowding.data %>%
  filter(population > 1000) %>%
  filter(!csa.hood.name %in% c('Castaic', 'Wholesale District')) %>%
  ggplot(aes(pct.crowded, rate.cases, size = population)) +
  geom_hline(yintercept = countywide.case.rate, linetype = 2) +
  geom_vline(xintercept = countywide.crowding.rate, linetype = 2) +
  annotate('text', x = 37.5, y = countywide.case.rate + 100, label = 'Countywide rate') +
  geom_smooth(method = 'lm', se = FALSE, color = 'salmon') +
  geom_point(color = '#B0B0B0') +
  geom_point(
    data = . %>% 
      filter(mapla.region.slug == 'southeast'),
    color = '#D55E00'
  ) +
  theme_minimal() +
  theme(legend.position = 'none') +
  labs(
    title = 'Crowding and COVID relationship for L.A. CSAs',
    subtitle = 'Southeast L.A. highlighted\nCircles sized by population',
    x = 'Percent of homes crowded in neighborhood',
    y = 'Cases per 1,000 people',
    caption = 'Neighborhoods with population >1,000 only\nExcluding Wholesale District and Castaic which include prison counts'
  )

plot.crowding.csa

plot.crowding.csa %>% save.plot('plots/csa-crowding.png')

region.crowding.data = csa.crowding.data %>% 
  # filter(!csa.hood.name %in% c('Castaic', 'Wholesale District')) %>%
  group_by(mapla.region.slug) %>% 
  summarise(
    cases = sum(cases),
    population = sum(population),
    rate.cases = cases / population * 100000,
    total = sum(total),
    crowded = sum(crowded),
    pct.crowded = crowded / total * 100
  ) %>% 
  filter(!mapla.region.slug %in% c('angeles-forest', 'santa-monica-mountains')) %>% 
  drop_na(mapla.region.slug)

region.crowding.data

plot.crowding.region = region.crowding.data %>%
  ggplot(aes(pct.crowded, rate.cases)) +
  geom_hline(yintercept = countywide.case.rate, linetype = 2) +
  geom_vline(xintercept = countywide.crowding.rate, linetype = 2) +
  annotate('text', x = 20, y = countywide.case.rate + 50, label = 'Countywide rate') +
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
    y = 'Cases per 1,000 people',
    caption = 'Excluding Wholesale District and Castaic which include prison counts'
  )

plot.crowding.region

plot.crowding.region %>% save.plot('plots/region-crowding.png')

region.crowding.data %>% write_csv('processed/region-crowding.csv')
