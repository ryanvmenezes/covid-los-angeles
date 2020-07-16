library(tidyverse)

save.plot = function (ggobj, filename) {
  ggsave(filename, plot = ggobj, width = 12, height = 8)
}

csa.daily = read_csv('processed/csa-daily.csv')

csa.daily

csa.latest = read_csv('processed/csa-latest.csv')

csa.latest

csa.poverty = read_csv('gis-census/csa-poverty.csv')

csa.poverty

csa.poverty %>% 
  ggplot(aes(pct.below.poverty)) +
  geom_density()

csa.poverty = csa.poverty %>% 
  mutate(
    poverty.bin = case_when(
      pct.below.poverty >= 0.25 ~ 'high >25%',
      pct.below.poverty < 0.25 & pct.below.poverty >= 0.05 ~ 'medium',
      pct.below.poverty < 0.05 ~ 'low <5%',
    )
  )

csa.poverty %>% count(poverty.bin)

csa.poverty %>% group_by(poverty.bin) %>% summarise(pop = sum(total))

daily.by.hood.poverty = csa.daily %>% 
  left_join(
    csa.poverty %>% 
      select(-total) %>% 
      rename(csa.hood.name = hood.name)
  ) %>% 
  group_by(poverty.bin, date) %>% 
  summarise(
    cases = sum(cases),
    population = sum(population),
    case.rate.100k = cases / population * 100000
  ) %>% 
  ungroup() #%>% 
  # mutate(
  #   poverty.bin = case_when(
  #     population > 1000000 ~ glue('{poverty.bin} ({round(population / 1000000, 1)}M)'),
  #     population < 1000000 ~ glue('{poverty.bin} ({round(population / 1000)}K)'),
  #   )
  # )

daily.by.hood.poverty

plot.poverty.case.rate = daily.by.hood.poverty %>% 
  ggplot(aes(date, case.rate.100k, color = poverty.bin)) +
  geom_line() +
  labs(color = 'Poverty level\nin neighborhood\n(county total)') +
  xlab('Date') +
  ylab('Cases per 100k') +
  theme_minimal()

plot.poverty.case.rate

plot.poverty.case.rate %>% save.plot('plots/csa-daily-poverty-rates.png')


