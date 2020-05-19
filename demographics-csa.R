library(glue)
library(tidyverse)

csa.daily = read_csv('processed/csa-daily-calcs.csv')

csa.daily

csa.race = read_csv('gis-census/csa-race.csv')

csa.race

csa.race %>% count(top.group, is.majority)
csa.race %>% count(top.group)
csa.race %>% group_by(top.group) %>% summarise(pop = sum(total))

daily.by.hood.race = csa.daily %>% 
  left_join(
    csa.race %>% 
      select(-total) %>% 
      select(csa.hood.name = hood.name, top.race = top.group, is.majority)
  ) %>% 
  group_by(top.race, date) %>% 
  summarise(
    cases = sum(confirmed.cases),
    population = sum(population),
    case.rate.100k = cases / population * 100000
  ) %>% 
  ungroup() %>% 
  mutate(
    top.race = case_when(
      population > 1000000 ~ glue('{top.race} ({round(population / 1000000, 1)}M)'),
      population < 1000000 ~ glue('{top.race} ({round(population / 1000)}K)'),
    )
  )

daily.by.hood.race

daily.by.hood.race %>% distinct(top.race, population)

plot.race.cases = daily.by.hood.race %>% 
  ggplot(aes(date, cases, color = top.race)) +
  geom_line() +
  labs(color = 'Top race\nin neighborhood\n(county total)') +
  ylab('Cases') +
  xlab('Date') +
  theme_minimal()

plot.race.cases

plot.race.case.rate = daily.by.hood.race %>% 
  ggplot(aes(date, case.rate.100k, color = top.race)) +
  geom_line() +
  labs(color = 'Top race\nin neighborhood\n(county total)') +
  ylab('Cases per 100K') +
  xlab('Date') +
  theme_minimal()

plot.race.case.rate

csa.poverty = read_csv('gis-census/csa-poverty.csv')

csa.poverty

csa.poverty %>% 
  ggplot(aes(pct.below.poverty)) +
  geom_density()

csa.poverty = csa.poverty %>% 
  mutate(
    poverty.bin = case_when(
      pct.below.poverty >= 0.15 ~ 'high',
      pct.below.poverty < 0.15 & pct.below.poverty >= 0.05 ~ 'medium',
      pct.below.poverty < 0.05 ~ 'low',
    )
  )

csa.poverty %>% count(poverty.bin)

daily.by.hood.poverty = csa.daily %>% 
  left_join(
    csa.poverty %>% 
      select(-total) %>% 
      rename(csa.hood.name = hood.name)
  ) %>% 
  group_by(poverty.bin, date) %>% 
  summarise(
    cases = sum(confirmed.cases),
    population = sum(population),
    case.rate.100k = cases / population * 100000
  ) %>% 
  ungroup() %>% 
  mutate(
    poverty.bin = case_when(
      population > 1000000 ~ glue('{poverty.bin} ({round(population / 1000000, 1)}M)'),
      population < 1000000 ~ glue('{poverty.bin} ({round(population / 1000)}K)'),
    )
  )

daily.by.hood.poverty

plot.poverty.cases = daily.by.hood.poverty %>% 
  ggplot(aes(date, cases, color = poverty.bin)) +
  geom_line() +
  labs(color = 'Poverty level\nin neighborhood\n(county total)') +
  xlab('Date') +
  ylab('Cases') +
  theme_minimal()

plot.poverty.cases

plot.poverty.case.rate = daily.by.hood.poverty %>% 
  ggplot(aes(date, case.rate.100k, color = poverty.bin)) +
  geom_line() +
  labs(color = 'Poverty level\nin neighborhood\n(county total)') +
  xlab('Date') +
  ylab('Cases per 100k') +
  theme_minimal()

plot.poverty.case.rate



ggsave(filename = 'plots/csa-daily-race-counts.png', plot = plot.race.cases, width = 11, height = 8)
ggsave(filename = 'plots/csa-daily-race-rates.png', plot = plot.race.case.rate, width = 11, height = 8)
ggsave(filename = 'plots/csa-daily-poverty-counts.png', plot = plot.poverty.cases, width = 11, height = 8)
ggsave(filename = 'plots/csa-daily-poverty-rates.png', plot = plot.poverty.case.rate, width = 11, height = 8)
