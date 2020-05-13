library(tidyverse)

region.daily = read_csv('processed/region-daily-calcs.csv')

westside.split = region.daily %>% 
  mutate(region = if_else(mapla.region.slug == 'westside', 'westside', 'not-westside')) %>% 
  group_by(region, date) %>% 
  summarise(
    confirmed.cases = sum(confirmed.cases),
    population = sum(population),
    case.rate.100k = confirmed.cases / population * 100000
  )

westside.split %>% distinct(region, population)

plot.rate.compare = westside.split %>%
  ggplot(aes(date, case.rate.100k, color = region)) +
  geom_line() +
  scale_x_date(limits = c(as.Date('2020-03-27'), as.Date('2020-06-01'))) +
  geom_text(
    data = . %>%
      group_by(region) %>%
      filter(date == max(date)),
    aes(label = region),
    hjust = 'left',
    nudge_x = 0.25
  ) +
  xlab('Date') +
  ylab('Cases per 100k') +
  theme_minimal() +
  theme(legend.position = 'none')

plot.rate.compare

ggsave(file = 'plots/westside-rate-compare.png', plot = plot.rate.compare, width = 11, height = 8)
