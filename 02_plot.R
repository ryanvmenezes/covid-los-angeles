library(glue)
library(lubridate)
library(tidyverse)

datasets.names = c('csa.daily', 'csa.latest', 'csa.recent.daily', 'csa.recent.latest',
                   'region.daily', 'region.latest', 'region.recent.daily', 'region.recent.latest')

datasets = datasets.names %>% 
  setNames(datasets.names) %>%
  map(~read_csv(glue('processed/{str_replace_all(.x, "\\\\.", "-")}.csv')))

datasets

min.date = ymd('2020-03-27')
max.date = today() + 15
two.weeks.ago.date = today() - 14

save.plot = function (ggobj, filename) {
  ggsave(filename, plot = ggobj, width = 12, height = 8)
}

# daily cases by csa

plot.csa.counts = datasets$csa.daily %>%
  ggplot(aes(date, cases, group = csa.hood.name)) +
  geom_line(color = 'grey', alpha = 0.3) +
  geom_line(
    data = . %>%
      semi_join(
        datasets$csa.latest %>%
          arrange(-cases) %>%
          head(10),
        by = 'csa.hood.name'
      ),
    aes(color = csa.hood.name)
  ) +
    geom_text(
    data = . %>%
      semi_join(
        datasets$csa.latest %>%
          arrange(-cases) %>%
          head(10),
        by = 'csa.hood.name'
      ) %>%
      group_by(csa.hood.name) %>%
      filter(date == max(date)),
    aes(
      label = csa.hood.name,
      color = csa.hood.name
    ),
    hjust = 'left',
    nudge_x = 0.25
  ) +
  scale_x_date(limits = c(min.date, max.date)) +
  theme_minimal() +
  theme(legend.position = 'none') +
  labs(
    title = 'L.A. County statistical areas: daily counts',
    x = 'Date',
    y = 'Cases'
  )

plot.csa.counts

plot.csa.counts %>% save.plot('plots/csa-counts.png')

# daily case counts by region

plot.region.counts = datasets$region.daily %>%
  filter(cases > 0) %>% 
  filter(mapla.region.slug != 'angeles-forest') %>% 
  ggplot(aes(date, cases, group = mapla.region.slug, color = mapla.region.slug)) +
  geom_line() +
  geom_text(
    data = . %>% 
      group_by(mapla.region.slug) %>% 
      filter(date == max(date)),
    aes(label = mapla.region.slug),
    hjust = 'left',
    nudge_x = 0.25
  ) +
  scale_x_date(limits = c(as.Date('2020-03-27'), max.date)) +
  theme_minimal() +
  theme(legend.position = 'none') +
  labs(
    title = 'L.A. Couty regions: cases',
    x = 'Date',
    y = 'Cases'
  )

plot.region.counts

plot.region.counts %>% save.plot('plots/region-counts.png')

# daily rates by region

plot.region.rates = datasets$region.daily %>%
  ggplot(aes(date, case.rate.100k, group = mapla.region.slug, color = mapla.region.slug)) +
  geom_line() +
  geom_text(
    data = . %>% 
      group_by(mapla.region.slug) %>% 
      filter(date == max(date)),
    aes(label = mapla.region.slug),
    hjust = 'left',
    nudge_x = 0.25
  ) +
  scale_x_date(limits = c(as.Date('2020-03-27'), max.date)) +
  theme_minimal() +
  theme(legend.position = 'none') +
  labs(
    title = 'L.A. County regions: case rates',
    x = 'Date',
    y = 'Cases per 100k people'
  )

plot.region.rates

plot.region.rates %>% save.plot('plots/region-rates.png')

# daily counts by csa, on a grid of regions

plot.grid.counts = datasets$csa.daily %>%
  filter(!mapla.region.slug %in% c('angeles-forest', 'santa-monica-mountains', NA)) %>%
  ggplot(aes(date, cases, group = csa.hood.name)) +
  geom_line(color = 'grey') +
  geom_line(
    data = . %>% 
      semi_join(
        datasets$csa.latest %>%
          group_by(mapla.region.slug) %>%
          arrange(-cases) %>%
          top_n(2, wt = cases),
        by = 'csa.hood.name'
      ),
    aes(color = csa.hood.name)
  ) +
  geom_text(
    data = . %>% 
      semi_join(
        datasets$csa.latest %>%
          group_by(mapla.region.slug) %>%
          arrange(-cases) %>%
          top_n(2, wt = cases),
        by = 'csa.hood.name'
      ) %>% 
      group_by(csa.hood.name) %>%
      filter(date == max(date)),
    aes(
      label = csa.hood.name,
      color = csa.hood.name
    ),
    hjust = 'left',
    nudge_x = 0.25,
    size = 3
  ) +
  scale_x_date(limits = c(as.Date('2020-03-27'), max.date + 15)) +
  facet_wrap(. ~ mapla.region.slug) +
  theme_minimal() +
  theme(legend.position = 'none') +
  labs(
    title = 'L.A. County regions',
    x = 'Date',
    y = 'Cases'
  )

plot.grid.counts

plot.grid.counts %>% save.plot('plots/grid-counts-csa-region.png')

# last two weeks count by region

plot.csa.counts.recent = datasets$csa.recent.daily %>%
  ggplot(aes(date, recent.cases, group = csa.hood.name)) +
  geom_line(color = 'grey', alpha = 0.3) +
  geom_line(
    data = . %>%
      semi_join(
        datasets$csa.recent.latest %>%
          arrange(-recent.cases) %>%
          head(10),
        by = 'csa.hood.name'
      ),
    aes(color = csa.hood.name)
  ) +
  geom_text(
    data = . %>%
      semi_join(
        datasets$csa.recent.latest %>%
          arrange(-recent.cases) %>%
          head(10),
        by = 'csa.hood.name'
      ) %>%
      group_by(csa.hood.name) %>%
      filter(date == max(date)),
    aes(
      label = csa.hood.name,
      color = csa.hood.name
    ),
    hjust = 'left',
    nudge_x = 0.25
  ) +
  scale_x_date(limits = c(two.weeks.ago.date, two.weeks.ago.date + 17)) +
  theme_minimal() +
  theme(legend.position = 'none') +
  labs(
    title = 'L.A. County statistical areas: cases in last two weeks',
    subtitle = glue('Since {today() - 14}'),
    x = 'Date',
    y = 'Cases'
  )

plot.csa.counts.recent

plot.csa.counts.recent %>% save.plot('plots/csa-counts-recent.png')

# last two weeks counts by region

plot.region.counts.recent = datasets$region.recent.daily %>%
  ggplot(aes(date, recent.cases, group = mapla.region.slug)) +
  geom_line(color = 'grey', alpha = 0.3) +
  geom_line(
    data = . %>%
      semi_join(
        datasets$region.recent.latest %>%
          arrange(-recent.cases) %>%
          head(10),
        by = 'mapla.region.slug'
      ),
    aes(color = mapla.region.slug)
  ) +
  geom_text(
    data = . %>%
      semi_join(
        datasets$region.recent.latest %>%
          arrange(-recent.cases) %>%
          head(10),
        by = 'mapla.region.slug'
      ) %>%
      group_by(mapla.region.slug) %>%
      filter(date == max(date)),
    aes(
      label = mapla.region.slug,
      color = mapla.region.slug
    ),
    hjust = 'left',
    nudge_x = 0.25
  ) +
  scale_x_date(limits = c(two.weeks.ago.date, two.weeks.ago.date + 17)) +
  theme_minimal() +
  theme(legend.position = 'none') +
  labs(
    title = 'L.A. County regions: cases in last two weeks',
    subtitle = glue('Since {today() - 14}'),
    x = 'Date',
    y = 'Cases'
  )

plot.region.counts.recent

plot.region.counts.recent %>% save.plot('plots/region-counts-recent.png')

# daily rates by region, last two weeks

plot.region.rates.recent = datasets$region.recent.daily %>%
  ggplot(aes(date, recent.case.rate.100k, group = mapla.region.slug)) +
  geom_line(color = 'grey', alpha = 0.3) +
  geom_line(
    data = . %>%
      semi_join(
        datasets$region.recent.latest %>%
          arrange(-recent.case.rate.100k) %>%
          head(10),
        by = 'mapla.region.slug'
      ),
    aes(color = mapla.region.slug)
  ) +
  geom_text(
    data = . %>%
      semi_join(
        datasets$region.recent.latest %>%
          arrange(-recent.case.rate.100k) %>%
          head(10),
        by = 'mapla.region.slug'
      ) %>%
      group_by(mapla.region.slug) %>%
      filter(date == max(date)),
    aes(
      label = mapla.region.slug,
      color = mapla.region.slug
    ),
    hjust = 'left',
    nudge_x = 0.25
  ) +
  scale_x_date(limits = c(two.weeks.ago.date, two.weeks.ago.date + 17)) +
  theme_minimal() +
  theme(legend.position = 'none') +
  labs(
    title = 'L.A. County regions: case rate in last two weeks',
    subtitle = glue('Since {today() - 14}'),
    x = 'Date',
    y = 'Cases per 100k people'
  )

plot.region.rates.recent

plot.region.rates.recent %>% save.plot('plots/region-rates-recent.png')
