library(glue)
library(tidyverse)

case.cutoff = 50
case.rate.cutoff = 100

csa.daily = read_csv('processed/csa-daily-calcs.csv') %>% 
  rename(
    days.since.case.cutoff = glue('days.since.{case.cutoff}.cases'),
    days.since.case.rate.cutoff = glue('days.since.{case.rate.cutoff}.cases.per.100k')
  )

csa.daily.latest = read_csv('processed/csa-daily-calcs-latest.csv') %>%
  rename(
    days.since.case.cutoff = glue('days.since.{case.cutoff}.cases'),
    days.since.case.rate.cutoff = glue('days.since.{case.rate.cutoff}.cases.per.100k')
  )

regions.daily = read_csv('processed/region-daily-calcs.csv') %>% 
  rename(
    days.since.case.cutoff = glue('days.since.{case.cutoff * 2}.cases'),
    days.since.case.rate.cutoff = glue('days.since.{case.rate.cutoff}.cases.per.100k')
  )

regions.daily.latest = read_csv('processed/region-daily-calcs-latest.csv') %>%
  rename(
    days.since.case.cutoff = glue('days.since.{case.cutoff * 2}.cases'),
    days.since.case.rate.cutoff = glue('days.since.{case.rate.cutoff}.cases.per.100k')
  )

max.date = as.Date('2020-06-01')
max.days.since = 70

# csa daily counts --------------------------------------------------------

plot.csa.counts.daily = csa.daily %>%
  filter(confirmed.cases > 0) %>% 
  ggplot(aes(date, confirmed.cases, group = csa.hood.name)) +
  geom_line(color = 'grey', alpha = 0.3) +
  geom_line(
    data = . %>%
      semi_join(
        csa.daily.latest %>%
          arrange(-confirmed.cases) %>%
          head(10),
        by = 'csa.hood.name'
      ),
    aes(color = csa.hood.name)
  ) +
  geom_text(
    data = . %>%
      semi_join(
        csa.daily.latest %>%
          arrange(-confirmed.cases) %>%
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
  geom_hline(yintercept = case.cutoff, linetype = "dotted") +
  scale_x_date(limits = c(as.Date('2020-03-27'), max.date)) +
  theme_minimal() +
  theme(legend.position = 'none') +
  xlab('Date') +
  ylab('Confirmed cases') +
  ggtitle('L.A. County statistical areas: daily counts')

plot.csa.counts.daily

# region daily counts -----------------------------------------------------

plot.region.counts.daily = regions.daily %>%
  filter(confirmed.cases > 0) %>% 
  filter(mapla.region.slug != 'angeles-forest') %>% 
  ggplot(aes(date, confirmed.cases, group = mapla.region.slug, color = mapla.region.slug)) +
  geom_line() +
  geom_text(
    data = . %>% 
      group_by(mapla.region.slug) %>% 
      filter(date == max(date)),
    aes(label = mapla.region.slug),
    hjust = 'left',
    nudge_x = 0.25
  ) +
  geom_hline(yintercept = case.cutoff * 2, linetype = 'dotted') +
  scale_x_date(limits = c(as.Date('2020-03-27'), max.date)) +
  theme_minimal() +
  theme(legend.position = 'none') +
  xlab('Date') +
  ylab('Confirmed cases') +
  ggtitle('L.A. County regions: daily counts')

plot.region.counts.daily

# csa daily rates ---------------------------------------------------------

plot.csa.rates.daily = csa.daily %>%
  filter(case.rate.100k > 0) %>% 
  ggplot(aes(date, case.rate.100k, group = csa.hood.name)) +
  geom_line(color = 'grey') +
  geom_line(
    data = . %>%
      semi_join(
        csa.daily.latest %>%
          arrange(-case.rate.100k) %>%
          head(10),
        by = 'csa.hood.name'
      ),
    aes(color = csa.hood.name)
  ) +
  geom_text(
    data = . %>%
      semi_join(
        csa.daily.latest %>%
          arrange(-case.rate.100k) %>%
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
  geom_hline(yintercept = case.rate.cutoff, linetype = 'dotted') +
  scale_x_date(limits = c(as.Date('2020-03-27'), max.date)) +
  theme_minimal() +
  theme(legend.position = 'none') +
  xlab('Date') +
  ylab('Cases per 100K people') +
  ggtitle('L.A. County statistical areas: daily case rates')

plot.csa.rates.daily

# region daily rates ------------------------------------------------------

plot.region.rates.daily = regions.daily %>%
  filter(case.rate.100k > 0) %>% 
  ggplot(aes(date, case.rate.100k, group = mapla.region.slug, color = mapla.region.slug)) +
  geom_line() +
  geom_line(
    data = . %>% 
      filter(mapla.region.slug == 'westside'),
    size = 1.4
  ) +
  geom_text(
    data = . %>% 
      group_by(mapla.region.slug) %>% 
      filter(date == max(date)),
    aes(label = mapla.region.slug),
    hjust = 'left',
    nudge_x = 0.25
  ) +
  geom_hline(yintercept = case.rate.cutoff, linetype = 'dotted') +
  scale_x_date(limits = c(as.Date('2020-03-27'), max.date)) +
  theme_minimal() +
  theme(legend.position = 'none') +
  xlab('Date') +
  ylab('Cases per 100k people') +
  ggtitle('L.A. County regions: daily case rates')

plot.region.rates.daily

# csa days since counts ---------------------------------------------------

plot.csa.counts.dayssince = csa.daily %>%
  filter(confirmed.cases > 0) %>%
  filter(days.since.case.cutoff > 0) %>%
  ggplot(aes(days.since.case.cutoff, confirmed.cases, group = csa.hood.name)) +
  geom_line(color = 'grey') +
  geom_line(
    data = . %>%
      semi_join(
        csa.daily.latest %>%
          arrange(-confirmed.cases) %>%
          head(10),
        by = 'csa.hood.name'
      ),
    aes(color = csa.hood.name)
  ) +
  geom_text(
    data = . %>%
      semi_join(
        csa.daily.latest %>%
          arrange(-confirmed.cases) %>%
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
  scale_x_continuous(limits = c(0, max.days.since)) +
  theme_minimal() +
  theme(legend.position = 'none') +
  xlab(glue('Days since case {case.cutoff}')) +
  ylab('Confirmed cases') +
  ggtitle('L.A. County statistical areas: Counts (standardized curves)')

plot.csa.counts.dayssince

# region days since counts ------------------------------------------------

plot.region.counts.dayssince = regions.daily %>%
  filter(confirmed.cases > 0) %>%
  filter(days.since.case.cutoff > 0) %>%
  ggplot(aes(days.since.case.cutoff, confirmed.cases, group = mapla.region.slug, color = mapla.region.slug)) +
  geom_line() +
  geom_text(
    data = . %>%
      group_by(mapla.region.slug) %>%
      filter(date == max(date)),
    aes(label = mapla.region.slug),
    hjust = 'left',
    nudge_x = 0.25
  ) +
  scale_x_continuous(limits = c(0, max.days.since)) +
  theme_minimal() +
  theme(legend.position = 'none') +
  xlab(glue('Days since case {case.cutoff * 2}')) +
  ylab('Confirmed cases') +
  ggtitle('L.A. County regions: Counts (standardized curves)')

plot.region.counts.dayssince


# regions days since rates ------------------------------------------------

plot.region.rates.dayssince = regions.daily %>%
  filter(confirmed.cases > 0) %>% 
  filter(days.since.case.rate.cutoff > 0) %>% 
  ggplot(aes(days.since.case.rate.cutoff, case.rate.100k, color = mapla.region.slug)) +
  geom_line() +
  geom_line(
    data = . %>% 
      filter(mapla.region.slug == 'westside'),
    size = 1.4
  ) +
  geom_text(
    data = . %>%
      group_by(mapla.region.slug) %>%
      filter(date == max(date)),
    aes(
      label = mapla.region.slug,
      color = mapla.region.slug
    ),
    hjust = 'left',
    nudge_x = 0.25
  ) +
  scale_x_continuous(limits = c(0, max.days.since)) +
  theme_minimal() +
  theme(legend.position = 'none') +
  xlab(glue('Days since case rate reached {case.rate.cutoff} case per 100K people')) +
  ylab('Cases per 100k people') +
  ggtitle('L.A. County regions: case rates (standardized curves)')

plot.region.rates.dayssince


# regions grid cases ------------------------------------------------------

plot.region.grid.counts.dayssince = csa.daily %>%
  filter(!mapla.region.slug %in% c('angeles-forest', 'santa-monica-mountains', NA)) %>% 
  filter(days.since.case.cutoff >= 0) %>% 
  ggplot(aes(days.since.case.cutoff, confirmed.cases, group = csa.hood.name)) +
  geom_line(color = 'grey') +
  geom_line(
    data = . %>% 
      semi_join(
        csa.daily.latest %>%
          group_by(mapla.region.slug) %>%
          arrange(-confirmed.cases) %>%
          top_n(2, wt = confirmed.cases),
        by = 'csa.hood.name'
      ),
    aes(color = csa.hood.name)
  ) +
  geom_text(
    data = . %>% 
      semi_join(
        csa.daily.latest %>%
          group_by(mapla.region.slug) %>%
          arrange(-confirmed.cases) %>%
          top_n(2, wt = confirmed.cases),
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
  scale_x_continuous(limits = c(0, max.days.since)) +
  # coord_trans(y = 'log10') +
  facet_wrap(. ~ mapla.region.slug) +
  ggtitle('L.A. County regions (standardized curves)') +
  xlab(glue('Days since case {case.cutoff}')) +
  ylab('Confirmed cases') +
  theme_minimal() +
  theme(legend.position = 'none')

plot.region.grid.counts.dayssince


ggsave('plots/csa-counts-daily.png', plot = plot.csa.counts.daily, width = 12, height = 8)
ggsave('plots/region-counts-daily.png', plot = plot.region.counts.daily, width = 12, height = 8)
ggsave('plots/csa-rates-daily.png', plot = plot.csa.rates.daily, width = 12, height = 8)
ggsave('plots/region-rates-daily.png', plot = plot.region.rates.daily, width = 12, height = 8)

ggsave('plots/csa-counts-dayssince.png', plot = plot.csa.counts.dayssince, width = 12, height = 8)
ggsave('plots/region-counts-dayssince.png', plot = plot.region.counts.dayssince, width = 12, height = 8)
ggsave('plots/region-rates-dayssince.png', plot = plot.region.rates.dayssince, width = 12, height = 8)

ggsave('plots/region-grid-counts-dayssince.png', plot = plot.region.grid.counts.dayssince, width = 12, height = 8)
