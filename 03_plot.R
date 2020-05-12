library(tidyverse)

csa.daily = read_csv('los-angeles/tables/csa-daily-calcs.csv')
csa.daily.latest = read_csv('los-angeles/tables/csa-daily-calcs-latest.csv')
regions.daily = read_csv('los-angeles/tables/region-daily-calcs.csv')
regions.daily.latest = read_csv('los-angeles/tables/region-daily-calcs-latest.csv')

# csa daily counts --------------------------------------------------------

plot.csa.counts.daily = csa.daily %>%
  filter(confirmed.cases > 0) %>% 
  ggplot(aes(date, confirmed.cases, group = hood.name)) +
  geom_line(color = 'grey') +
  geom_line(
    data = . %>%
      semi_join(
        csa.daily.latest %>%
          arrange(-confirmed.cases) %>%
          head(10),
        by = 'hood.name'
      ),
    aes(color = hood.name)
  ) +
  geom_text(
    data = . %>%
      semi_join(
        csa.daily.latest %>%
          arrange(-confirmed.cases) %>%
          head(10),
        by = 'hood.name'
      ) %>%
      group_by(hood.name) %>%
      filter(date == max(date)),
    aes(
      label = hood.name,
      color = hood.name
    ),
    hjust = 'left',
    nudge_x = 0.25
  ) +
  scale_x_date(limits = c(as.Date('2020-03-27'), as.Date('2020-06-01'))) +
  theme_minimal() +
  theme(legend.position = 'none') +
  xlab('Date') +
  ylab('Confirmed cases') +
  ggtitle('L.A. County statistical areas: daily counts')

plot.csa.counts.daily

# plot.csa.log.counts.daily = plot.csa.counts.daily +
#   coord_trans(y = 'log10') +
#   ylab('Confirmed cases (log scale)')
# 
# plot.csa.log.counts.daily

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
  scale_x_date(limits = c(as.Date('2020-03-27'), as.Date('2020-06-01'))) +
  theme_minimal() +
  theme(legend.position = 'none') +
  xlab('Date') +
  ylab('Confirmed cases') +
  ggtitle('L.A. County regions: daily counts')

plot.region.counts.daily

plot.region.log.counts.daily = plot.region.counts.daily +
  coord_trans(y = 'log10') +
  ylab('Confirmed cases (log scale)')

plot.region.log.counts.daily

# csa daily rates ---------------------------------------------------------

plot.csa.rates.daily = csa.daily %>%
  filter(case.rate > 0) %>% 
  ggplot(aes(date, case.rate, group = hood.name)) +
  geom_line(color = 'grey') +
  geom_line(
    data = . %>%
      semi_join(
        csa.daily.latest %>%
          arrange(-case.rate) %>%
          head(10),
        by = 'hood.name'
      ),
    aes(color = hood.name)
  ) +
  geom_text(
    data = . %>%
      semi_join(
        csa.daily.latest %>%
          arrange(-case.rate) %>%
          head(10),
        by = 'hood.name'
      ) %>%
      group_by(hood.name) %>%
      filter(date == max(date)),
    aes(
      label = hood.name,
      color = hood.name
    ),
    hjust = 'left',
    nudge_x = 0.25
  ) +
  scale_x_date(limits = c(as.Date('2020-03-27'), as.Date('2020-06-01'))) +
  theme_minimal() +
  theme(legend.position = 'none') +
  xlab('Date') +
  ylab('Confirmed cases') +
  ggtitle('L.A. County statistical areas: daily case rates')

plot.csa.rates.daily

# region daily rates ------------------------------------------------------

plot.region.rates.daily = regions.daily %>%
  filter(case.rate > 0) %>% 
  ggplot(aes(date, case.rate, group = mapla.region.slug, color = mapla.region.slug)) +
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
  scale_x_date(limits = c(as.Date('2020-03-27'), as.Date('2020-06-01'))) +
  theme_minimal() +
  theme(legend.position = 'none') +
  xlab('Date') +
  ylab('Case rates') +
  ggtitle('L.A. County regions: daily case rates')

plot.region.rates.daily

# csa days since counts ---------------------------------------------------

plot.csa.counts.dayssince = csa.daily %>%
  filter(confirmed.cases > 0) %>% 
  filter(days.since.case.cutoff > 0) %>% 
  ggplot(aes(days.since.case.cutoff, confirmed.cases, group = hood.name)) +
  geom_line(color = 'grey') +
  geom_line(
    data = . %>%
      semi_join(
        csa.daily.latest %>%
          arrange(-confirmed.cases) %>%
          head(10),
        by = 'hood.name'
      ),
    aes(color = hood.name)
  ) +
  geom_text(
    data = . %>%
      semi_join(
        csa.daily.latest %>%
          arrange(-confirmed.cases) %>%
          head(10),
        by = 'hood.name'
      ) %>%
      group_by(hood.name) %>%
      filter(date == max(date)),
    aes(
      label = hood.name,
      color = hood.name
    ),
    hjust = 'left',
    nudge_x = 0.25
  ) +
  scale_x_continuous(limits = c(0, 50)) +
  theme_minimal() +
  theme(legend.position = 'none') +
  xlab('Days since case 50') +
  ylab('Confirmed cases') +
  ggtitle('L.A. County statistical areas: Counts (standardized curves)')

plot.csa.counts.dayssince

# plot.csa.log.counts.dayssince = plot.csa.counts.dayssince +
#   coord_trans(y = 'log10') +
#   ylab('Confirmed cases (log scale)')
# 
# plot.csa.log.counts.dayssince

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
  scale_x_continuous(limits = c(0, 50)) +
  theme_minimal() +
  theme(legend.position = 'none') +
  xlab('Days since case 100') +
  ylab('Confirmed cases') +
  ggtitle('L.A. County regions: Counts (standardized curves)')

plot.region.counts.dayssince

# plot.region.log.counts.dayssince = plot.region.counts.dayssince +
#   coord_trans(y = 'log10') +
#   ylab('Confirmed cases (log scale)')
# 
# plot.region.log.counts.dayssince


# csa days since rates ----------------------------------------------------

plot.csa.rates.dayssince = csa.daily %>%
  filter(confirmed.cases > 0) %>% 
  filter(days.since.case.rate.cutoff > 0) %>% 
  ggplot(aes(days.since.case.rate.cutoff, case.rate, group = hood.name)) +
  geom_line(color = 'grey') +
  geom_line(
    data = . %>%
      semi_join(
        csa.daily.latest %>%
          arrange(-case.rate) %>%
          head(10),
        by = 'hood.name'
      ),
    aes(color = hood.name)
  ) +
  geom_text(
    data = . %>%
      semi_join(
        csa.daily.latest %>%
          arrange(-case.rate) %>%
          head(10),
        by = 'hood.name'
      ) %>%
      group_by(hood.name) %>%
      filter(date == max(date)),
    aes(
      label = hood.name,
      color = hood.name
    ),
    hjust = 'left',
    nudge_x = 0.25
  ) +
  scale_x_continuous(limits = c(0, 50)) +
  theme_minimal() +
  theme(legend.position = 'none') +
  xlab('Days since case rate reached 1 case/1,000 people') +
  ylab('Case rate') +
  ggtitle('L.A. County statistical areas: case rates (standardized curves)')

plot.csa.rates.dayssince


# regions days since rates ------------------------------------------------

plot.region.rates.dayssince = regions.daily %>%
  filter(confirmed.cases > 0) %>% 
  filter(days.since.case.rate.cutoff > 0) %>% 
  ggplot(aes(days.since.case.rate.cutoff, case.rate, color = mapla.region.slug)) +
  geom_line() +
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
  scale_x_continuous(limits = c(0, 40)) +
  theme_minimal() +
  theme(legend.position = 'none') +
  xlab('Days since case rate reached 1 case/2,000 people') +
  ylab('Case rate') +
  ggtitle('L.A. County regions: case rates (standardized curves)')

plot.region.rates.dayssince


# regions grid cases ------------------------------------------------------

plot.region.grid.counts.dayssince = csa.daily %>%
  filter(!mapla.region.slug %in% c('angeles-forest', 'santa-monica-mountains', NA)) %>% 
  filter(days.since.case.cutoff >= 0) %>% 
  ggplot(aes(days.since.case.cutoff, confirmed.cases, group = hood.name)) +
  geom_line(color = 'grey') +
  geom_line(
    data = . %>% 
      semi_join(
        csa.daily.latest %>%
          group_by(mapla.region.slug) %>%
          arrange(-confirmed.cases) %>%
          top_n(2, wt = confirmed.cases),
        by = 'hood.name'
      ),
    aes(color = hood.name)
  ) +
  geom_text(
    data = . %>% 
      semi_join(
        csa.daily.latest %>%
          group_by(mapla.region.slug) %>%
          arrange(-confirmed.cases) %>%
          top_n(2, wt = confirmed.cases),
        by = 'hood.name'
      ) %>% 
      group_by(hood.name) %>%
      filter(date == max(date)),
    aes(
      label = hood.name,
      color = hood.name
    ),
    hjust = 'left',
    nudge_x = 0.25,
    size = 3
  ) +
  scale_x_continuous(limits = c(0, 60)) +
  # coord_trans(y = 'log10') +
  facet_wrap(. ~ mapla.region.slug) +
  ggtitle('L.A. County regions (standardized curves)') +
  xlab('Days since case 50') +
  ylab('Confirmed cases (log scale)') +
  theme_minimal() +
  theme(legend.position = 'none')

plot.region.grid.counts.dayssince


# # regions grid case rates -------------------------------------------------
# 
# plot.regions.case.rate.grid = csa.daily %>%
#   filter(!mapla.region.slug %in% c('angeles-forest', 'santa-monica-mountains', NA)) %>% 
#   filter(days.since.case.rate.cutoff >= 0) %>% 
#   ggplot(aes(days.since.case.rate.cutoff, case.rate, group = hood.name)) +
#   geom_line(color = 'grey') +
#   geom_line(
#     data = . %>% 
#       semi_join(
#         csa.daily.latest %>%
#           group_by(mapla.region.slug) %>%
#           arrange(-case.rate) %>%
#           top_n(2, wt = case.rate),
#         by = 'hood.name'
#       ),
#     aes(color = hood.name)
#   ) +
#   geom_text(
#     data = . %>% 
#       semi_join(
#         csa.daily.latest %>%
#           group_by(mapla.region.slug) %>%
#           arrange(-case.rate) %>%
#           top_n(2, wt = case.rate),
#         by = 'hood.name'
#       ) %>% 
#       group_by(hood.name) %>%
#       filter(date == max(date)),
#     aes(
#       label = hood.name,
#       color = hood.name
#     ),
#     hjust = 'left',
#     nudge_x = 0.25,
#     size = 3
#   ) +
#   scale_x_continuous(limits = c(0, 60)) +
#   # coord_trans(y = 'log10') +
#   facet_wrap(. ~ mapla.region.slug) +
#   ggtitle('L.A. County Regions') +
#   xlab('Days since case 50') +
#   ylab('Confirmed cases (log scale)') +
#   theme_minimal() +
#   theme(legend.position = 'none')
# 
# plot.regions.case.rate.grid


# custom ------------------------------------------------------------------

csa.daily %>% 
  filter(hood.name %in% c('Bel Air', 'Beverly Crest', 'Brentwood', 'Pico-Union', 'Vermont Square', 'Westlake')) %>% 
  filter(days.since.case.rate.cutoff >= 0) %>%
  ggplot(aes(days.since.case.rate.cutoff, case.rate, color = hood.name)) +
  geom_line() +
  geom_text(
    data = . %>%
      group_by(hood.name) %>% 
      filter(days.since.case.rate.cutoff == max(days.since.case.rate.cutoff)),
    aes(label = hood.name),
    hjust = 'left',
    nudge_x = 0.25
  ) +
  scale_x_continuous(limits = c(0, 45)) +
  theme_minimal() +
  theme(legend.position = 'none')

csa.daily %>% 
  filter(hood.name %in% c('Bel Air', 'Beverly Crest', 'Brentwood', 'Pico-Union', 'Vermont Square', 'Westlake')) %>% 
  # filter(days.since.case.rate.cutoff >= 0) %>%
  ggplot(aes(date, case.rate, color = hood.name)) +
  geom_line() +
  geom_text(
    data = . %>%
      group_by(hood.name) %>% 
      filter(date == max(date)),
    aes(label = hood.name),
    hjust = 'left',
    nudge_x = 0.25
  ) +
  # scale_x_continuous(limits = c(0, 45)) +
  theme_minimal() +
  theme(legend.position = 'none')


ggsave('los-angeles/plots/csa-counts-daily.png', plot = plot.csa.counts.daily, width = 12, height = 8)
ggsave('los-angeles/plots/csa-counts-dayssince.png', plot = plot.csa.counts.dayssince, width = 12, height = 8)
ggsave('los-angeles/plots/csa-rates-daily.png', plot = plot.csa.rates.daily, width = 12, height = 8)
ggsave('los-angeles/plots/csa-rates-dayssince.png', plot = plot.csa.rates.dayssince, width = 12, height = 8)
ggsave('los-angeles/plots/region-counts-daily.png', plot = plot.region.counts.daily, width = 12, height = 8)
ggsave('los-angeles/plots/region-log-counts-daily.png', plot = plot.region.log.counts.daily, width = 12, height = 8)
ggsave('los-angeles/plots/region-counts-dayssince.png', plot = plot.region.counts.dayssince, width = 12, height = 8)
ggsave('los-angeles/plots/region-rates-daily.png', plot = plot.region.rates.daily, width = 12, height = 8)
ggsave('los-angeles/plots/region-rates-dayssince.png', plot = plot.region.rates.dayssince, width = 12, height = 8)
ggsave('los-angeles/plots/region-grid-counts-dayssince.png', plot = plot.region.grid.counts.dayssince, width = 12, height = 8)
