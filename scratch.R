
# # csa days since rates ----------------------------------------------------
# 
# plot.csa.rates.dayssince = csa.daily %>%
#   filter(confirmed.cases > 0) %>% 
#   filter(days.since.case.rate.cutoff > 0) %>% 
#   ggplot(aes(days.since.case.rate.cutoff, case.rate, group = csa.hood.name)) +
#   geom_line(color = 'grey') +
#   geom_line(
#     data = . %>%
#       semi_join(
#         csa.daily.latest %>%
#           arrange(-case.rate) %>%
#           head(10),
#         by = 'csa.hood.name'
#       ),
#     aes(color = csa.hood.name)
#   ) +
#   geom_text(
#     data = . %>%
#       semi_join(
#         csa.daily.latest %>%
#           arrange(-case.rate) %>%
#           head(10),
#         by = 'csa.hood.name'
#       ) %>%
#       group_by(csa.hood.name) %>%
#       filter(date == max(date)),
#     aes(
#       label = csa.hood.name,
#       color = csa.hood.name
#     ),
#     hjust = 'left',
#     nudge_x = 0.25
#   ) +
#   scale_x_continuous(limits = c(0, 50)) +
#   theme_minimal() +
#   theme(legend.position = 'none') +
#   xlab('Days since case rate reached 1 case/1,000 people') +
#   ylab('Case rate') +
#   ggtitle('L.A. County statistical areas: case rates (standardized curves)')
# 
# plot.csa.rates.dayssince



# # regions grid case rates -------------------------------------------------
# 
# plot.regions.case.rate.grid = csa.daily %>%
#   filter(!mapla.region.slug %in% c('angeles-forest', 'santa-monica-mountains', NA)) %>% 
#   filter(days.since.case.rate.cutoff >= 0) %>% 
#   ggplot(aes(days.since.case.rate.cutoff, case.rate, group = csa.hood.name)) +
#   geom_line(color = 'grey') +
#   geom_line(
#     data = . %>% 
#       semi_join(
#         csa.daily.latest %>%
#           group_by(mapla.region.slug) %>%
#           arrange(-case.rate) %>%
#           top_n(2, wt = case.rate),
#         by = 'csa.hood.name'
#       ),
#     aes(color = csa.hood.name)
#   ) +
#   geom_text(
#     data = . %>% 
#       semi_join(
#         csa.daily.latest %>%
#           group_by(mapla.region.slug) %>%
#           arrange(-case.rate) %>%
#           top_n(2, wt = case.rate),
#         by = 'csa.hood.name'
#       ) %>% 
#       group_by(csa.hood.name) %>%
#       filter(date == max(date)),
#     aes(
#       label = csa.hood.name,
#       color = csa.hood.name
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

# csa.daily %>% 
#   filter(csa.hood.name %in% c('Bel Air', 'Beverly Crest', 'Brentwood', 'Pico-Union', 'Vermont Square', 'Westlake')) %>% 
#   filter(days.since.case.rate.cutoff >= 0) %>%
#   ggplot(aes(days.since.case.rate.cutoff, case.rate, color = csa.hood.name)) +
#   geom_line() +
#   geom_text(
#     data = . %>%
#       group_by(csa.hood.name) %>% 
#       filter(days.since.case.rate.cutoff == max(days.since.case.rate.cutoff)),
#     aes(label = csa.hood.name),
#     hjust = 'left',
#     nudge_x = 0.25
#   ) +
#   scale_x_continuous(limits = c(0, 45)) +
#   theme_minimal() +
#   theme(legend.position = 'none')
# 
# csa.daily %>% 
#   filter(csa.hood.name %in% c('Bel Air', 'Beverly Crest', 'Brentwood', 'Pico-Union', 'Vermont Square', 'Westlake')) %>% 
#   # filter(days.since.case.rate.cutoff >= 0) %>%
#   ggplot(aes(date, case.rate, color = csa.hood.name)) +
#   geom_line() +
#   geom_text(
#     data = . %>%
#       group_by(csa.hood.name) %>% 
#       filter(date == max(date)),
#     aes(label = csa.hood.name),
#     hjust = 'left',
#     nudge_x = 0.25
#   ) +
#   # scale_x_continuous(limits = c(0, 45)) +
#   theme_minimal() +
#   theme(legend.position = 'none')
