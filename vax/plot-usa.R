library(tidyverse)
library(tidycensus)

cdc = read_csv('vax/cdc/cdc-2021-02-01-13-55.csv', skip = 3, na = 'N/A')

cdc
 
# state.pop = get_acs(variables = 'B01003_001', geography = 'state', year = 2018)
# 
# state.pop

state.codes = fips_codes %>%
  as_tibble() %>% 
  distinct(state.code = state, state = state_name)

state.codes

cdc.clean = cdc %>% 
  transmute(
    state = `State/Territory/Federal Entity`,
    distributed = `Total Distributed`,
    administered = `Total Administered`,
    dpp = `Distributed per 100K`,
    app = `Administered per 100K`,
    popest = distributed / (dpp / 100000),
    dosed = `People with 1+ Doses`,
    dosed2 = `People with 2 Doses`,
    dosed.pct = `People with 1+ Doses` / popest,
    supply.per.pop = distributed / popest,
    dose.usage = administered / distributed,
    avg.supply.per.pop = sum(distributed, na.rm = TRUE) / sum(popest, na.rm = TRUE),
    avg.dose.usage = sum(administered, na.rm = TRUE) / sum(distributed, na.rm = TRUE)
  ) %>% 
  left_join(state.codes) %>% 
  # filter(is.na(state.code) & !is.na(popest))
  mutate(
    state.code = case_when(
      state == 'Federated States of Micronesia' ~ 'FSM',
      state == 'Marshall Islands' ~ 'MAR',
      state == 'New York State' ~ 'NY',
      state == 'Republic of Palau' ~ 'PAL',
      state == 'Virgin Islands' ~ 'USVI',
      state.code == 'AS' ~ 'ASAM',
      state.code == 'MP' ~ 'NMI',
      state.code == 'GU' ~ 'GUAM',
      TRUE ~ state.code
    )
  )

cdc.clean

# 1: where the vaccine is being delivered

plot.pop.dist = cdc.clean %>% 
  filter(nchar(state.code) == 2) %>% 
  ggplot(aes(popest, distributed, label = state.code)) + 
  geom_text() +
  geom_point(data = . %>% filter(state.code == 'CA'), color = 'red') +
  geom_abline(slope = 0.05, linetype = 2) +
  geom_abline(slope = 0.10, linetype = 2) +
  geom_abline(slope = 0.15, linetype = 2) +
  labs(
    title = 'Where the vaccine is going',
    x = 'Population',
    subtitle = 'Number of doses distributed',
    y = ''
  ) +
  annotate(geom = 'text', x = 3e7, y = 4.5e6, label = '15%', color = 'red', size = 7) +
  annotate(geom = 'text', x = 3.5e7, y = 3.5e6, label = '10%', color = 'red', size = 7) +
  annotate(geom = 'text', x = 3e7, y = 1.5e6, label = '5%', color = 'red', size = 7) +
  theme_minimal()

plot.pop.dist

# 2: how much the delivered doses are being used

plot.del.admin = cdc.clean %>% 
  filter(nchar(state.code) == 2) %>% 
  ggplot(aes(distributed, administered, label = state.code)) + 
  geom_text() +
  geom_point(data = . %>% filter(state.code == 'CA'), color = 'red') +
  geom_abline(slope = 1, linetype = 2) +
  geom_abline(slope = 0.5, linetype = 2) +
  geom_abline(slope = 0.25, linetype = 2) +
  labs(
    title = 'How much delivered doses are being used',
    x = 'Doses delivered',
    subtitle = 'Number of doses administered (compared to delivered)',
    y = ''
  ) +
  annotate(geom = 'text', x = 1.5e6, y = 15e5, label = '100%', color = 'red', size = 7) +
  annotate(geom = 'text', x = 3.5e6, y = 175e4, label = '50%', color = 'red', size = 7) +
  annotate(geom = 'text', x = 4e6, y = 1e6, label = '25%', color = 'red', size = 7) +
  theme_minimal()

plot.del.admin

# 3: how much of the population has been given the vax

plot.pop.dosed = cdc.clean %>% 
  filter(nchar(state.code) == 2) %>% 
  ggplot(aes(popest, dosed, label = state.code)) + 
  geom_text() +
  geom_point(data = . %>% filter(state.code == 'CA'), color = 'red') +
  geom_abline(slope = 0.05, linetype = 2) +
  geom_abline(slope = 0.02, linetype = 2) +
  geom_abline(slope = 0.10, linetype = 2) +
  labs(
    title = 'Who\'s getting the vaccine',
    x = 'Population',
    subtitle = 'People receiving first dose',
    y = ''
  ) +
  annotate(geom = 'text', x = 1.5e7, y = 15e5, label = '10%', color = 'red', size = 7) +
  annotate(geom = 'text', x = 3.5e7, y = 17.5e5, label = '5%', color = 'red', size = 7) +
  annotate(geom = 'text', x = 2.5e7, y = 5e5, label = '2%', color = 'red', size = 7) +
  theme_minimal()

plot.pop.dosed

plot.pop.dist.log = plot.pop.dist +
  coord_trans(x = 'log10', y = 'log10') 

plot.pop.dist.log

plot.del.admin.log = plot.del.admin +
  coord_trans(x = 'log10', y = 'log10')

plot.del.admin.log

plot.pop.dosed.log = plot.pop.dosed +
  coord_trans(x = 'log10', y = 'log10')

plot.pop.dosed.log

ggsave(filename = 'vax/plots/01-distribution.png', plot = plot.pop.dist, width = 11, height = 8)
ggsave(filename = 'vax/plots/02-delivery.png', plot = plot.del.admin, width = 11, height = 8)
ggsave(filename = 'vax/plots/03-distribution.png', plot = plot.pop.dosed, width = 11, height = 8)

cdc.clean %>% 
  filter(nchar(state.code) == 2) %>% 
  ggplot(aes(supply.per.pop, dose.usage, label = state.code, size = popest)) +
  # geom_text() +
  geom_point() +
  geom_point(data = . %>% filter(state.code == 'CA'), color = 'red') +
  geom_vline(aes(xintercept = avg.supply.per.pop)) +
  geom_hline(aes(yintercept = avg.dose.usage))


# states = get_acs(variables = 'B01001_001', geography = 'state', geometry = TRUE)
# 
# states
# 
# states %>% 
#   left_join(cdc.clean, by = c('NAME' = 'state')) %>% 
#   filter(!state.code %in% c('HI', 'AK')) %>% 
#   ggplot() +
#   geom_sf(aes(fill = dose.usage)) 

cdc.clean %>% 
  filter(nchar(state.code) == 2) %>% 
  mutate(pct.dosed = dosed / popest) %>% 
  arrange(dpp) %>% 
  mutate(state.code = fct_inorder(state.code)) %>% 
  ggplot(aes(state.code, dpp)) +
  geom_bar(stat = 'identity') +
  geom_bar(stat = 'identity', data = . %>% filter(state.code == 'CA'), fill = 'orange') +
  coord_flip()

cdc.clean %>% 
  filter(nchar(state.code) == 2) %>% 
  mutate(pct.dosed = dosed / popest) %>% 
  arrange(dose.usage) %>% 
  mutate(state.code = fct_inorder(state.code)) %>% 
  ggplot(aes(state.code, dose.usage)) +
  geom_bar(stat = 'identity') +
  geom_bar(stat = 'identity', data = . %>% filter(state.code == 'CA'), fill = 'orange') +
  coord_flip()

cdc.clean %>% 
  filter(nchar(state.code) == 2) %>% 
  mutate(pct.dosed = dosed / popest) %>% 
  arrange(pct.dosed) %>% 
  mutate(state.code = fct_inorder(state.code)) %>% 
  ggplot(aes(state.code, pct.dosed)) +
  geom_bar(stat = 'identity') +
  geom_bar(stat = 'identity', data = . %>% filter(state.code == 'CA'), fill = 'orange') +
  coord_flip()


# cdc.clean %>% 
#   filter(nchar(state.code) == 2) %>% 
#   ggplot(aes(popest, administered, label = state.code)) + 
#   geom_text() +
#   geom_point(data = . %>% filter(state.code == 'CA'), color = 'red') +
#   scale_x_log10() +
#   scale_y_log10() +
#   # coord_trans(x = 'log10', y = 'log10') +
#   geom_smooth(method = 'lm', se = FALSE) +
#   labs(
#     title = 'How much of the vaccine is being utilized',
#     x = 'Doses distributed (log)',
#     y = 'Doses administered (log)'
#   ) +
#   theme_minimal()
# 
# 
# cdc.clean %>% 
#   filter(nchar(state.code) == 2) %>% 
#   ggplot(aes(popest, distributed, label = state.code)) + 
#   geom_text() +
#   geom_point(data = . %>% filter(state.code == 'CA'), color = 'red') +
#   scale_x_log10() +
#   scale_y_log10() +
#   # coord_trans(x = 'log10', y = 'log10') +
#   geom_smooth(method = 'lm', se = FALSE) +
#   labs(
#     title = 'Where the vaccine is going',
#     x = 'Population (log)',
#     y = 'Doses administered (log)'
#   ) +
#   theme_minimal()


cdc.clean %>% 
  filter(nchar(state.code) == 2) %>%
  ggplot(aes(supply.per.pop, dose.usage, label = state.code, size = popest)) +
  # geom_text() +
  geom_point() +
  geom_point(data = . %>% filter(state.code == 'CA'), color = 'red') +
  labs(
    x = 'Supply per capita',
    y = 'Percent of doses administered'
  ) +
  theme_minimal()


cdc.clean %>% 
  # filter(nchar(state.code) == 2) %>%
  ggplot(aes(dose.usage, dosed.pct, label = state.code, size = popest, label = state.code)) +
  geom_point(color = 'grey') +
  geom_point(data = . %>% filter(state.code == 'CA'), color = 'red') +
  labs(
    x = 'Percent of doses administered',
    y = 'Percent of population receiving one dose'
  ) +
  geom_text(aes(size = NULL)) +
  # geom_hline(yintercept = 0.85) +
  theme_minimal()
