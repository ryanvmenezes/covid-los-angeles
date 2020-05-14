library(sf)
library(tidyverse)
library(tidycensus)

options(tigris_use_cache = TRUE)

get.acs.custom = function(...) {
  get_acs(
    geography = 'tract',
    state = '06',
    county = '037',
    year = 2018,
    cache_table = TRUE,
    ...
  )
}

xwalk = read_csv('gis-census/csa-tract-xwalk.csv')

xwalk

# race --------------------------------------------------------------------

race = get.acs.custom(
  variables = c(
    total = 'B03002_001', # total
    white = 'B03002_003', # not.hisp.white
    black = 'B03002_004', # not.hisp.black
    other = 'B03002_005', # not.hisp.am.ind
    asian = 'B03002_006', # not.hisp.asian
    other = 'B03002_007', # not.hisp.hawaiian.pacisl
    other = 'B03002_008', # not.hisp.other
    other = 'B03002_009', # not.hisp.two.more
    latino = 'B03002_012' # hisp
  )
)

csa.race = race %>% 
  group_by(tract.fips = GEOID, variable) %>% 
  summarise(estimate = sum(estimate)) %>% 
  left_join(xwalk) %>% 
  mutate(app.estimate = pct.of.tract * estimate) %>% 
  group_by(hood.name, variable) %>% 
  summarise(estimate = sum(app.estimate)) %>% 
  pivot_wider(names_from = 'variable', values_from = 'estimate') %>% 
  select(hood.name, total, everything()) %>% 
  mutate_at(vars(-hood.name, -total), ~. / total) %>% 
  rename_at(vars(-hood.name, -total), ~str_c('pct.', .)) %>% 
  ungroup() %>% 
  filter(!is.na(hood.name))

top.group = csa.race %>% 
  select(-total) %>% 
  pivot_longer(-hood.name, names_to = 'group', values_to = 'pct') %>% 
  group_by(hood.name) %>% 
  top_n(n = 1, wt = pct) %>% 
  ungroup() %>% 
  transmute(
    hood.name,
    top.group = str_replace(group, 'pct.', ''),
    pct,
    is.majority = pct > 0.5
  )

csa.race = csa.race %>% 
  left_join(
    top.group %>% select(-pct)
  )

csa.race

csa.race %>% write_csv('gis-census/csa-race.csv', na = '')

# poverty -----------------------------------------------------------------

poverty = get.acs.custom(
  variables = c(
    total = 'B17001_001',
    below.poverty = 'B17001_002'
  )
)

csa.poverty = poverty %>% 
  select(tract.fips = GEOID, variable, estimate) %>% 
  left_join(xwalk) %>% 
  mutate(app.estimate = pct.of.tract * estimate) %>% 
  group_by(hood.name, variable) %>% 
  summarise(estimate = sum(app.estimate)) %>% 
  pivot_wider(names_from = 'variable', values_from = 'estimate') %>% 
  ungroup() %>%  
  transmute(
    hood.name,
    total,
    pct.below.poverty = below.poverty / total
  )

csa.poverty

csa.poverty %>% write_csv('gis-census/csa-poverty.csv', na = '')
