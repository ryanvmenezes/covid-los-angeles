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

csa = st_read('processed/countywide-statistical-areas-consolidated.geojson', as_tibble = TRUE, stringsAsFactors = FALSE)

csa

la.tracts = get.acs.custom(
  variables = c(
    population = 'B01003_001'
  ),
  geometry = TRUE
)

la.tracts

intersection = csa %>% 
  select(-population) %>% 
  st_transform(3311) %>% 
  st_intersection(
    la.tracts %>% 
      select(tract.fips = GEOID, population = estimate) %>% 
      st_transform(3311) %>% 
      mutate(tract.area = st_area(.))
  ) %>% 
  mutate(
    piece.area = st_area(.),
    pct.of.tract = as.double(piece.area / tract.area)
  )

intersection

csa.tract.xwalk = intersection %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  select(hood.name, tract.fips, pct.of.tract)

csa.tract.xwalk

csa.tract.xwalk %>% write_csv('gis-census/csa-tract-xwalk.csv', na = '')
