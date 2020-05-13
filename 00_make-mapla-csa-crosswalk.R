library(sf)
library(jsonlite)
library(tidyverse)

# import ------------------------------------------------------------------

## shapefile of countywide statistical areas

CSA = st_read('raw/los-angeles-countywide-statistical-areas.geojson', as_tibble = TRUE, stringsAsFactors = FALSE)

CSA

# CSA %>% ggplot() + geom_sf()

## shapefile of mapping la neighborhoods

MAPLA = st_read('raw/la-county-neighborhoods-v6.geojson', as_tibble = TRUE, stringsAsFactors = FALSE)

MAPLA

# MAPLA %>% ggplot() + geom_sf()

## a recent pull of covid data from the la county shiny dashboard
## needed for neighborhood populations according to the county

CSA.POP = read_csv('raw/city_community_table.csv')

CSA.POP

## populations of mapping la neighborhoods (2017 ACS)

MAPLA.POP = read_csv('raw/mapla_race_2017.csv')

MAPLA.POP

## crosswalk from mapping la neighborhood to mapping la region

MAPLA.XWALK = read_csv('raw/nsa_list.csv')

MAPLA.XWALK

# clean up mapping la populations -----------------------------------------

mapla.hood.pop = MAPLA.XWALK %>% 
  select(hood.name = nsa, region.name = rsa) %>% 
  left_join(
    MAPLA.POP %>% 
      group_by(hood.name = community_parent) %>% 
      summarise(population = sum(tot_pop_univ))
  ) %>% 
  mutate(
    region.slug = str_to_lower(region.name),
    region.slug = str_replace_all(region.slug, ' ', '-'),
    region.slug = str_replace_all(region.slug, '\\.', ''),
  )

mapla.hood.pop

mapla.region.pop = mapla.hood.pop %>% 
  group_by(region.name, region.slug) %>% 
  summarise(population = sum(population))

mapla.region.pop %>% arrange(-population)

# clean up csa population file --------------------------------------------

csa.pop = CSA.POP %>% 
  select(label = geo_merge, population) %>% 
  bind_rows(
    ## manually add these cities missing from population list
    tribble(
      ~label, ~population,
      'City of Long Beach', 467354,
      'City of Pasadena', 138101,
      'City of Avalon', 3723,
    )
  ) %>% 
  arrange(-population)

csa.pop

# clean mapping la --------------------------------------------------------

mapla.hoods = MAPLA %>%
  transmute(
    hood.name = name,
    county = map_chr(metadata, ~fromJSON(.)$county),
    region = map_chr(metadata, ~fromJSON(.)$region),
  ) %>% 
  filter(county == 'los-angeles') %>% 
  select(-county)

mapla.hoods

# mapla.hoods %>% ggplot() + geom_sf()

# consolidate countywide statistical areas --------------------------------

csa.name.xwalk = CSA %>% 
  st_set_geometry(NULL) %>% 
  select(label) %>%
  transmute(
    label,
    hood.name = str_replace(label, 'City of ', ''),
    hood.name = str_replace(hood.name, 'Unincorporated - ', ''),
    hood.name = str_replace(hood.name, 'Los Angeles - ', ''),
    la.hood = str_detect(label, 'Los Angeles - '),
    uninc.hood = str_detect(label, 'Unincorporated - '),
    inc.city = str_detect(label, 'City of')
  )

csa.name.xwalk

csa.hoods.list = csa.name.xwalk %>% 
  left_join(csa.pop) %>% 
  group_by(hood.name) %>% 
  summarise(
    has.la.hood = any(la.hood),
    has.uninc.hood = any(uninc.hood),
    has.inc.city = any(inc.city),
    count.original = n(),
    original.labels = str_c(label, collapse = ' | '),
    population = sum(population, na.rm = TRUE)
  ) %>% 
  mutate(
    hood.type = case_when(
      has.la.hood ~ 'segment-of-a-city',
      has.inc.city ~ 'standalone-city',
      has.uninc.hood ~ 'unincorporated-area',
      TRUE ~ 'OTHER'
    )
  )

csa.hoods.list

## geospatial dissolve

csa.hoods = CSA %>% 
  left_join(csa.pop) %>%
  mutate(
    hood.name = str_replace(label, 'City of ', ''),
    hood.name = str_replace(hood.name, 'Unincorporated - ', ''),
    hood.name = str_replace(hood.name, 'Los Angeles - ', ''),
  ) %>% 
  group_by(hood.name) %>% 
  summarise(population = sum(population, na.rm = TRUE))

csa.hoods

# csa.hoods %>% ggplot() + geom_sf()

# spatial join ------------------------------------------------------------

## reproject into meters to do area calculations

mapla.hoods.m = mapla.hoods %>%
  st_transform(3311) %>% 
  transmute(
    mapla.hood.name = hood.name,
    mapla.region.slug = region,
    mapla.area = as.double(st_area(.))
  )

mapla.hoods.m

csa.hoods.m = csa.hoods %>% 
  st_transform(3311) %>% 
  transmute(
    csa.hood.name = hood.name,
    csa.area = as.double(st_area(.))
  )

csa.hoods.m

intersected = st_intersection(mapla.hoods.m, csa.hoods.m) %>% 
  mutate(
    piece.area = as.double(st_area(.)),
    pct.mapla = piece.area / mapla.area,
    pct.csa = piece.area / csa.area,
  )

intersected

## plot to compare differences in hoods under csa interpretation vs mapping la interpretation

# hood = 'Los Feliz'
# 
# single.hood = intersected %>%
#   filter(csa.hood.name == hood | mapla.hood.name == hood) %>% 
#   mutate(
#     side = case_when(
#       mapla.hood.name == hood & csa.hood.name == hood ~ 'both',
#       mapla.hood.name == hood & csa.hood.name != hood ~ 'mapla',
#       mapla.hood.name != hood & csa.hood.name == hood ~ 'csa',
#     )
#   )
# 
# single.hood %>% st_set_geometry(NULL)
# 
# single.hood %>% 
#   ggplot() +
#   geom_sf(aes(fill = side))

## pick the region where more than 50% of the csa falls into 
## can do 1:1 matches on region instead of apportioned matches

csa.top.region = intersected %>% 
  st_set_geometry(NULL) %>% 
  group_by(csa.hood.name, mapla.region.slug) %>% 
  summarise(pct.csa = sum(pct.csa)) %>% 
  arrange(csa.hood.name, mapla.region.slug) %>% 
  group_by(csa.hood.name) %>% 
  top_n(1, wt = pct.csa) %>%
  rename(top.region.pct.csa = pct.csa) %>% 
  arrange(-top.region.pct.csa) %>% 
  ungroup() %>% 
  left_join(
    csa.hoods.list %>% 
      select(csa.hood.name = hood.name, population)
  )

csa.top.region

## anything that isn't 50% in one region?

csa.top.region %>% filter(top.region.pct.csa < 0.5)

## plot to compare region totals from csa combining vs. mapping la

region.pop.comparison = csa.top.region %>% 
  group_by(mapla.region.slug) %>% 
  summarise(pop = sum(population)) %>% 
  arrange(-pop) %>% 
  full_join(mapla.region.pop, by = c('mapla.region.slug' = 'region.slug')) 

region.pop.comparison

# region.pop.comparison %>% 
#   ggplot(aes(pop, population)) +
#   geom_point() +
#   geom_abline(slope = 1, intercept = 0) +
#   geom_text(aes(label = mapla.region.slug))

## join region names to master hoods list

csa.hoods.list = csa.hoods.list %>% 
  rename(csa.hood.name = hood.name) %>% 
  left_join(csa.top.region %>% select(-population, -top.region.pct.csa))

csa.hoods.list

# export ------------------------------------------------------------------

## consolidated shapefile 

csa.hoods %>% 
  st_write('processed/countywide-statistical-areas-consolidated.geojson', delete_dsn = TRUE)

## master hoods list details consolidation and joining

csa.hoods.list %>% 
  write_csv('processed/countywide-statistical-areas-cleaned-list.csv', na = '')

## crosswalk from csa to hood (could be used to apportionment)

intersected %>%
  st_set_geometry(NULL) %>% 
  write_csv('processed/mapla-hood-csa-crosswalk.csv', na = '')

## crosswalk from csa to region (1:1 assigment, no apportionment)

csa.top.region %>% 
  write_csv('processed/mapla-region-csa-crosswalk.csv', na = '')
