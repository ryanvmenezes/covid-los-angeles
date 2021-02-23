library(sf)
library(tidyverse)
library(tidycensus)

# census_api_key('45abd686abf7a53c954b1e3d26eadca0631584fa', install = TRUE)
# Sys.getenv("CENSUS_API_KEY")

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

# crowding ----------------------------------------------------------------

occupancy = get.acs.custom(
  variables = c(
    # https://www.socialexplorer.com/data/ACS2018_5yr/metadata/?ds=ACS18_5yr&table=B25014
    "total" = 'B25014_001',
    "owner_occ" = 'B25014_002',
    "owner_occ_0.5_or_less_per_room" = 'B25014_003',
    "owner_occ_0.51_to_1.0_per_room" = 'B25014_004',
    "owner_occ_1.01_to_1.5_per_room" = 'B25014_005',
    "owner_occ_1.51_to_2.0_per_room" = 'B25014_006',
    "owner_occ_2.01_or_more_per_room" = 'B25014_007',
    "renter_occ" = 'B25014_008',
    "renter_occ_0.5_or_less_per_room" = 'B25014_009',
    "renter_occ_0.51_to_1.0_per_room" = 'B25014_010',
    "renter_occ_1.01_to_1.5_per_room" = 'B25014_011',
    "renter_occ_1.51_to_2.0_per_room" = 'B25014_012',
    "renter_occ_2.01_or_more_per_room" = 'B25014_013'
  )
)

occupancy

occupancy.totals = occupancy %>% 
  filter(variable == 'total') %>% 
  select(GEOID, total = estimate)

occupancy.totals

occupancy.crowded = occupancy %>% 
  filter(str_detect(variable, '1.01|1.51|2.01')) %>% 
  group_by(GEOID) %>% 
  summarise(crowded = sum(estimate))

occupancy.crowded

csa.crowded = occupancy.totals %>%
  left_join(occupancy.crowded) %>% 
  rename(tract.fips = GEOID) %>% 
  left_join(xwalk) %>% 
  mutate(
    app.total = pct.of.tract * total,
    app.crowded = pct.of.tract * crowded
  ) %>% 
  group_by(hood.name) %>% 
  summarise(
    total = sum(app.total),
    crowded = sum(app.crowded),
    pct.crowded = crowded / total
  )

csa.crowded

csa.crowded %>% write_csv('gis-census/csa-crowded.csv')


# area --------------------------------------------------------------------

csa = st_read('processed/countywide-statistical-areas-consolidated.geojson', as_tibble = TRUE, stringsAsFactors = FALSE)

csa

area = csa %>%
  st_transform(3857) %>% 
  mutate(area = st_area(.)) %>% 
  st_set_geometry(NULL) %>% 
  mutate(area = as.double(area) / 2.59e+6) %>% 
  mutate(density = population / area)

area

area %>% write_csv('gis-census/area.csv')


# age ---------------------------------------------------------------------

age.raw = get.acs.custom(
  variables = c(
    "total" = 'B01001_001',
    "male" = 'B01001_002',
    "male_0_4" = 'B01001_003',
    "male_5_9" = 'B01001_004',
    "male_10_14" = 'B01001_005',
    "male_15_17" = 'B01001_006',
    "male_18_19" = 'B01001_007',
    "male_20" = 'B01001_008',
    "male_21" = 'B01001_009',
    "male_22_24" = 'B01001_010',
    "male_25_29" = 'B01001_011',
    "male_30_34" = 'B01001_012',
    "male_35_39" = 'B01001_013',
    "male_40_44" = 'B01001_014',
    "male_45_49" = 'B01001_015',
    "male_50_54" = 'B01001_016',
    "male_55_59" = 'B01001_017',
    "male_60_61" = 'B01001_018',
    "male_62_64" = 'B01001_019',
    "male_65_66" = 'B01001_020',
    "male_67_69" = 'B01001_021',
    "male_70_74" = 'B01001_022',
    "male_75_79" = 'B01001_023',
    "male_80_84" = 'B01001_024',
    "male_85_over" = 'B01001_025',
    "female" = 'B01001_026',
    "female_0_4" = 'B01001_027',
    "female_5_9" = 'B01001_028',
    "female_10_14" = 'B01001_029',
    "female_15_17" = 'B01001_030',
    "female_18_19" = 'B01001_031',
    "female_20" = 'B01001_032',
    "female_21" = 'B01001_033',
    "female_22_24" = 'B01001_034',
    "female_25_29" = 'B01001_035',
    "female_30_34" = 'B01001_036',
    "female_35_39" = 'B01001_037',
    "female_40_44" = 'B01001_038',
    "female_45_49" = 'B01001_039',
    "female_50_54" = 'B01001_040',
    "female_55_59" = 'B01001_041',
    "female_60_61" = 'B01001_042',
    "female_62_64" = 'B01001_043',
    "female_65_66" = 'B01001_044',
    "female_67_69" = 'B01001_045',
    "female_70_74" = 'B01001_046',
    "female_75_79" = 'B01001_047',
    "female_80_84" = 'B01001_048',
    "female_85_over" = 'B01001_049'
  )
)

age.raw

age.65 = age.raw %>% 
  select(-NAME, -moe) %>% 
  filter(!variable %in% c('total', 'male', 'female')) %>% 
  separate(variable, into = c('gender', 'lo', 'hi')) %>% 
  mutate(hi = coalesce(hi, lo)) %>% 
  mutate(lo = as.numeric(lo)) %>% 
  mutate(group = if_else(lo >= 65, 'gte65', 'lt65')) %>% 
  group_by(tract.fips = GEOID, group) %>% 
  summarise(pop = sum(estimate)) %>% 
  pivot_wider(names_from = group, values_from = pop) %>% 
  mutate(total = gte65 + lt65) %>% 
  left_join(xwalk) %>% 
  mutate(
    prop.gte65 = pct.of.tract * gte65,
    prop.total = pct.of.tract * total
  ) %>% 
  group_by(hood.name) %>% 
  summarise(
    gte65 = sum(prop.gte65),
    total = sum(prop.total),
  ) %>% 
  mutate(pct.gte65 = gte65 / total)

age.65

age.65 %>% write_csv('gis-census/csa-age-65.csv')
