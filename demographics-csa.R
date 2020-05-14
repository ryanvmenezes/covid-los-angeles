library(foreign)
library(tidyverse)

csa.list = read_csv('processed/countywide-statistical-areas-cleaned-list.csv')

csa.list

demographics = read.dbf('raw/DEMOGRAPHY_2018_Revised/2018/source/PopEst2018_CT10FIP18_CSA_age_race_gender_summary.dbf', as.is = TRUE) %>% 
  as_tibble()

demographics

demographics %>% 
  count(demo.csa = CSA) %>% 
  full_join(csa.list, by = c('demo.csa' = 'csa.hood.name')) %>% 
  view()
