library(tidyverse)
library(rvest)

page = read_html('http://publichealth.lacounty.gov/media/Coronavirus/locations.htm#vaccinated')

vax.table = page %>% 
  html_node('div#vaccinated + div') %>% 
  html_node('table') %>% 
  html_table() %>% 
  as_tibble()

vax.table  

vax.table %>% write_csv('vax/la-county-vaccinated-hoods.csv')
