library(tidyverse)
library(rvest)
library(tidycensus)

url = 'https://www.latimes.com/projects/california-coronavirus-cases-tracking-outbreak/covid-19-vaccines-distribution/'

page = read_html(url)

page

ca.data = page %>% 
  html_nodes('.vaccine-region-copy') %>% 
  map_dfr(
    ~tibble(
      region = .x %>% 
        html_node('.headline') %>% 
        html_text(),
      doses = .x %>% 
        html_node('.doses') %>% 
        html_text(),
      population = .x %>% 
        html_node('.description.population') %>% 
        html_text()
    )
  ) %>% 
  mutate(
    doses = str_remove(doses, ' doses administered') %>% parse_number(),
    population = map_dbl(
      population,
      ~.x %>% 
        str_remove('( million)* residents') %>% 
        parse_number() *
        if_else(str_detect(.x, 'million'), 1000000, 1)
    )
  )

ca.data
  
ca.data %>% 
  ggplot(aes(population, doses, label = region)) +
  geom_text() +
  geom_point(color = 'red') +
  geom_smooth(method = 'lm', se = FALSE) +
  theme_minimal()

