library(tidyverse)
library(lubridate)
library(glue)

cdc.time = read_csv('vax/cdc-vacccinations-timeseries.csv')

ca.cdc = cdc.time %>% 
  filter(name == 'California')

ca.cdc

ca.admin = ca.cdc %>% 
  arrange(date) %>% 
  select(date, administered_dose2)

ca.admin

curr.vax = ca.admin %>% 
  filter(date == max(date)) %>% 
  pull(administered_dose2)

curr.date = ca.admin %>% 
  filter(date == max(date)) %>% 
  pull(date)

l7day.rate = ca.admin %>% 
  tail(7) %>% 
  select(administered_dose2) %>% 
  summarise(
    lo = min(administered_dose2),
    hi = max(administered_dose2)
  ) %>% 
  summarise(
    avg = (hi - lo) / 7
  ) %>% 
  pull()

l7day.rate

proj = tibble(
  date = seq(curr.date, ymd('2022-01-01'), by = 'days'),
) %>% 
  mutate(
    curr.rate = curr.vax + row_number() * l7day.rate,
    double.rate = curr.vax + row_number() * l7day.rate * 2,
    triple.rate = curr.vax + row_number() * l7day.rate * 3,
  )

proj


ca.admin %>% 
  ggplot(aes(date, administered_dose2)) +
  geom_hline(yintercept = 30000000, color = 'blue') +
  annotate('text', x = ymd('2021-04-01'), y = 31000000, label = '30 million Californians vaccinated (~75%)', color = 'blue') +
  geom_hline(yintercept = 40000000, color = 'blue') +
  annotate('text', x = ymd('2021-04-01'), y = 41000000, label = 'Population of California', color = 'blue') +
  geom_line() +
  geom_line(data = proj, aes(y = curr.rate), linetype = 2) +
  geom_line(data = proj, aes(y = double.rate), linetype = 2) +
  geom_line(data = proj, aes(y = triple.rate), linetype = 2) +
  annotate('text', x = ymd('2022-01-05'), y = max(proj$curr.rate), label = 'Current rate\n37,356 people per day', hjust = 'left') +
  annotate('text', x = ymd('2022-01-05'), y = max(proj$double.rate), label = 'Double rate', hjust = 'left') +
  annotate('text', x = ymd('2022-01-05'), y = max(proj$triple.rate), label = 'Triple rate', hjust = 'left') +
  geom_point(
    data = . %>% 
      filter(date == max(date)),
    color = 'red',
    size = 5
  ) +
  labs( 
    title = 'Californians receiving second dose of vaccine',
    x = '',
    y = ''
  ) +
  scale_x_date(limits = c(ymd('2021-01-01', '2022-03-15'))) +
  theme_minimal()
  
