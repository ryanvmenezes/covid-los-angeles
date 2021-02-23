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

curr.vax

curr.date = ca.admin %>% 
  filter(date == max(date)) %>% 
  pull(date)

curr.date

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

summary = tibble(
  rate = c('current', 'double', 'triple'),
  vaxperday = l7day.rate * 1:3,
  daystilherd = (30000000 - curr.vax) / vaxperday,
  dateherd = curr.date + daystilherd,
  daystilpop = (40000000 - curr.vax) / vaxperday,
  datepop = curr.date + daystilpop
)

summary

(as.numeric(ymd('2022-01-01') - today()) * l7day.rate + curr.vax) / 40000000

((0.8 * 40000000) - curr.vax) / (as.numeric(ymd('2022-01-01') - today())) / l7day.rate 
((0.8 * 40000000) - curr.vax) / (as.numeric(ymd('2021-07-01') - today())) / l7day.rate 

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
  geom_hline(yintercept = 30000000, color = 'coral') +
  annotate('text', x = ymd('2021-04-01'), y = 31000000, label = '30 million Californians vaccinated (~75%)', color = 'coral') +
  geom_hline(yintercept = 40000000, color = 'coral') +
  annotate('text', x = ymd('2021-04-01'), y = 41000000, label = 'Population of California', color = 'coral') +
  geom_line() +
  geom_line(data = proj, aes(y = curr.rate), linetype = 2) +
  geom_line(data = proj, aes(y = double.rate), linetype = 2) +
  geom_line(data = proj, aes(y = triple.rate), linetype = 2) +
  annotate(
    'text',
    x = ymd('2022-01-05'),
    y = max(proj$curr.rate),
    label = glue('Current rate\n{format(round(l7day.rate), big.mark=",")} / day\nReached {summary$dateherd[1]}'),
    hjust = 'left'
  ) +
  annotate(
    'text',
    x = summary$datepop[2] + 5,
    y = 40000000,
    label = glue('Double rate\n {summary$dateherd[2]}'),
    hjust = 'left'
  ) +
  annotate(
    'text',
    x = summary$datepop[3] + 5,
    y = 40000000,
    label = glue('Triple rate\n {summary$dateherd[3]}'),
    hjust = 'left'
  ) +
  # annotate('text', x = ymd('2022-01-05'), y = max(proj$double.rate), label = 'Double rate', hjust = 'left') +
  # annotate('text', x = ymd('2022-01-05'), y = max(proj$triple.rate), label = 'Triple rate', hjust = 'left') +
  geom_point(
    data = . %>% 
      filter(date == max(date)),
    color = 'red',
    size = 5
  ) +
  labs(
    title = 'Californians receiving second dose of vaccine',
    x = '',
    y = '',
    caption = 'calculated 2/18/2021'
  ) +
  scale_y_continuous(limits = c(0, 41000000)) +
  scale_x_date(limits = c(ymd('2021-01-01', '2022-03-15'))) +
  theme_minimal()
  
