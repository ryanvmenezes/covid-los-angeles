# hand-entered data from LAT spreadsheet 

download.file(
    'https://docs.google.com/spreadsheets/d/e/2PACX-1vTpZB-OMT3gU9Rs7ZqU0ZFJT8oZXHiv6xRT79fDQU7dmbISu6rciNrCmoNw7R-oH4DqrGdzHMDqtPFC/pub?output=csv&gid=254031884',
    'data/raw/lat-la-csa-daily.csv'
)


# cities list in l.a. county

lat.la.cities = read_csv('data/raw/covid/lat-la-cities.csv', skip = 4)

lat.la.cities

# process la county cities ------------------------------------------------

la.cities = lat.la.cities %>% 
  select(-X4)

la.cities