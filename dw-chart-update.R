# devtools::install_github("munichrocker/DatawRappr")
# 
# p3Whe1AWae1s7eIYw2oa2f6EZCSDdkuHulUlEG4liTuc36KixNn2XjJVaBHs8Clz

library(tidyverse)
library(DatawRappr)

datawrapper_auth(api_key = "p3Whe1AWae1s7eIYw2oa2f6EZCSDdkuHulUlEG4liTuc36KixNn2XjJVaBHs8Clz")

dw_test_key()

data = read_csv('processed/region-crowding.csv')

data

dw_data_to_chart(data, chart_id = 'y2ZrR')