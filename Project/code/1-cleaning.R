#load libraries
library(tidyverse)

#load and tidy county socioeconomic data
county_data = read_tsv('data/raw/county_data.tsv') %>%
  rename_all(tolower) %>% #remove repetitive variables
  select(-uninsured, - median_income, -some_college, -hs_completion,
         -children_poverty_percent) %>% #remove variables repeated in other sets
  na.omit()

#load raw county healthcare expenditure data
county_health_spending_raw = read_csv('data/raw/county_health_cost_data.csv')

#tidy county healthcare expenditure data
county_health_spending = county_health_spending_raw %>% #rename
  rename('fips' = `State and County FIPS Code`) %>%
  rename('per_capita_health_cost' = `Actual Per Capita Costs`)

county_health_spending = county_health_spending %>% #clean
  mutate(per_capita_health_cost = gsub('\\$','', per_capita_health_cost)) %>%
  mutate(per_capita_health_cost = as.numeric(per_capita_health_cost))

county_health_spending = county_health_spending %>% #filter
  select(fips, per_capita_health_cost) %>%
  na.omit()

#load raw county 2020 presidential election results
county_election_results_raw = read_csv('data/raw/countypres.csv')

#tidy election results
county_election_results = county_election_results_raw %>%
  rename('fips' = `county_fips`) %>%
  filter(year == 2020) %>% #filter
  select(fips, party, candidatevotes) %>%
  group_by(fips) %>%
  mutate(max = max(candidatevotes)) %>%
  filter(max == candidatevotes) %>%
  select(-c(max)) %>%
  na.omit()

county_election_results = county_election_results %>%
  select(fips, party) %>% #select
  mutate(party = tolower(party)) #lowercase

#load raw county demographics data
county_demographics_raw = read_csv('data/raw/county_demographics.csv')

#tidy demographic data
county_demographics = county_demographics_raw %>%
  select(-state, -name) %>% #remove non- and repetitive features
  select(-households, -mean_work_travel, -unemployment_rate) %>%
  na.omit()

#change fips to char
county_demographics$fips = as.character(county_demographics$fips)

#merge datasets
health_data_join1 = inner_join(county_health_spending, county_data, by = 'fips')
health_data_join2 = inner_join(health_data_join1, county_election_results, by = 'fips')
health_data = inner_join(health_data_join2, county_demographics, by = 'fips')

#write cleaned data to file
write_csv(health_data, file = 'data/clean/health_data.csv')