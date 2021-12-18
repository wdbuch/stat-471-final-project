#load libraries
library(tidyverse)

#read County Health Rankings & Roadmap socioeconomic data in from data folder
county_data = read_tsv('data/raw/county_data.tsv')

#read CMS healthcare spending data in from data folder
county_health_spending_raw = read_csv('data/raw/county_health_cost_data.csv')

#read Harvard Dataverse 2020 presidential election data in from data folder
county_election_results_raw = read_csv('data/raw/countypres.csv')

#read US Census data in from data folder
county_demographics_raw = read_csv('data/raw/county_demographics.csv')