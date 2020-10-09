
#################################################################################
# Program: P02_master_analysis_script
# Author: Benjamin Sas Trakinsky
# Date: 04/10/20
###################################################################################

# Load libraries ----------------------------------------------------------

library(tidyverse)
library(magrittr)
library(rvest)
library(RSelenium)
library(xml2)
library(furrr)
library(lubridate)
library(fuzzyjoin)

rm(list=ls())


# Load data ---------------------------------------------------------------

# political affiliation
political_affiliation_long <- readRDS('./scraped_data/political_affiliation.rds')

# Clean political affiliation
political_affiliation_wide <- political_affiliation_long %>% 
  select(-regex) %>% 
  pivot_wider(names_from = variable, values_from = value)

# Coalition information
p9_coalitions <- readRDS('./scraped_data/p9_coalitions_info.rds')

# Coalition relationships
coalition_relationships <- readRDS('./scraped_data/table_coalition_relationships_p9.rds')

# Legislative periods information
legislative_periods_info <- readRDS('./scraped_data/table_legislative_periods_info.rds')

# Vote information
vote_information <- readRDS('./scraped_data/table_vote_information.rds')

# vote detail: 2002-2018
votatacion_detalle_2002_2018 <- readRDS('./scraped_data/vote_info_02_18.rds')

# vote details:2018-2020
votatacion_detalle_2018_2020 <- readRDS('./scraped_data/table_vote_details.rds')

# vote details long
votatacion_detalle_long <- readRDS('./scraped_data/table_vote_details_long.rds')



# Check all diputados present in data have affiliation --------------------

all_diputados <- votatacion_detalle_long %>% 
  select(diputado_id) %>% 
  mutate(diputado_id = diputado_id %>% as.double()) %>% 
  distinct() %>% 
  left_join(political_affiliation_wide, by = c("diputado_id" = "DIPID") )
  
missing_data <- all_diputados %>% 
  filter(str_length(party)<2 )

input_missing_data <- tribble(
  ~diputado_id, ~party, ~bancada, ~region, ~comunas,
  1026, "Renovación Nacional","","","",
  970, "Unión Demócrata Independiente","","","",
  949, "Renovación Nacional","","","")

all_diputados_winfo <- all_diputados %>% 
  filter(str_length(party)>2 ) %>% 
  union_all(input_missing_data)


# Get data ready for splitting --------------------------------------------

# Objective 1: Add a column with legislative period belonging to each vote.
vote_information_cleandate <- vote_information %>% 
  mutate(fecha = date(fecha))

legislative_periods_info_winterval <- legislative_periods_info %>% 
  mutate(date_interval = lubridate:::interval(date_start,date_end))

vote_information_cleandate
legislative_periods_info_winterval

# Make fuzzy match betwen dataframes
fuzzy_date_match <- fuzzy_left_join(
  vote_information_cleandate, legislative_periods_info_winterval,
  by = c(
    "fecha" = "date_start",
    "fecha" = "date_end"
  ),
  match_fun = list(`>=`, `<=`) )

# Data ready for splitting
vote_information_splitready <- fuzzy_date_match %>% 
  select(vote_id,fecha,tipo_codigo,resultado,quorum,boletin,tramite,informe,starts_with("total"),articulo,sesion,legislative_period_id,legislative_name,legislative_name)

# Time-splitting: or variable splitting -----------------------------------

# Filter votes to only have this period
p9_votes <- vote_information_splitready %>% 
  filter(legislative_period_id==9) %>% 
  mutate(year = year(fecha))

p9_votes_rel <- p9_votes %>% 
  select(vote_id,year)


# Grab only votacion_detail that matches the ids
p9_votacion_detalle <- votatacion_detalle_long %>% 
  inner_join(p9_votes_rel)

p9_votacion_detalle %>% 
  count(vote)


# Split data by years
p9_votacion_detalle_by_year <- p9_votacion_detalle %>% 
  split(p9_votacion_detalle$year)

# Grab network data and descriptive statistics ----------------------------
source('function_calculation_votingpatterns_descstats.R')

# Run function for each year of data and save results

results_2018 <- calculate_votingpatterns_descstats(p9_votacion_detalle_by_year[["2018"]],p9_coalitions,split_name = "2018")
results_2019 <- calculate_votingpatterns_descstats(p9_votacion_detalle_by_year[["2019"]],p9_coalitions,split_name = "2019")
results_2020 <- calculate_votingpatterns_descstats(p9_votacion_detalle_by_year[["2020"]],p9_coalitions,split_name = "2020")


# Load results from network data and desc stats ---------------------------

results_2018 <- readRDS('./scraped_data/network_n_stats_2018.rds')
results_2019 <- readRDS('./scraped_data/network_n_stats_2019.rds')
results_2020 <- readRDS('./scraped_data/network_n_stats_2020.rds')

# Create network data -----------------------------------------------------

network_data <- reduce(list(results_2018$network %>% 
                           mutate(year=2018),results_2019$network %>% 
                           mutate(year=2019),results_2020$network %>% 
                           mutate(year=2020) ), union_all)

# Votes in agreement all present
votes_by_coal <- network_data %>% 
  group_by(pair_coalition, year) %>% 
  summarise(mean_same = mean(n_same_bothpresent, na.rm = TRUE),
            mean_opp = mean(n_opposite_bothpresent, na.rm = TRUE))


votes_by_coal

votes_by_coal %>% 
  ggplot(aes(year,mean_same)) +
  geom_line(aes(color=pair_coalition)) +
  geom_point(aes(color=pair_coalition)) +
  ggtitle("Votos de acuerdo")
  
votes_by_coal %>% 
  ggplot(aes(year,mean_opp)) +
  geom_line(aes(color=pair_coalition)) +
  geom_point(aes(color=pair_coalition)) +
  ggtitle("Votos desacuerdo")




results_2018$network$n_same_bothpresent %>% mean(na.rm=TRUE)
results_2019$network$n_same_bothpresent %>% mean(na.rm=TRUE)
results_2020$network$n_same_bothpresent %>% mean(na.rm=TRUE)


results_2018$network %>% 
  group_by(pair_coalition) %>% 
  summarise(mean_same = mean(n_same_total, na.rm = TRUE),
            mean_opp = mean(n_opposite_total, na.rm = TRUE))












