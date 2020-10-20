
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
political_affiliation <- readRDS('./scraped_data/political_affiliation_wtime.rds')

# Pair membership
pair_membership <- readRDS('./scraped_data/table_pair_membership.rds')

# Legislative periods information
legislative_periods_info <- readRDS('./scraped_data/table_legislative_periods_info.rds')

# Vote information - all
vote_information <- readRDS('./scraped_data/table_vote_information.rds')

# vote detail - all
vote_details <- readRDS('./scraped_data/table_vote_details.rds')

# vote detail - all (long)
votatacion_detalle_long <- readRDS('./scraped_data/table_vote_details_long.rds')

# Get data ready for splitting --------------------------------------------

# Objective 1: Add a column with legislative period belonging to each vote.
vote_information_cleandate <- vote_information %>% 
  mutate(fecha = date(fecha))

legislative_periods_info_winterval <- legislative_periods_info %>% 
  mutate(date_interval = lubridate:::interval(date_start,date_end))

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
  mutate(year = year(fecha),
         month = month(fecha)) %>% 
  select(vote_id,fecha,year,month,tipo_codigo,resultado,quorum,boletin,tramite,informe,starts_with("total"),articulo,sesion,legislative_period_id,legislative_name,legislative_name)

# Time-splitting: or variable splitting -----------------------------------

# nest by year-month
vote_information_nested <- vote_information_splitready %>% 
  mutate(year_month = fecha %>% floor_date("month")) %>% 
  group_by(legislative_name,year_month) %>% 
  nest()

vote_information_nested

# Use split votes on voting details to grab votes correspinding -----------------------------

join_vote_details <- function(df1,df2 = votatacion_detalle_long){
  df1 %>% 
    inner_join(df2, by = "vote_id")
}

# Add the voting details that match each period
vote_information_nested_wdetails <- vote_information_nested %>% 
  mutate(vote_details = map(data,join_vote_details))


# Grab network data and descriptive statistics ----------------------------
source('function_calculation_votingpatterns_descstats.R')

# Apply nest to first to year_month
results <- vote_information_nested_wdetails %>% 
  mutate(count_result = map(vote_details,calculate_votingpatterns))

# Save results of all year_months
saveRDS(results,'./scraped_data/network_all_year_months.rds')


# Create network data -----------------------------------------------------

# Simply take the counts from results
network_data <- results %>% 
  select(-data,-vote_details) %>% 
  unnest(c(count_result))


# Attach political affiliation --------------------------------------------

# Political affiliation
political_affiliation_party <- political_affiliation %>% 
  select(-bancada,-region,-comunas,-aux)

# Join political affiliation to network data
network_data_waffiliation <- network_data %>% 
  mutate_at(vars(dip_1,dip_2),as.double) %>% 
  left_join(political_affiliation_party, by = c("dip_1"="diputado_id","year_month"="date")) %>%  # diptuado 1
  rename(party1 = party) %>% 
  left_join(political_affiliation_party, by = c("dip_2"="diputado_id","year_month"="date")) %>%   # diptuado 2
  rename(party2 = party)


# Attach pair membership --------------------------------------------------

# We need to make two joins to make sure there are matches (data is in unique combo set)
network_data_wpairmembership <- network_data_waffiliation %>% 
  left_join(pair_membership, by = c("party1" = "party1", "party2" = "party2")) %>% # first run
  left_join(pair_membership, by = c("party2" = "party1", "party1" = "party2")) %>%  # second run
  mutate(rel_party = ifelse(!is.na(rel_party.x),rel_party.x,rel_party.y),
         rel_coalition_gen = ifelse(!is.na(rel_coalition_gen.x),rel_coalition_gen.x,rel_coalition_gen.y),
         rel_coalition = ifelse(!is.na(rel_coalition.x),rel_coalition.x,rel_coalition.y),
         coalition1 = ifelse(!is.na(coalition1.x),coalition1.x,coalition1.y),
         coalition2 = ifelse(!is.na(coalition2.x),coalition2.x,coalition2.y)) %>% 
  select(-ends_with(".y"),-ends_with(".x"))


# Export network data by year_month ---------------------------------------------

saveRDS(network_data_wpairmembership,'table_network_by_year_month.rds')











