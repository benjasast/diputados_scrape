
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


# Legislative periods information
legislative_periods_info <- readRDS('./scraped_data/table_legislative_periods_info.rds')

# Vote information
vote_information <- readRDS('./scraped_data/table_vote_information.rds')

# vote details
votatacion_detalle <- readRDS('./scraped_data/table_vote_details.rds')

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

# Split data into time periods, or other variables --------------------------------------------

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

# Check how many times voted together -------------------------------------
source('function_count_voting_pattern.R')


# Time-splitting: or variable splitting -----------------------------------

# Filter votes to only have this period
period_9 <- vote_information_splitready %>% 
  filter(legislative_period_id==9)

period_9_vote_ids <- period_9 %>% 
  select(vote_id) %>% 
  distinct()

# Grab only votacion_detail that matches the ids
votatacion_detalle_period_9 <- votatacion_detalle_long %>% 
  inner_join(period_9_vote_ids)



# Obtain voting patterns and descriptive stats ----------------------------




# Add pair characteristics ------------------------------------------------

vote_pattern_period_9_wpairinfo <- vote_pattern_period_9_w_affiliation %>% 
  left_join(p9_coalitions, by = c("dip1_party" = "party") ) %>% 
  rename(dip1_coalition = coalition) %>% 
  left_join(p9_coalitions, by = c("dip2_party" = "party") ) %>% 
  rename(dip2_coalition = coalition)
  

# Add types of pair
vote_pattern_period_9_wpairinfo_clean <- vote_pattern_period_9_wpairinfo %>% 
  left_join(coalition_relationships3, by = c("dip1_coalition" = "coalition1", "dip2_coalition" = "coalition2")) %>% 
  left_join(coalition_relationships3, by = c("dip2_coalition" = "coalition1", "dip1_coalition" = "coalition2")) %>% 
  mutate(pair_coalition = ifelse(!is.na(pair_coalition.x),pair_coalition.x,pair_coalition.y),
         pair_coalition_gen = ifelse(!is.na(pair_coalition_gen.x),pair_coalition_gen.x,pair_coalition_gen.y) ) %>% 
  select(-ends_with('.y'),- ends_with('.x') )


# Descriptive statistics --------------------------------------------------

nvotes_p9 <- nrow(period_9)

# Normalize percentage by total votes in year and votes present
p9_vote_pattern_normalized <- vote_pattern_period_9_wpairinfo_clean %>% 
  mutate_at(vars(n_same,n_opposite), .funs = list(bothpresent = ~./votaciones_both_present) ) %>% 
  mutate_at(vars(n_same,n_opposite), .funs = list(total = ~./nvotes_p9) )

descriptive_stats1 <- p9_vote_pattern_normalized %>% 
  group_by(pair_coalition) %>% 
  summarise(avg_nsame = mean(n_same_bothpresent, na.rm = TRUE),
            avg_nopp = mean(n_opposite_bothpresent, na.rm = TRUE))

descriptive_stats2 <- p9_vote_pattern_normalized %>% 
  group_by(pair_coalition_gen) %>% 
  summarise(avg_nsame = mean(n_same_bothpresent, na.rm = TRUE),
            avg_nopp = mean(n_opposite_bothpresent, na.rm = TRUE))

descriptive_stats3 <- p9_vote_pattern_normalized %>% 
  group_by(pair_coalition) %>% 
  summarise(avg_nsame = mean(n_same_total, na.rm = TRUE),
            avg_nopp = mean(n_opposite_total, na.rm = TRUE))

descriptive_stats4 <- p9_vote_pattern_normalized %>% 
  group_by(pair_coalition_gen) %>% 
  summarise(avg_nsame = mean(n_same_total, na.rm = TRUE),
            avg_nopp = mean(n_opposite_total, na.rm = TRUE))























