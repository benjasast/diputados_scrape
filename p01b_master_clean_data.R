
#################################################################################
# Program: P01b_master_clean_data
# Author: Benjamin Sas Trakinsky
# Date: 04/10/20
###################################################################################

# Objective, data was saved in lists and long format. 
# Put data in wide format, and save in tables that can be used and joined for analysis

library(tidyverse)
library(magrittr)
library(rvest)
library(RSelenium)
library(xml2)
library(furrr)
library(rlist)
library(lubridate)
library(fuzzyjoin)


rm(list=ls())



# Load data ---------------------------------------------------------------

vote_details_02_20 <- readRDS('./scraped_data/vote_info_02_18.rds')
vote_details_18_20 <- readRDS('./scraped_data/vote_info_18_20.rds')

# Put all votacion detalle together
vote_details <- vote_details_02_20 %>% 
  union_all(vote_details_18_20)


# Extract vote information ------------------------------------------------

grab_vote_information <- function(index){
  vote_details[[index]][[1]]
}

# Grab vote info
vote_information_table <- map(1:length(vote_details),grab_vote_information) %>% 
  reduce(union_all)

# Clean sesion - only the relevant stuff: we have multiple values
clean_sesion <- vote_information_table %>% 
  filter(variable=="sesion") %>% 
  filter(grepl('^\\d{4}$',value))

# Keep only the sessions that are worth it
vote_information_table_clean <- vote_information_table %>% 
  filter(variable!="sesion") %>% 
  union_all(clean_sesion) %>% 
  select(-index)

# Pivot data wider
vote_information_table_wide <- vote_information_table_clean %>% 
  pivot_wider(names_from = variable,values_from = value)

# Export table
saveRDS(vote_information_table_wide,'./scraped_data/table_vote_information.rds')


# Extract voting detail ---------------------------------------------------

grab_vote_detail <- function(index){
  vote_details[[index]][[2]]
}

# Grab vote detail
vote_detail_table <- map(1:length(vote_details),grab_vote_detail) %>% 
  reduce(union_all)

# mutate vote to factor variable
vote_detail_table_clean <- vote_detail_table %>% 
  mutate(vote = as_factor(vote))

# Pivot wider
vote_detail_table_wide <- vote_detail_table_clean %>% 
  pivot_wider(names_from = diputado_id,values_from = vote)

# Save
saveRDS(vote_detail_table_wide,'./scraped_data/table_vote_details.rds')

# Save long format too
saveRDS(vote_detail_table_clean,'./scraped_data/table_vote_details_long.rds')


# Political parties -------------------------------------------------------

political_affiliation <- readRDS('./scraped_data/political_affiliation.rds')

# Make sure all diputados present in the data have an assigned party.

# Make sure the information is yearly so it can be changed appropiately, in case of changes to party
# membership.

vote_detail_table_clean <- readRDS('./scraped_data/table_vote_details_long.rds')

# list with all diputados present in our data
diputado_id_list <- vote_detail_table_clean %>% 
  select(diputado_id) %>% 
  mutate(diputado_id = diputado_id %>% as.double()) %>% 
  distinct()


# Check they all have information - and pivot wider
political_affiliation_raw <- political_affiliation %>% 
  rename(diputado_id = DIPID)  %>% 
  inner_join(diputado_id_list) %>% 
  select(-regex) %>% 
  pivot_wider(names_from = variable, values_from = value)

no_party <- political_affiliation_raw %>% 
  filter(str_length(party)==0)

# Get input for diputados without web info (object party_input created)
source('political_affiliation_input.R')

# put information for all diputados
political_affiliation <- political_affiliation_raw %>% 
  anti_join(no_party, by = "diputado_id") %>% 
  union_all(party_input) %>% 
  mutate(aux = 1)

# Create a time grid: MONTH-YEAR
time_grid <- seq.Date('2002-01-01' %>% as.Date(),'2020-12-01' %>% as.Date(), by = "month")
time_df <- tibble(date = time_grid) %>% 
  mutate(aux=1)

# Make a cross join
political_affiliation_wtime <- political_affiliation %>% 
  full_join(time_df, by = "aux")

# Prepare corrections for party switches -------------------------------------

# Import party switch data
party_switches_raw <- readxl::read_xlsx('./scraped_data/table_party_switches.xlsx') %>% 
  select(-old_version_name) %>% 
  mutate_at(vars(ends_with("date")),~lubridate::as_date(.) %>% floor_date("month")) %>%  # Make sure changes are assigned at beggining of month
  rename(s1_party =  s1_part,
         original_party = partido)

# Data in long format: with switches to match.
party_switches_long <- party_switches_raw %>%
  pivot_longer(starts_with("s"),
               names_to = c("set", ".value"),
               names_pattern = "(.+)_(.+)" ) %>% 
  na.omit()


# Create intervals (for party membership)
party_switches_long_ready <- party_switches_long %>% 
  arrange(id_diputado,date) %>% 
  group_by(id_diputado) %>% 
  mutate(end_date = lag(date)-1) %>%
  mutate(end_date = ifelse(is.na(end_date),as_date('2025-01-01'),end_date) %>% as_date()) %>%  # Add fake end date for unique casee
  rename(start_date = date) %>% 
  select(id_diputado, party, start_date, end_date)

# Extract data for original parties (use for inputting original party) -- as current information is scrapped as last party
party_switches_original_parties <- party_switches_raw %>% 
  select(id_diputado,original_party) %>% 
  distinct()


# Introduce switches ------------------------------------------------------

# Keep only guys with switches
df_implement_switches <- political_affiliation_wtime %>% 
  filter(diputado_id %in% party_switches_original_parties$id_diputado)

# Put the original party (as starting point)
df_woriginal_party <- df_implement_switches %>% 
  select(-party) %>% 
  inner_join(party_switches_original_parties, by = c("diputado_id"="id_diputado" )) %>% 
  rename(party = original_party)

# Put the party changes

# Make fuzzy match betwen dataframes
  fuzzy_date_match <- fuzzy_left_join(
    df_woriginal_party, party_switches_long_ready,
    by = c(
      "diputado_id" = "id_diputado",
      "date" = "start_date",
      "date" = "end_date"
    ),
    match_fun = list(`==`,`>=`, `<=`) )
  

# Grab the neccesary changes
data_to_input <- fuzzy_date_match %>% 
  mutate(party = ifelse(is.na(party.y),party.x,party.y) ) %>% 
  select(-party.x,-party.y,-id_diputado,-start_date,-end_date)


# Final party affiliation data
political_affiliation_wtime_final <- political_affiliation_wtime %>% 
  anti_join(df_implement_switches, by = "diputado_id") %>% 
  union_all(data_to_input)


# Save political affiliation w_time
saveRDS(political_affiliation_wtime_final,'./scraped_data/political_affiliation_wtime.rds')

# Coalitions data --------------------------------------------------------------

alianza <- 'Coalición por el Cambio'
concertacion <- 'Ex-Nueva Mayoria'
fa <- 'Frente Amplio'
ind <- 'Independientes'

# Political coalitions in period 9
political_coalitions <- tribble(~party, ~coalition,
  'Renovación Nacional', alianza,
  'Unión Demócrata Independiente',alianza,
  'Independientes',ind,
  'Partido Socialista',concertacion,
  'Partido Demócrata Cristiano',concertacion,
  'Partido Comunista',concertacion,
  'Partido Por la Democracia',concertacion,
  'Revolución Democrática',fa,
  'Evolución Política',alianza,
  'Partido Regionalista Independiente', alianza,
  'Partido Convergencia Social',fa,
  'Partido Radical de Chile',concertacion,
  'Federación Regionalista Verde Social',ind,
  'Partido Comunes',fa,
  'Partido Humanista',fa,
  'Partido Liberal de Chile',fa,
  'Partido Ecologista Verde',fa,
  'Partido Republicano',ind,
  'Partido Izquierda Ciudadana', fa,
  'Movimiento Amplio Social', ind,
  'Partido Amplitud', ind)


# Save data
saveRDS(political_coalitions,'./scraped_data/p9_coalitions_info.rds')


# Pair membership data ----------------------------------------------------

# Create all possible combinations
party_list <- political_affiliation %>% 
  select(party) %>% 
  rename(party1 = party) %>% #convenient name for later
  distinct() 

# Create unique pairs - include the equals
party_pairs <- combn(party_list$party1,2) %>% 
  t() %>% 
  as_tibble() %>% 
  rename(party1 = V1,
         party2 = V2) %>% 
  union_all(party_list) %>% 
  mutate(party2 = ifelse(is.na(party2),party1,party2))


# Include coalition information
party_pairs_wcoalition <- party_pairs %>% 
  left_join(political_coalitions_p9, by = c("party1" = "party") ) %>% 
  rename(coalition1 = coalition) %>% 
  left_join(political_coalitions_p9, by = c("party2" = "party") ) %>% 
  rename(coalition2 = coalition)
  
# Relationship 1: inter-coalition or sampe party
party_pairs_rel1 <- party_pairs_wcoalition %>% 
  mutate(rel_party = case_when ( (party1==party2) & (party1!=ind) ~ 'Same Party',
                                 party1!=party2 & coalition1==coalition2 & coalition1==concertacion ~ 'Inter-concertacion',
                                 party1!=party2 & coalition1==coalition2 & coalition1==alianza ~ 'Inter-alianza',
                                 party1!=party2 & coalition1==coalition2 & coalition1==fa ~ 'Inter-FA' ))

# Relationship 2: Same coalition or different coalition
party_pairs_rel2 <- party_pairs_rel1 %>% 
  mutate(rel_coalition_gen = case_when ( coalition1==coalition2 & coalition1!=ind ~ 'Same coalition',
                                         coalition1!=coalition2 & (coalition1!=ind & coalition2!=ind) ~ 'Cross coalition',
                                         coalition1==ind | coalition2==ind ~ 'Independet present') )


# Relationship 3: relationships by coalition
party_pairs_rel3 <- party_pairs_rel2 %>% 
  mutate(rel_coalition = case_when(
    coalition1==coalition2 & coalition1==concertacion ~ 'concertacion-concertacion',
    coalition1==coalition2 & coalition1==alianza ~ 'alianza-alianza',
    coalition1==coalition2 & coalition1==fa ~ 'FA-FA',
    coalition1==concertacion & coalition2==alianza ~ 'alianza-concertacion',
    coalition1==alianza & coalition2==concertacion ~ 'alianza-concertacion',
    coalition1==fa & coalition2==alianza ~ 'alianza-FA',
    coalition1==alianza & coalition2==fa ~ 'alianza-FA',
    coalition1==fa & coalition2==concertacion ~ 'concertacion-FA',
    coalition1==concertacion & coalition2==fa ~ 'concertacion-FA',
    coalition1==ind | coalition2==ind ~ 'Independent present'))


# Save data (pair membership)
saveRDS(party_pairs_rel3,'./scraped_data/table_pair_membership.rds')
























