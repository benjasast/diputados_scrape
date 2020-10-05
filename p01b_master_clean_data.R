
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

rm(list=ls())



# Load data ---------------------------------------------------------------

vote_details <- readRDS('./scraped_data/vote_info_18_20.rds')


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

# Political coalitions ----------------------------------------------------

alianza <- 'Coalición por el Cambio'
concertacion <- 'Ex-Nueva Mayoria'
fa <- 'Frente Amplio'
ind <- 'Independientes'


# Political coalitions in period 9
political_coalitions_p9 <- tribble(~party, ~coalition,
  'Renovación Nacional', alianza,
  'Unión Demócrata Independiente',alianza,
  'Independientes',ind,
  'Partido Socialista',concertacion,
  'Partido Demócrata Cristiano',concertacion,
  'Partido Comunista',concertacion,
  'Partido Por la Democracia',concertacion,
  'Revolución Democrática',fa,
  'Evolución Política',alianza,
  'Partido Convergencia Social',fa,
  'Partido Radical de Chile',concertacion,
  'Federación Regionalista Verde Social',ind,
  'Partido Comunes',fa,
  'Partido Humanista',fa,
  'Partido Liberal de Chile',fa,
  'Partido Ecologista Verde',fa,
  'Partido Republicano',ind)


# Save data
saveRDS(political_coalitions_p9,'./scraped_data/p9_coalitions_info.rds')





























