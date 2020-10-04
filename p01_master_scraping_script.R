
#################################################################################
# Program: P01_master_scraping_script
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

rm(list=ls())


# Grab votaciones detalles (a favor, contra, etc) -------------------------
source('function_grab_voting_details_and_info.R')

# Parameter is vote_id where the latest can be checked at:
# https://www.camara.cl/legislacion/sala_sesiones/votaciones.aspx

# 2002-2020
vote_id_start <- 1200
vote_id_end <- 36000


# Create tibble of data to store ------------------------------------------
vote_data <- tibble(vote_id = seq(vote_id_start,vote_id_end))

# Vectorize function to obtain information
grab_voting_details_and_info_V <- Vectorize(grab_voting_details_and_info)

# Apply function to grab votacion detalle ----------------------------------------------------------

# Apply function in parallel
plan(multisession, workers = 4)

# Get results
vote_info <- future_map(vote_data$vote_id,grab_voting_details_and_info, .progress = TRUE)

# Remove all the nulls
vote_info_wo_nulls <- purrr::compact(vote_info)

# Save votacion detalle_info
votacion_detalle_filename <- str_c('./scraped_data/votacion_detalle_',vote_id_start,'_',vote_id_end,'.rds')
saveRDS(vote_info_wo_nulls,votacion_detalle_filename)


# Grab diputados personal information -------------------------------------
source('function_grab_diputados_personal_information.R')

# No 7 was skipped by congress somehow (data is good, just a weird thing)
ids_legislative_periods <- c(1,2,3,4,5,6,8,9)

diputados_personal_info <- map(ids_legislative_periods,grab_diputados_personal_information)

diputados_personal_info[[4]]

# Save data
saveRDS(diputados_personal_info,'./scraped_data/diputados_personal_info.rds')


# Attach Political parties ------------------------------------------------
source('function_grab_political_affiliation.R')

# Vectorize the function
grab_political_affiliation_V <- Vectorize(grab_political_affiliation)

# List with unique diputados_id
list_dip_ids <- diputados_personal_info %>% 
  reduce(full_join) %>% 
  select(DIPID) %>% 
  mutate(DIPID = DIPID %>% as.double()) %>% 
  distinct()

# Grab information
political_affiliation <- list_dip_ids %>% 
  mutate(try = grab_political_affiliation_V(DIPID))



























