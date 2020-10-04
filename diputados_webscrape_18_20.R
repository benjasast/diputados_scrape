#######################################
# Program: diputados_webscrape_18_20
# Author: Benjamin Sas Trakinsky
# Date: 03/10/20
######################################

# Objective: scrape results of votaciones in congress between 2018 and 2020

library(tidyverse)
library(magrittr)
library(rvest)
library(RSelenium)
library(xml2)
library(furrr)

# Custom functions
source('function_grab_voting_details_and_info.R')

# Parameters --------------------------------------------------------------

#vote_id_start <- 28235

vote_id_start <- 28930
vote_id_end <- 36000

# Create tibble of data to store ------------------------------------------
vote_data <- tibble(vote_id = seq(vote_id_start,vote_id_end))

# Vectorize function to obtain information
grab_voting_details_and_info_V <- Vectorize(grab_voting_details_and_info)

# Apply function to grab information ----------------------------------------------------------

# Apply function in parallel
plan(multisession, workers = 4)

vote_info <- future_map(vote_data$vote_id,grab_voting_details_and_info, .progress = TRUE)


grab_voting_details_and_info(28943) # Is trouble


