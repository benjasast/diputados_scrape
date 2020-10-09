#######################################
# Program: diputados_webscrape
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

rm(list=ls())

# Custom functions
source('function_grab_voting_details_and_info.R')

# Parameters --------------------------------------------------------------

# 2002-2018
vote_id_start <- 1200
vote_id_end <- 28234

# # 2018-2020
# vote_id_start <- 28235
# vote_id_end <- 36000

# Create tibble of data to store ------------------------------------------
vote_data <- tibble(vote_id = seq(vote_id_start,vote_id_end))

# Exclude very strange anomalies (data not well inputed from website)
anomalies <- c(10264)

vote_data <- vote_data %>% 
  filter(!vote_id %in% anomalies)


# Apply function to grab information ----------------------------------------------------------

# Apply function in parallel
plan(multisession)

# Get results
vote_info <- future_map(vote_data$vote_id,grab_voting_details_and_info, .progress = TRUE)

# Error (vote_id 1553)
# Error in xml_children(x)[[search]] : subscript out of bounds

# Remove all the nulls
vote_info_wo_nulls <- purrr::compact(vote_info)

# Save data ---------------------------------------------------------------
saveRDS(vote_info_wo_nulls,'./scraped_data/vote_info_02_18.rds')












