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

# Custom functions
source('function_grab_voting_details_and_info.R')

# Parameters --------------------------------------------------------------

vote_id_start <- 28235
vote_id_end <- 36000









