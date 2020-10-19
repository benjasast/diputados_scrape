
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

political_affiliation %>% 
  select(party) %>% 
  distinct()

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

# nest votes by year only (just for now) - we will do year-month later
vote_information_nested <- vote_information_splitready %>% 
  group_by(legislative_name,year) %>% 
  nest()

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

check <- vote_information_nested_wdetails %>% 
  select(year,legislative_name)

# Let's compute separately for each year-period (maybe I will put this in the nest later)
results_2002 <- calculate_votingpatterns(vote_information_nested_wdetails$vote_details[[1]],'2002')
results_2003 <- calculate_votingpatterns(vote_information_nested_wdetails$vote_details[[2]],'2003')
results_2004 <- calculate_votingpatterns(vote_information_nested_wdetails$vote_details[[3]],'2004')
results_2005 <- calculate_votingpatterns(vote_information_nested_wdetails$vote_details[[4]],'2005')
results_n2006 <- calculate_votingpatterns(vote_information_nested_wdetails$vote_details[[5]],'n_2006')


results_2006 <- calculate_votingpatterns(vote_information_nested_wdetails$vote_details[[6]],'2006')
results_2007 <- calculate_votingpatterns(vote_information_nested_wdetails$vote_details[[7]],'2007')
results_2008 <- calculate_votingpatterns(vote_information_nested_wdetails$vote_details[[8]],'2008')
results_2009 <- calculate_votingpatterns(vote_information_nested_wdetails$vote_details[[9]],'2009')
results_n_2010 <- calculate_votingpatterns(vote_information_nested_wdetails$vote_details[[10]],'n_2010')

results_2010 <- calculate_votingpatterns(vote_information_nested_wdetails$vote_details[[11]],'2010')
results_2011 <- calculate_votingpatterns(vote_information_nested_wdetails$vote_details[[12]],'2011')
results_2012 <- calculate_votingpatterns(vote_information_nested_wdetails$vote_details[[13]],'2012')
results_2013 <- calculate_votingpatterns(vote_information_nested_wdetails$vote_details[[14]],'2013')
results_n_2014 <- calculate_votingpatterns(vote_information_nested_wdetails$vote_details[[15]],'n_2014')

results_2014 <- calculate_votingpatterns(vote_information_nested_wdetails$vote_details[[16]],'2014')
results_2015 <- calculate_votingpatterns(vote_information_nested_wdetails$vote_details[[17]],'2015')
results_2016 <- calculate_votingpatterns(vote_information_nested_wdetails$vote_details[[18]],'2016')
results_2017 <- calculate_votingpatterns(vote_information_nested_wdetails$vote_details[[19]],'2017')
results_n_2018 <- calculate_votingpatterns(vote_information_nested_wdetails$vote_details[[20]],'n_2018')

results_2018 <- calculate_votingpatterns(vote_information_nested_wdetails$vote_details[[21]],'2018')
results_2019 <- calculate_votingpatterns(vote_information_nested_wdetails$vote_details[[22]],'2019')
results_2020 <- calculate_votingpatterns(vote_information_nested_wdetails$vote_details[[23]],'2020')



# Create network data -----------------------------------------------------

# Skipping overlapping years for now
network_data <- list(results_2002 %>% mutate(year=2002),
                     results_2003 %>% mutate(year=2003),
                     results_2004 %>% mutate(year=2004),
                     results_2005 %>% mutate(year=2005),
                     results_2006 %>% mutate(year=2006),
                     results_2007 %>% mutate(year=2007),
                     results_2008 %>% mutate(year=2008),
                     results_2009 %>% mutate(year=2009),
                     results_2010 %>% mutate(year=2010),
                     results_2011 %>% mutate(year=2011),
                     results_2012 %>% mutate(year=2012),
                     results_2013 %>% mutate(year=2013),
                     results_2014 %>% mutate(year=2014),
                     results_2015 %>% mutate(year=2015),
                     results_2016 %>% mutate(year=2016),
                     results_2017 %>% mutate(year=2017),
                     results_2018 %>% mutate(year=2018),
                     results_2019 %>% mutate(year=2019),
                     results_2020 %>% mutate(year=2020) ) %>% 
  reduce(union_all)


# Attach political affiliation --------------------------------------------

# Political affiliation by year
political_affiliation_year <- political_affiliation %>% 
  mutate(year = year(date)) %>% 
  select(diputado_id,party,year) %>% 
  group_by(diputado_id,year) %>% 
  slice(1) %>% 
  ungroup()


# Join political affiliation to network data
network_data_waffiliation <- network_data %>% 
  mutate_at(vars(dip_1,dip_2),as.double) %>% 
  left_join(political_affiliation_year, by = c("dip_1"="diputado_id","year"="year")) %>%  # diptuado 1
  rename(party1 = party) %>% 
  left_join(political_affiliation_year, by = c("dip_2"="diputado_id","year"="year")) %>%   # diptuado 2
  rename(party2 = party)

# Attach pair membership --------------------------------------------------

# We need to make two joins to make sure there are matches (data is in unique combo set)
network_data_wpairmembership <- network_data_waffiliation %>% 
  left_join(pair_membership, by = c("party1" = "party1", "party2" = "party2")) %>% # first run
  left_join(pair_membership, by = c("party2" = "party1", "party1" = "party2")) %>%  # second run
  mutate(rel_party = ifelse(!is.na(rel_party.x),rel_party.x,rel_party.y),
         rel_coalition_gen = ifelse(!is.na(rel_coalition_gen.x),rel_coalition_gen.x,rel_coalition_gen.y),
         rel_coalition = ifelse(!is.na(rel_coalition.x),rel_coalition.x,rel_coalition.y) ) %>% 
  select(-rel_party.x, -rel_party.y, -rel_coalition_gen.x,
         -rel_coalition_gen.y, -rel_coalition.x, -rel_coalition.y)



# Export network data by year ---------------------------------------------

saveRDS(network_data_wpairmembership,'table_network_by_year.rds')











