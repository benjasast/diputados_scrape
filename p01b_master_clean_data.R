
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

# Check the ones without party
no_party <-  political_affiliation_raw %>% 
  filter(str_length(party)==0)

no_party[171:180,]

political_affiliation_raw %>% 
  count(party) %>% 
  arrange(desc(n))

# Input information for the non-present
party_input <- tribble(
  ~diputado_id, ~party,
  132, "Renovación Nacional",
  137, "Partido Demócrata Cristiano",
  150, "Partido Socialista",
  155, "Renovación Nacional",
  169, "Partido Por la Democracia",
  175, "Unión Demócrata Independiente",
  176, "Partido Socialista",
  178, "Renovación Nacional",
  179, "Unión Demócrata Independiente",
  206, "Independientes",
  208, "Unión Demócrata Independiente",
  211, "Partido Por la Democracia",
  220, "Partido Demócrata Cristiano",
  222, "Partido Demócrata Cristiano",
  239, "Partido Demócrata Cristiano",
  802, "Partido Socialista",
  806, "Renovación Nacional",
  821, "Unión Demócrata Independiente",
  822, "Unión Demócrata Independiente",
  858, "Partido Demócrata Cristiano",
  864, "Unión Demócrata Independiente",
  870, "Partido Socialista",
  873, "Partido Por la Democracia",
  877, "Partido Demócrata Cristiano",
  887, "Unión Demócrata Independiente",
  906, "Unión Demócrata Independiente",
  932, "Partido Demócrata Cristiano",
  933, "Partido Demócrata Cristiano",
  962, "Unión Demócrata Independiente",
  158, "Partido Por la Democracia",
  177, "Partido Por la Democracia",
  181, "Partido Demócrata Cristiano",
  193, "Partido Socialista",
  195, "Partido Socialista",
  205, "Partido Por la Democracia",
  224, "Partido Demócrata Cristiano",
  804, "Partido Socialista",
  805, "Unión Demócrata Independiente",
  818, "Renovación Nacional",
  819, "Partido Por la Democracia",
  832, "Partido Socialista",
  834, "Renovación Nacional",
  841, "Partido Demócrata Cristiano",
  860, "Partido Demócrata Cristiano",
  863, "Renovación Nacional",
  871, "Unión Demócrata Independiente",
  884, "Unión Demócrata Independiente",
  892, "Partido Por la Democracia",
  904, "Partido Por la Democracia",
  912, "Renovación Nacional",
  914, "Partido Radical de Chile",
  140, "Partido Demócrata Cristiano",
  159, "Unión Demócrata Independiente",
  161, "Renovación Nacional",
  165, "Unión Demócrata Independiente",
  170, "Partido Demócrata Cristiano",
  187, "Unión Demócrata Independiente",
  190, "Partido Demócrata Cristiano",
  213, "Partido Demócrata Cristiano",
  221, "Partido Por la Democracia",
  807, "Unión Demócrata Independiente",
  814, "Renovación Nacional",
  817, "Partido Socialista",
  825, "Renovación Nacional",
  826, "Unión Demócrata Independiente",
  828, "Unión Demócrata Independiente",
  842, "Renovación Nacional",
  849, "Partido Por la Democracia",
  854, "Partido Por la Democracia",
  859, "Partido Por la Democracia",
  878, "Partido Demócrata Cristiano",
  881, "Renovación Nacional",
  889, "Unión Demócrata Independiente",
  899, "Partido Por la Democracia",
  918, "Partido Demócrata Cristiano",
  921, "Renovación Nacional",
  947, "Partido Demócrata Cristiano",
  151, "Unión Demócrata Independiente",
  164, "Independientes",
  166, "Independientes",
  173, "Partido Radical de Chile",
  185, "Partido Demócrata Cristiano",
  202, "Partido Demócrata Cristiano",
  209, "Independientes",
  226, "Partido Demócrata Cristiano",
  234, "Unión Demócrata Independiente",
  242, "Renovación Nacional",
  800, "Partido Por la Democracia",
  808, "Partido Demócrata Cristiano",
  812, "Unión Demócrata Independiente",
  813, "Renovación Nacional",
  816, "Partido Demócrata Cristiano",
  823, "Unión Demócrata Independiente",
  830, "Unión Demócrata Independiente",
  840, "Unión Demócrata Independiente",
  857, "Unión Demócrata Independiente",
  868, "Renovación Nacional",
  882, "Independientes",
  886, "Partido Por la Democracia",
  888, "Partido Radical de Chile",
  890, "Independientes",
  894, "Partido Demócrata Cristiano",
  895, "Unión Demócrata Independiente",
  902, "Partido Por la Democracia",
  903, "Independientes",
  907, "Unión Demócrata Independiente",
  910, "Partido Por la Democracia",
  916, "Partido Por la Democracia",
  801, "Renovación Nacional",
  809, "Unión Demócrata Independiente",
  820, "Renovación Nacional",
  824, "Partido Socialista",
  829, "Partido Demócrata Cristiano",
  831, "Unión Demócrata Independiente",
  833, "Partido Socialista",
  835, "Independientes",
  836, "Partido Radical de Chile",
  838, "Unión Demócrata Independiente",
  839, "Partido Por la Democracia",
  845, "Partido Por la Democracia",
  846, "Renovación Nacional",
  847, "Partido Demócrata Cristiano",
  851, "Renovación Nacional",
  852, "Partido Por la Democracia",
  853, "Independientes",
  861, "Unión Demócrata Independiente",
  867, "Renovación Nacional",
  874, "Unión Demócrata Independiente",
  876, "Partido Por la Democracia",
  880, "Partido Socialista",
  883, "Partido Socialista",
  891, "Renovación Nacional",
  896, "Renovación Nacional",
  900, "Partido Radical de Chile",
  901, "Partido Socialista",
  905, "Unión Demócrata Independiente",
  909, "Renovación Nacional",
  911, "Partido Demócrata Cristiano",
  915, "Renovación Nacional",
  919, "Unión Demócrata Independiente",
  922, "Partido Por la Democracia",
  924, "Partido Socialista",
  927, "Renovación Nacional",
  928, "Unión Demócrata Independiente",
  929, "Partido Por la Democracia",
  930, "Partido Comunista",
  934, "Partido Demócrata Cristiano",
  935, "Renovación Nacional",
  938, "Unión Demócrata Independiente",
  939, "Unión Demócrata Independiente",
  941, "Partido Socialista",
  943, "Independientes",
  944, "Unión Demócrata Independiente",
  948, "Renovación Nacional",
  951, "Unión Demócrata Independiente",
  954, "Unión Demócrata Independiente",
  955, "Unión Demócrata Independiente",
  958, "Partido Por la Democracia",
  960, "Partido Por la Democracia",
  964, "Unión Demócrata Independiente",
  965, "Unión Demócrata Independiente",
  966, "Unión Demócrata Independiente",
  969, "Partido Demócrata Cristiano",
  970, "Unión Demócrata Independiente",
  977, "Partido Demócrata Cristiano",
  978, "Unión Demócrata Independiente",
  979, "Independientes",
  980, "Partido Por la Democracia",
  983, "Partido Demócrata Cristiano",
  988, "Evolución Política",
  1004, "Partido Socialista",
  1007, "Partido Por la Democracia",
  1026, "Renovación Nacional"
  
)




# Coalitions --------------------------------------------------------------

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





























