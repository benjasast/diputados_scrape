####################################################################
# Program: function_calculate_votingpatterns_descstats
# Author: Benjamin Sas Trakinsky
# Fecha: 05/10/20
####################################################################


# Custom functions -------------------------------------

# Check how many votes together
source('function_count_voting_pattern.R')


# Function ----------------------------------------------------------------

calculate_votingpatterns_descstats <- function(df_votacion_detalle,split_name,df_political_affiliation = all_diputados_winfo){


# Calculate voting pattern (counts) ---------------------------------------

vote_pattern <- count_voting_pattern(df_votacion_detalle)

# Attach political affiliation
vote_pattern_affiliation <- vote_pattern %>% 
  mutate_at(vars(dip_1,dip_2), ~ as.double(.)) %>% 
  left_join(df_political_affiliation, by = c("dip_1" = "diputado_id")) %>% # Join with affiliation data for dip1
  rename_at(vars(party,bancada,region,comunas),~paste0('dip1_',.)) %>% 
  left_join(df_political_affiliation, by = c("dip_2" = "diputado_id")) %>% # Join with affiliation data for dip2
  rename_at(vars(party,bancada,region,comunas),~paste0('dip2_',.))

# Save voting patterns
filename <- str_c('./scraped_data/vote_pattern_',split_name,'.rds')
saveRDS(vote_pattern_period_9,filename)







}






