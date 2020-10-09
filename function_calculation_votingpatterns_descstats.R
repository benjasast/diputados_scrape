####################################################################
# Program: function_calculate_votingpatterns_descstats
# Author: Benjamin Sas Trakinsky
# Fecha: 05/10/20
####################################################################


# Custom functions -------------------------------------

# Check how many votes together
source('function_count_voting_pattern.R')


# Function ----------------------------------------------------------------

calculate_votingpatterns_descstats <- function(df_votacion_detalle,df_coalition_info,
                                               df_coalition_relationships = coalition_relationships,
                                               split_name,df_political_affiliation = all_diputados_winfo){


# Calculate voting pattern (counts) ---------------------------------------

vote_pattern <- count_voting_pattern(df_votacion_detalle)

# Attach political affiliation
vote_pattern_affiliation <- vote_pattern %>% 
  mutate_at(vars(dip_1,dip_2), ~ as.double(.)) %>% 
  left_join(df_political_affiliation, by = c("dip_1" = "diputado_id")) %>% # Join with affiliation data for dip1
  rename_at(vars(party,bancada,region,comunas),~paste0('dip1_',.)) %>% 
  left_join(df_political_affiliation, by = c("dip_2" = "diputado_id")) %>% # Join with affiliation data for dip2
  rename_at(vars(party,bancada,region,comunas),~paste0('dip2_',.))

# Add coalition information -----------------------------------------------

vote_pattern_coalition <- vote_pattern_affiliation %>% 
  left_join(df_coalition_info, by = c("dip1_party" = "party") ) %>% 
  rename(dip1_coalition = coalition) %>% 
  left_join(df_coalition_info, by = c("dip2_party" = "party") ) %>% 
  rename(dip2_coalition = coalition)


# Add pair information (cross-party, cross-coaliation,etc) ----------------

# Add types of pair
vote_pattern_pairinfo <- vote_pattern_coalition %>% 
  left_join(df_coalition_relationships, by = c("dip1_coalition" = "coalition1", "dip2_coalition" = "coalition2")) %>% 
  left_join(df_coalition_relationships, by = c("dip2_coalition" = "coalition1", "dip1_coalition" = "coalition2")) %>% 
  mutate(pair_coalition = ifelse(!is.na(pair_coalition.x),pair_coalition.x,pair_coalition.y),
         pair_coalition_gen = ifelse(!is.na(pair_coalition_gen.x),pair_coalition_gen.x,pair_coalition_gen.y) ) %>% 
  select(-ends_with('.y'),- ends_with('.x') )


# Add normalized variables ------------------------------------------------

n_votes_split <- df_votacion_detalle %>% 
  select(vote_id) %>% 
  distinct() %>% 
  nrow()

vote_pattern_normalized <- vote_pattern_pairinfo %>% 
  mutate_at(vars(n_same,n_opposite), .funs = list(bothpresent = ~./votaciones_both_present) ) %>% 
  mutate_at(vars(n_same,n_opposite), .funs = list(total = ~./nvotes_p9) ) 


# Calculate descriptive statistics ----------------------------------------


# Descriptive statistics --------------------------------------------------

get_descriptive_stats <- function(group_var,eval_var1,eval_var2){
  
  gvar <- enquo(group_var)
  eval1 <- enquo(eval_var1)
  eval2 <- enquo(eval_var2)
  
  vote_pattern_normalized %>% 
    group_by(!!gvar) %>% 
    summarise(avg_same = mean(!!eval1, na.rm = TRUE),
              avg_opp = mean(!!eval2, na.rm = TRUE))
    
}

# Grab coalition information: Both present normalization
ds_coalition_bp <- get_descriptive_stats(pair_coalition,n_same_bothpresent,n_opposite_bothpresent)

# Grab coalition information: total normalization
ds_coalition_total <- get_descriptive_stats(pair_coalition,n_same_total,n_opposite_total)

# Grab coalition_gen information: Both present normalization
ds_coalition_gen_bp <- get_descriptive_stats(pair_coalition_gen,n_same_bothpresent,n_opposite_bothpresent)

# Grab coalition_gen information: total normalization
ds_coalition_gen_total <- get_descriptive_stats(pair_coalition_gen,n_same_total,n_opposite_total)



# Output ------------------------------------------------------------------

# List with network data and descriptive stats
output <- list(network = vote_pattern_normalized,ds1 = ds_coalition_bp,ds2 = ds_coalition_total,ds3 = ds_coalition_gen_bp,ds4 = ds_coalition_gen_total)

# Save output
filename <- str_c('./scraped_data/network_n_stats_',split_name,'.rds')
saveRDS(output,filename)

# Return
output

}






