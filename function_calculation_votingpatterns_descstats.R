####################################################################
# Program: function_calculate_votingpatterns_descstats
# Author: Benjamin Sas Trakinsky
# Fecha: 05/10/20
####################################################################


# Custom functions -------------------------------------

# Check how many votes together
source('function_count_voting_pattern.R')

# Function ----------------------------------------------------------------

calculate_votingpatterns <- function(df_votacion_detalle,split_name){

# Calculate voting pattern (counts) ---------------------------------------

vote_pattern <- count_voting_pattern(df_votacion_detalle)

# Add normalized variables ------------------------------------------------

# Normalizing variable is number of times both were present together in period
vote_pattern_normalized <- vote_pattern %>% 
  mutate_at(vars(n_same,n_opposite,n_abst), .funs = list(bothpresent = ~./votaciones_both_present) )

# Output ------------------------------------------------------------------

# List with network data and descriptive stats
output <- vote_pattern_normalized

# Save output
filename <- str_c('./scraped_data/network_n_stats_',split_name,'.rds')
saveRDS(output,filename)

# Return
output

}






