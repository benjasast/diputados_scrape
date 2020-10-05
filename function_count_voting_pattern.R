#########################################################################
# Program: function_count_votintg_pattern
# Author: Benjamin Sas Trakinsky
# Date: 04/10/2020
########################################################################


count_voting_pattern <- function(df_votes_detail_long){

  # Grab all pairs from data ------------------------------------------------
  
  dip_ids <- df_votes_detail_long %>% 
    select(diputado_id) %>% 
    distinct() %>% 
    as_vector()
  
  length(dip_ids)
  
  pairs <- combn(dip_ids,2) %>% 
    t() %>% 
    as_tibble() %>% 
    rename(dip1 = V1,
           dip2 = V2)
  
  
  # function for voting patterns of pairs ----------------------------------------
  
  pair_voting_pattern <- function(id1,id2){
  
    votes_only_pair <- df_votes_detail_long %>% 
      filter(diputado_id==id1 | diputado_id==id2)
    
    votes_both_present <- votes_only_pair %>% 
      group_by(vote_id) %>% 
      mutate(n_obs = n()) %>% 
      filter(n_obs==2) %>% 
      mutate(same_vote = first(vote)==last(vote),
             opposite_vote = first(vote)=="AFIRMATIVO" & last(vote)=="CONTRA" |
               first(vote)=="CONTRA" & last(vote)=="AFIRMATIVO" ) %>% 
      slice(1) %>% # Keep one observation only
      ungroup()
    
     summary_vote_pattern <- votes_both_present %>% 
      summarise(n_same = sum(same_vote),
                n_opposite = sum(opposite_vote) ) %>% 
      mutate(dip_1 = id1,
             dip_2 = id2,
             votaciones_both_present = nrow(votes_both_present)) 
     
     # Output
     summary_vote_pattern
     
  }
  
  # Try function
  #pair_voting_pattern("1017","1018")
  
  
  # Extract voting patterns of pairs ----------------------------------------
  
  # Grab them in parallel
  plan(multisession)
  
  try <- future_map2(pairs$dip1,pairs$dip2,pair_voting_pattern) %>% 
    reduce(union_all)

}












 