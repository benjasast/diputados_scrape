####################################################################
# Program: function_grab_voting_details_and_info
# Author: Benjamin Sas Trakinsky
# Fecha: 03/10/20
####################################################################

grab_voting_details_and_info <- function(vote_id){

  # Function - visit vote_id and extract votes ------------------------------
  vote_detalle_base_url <- 'http://opendata.congreso.cl/wscamaradiputados.asmx/getVotacion_Detalle?prmVotacionID='
  
  # Visit website -----------------------------------------------------------
  
  url <- str_c(vote_detalle_base_url,vote_id)
  webpage <- read_xml(url)

  # Check if website is empty or not ----------------------------------------
  
  length_website_attr <- webpage %>% 
    xml_contents() %>% 
    length()
  
  if (length_website_attr==0){
    print(str_c('vote_id ',vote_id, ' is invalid'))
  }
  
  if (length_website_attr>0 & length_website_attr!=16){
    
    output <- tibble(vote_id = vote_id,
                     n_attributes = length_website_attr)
    
    write_csv(output,'special_vote_id.csv',append = TRUE)
    print(str_c('vote_id ',vote_id, ' is special'))

  }
  

  # Continue only if website is not-empty
  if (length_website_attr==16){
    
    # Variables to scrape: voting info -----------------------------------------------------
    
    # Grab relevant content by index:
    vote_information <- tribble(
      ~variable, ~ index,
      "vote_id", 1,
      "fecha" , 2,
      "tipo_codigo" , 3,
      "resultado", 4,
      "quorum", 5,
      "sesion", 6,
      "boletin", 7,
      "articulo", 8,
      "tramite", 9,
      "informe", 10,
      "total_afir", 11,
      "total_neg", 12,
      "total_abs", 13,
      "total_dispen", 14)
    
    # Nest variables
    vote_information_nested <- vote_information %>% 
      group_by(variable) %>% 
      nest()
    
    # Function to grab voting info --------------------------------------------
    grab_vote_info_variable <- function(data_df,text_option=TRUE){
      n_index <- data_df %>% 
        select(index)
      
      index <- n_index[[1]]
      
      # Text option
      if (text_option==TRUE){
        xml_child(webpage,index) %>% 
          xml_contents() %>% 
          xml_text()
      }
      
      else{
        # Non-text option
        xml_child(webpage,index) %>% 
          xml_contents()
      }
      
    }
    
    # Implement voting info function and format -------------------------------
    
    vote_information_ready <- vote_information_nested %>% 
      mutate(value = map(data,grab_vote_info_variable ) ) %>% 
      unnest(cols = c(data, value)) %>% 
      ungroup()
    
    # Format information
    vote_id_data <- vote_information_ready %>% 
      filter(variable=="vote_id") %>% 
      select(value) %>% 
      first()
    
    vote_information_format <- vote_information_ready %>% 
      filter(variable!="vote_id") %>% 
      mutate(vote_id = vote_id_data)
    
    
    # Grab voting detalle -----------------------------------------------------
    
    vote_detalle <- tribble(
      ~ variable, ~ index,
      "votos", 15,
      "pareos", 16)
    
    vote_detalle_nested <- vote_detalle %>% 
      group_by(variable) %>% 
      nest()
    
    vote_detalle_ready <- vote_detalle_nested %>% 
      mutate(value = map(data,grab_vote_info_variable))
    
    df_vote_details <- tibble(
      diputado_id = vote_detalle_ready$value[[1]] %>% 
        str_extract_all(("\\d+")) %>% 
        unlist(),
      vote = vote_detalle_ready$value[[1]] %>% 
        str_extract_all(("[A-Z]+$")) %>% 
        unlist() )
    
    df_vote_details_format <- df_vote_details %>% 
      mutate(vote_id = vote_id_data)
    
    
    # Output with all information ---------------------------------------------
    
    # Output will be a list containing the voting information (metadata) and voting details
    list_vote_output <- list(vote_information_format,df_vote_details_format)
    
    # Return output
    list_vote_output
    
  }

}
