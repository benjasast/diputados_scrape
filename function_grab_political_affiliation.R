####################################################################
# Program: function_grab_political_affiliation
# Author: Benjamin Sas Trakinsky
# Fecha: 04/10/20
####################################################################

grab_political_affiliation <- function(dip_id){

  # Function - diputado_id and grab political affiliation and other variables ------------------------------
  vote_detalle_base_url_1 <- 'https://www.camara.cl/diputados/detalle/ficha_parlamentaria.aspx?prmId='
  vote_detalle_base_url_2 <- '#ficha-diputados'
  
  

  # Visit website -----------------------------------------------------------
  url <- str_c(vote_detalle_base_url_1,dip_id,vote_detalle_base_url_2)
  webpage <- read_html(url)
  
  
  # Extract information -----------------------------------------------------
  
  # Relevant info in HTML
  rel_info <- webpage %>% 
    html_node(".m-left14") %>% 
    html_text()
  
  # Variables to be extracted
  info_to_extract <- tribble(
    ~variable, ~regex,
    'party', 'Partido:',
    'bancada', 'Bancada:',
    'region', 'Región:',
    'comunas', 'Comunas:')
  
  # Extract from relevant text
  function_extract_info <- function(regex){
    
    regex_1 <- str_c(regex,'.*')
    regex_2 <- str_c(regex,' ')
    
    rel_info %>% 
      str_extract(regex_1) %>% 
      str_remove(regex_2)
    
  }
  
  # Vectorized version
  function_extract_info_V <- Vectorize(function_extract_info)
  
  # Get all the info into DF
  info_extracted <- info_to_extract %>% 
    mutate(value = function_extract_info_V(regex) ) %>% 
    mutate(value = str_remove(value,'^\\s+')) # remove leading spaces
  
  # Output ------------------------------------------------------------------
  
  info_extracted

}


