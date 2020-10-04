#########################################################################
# Program: function_grab_diputados_personal_information
# Author: Benjamin Sas Trakinsky
# Date: 03/10/2020
########################################################################

library(xml2)
library(tidyverse)
library(magrittr)
library(furrr)


grab_diputados_personal_information <- function(id_legislative_period){

  # Visit each periodo legislativo and grab IDs -----------------------------
  
  diputado_base_url <- 'http://opendata.congreso.cl/wscamaradiputados.asmx/getDiputados_Periodo?prmPeriodoID='
  
  # Visit website -----------------------------------------------------------
  
  url <- str_c(diputado_base_url,id_legislative_period)
  
  webpage2 <- XML::xmlParse(url)
  data_frame <- XML::xmlToDataFrame(webpage2)
  

  # Output data frame -------------------------------------------------------

  # Out the dataframe as it is
  data_frame
  
}








