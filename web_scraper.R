library(RSelenium)
library(wdman)
library(rvest)
library(tidyverse)

rm(list=ls())


# Open docker -------------------------------------------------------------
remDr <- rsDriver(port = 44450L,
                  browser = "firefox")

remDr2 <- remDr[["client"]]


# Landing website ---------------------------------------------------------

# Set window size
remDr2$setWindowSize(1280L, 1024L)

# Open landing website
landing <- 'http://opendata.congreso.cl/'
remDr2$navigate(landing)



# Useful functions --------------------------------------------------------

#1.0 Click on Xpath

click_xpath <- function(xpath){
  webElem <- remDr2$findElement(using = 'xpath', value = xpath )
  remDr2$mouseMoveToLocation(webElement = webElem)
  remDr2$click(1)
  Sys.sleep(2)
}

textbox_enternumber <- function(textbox_xpath,number){
  textbox_xpath <- textbox_xpath
  number <- number %>% as.character()

  textbox_elem <- remDr2$findElement(using = 'xpath', value = textbox_xpath )
  textbox_elem$sendKeysToElement(list(number, key="enter"))
  Sys.sleep(2)
}


# Task 1 - Grab IDs and dates for parliamentary periods (legislaturas) -----------------------------------------

# Tab Legislaturas
tab_legislaturas <- '/html/body/div[1]/div[2]/div[2]/div/table/tbody/tr/td[1]/div[1]/table/tbody/tr[2]/td/table/tbody/tr[2]/td/ul/li[8]/a'
click_xpath(tab_legislaturas)

# New tab is opened we need to switch
windows_handles <- remDr2$getWindowHandles()
remDr2$switchToWindow(windows_handles[[2]])

# Check XML data
icon_datosxml <- '//*[@id="Button1"]'
click_xpath(icon_datosxml)

# Use rvest to get a table of all legislaturas
legislatura_xml <- read_xml(remDr2$getPageSource()[[1]])

# I cannot scrape this somehow, will import as character
legislatura_text <- legislatura_xml %>% 
  xml_contents() %>% 
  as.character()

# Create DF with information
df_legislatura <- tibble(raw_text = legislatura_text)

df_legislatura <- df_legislatura %>% 
  mutate(legislatura_id = str_extract(raw_text,'<ID>.*<\\/ID>') %>% str_replace('\\D*','') %>% str_replace('\\D*$','') ) %>%
  mutate(legislatura_num = str_extract(raw_text,'<Numero>.*<\\/Numero>') %>% str_replace('\\D*','') %>% str_replace('\\D*$','') ) %>% 
  mutate(legislatura_type = str_extract(raw_text,'<Tipo Codigo.*<\\/Tipo>' ) %>%  str_extract('(Extra|Ordi).*ia')) %>% 
  mutate(legislatura_startdate = str_extract(raw_text,'\\d\\d\\d\\d-\\d\\d-\\d\\d')) %>% 
  mutate(legislatura_enddate = str_extract(raw_text,'\\d\\d\\d\\d-\\d\\d-\\d\\d')) %>% 
  select(-raw_text)

df_legislatura %>% 
  tail(2)

# Task 2 - grab all the sessions on a legislatura --------------------------



# Function to grab the session information of all legislaturas

grab_session_info <- function(legislatura_id){

  # Open URL
  sessions_page <- str_c(landing,'/pages/sesiones.aspx')
  remDr2$navigate(sessions_page)
  
  # Input number of a legislatura in a box
  legislatura_id <- legislatura_id %>% as.character()
  textbox_enternumber('//*[@id="txtID"]',legislatura_id)
  
  # rest
  Sys.sleep(10)
  
  # Grab all the text
  url <- remDr2$getPageSource()[[1]]
  sessions_xml <- read_xml(url)
  
  sessions_text <- sessions_xml %>% 
    xml_contents() %>% 
    as.character()
  
  df_sessions <- tibble(raw_text = sessions_text) %>% 
    mutate(legislatura_id = legislatura_id %>% as.integer()) %>% 
    mutate(session_id = str_extract(raw_text,'<ID>.*<\\/ID>') %>% str_replace('\\D*','') %>% str_replace('\\D*$','') ) %>% 
    mutate(session_date = str_extract(raw_text,'\\d\\d\\d\\d-\\d\\d-\\d\\d')) %>% 
    mutate(sessions_starttime = str_extract(raw_text,'<Fecha>.*<\\/Fecha>') %>% str_extract('T.*') %>% str_replace('T',"") %>% str_replace('</Fecha>','')) %>% 
    mutate(sessions_endtime = str_extract(raw_text,'FechaTermino>.*<' ) %>% str_extract('T\\d.*') %>% str_replace('(T)','') %>% str_replace('<','') ) %>% 
    mutate(sessions_type = str_extract(raw_text,'((Extra|Ordi).*ia)|(Especial)')) %>% 
    mutate(sessions_status = str_extract(raw_text,'Codigo.*</Estado>' ) %>% str_extract('>.*<') %>% str_replace('(>)','') %>% str_replace('<','') ) %>% 
    select(-raw_text)
  
  df_sessions

}

# Grab the session information for all legislaturas
legislaturas_id_list <- df_legislatura$legislatura_id
df_sessions <- map(legislaturas_id_list,grab_session_info) %>% reduce(full_join)


# Grab all boletin numbers - in a given session ---------------------------


# Specify the session
session_id <- '2775'
boletin_session_url <- str_c(landing,'/wscamaradiputados.asmx/getSesionBoletinXML?prmSesionID=',session_id)

# Open the boletin of the session
remDr2$navigate(boletin_session_url)

# Grab the text from the boletin
url <- remDr2$getPageSource()[[1]]
boletin_session_xml <- read_xml(url)

boletin_session_text <- boletin_session_xml %>% 
  xml_contents() %>% 
  as.character()


boletin_session_text

# Grab all discussed boletin in the session
check <- boletin_session_text %>% 
  str_extract_all('\\d*-\\d.*\\.') %>% 
  str_extract_all('(\\d*-\\d*|\\(\\d*-\\d*\\))')

check

















