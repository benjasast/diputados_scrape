library(RSelenium)
library(wdman)
library(rvest)
library(tidyverse)
library(magrittr)

rm(list=ls())


# Open docker -------------------------------------------------------------
remDr <- rsDriver(port = 44440L,
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

# From the transcript of each session (boletin de sesion) we will grab the numbers of each boletin (of laws) that were discussed in the session
grab_boletin_nums_session <- function(session_id){

  # Specify the session
  session_id <- session_id %>% as.character()
  boletin_session_url <- str_c(landing,'/wscamaradiputados.asmx/getSesionBoletinXML?prmSesionID=',session_id)
  
  # Open the boletin of the session
  remDr2$navigate(boletin_session_url)
  
  Sys.sleep(15)
  
  # Grab the text from the boletin
  url <- remDr2$getPageSource()[[1]]
  boletin_session_xml <- read_xml(url)
  
  boletin_session_text <- boletin_session_xml %>% 
    xml_contents() %>% 
    as.character()
  
  # Account for parsing error - from the website
  check_first_line <- boletin_session_text[[1]]
  check_first_line_text <- check_first_line %>% str_extract('.{17}')
  
  # Condition -- parsing error
  if (check_first_line_text=="XML Parsing Error"){
    print(str_c('session_id number ',session_id,' does not have a session boletin website'))
  }
  else{
    # Grab all discussed boletin in the session
    boletin_text_withunknown <- boletin_session_text %>% 
      str_extract_all('\\d*-\\d.*\\.') %>% 
      str_extract_all('(\\d*-\\d*|\\(\\d*-\\d*\\))') # I don't know what the (410-359),(22-360) terms are?
    
    boletin_list_withunknown <- tibble(boletin_num = boletin_text_withunknown[[1]])
    
    # Keep only observations known to be a boletin_num
    boletin_list <- boletin_list_withunknown %>%
      mutate(session_id = session_id) %>% 
      filter(!grepl('\\(.*',boletin_num)) %>% 
      filter(!grepl('^-', boletin_num)) %>% 
      mutate(len = str_count(boletin_num)) %>% 
      filter(len<mean(len,na.rm = TRUE)+1)
    
    boletin_list
  }
  
}

# Grab all sessions with data (Sessions have data starting in legislatura_id==26 , which is in 2000)
session_id_list <- df_sessions %>% 
  filter(legislatura_id>=26) %>% 
  select(session_id)

session_id_list_vec <- session_id_list$session_id

# Let's apply the function
df_boletin_nums <- map(session_id_list_vec,grab_boletin_nums_session)
  
# Filter out non-tibble results - and make a full_join
df_boletin_nums <- df_boletin_nums[lapply(df_boletin_nums, length) > 1] %>% 
  reduce(full_join)

df_boletin_nums




# (option 2) Grab all boleting numbers in a given session and vote IDs ---------------------------------------



# Grab Legislaturas from dropdown -----------------------------------------

# Grab information from sesiones
sesiones_landing <- 'https://www.camara.cl/legislacion/sesiones_sala/sesiones_sala.aspx'
remDr2$navigate(sesiones_landing)

# Grab all legislaturas in the dropdown menu
legislaturas_website_list <- xml2::read_html(remDr2$getPageSource()[[1]]) %>% 
  rvest::html_nodes("#ContentPlaceHolder1_ContentPlaceHolder1_ddlLegislatura") %>%
  rvest::html_text() %>%
  dplyr::tibble(legislatura_name = .)

legislaturas_webiste_numbers <- legislaturas_website_list %$% 
  str_extract_all(legislatura_name,'\\d\\d\\d')

legislaturas_website_list <- tibble(numbers = legislaturas_webiste_numbers %>% unlist) %>% 
  mutate(legislatura_text = str_c('Legislatura',' ',numbers),
         row = row_number(),
         css_path = str_c('#ContentPlaceHolder1_ContentPlaceHolder1_ddlLegislatura > option:nth-child(',row,')')) 


# Click legislatura dropdown

# Function to go to click a certain legislatura
click_website_legislatura <- function(legislatura_css_path){
  click_xpath('//*[@id="ContentPlaceHolder1_ContentPlaceHolder1_ddlLegislatura"]')
  element <- remDr2$findElement(using = 'css selector', legislatura_css_path)
  element$clickElement()  
}



# Grab sesiones from dropdown ---------------------------------------------


# Grab all legislaturas in the dropdown menu
sessions_website_list <- xml2::read_html(remDr2$getPageSource()[[1]]) %>% 
  rvest::html_nodes("#ContentPlaceHolder1_ContentPlaceHolder1_ddlSesion") %>%
  rvest::html_text() %>%
  dplyr::tibble(sessions_name = . )

sessions_webiste_numbers <- sessions_website_list %$% 
  str_extract_all(sessions_name,'\\d\\d\\d')

legislaturas_website_list <- tibble(session_number = sessions_webiste_numbers %>% unlist) %>% 
         mutate(row = row_number(),
         css_path = str_c('#ContentPlaceHolder1_ContentPlaceHolder1_ddlSesion > option:nth-child(',row,')')) 


# Click session dropdown

# Function to go to click a certain legislatura
click_website_session <- function(session_css_path){
  click_xpath('//*[@id="ContentPlaceHolder1_ContentPlaceHolder1_ddlSesion"]')
  element <- remDr2$findElement(using = 'css selector', session_css_path)
  element$clickElement()  
}

# Try
#click_website_session(legislaturas_website_list$css_path[[2]])


# Grab information from tabla for each legislatura-session combo ---------

# Basic info (session and date)
basic_info <- xml2::read_html(remDr2$getPageSource()[[1]]) %>% 
  rvest::html_nodes("#descripcion") %>%
  rvest::html_text()


# How many 'Proyectos' to scrape?
n_proyectos <- xml2::read_html(remDr2$getPageSource()[[1]]) %>% 
  rvest::html_nodes("div.box-proyecto") %>% 
  html_children() %>% 
  length()

# Click on each Votaciones tab
votaciones_tab_xpath <- tibble(n = 1:n_proyectos) %>% 
  mutate(votaciones_tab_xpath =   str_c('/html/body/div[1]/form/div[3]/section/div[2]/article/div[3]/div[', n ,']/div/div/ul/li[5]/a/span') )

# Click for all of them
click_votaciones_tab <- function(votacion_tab_xpath){
  click_xpath(votacion_tab_xpath)
  webElem <- remDr2$findElement(using = 'xpath', value = xpath )
  }

map(votaciones_tab_xpath$votaciones_tab_xpath,click_votaciones_tab)

# Grab information for each proyecto --------------------------------------

# Scrape information from each proyecto
proyecto <- xml2::read_html(remDr2$getPageSource()[[1]]) %>% 
  html_nodes("div.box-proyecto:nth-child(1)")

# Tipo de proyecto y boletin aca
strong_elements <- proyecto %>% 
  html_nodes("strong") %>% 
  html_text()

proyecto_tipo <- strong_elements[[1]]
proyecto_boletin <- strong_elements[[1]]

proyecto_materia <- proyecto %>% 
  html_node("h2") %>% 
  html_text()

proyecto_extrainfo <- proyecto %>% 
  html_node("#tab1-1 > p:nth-child(3)") %>% 
  html_text()

