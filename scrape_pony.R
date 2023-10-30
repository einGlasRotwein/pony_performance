
# Install required packages if needed, and enter your username and password on 
# EquinePassion

library(rvest)
library(tidyverse)

source("pony_scrape_function.R")

## LOGIN -----------------------------------------------------------------------

ep_url <- "https://www.equinepassion.de/"
ep_session <- session(ep_url )

login <- 
  ep_session %>% 
  html_element(css = 'form') %>% 
  html_form() %>% 
  html_form_set(
    spieler = "your username", # enter your username
    passwort = "password" # enter your password
  )

logged_in <- ep_session %>% session_submit(login)

## SCRAPE ----------------------------------------------------------------------

pony_scraper("4592324", logged_in)
