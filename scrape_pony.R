
# Install required packages if needed, and enter your username and password on 
# EquinePassion

library(rvest)
library(tidyverse)
library(googlesheets4)

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

pony_scraper("3087926", logged_in) # erwachsen eigen
pony_scraper("4592324", logged_in) # fohlen eigen

pony_scraper("2566804", logged_in) # erwachsen fremd
pony_scraper("4539300", logged_in) # fohlen fremd

pony_scraper("3827343", logged_in) # erwachsen händler
pony_scraper("4360096", logged_in) # fohlen händler

pony_scraper("4405124", logged_in) # fohlen pv

pony_scraper("2719360", logged_in) # erwachsen auktion

pony_scraper("2732991", logged_in)

## ADD TO SHEET ----------------------------------------------------------------

sheet_url <- "sheet_url" # enter google sheet id

to_add <- pony_scraper("4599145", logged_in)

# Log in with google account (can be pre-authorized via googlesheets4) and add 
# new pony to sheet
update_ponies(to_add, sheet_url, "ponies")
