
#' Scrape the data of a horse in EquinePassion
#'
#' @param horse_id Character. Horse's id as listed in the passport.
#' @param session An rvest session where you are already logged in with your 
#'                EquinePassion user ID. See main script.
#' @param flaxen Integer. Can either be 1 = visible flaxen, .5 = hidden flaxen 
#'               or 0 = no flaxen. Is not testable and thus needs to be set 
#'               manually. Defaults to 0.
#'
#' @return A data frame with one row, containing the horse's relevant data.
#' 
#' @details
#' Uses the rvest package to scrape information from the web, and the tidyverse 
#' to wrangle the data.
#' 

pony_scraper <- function(horse_id, session, flaxen = 0) {
  
  flaxen <- 0 # needs to be set manually
  
  ## SETTINGS --------------------------------------------------------------------
  
  n_disciplines <- 11
  
  ## GENERAL ---------------------------------------------------------------------
  
  horse <- 
    paste0(
      "https://www.equinepassion.de/intern/index.php?page=pferd&pferd=", 
      horse_id, 
      "&kat=allgemein&s=&f=&g="
    )
  
  steckbrief <- 
    session %>% 
    session_jump_to(horse)
  
  steckbrief_text <- 
    steckbrief %>% 
    html_elements(xpath = '//*[@id="inhalt"]/text()') %>% 
    as.character()
  
  registered_status <- 
    gsub(
      "Papiere: ", "", steckbrief_text[grepl("Papiere:", steckbrief_text)]
    )
  
  sex_german <- 
    gsub(
      "Geschlecht: ", "", steckbrief_text[grepl("Geschlecht:", steckbrief_text)]
    )
  
  # Foals have two values (current + expected height). Filter out expected height.
  height <- steckbrief_text[grepl("Stock", steckbrief_text)]
  
  if (grepl("Endmaß", height)) {
    height <- gsub(".*Endmaß: (.+) cm.*", "\\1", height)
  } else {
    height <- as.numeric(gsub(",", ".", gsub("[^,0-9]+", "", height)))
  }
  
  ## BLOOD PERCENTAGES -----------------------------------------------------------
  
  poa <- steckbrief_text[grepl("Pony of the Americas", steckbrief_text)]
  poa <- 
    as.numeric(
      ifelse(
        length(poa) == 0,
        0,
        gsub("[^.0-9]+", "", poa)
      )
    )
  
  shetland <- steckbrief_text[grepl("Shetlandpony", steckbrief_text)]
  shetland <- 
    as.numeric(
      ifelse(
        length(shetland) == 0,
        0,
        gsub("[^.0-9]+", "", shetland)
      )
    )
  
  paint <- steckbrief_text[grepl("American Paint Horse", steckbrief_text)]
  paint <- 
    as.numeric(
      ifelse(
        length(paint) == 0,
        0,
        gsub("[^.0-9]+", "", paint)
      )
    )
  
  quarter <- steckbrief_text[grepl("American Quarter Horse", steckbrief_text)]
  quarter <- 
    as.numeric(
      ifelse(
        length(quarter) == 0,
        0,
        gsub("[^.0-9]+", "", quarter)
      )
    )
  
  appaloosa <- steckbrief_text[grepl("Appaloosa", steckbrief_text)]
  appaloosa <- 
    as.numeric(
      ifelse(
        length(appaloosa) == 0,
        0,
        gsub("[^.0-9]+", "", appaloosa)
      )
    )
  
  ## COLOUR GENES ----------------------------------------------------------------
  
  gene_url <- 
    paste0(
      "https://www.equinepassion.de/intern/index.php?page=pferd&kat=training&pferd=",
      horse_id,
      "&kat=zucht&s=&f=&g="
    )
  
  gene_page <- 
    session %>% 
    session_jump_to(gene_url)
  
  extension <- 
    gene_page %>% 
    html_elements(xpath = '//*[@id="inhalt"]/div[11]/div[1]') %>% 
    html_text()
  extension <- gsub("(Extension|\\/| )", "", extension)
  extension <- ifelse(extension %in% c("beantragt", "nicht getestet"), NA, extension)
  
  agouti <- 
    gene_page %>% 
    html_elements(xpath = '//*[@id="inhalt"]/div[11]/div[2]') %>% 
    html_text()
  agouti <- gsub("(Agouti|\\/| )", "", agouti)
  agouti <- ifelse(agouti %in% c("beantragt", "nicht getestet"), NA, agouti)
  
  cream_pearl <- 
    gene_page %>% 
    html_elements(xpath = '//*[@id="inhalt"]/div[11]/div[3]') %>% 
    html_text()
  cream_pearl <- gsub("(Cream|Pearl|\\/| )", "", cream_pearl)
  cream_pearl <- ifelse(cream_pearl %in% c("beantragt", "nicht getestet"), NA, cream_pearl)
  
  dun <- 
    gene_page %>% 
    html_elements(xpath = '//*[@id="inhalt"]/div[11]/div[4]') %>% 
    html_text()
  dun <- gsub("(Dun|\\/| )", "", dun)
  dun <- ifelse(agouti %in% c("beantragt", "nicht getestet"), NA, dun)
  
  champagne <- 
    gene_page %>% 
    html_elements(xpath = '//*[@id="inhalt"]/div[11]/div[5]') %>% 
    html_text()
  champagne <- gsub("(Champagne|\\/| )", "", champagne)
  champagne <- ifelse(champagne %in% c("beantragt", "nicht getestet"), NA, champagne)
  
  mushroom <- 
    gene_page %>% 
    html_elements(xpath = '//*[@id="inhalt"]/div[11]/div[6]') %>% 
    html_text()
  mushroom <- gsub("(Mushroom|\\/| )", "", mushroom)
  mushroom <- ifelse(mushroom %in% c("beantragt", "nicht getestet"), NA, mushroom)
  
  silver <- 
    gene_page %>% 
    html_elements(xpath = '//*[@id="inhalt"]/div[11]/div[7]') %>% 
    html_text()
  silver <- gsub("(Silver|\\/| )", "", silver)
  silver <- ifelse(silver %in% c("beantragt", "nicht getestet"), NA, silver)
  
  graying <- 
    gene_page %>% 
    html_elements(xpath = '//*[@id="inhalt"]/div[11]/div[8]') %>% 
    html_text()
  graying <- gsub("(Graying|\\/| )", "", graying)
  graying <- ifelse(graying %in% c("beantragt", "nicht getestet"), NA, graying)
  
  kit <- 
    gene_page %>% 
    html_elements(xpath = '//*[@id="inhalt"]/div[11]/div[9]') %>% 
    html_text()
  kit <- gsub("(KIT|\\/| )", "", kit)
  kit <- ifelse(kit %in% c("beantragt", "nicht getestet"), NA, kit)
  
  overo <- 
    gene_page %>% 
    html_elements(xpath = '//*[@id="inhalt"]/div[11]/div[10]') %>% 
    html_text()
  overo <- gsub("(Overo|\\/| )", "", overo)
  overo <- ifelse(overo %in% c("beantragt", "nicht getestet"), NA, overo)
  
  # 1
  "Leopard & Patn1 LP/lp   n/n"
  # 2
  "Leopard & Patn1 lp/lpPatn1 beantragt"
  
  leopard <- 
    gene_page %>% 
    html_elements(xpath = '//*[@id="inhalt"]/div[11]/div[11]') %>% 
    html_text()
  leopard <- sub("Leopard & Patn1 ", "", leopard)
  first_word_lp <- str_extract(leopard, "^[A-z]+")
  
  if (first_word_lp %in% c("nicht", "beantragt")) {
    leopard <- NA
  } else {
    # Ugly hardcoded solution because Pat1 messes up the structure, depending 
    # on whether it's tested or not ...
    leopard <- substr(leopard, 1, 5)
  }
  leopard <- sub("\\/", "", leopard)
  
  splashed_white <- 
    gene_page %>% 
    html_elements(xpath = '//*[@id="inhalt"]/div[11]/div[12]') %>% 
    html_text()
  splashed_white <- gsub("(Splashed White|\\/| )", "", splashed_white)
  splashed_white <- ifelse(splashed_white %in% c("beantragt", "nicht getestet"), NA, splashed_white)
  
  dapples <- 
    ifelse(
      grepl("Dapples", steckbrief_text[grepl("Farbe:", steckbrief_text)]),
      1,
      0
    )
  
  rabicano <- 
    ifelse(
      grepl("Rabicano", steckbrief_text[grepl("Zeichnung:", steckbrief_text)]),
      1,
      0
    )
  
  sooty <-
    ifelse(
      grepl("Sooty", steckbrief_text[grepl("Farbe:", steckbrief_text)]),
      1,
      0
    )
  
  ## TRAIL -----------------------------------------------------------------------
  
  trail_url <- 
    paste0(
      "https://www.equinepassion.de/intern/index.php?page=pferd&kat=training&pferd=",
      horse_id,
      "&disz=trail&s=&f=&g=#training"
    )
  
  trail_page <- 
    session %>% 
    session_jump_to(trail_url)
  
  trail_text <- 
    trail_page %>% 
    html_elements(xpath = '//*[@id="inhalt"]/text()') %>% 
    as.character()
  
  trail_potential <- trail_text[grepl("Turnierklasse", trail_text)]
  
  # For grown horses, the trail potential is a single value on the training page
  # For foals, the training page is a list of all disciplines. Since the order is 
  # customised for every player, we need to find the position of the trail 
  # potential first
  # get list of disciplines (there are 11)
  if (length(trail_potential) != 1) {
    discipline_order <- vector("character", n_disciplines)
    
    for (i in 1:n_disciplines) {
      discipline_order[i] <- 
        as.character(
          html_elements(
            trail_page, 
            xpath = paste0('//*[@id="inhalt"]/span[', i, ']/span')
          )
        )
    }
    
    trail_position <- which(grepl("Trail", discipline_order))
    trail_potential <- trail_potential[trail_position]
  }
  
  trail_potential <- gsub(".*\\((.+) Punkte.*", "\\1", trail_potential)
  trail_potential <- as.numeric(trail_potential)
  
  ## REINING ---------------------------------------------------------------------
  
  reining_url <- 
    paste0(
      "https://www.equinepassion.de/intern/index.php?page=pferd&kat=training&pferd=",
      horse_id,
      "&disz=reining&s=&f=&g=#training"
    )
  
  reining_page <- 
    session %>% 
    session_jump_to(reining_url)
  
  reining_text <- 
    reining_page %>% 
    html_elements(xpath = '//*[@id="inhalt"]/text()') %>% 
    as.character()
  
  reining_potential <- reining_text[grepl("Turnierklasse", reining_text)]
  
  if (length(reining_potential) != 1) {
    discipline_order <- vector("character", n_disciplines)
    
    for (i in 1:n_disciplines) {
      discipline_order[i] <- 
        as.character(
          html_elements(
            trail_page, 
            xpath = paste0('//*[@id="inhalt"]/span[', i, ']/span')
          )
        )
    }
    
    reining_position <- which(grepl("Reining", discipline_order))
    reining_potential <- reining_potential[reining_position]
  }
  
  reining_potential <- gsub(".*\\((.+) Punkte.*", "\\1", reining_potential)
  reining_potential <- as.numeric(reining_potential)
  
  ## BUILD DATA FRAME ------------------------------------------------------------
  
  output <- 
    data.frame(
      name =
        steckbrief %>% 
        html_elements(xpath = '//*[@id="inhalt"]/a[3]/b') %>% 
        as.character() %>% 
        gsub("<.*?>", "", .),
      registered = 
        case_when(
          registered_status == "nein (" ~ 0,
          (
            registered_status == "beantragt"  & 
              grepl(
                "Pintaloosa Pony", 
                steckbrief_text[grepl("Rasse:", steckbrief_text)]
              )
          ) ~ .5,
          (
            registered_status == "ja" & 
              grepl(
                "Pintaloosa Pony", 
                steckbrief_text[grepl("Rasse:", steckbrief_text)]
              )
          ) ~ 1,
          TRUE ~ 0
        ),
      id = gsub("[^0-9]+", "", steckbrief_text[grepl("ID:", steckbrief_text)]),
      breeding_year = 
        gsub(
          "[^0-9]+", "", steckbrief_text[grepl("Geburtsdatum:", steckbrief_text)]
        ),
      sex = 
        case_when(
          sex_german == "Hengst " ~ "stallion",
          sex_german == "Stute " ~ "mare"
        ),
      height = height,
      trail = trail_potential,
      reining = reining_potential,
      superhorse = trail_potential + reining_potential,
      skin = gsub("Statur: |(-Typ)", "", steckbrief_text[grepl("Statur:", steckbrief_text)]),
      poa = poa,
      shetland = shetland,
      paint = paint,
      quarter = quarter,
      appaloosa = appaloosa,
      extension = extension,
      agouti = agouti,
      cream_pearl = cream_pearl,
      dun = dun,
      champagne = champagne,
      mushroom = mushroom,
      silver = silver,
      graying = graying,
      kit = kit,
      overo = overo,
      leopard = leopard,
      splashed_white = splashed_white,
      flaxen = flaxen,
      dapples = dapples,
      rabicano = rabicano,
      sooty = sooty
    )
  
  return(output)
}
