
#' Scrape the data of a horse in EquinePassion
#'
#' @param horse_ids Character vector. Horse id(s) as listed in the passport.
#' @param my_session An rvest session where you are already logged in with your 
#'                EquinePassion user ID. See main script.
#' @param flaxen Integer vector. Can either be 1 = visible flaxen, .5 = hidden 
#'               flaxen or 0 = no flaxen. Is not testable and thus needs to be 
#'               set manually. Defaults to 0.
#'
#' @return A data frame with one row, containing the horse's relevant data.
#' 
#' @details
#' Uses the rvest package to scrape information from the web, and the tidyverse 
#' to wrangle the data.
#' 

pony_scraper <- function(horse_ids, my_session, flaxen = 0) {
  
  ## SETTINGS ------------------------------------------------------------------
  
  n_disciplines <- 11
  output <- data.frame(matrix(nrow = length(horse_ids), ncol = 32))
  names(output) <- 
    c(
      "name", "owner", "registered", "id", "breeding_year", "sex", "height", 
      "trail", "reining", "superhorse", "skin", "poa", "shetland", "paint", 
      "quarter", "appaloosa", "extension", "agouti", "cream_pearl", "dun", 
      "champagne", "mushroom", "silver", "graying", "kit", "overo", "leopard", 
      "splashed_white", "flaxen", "dapples", "rabicano", "sooty"      
    )
  
  ## GENERAL -------------------------------------------------------------------
  
  for (i_horse in seq_along(horse_ids)) {
    
    temp_horse <- horse_ids[i_horse]
    
    horse <- 
      paste0(
        "https://www.equinepassion.de/intern/index.php?page=pferd&pferd=", 
        temp_horse, 
        "&kat=allgemein&s=&f=&g="
      )
    
    steckbrief <- 
      my_session %>% 
      session_jump_to(horse)
    
    steckbrief_text <- 
      steckbrief %>% 
      html_elements(xpath = '//*[@id="inhalt"]/text()') %>% 
      as.character()
    
    # Check if horse still exists
    out_of_game <- any(grepl("abgegeben an|Freizeitreiter", steckbrief_text))
    
    died <- 
      steckbrief %>% 
      html_elements(xpath = '//*[@id="inhalt"]/div/text()') %>% 
      grepl("verstorben", .) %>% 
      any()
    
    if (out_of_game | died) {
      stop(paste0("ID ", temp_horse,  " ist nicht mehr im Spiel."))
    }
    
    horse_name <- 
      steckbrief %>% 
      html_elements(xpath = '//*[@id="inhalt"]/h1') %>% 
      as.character() %>% 
      # delete everything in italics
      gsub("<i>.*</i>", "", .) %>% 
      # delete html
      gsub("<.*?>|\\\n", "", .) %>% 
      trimws()
    
    if (length(steckbrief_text[grepl("Papiere:", steckbrief_text)]) != 0) {
      
      # If owned by a player: Take advantage of the fact that the last person 
      # in the owner history is the last link of "inhalt".
      # Bit of a dirty hack, but there is unfortunately no other way to 
      # consistently identify the owner element.
      owner <- 
        steckbrief %>% 
        html_elements(xpath = '//*[@id="inhalt"]/a/text()') %>% 
        as.character() %>% 
        tail(1)
      
      registered_status <- 
        gsub(
          "Papiere: ", "", steckbrief_text[grepl("Papiere:", steckbrief_text)]
        )
      
      registered_status <-
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
      
      id <- gsub("[^0-9]+", "", steckbrief_text[grepl("ID:", steckbrief_text)])
      
      breeding_year <- 
        gsub(
          "[^0-9]+", "", steckbrief_text[grepl("Geburtsdatum:", steckbrief_text)]
        )
      
      skin <- 
        gsub("Statur: |(-Typ)", "", steckbrief_text[grepl("Statur:", steckbrief_text)])
      
      colour <- steckbrief_text[grepl("Farbe:", steckbrief_text)]
      pattern <- steckbrief_text[grepl("Zeichnung:", steckbrief_text)]
      
    } else {
      # If horse is for sale, structure is different
      handler_steckbrief <- 
        steckbrief %>% 
        html_element(xpath = '//*[@id="inhalt"]/div[2]') %>% 
        html_text()
      
      # If the horse is for sale, we find the current owner (Hengststation, 
      # Händler ...) at the second-to-last element of steckbrief_text (last 
      # element is .)
      owner <- steckbrief_text[length(steckbrief_text) - 1]
      owner <- gsub(".*verkauft an (.+) für.*", "\\1", owner)
      
      # Check if horse is sold at the auction
      if (grepl("wird versteigert", handler_steckbrief)) owner <- "Auktionshaus"
      
      registered_status <- 
        gsub(".*Rasse: (.+)Statur.*", "\\1", handler_steckbrief)
      
      registered_status <-
        ifelse(
          registered_status == "Pintaloosa-Pony mit Papieren",
          1,
          0
        )
      
      sex_german <- gsub(".*Geschlecht: (.+)Alter.*", "\\1", handler_steckbrief)
      
      height <- gsub(".*Stockmaß: (.+)Preis.*", "\\1", handler_steckbrief)
      
      if (grepl("Endmaß", height)) {
        height <- gsub(".*Endmaß: (.+) cm.*", "\\1", height)
      } else {
        height <- as.numeric(gsub(",", ".", gsub("[^,0-9]+", "", height)))
      }
      
      id <- temp_horse
      
      breeding_year <- 
        steckbrief %>% 
        html_elements(xpath = '//*[@id="inhalt"]/text()') %>% 
        as.character()
      
      breeding_year <- 
        gsub("[^0-9]", "", breeding_year[grepl("geboren bei", breeding_year)])
      
      skin <- gsub(".*Statur: (.+)Farbe.*", "\\1", handler_steckbrief)
      skin <- gsub("\\r\\n|(-Typ)", "", skin)
      
      colour <- gsub(".*Farbe: (.+)Zeichnung.*", "\\1", handler_steckbrief)
      pattern <- gsub(".*Zeichnung: (.+)Geschlecht.*", "\\1", handler_steckbrief)
    }
    
    ## BLOOD PERCENTAGES -----------------------------------------------------------
    
    poa <- steckbrief_text[grepl("% Pony of the Americas", steckbrief_text)]
    poa <- 
      as.numeric(
        ifelse(
          length(poa) == 0,
          0,
          gsub("[^.0-9]+", "", poa)
        )
      )
    
    shetland <- steckbrief_text[grepl("% Shetlandpony", steckbrief_text)]
    shetland <- 
      as.numeric(
        ifelse(
          length(shetland) == 0,
          0,
          gsub("[^.0-9]+", "", shetland)
        )
      )
    
    paint <- steckbrief_text[grepl("% American Paint Horse", steckbrief_text)]
    paint <- 
      as.numeric(
        ifelse(
          length(paint) == 0,
          0,
          gsub("[^.0-9]+", "", paint)
        )
      )
    
    quarter <- steckbrief_text[grepl("% American Quarter Horse", steckbrief_text)]
    quarter <- 
      as.numeric(
        ifelse(
          length(quarter) == 0,
          0,
          gsub("[^.0-9]+", "", quarter)
        )
      )
    
    appaloosa <- steckbrief_text[grepl("% Appaloosa", steckbrief_text)]
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
        "https://www.equinepassion.de/intern/index.php?page=pferd&pferd=",
        temp_horse,
        "&kat=zucht"
      )
    
    gene_page <- 
      my_session %>% 
      session_jump_to(gene_url)
    
    gene_boxes <- 
      gene_page %>% 
      html_elements('.flexbox_item_farbgen') %>% 
      html_text()
    
    extension <- gene_boxes[1]
    extension <- gsub("(Extension|\\/| )", "", extension)
    extension <- 
      ifelse(
        extension %in% c("beantragt", "nichtgetestet"), NA_character_, extension
      )
    
    agouti <- gene_boxes[2]
    agouti <- gsub("(Agouti|\\/| )", "", agouti)
    agouti <- 
      ifelse(
        agouti %in% c("beantragt", "nichtgetestet"), NA_character_, agouti
      )
    
    cream_pearl <- gene_boxes[3]
    cream_pearl <- gsub("(Cream|Pearl|\\/| )", "", cream_pearl)
    cream_pearl <- 
      ifelse(
        cream_pearl %in% c("beantragt", "nichtgetestet"), NA_character_, cream_pearl
      )
    
    dun <- gene_boxes[4]
    dun <- gsub("(Dun|\\/| )", "", dun)
    dun <- 
      ifelse(agouti %in% c("beantragt", "nichtgetestet"), NA_character_, dun)
    
    champagne <- gene_boxes[5]
    champagne <- gsub("(Champagne|\\/| )", "", champagne)
    champagne <- 
      ifelse(
        champagne %in% c("beantragt", "nichtgetestet"), NA_character_, champagne
      )
    
    mushroom <- gene_boxes[6]
    mushroom <- gsub("(Mushroom|\\/| )", "", mushroom)
    mushroom <- 
      ifelse(
        mushroom %in% c("beantragt", "nichtgetestet"), NA_character_, mushroom
      )
    
    silver <- gene_boxes[7]
    silver <- gsub("(Silver|\\/| )", "", silver)
    silver <- 
      ifelse(
        silver %in% c("beantragt", "nichtgetestet"), NA_character_, silver
      )
    
    graying <- gene_boxes[8]
    graying <- gsub("(Graying|\\/| )", "", graying)
    graying <- 
      ifelse(
        graying %in% c("beantragt", "nichtgetestet"), NA_character_, graying
      )
    
    kit <- gene_boxes[9]
    kit <- gsub("(KIT|\\/| )", "", kit)
    kit <- 
      ifelse(
        kit %in% c("beantragt", "nichtgetestet"), NA_character_, kit
      )
    
    overo <- gene_boxes[10]
    overo <- gsub("(Overo|\\/| )", "", overo)
    overo <- 
      ifelse(
        overo %in% c("beantragt", "nichtgetestet"), NA_character_, overo
      )
    
    leopard <- gene_boxes[11]
    leopard <- sub("Leopard & Patn1 ", "", leopard)
    first_word_lp <- str_extract(leopard, "^[A-z]+")
    
    if (first_word_lp %in% c("nicht", "Leopard")) {
      leopard <- NA_character_
    } else {
      # Ugly hardcoded solution because Pat1 messes up the structure, depending 
      # on whether it's tested or not ...
      leopard <- substr(leopard, 1, 5)
    }
    leopard <- sub("\\/", "", leopard)
    
    splashed_white <- gene_boxes[12]
    splashed_white <- gsub("(Splashed White|\\/| )", "", splashed_white)
    splashed_white <- ifelse(splashed_white %in% c("beantragt", "nichtgetestet"), NA, splashed_white)
    
    dapples <- 
      ifelse(
        grepl("Dapples", colour),
        1,
        0
      )
    
    rabicano <- 
      ifelse(
        grepl("Rabicano", pattern),
        1,
        0
      )
    
    sooty <-
      ifelse(
        grepl("Sooty", colour),
        1,
        0
      )
    
    ## TRAIL -----------------------------------------------------------------------
    
    trail_url <- 
      paste0(
        "https://www.equinepassion.de/intern/index.php?page=pferd&kat=training&pferd=",
        temp_horse,
        "&disz=trail&s=&f=&g=#training"
      )
    
    trail_page <- 
      my_session %>% 
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
        temp_horse,
        "&disz=reining&s=&f=&g=#training"
      )
    
    reining_page <- 
      my_session %>% 
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
    
    output$name[i_horse] <- horse_name
    
    output$owner[i_horse] <- owner
    output$registered[i_horse] <- registered_status
    output$id[i_horse] <- as.numeric(id)
    output$breeding_year[i_horse] <- as.numeric(breeding_year)
    
    output$sex[i_horse] <- 
      case_when(
        sex_german == "Hengst " ~ "stallion",
        sex_german == "Stute " ~ "mare",
        owner == "Hengststation" ~ "stallion"
      )
    
    output$height[i_horse] <- as.numeric(height)
    output$trail[i_horse] <- as.numeric(trail_potential)
    output$reining[i_horse] <- as.numeric(reining_potential)
    output$superhorse[i_horse] <- trail_potential + reining_potential
    output$skin[i_horse] <- trimws(skin)
    output$poa[i_horse] <- poa
    output$shetland[i_horse] <- shetland
    output$paint[i_horse] <- paint
    output$quarter[i_horse] <- quarter
    output$appaloosa[i_horse] <- appaloosa
    output$extension[i_horse] <- extension
    output$agouti[i_horse] <- agouti
    output$cream_pearl[i_horse] <- cream_pearl
    output$dun[i_horse] <- dun
    output$champagne[i_horse] <- champagne
    output$mushroom[i_horse] <- mushroom
    output$silver[i_horse] <- silver
    output$graying[i_horse] <- graying
    output$kit[i_horse] <- kit
    output$overo[i_horse] <- overo
    output$leopard[i_horse] <- leopard
    output$splashed_white[i_horse] <- splashed_white
    output$flaxen[i_horse] <- flaxen
    output$dapples[i_horse] <- dapples
    output$rabicano[i_horse] <- rabicano
    output$sooty[i_horse] <- sooty
  }
  
  return(output)
}

update_ponies <- function(new_ponies, sheet_url, sheet_name) {
  safety <- readline(cat("Overwrite sheet? (1 = yes, 0 = no)\n"))
  
  if (safety == 0) {
    
    return(cat("Update cancelled."))
    
  } else if (safety == 1) {
    
    ponies <- read_sheet(sheet_url)
    
    # Check if pony already exists in sheet.
    if (any(new_ponies$id %in% ponies$id)) {
      
      confirm_update <- 
        readline(
          cat(
            paste0(
              "The following IDs already exists:\n", 
              paste0(new_ponies$id[new_ponies$id %in% ponies$id], collapse = "\n"),
              "\n\n",
              "Overwrite? (1 = yes, 0 = no)\n"
            )
          )
        )
      
      if (confirm_update == 0) {
        
        return(cat("Update cancelled."))
        
      } else if (confirm_update == 1) {
        
        # Order so data is overwritten in the right places
        ponies <- ponies[order(ponies$id), ]
        new_ponies <- new_ponies[order(new_ponies$id), ]
        
        # never overwrite flaxen column
        flaxen_id <- which(names(ponies) == "flaxen")
        ponies[which(ponies$id %in% new_ponies$id), -flaxen_id] <- 
          new_ponies[ , -flaxen_id]
        sheet_write(ponies, sheet_url, sheet = sheet_name)
        
      } else {
        
        stop("Invalid input. Please enter either 1 (yes) or 0 (no).")
        
      }
      
    } else {
      
      sheet_append(sheet_url, new_ponies, sheet = sheet_name) 
      
    }
    
    return(cat("Sheet updated."))
    
  } else {
    
    stop("Invalid input. Please enter either 1 (yes) or 0 (no).")
    
  }
}
