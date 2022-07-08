
library(googlesheets4)
library(tidyverse)

julis_theme <- 
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "top")

sheet_url <- "https://docs.google.com/spreadsheets/d/1nfbcVCFfnxbql_A2qRDk6LqIUCsSNn6KPNdzEhu5z-Y/edit#gid=1416189352"
pony <- read_sheet(sheet_url)

pony <- pony %>% 
  mutate(name_ws = paste0("  ", name, "  "))

highlight_horse <- "How Legends R Forged"

pony %>% 
  mutate(highlight = ifelse(name %in% highlight_horse, TRUE, FALSE)) %>% 
  
  ggplot(aes(x = height, y = trail, colour = highlight)) +
  geom_vline(xintercept = 100) +
  geom_vline(xintercept = 140) +
  geom_point(alpha = .5, size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_text(aes(label = name_ws), vjust = .3, hjust = "right") +
  geom_hline(aes(yintercept = mean(trail)), colour = "grey") +
  geom_vline(aes(xintercept = mean(height)), colour = "grey") +
  scale_colour_manual(values = c("black", "red")) +
  julis_theme +
  theme(legend.position = "none")

# Does the horse fulfill the criteria?
pony %>% 
  filter(
    breeding_year <= 2040,
    is.na(registered),
    skin == "Western",
    shetland <= 30 & shetland >= 1 &
      height >= 100 & height <= 140 &
      leopard %in% c("LPlp", "LPLP") &
      (
        kit %in% c("Tobn", "TobTob", "Sbn") |
          splashed_white == "SWSW" |
          overo == "Oo"
      )
  ) %>% 
  select(name, id)

pony %>% 
  mutate(highlight = ifelse(
    breeding_year <= 2039 &
      skin == "Western" &
      shetland <= 30 & shetland >= 1 &
      height >= 100 & height <= 140 &
      leopard %in% c("LPlp", "LPLP") &
      (
        kit %in% c("Tobn", "TobTob", "Sbn") |
          splashed_white == "SWSW" |
          overo == "Oo"
      ),
    TRUE,
    FALSE
  )) %>% 
  
  ggplot(aes(x = height, y = trail)) +
  geom_vline(xintercept = 100) +
  geom_vline(xintercept = 140) +
  geom_point(aes(colour = highlight), alpha = .5, size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_text(aes(label = name_ws, colour = highlight), vjust = .3, hjust = "right") +
  geom_hline(aes(yintercept = mean(trail)), colour = "grey") +
  geom_vline(aes(xintercept = mean(height)), colour = "grey") +
  scale_colour_manual(values = c("black", "red")) +
  julis_theme +
  theme(legend.position = "none")

pony %>% 
  
  ggplot(aes(x = height, y = trail, colour = sex)) +
  geom_point(alpha = .5, size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_text(aes(label = name_ws), vjust = .3, hjust = "right") +
  geom_hline(aes(yintercept = mean(trail)), colour = "grey") +
  geom_vline(aes(xintercept = mean(height)), colour = "grey") +
  julis_theme

dam_shetland <- 50

pony %>% 
  filter(
    sex == "stallion", 
    breeding_year <= 2041,
    # flaxen > 0,
    # grepl("Mu", mushroom),
    # grepl("Prl", cream_pearl),
    # grepl("LP", leopard),
    (
      kit %in% c("Tobn", "TobTob", "Sbn") |
        splashed_white == "SWSW" |
        overo == "Oo"
    )
         ) %>% 
  
  ggplot(aes(x = height, y = trail)) +
  geom_point(aes(colour = shetland <= 60 - dam_shetland), size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_text(aes(label = name_ws), vjust = .3, hjust = "right") +
  geom_hline(aes(yintercept = mean(trail)), colour = "grey") +
  geom_vline(aes(xintercept = mean(height)), colour = "grey") +
  geom_vline(xintercept = 100) +
  geom_vline(xintercept = 140) +
  julis_theme

# Filter Kinderponies
pony %>% 
  filter(
    breeding_year <= 2036,
    height <= 110
  ) %>% 
  arrange(desc(trail))

# Ab 9 Jahre
pony %>% 
  filter(
    breeding_year <= 2036,
    height <= 130
  ) %>% 
  arrange(desc(trail)) %>% 
  select(name, sex, trail)

# Wie viele Pferde erfüllen die Anforderungen?

# Rasseanteile
# 1.0 % bis 30.0 % Shetlandpony
# 
# 70.0 % bis 99.0 % von einer dieser Rassen:
# American Paint Horse
# American Quarter Horse
# Appaloosa
# Pony of the Americas

# Zwischen 1 - 30 % Shetty reicht als Filter - der Rest sind automatisch die anderen
# Rassen.

# Stockmaß: Zwischen 100 und 140

# Farbe: LP oder LPLP
# Außerdem: Entweder Tobn oder TobTob oder Oo oder Sbn oder SWSW

pony %>% 
  filter(
    shetland <= 30 & shetland >= 1 &
      height >= 100 & height <= 140 &
      leopard %in% c("LPlp", "LPLP") &
      (
        kit %in% c("Tobn", "TobTob", "Sbn") |
          splashed_white == "SWSW" |
          overo == "Oo"
      )
  ) %>% 
  nrow()

pony %>% 
  filter(
    shetland <= 30 & shetland >= 1 &
      height >= 100 & height <= 140 &
      leopard %in% c("LPlp", "LPLP") &
      (
        kit %in% c("Tobn", "TobTob", "Sbn") |
          splashed_white == "SWSW" |
          overo == "Oo"
      )
  )

# Gruppenebene: Sind alle erlaubten Farben mindestens einmal dabei?
# (Macht erst Sinn, sobald es Pferde gibt, die die Rasseanforderungen erfüllen.)
# Beachten: Seal bay, wild bay und cream homo/heterozygot muss dabei sein!
