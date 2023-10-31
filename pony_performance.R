
library(googlesheets4)
library(tidyverse)

custom_theme <- 
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.position = "top"
  )

sheet_url <- "https://docs.google.com/spreadsheets/d/1uDBoNSobxQK0mpM5YOBRUNxGW6LoTFjf6zkTaLDKu5s/edit#gid=212293532" 

# Log in with google Account (can be pre-authorized via googlesheets4)
pony <- read_sheet(sheet_url)

# To create space around the names for the plot
pony <- 
  pony %>% 
  mutate(name_ws = paste0("  ", name, "  "))

## HIGHLIGHT HORSE -------------------------------------------------------------

# Plot all horses (height/skill) and highlight a single horse to show where the 
# horse stands relative to the other horses.

highlight_horse <- "Still Addicted"
discipline <- sym("superhorse") # or "trail" or "reining"

pony %>% 
  mutate(highlight = ifelse(name %in% highlight_horse, TRUE, FALSE)) %>% 
  # Exclude some Reining stallions that distort the plot
  filter(!name %in% c("Rocher", "Charmed", "Shalimar", "Social Network", "Joey")) %>% 
  ggplot(aes(x = height, y = !!discipline, colour = highlight)) +
  # height limits for registering Pintaloosa Ponys
  geom_vline(xintercept = 100) +
  geom_vline(xintercept = 140) +
  # mean values for height + potential
  geom_hline(aes(yintercept = mean(!!discipline, na.rm = TRUE)), colour = "grey") +
  geom_vline(aes(xintercept = mean(height)), colour = "grey") +
  geom_point(alpha = .5, size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_text(aes(label = name_ws), vjust = .3, hjust = "right") +
  scale_colour_manual(values = c("black", "red")) +
  scale_x_continuous(breaks = seq(100, 140, 5)) +
  scale_y_continuous(breaks = seq(50, 400, 25)) +
  custom_theme +
  theme(legend.position = "none")

## FIND STALLION ---------------------------------------------------------------

# Specify Shetland blood percentage of the mare and display which stallions 
# would result in a foal that fulfills the criteria for being registered as 
# Pintaloosa Pony

dam_shetland <- 26.28

discipline <- sym("superhorse") # or "trail" or "reining"

pony %>% 
  filter(
    sex == "stallion", 
    breeding_year <= 2054,
    ## COLOUR FILTERS - UNCOMMENT AS NEEDED ##
    # grepl("E", extension),
    # flaxen > 0,
    # grepl("SW", splashed_white),
    # grepl("Mu", mushroom),
    # grepl("Cr", cream_pearl),
    # !grepl("O", overo),
    # grepl("Prl", cream_pearl),
    # grepl("LP", leopard),
    # (
    #   grepl("Tob|Sb", kit) |
    #     splashed_white == "SWSW" |
    #     overo == "Oo"
    # )
  ) %>%
  # Exclude some Reining stallions that distort the plot
  filter(!name %in% c("Rocher", "Charmed", "Shalimar", "Social Network", "Joey")) %>% 
  mutate(
    registration = 
      ifelse(
        shetland <= 60 - dam_shetland, 
        "possible", 
        "not possible"
        )
    ) %>% 
  
  ggplot(aes(x = height, y = !!discipline)) +
  geom_hline(aes(yintercept = mean(!!discipline, na.rm = TRUE)), colour = "grey") +
  geom_vline(aes(xintercept = mean(height, na.rm = TRUE)), colour = "grey") +
  geom_vline(xintercept = 100) +
  geom_vline(xintercept = 140) +
  geom_point(aes(colour = registration), size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_text(aes(label = name_ws), vjust = .3, hjust = "right") +
  scale_x_continuous(breaks = seq(100, 140, 5)) +
  scale_y_continuous(breaks = seq(50, 400, 25)) +
  custom_theme

## FIND SHOW HORSES ------------------------------------------------------------

# Filter ponies that are of the right age and height to go on shows with 
# children. Show those with the highest potential first.

ingame_year <- 2057
discipline <- sym("reining") # or "trail" or "reining"

# Ponies for young children
pony %>% 
  filter(
    breeding_year <= (2057 - 3),
    height <= 110
  ) %>% 
  arrange(desc(!!discipline)) %>% 
  select(name, id, breeding_year, sex, !!discipline)

# Children 9 years or older
# (technically, children can't ride horses > 150 cm, but no Pintaloosa Pony 
# should be above that threshold, so we don't filter for that)
pony %>% 
  filter(
    breeding_year <= (2057 - 3),
    height <= 130
  ) %>% 
  arrange(desc(!!discipline)) %>% 
  select(name, id, breeding_year, sex, !!discipline)
