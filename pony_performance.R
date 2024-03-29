
library(googlesheets4)
library(tidyverse)

custom_theme <- 
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.position = "top"
  )

sheet_url <- "sheet_url" # enter sheet url

# Log in with google account (can be pre-authorized via googlesheets4)
data <- read_sheet(sheet_url)

# To create space around the names for the plot
data <- 
  data %>% 
  mutate(name_ws = paste0("  ", name, "  "))

## HIGHLIGHT HORSE -------------------------------------------------------------

# Plot all horses (height/skill) and highlight a single horse to show where the 
# horse stands relative to the other horses.

highlight_horse <- "Maw Blood"
disc <- "superhorse" # or "trail" or "reining"

data %>% 
  mutate(highlight = ifelse(name %in% highlight_horse, TRUE, FALSE)) %>% 
  filter(discipline == disc | name == highlight_horse) %>% 
  ggplot(aes(x = height, y = !!sym(disc), colour = highlight, alpha = highlight)) +
  # height limits for registering Pintaloosa Ponys
  geom_vline(xintercept = 100) +
  geom_vline(xintercept = 140) +
  # mean values for height + potential
  geom_hline(aes(yintercept = mean(!!sym(disc), na.rm = TRUE)), colour = "grey") +
  geom_vline(aes(xintercept = mean(height)), colour = "grey") +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_text(aes(label = name_ws), vjust = .3, hjust = "right") +
  scale_colour_manual(values = c("black", "red")) +
  scale_alpha_manual(values = c(.3, 1)) +
  scale_x_continuous(breaks = seq(100, 140, 5)) +
  scale_y_continuous(breaks = seq(50, 400, 25)) +
  custom_theme +
  theme(legend.position = "none")

## FIND STALLION ---------------------------------------------------------------

# Specify Shetland blood percentage of the mare and display which stallions 
# would result in a foal that fulfills the criteria for being registered as 
# Pintaloosa Pony

ingame_year <- 2061
dam_shetland <- 27.35

discipline <- sym("superhorse") # or "trail" or "reining"

data %>% 
  filter(
    sex == "stallion", 
    breeding_year <= (ingame_year - 3),
    ## COLOUR FILTERS - UNCOMMENT AS NEEDED ##
    # grepl("E", extension),
    # grepl("aa|At", agouti),
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
  filter(!name %in% c("Charmed", "Social Network", "Joey")) %>%
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

ingame_year <- 2060
discipline <- sym("superhorse") # or "trail" or "reining"

# Ponies for young children
data %>% 
  filter(
    breeding_year <= (ingame_year - 3),
    height <= 110
  ) %>% 
  arrange(desc(!!discipline)) %>% 
  select(name, id, breeding_year, sex, !!discipline)

# Children 9 years or older
# (technically, children can't ride horses > 150 cm, but no Pintaloosa Pony 
# should be above that threshold, so we don't filter for that)
data %>% 
  filter(
    breeding_year <= (ingame_year - 3),
    height <= 130
  ) %>% 
  arrange(desc(!!discipline)) %>% 
  select(name, id, breeding_year, sex, !!discipline)
