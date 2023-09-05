
library(googlesheets4)
library(tidyverse)

julis_theme <- 
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "top")

sheet_url <- "https://docs.google.com/spreadsheets/d/1uDBoNSobxQK0mpM5YOBRUNxGW6LoTFjf6zkTaLDKu5s/edit#gid=212293532" 
pony <- read_sheet(sheet_url)

pony <- 
  pony %>% 
  mutate(
    name_ws = paste0("  ", name, "  "),
    superhorse = trail + reining
  )

highlight_horse <- "African Tribe"

pony %>% 
  mutate(highlight = ifelse(name %in% highlight_horse, TRUE, FALSE)) %>% 
  # filter(registered == 1, sex == "stallion",
  #        !grepl("Prl", cream_pearl), !grepl("Mu", mushroom)) %>%
  
  ggplot(aes(x = height, y = superhorse, colour = highlight)) +
  geom_vline(xintercept = 100) +
  geom_vline(xintercept = 140) +
  geom_point(alpha = .5, size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_text(aes(label = name_ws), vjust = .3, hjust = "right") +
  geom_hline(aes(yintercept = mean(superhorse, na.rm = TRUE)), colour = "grey") +
  geom_vline(aes(xintercept = mean(height)), colour = "grey") +
  scale_colour_manual(values = c("black", "red")) +
  scale_x_continuous(breaks = seq(100, 140, 5)) +
  scale_y_continuous(breaks = seq(200, 350, 25)) +
  julis_theme +
  theme(legend.position = "none")

# Does the horse fulfill the criteria?
pony %>% 
  filter(
    is.na(registered),
    skin == "Western",
    shetland <= 30 & shetland >= 1 &
      height >= 100 & height <= 140 &
      leopard %in% c("LPlp", "LPLP") &
      (
        grepl("Tob|Sb", kit) |
          splashed_white == "SWSW" |
          overo == "Oo"
      )
  ) %>% 
  select(name, id)

pony %>% 
  
  ggplot(aes(x = height, y = trail, colour = sex)) +
  geom_point(alpha = .5, size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_text(aes(label = name_ws), vjust = .3, hjust = "right") +
  geom_hline(aes(yintercept = mean(trail)), colour = "grey") +
  geom_vline(aes(xintercept = mean(height)), colour = "grey") +
  julis_theme

dam_shetland <- 12.11

pony %>% 
  filter(
    sex == "stallion", 
    breeding_year <= 2052,
    # grepl("E", extension),
    # pangare > 0,
    # flaxen > 0,
    # grepl("SW", splashed_white),
    # grepl("Mu", mushroom),
    # grepl("Cr", cream_pearl),
    grepl("Prl", cream_pearl),
    # grepl("LP", leopard),
    # (
    #   grepl("Tob|Sb", kit) |
    #     splashed_white == "SWSW" |
    #     overo == "Oo"
    # )
  ) %>%
  
  ggplot(aes(x = height, y = superhorse)) +
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
    breeding_year <= 2055,
    height <= 110
  ) %>% 
  arrange(desc(superhorse))

# Ab 9 Jahre
pony %>% 
  filter(
    breeding_year <= 2055,
    height <= 130
  ) %>% 
  arrange(desc(superhorse)) %>% 
  select(name, id, sex, superhorse)
