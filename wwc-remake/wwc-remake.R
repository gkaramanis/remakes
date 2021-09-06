library(tidyverse)
library(ggimage)
library(here)
library(grid)

wwc_outcomes <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/wwc_outcomes.csv")

# Assign line y and yend depending on win_status
winloss <- wwc_outcomes %>%
  group_by(team) %>%
  mutate(
    game_n = row_number(),
    y = case_when(
      win_status == "Lost" ~ -1.33,
      win_status == "Won" ~ 1.33,
      TRUE ~ 0.33),
    yend = case_when(
      win_status == "Lost" ~ 0.33,
      TRUE ~ -0.33)
    )

# Customize legend draw key
GeomSegment$draw_key <- function(data, params, size) {
  segmentsGrob(x0 = 0.5, y0 = 0, x1 = 0.5, y1 = 1, # make line vertical
               gp = gpar(col = alpha(data$colour, data$alpha),
                         lwd = data$size * .pt, lty = data$linetype,
                         lineend = "round")) # round lineend
  }  

ggplot(winloss) +
  # Win/draw/loss lines
  geom_segment(aes(x = game_n, xend = game_n, y = y, yend = yend, color = win_status), lineend = "round", size = 0.6) +
  # Country codes and flags
  geom_text(aes(label = team, x = -1, y = 0),
    hjust = 1, size = 3, check_overlap = TRUE,
    family = "IBM Plex Mono", fontface = "bold") +
  geom_image(x = -10, y = 0, asp = 24, size = 0.04,
             aes(image = here("wwc-remake", "flags", paste0(team, ".png")))) +
  # Scales
  scale_color_manual(values = c("darkred", "grey60", "darkblue"),
    labels = c("Loss", "Draw", "Win"), guide = guide_legend(override.aes = list(size = 1))) +
  coord_fixed(xlim = c(-10, 50), clip = "off") +
  scale_x_continuous(breaks = c(10, 30, 50)) +
  # Title, subtitle and caption
  labs(
    title = "Wins, draws and losses for the teams that have taken part\nin the Women's World Cup from 1991 to 2019",
    subtitle = "USA have the most total wins (42), followed by Germany (31) and Norway (23). USA have\nthe two longest winning streaks (12 and 11) and Norway the third longest (10). Germany\nhold the longest undefeated streak (15), USA the second and third one (14 and 11).",
    caption = "Source: data.world · Graphic: Georgios Karamanis",
    x = "Number of matches played in WWC"
  ) +
  facet_wrap(~ team, ncol = 2) +
  # Theme
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "grey97", color = "white"),
    legend.position = c(0.45, 1.08),
    legend.direction = "horizontal",
    legend.key.size = unit(0.35, "line"),
    legend.text = element_text(color = "grey10", size = 8,
                               family = "IBM Plex Mono"),
    legend.title = element_blank(),
    plot.margin = margin(20, 40, 10, 40),
    panel.grid = element_blank(),
    axis.text.x = element_text(color = "grey30", size = 7,
                               family = "IBM Plex Mono"),
    axis.title.x = element_text(color = "grey10", size = 7,
                               family = "IBM Plex Mono", margin = margin(10, 0, 0, 0)),
    axis.ticks.x = element_line(color = "grey60", size = 0.25),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(size = 11, family = "IBM Plex Serif", face = "bold"),
    plot.subtitle = element_text(size = 8, family = "IBM Plex Sans",
                                 margin = margin(0, 0, 40, 0)),
    plot.caption = element_text(size = 5.5, color = "grey40",
                                family = "IBM Plex Mono", hjust = 0.5,
                                margin = margin(25, 0, 0, 0)),
    strip.text = element_blank()
  )

ggsave(here("wwc-remake", "wwc.png"),
         width = 6, height = 6, dpi = 300)
