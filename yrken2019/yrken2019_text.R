library(tidyverse)
library(janitor)
library(ragg)
library(ggfx)
library(gridfont)

yrken <- read_tsv(here::here("yrken2019", "data", "yrken.tsv")) %>% 
  clean_names() %>% 
  mutate(
    andel_kvinnor = andel_kvinnor / 100,
    yrke = fct_reorder(yrke, andel_kvinnor),
    n = row_number()
  ) 

yrken_grid <- yrken %>% 
  rowwise() %>% 
  mutate(yrke = str_remove(yrke, "\n")) %>% 
  summarise(create_text_df(yrke, font = "smooth"), n = cur_group_id()) %>% 
  ungroup() %>% 
  group_by(n) %>% 
  mutate(
    x_max = max(x, na.rm = TRUE),
    y_max = max(y, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  mutate(
    x = x/x_max * 250,
    y = y/y_max * 30
  ) %>% 
  left_join(yrken)

bg_col = "grey93"
pal <- c("#448D88", "#7900f1", "#DD644E") # men, women, orange

ggplot(yrken_grid) +
  geom_vline(xintercept = 125, color = pal[3], size = 0.6) +
  as_reference(
    geom_rect(aes(xmin = -1.5, ymin = n * 28 + 2, xmax = andel_kvinnor * 250, ymax = n * 28 + 30), color = NA),
    id = "women_r"
  ) +
  as_reference(
    geom_rect(aes(xmin = 251.5, ymin = n * 28 + 2, xmax = 250 - (1 - andel_kvinnor) * 250, ymax = n * 28 + 30), color = NA),
    id = "men_r"
  ) +
  with_blend(
    geom_path(aes(x, y + n * 28, group = interaction(char_idx, stroke, n), size = 1/x_max), na.rm = TRUE, color = pal[2]),
    bg_layer = "women_r",
    blend_type = "in"
  ) +
  with_blend(
    geom_path(aes(x, y + n * 28, group = interaction(char_idx, stroke, n), size = 1/x_max), na.rm = TRUE, color = pal[1]),
    bg_layer = "men_r",
    blend_type = "in"
  ) +
  geom_rect(aes(xmin = -1.5, ymin = n * 28 + 5, xmax = andel_kvinnor * 250, ymax = n * 28 + 7), color = NA, fill = pal[2]) +
  scale_size_continuous(range = c(0.75, 1.25)) +
  scale_color_identity() +
  coord_fixed() +
  theme_void() +
  theme(
    legend.position = "none"
  )
