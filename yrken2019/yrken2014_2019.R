library(tidyverse)
library(hablar)
library(ggbump)
library(colorspace)

yrken2014_2018 <- read_csv(here::here("yrken2019", "data", "yrken2014_2018.csv"))
yrken2019 <- read_csv(here::here("yrken2019", "data", "yrken2019.csv"))
yrken <- yrken2014_2018 %>% 
  left_join(yrken2019)

yrken_topp <- yrken %>% 
  pivot_longer(cols = c("2014":"2019"), names_to = "year", values_to = "n") %>% 
  rename("yrke" = `Yrke (SSYK 2012)`) %>% 
  mutate(yrke = str_remove(yrke, "\\d+ ")) %>% 
  filter(yrke != "yrke okänt") %>% 
  retype() %>% 
  group_by(year) %>%
  slice_max(order_by = n, n = 15) %>% 
  mutate(rank = row_number()) %>% 
  ungroup()

# http://medialab.github.io/iwanthue/
pal <- c("#a3b267",
         "#b300cd",
         "#b08a00",
         "#0050e2",
         "#ff892c",
         "#004291",
         "#ff5147",
         "#b7a1db",
         "#ae5900",
         "#641969",
         "#4e3f00",
         "#ff4fb6",
         "#7f000f",
         "#f587c2",
         "#c70061",
         "#a9626a")

ggplot(yrken_topp) +
  geom_bump(aes(x = year, y = rank, group = yrke, color = yrke), size = 1.5) +
  geom_point(aes(x = year, y = rank, color = yrke), size = 2) +
  geom_text(data = yrken_topp %>% filter(year == 2019), aes(x = 2019, y = rank, label = paste0(1:15, ". ", str_wrap(yrke, 25)), color = yrke), hjust = 0, nudge_x = 0.1, family = "October Condensed Tamil Medium", lineheight = 0.9, size = 3.8) +
  scale_y_reverse(breaks = 1:15) +
  scale_x_continuous(limits = c(2014, 2020), breaks = 2014:2019) +
  scale_color_manual(values = pal) +
  coord_cartesian(clip = "off") +
  labs(
    title = "De 15 vanligaste yrkena för kvinnor, 2014-2019",
    subtitle = "Baserat på antal anställda 16-64 år, yrken enligt SSYK 2012",
    caption = "Källa: SCB · Grafik: Georgios Karamanis"
    ) +
  theme_void(base_family = "DIN Condensed Bold") +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey97", color = NA),
    axis.text.x = element_text(color = "grey40", size = 14),
    plot.title = element_text(hjust = 0.5, size = 24),
    plot.subtitle = element_text(hjust = 0.5, size = 18),
    plot.caption = element_text(hjust = 0.5, size = 12, color = "grey30", margin = margin(20, 0, 0, 0)),
    plot.margin = margin(20, 50, 20, 10)
  ) +
  ggsave(here::here("yrken2019", "plots", "yrken2014_2019.png"), dpi = 320, height = 10, width = 8)
