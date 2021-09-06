library(tidyverse)
library(hablar)
library(ggbump)
library(colorspace)

yrken2014_2018 <- read_csv(here::here("yrken2019", "data", "yrken2014_2018_4.csv"))
yrken2019 <- read_csv(here::here("yrken2019", "data", "yrken2019_4.csv"))
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
         "#7f000f",
         "#b7a1db",
         "#ae5900",
         "#641969",
         "#4e3f00",
         "#ff4fb6",
         "#f587c2",
         "#c70061",
         "#a9626a")

ggplot(yrken_topp) +
  geom_bump(aes(x = year, y = rank, group = yrke, color = yrke), size = 1.75) +
  geom_point(aes(x = year, y = rank, color = yrke), size = 2.5) +
  geom_text(data = yrken_topp %>% filter(year == 2019), aes(x = 2019, y = rank, label = paste0(1:15, ". ", str_wrap(yrke, 25)), color = yrke), hjust = 0, nudge_x = 0.1, family = "October Condensed Tamil Medium", lineheight = 0.9, size = 3.8) +
  geom_text(data = yrken_topp %>% filter(year == 2014), aes(x = 2014, y = rank, label = paste0(1:15, ". ", str_wrap(yrke, 25)), color = yrke), hjust = 1, nudge_x = -0.1, family = "October Condensed Tamil Medium", lineheight = 0.9, size = 3.8) +
  scale_y_reverse() +
  scale_x_continuous(limits = c(2013, 2020), breaks = 2014:2019) +
  scale_color_manual(values = pal) +
  coord_cartesian(clip = "off") +
  labs(
    title = "De 15 vanligaste yrkena för kvinnor, 2014-2019",
    subtitle = "Efter antal anställda 16-64 år, yrken enligt 4-siffrig SSYK 2012",
    caption = "Källa: SCB · Grafik: Georgios Karamanis"
    ) +
  theme_void(base_family = "DIN Condensed Bold") +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey97", color = NA),
    axis.text.x = element_text(color = "grey50", size = 18),
    plot.title = element_text(hjust = 0.5, size = 24, color = "grey10"),
    plot.subtitle = element_text(hjust = 0.5, size = 18, color = "grey20", margin = margin(5, 0, 20, 0)),
    plot.caption = element_text(hjust = 0.5, size = 12, color = "grey60", margin = margin(20, 0, 0, 0)),
    plot.margin = margin(20, 60, 20, 60)
  ) 

ggsave(here::here("yrken2019", "plots", "yrken2014_2019.png"), dpi = 320, height = 10, width = 9)
