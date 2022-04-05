library(tidyverse)
library(ggtext)
library(ggrepel)
library(readxl)

# Fuente: https://www.dane.gov.co/index.php/estadisticas-por-tema/pobreza-y-condiciones-de-vida/encuesta-multiproposito
data <- read_xlsx("05-slope/data/data.xlsx")

final_data <- data %>%
  mutate(Label = round((`2017`-`2014`)/`2014` * 100,1)) %>%
  pivot_longer(cols = c(`2017`, `2014`), values_to = "Pct_Internet", names_to = "Año") %>%
  mutate(Año = as.numeric(Año)) %>%
  mutate(Color = case_when(Localidad == "Total Bogotá" ~ "Bogotá",
                           Localidad == "Rafael Uribe Uribe" ~ "Rafael Uribe Uribe",
                           Localidad == "Santafé" ~"Santafé",
                           TRUE ~ "Resto")) %>%
  mutate(Label = case_when(Año == 2017 ~ glue::glue("{Localidad}: ▲{Label} p.p."),
                           Año == 2014 ~ ""))


ggplot(data = final_data,
       aes(x = Año,
           y = Pct_Internet,
           group = Localidad)) +
  geom_line(aes(color = Color), size = 0.5) +
  geom_point(aes(color = Color), size = 1) +
  geom_text_repel(aes(label = Label, color = Color),
                  size = 1.75, hjust = 0,
                  force = 0.5, nudge_x = 0.33,
                  direction = "y", segment.size = 0.15,
                  segment.alpha = 0.5, fontface = "bold") +
  scale_x_continuous(position = "bottom", 
                     breaks = c(2014, 2017)) +
  coord_cartesian(xlim = c(2014, 2018.5)) +
  scale_color_manual(values = c(`Bogotá` = "#000137",
                                `Rafael Uribe Uribe` = "#eca400",
                                `Santafé` = "#248232",
                                `Resto` = "grey")) +
  scale_fill_manual(values = c(`Bogotá` = "#000137",
                               `Rafael Uribe Uribe` = "#eca400",
                               `Santafé` = "#248232",
                               `Resto` = "grey")) +
  labs(
    title = "La cobertura de internet en Bogotá \naumentó entre 2014 y 2017",
    subtitle = glue::glue("Entre las localidades urbanas, la cobertura incrementó más en
                          <b><span style='color:\"#248232\"'>Santafé</span></b>
                          y <br> menos en
                          <b><span style='color:\"#eca400\"'>Rafael Uribe Uribe</span></b>."),
    caption = "Fuente: Encuesta Multipropósito | Elaborado por Camilo Martínez (@camartinezbu)",
    y = "% Hogares con conexión a internet"
  ) +
  theme (
    text = element_text(family = "Assistant"),
    plot.title = element_text(face = "bold", size = 18),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(size = 9, lineheight = 1.05),
    plot.caption = element_text(size = 6.5),
    plot.margin = margin(t = 8, l = 5, r = 5, b = 5),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.background = element_rect(fill = "white"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 8),
    axis.ticks = element_blank(),
    axis.text.x = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold"),
    legend.position = "none"
  )

ggsave("05-slope/plot.png", height = 4, width = 4)
