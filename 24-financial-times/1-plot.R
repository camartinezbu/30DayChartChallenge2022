library(tidyverse)
library(readxl)
library(patchwork)

# Fuente: Estadisticas Sector Externo, Banco de la República
data <- readxl::read_xlsx("24-financial-times/data/IED_en_Colombia.xlsx") %>%
  pivot_longer(-c(Año), names_to = "Sector", values_to = "Valor") %>%
  filter(Sector != "Total actividades económicas")

# Pintar rectángulo
rectangle_data <- data.frame(
  x=c(-1, 2, 2, -1),
  y=c(2, 2, 2.25, 2.25)
)

rectangle <- ggplot(rectangle_data) +
  geom_polygon(aes(x=x, y =y), fill = "black") +
  coord_equal() +
  theme_void()

# Pintar gráfico completo

plot <- ggplot(data, aes(x = Año, y = Valor, 
                         group = Sector, fill = Sector, color = Sector)) +
  geom_area() +
  labs(
    title = "Flujos de Inversión Extranjera Directa en Colombia",
    subtitle = "Cifras en millónes de dólares estadounidenses",
    caption = "Fuente: Banco de la República | Elaborado por Camilo Martínez (@camartinezbu)",
    y = "USD (MM)"
  ) +
  scale_x_continuous(
    breaks = c(1996:2021),
    expand = expansion(mult = 0.03, add = 0)
  ) +
  scale_y_continuous(
    expand = expansion(mult = 0.03, add = 0)
  ) +
  scale_color_manual(
    values = c(
      `Sector petrolero` = "#014fa1",
      `Resto de Sectores` = "#ff4b8f"
    )
  ) +
  scale_fill_manual(
    values = c(
      `Sector petrolero` = "#98c8fa",
      `Resto de Sectores` = "#ffa1c5"
    )
  ) +
  theme(
    text = element_text(family = "Fira Sans Condensed", color = "#66615e"),
    plot.title = element_text(family = "Fira Sans Condensed ExtraBold", size = 18, color = "black"),
    plot.title.position = "plot",
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(),
    plot.caption.position = "plot",
    plot.background = element_rect(fill = "#fff1e4"),
    plot.margin = margin(t = 18, b = 8, l = 12, r = 12),
    panel.background = element_blank(),
    panel.border = element_blank(),
    legend.position = c(.15, .825),
    legend.direction = "vertical",
    legend.background = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.key.height = unit(0.5, "cm"),
    legend.key.width = unit(0.5, "cm"),
    panel.grid.major.y = element_line(color = "#e3ddcd"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 8, angle = 90),
    axis.title.y = element_text(size = 9),
    axis.text.y = element_text(size = 8),
    axis.ticks = element_blank()
  )

plot +
  inset_element(rectangle,
                top = 1.18,
                bottom = 1.165,
                left = -0.21,
                right = 0.3,
                align_to = "plot")

ggsave("24-financial-times/plot.png", height = 5, width = 7)