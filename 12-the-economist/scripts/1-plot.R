library(tidyverse)
library(ggtext)
library(patchwork)

# Fuente: DANE https://sitios.dane.gov.co/cnpv/#!/hogares_part_num_pers
data <- readxl::read_xlsx("12-the-economist/data/data.xlsx") %>%
  pivot_longer(c(Censo_2005, Censo_2018), names_to = "Año",
               values_to = "Valor") %>%
  mutate(Año = case_when(Año == "Censo_2005" ~ "2005",
                         Año == "Censo_2018" ~ "2018")) %>%
  mutate(No_personas = factor(No_personas,
                              levels = c(
                                "5 personas o más",
                                "4 personas",
                                "3 personas",
                                "2 personas",
                                "1 persona"
                              ),
                              ordered = TRUE))

rectangle_data <- data.frame(
  x=c(-1, 1, 1, -1),
  y=c(2, 2, 2.5, 2.5)
)

rectangle <- ggplot(rectangle_data) +
  geom_polygon(aes(x=x, y =y), fill = "#e3120b") +
  coord_equal() +
  theme_void()



full <- ggplot(data) +
  geom_point(aes(x = Valor, y = No_personas, color = Año), size = 3) +
  geom_path(aes(x = Valor, y = No_personas, group = No_personas),
            arrow = arrow(length = unit(0.125, "cm"), type = "closed")) +
  scale_color_manual(
    values = c(
      "2005" = "#336666",
      "2018" = "#e3120b"
    )
  ) +
  expand_limits(x = c(10,35)) +
  scale_x_continuous(
    breaks = c(10, 15, 20, 25, 30, 35)
  ) +
  labs(
    title = "Cada vez hay más hogares unipersonales \nen Colombia",
    subtitle = "Porcentaje de hogares particulares según el número de personas\n(Censos 2005-2018)",
    caption = "Fuente: DANE | Elaborado por Camilo Martínez (@camartinezbu)",
    x = "Porcentaje"
  ) +
  theme(
    text = element_text(family = "Fira Sans Condensed"),
    plot.title = element_text(face = "bold", size = 20),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.margin = margin(t = 10, l = 5, r = 5, b = 3),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 8),
    axis.title.x = element_text(family = "Fira Sans Condensed Medium", size = 10),
    panel.grid.major.x = element_line(color = "lightgrey"),
    panel.background = element_blank(),
    legend.position = c(.855, .9),
    legend.justification = "center",
    legend.direction = "vertical",
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.background = element_blank()
  )

full +
  inset_element(rectangle,
                left = -1,
                bottom = 0.2,
                right = 0.2,
                top = 3.2,
                align_to = "plot") +
  inset_element(rectangle,
                left = -1,
                bottom = 0.2,
                right = 2,
                top = 4.05,
                align_to = "plot")

ggsave("12-the-economist/plot.png", height = 4, width = 5)
