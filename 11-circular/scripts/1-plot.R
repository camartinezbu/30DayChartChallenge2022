library(tidyverse)
library(ggraph)
library(igraph)

# Source: https://www.transmilenio.gov.co/publicaciones/150402/publicacionesmapa-interactivo-de-transmilenio/
vertices <- readxl::read_xlsx("11-circular/data/TM.xlsx", sheet = "vertices")
edges <- readxl::read_xlsx("11-circular/data/TM.xlsx", sheet = "edges")

# Create graph
graph_tm <- graph_from_data_frame(edges, vertices = vertices)

# Plot graph
ggraph(graph_tm, circular = TRUE) +
  geom_edge_diagonal(alpha = 0.2) +
  geom_node_point(aes(filter = leaf, 
                      color = zone,
                      x = x*1.05,
                      y = y*1.05)) +
  geom_node_label(aes(filter = branch,
                      label = name,
                      x = x*2.3,
                      y = y*2.3,
                      fill = zone),
                  color = "white",
                  fontface = "bold",
                  size = 3,
                  label.padding = unit(0.075, "cm")) +
  labs(
    title = "Distribución de las estaciones troncales de Transmilenio",
    caption = "Fuente: Transmilenio | Elaborado por Camilo Martínez (@camartinezbu)"
  ) +
  scale_color_manual(
    values = c(
      "A" = "#dd0820",
      "B" = "#63b332",
      "C" = "#feb533",
      "D" = "#76579d",
      "E" = "#a76120",
      "F" = "#dd0820",
      "G" = "#0083cf",
      "H" = "#f47c23",
      "J" = "#DF96A3",
      "K" = "#d2a973",
      "L" = "#009093",
      "M" = "#a40f6c",
      "T" = "#8a8623"
    )
  ) +
  scale_fill_manual(
    values = c(
      "A" = "#dd0820",
      "B" = "#63b332",
      "C" = "#feb533",
      "D" = "#76579d",
      "E" = "#a76120",
      "F" = "#dd0820",
      "G" = "#0083cf",
      "H" = "#f47c23",
      "J" = "#DF96A3",
      "K" = "#d2a973",
      "L" = "#009093",
      "M" = "#a40f6c",
      "T" = "#8a8623"
    )
  ) +
  coord_equal() +
  theme(
    text = element_text(family = "Assistant"),
    plot.title = element_text(face = "bold",
                              hjust = 0.5,
                              size = 16.5),
    plot.caption = element_text(hjust = 0.5),
    plot.margin = margin(t = 10, l = 5, r = 5, b = 3),
    legend.position = "none",
    panel.background = element_rect(fill = "white")
  )

ggsave("11-circular/plot.png", height = 6, width = 6)

