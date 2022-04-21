library(tidyverse)
library(ggtext)

# Load OURdata Index results
data <- read_delim("18-OECD/data/OURdata_Index.csv", delim = ";")

data_final <- data %>%
  mutate(Pais = factor(Pais, levels = rev(unique(data$Pais)), ordered = TRUE)) %>%
  pivot_longer(-c(Pais, Label), names_to = "Variable", values_to = "Puntaje")

# Create label vector
variable_labels = c(glue::glue("<b><span style='color:\"#ee5321\"'>Pilar 1<br>Disponibilidad<br>de los datos</span></b>"),
                    glue::glue("<b><span style='color:\"#224d64\"'>Pilar 2<br>Accesibilidad<br>de los datos</span></b>"),
                    glue::glue("<b><span style='color:\"#75b8c9\"'>Pilar 3<br>Apoyo del<br>gobierno para<br>su reutilización</span></b>"),
                    glue::glue("<b>Puntaje<br>Total</b>"))

names(variable_labels) <- c("Pilar1", "Pilar2", "Pilar3", "Total")


ggplot(data_final, aes(x = Pais, y = Puntaje, fill = Variable)) +
  geom_col() +
  geom_text(aes(label = Puntaje, hjust = -0.2), size = 3.25, fontface = "bold") +
  coord_flip() +
  facet_wrap(~Variable, ncol = 4, scales = "free_x",
             labeller = labeller(Variable = variable_labels)
  ) +
  labs(
    title = "Colombia está en el top 3 de países en el\nÍndice OURdata de la OCDE",
    subtitle = glue::glue("Este índice estudia el diseño e implementación de 
                          políticas de datos abiertos en el nivel<br>central de 
                          gobierno a través de 3 pilares. A continuación se 
                          presentan los resultados de<br>la medición de 2019."),
    caption = "Fuente: OCDE | Elaborado por Camilo Martínez (@camartinezbu)"
  ) +
  expand_limits(y = 40) +
  scale_fill_manual(
    values = c(
      "Pilar1" = "#ee5321",
      "Pilar2" = "#224d64",
      "Pilar3" = "#98d3e2",
      "Total" = "#ffd500"
    )
  ) +
  theme(
    text = element_text(family = "Fira Sans Condensed"),
    plot.title = element_text(face = "bold", size = 24, hjust = 0.5),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(lineheight = 1.1),
    plot.margin = margin(t = 10, l = 10, r= 10, b = 5),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_text(),
    axis.ticks.y = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_markdown()
  )

ggsave("18-OECD/plot.png", height = 7, width = 6)