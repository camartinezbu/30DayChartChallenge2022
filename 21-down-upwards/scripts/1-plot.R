library(tidyverse)
library(ggtext)

# Note: Set default graphics device to AGG

# Load Country Data
data <- readxl::read_xlsx("21-down-upwards/data/IMD_World_Talent_Ranking.xlsx") %>%
  pivot_longer(-c(Pais, Codigo), names_to = "Año", values_to = "Ranking")

# Load Emojis
emoji_data <- data.frame(
  country = c("Suiza",
              "Dinamarca",
              "Suecia",
              "Austria",
              "Luxemburgo",
              "Noruega",
              "Islandia",
              "Finlandia",
              "Holanda",
              "Singapur",
              "Alemania",
              "Canadá",
              "Bélgica"),
  emoji_link = c("21-down-upwards/data/img/flag-switzerland_1f1e8-1f1ed.png",
                 "21-down-upwards/data/img/flag-denmark_1f1e9-1f1f0.png",
                 "21-down-upwards/data/img/flag-sweden_1f1f8-1f1ea.png",
                 "21-down-upwards/data/img/flag-austria_1f1e6-1f1f9.png",
                 "21-down-upwards/data/img/flag-luxembourg_1f1f1-1f1fa.png",
                 "21-down-upwards/data/img/flag-norway_1f1f3-1f1f4.png",
                 "21-down-upwards/data/img/flag-iceland_1f1ee-1f1f8.png",
                 "21-down-upwards/data/img/flag-finland_1f1eb-1f1ee.png",
                 "21-down-upwards/data/img/flag-netherlands_1f1f3-1f1f1.png",
                 "21-down-upwards/data/img/flag-singapore_1f1f8-1f1ec.png",
                 "21-down-upwards/data/img/flag-germany_1f1e9-1f1ea.png",
                 "21-down-upwards/data/img/flag-canada_1f1e8-1f1e6.png",
                 "21-down-upwards/data/img/flag-belgium_1f1e7-1f1ea.png")
)

# Join Data
data_final <- data %>%
  left_join(emoji_data, by = c("Pais" = "country"))

# Create function to display emojis
func_link_to_img <- function(x, size = 15) {
  paste0("<img src='", x, "' width='", size, "'/>")
}


ggplot(data_final, aes(x = Año, y = Ranking, group = Pais, color = Pais, fill = Pais)) +
  geom_line(size = 1.25) +
  ggtext::geom_richtext(
    aes(label = func_link_to_img(emoji_link)),
    fill = NA, label.color = NA,  size = 0.1
  ) +
  geom_text(aes(label = Codigo, y = Ranking + 0.4), 
            size = 2, color = "black", fontface = "bold") +
  labs(
    title = "Suiza lidera el Índice Global de Talento de IMD",
    subtitle = glue::glue("Este índice compara 64 países en torno a 3 factores:\n
                          • <b>Inversión y desarrollo</b>: Los recursos asignados para la generación de una fuerza de trabajo local.\n
                          • <b>Atractivo</b>: La medida en que una economía atrae talento extranjero y retiene el local.\n
                          • <b>Disponiblidad</b>: La calidad de las habilidades y competencias que están disponibles en un país."),
    caption = "Fuente: IMD World Talent Index | Elaborado por Camilo Martínez (@camartinezbu)"
  ) +
  scale_y_continuous(trans = "reverse",
                     breaks = c(1:10),
                     limits = c(10.4, 1)
                     ) +
  scale_x_discrete(
    expand = expansion(mult = 0.05, add = 0)
  ) +
  theme(
    text = element_text(family = "Fira Sans Condensed"),
    plot.title = element_text(face = "bold", size = 21),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(size = 10, lineheight = 0.5),
    panel.background = element_blank(),
    panel.grid.major = element_line(size = 0.25, color = "#888888", linetype = "dotted"),
    axis.ticks = element_blank(),
    legend.position = "none",
    plot.margin = margin(t = 15, l = 10, r = 10, b = 3)
  )

ggsave("21-down-upwards/plot.png", height = 6, width = 6)

