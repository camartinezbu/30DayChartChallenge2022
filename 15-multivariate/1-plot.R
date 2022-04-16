library(tidyverse)
library(GGally)

# Load Pokemon Data
# Source: https://www.kaggle.com/datasets/mariotormo/complete-pokemon-dataset-updated-090420
pokemon <- read_csv("15-multivariate/data/pokedex_(Update_04.21).csv") %>%
  select(hp, attack, status, weight_kg, generation)
  

# Plot hp, attack, weight, type_1
ggplot(pokemon, aes(x = hp, y = attack, color = status, size = weight_kg)) +
  geom_point(alpha = 0.75) +
  facet_wrap(~generation, ncol = 4) +
  scale_color_manual(
    values = c(
      'Mythical' = "#ffcb05",
      'Legendary' = "#e72d2e",
      'Sub Legendary' = "#ff6100",
      'Normal' = "#2a75bb"),
    labels = c(
      "Mítico",
      "Legendario",
      "Sub-legendario",
      "Normal"
    )
  ) +
  scale_size_continuous(
    breaks = c(125, 250, 500, 750)
  ) +
  guides(col = guide_legend(order = 1),
         size = guide_legend(order = 2)) +
  expand_limits(y = 200) +
  labs(
    title = "Atributos de Pokémon a lo largo de las generaciones",
    subtitle = "(Generaciones 1 a 8)",
    caption = "Fuente: Complete Pokémon Dataset en Kaggle | Elaborado por Camilo Martínez (@camartinezbu)",
    y = "Ataque",
    x = "Puntos de salud",
    color = "Estatus",
    size = "Peso (kg)"
  ) +
  theme(
    text = element_text(family = "Kanit"),
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.title.position = "plot",
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption.position = "plot",
    panel.background = element_blank(),
    panel.grid.minor = element_blank(),
    legend.key = element_blank(),
    panel.border = element_rect(color = "lightgrey", fill = NA)
  )

ggsave("15-multivariate/plot.png", width = 7, height = 5)