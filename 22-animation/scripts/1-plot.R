library(tidyverse)
library(readxl)
library(gganimate)

# Load Population Data
# Source: DANE
pop <- read_xlsx("22-animation/data/pop_col.xlsx")

# Transform data
pop_new <- pop %>%
  pivot_longer(-c(Año, Total), names_to = "Sexo_Edad", values_to = "Poblacion") %>%
  separate(Sexo_Edad, sep = "_", into = c("Sexo", "Edad")) %>%
  mutate(Edad = as.numeric(Edad)) %>%
  mutate(Rango_Edad = case_when(Edad < 5 ~ "0 a 4",
                                Edad >= 5 & Edad < 10 ~ "5 a 9",
                                Edad >= 10 & Edad < 15 ~ "10 a 14",
                                Edad >= 15 & Edad < 20 ~ "14 a 19",
                                Edad >= 20 & Edad < 25 ~ "20 a 24",
                                Edad >= 25 & Edad < 30 ~ "25 a 29",
                                Edad >= 30 & Edad < 35 ~ "30 a 34",
                                Edad >= 35 & Edad < 40 ~ "35 a 39",
                                Edad >= 40 & Edad < 45 ~ "40 a 44",
                                Edad >= 45 & Edad < 50 ~ "45 a 49",
                                Edad >= 50 & Edad < 55 ~ "50 a 54",
                                Edad >= 55 & Edad < 60 ~ "55 a 59",
                                Edad >= 60 & Edad < 65 ~ "60 a 64",
                                Edad >= 65 & Edad < 70 ~ "65 a 69",
                                Edad >= 70 & Edad < 75 ~ "70 a 74",
                                Edad >= 75 & Edad < 80 ~ "75 a 79",
                                Edad >= 80 & Edad < 85 ~ "80 a 84",
                                Edad >= 85 ~ "85 o más"
                                )) %>%
  mutate(Rango_Edad = factor(Rango_Edad, ordered = TRUE,
                             levels = c(
                               "0 a 4", "5 a 9", "10 a 14", "14 a 19",
                               "20 a 24", "25 a 29", "30 a 34", "35 a 39",
                               "40 a 44", "45 a 49", "50 a 54", "55 a 59",
                               "60 a 64", "65 a 69", "70 a 74", "75 a 79",
                               "80 a 84", "85 o más"
                             ))) %>%
  group_by(Año, Total, Sexo, Rango_Edad) %>%
  summarise(Poblacion_Rango = sum(Poblacion)) %>%
  mutate(Porcentaje_Rango = Poblacion_Rango / Total)


plot <- ggplot(pop_new, aes(x = Rango_Edad, fill = Sexo, 
                            y = ifelse(
                              test = Sexo == "Hombres",
                              yes = -Porcentaje_Rango, no = Porcentaje_Rango
                            ))) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 2),
                     breaks = c(-0.1, -0.08, -0.06, -0.04, -0.02, 0,
                                0.02, 0.04, 0.06, 0.08, 0.1),
                     limits = c(-0.1, 0.1)) +
  geom_label(aes(x = 18, y = -.045, label = "Hombres"), fill = "#ff7300", color = "white", fontface = "bold") +
  geom_label(aes(x = 18, y = .045, label = "Mujeres"), fill = "#00b1be", color = "white", fontface = "bold")

anim <- plot + transition_time(Año, range = c(1950, 2050)) +
  labs(
    title = "100 años de crecimiento poblacional en Colombia",
    subtitle = "Año: {floor(frame_time)}",
    caption = "Fuente: DANE, Proyecciones y retroproyecciones Censo 2018 | Elaborado por Camilo Martínez (@camartinezbu)"
  ) +
  scale_fill_manual(
    values = c(
      "Hombres" = "#ff7300",
      "Mujeres" = "#00b1be"
    )
  ) +
  theme(
    text = element_text(family = "Fira Sans Condensed"),
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.title.position = "plot",
    plot.subtitle = element_text(hjust = 0.5),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

anim_save("22-animation/anim.gif", 
          nframes = 200,
          fps = 20,
          animation = anim,
          width = 6, height = 6, 
          units = "in", res = 150)
