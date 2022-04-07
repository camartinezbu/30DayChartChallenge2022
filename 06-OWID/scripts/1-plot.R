library(tidyverse)
library(ggrepel)
library(lubridate)

relevant_labels = c(
  "Theseus",
  "ADALINE",
  "Perceptron Mark I",
  "Samuel Neural Checkers",
  "Neocognition",
  "NetTalk",
  "System 11",
  "RNN for Speech",
  "TD-Gammon",
  "BiLSTM for Speech",
  "NPLM",
  "AlexNet",
  "AlphaGo Master",
  "AlphaGo Lee",
  "MSRA (C, PReLU)",
  "MoE",
  "SEER",
  "DALL-E",
  "PaLM (540B)",
  "GPT",
  "GPT-3 175B",
  "BERT-Large"
)

data <- read_csv("06-OWID/data/ai-training-computation.csv") %>%
  drop_na(`Computation used in training AI systems (petaFLOPs)`) %>%
  mutate(label = case_when(Entity %in% relevant_labels ~ Entity,
                           TRUE ~ "")) %>%
  mutate(Domain_es = case_when(Domain == "Drawing" ~ "Ilustración",
                               Domain == "Driving" ~ "Conducción",
                               Domain == "Games" ~ "Juegos",
                               Domain == "Language" ~"Lenguaje",
                               Domain == "Multimodal" ~ "Multimodal",
                               Domain == "Other" ~ "Otro",
                               Domain == "Recommendation" ~"Recomendación",
                               Domain == "Robotics" ~ "Robótica",
                               Domain == "Speech" ~ "Habla",
                               Domain == "Vision" ~ "Visión"))

ggplot(data, aes(x = Day, 
                 y = `Computation used in training AI systems (petaFLOPs)`,
                 label = label))+
  geom_point(aes(color = Domain_es), alpha = 0.8) +
  geom_text_repel(aes(color = Domain_es), 
                  size = 2, 
                  force = 0.5,
                  force_pull = 2,
                  fontface = "bold",
                  max.overlaps = Inf,
                  bg.color = "white",
                  bg.r = 0.15) +
  scale_y_log10(
    breaks = c(
      0.000000000001,
      0.0000000001,
      0.00000001,
      0.000001,
      0.0001,
      0.01,
      1,
      100,
      10000,
      1000000,
      100000000),
    labels = c(
      "<1e-11",
      "0.0000000001",
      "0.00000001",
      "0.000001",
      "0.0001",
      "0.01",
      "1",
      "100",
      "10.000",
      "1 millón",
      "100 millones"
    )
  ) +
  scale_x_date(
    breaks = c(
      as_date("1950-01-01"),
      as_date("1960-01-01"),
      as_date("1970-01-01"),
      as_date("1980-01-01"),
      as_date("1990-01-01"),
      as_date("2000-01-01"),
      as_date("2010-01-01"),
      as_date("2020-01-01")
    ),
    labels = c(
      "1950",
      "1960",
      "1970",
      "1980",
      "1990",
      "2000",
      "2010",
      "2020"
    )
  ) +
  labs(
    title = "Computación estimada para el entrenamiento de sistemas de IA",
    subtitle = "Esta es una selección de sistemas de inteligencia artificial destacados que usaron una gran capacidad de computación en su entrenamiento.\nLa capacidad de computación se mide en petaFLOPs, que equivalen a 10^15 operaciones de punto flotante.",
    caption = "Fuente: Sevilla et al. (2022) | Elaborado por Camilo Martínez (@camartinezbu)\n Nota: Las estimaciones contienen incertidumbre, pero se espera que sean correctas en un factor de ~2.",
    y = "petaFLOPs",
    x = "Fecha de publicación"
  ) +
  theme(
    text = element_text(family = "Assistant"),
    strip.background = element_rect(colour = "white", fill = "white"),
    plot.title = element_text(face = "bold", size = 18.5),
    plot.title.position = "plot",
    plot.subtitle = element_text(size = 9.5),
    panel.background = element_blank(),
    panel.grid.major.y = element_line(color = "lightgrey",
                                      linetype = "dashed",
                                      size = 0.5),
    panel.grid.major.x = element_line(color = "lightgrey",
                                      linetype = "dashed",
                                      size = 0.5),
    axis.ticks = element_blank(),
    legend.justification = "left",
    legend.direction = "vertical",
    legend.title = element_blank(),
    legend.key = element_blank(),
    plot.caption.position = "plot"
  ) 


ggsave("06-OWID/plot.png", width = 8, height = 6)
