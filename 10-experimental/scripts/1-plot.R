library(tidyverse)
library(ggpage)

# File created manually
chapter <- readxl::read_xlsx("10-experimental/data/raw/first_chapter.xlsx") %>%
  mutate(text = str_trim(text)) 

chapter %>%
  ggpage_build() %>%
  mutate(palabras = case_when(word == "macondo" ~ "Macondo",
                              word == "melquiades" ~ "Melquíades",
                              word == "melquíades" ~ "Melquíades",
                              word == "úrsula" ~ "Úrsula",
                              word == "josé" ~ "José Arcadio (padre)",
                              word == "arcadio" ~ "José Arcadio (padre)",
                              word == "aureliano" ~ "Aureliano",
                             TRUE ~ "Texto")) %>%
  mutate(palabras = case_when(
    word == "josé" & (page == 14 & line == 16) ~ "José Arcadio (hijo)",
    word == "arcadio" & (page == 14 & line == 16) ~ "José Arcadio (hijo)",
    word == "josé" & (page == 18 & line == 1) ~ "José Arcadio (hijo)",
    word == "arcadio" & (page == 18 & line == 1) ~ "José Arcadio (hijo)",
    TRUE ~ palabras)) %>%
  ggpage_plot(aes(fill = palabras)) +
  labs(
    title = "Menciones en el primer capítulo de\nCien Años de Soledad",
    caption = "Fuente: Cien Años de Soledad | Elaborado por Camilo Martínez (@camartinezbu)"
  ) +
  scale_fill_manual(
    values = c(
      `Macondo` = "#FFE500",
      `Melquíades` = "#00a2ff",
      `José Arcadio (padre)` = "#ff7700",
      `Úrsula` = "#FC2288",
      `José Arcadio (hijo)` = "#00f5ed",
      `Aureliano` = "#00E070",
      `Texto` = "darkgrey"
    )
  ) +
  theme(
    text = element_text(family = "Assistant", colour = "white"),
    plot.background = element_rect(colour = "#4c4c4c", fill = "#4c4c4c"),
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
    plot.caption = element_text(hjust = 0.5),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.margin = margin(t = 10, l = 3, r = 3, b = 5),
    legend.position = "top",
    legend.justification = "center",
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.text = element_text(face = "bold", size = 8),
    legend.margin = margin(t = 15, l = 0, r = 0, b = 0)
  )
  
ggsave("10-experimental/plot.png", height = 8, width = 5)

