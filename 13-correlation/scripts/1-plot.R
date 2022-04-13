library(tidyverse)
library(datasauRus)
library(patchwork)

datasaurus <- datasauRus::datasaurus_dozen %>%
  filter(dataset == "dino")

not_datasaurus <- datasauRus::datasaurus_dozen %>%
  filter(dataset != "dino")

plot1 <- ggplot(datasaurus, aes(x = x, y = y)) +
  geom_point(colour = "white", size = 0.8) +
  coord_equal() +
  labs(
    title = "Recuerda siempre\nexplorar\n tus datos...",
    caption = "...te puedes encontrar sorpresas"
  ) +
  theme(
    text = element_text(family = "Assistant", color = "white"),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 30),
    plot.caption = element_text(face = "bold", hjust = 0.5, size = 17),
    plot.background = element_rect(fill = "#29465b", colour = "#29465b"),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank()
  )

plot2 <- ggplot(not_datasaurus, aes(x = x, y = y)) +
  geom_point(colour = "white", size = 0.25) +
  coord_equal() +
  facet_wrap(~dataset, ncol = 6) +
  labs(
    title = "Todos estos conjuntos de datos comparten \nla media y la desviación estándar de las variables (x,y),\nasí como su correlación.",
    caption = "Fuente: Paquete datasauRus de R | Elaborado por Camilo Martínez (@camartinezbu)"
  ) +
  theme(
    text = element_text(family = "Assistant", color = "white"),
    plot.title = element_text(hjust = 0.5, size = 10),
    plot.caption = element_text(size = 7),
    plot.background = element_rect(fill = "#29465b", colour = "#29465b"),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    strip.background = element_blank(),
    strip.text = element_blank()
  )

plot_final <- plot1 / plot2 +
  plot_layout(heights = c(2, 1))

ggsave("13-correlation/plot.png", plot = plot_final,
       width = 4, height = 6)
