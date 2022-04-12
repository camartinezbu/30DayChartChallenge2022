library(tidyverse)
library(readxl)
library(ggbeeswarm)

# Fuente: https://www.dane.gov.co/index.php/estadisticas-por-tema/tecnologia-e-innovacion/tecnologias-de-la-informacion-y-las-comunicaciones-tic/indicadores-basicos-de-tic-en-hogares
data <- read_xlsx("09-statistics/data/data.xlsx") %>%
  filter(Departamento != "Total Nacional") %>%
  pivot_longer(c(-Departamento), names_to = "Variable", values_to = "Value") %>%
  mutate(Variable = factor(Variable,
                           levels = c(
                             "Copiar_mover",
                             "Copiar_entre_docs",
                             "Enviar_correos",
                             "Instalar_dispositivos",
                             "Formulas_matematicas",
                             "Crear_presentaciones",
                             "Transferir_archivos",
                             "Descargar_programas",
                             "Utilizar_lenguaje_programacion"
                           ),
                           ordered = TRUE))

ggplot(data) +
  geom_beeswarm(aes(x = Variable, y = Value), 
                fill = "#ffa364", colour = "#fc7643", 
                pch = 21) +
  coord_flip() +
  labs(
    title = "11,8% de las personas que usaron computadores \nen Colombia saben programar",
    subtitle = "Por su parte, más del 88% manifestaron poder copiar o mover un archivo o carpeta. ",
    caption = "Fuente: Encuesta Nacional de Calidad de Vida 2019 | Elaborado por Camilo Martínez (@camartinezbu)",
    y = "Porcentaje de personas",
    x = "Habilidades"
  ) +
  scale_x_discrete(
    limits = rev(levels(data$Variable)),
    labels = c(
      "Copiar_mover" = "Copiar o mover un archivo o carpeta",
      "Copiar_entre_docs" = "Usar las funciones de copiar y pegar para \nduplicar o mover información entre documentos",
      "Enviar_correos" = "Enviar correos electrónicos con archivos adjuntos \n(documentos, fotos, videos, etc.)",
      "Instalar_dispositivos" = "Conectar o instalar dispositivos adicionales \n(ej. Impresora, módem, cámara, etc.)",
      "Formulas_matematicas" = "Usar fórmulas matemáticas básicas en una \nhoja de cálculo (excel, open office calc, etc.)",
      "Crear_presentaciones" = "Crear presentaciones mediante un programa \nespecializado para ello (Power Point, prezi, otros)",
      "Transferir_archivos" = "Transferir archivos entre computadores \ny otros dispositivos (USB, celular, etc.)",
      "Descargar_programas" = "Descargar o instalar programas \ncomputacionales (software)",
      "Utilizar_lenguaje_programacion" = "Utilizar un lenguaje de programación especializado"
    )) +
  scale_y_continuous(position = "right") +
  annotate("text", x = 9, y = 50, label = "Guajira", size = 3.5) +
  geom_segment(x = 9, y = 58, 
               xend = 9, yend = 68,
               arrow = arrow(length = unit(0.2, "cm"))) +
  geom_curve(x = 8.7, y = 50,
             xend = 8, yend = 63,
             arrow = arrow(length = unit(0.2, "cm")),
             curvature = 0.3) +
  annotate("text", x = 1.4, y = 40, label = "Caquetá", size = 3.5) +
  geom_curve(x = 1.4, y = 32,
             xend = 1.2, yend = 21.25,
             arrow = arrow(length = unit(0.2, "cm")),
             curvature = 0.3) + 
  annotate("text", x = 0.6, y = 39, label = "Quindio", size = 3.5) +
  geom_curve(x = 0.6, y = 32,
             xend = 0.9, yend = 21,
             arrow = arrow(length = unit(0.2, "cm")),
             curvature = -0.3) + 
  annotate("text", x = 5.5, y = 87, label = "Bogotá", size = 3.5) +
  geom_curve(x = 5.5, y = 80,
             xend = 5.8, yend = 75.2,
             arrow = arrow(length = unit(0.2, "cm")),
             curvature = -0.3) + 
  annotate("text", x = 6.5, y = 92, label = "Antioquia", size = 3.5) +
  geom_curve(x = 6.5, y = 84,
             xend = 6.15, yend = 80,
             arrow = arrow(length = unit(0.2, "cm")),
             curvature = 0.3) + 
  theme(
    text = element_text(family = "Assistant"),
    plot.title = element_text(face = "bold", size = 21.5),
    plot.title.position = "plot",
    plot.subtitle = element_text(size = 13),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.minor =  element_blank(),
    panel.grid.major.x = element_line(color = "lightgrey"),
    legend.position = "none",
    plot.margin = margin(t = 15, l = 12, r = 12, b = 10)
  )

ggsave("09-statistics/plot.png", height = 7, width = 7)
