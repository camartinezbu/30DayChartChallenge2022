library(tidyverse)
library(ggtext)
library(waffle)

data <- read_csv("01-part-to-whole/data/final_dataset.csv") %>%
  filter(Area != "Sin clasificar") %>%
  mutate(values = round(Porcentaje)) %>%
  mutate(Genero = factor(Genero)) %>%
  mutate(Genero = fct_recode(Genero, Hombres = "1", Mujeres = "2")) %>%
  mutate(Area = fct_relevel(Area, c("Ciencias de la salud",
                                    "Ciencias de la educación",
                                    "Ciencias sociales y humanas",
                                    "Economía administración contaduría y afines",
                                    "Bellas artes",
                                    "Matemáticas y ciencias naturales",
                                    "Agronomía veterinaria y afines",
                                    "Ingeniería arquitectura urbanismo y afines"))) %>%
  arrange(desc(Genero))

labels <- c(`Ciencias de la salud` = glue::glue("Ciencias de <br>la salud <b><span style='color:{colorspace::darken(\"#8300f2\", 0.2)}'>(69%)</span></b>"),
            `Ciencias de la educación` = glue::glue("Ciencias de <br>la educación <b><span style='color:{colorspace::darken(\"#8300f2\", 0.2)}'>(62%)</span></b>"),
            `Ciencias sociales y humanas` = glue::glue("Ciencias sociales <br>y humanas <b><span style='color:{colorspace::darken(\"#8300f2\", 0.2)}'>(62%)</span></b>"),
            `Economía administración contaduría y afines` = glue::glue("Economía, administración,<br> contaduría y afines <b><span style='color:{colorspace::darken(\"#8300f2\", 0.2)}'>(58%)</span></b>"),
            `Bellas artes` = glue::glue("Bellas artes <br><b><span style='color:{colorspace::darken(\"#8300f2\", 0.2)}'>(51%)</span></b>"),
            `Matemáticas y ciencias naturales` = glue::glue("Matemáticas y<br> ciencias naturales <b><span style='color:{colorspace::darken(\"#8300f2\", 0.2)}'>(51%)</span></b>"),
            `Agronomía veterinaria y afines` = glue::glue("Agronomía, <br>veterinaria y afines <b><span style='color:{colorspace::darken(\"#8300f2\", 0.2)}'>(51%)</span></b>"),
            `Ingeniería arquitectura urbanismo y afines` = glue::glue("Ingeniería, arquitectura,<br> urbanismo y afines <b><span style='color:{colorspace::darken(\"#8300f2\", 0.2)}'>(32%)</span></b>"))

plot <- ggplot(data = data, aes(fill = Genero, values = values)) +
  geom_waffle(colour = "white", n_rows = 10, show.legend = FALSE, flip = TRUE) +
  facet_wrap(~Area, ncol = 4, labeller = as_labeller(labels)) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  coord_equal() +
  labs(
    title = glue::glue("La participación de las
    <b><span style='color:{colorspace::darken(\"#8300f2\", 0.2)}'>mujeres</span></b>
    es menor en las ingenierías"),
    subtitle = "Porcentaje de mujeres matriculadas en programas de educación superior en Colombia durante el primer semestre de 2020.",
    caption = "Fuente: Ministerio de Educación Nacional | Elaborado por Camilo Martínez (@camartinezbu)"
  ) +
  theme(
    text = element_text(family = "Assistant"),
    strip.background = element_rect(colour = "white", fill = "white"),
    plot.margin = margin(t = 10, b = 10),
    plot.title = element_markdown(family = "Assistant Bold", size = 18.5),
    plot.subtitle = element_text(size = 9),
    strip.text = element_markdown()
  ) + 
  scale_fill_manual(
    name = NULL,
    values = c("#dedede", "#8300f2")
  )

ggsave("01-part-to-whole/plot.png", width = 7.5, height = 5)

