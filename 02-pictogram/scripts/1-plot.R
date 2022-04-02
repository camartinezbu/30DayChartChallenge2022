library(tidyverse)
library(waffle)
library(ggtext)

# https://www.ucsusa.org/resources/satellite-database
data <- read_tsv("02-pictogram/data/database.csv") %>%
  head(-2)
  
data_final <- data %>%
  mutate(Users = case_when(Users == "Earth Observation" ~ "Government",
                           TRUE ~Users)) %>%
  count(Users) %>%
  mutate(Users = paste(Users, "/", sep = "")) %>%
  mutate(Users_new = str_extract(Users, "\\w*/")) %>%
  mutate(Users_new = str_sub(Users_new, 1, nchar(Users_new)-1)) %>%
  mutate(Users_new = factor(Users_new)) %>%
  mutate(Users_new = fct_relevel(Users_new, c("Commercial", "Government", 
                                              "Military", "Civil"))) %>%
  group_by(Users_new) %>%
  summarise(Total = sum(n)) %>%
  ungroup() %>%
  mutate(Pct = round(Total / sum(Total) * 100)) %>%
  mutate(Pct = case_when(Pct == 13 ~14,
                        TRUE ~ Pct))
  

plot <- ggplot(data_final, aes(label = Users_new, values = Pct)) +
  geom_pictogram(n_rows = 5, aes(colour = Users_new), 
                 family = "FontAwesome5Free-Solid", size = 6,
                 show.legend = FALSE) +
  scale_color_manual(values = c("#fca311", "#28427b", "#007a02", "#cc0000")) +
  scale_label_pictogram(values = c("satellite", "satellite", "satellite", "satellite")) +
  scale_x_discrete(expand = expansion(mult = c(0, 0))) +
  scale_y_discrete(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "La mayoría de satélites en órbita son comerciales",
    subtitle = glue::glue("Con corte al 1 de enero de 2022 se registraron 4.852 satélites en órbita alrededor de la Tierra. 
                          <br> De ellos, 3490 son 
                          <b><span style='color:{colorspace::darken(\"#fca311\", 0.2)}'>comerciales</span></b>,
                          654 de <b><span style='color:{colorspace::darken(\"#28427b\", 0.2)}'>gobiernos</span></b>,
                          552 <b><span style='color:{colorspace::darken(\"#007a02\", 0.2)}'>militares</span></b> y 
                          156 <b><span style='color:{colorspace::darken(\"#cc0000\", 0.2)}'>civiles</span></b>."),
    caption = "Fuente: UCS Satellite Database | Elaborado por Camilo Martínez (@camartinezbu)"
  ) +
  theme(
    text = element_text(family = "Assistant"),
    panel.background = element_blank(),
    panel.border = element_blank(),
    plot.margin = margin(t = 7, b = 5),
    plot.title = element_markdown(family = "Assistant Bold", size = 18.5),
    plot.subtitle = element_markdown(size = 9),
    panel.grid = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  coord_equal()
  
ggsave("02-pictogram/plot.png", width = 6, height = 2.5)
