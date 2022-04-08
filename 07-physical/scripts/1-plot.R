library(tidyverse)
library(sf)
library(osmdata)

# Define bbox of the plot
bog_bbox <- st_bbox(c(xmin = -74.243302, xmax = -74.019593, 
                      ymin = 4.504405, ymax = 4.770719),
                    crs = 4326)

# Retrieve street data from Bogota
bog_roads <- bog_bbox %>%
  opq() %>%
  add_osm_feature("highway", 
                  c("motorway", 
                    "primary", 
                    "secondary", 
                    "tertiary")) %>%
  osmdata_sf()

bog_small_roads <- bog_bbox %>%
  opq() %>%
  add_osm_feature("highway",
                  c("residential")) %>%
  osmdata_sf()

bog_roads_final <- bog_roads$osm_lines %>%
  filter(is.na(planned)) %>%
  mutate(name_type = case_when(
    str_detect(name, "Avenida Calle") ~ "Avenida",
    str_detect(name, "Avenida Carrera") ~ "Avenida",
    str_detect(name, "Avenida Diagonal") ~ "Avenida",
    str_detect(name, "Avenida Transversal") ~ "Avenida",
    str_detect(name, "Calle") ~ "Calle",
    str_detect(name, "Carrera") ~ "Carrera",
    str_detect(name, "Diagonal") ~ "Diagonal",
    str_detect(name, "Transversal") ~ "Transversal",
    str_detect(name, "Avenida") ~ "Avenida",
    TRUE ~ "Otro"
  ))

bog_small_roads_final <- bog_small_roads$osm_lines %>%
  mutate(name_type = case_when(
    str_detect(name, "Avenida Calle") ~ "Avenida",
    str_detect(name, "Avenida Carrera") ~ "Avenida",
    str_detect(name, "Avenida Diagonal") ~ "Avenida",
    str_detect(name, "Avenida Transversal") ~ "Avenida",
    str_detect(name, "Calle") ~ "Calle",
    str_detect(name, "Carrera") ~ "Carrera",
    str_detect(name, "Diagonal") ~ "Diagonal",
    str_detect(name, "Transversal") ~ "Transversal",
    str_detect(name, "Avenida") ~ "Avenida",
    TRUE ~ "Otro"
  ))


# plot
plot <- ggplot() +
  geom_sf(data = bog_small_roads_final,
          aes(color = name_type),
          size = 0.15) + 
  geom_sf(data = bog_roads_final,
          aes(color = name_type),
          size = .5) + 
  labs(
    title = "Nombres de las vías en Bogotá",
    caption = "Fuente: Open Street Map | Elaborado por Camilo Martínez (@camartinezbu)"
  ) +
  scale_color_manual(
    values = c(
      `Avenida` = "#f1007d",
      `Calle` = "#8ecd8a",
      `Carrera` = "#f6cb6d",
      `Diagonal` = "#5d129f",
      `Transversal` = "#008e5f",
      `Otro` = "lightgrey"
    )
  ) +
  guides(colour = guide_legend(nrows = 1)) +
  theme(
    text = element_text(family = "Assistant"),
    plot.title = element_text(face = "bold",
                              size = 22,
                              hjust = 0.5),
    plot.title.position = "plot",
    plot.margin = margin(t = 20, l = 10, r = 10, b = 10),
    panel.background = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.margin = margin(t = 10, r = 0, b = 0, l = 0),
    legend.box.margin = margin(t = 0, r = -10, b = -20, l = -10)
  ) 

ggsave("07-physical/plot.png", plot = plot, height = 9, width = 6.5)



