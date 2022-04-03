library(tidyverse)
library(sf)
library(patchwork)
library(ggtext)

# Change default renderer
X11(type = "cairo")

# bbox for continental colombia: -80.586726,-4.695470,-65.107810,13.015185
x_continental <- c(-80.586726, -65.107810)
y_continental <- c(-4.695470, 13.015185)

# bbox for san andres: -81.741015, 12.476746,-81.684211,12.602285
x_san_andres <- c(-81.741015, -81.684211)
y_san_andres <- c(12.476746, 12.602285)

# bbox for providencia: -81.416775,13.313685,-81.335236,13.400033
x_providencia <- c(-81.416775, -81.335236)
y_providencia <- c(13.313685, 13.400033)

# load geometries
colombia1908 <- st_read("03-historical/data/raw/Division_Admin_1908.shp") %>%
  st_transform(crs = 4326)
colombia1912 <- st_read("03-historical/data/raw/Division_Admin_1912.shp") %>%
  st_transform(crs = 4326)
colombia1928 <- st_read("03-historical/data/raw/Division_Admin_1928.shp") %>%
  st_transform(crs = 4326)
colombia1942 <- st_read("03-historical/data/raw/Division_Admin_1942.shp") %>%
  st_transform(crs = 4326)
colombia1958 <- st_read("03-historical/data/raw/Division_Admin_1958.shp") %>%
  st_transform(crs = 4326)
colombia1966 <- st_read("03-historical/data/raw/Division_Admin_1966.shp") %>%
  st_transform(crs = 4326)
colombia1973 <- st_read("03-historical/data/raw/Division_Admin_1973.shp") %>%
  st_transform(crs = 4326)
colombia1985 <- st_read("03-historical/data/raw/Division_Admin_1985.shp") %>%
  st_transform(crs = 4326)
colombia1991 <- st_read("03-historical/data/raw/MGN_DPTO_POLITICO.shp") %>%
  st_transform(crs = 4326) %>%
  mutate(CLASE = case_when(DPTO_CCDGO == "11" ~ "Distrito Capital",
                           TRUE ~ "Departamento"))

# Functions to create maps
create_continental_map <- function(data, year) {
  ggplot() +
    geom_sf(data = data, aes(fill = CLASE), color = "white", lwd = 0.5) +
    coord_sf(xlim = x_continental, ylim = y_continental) +
    labs(title = year) +
    theme(
      text = element_text(family = "Assistant"),
      panel.grid = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "none",
      plot.title = element_text(face = "bold", size = 18)
    )
}

create_san_andres_map <- function(data, year) {
  ggplot() +
    geom_sf(data = data, aes(fill = CLASE), color = "white",
            show.legend = FALSE) +
    coord_sf(xlim = x_san_andres, ylim = y_san_andres) +
    theme(
      text = element_text(family = "Assistant"),
      panel.grid = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    )
}

create_providencia_map <- function(data, year) {
  ggplot() +
    geom_sf(data = data, aes(fill = CLASE), color = "white",
            show.legend = FALSE) +
    coord_sf(xlim = x_providencia, ylim = y_providencia) +
    theme(
      text = element_text(family = "Assistant"),
      panel.grid = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    )
}

create_full_map <- function(continental_map, san_andres_map, providencia_map) {
  continental_map +
    annotation_custom(
      grob = ggplotGrob(san_andres_map),
      xmin = -76.5,
      xmax = -81,
      ymin = 10,
      ymax = 14
    ) +
    annotation_custom(
      grob = ggplotGrob(providencia_map),
      xmin = -74.5,
      xmax = -80.5,
      ymin = 11,
      ymax = 13
    )
}

# Colors
comisaria_color <- "#fcc800"
departamento_color <- "#e05652"
distrito_capital_color <- "#4aa042"
distrito_especial_color <- "#3fcfde"
intendencia_color <- "#006aa3"

# For 1908
my_scale1908 <- scale_fill_manual(
  name = NULL,
  values = c(departamento_color, distrito_capital_color, intendencia_color),
  labels = c("Departamento", "Distrito Capital", "Intendencia")
)

continental1908 <- create_continental_map(colombia1908, "1908") + my_scale1908
san_andres1908 <- create_san_andres_map(colombia1908) + my_scale1908
providencia1908 <- create_providencia_map(colombia1908) + my_scale1908

full1908 <- create_full_map(continental1908, san_andres1908, providencia1908)
  
# For 1912
my_scale1912 <- scale_fill_manual(
  name = NULL,
  values = c(comisaria_color, departamento_color, intendencia_color),
  labels = c("Comisaría", "Departamento", "Intendencia")
)

continental1912 <- create_continental_map(colombia1912, "1912") + my_scale1912
san_andres1912 <- create_san_andres_map(colombia1912) + my_scale1912
providencia1912 <- create_providencia_map(colombia1912) + my_scale1912

full1912 <- create_full_map(continental1912, san_andres1912, providencia1912)

# For 1928
my_scale1928 <- scale_fill_manual(
  name = NULL,
  values = c(comisaria_color, departamento_color, intendencia_color),
  labels = c("Comisaría", "Departamento", "Intendencia")
)

continental1928 <- create_continental_map(colombia1928, "1928") + my_scale1928
san_andres1928 <- create_san_andres_map(colombia1928) + my_scale1928
providencia1928 <- create_providencia_map(colombia1928) + my_scale1928

full1928 <- create_full_map(continental1928, san_andres1928, providencia1928)

# For 1942
my_scale1942 <- scale_fill_manual(
  name = NULL,
  values = c(comisaria_color, departamento_color, intendencia_color),
  labels = c("Comisaría", "Departamento", "Intendencia")
)

continental1942 <- create_continental_map(colombia1942, "1942") + my_scale1942 
san_andres1942 <- create_san_andres_map(colombia1942) + my_scale1942
providencia1942 <- create_providencia_map(colombia1942) + my_scale1942

full1942 <- create_full_map(continental1942, san_andres1942, providencia1942)

# For 1958
my_scale1958<- scale_fill_manual(
  name = NULL,
  values = c(comisaria_color, departamento_color, distrito_especial_color, intendencia_color),
  labels = c("Comisaría", "Departamento", "Distrito Especial", "Intendencia")
)

continental1958 <- create_continental_map(colombia1958, "1958") + my_scale1958
san_andres1958 <- create_san_andres_map(colombia1958) + my_scale1958
providencia1958 <- create_providencia_map(colombia1958) + my_scale1958

full1958 <- create_full_map(continental1958, san_andres1958, providencia1958)

# For 1966
my_scale1966 <- scale_fill_manual(
  name = NULL,
  values = c(comisaria_color, departamento_color, distrito_especial_color, intendencia_color),
  labels = c("Comisaría", "Departamento", "Distrito Especial", "Intendencia")
)

continental1966 <- create_continental_map(colombia1966, "1966") + my_scale1966
san_andres1966 <- create_san_andres_map(colombia1966) + my_scale1966
providencia1966 <- create_providencia_map(colombia1966) + my_scale1966

full1966 <- create_full_map(continental1966, san_andres1966, providencia1966)

# For 1973
my_scale1973 <- scale_fill_manual(
  name = NULL,
  values = c(comisaria_color, departamento_color, distrito_especial_color, intendencia_color),
  labels = c("Comisaría", "Departamento", "Distrito Especial", "Intendencia")
)

continental1973 <- create_continental_map(colombia1973, "1973") + my_scale1973
san_andres1973 <- create_san_andres_map(colombia1973) + my_scale1973
providencia1973 <- create_providencia_map(colombia1973) + my_scale1973

full1973 <- create_full_map(continental1973, san_andres1973, providencia1973)

# For 1985
my_scale1985 <- scale_fill_manual(
  name = NULL,
  values = c(comisaria_color, departamento_color, distrito_especial_color, intendencia_color),
  labels = c("Comisaría", "Departamento", "Distrito Especial", "Intendencia")
)

continental1985 <- create_continental_map(colombia1985, "1985") + my_scale1985
san_andres1985 <- create_san_andres_map(colombia1985) + my_scale1985
providencia1985 <- create_providencia_map(colombia1985) + my_scale1985

full1985 <- create_full_map(continental1985, san_andres1985, providencia1985)

# For 1991 - Presente
my_scale1991 <- scale_fill_manual(
  name = NULL,
  values = c(departamento_color, distrito_capital_color),
  labels = c("Departamento", "Distrito Capital")
)

continental1991 <- create_continental_map(colombia1991, "Presente") + my_scale1991
san_andres1991 <- create_san_andres_map(colombia1991) + my_scale1991
providencia1991 <- create_providencia_map(colombia1991) + my_scale1991

full1991 <- create_full_map(continental1991, san_andres1991, providencia1991)

# Full map
plot <- (full1908 + full1912 + full1928) / (full1942 + full1958 + full1966) / (full1973 + full1985 + full1991) +
  plot_annotation(
    title = "La división administrativa en Colombia ha cambiado varias veces",
    subtitle = glue::glue("Aunque hoy en día Colombia se divide en 32
                <b><span style='color:{colorspace::darken(departamento_color, 0.2)}'>departamentos</span></b>
                y 1 <b><span style='color:{colorspace::darken(distrito_capital_color, 0.2)}'>distrito capital</span></b>,
                a lo largo de la <br> historia también ha tenido
                <b><span style='color:{colorspace::darken(comisaria_color, 0.2)}'>comisarias</span></b>,
                <b><span style='color:{colorspace::darken(intendencia_color, 0.2)}'>intendencias</span></b> y
                1 <b><span style='color:{colorspace::darken(distrito_especial_color, 0.2)}'>distrito especial</span></b>."),
    caption = "Fuente: Colombia en Mapas | Elaborado por Camilo Martínez (@camartinezbu)",
    theme = theme(
      text = element_text(family = "Assistant"),
      plot.title = element_markdown(family = "Assistant", face = "bold", size = 28),
      plot.subtitle = element_markdown(size = 20),
      plot.margin = margin(t = 20, b = 10, l = 10, r = 10),
      plot.caption = element_text(size = 18)
    )
  )

ggsave("03-historical/plot.png", plot = plot, width = 12, height = 16, type = "cairo")
