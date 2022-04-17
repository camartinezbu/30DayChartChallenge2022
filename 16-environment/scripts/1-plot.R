library(tidyverse)
library(sf)

# Faster map rendering
X11(type = "cairo")

# Load green businesses data
# https://www.datos.gov.co/Ambiente-y-Desarrollo-Sostenible/Lista-de-Negocios-Verdes-y-Sostenibles/rggv-qcwf
negocios_verdes <- read_csv("16-environment/data/Lista_de_Negocios_Verdes_y_Sostenibles.csv") %>%
  mutate(DEPARTAMENTO = str_to_lower(DEPARTAMENTO)) %>%
  mutate(MUNICIPIO = str_to_lower(MUNICIPIO)) %>%
  mutate(DEPARTAMENTO = case_when(MUNICIPIO == "bogotá, d.c." ~ "bogotá, d.c.",
                                  TRUE ~ DEPARTAMENTO))
  
# Load municipalities data
municipios <- readxl::read_xlsx("16-environment/data/LISTA DIVIPOLA.xlsx") %>%
  mutate(DPNOM = str_to_lower(DPNOM)) %>%
  mutate(MPIO = str_to_lower(MPIO)) %>%
  mutate(DPNOM = case_when(DPNOM == "archipiélago de san andrés" ~ "archipiélago de san andrés, providencia y santa catalina",
                           TRUE ~ DPNOM))

# Join businesses and municipalities
municipios_negocios <- negocios_verdes %>%
  left_join(municipios, by = c("DEPARTAMENTO" = "DPNOM", "MUNICIPIO" = "MPIO")) %>%
  group_by(DPMP) %>%
  count()

# Load Departments Geometry
# Source: https://geoportal.dane.gov.co/servicios/descarga-y-metadatos/descarga-mgn-marco-geoestadistico-nacional/

shp_departamentos <- st_read("~/OneDrive/Mapas/Nacional/ADMINISTRATIVO/MGN_DPTO_POLITICO.shp")

# Load Municipalities Geometry
# Source: https://geoportal.dane.gov.co/servicios/descarga-y-metadatos/descarga-mgn-marco-geoestadistico-nacional/

shp_municipios <- st_read("~/OneDrive/Mapas/Nacional/ADMINISTRATIVO/MGN_MPIO_POLITICO.shp")

# Calculate municipalities centroids
centroids_municipios <- st_centroid(shp_municipios)

# Join businesses, municipalities and centroids
final_data <- centroids_municipios %>%
  left_join(municipios_negocios, by = c("MPIO_CCDGO" = "DPMP"))

ggplot() +
  geom_sf(data = shp_departamentos, fill = "white") +
  geom_sf(data = final_data, aes(size = n), 
          alpha = 0.4, color = "#16BE5A") +
  labs(
    title = "Negocios verdes \ny sostenibles en Colombia",
    subtitle = "Negocios certificados por el Ministerio de Ambiente \ny Desarrollo Sostenible entre 2014 y 2020.",
    caption = "Fuente: Ministerio de Ambiente y Desarrollo Sostenible | Elaborado por Camilo Martínez (@camartinezbu)\nNota: La lista de negocios incluye los niveles de resultado Inicial, Básico, Intermedio, Satisfactorio, Avanzado e Ideal.\nPara ubicar los puntos en el mapa se usó el centroide del polígono asociado a cada municipio.",
    size = "No. de negocios"
  ) +
  scale_size_area(
    breaks = c(15, 30, 60),
    max_size = 3
  ) +
  theme(
    text = element_text(family = "Assistant"),
    plot.title = element_text(face = "bold", size = 23, hjust = 0.5),
    plot.title.position = "plot",
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    plot.background = element_rect(fill = "#c6e4ee", colour = "#c6e4ee"),
    plot.caption = element_text(size = 6.5, hjust = 0.5),
    plot.caption.position = "plot",
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.position = "top",
    legend.justification = "center",
    legend.direction = "horizontal",
    legend.title = element_text(size = 8),
    legend.margin = margin(t = 5, b = -10)
  )
  
ggsave("16-environment/plot.png", 
       height = 6, width = 4.75,
       type = "cairo")
