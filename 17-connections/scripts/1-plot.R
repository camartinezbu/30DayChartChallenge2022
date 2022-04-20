library(tidyverse)
library(tidygraph)
library(ggraph)
library(sf)
library(sfnetworks)

# Load geometries ----

##  Load Departments Geometry
##  Source: https://geoportal.dane.gov.co/servicios/descarga-y-metadatos/descarga-mgn-marco-geoestadistico-nacional/

shp_departamentos <- st_read("~/OneDrive/Mapas/Nacional/ADMINISTRATIVO/MGN_DPTO_POLITICO.shp") %>%
  filter(DPTO_CNMBR != "ARCHIPIÉLAGO DE SAN ANDRÉS, PROVIDENCIA Y SANTA CATALINA")

## Load Municipalities Geometry
## Source: https://geoportal.dane.gov.co/servicios/descarga-y-metadatos/descarga-mgn-marco-geoestadistico-nacional/

shp_municipios <- st_read("~/OneDrive/Mapas/Nacional/ADMINISTRATIVO/MGN_MPIO_POLITICO.shp")

## Calculate centroids
centroids_municipios <- st_centroid(shp_municipios)

# Load Vertices ----
vertices <- readxl::read_xlsx("17-connections/data/data_graph.xlsx", sheet = "Vertices")

vertices_centroids <- centroids_municipios %>%
  inner_join(vertices, by = c("MPIO_CCDGO" = "divipola"))

## Extract vertices coordinates
vertices_coords <- vertices_centroids %>%
  st_coordinates()

# Load Edges ----
edges <- readxl::read_xlsx("17-connections/data/data_graph.xlsx", sheet = "Edges")

# Create Graph ----
osc_graph <- sfnetwork(vertices_centroids, edges, edges_as_lines = TRUE, node_key = "MPIO_CCDGO") %>%
  activate("nodes")

# Plot graph ----
X11(type = "cairo")

ggraph(osc_graph, layout = vertices_coords) +
  geom_sf(data = shp_departamentos, color = "white", 
          fill = "#333333", alpha = .75, lwd = 0.05) +
  geom_node_point(aes(color = aglomeracion, shape = funcion, size = funcion))+
  geom_edge_fan(aes(color = aglomeracion), alpha = 0.25) +
  annotate(
    "text", 
    label = "El DNP define las Aglomeraciones Urbanas como \n\'ciudades funcionales cuyas actividades \nhan desbordado el límite político-\nadministrativo de la ciudad núcleo \ny desarrollan sus actividades\nen municipios aledaños'.", 
    x = -82.5, y = 10.5, 
    color = "white", size = 5,
    family  ="Assistant", hjust = 0
  ) +
  annotate(
    "text", 
    label = "En el Sistema de Ciudades \nhay 18 Aglomeraciones \nUrbanas, que agrupan a \n113 municipios.", 
    x = -82.5, y = 4.5, 
    color = "white", size = 5,
    family  ="Assistant", hjust = 0
  ) +
  labs(
    title = "Aglomeraciones Urbanas del \nSistema de Ciudades",
    caption = "Fuente: Misión Sistema de Ciudades | Elaborado por Camilo Martínez (@camartinezbu)"
  ) +
  coord_sf(
    xlim = c(-82.8, -71),
    ylim = c(0.9, 12.64),
    expand = FALSE
  ) +
  scale_shape_manual(
    values = c(
      "Centro aglomeración" = 15,
      "En aglomeración" = 16 
    )
  ) +
  scale_size_manual(
    values = c(
      "Centro aglomeración" = 1.75,
      "En aglomeración" = 0.25 
    )
  ) +
  theme(
    text = element_text(family = "Assistant", color = "white"),
    plot.title = element_text(face = "bold", size = 25, hjust = 0.5),
    plot.subtitle = element_text(size = 10),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = "black", color = "black"),
    panel.background = element_blank(),
    legend.position = "none"
  )

ggsave("17-connections/plot.png", height = 7, width = 6, type = "cairo")
