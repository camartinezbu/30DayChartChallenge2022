library(tidyverse)
library(sf)
library(raster)
library(rasterVis)
library(elevatr)
library(ggridges)

# Create bboxes
colombia_bbox_df <- data.frame(x = c(-79.01021, -66.84722),
                               y = c(-4.229406, 12.459443))

san_andres_bbox_df <- data.frame(x = c(-81.741015, -81.684211),
                                 y = c(12.476746, 12.602285))

providencia_bbox_df <- data.frame(x = c(-81.416775, -81.335236),
                                  y = c(13.313685, 13.400033))
  
# Get elevation data based on bbox
prj_dd <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

elev_col <- get_elev_raster(colombia_bbox_df, prj =  prj_dd, z = 4, clip = "bbox")

raster::plot(elev_col)

elev_san_andres <- get_elev_raster(san_andres_bbox_df, prj = prj_dd, z = 10, clip = "bbox")

raster::plot(elev_san_andres)

elev_providencia <- get_elev_raster(providencia_bbox_df, prj = prj_dd, z = 10, clip = "bbox")

raster::plot(elev_providencia)

# raster::plot(elev_col)

elev_col_df <- data.frame(sampleRegular(elev_col, 1000000, xy=TRUE))
names(elev_col_df) <- c('x', 'y', 'elevation')

y_values <- elev_col_df %>%
  dplyr::select(y) %>%
  distinct()

y_values_subset <- y_values %>%
  slice(which(row_number() %% 5 == 1)) %>%
  pull()

elev_col_df_redux <- elev_col_df %>%
  filter(y %in% y_values_subset)

elev_san_andres_df <- data.frame(sampleRegular(elev_san_andres, 1000000, xy=TRUE))
names(elev_san_andres_df) <- c('x', 'y', 'elevation')

y_values_san_andres <- elev_san_andres_df %>%
  dplyr::select(y) %>%
  distinct()

y_values_san_andres_subset <- y_values_san_andres %>%
  slice(which(row_number() %% 30 == 1)) %>%
  pull()

elev_san_andres_df_redux <- elev_san_andres_df %>%
  filter(y %in% y_values_san_andres_subset)

elev_providencia_df <- data.frame(sampleRegular(elev_providencia, 1000000, xy=TRUE))
names(elev_providencia_df) <- c('x', 'y', 'elevation')

y_values_providencia <- elev_providencia_df %>%
  dplyr::select(y) %>%
  distinct()

y_values_providencia_subset <- y_values_providencia %>%
  slice(which(row_number() %% 30 == 1)) %>%
  pull()

elev_providencia_df_redux <- elev_providencia_df %>%
  filter(y %in% y_values_providencia_subset)

san_andres_map <- ggplot() +
  geom_density_ridges(data = elev_san_andres_df_redux,
                      aes(x, y, 
                          group = y,
                          height = elevation),
                      stat = "identity", scale=.5,
                      fill="black", color="white") +
  theme_void() + 
  theme(panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"))

providencia_map <- ggplot() +
  geom_density_ridges(data = elev_providencia_df_redux,
                      aes(x, y, 
                          group = y,
                          height = elevation),
                      stat = "identity", scale=.51,
                      fill="black", color="white") +
  theme_void() + 
  theme(panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"))

distance <- 1.5

ggplot() +
  geom_density_ridges(data = elev_col_df_redux,
                      aes(x, y, 
                          group = y,
                          height = elevation),
                      stat = "identity", scale=6,
                      fill="black", color="white") +
  expand_limits(y = c(colombia_bbox_df[1,2] - distance, colombia_bbox_df[2,2] + distance),
                x = c(colombia_bbox_df[1,1] - distance, colombia_bbox_df[2,1] + distance)) +
  labs(
    title = "Relieve de\nColombia",
    caption = "Fuente: AWS Terrain Tiles | Elaborado por Camilo Martínez (@camartinezbu)"
  ) +
  theme_void() + 
  theme(panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        text = element_text(family = "Assistant", colour = "white"),
        plot.title= element_text(size = 26, face = "bold", hjust = 0.5),
        plot.margin = margin(t = 8, l = 4, r = 4, b = 4),
        plot.caption = element_text(hjust = 0.5)) +
# Comment the code below to toggle San Andrés and Providencia
    annotation_custom(
    grob = ggplotGrob(san_andres_map),
    xmin = -77.5,
    xmax = -77,
    ymin = 12,
    ymax = 14) + 
  annotation_custom(
    grob = ggplotGrob(providencia_map),
    xmin = -76.75,
    xmax = -76.25,
    ymin = 13,
    ymax = 14)

ggsave("08-mountains/plot2.png", height = 6, width = 6)


