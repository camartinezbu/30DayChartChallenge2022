library(tidyverse)
library(sf)
library(raster)
library(rasterVis)
library(elevatr)
library(ggridges)

# Create Continental Colombia bbox
colombia_bbox_df <- data.frame(x = c(-79.01021, -66.84722),
                               y = c(-4.229406, 12.459443))

# Get elevation data based on bbox
prj_dd <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

elev_col <- get_elev_raster(colombia_bbox_df, prj =  prj_dd, z = 4, clip = "bbox")

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
        plot.caption = element_text(hjust = 0.5))

ggsave("08-mountains/plot.png", height = 6, width = 6)


