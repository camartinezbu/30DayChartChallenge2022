library(tidyverse)
library(readxl)
library(shadowtext)
library(lubridate)

# Load data of personal ibjuries in Colombia
# Source: https://www.policia.gov.co/grupo-informacion-criminalidad/estadistica-delictiva
# Downloaded on the 27th of April 2022
data <- read_xls("23-tiles/data/lesiones_personales_1.xls")

tz(data$FECHA_HECHO) <- "America/Bogota"

# Create daily count
historico_diario <- data %>%
  transmute(FECHA = ymd(FECHA_HECHO), CANTIDAD) %>%
  group_by(FECHA) %>%
  summarise(REGISTROS = sum(CANTIDAD)) %>%
  ungroup() %>%
  mutate(DIA = wday(FECHA, label = T, week_start = 1, locale = "es_ES")) %>%
  mutate(MES = month(FECHA, label = T, locale = "es_ES")) %>%
  mutate(SEMANA = isoweek(FECHA))

historico_diario$SEMANA[historico_diario$MES =="ene" & historico_diario$SEMANA == 4] = 5
historico_diario$SEMANA[historico_diario$MES =="ene" & historico_diario$SEMANA == 3] = 4
historico_diario$SEMANA[historico_diario$MES =="ene" & historico_diario$SEMANA == 2] = 3
historico_diario$SEMANA[historico_diario$MES =="ene" & historico_diario$SEMANA == 1] = 2
historico_diario$SEMANA[historico_diario$MES =="ene" & historico_diario$SEMANA == 53] = 1
historico_diario$SEMANA[historico_diario$MES =="dic" & historico_diario$SEMANA == 1] = 53 

historico_diario <- historico_diario %>%
  group_by(MES) %>%
  mutate(SEMANA_MES = 1 + SEMANA - min(SEMANA))

# Heatmap calendar
ggplot(data = historico_diario, 
       aes(x = DIA, y = -SEMANA_MES, fill = REGISTROS)) +
  geom_tile(colour = "white") +
  geom_shadowtext(aes(label = day(FECHA)), size = 3, 
                  color = "white", face = "bold",
                  gp = gpar(cex = 0.005)) +
  scale_fill_viridis_c(name = "Registros", direction = -1, 
                       guide = guide_colourbar(barheight = 0.75, barwidth = 10), 
                       option = "A") +
  facet_wrap(~MES, nrow = 4, ncol = 3, scales = "free") +
  labs(title = "Comportamiento de las lesiones personales\nen Colombia durante 2021",
       caption = "Fuente: SIEDCO | Elaborado por Camilo Martínez (@camartinezbu)") +
  theme(
    text = element_text(family = "Fira Sans Condensed"),
    plot.title = element_text(hjust = 0.5, size = 22, face = "bold",
                              margin = margin(0,0,0.5,0, unit = "cm")),
    plot.title.position = "plot",
    plot.caption.position  = "plot",
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.title.align = 0.5,
    legend.margin = margin(t = -5, l = -5, r = -5, b = -5),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 15),
    panel.border = element_rect(colour = "grey", fill=NA, size=1)
  )

ggsave("23-tiles/plot.png", height = 8, width = 6)
