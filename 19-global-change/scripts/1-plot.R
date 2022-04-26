library(tidyverse)
library(ggtext)

# Load Infant Mortality Rate Data
# Source: World Bank Data
data <- read_csv("19-global-change/data/mortality_rate_world_bank.csv") %>%
  head(4) %>%
  select(-`Series Name`, -`Series Code`) %>%
  pivot_longer(-c(`Country Name`, `Country Code`), names_to = "Year", values_to = "Rate") %>%
  mutate(Year = as.numeric(str_extract(Year, "\\d{4}"))) %>%
  mutate(`Country Name` = case_when(
    `Country Name` == "High income" ~ "Ingreso alto",
    `Country Name` == "Low income" ~ "Ingreso bajo",
    `Country Name` == "Lower middle income" ~ "Ingreso mediano bajo",
    `Country Name` == "Upper middle income" ~ "Ingreso mediano alto",
  )) %>%
  mutate(label = case_when(
    Year == 2020 ~ paste0(`Country Name`, ": ", Rate)
  ))

# Create line plot
ggplot(data, aes(x = Year, y = Rate, color = `Country Name`, label = label)) +
  geom_line(size = 1.25) +
  geom_rect(aes(xmin = 2020, xmax = 2031.5, ymin = 0, ymax = 110), 
            fill = "white", color = "white") +
  geom_richtext(
    family = "Fira Sans Condensed",
    fontface = "bold",
    hjust = "left",
    size = 2.5,
    fill = NA,
    label.color = NA
  ) +
  scale_x_continuous(
    breaks = c(1990, 2000, 2010, 2020),
    limits = c(1990, 2031.5),
    expand = expansion(mult = 0, add = 0)
  ) +
  scale_y_continuous(
    breaks = c(0, 20, 40, 60, 80, 100)
  ) +
  scale_color_manual(
    values = c(
      "Ingreso bajo" = "#F6511D",
      "Ingreso mediano bajo" = "#FFB400",
      "Ingreso mediano alto" = "#00A6ED",
      "Ingreso alto" = "#7FB800"
    )
  ) +
  labs(
    title = "La tasa de mortalidad infantil ha caido\nen todo el mundo",
    subtitle = "Sin embargo, persisten brechas importantes al desagregar por niveles de ingreso:\nla tasa en paises de ingreso bajo es 11 veces la tasa en paises de ingreso alto.",
    caption = "Fuente: World Bank Data | Elaborado por Camilo Martínez (@camartinezbu)\nNota: La tasa de mortalidad infantil se define como el número de niños \nque mueren antes de cumplir un año por cada 1.000 nacimientos.",
    y = "Tasa de mortalidad infantil"
  ) +
  theme(
    text = element_text(family  ="Fira Sans Condensed"),
    plot.title = element_text(face = "bold", size = 22),
    plot.title.position = "plot",
    plot.subtitle = element_text(size = 10.5),
    plot.caption = element_text(size = 8, hjust = 0.5),
    plot.caption.position = "plot",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(size = 0.5, color = "#dddddd"),
    panel.background = element_blank(),
    legend.position = "none",
    axis.line = element_blank(),
    axis.ticks = element_line(size = 0.5, color = "black"),
    axis.title.x = element_blank()
  )

ggsave("19-global-change/plot.png", height = 5, width = 5)