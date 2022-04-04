library(tidyverse)
library(ggtext)
library(ggraph)
library(igraph)

# Data Source https://www.gbif.org/dataset/5f78d639-26ec-473b-90c9-4efe59a0632d#description
data <- read_tsv("04-flora/data/0205303-210914110416597.csv")

# Create graph edges
create_edges <- function(dataset, column1, column2) {
  dataset %>%
    select(!! sym(column1), !! sym(column2)) %>%
    distinct(!! sym(column1), !! sym(column2)) %>%
    rename("from" = !! sym(column1), "to" = !! sym(column2)) %>%
    drop_na()
}

kingdom_to_phylum<- create_edges(data, "kingdom", "phylum")
phylum_to_class <- create_edges(data, "phylum", "class")
class_to_order <- create_edges(data, "class", "order")
order_to_family <- create_edges(data, "order", "family")
# family_to_genus <- create_edges(data, "family", "genus")
# genus_to_species <- create_edges(data, "genus", "species")

edges <- bind_rows(
  kingdom_to_phylum,
  phylum_to_class,
  class_to_order,
  order_to_family #,
#  family_to_genus,
#  genus_to_species
)

# Create graph vertices
create_vertices <- function(dataset, column) {
  dataset %>%
    select(!! sym(column)) %>%
    distinct(!! sym(column)) %>%
    rename("name" = !! sym(column)) %>%
    drop_na() %>%
    mutate(size = 0)
}

kingdoms <- create_vertices(data, "kingdom")
phylums <- create_vertices(data, "phylum")
classes <- create_vertices(data, "class")
orders <- create_vertices(data, "order")
families <- create_vertices(data, "family")
# genera <- create_vertices(data, "genus")
# species <- create_vertices(data, "species")

size_families <- data %>%
  count(family) %>%
  rename(size = n)

families <- families %>%
  arrange(name) %>%
  select(-size) %>%
  bind_cols(size_families) %>%
  select(-family)

vertices <- bind_rows(
  kingdoms,
  phylums,
  classes,
  orders,
  families #,
#  genera,
#  species
)

vertices <- vertices %>%
  mutate(label_fungi = case_when(name == "Fungi" ~ "Fungi")) %>%
  mutate(label_plantae = case_when(name == "Plantae" ~ "Plantae"))

# Create graph
graph <- graph_from_data_frame(edges, vertices = vertices)

# Create plot
set.seed(1216711759)

ggraph(graph, layout = 'circlepack', weight = size) +
  geom_node_circle(aes(fill = depth)) +
  geom_node_label(aes(label = label_fungi), size = 3, 
                  nudge_x = p$data$x * .13, 
                  nudge_y = p$data$y * -.13,
                  fontface = "bold.italic",
                  color = "#440154") +
  geom_node_label(aes(label = label_plantae), size = 3,
                  fontface = "bold.italic",
                  color = "#440154") +
  scale_fill_viridis() +
  labs(
    title = "Se registran más de 4857 especies de flora en la \ncuenca del río Orinoco",
    subtitle = glue::glue("Estas especies se dividen en 2 
                          <b><span style='color:{colorspace::darken(\"#440154\", 0.2)}'>reinos</span></b>,
                          5 <b><span style='color:{colorspace::darken(\"#31688e\", 0.2)}'>filos</span></b>,
                          12 <b><span style='color:{colorspace::darken(\"#35b779\", 0.2)}'>clases</span></b> y
                          83 <b><span style='color:{colorspace::darken(\"#fde725\", 0.2)}'>órdenes</span></b>."),
    caption = "Fuente: Instituto Amazónico de Investigaciones Científicas | Elaborado por Camilo Martínez (@camartinezbu)"
  ) +
  theme(
    text = element_text(family = "Assistant"),
    legend.position = "none",
    panel.background = element_rect(fill = "white"),
    plot.margin = margin(t = 10, b = 5, l = 5, r = 5),
    plot.title = element_text(face = "bold", size = 23),
    plot.subtitle = element_markdown(size = 13, lineheight = 1.05)
  )

ggsave("04-flora/plot.png", height = 8, width = 7)
