library(tidyverse)

# https://www.datos.gov.co/Educaci-n/MEN_MATRICULA_ESTADISTICA_ES/5wck-szir
matricula <- read_csv("01-part-to-whole/data/raw/MEN_MATRICULA_ESTADISTICA_ES.csv")

# https://www.datos.gov.co/Educaci-n/MEN_PROGRAMAS_DE_EDUCACI-N_SUPERIOR/upr9-nkiz
programas <- read_csv("01-part-to-whole/data/raw/MEN_PROGRAMAS_DE_EDUCACI_N_SUPERIOR.csv")

matricula_redux <- matricula %>%
  filter(Año == 2020, Semestre == 1) %>%
  select(`Programa Académico`, Id_Area, `Id Género`, Año, `Total Matriculados`)

areas_conocimiento <- programas %>%
  select(codigoareaconocimiento, nombreareaconocimiento) %>%
  distinct(codigoareaconocimiento, nombreareaconocimiento) %>%
  filter(!is.na(nombreareaconocimiento))

final_dataset <- matricula_redux %>%
  left_join(areas_conocimiento, by = c("Id_Area" = "codigoareaconocimiento")) %>%
  group_by(Id_Area, nombreareaconocimiento, `Id Género`) %>%
  summarise(Matriculados = sum(`Total Matriculados`)) %>%
  mutate(Porcentaje = Matriculados / sum(Matriculados) * 100) %>%
  rename(Id = Id_Area, Area = nombreareaconocimiento, Genero = `Id Género`)

write_csv(final_dataset, "01-part-to-whole/data/final_dataset.csv")
