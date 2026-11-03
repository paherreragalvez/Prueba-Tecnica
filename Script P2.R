rm(list = ls())
library(readr)
library(dplyr)
library(srvyr)
library(knitr)
library(tidyr)
library(readxl)
library(ggplot2)

# Gráfico Banco Central

pib_regional <- read_excel("datos/CCNN2018_PIB_V_ACT_T.xlsx", sheet = "Cuadro", skip = 2) %>% janitor::clean_names()

pib_regional$periodo <- as.integer(substr(pib_regional$periodo, 1, 4))

pib_regional |>
  select(-x1_produccion_de_bienes) |>
  pivot_longer(
    cols = -periodo,
    names_to = "sector",
    values_to = "pib_valor"
  ) |>
  mutate(
    sector = recode(
      sector,
      "x1_1_mineria" = "Minería",
      "x1_2_industria" = "Industria",
      "x1_3_resto_de_bienes" = "Resto de bienes",
      "x2_comercio" = "Comercio",
      "x3_servicios" = "Servicios"
    )
  ) |>
  ggplot(aes(x = periodo, y = pib_valor, group = sector, color = sector)) +
  geom_line(linewidth = 0.9, alpha = 0.8) +
  geom_point(size = 1.8) +
  labs(
    title = "Evolución del PIB Regional por Sector Económico",
    subtitle = "Región de Valparaíso",
    x = "Año",
    y = "PIB (miles de millones de pesos encadenados)",
    color = "Sector Económico"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9, face = "bold"),
    plot.title = element_text(face = "bold", size = 12)
  ) +
  scale_y_continuous(
    labels = scales::number_format(big.mark = ".", decimal.mark = ",")
  ) +
  scale_x_continuous(
    breaks = scales::pretty_breaks(),
    labels = scales::number_format(accuracy = 1)
  )



# Importar bases de datos.
ene_0826 <- read_csv2("datos/ene-2026-08-jas.csv")

# Definición global para el tratamiento de estratos con una sola PSU (lonely PSUs)
options(survey.lonely.psu = "certainty")

# Diseños muestrales
ene_diseno26 <- ene_0826 |>
  as_survey_design(ids = conglomerado, strata = estrato, weights = fact_cal, nest = TRUE)

# Filtros diseños muestrales (Valparaíso, Mujeres, >= 15 años)
f_ene_diseno26 <- ene_diseno26 |> filter(region == 5, sexo == 2, edad >= 15)

#########################################################

# Vector con las etiquetas oficiales de las secciones CAENES (Anexo 5 ENE)
etiquetas_caenes <- c(
  "A. Agricultura, ganadería, silvicultura y pesca",
  "B. Explotación de minas y canteras",
  "C. Industrias manufactureras",
  "D. Suministro de electricidad y gas",
  "E. Suministro de agua y gestión de desechos",
  "F. Construcción",
  "G. Comercio y reparación de vehículos",
  "H. Transporte y almacenamiento",
  "I. Alojamiento y servicio de comidas",
  "J. Información y comunicaciones",
  "K. Actividades financieras y de seguros",
  "L. Actividades inmobiliarias",
  "M. Actividades profesionales, científicas y técnicas",
  "N. Servicios administrativos y de apoyo",
  "O. Administración pública y defensa",
  "P. Enseñanza",
  "Q. Atención de la salud humana y asistencia social",
  "R. Actividades artísticas y recreativas",
  "S. Otras actividades de servicios",
  "T. Actividades de los hogares como empleadores",
  "U. Actividades de organizaciones extraterritoriales"
)

# 2. Estimación para Agosto 2026 (Solo proporción sin se/cv)
t26 <- f_ene_diseno26 |>
  filter(cae_especifico %in% 1:7, r_p_rev4cl_caenes %in% 1:21) |>
  mutate(rama_nombre = factor(r_p_rev4cl_caenes, levels = 1:21, labels = etiquetas_caenes)) |>
  group_by(rama_nombre) |>
  summarise(prop_empleo = survey_mean() * 100) |>
  mutate(prop_2026 = format(round(prop_empleo, 2), decimal.mark = ",", nsmall = 2)) |>
  select(rama_nombre, prop_2026)

# Presentación en Tabla Tabular con kable

t26_servicios <- t26 |>
  filter(rama_nombre %in% c(
  "H. Transporte y almacenamiento",
  "I. Alojamiento y servicio de comidas",
  "J. Información y comunicaciones",
  "K. Actividades financieras y de seguros",
  "L. Actividades inmobiliarias",
  "M. Actividades profesionales, científicas y técnicas",
  "N. Servicios administrativos y de apoyo",
  "O. Administración pública y defensa",
  "P. Enseñanza",
  "Q. Atención de la salud humana y asistencia social",
  "R. Actividades artísticas y recreativas",
  "S. Otras actividades de servicios",
  "T. Actividades de los hogares como empleadores",
  "U. Actividades de organizaciones extraterritoriales"
  ))

kable(
  t26_servicios,
  col.names = c("Rama de Actividad (CAENES)", "Prop. Ocupadas Ago-2026 (%)"),
  caption = "Participación del Empleo Femenino por Rama - Región de Valparaíso (Solo Servicios)",
  align = c("l", "r")
)


t26 |> select(rama_nombre) |> View()

# 6. Presentar la tabla final agrupada
kable(
  tabla_agrupada,
  col.names = c("Rama de Actividad (CAENES)", "Ago-2025 (%)", "Ago-2026 (%)"),
  caption = "Participación del Empleo Femenino por Rama (Agrupando < 5% en 2026) - Región de Valparaíso",
  align = c("l", "r")
)

