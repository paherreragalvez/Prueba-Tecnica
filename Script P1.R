# https://notebook.google.com/notebook/097d4f65-b808-4cc9-9461-176d6db6fba4

# library(tidyr) # Para funciones de limpieza como replace_na
# library(arrow) # Para guardar en formato Parquet
# library(haven)
# library(ggplot2)
# library(scales) # Para normalizar variables (para clusters)
# library(skimr) # Compact and Flexible Summaries of Data

rm(list = ls())
library(readr)
library(dplyr)
library(srvyr)

# Importar bases de datos.

ene_0825 <- read_csv2("datos/ene-2025-08-jas.csv")
ene_0826 <- read_csv2("datos/ene-2026-08-jas.csv")

# Definición global para el tratamiento de estratos con una sola PSU (lonely PSUs)
options(survey.lonely.psu = "certainty")

# Diseños muestrales
ene_diseno25 <- ene_0825 |>
  as_survey_design(
    ids = conglomerado,
    strata = estrato,
    weights = fact_cal,
    nest = TRUE)

ene_diseno26 <- ene_0826 |>
  as_survey_design(
    ids = conglomerado,
    strata = estrato,
    weights = fact_cal,
    nest = TRUE)

# Filtros diseños muestrales

f_ene_diseno25 <- ene_diseno25 |> filter(
  region == 5,
  sexo == 2,
  edad >= 15)

f_ene_diseno26 <- ene_diseno26 |> filter(
  region == 5,
  sexo == 2,
  edad >= 15)

# PREGUNTA 1. ###############################

# a.	la tasa de desocupación

td_25 <- f_ene_diseno25 |> summarise(
  tasa_desocupacion = survey_ratio(
    numerator = cae_especifico %in% 8:9,  # Desocupadas (Cesantes + BTPV)
    denominator = cae_especifico %in% 1:9, # Fuerza de Trabajo femenina
    vartype = c("se", "cv"),
    na.rm = TRUE
    ) * 100
    )

print(td_25)

td_26 <- f_ene_diseno26 |> summarise(
  tasa_desocupacion = survey_ratio(
    numerator = cae_especifico %in% 8:9,  # Desocupadas (Cesantes + BTPV)
    denominator = cae_especifico %in% 1:9, # Fuerza de Trabajo femenina
    vartype = c("se", "cv"),
    na.rm = TRUE
    ) * 100
    )

print(td_26)

# b.	la tasa de participación,

tp_25 <- f_ene_diseno25 |>
  summarise(
    tasa_participacion = survey_ratio(
      numerator = cae_especifico %in% 1:9,   # Fuerza de Trabajo (FT)
      denominator = TRUE,                    # PET (edad >= 15 ya filtrada)
      vartype = c("se", "cv"),
      na.rm = TRUE
    ) * 100
  )

print(tp_25)

tp_26 <- f_ene_diseno26 |>
  summarise(
    tasa_participacion = survey_ratio(
      numerator = cae_especifico %in% 1:9,   # Fuerza de Trabajo (FT)
      denominator = TRUE,                    # PET (edad >= 15 ya filtrada)
      vartype = c("se", "cv"),
      na.rm = TRUE
    ) * 100
  )

print(tp_26)

# c.	la tasa de ocupación y

to_25 <- f_ene_diseno25 |>
  summarise(
    tasa_ocupacion = survey_ratio(
      numerator = cae_especifico %in% 1:7,   # Ocupados (O)
      denominator = TRUE,                    # PET (edad >= 15 ya filtrada)
      vartype = c("se", "cv"),
      na.rm = TRUE
    ) * 100
  )

print(to_25)

to_26 <- f_ene_diseno26 |>
  summarise(
    tasa_ocupacion = survey_ratio(
      numerator = cae_especifico %in% 1:7,   # Ocupados (O)
      denominator = TRUE,                    # PET (edad >= 15 ya filtrada)
      vartype = c("se", "cv"),
      na.rm = TRUE
    ) * 100
  )

print(to_26)

# d.	la tasa de ocupación informal.

toi_25 <- f_ene_diseno25 |>
  # Se acota la subpoblación únicamente a los Ocupados
  filter(cae_especifico %in% 1:7) |>
  summarise(
    tasa_ocupacion_informal = survey_ratio(
      numerator = ocup_form == 2,            # Ocupados Informales (OI)
      denominator = TRUE,                    # Total de Ocupados
      vartype = c("se", "cv"),
      na.rm = TRUE
    ) * 100
  )

print(toi_25)

toi_26 <- f_ene_diseno26 |>
  # Se acota la subpoblación únicamente a los Ocupados
  filter(cae_especifico %in% 1:7) |>
  summarise(
    tasa_ocupacion_informal = survey_ratio(
      numerator = ocup_form == 2,            # Ocupados Informales (OI)
      denominator = TRUE,                    # Total de Ocupados
      vartype = c("se", "cv"),
      na.rm = TRUE
    ) * 100
  )

print(toi_26)

# Valores

print(td_25)
print(td_26)

print(tp_25)
print(tp_26)

print(to_25)
print(to_26)

print(toi_25)
print(toi_26)

# PREGUNTA 2. ###############################

# Cálculo Fuerza de Trabajo (FT) femenina

ft25 <- f_ene_diseno25 %>%
  summarise(enroll_tot = survey_total(cae_especifico %in% 1:9))

ft26 <- f_ene_diseno26 %>%
  summarise(enroll_tot = survey_total(cae_especifico %in% 1:9))

# Cálculo Ocupadas femenina

#
o25 <- f_ene_diseno25 %>%
  summarise(enroll_tot = survey_total(cae_especifico %in% 1:7))

o26 <- f_ene_diseno26 %>%
  summarise(enroll_tot = survey_total(cae_especifico %in% 1:7))

# Cálculo Desocupadas femenina

d25 <- f_ene_diseno25 %>%
  summarise(enroll_tot = survey_total(cae_especifico %in% 8:9))

d26 <- f_ene_diseno26 %>%
  summarise(enroll_tot = survey_total(cae_especifico %in% 8:9))

#
ft26-ft25
o26-o25
d26-d25

# PREGUNTA 3. ###############################

#3.	Indica si el cambio en la tasa de desocupación es estadísticamente distinto de cero y explica cómo lo determinaste.

# Extraer valores y errores estándar
val_25 <- td_25$tasa_desocupacion
se_25  <- td_25$tasa_desocupacion_se # (revisa el nombre exacto de la columna de error estándar generada por survey_ratio)

val_26 <- td_26$tasa_desocupacion
se_26  <- td_26$tasa_desocupacion_se

# 1. Calcular la diferencia
diff_tasa <- val_26 - val_25

# 2. Error estándar de la diferencia
se_diff <- sqrt(se_25^2 + se_26^2)

# 3. Estadístico t
t_stat <- diff_tasa / se_diff

# 4. Grados de libertad aproximados o valor p (asumiendo distribución normal estándar para muestras grandes de ENE)
p_value <- 2 * (1 - pnorm(abs(t_stat)))

cat("Diferencia:", diff_tasa, "\n")
cat("Estadístico t:", t_stat, "\n")
cat("Valor p:", p_value, "\n")

if(p_value < 0.05) {
  print("El cambio en la tasa de desocupación es estadísticamente SIGNIFICATIVO al 95% de confianza.")
} else {
  print("El cambio NO es estadísticamente significativo.")
}
