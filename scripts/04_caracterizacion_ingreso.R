library(tidyverse)
library(haven)
library(here)

# Compara un valor obtenido con el publicado e imprime OK / REVISAR
chequear <- function(obtenido, esperado, que, tol = 0) {
  ok <- isTRUE(abs(obtenido - esperado) <= tol)
  cat(sprintf("[%s] %s: obtenido %s, esperado %s\n",
              if (ok) "OK" else "REVISAR", que, obtenido, esperado))
}

como_texto <- function(x) as.character(haven::as_factor(x))


# ==============================================================================
# PARTE 1. Lectura de las bases
# ==============================================================================

# ---- Economía 2012 -----------------------------------------------------------
# Se parte de TODOS los matriculados (no de Datos_abandono_gen2012, que descarta
# a quienes nunca tuvieron actividad).
mat12 <- read_dta("data/clean/Matriculados_gen2012.dta", encoding = "latin1") %>%
  distinct(estci)

aband12 <- read_dta("data/clean/Muestra_gen2012.dta", encoding = "latin1") %>%
  distinct(estci) %>%
  mutate(abandono = 1L)

form12 <- read_dta("data/clean/form_ingreso_gen2012.dta", encoding = "latin1") %>%
  filter(Carrera == 12) %>%
  distinct(C_I, .keep_all = TRUE)

c12 <- mat12 %>%
  left_join(aband12, by = "estci") %>%
  mutate(abandono = coalesce(abandono, 0L)) %>%
  left_join(form12, by = c("estci" = "C_I")) %>%
  mutate(carrera = "Lic. en Economía", cohorte = 2012L)

chequear(nrow(c12), 343, "inscriptos 2012")
chequear(sum(c12$abandono), 111, "abandonos 2012")
chequear(sum(!is.na(c12$Carrera)), nrow(c12), "inscriptos 2012 con formulario")

# ---- Contador Público 2019 ---------------------------------------------------
form24 <- read_dta("data/raw/2024_gen2019/TODOS Cdor form_ingreso_ EFI2024.dta") %>%
  distinct(numerodoc, .keep_all = TRUE)
chequear(nrow(form24), 1280, "inscriptos 2019 en el formulario")

aband24 <- read_dta("data/raw/2025_complemento/Datos abandono_25.dta") %>%
  filter(is.na(borrar_persona) | borrar_persona != 1) %>%
  transmute(numerodoc, abandono = v_abandono) %>%
  distinct()


if (anyDuplicated(aband24$numerodoc) > 0)
  stop("Hay estudiantes con más de un valor de v_abandono en Datos abandono_25.",
       call. = FALSE)

c24 <- form24 %>%
  left_join(aband24, by = "numerodoc") %>%
  mutate(abandono = coalesce(as.integer(abandono), 0L),
         carrera = "Contador Público", cohorte = 2019L)

chequear(nrow(c24), 1280, "inscriptos 2019")
chequear(sum(c24$abandono), 375, "abandonos 2019")


# ==============================================================================
# PARTE 2. Recodificación a variables armonizadas
# ==============================================================================
objetivo_vars <- c("sexo", "procedencia", "vive_con_padres", "vive_con_ambos_padres",
                   "educ_media", "transicion_inmediata", "trabaja_ingreso",
                   "busca_empleo", "hijos", "extranjero")

# ---- Economía 2012 -----------------------------------------------------------
# sexto_sector no tiene etiquetas. Según el Anexo 6 (públicos 46 %, privados 54 %),
# el código 2 es "Público".
CODIGO_PUBLICO_2012 <- 2

res_txt  <- como_texto(c12$res_lug)
trab_txt <- como_texto(c12$enc_trab)
sit_txt  <- str_to_lower(como_texto(c12$sit_no_t))
nac_txt  <- como_texto(c12$nac_lug)
paises_fuera <- c("Europa", "Brasil", "Argentina", "Chile")

c12 <- c12 %>% mutate(
  sexo = case_when(como_texto(Sexo) == "Mujer"  ~ "Femenino",
                   como_texto(Sexo) == "Hombre" ~ "Masculino"),
  procedencia = case_when(is.na(res_lug) ~ NA_character_,
                          res_txt == "Montevideo" ~ "Montevideo",
                          res_txt == "Canelones"  ~ "Canelones",
                          TRUE ~ "Interior"),
  vive_con_padres = case_when(is.na(padres) ~ NA_character_,
                              as.numeric(padres) > 0 ~ "Sí", TRUE ~ "No"),
  vive_con_ambos_padres = case_when(is.na(padres) ~ NA_character_,
                                    as.numeric(padres) == 2 ~ "Sí", TRUE ~ "No"),
  educ_media = case_when(is.na(sexto_sector) ~ NA_character_,
                         as.numeric(sexto_sector) == CODIGO_PUBLICO_2012 ~ "Público",
                         TRUE ~ "Privado"),             # "Exterior": sin casos en 2012
  transicion_inmediata = case_when(is.na(egre_sexto) ~ NA_character_,
                                   as.numeric(as.character(egre_sexto)) == 2011 ~ "Sí",
                                   TRUE ~ "No"),
  trabaja_ingreso = case_when(is.na(enc_trab) ~ NA_character_,
                              trab_txt == "No trabaja" ~ "No trabaja",
                              TRUE ~ "Trabaja"),
  # sit_no_t solo se completa para quienes no trabajan (la categoría sin etiqueta
  # corresponde a quienes ya trabajan)
  # "No busca" = "No trabajó y no busca trabajo" (174 casos, 54 % como en el artículo);
  # quienes trabajaron antes y no buscan (11) van aparte
  busca_empleo = case_when(is.na(enc_trab) ~ NA_character_,
                           trab_txt != "No trabaja" ~ "Ya trabaja",
                           str_detect(sit_txt, "^no trabaj.* no busca") ~ "No busca",
                           str_detect(sit_txt, "^trabaj.* no busca") ~ "Trabajó antes, no busca",
                           TRUE ~ "Busca"),
  hijos = case_when(is.na(hij_num) ~ NA_character_,
                    as.numeric(hij_num) > 0 ~ "Sí", TRUE ~ "No"),
  extranjero = case_when(is.na(nac_lug) ~ NA_character_,
                         nac_txt %in% paises_fuera | str_starts(nac_txt, "Per") ~ "Sí",
                         TRUE ~ "No")
)

# ---- Contador Público 2019 ---------------------------------------------------
sin_tildes <- function(x) str_to_lower(stringi::stri_trans_general(x, "Latin-ASCII"))

dept_a_grupo <- function(x) {
  txt <- str_to_upper(como_texto(x))
  case_when(is.na(x) ~ NA_character_,
            str_detect(txt, "MONTEVIDEO") | txt == "1" ~ "Montevideo",
            str_detect(txt, "CANELONES")  | txt == "3" ~ "Canelones",
            TRUE ~ "Interior")
}

deptos_uy <- c("Montevideo", "Artigas", "Canelones", "Cerro Largo", "Colonia", "Durazno",
               "Flores", "Florida", "Lavalleja", "Maldonado", "Paysandú", "Río Negro",
               "Rivera", "Rocha", "Salto", "San José", "Soriano", "Tacuarembó",
               "Treinta y Tres")

# Educación media cursada fuera de Uruguay: el "departamento" del liceo no es uno de los 19
media_fuera <- !is.na(c24$ep_14_4_1_fe) &
  !(sin_tildes(como_texto(c24$ep_14_4_1_fe)) %in% sin_tildes(deptos_uy))
inst_txt <- str_to_lower(como_texto(c24$ep_14_4_2_fe))
trab_24  <- sin_tildes(como_texto(c24$t_15_fe))
pais_mas_frecuente <- names(which.max(table(c24$cod_paisnacimiento_fe)))   # 858 = Uruguay

c24 <- c24 %>% mutate(
  sexo = case_when(como_texto(sexo_fe) == "F" ~ "Femenino",
                   como_texto(sexo_fe) == "M" ~ "Masculino"),
  procedencia = dept_a_grupo(sd_1_1_fe),             # residencia actual
  vive_con_padres = case_when(is.na(sd_11_1_fe) ~ NA_character_,
                              as.numeric(sd_11_1_fe) > 0 ~ "Sí", TRUE ~ "No"),
  vive_con_ambos_padres = case_when(is.na(sd_11_1_fe) ~ NA_character_,
                                    as.numeric(sd_11_1_fe) == 2 ~ "Sí", TRUE ~ "No"),
  educ_media = case_when(media_fuera ~ "Exterior",
                         is.na(ep_14_4_2_fe) ~ NA_character_,
                         str_detect(inst_txt, "p[uú]blic") ~ "Público",
                         str_detect(inst_txt, "privad") ~ "Privado"),
  transicion_inmediata = case_when(is.na(ep_14_4_3_fe) ~ NA_character_,
                                   as.numeric(ep_14_4_3_fe) == 2018 ~ "Sí", TRUE ~ "No"),
  trabaja_ingreso = case_when(is.na(t_15_fe) ~ NA_character_,
                              trab_24 == "no" ~ "No trabaja",
                              trab_24 == "si" ~ "Trabaja"),
  busca_empleo = case_when(is.na(t_16_fe) ~ NA_character_,
                           sin_tildes(como_texto(t_16_fe)) == "si" ~ "Busca",
                           TRUE ~ "No busca"),
  hijos = case_when(is.na(sd_11_4_fe) ~ NA_character_,
                    as.numeric(sd_11_4_fe) > 0 ~ "Sí", TRUE ~ "No"),
  extranjero = case_when(is.na(cod_paisnacimiento_fe) ~ NA_character_,
                         as.character(cod_paisnacimiento_fe) != pais_mas_frecuente ~ "Sí",
                         TRUE ~ "No")
)


# ==============================================================================
# PARTE 3. Tablas agregadas
# ==============================================================================
largo <- bind_rows(c12, c24) %>%
  select(carrera, cohorte, abandono, all_of(objetivo_vars)) %>%
  pivot_longer(all_of(objetivo_vars), names_to = "variable", values_to = "categoria") %>%
  filter(!is.na(categoria))

pendientes <- expand_grid(cohorte = c(2012L, 2019L), variable = objetivo_vars) %>%
  anti_join(distinct(largo, cohorte, variable), by = c("cohorte", "variable"))
if (nrow(pendientes) > 0) {
  cat("\nVariables sin ningún valor recodificado:\n")
  print(pendientes, n = Inf)
}

# Composición de cada grupo (p. ej. % de mujeres entre quienes abandonan)
composicion <- bind_rows(
  largo %>% mutate(grupo = "Total"),
  largo %>% filter(abandono == 1L) %>% mutate(grupo = "Abandono"),
  largo %>% filter(abandono == 0L) %>% mutate(grupo = "No abandono")
) %>%
  count(carrera, cohorte, grupo, variable, categoria, name = "n") %>%
  group_by(carrera, cohorte, grupo, variable) %>%
  mutate(n_validos = sum(n), pct = 100 * n / n_validos) %>%
  ungroup()

# Tasa de abandono dentro de cada categoría (p. ej. % de abandono entre varones)
tasas <- largo %>%
  group_by(carrera, cohorte, variable, categoria) %>%
  summarise(n = n(), n_abandono = sum(abandono),
            tasa_abandono = 100 * mean(abandono), .groups = "drop")


# ==============================================================================
# PARTE 4. Exportación
# ==============================================================================
# Se suprimen celdas con 1 a 4 casos. Revisar a mano que la celda complementaria
# (la que se obtiene por resta) no revele el dato suprimido.
composicion_pub <- composicion %>%
  mutate(suprimido = n > 0L & n < 5L,
         n   = replace(n,   suprimido, NA),
         pct = replace(pct, suprimido, NA)) %>%
  select(-suprimido)

tasas_pub <- tasas %>%
  mutate(suprimido = n < 5L | (n_abandono > 0L & n_abandono < 5L),
         n             = replace(n,             suprimido, NA),
         n_abandono    = replace(n_abandono,    suprimido, NA),
         tasa_abandono = replace(tasa_abandono, suprimido, NA)) %>%
  select(-suprimido)

write_csv(composicion_pub, "resultados/caracterizacion_ingreso.csv")
write_csv(tasas_pub, "resultados/caracterizacion_tasa_abandono.csv")

control_pub <- control %>%
  mutate(suprimido = !is.na(n) & n < 5L,
         obtenido = replace(obtenido, suprimido, NA),
         dif      = replace(dif,      suprimido, NA),
         estado   = if_else(suprimido, "SUPRIMIDO (n < 5)", estado)) %>%
  select(-n, -suprimido)
write_csv(control_pub,     here("resultados", "caracterizacion_control.csv"))
