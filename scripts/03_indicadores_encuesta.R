
source('scripts/00_librerias.R')

# ---- Funciones auxiliares ----------------------------------------------------

# Devuelve la columna de la encuesta 2024 cuyo nombre empieza con `prefijo` (y que
# contiene "[etiqueta" si la pregunta tiene sub-ítems). Falla si no es única, así un
# cambio de redacción en el cuestionario no pasa desapercibido.
obtener_col <- function(df, prefijo, etiqueta = NULL) {
  nm <- names(df)[startsWith(names(df), prefijo)]
  if (!is.null(etiqueta)) nm <- nm[str_detect(nm, fixed(paste0("[", etiqueta)))]
  stopifnot("la pregunta no se identifica de forma única" = length(nm) == 1)
  df[[nm]]
}

sin_tildes <- function(x) str_to_lower(stri_trans_general(x, "Latin-ASCII"))

# ==============================================================================
# LECTURA
# ==============================================================================

# ---- Encuesta 2024 (Contador Público, gen. 2019) -----------------------------
enc24 <- read_excel("data/raw/2024_gen2019/encuesta Cdor_EFI2024.xlsx") |> 
  mutate(id = row_number())

# saco la gente que contestó que no abandonó
enc24 <- enc24 |>
  filter(obtener_col(enc24, "1. ¿Consideras que abandonaste") == "Sí")

# ---- Encuesta 2017 (Lic. en Economía, gen. 2012) -----------------------------
enc17 <- read_excel("data/raw/2017_gen2012/encuesta_EFI2017.xls") |>
  mutate(across(where(is.character), ~ gsub("s�", "Si", .x)),
         p8 = gsub("complet�", "completo", p8)) |>
  filter(gen == 2012)

# ==============================================================================
# INDICADORES
# ==============================================================================

# ---- Indicador 1 - Motivo Personal / Institucional / Mixto -------------------

## 2024
# renombra variables a utilizar en el indicador
# (p4x = pregunta 4.a, ítem x; mismo orden que en el cuestionario)
motivos_24 <- c(
  p4a = "Motivos laborales",
  p4b = "Cambio a otra carrera en la UDELAR",
  p4c = "Cambio a la misma carrera fuera de la UDELAR",
  p4d = "Cambio a otros estudios terciarios",
  p4e = "Cambio a otros estudios no terciarios",
  p4f = "Lo que aprendías",
  p4g = "No te sentías lo suficientemente preparado",
  p4h = "Motivos familiares",
  p4i = "Problemas relacionados a la estructura curricular",
  p4j = "Problemas de salud",
  p4k = "Motivos económicos",
  p4l = "Distancia de la facultad",
  p4m = "Te diste cuenta que tu vocación era otra",
  p4n = "Falta de amigos",
  p4o = "Los horarios de los cursos",
  p4p = "Masividad de los grupos",
  p4q = "Ambiente institucional",
  p4r = "No te iba bien en Conceptos Contables",
  p4s = "Te ponías nerviosa/o",
  p4t = "Te preocupaba tener calificaciones bajas",
  p4u = "Tuviste dificultades de adaptación",
  p4v = "No encontraste buena disponibilidad",
  p4w = "Te fuiste de viaje"
)
for (v in names(motivos_24)) {
  enc24[[v]] <- ifelse(obtener_col(enc24, "4.a.", motivos_24[[v]]) == "Si", 1, 0)
}

# saco las personas que dijeron que abandonaron pero dejaron toda la pregunta 4.a
# en blanco: se toman como que no contestaron la encuesta
sin_respuesta <- rowSums(!is.na(enc24[names(motivos_24)])) == 0
cat("Excluidos por no responder la pregunta 4.a:", sum(sin_respuesta), "(se esperaban 2)\n")
enc24 <- enc24[!sin_respuesta, ] # id %in% c(53, 54)
cat("N 2024:", nrow(enc24), "(esperado 63)\n")

enc24 <- enc24 |>
  mutate(institucional = rowSums(across(c(p4c, p4i, p4o, p4p, p4q, p4v)), na.rm = T),
         personal = rowSums(across(c(p4a, p4b, p4d, p4e, p4f, p4g, p4h,
                                     p4j, p4k, p4l, p4m, p4n, p4r, p4s, p4t, p4u, p4w)),
                            na.rm = T)) |>
  mutate(institucional = ifelse(institucional > 0, 1, 0),
         personal = ifelse(personal > 0, 1, 0)) |>
  mutate(mixto = ifelse((personal == 1) & (institucional == 1), 1, 0))

## 2017
# renombra variables
enc17 <- enc17 |>
  mutate(p3_a = ifelse(p3_1 == 'Si', 1, 0),
         p3_b = ifelse(p3_2 == 'Si', 1, 0),
         p3_c = ifelse(p3_3 == 'Si', 1, 0),
         p3_d = ifelse(p3_4 == 'Si', 1, 0),
         p3_e = ifelse(p3_5 == 'Si', 1, 0),
         p3_f = ifelse(p3_6 == 'Si', 1, 0),
         p3_g = ifelse(p3_7 == 'Si', 1, 0),
         p3_h = ifelse(p3_8 == 'Si', 1, 0),
         p3_i = ifelse(p3_9 == 'Si', 1, 0),
         p3_j = ifelse(p3_10 == 'Si', 1, 0),
         p3_k = ifelse(p3_11 == 'Si', 1, 0),
         p3_l = ifelse(p3_12 == 'Si', 1, 0),
         p3_m = ifelse(p3_13 == 'Si', 1, 0),
         p3_n = ifelse(p3_14 == 'Si', 1, 0),
         p3_o = ifelse(p3_15 == 'Si', 1, 0),
         p3_p = ifelse(p3_16 == 'Si', 1, 0),
         p3_q = ifelse(p3_17 == 'Si', 1, 0))

# si la persona se cambia a una carrera privada pero similar -> institucional
carreras_similares <- c(
  'Administración de Empresas', 'Contador Público', 'Administración de empresas-EDA',
  'Economía, Universidad Católica', 'ECONOMIA ORT', 'Contador',
  'Administración de empresas', 'Administración Privada', 'Contador Publico',
  'Negocios Internacionales', 'Negocios Internacionales (ucudal)')

enc17 <- enc17 |>
  mutate(carreras = p6 %in% carreras_similares,
         p3_c2 = ifelse((p3_c == 1) & carreras, 1, 0),     # institucional
         p3_c3 = ifelse((p3_c == 1) & !carreras, 1, 0))    # personal

enc17 <- enc17 |>
  mutate(institucional = rowSums(across(c(p3_c2, p3_n, p3_o)), na.rm = T),
         personal = rowSums(across(c(p3_a, p3_b, p3_c3, p3_d, p3_e, p3_f, p3_g, p3_h, p3_i,
                                     p3_j, p3_k, p3_l, p3_m, p3_p, p3_q)), na.rm = T)) |>
  mutate(personal = ifelse(personal > 0, 1, 0),
         institucional = ifelse(institucional > 0, 1, 0)) |>
  mutate(mixto = ifelse((institucional == 1) & (personal == 1), 1, 0))

cat("N 2017:", nrow(enc17), "(esperado 80)\n")

# ---- Indicador 2 - Motivo intra-personal -------------------------------------

## 2024
enc24 <- enc24 |>
  mutate(economico_laboral = rowSums(across(c(p4a, p4k)), na.rm = T),
         familiar_salud = rowSums(across(c(p4h, p4j)), na.rm = T),
         vocacion_motivacion = rowSums(across(c(p4b, p4d, p4e, p4f, p4g, p4m)), na.rm = T),
         social_emocional = rowSums(across(c(p4n, p4r, p4s, p4t, p4u)), na.rm = T),
         distancia = p4l) |>
  mutate(economico_laboral = ifelse(economico_laboral == 0, 0, 1),
         familiar_salud = ifelse(familiar_salud == 0, 0, 1),
         vocacion_motivacion = ifelse(vocacion_motivacion == 0, 0, 1),
         social_emocional = ifelse(social_emocional == 0, 0, 1))

## 2017
enc17 <- enc17 |>
  mutate(economico_laboral = rowSums(across(c(p3_a, p3_j)), na.rm = T),
         familiar_salud = rowSums(across(c(p3_f, p3_i)), na.rm = T),
         vocacion_motivacion = rowSums(across(c(p3_b, p3_c3, p3_d, p3_e, p3_g, p3_h)), na.rm = T),
         social_emocional = rowSums(across(c(p3_l, p3_m, p3_p)), na.rm = T),
         distancia = p3_k) |>
  mutate(economico_laboral = ifelse(economico_laboral == 0, 0, 1),
         familiar_salud = ifelse(familiar_salud == 0, 0, 1),
         vocacion_motivacion = ifelse(vocacion_motivacion == 0, 0, 1),
         social_emocional = ifelse(social_emocional == 0, 0, 1))

# ---- Indicador 3 - Motivo Sistémico/Instruccional ----------------------------

## 2024
enc24 <- enc24 |>
  mutate(instruccional = rowSums(across(c(p4b, p4c, p4d)), na.rm = T)) |>
  mutate(instruccional = ifelse(instruccional == 0, 0, 1)) |>
  mutate(sistemico = ifelse(instruccional == 1, 0, 1))

## 2017
enc17 <- enc17 |>
  mutate(p5 = ifelse(p5 == 'Si', 1, 0),
         p7 = ifelse(p8 %in% c('completo', 'sigue asistiendo'), 1, 0)) |>
  mutate(instruccional = ifelse((p5 == 1) & (p7 == 1), 1, 0),
         sistemico = ifelse(instruccional == 0, 1, 0))

# ---- Indicador 4 - Integración social ----------------------------------------

## 2024
# renombrar variables
enc24 <- enc24 |>
  mutate(p21    = ifelse(obtener_col(enc24, "21.") == 'No', 0, 1),
         p23i   = ifelse(obtener_col(enc24, "23.", "¿Hiciste nuevos amigos") == 'No', 0, 1),
         p23ii  = ifelse(obtener_col(enc24, "23.", "¿Te vinculaste o participaste") == 'No', 0, 1),
         p23iii = ifelse(obtener_col(enc24, "23.", "¿Participaste en actividades sociales") == 'No', 0, 1),
         p23iv  = ifelse(obtener_col(enc24, "23.", "¿Tenías un grupo de estudio") == 'No', 0, 1))

enc24 <- enc24 |>
  mutate(integracion_social = rowSums(across(c(p21, p23i, p23ii, p23iii, p23iv)), na.rm = T))

## 2017
# renombrar variables
enc17 <- enc17 |>
  mutate(p28_a = ifelse(p28 == 'Si, a facultad', 1, 0),
         p29_a = ifelse(p29_1 == 'Si', 1, 0),
         p29_b = ifelse(p29_2 == 'Si', 1, 0),
         p29_c = ifelse(p29_3 == 'Si', 1, 0),
         p29_d = ifelse(p29_4 == 'Si', 1, 0))

enc17 <- enc17 |>
  mutate(integracion_social = rowSums(across(c(p28_a, p29_a, p29_b, p29_c, p29_d)), na.rm = T))

# ---- Indicador 5 - Vulnerabilidad académica ----------------------------------

## 2024
# p11: dijo que habría necesitado apoyo en alguna UC clave de primer año.
# La pregunta es de selección múltiple separada por ", ": se compara cada opción por
# igualdad exacta (así "Contabilidad General I" no captura "... II").
uc_con_apoyo <- c('Cálculo I', 'Cálculo I/B', 'Cálculo I/A', 'Contabilidad General I',
                  'Conceptos Contables',
                  'Introducción a la Microeconomía / Análisis de las Interacciones Económicas')

enc24 <- enc24 |>
  mutate(
    # asistencia: se comparan textos sin tildes ni mayúsculas para no depender de
    # cómo venga codificada la tilde
    p3  = ifelse(sin_tildes(obtener_col(enc24, "3. ")) %in%
                   c('nunca asisti', 'casi nunca asisti', 'asisti poco'), 1, 0),
    p11 = ifelse(map_lgl(str_split(obtener_col(enc24, "11."), ", "),
                         ~ any(.x %in% uc_con_apoyo)), 1, 0))

enc24 <- enc24 |>
  mutate(vulnerabilidad_academica = rowSums(across(c(p3, p4r, p4g, p11)), na.rm = T))

## 2017
uc_dificiles <- c('Cálculo I', 'Cálculo', 'Cálculo 1', 'Calculo 1', 'CALCULO',
                  'CALCULO 1', 'Intro ec', 'Intro a la Economía')

enc17 <- enc17 |>
  mutate(p35 = ifelse(p35 == 'no', 1, 0),
         p38 = ifelse((p38_1 %in% uc_dificiles) | (p38_2 %in% uc_dificiles), 1, 0),
         p39_c = ifelse(p39_3 == 'Si', 1, 0),
         p39_e = ifelse(p39_5 == 'Si', 1, 0))

enc17 <- enc17 |>
  mutate(vulnerabilidad_academica = rowSums(across(c(p35, p38, p39_c, p39_e)), na.rm = T))


# ==============================================================================
# TABLAS
# ==============================================================================

# ---- Género x carrera --------------------------------------------------------
# 2024: `mujer` viene en la hoja "muestra"
muestra24 <- read_excel("data/raw/2024_gen2019/Muestra Cdor_EFI2024.xls",
                        sheet = "muestra") |>
  select(numerodoc, mujer) |>
  distinct()

n_antes <- nrow(enc24)
enc24 <- left_join(enc24, muestra24, by = c("Ingresa la CI" = "numerodoc"))
stopifnot("el cruce con la muestra duplicó filas" = nrow(enc24) == n_antes)

# 2017: `Sexo` viene del archivo de formulario de ingreso
sexo17 <- read_dta("data/clean/form_ingreso_gen2012.dta") |> 
  select(C_I, Sexo) |>
  distinct()

n_antes <- nrow(enc17)
enc17 <- left_join(enc17, sexo17, by = c('estci' = 'C_I')) |>
  mutate(mujer = ifelse(Sexo == 'Mujer', 1, 0))
stopifnot("el cruce con actividades duplicó filas" = nrow(enc17) == n_antes)

(enc24$mujer |> table()) / nrow(enc24)
(enc17$mujer |> table()) / nrow(enc17)

# ---- Tipo de abandono x carrera ----------------------------------------------
# Proporción de abandono instruccional y sistémico
(enc24$instruccional |> sum()) / nrow(enc24)
(enc24$sistemico |> sum()) / nrow(enc24)
(enc17$instruccional |> sum(na.rm = T)) / nrow(enc17)
(enc17$sistemico |> sum(na.rm = T)) / nrow(enc17)

# ---- Tipo de abandono x género x carrera -------------------------------------
(enc24 |> filter(instruccional == 1) |> pull(mujer) |> table()) /
  (enc24 |> filter(instruccional == 1) |> nrow())
(enc24 |> filter(sistemico == 1) |> pull(mujer) |> table()) /
  (enc24 |> filter(sistemico == 1) |> nrow())

(enc17 |> filter(instruccional == 1) |> pull(mujer) |> table()) /
  (enc17 |> filter(instruccional == 1) |> nrow())
(enc17 |> filter(sistemico == 1) |> pull(mujer) |> table()) /
  (enc17 |> filter(sistemico == 1) |> nrow())

# ---- Motivo de abandono x carrera (disjuntos -> con mixto) -------------------
# Antes la segunda línea filtraba `personal==0 & instruccional==1` (variable del
# indicador 3 en vez del 1); acá es `institucional`, para que las tres sumen 100 %.
for (d in list(enc24, enc17)) {
  print(c(solo_personal      = nrow(filter(d, personal == 1 & institucional == 0)),
          solo_institucional = nrow(filter(d, personal == 0 & institucional == 1)),
          mixto              = nrow(filter(d, mixto == 1))) / nrow(d))
  # cuántos al menos personal / al menos institucional
  print(c(al_menos_personal      = nrow(filter(d, personal == 1)),
          al_menos_institucional = nrow(filter(d, institucional == 1))) / nrow(d))
}

# ---- Motivos institucionales: el más elegido ---------------------------------
enc24 |>
  summarise(p4c = sum(p4c, na.rm = T), p4i = sum(p4i, na.rm = T), p4o = sum(p4o, na.rm = T),
            p4p = sum(p4p, na.rm = T), p4q = sum(p4q, na.rm = T), p4v = sum(p4v, na.rm = T)) |>
  pivot_longer(everything(), names_to = "pregunta", values_to = "n") |>
  slice_max(n, n = 1, with_ties = FALSE) |>
  mutate(prop = n / nrow(enc24))
# p4p  Masividad de los grupos 46%

enc17 |>
  summarise(p3_c2 = sum(p3_c2, na.rm = T), p3_n = sum(p3_n, na.rm = T), p3_o = sum(p3_o, na.rm = T)) |>
  pivot_longer(everything(), names_to = "pregunta", values_to = "n") |>
  slice_max(n, n = 1, with_ties = FALSE) |>
  mutate(prop = n / nrow(enc17))
# p3_o Dejaste la carrera porque te desmotivó pasar de un centro educativo con
# grupos reducidos a una facultad con grupos de hasta 500   42%

# ---- La facultad podría haber hecho algo -------------------------------------
enc24 <- enc24 |>
  mutate(p9 = ifelse(obtener_col(enc24, "9.") == 'Sí', 1, 0))
sum(enc24$p9) / nrow(enc24)      # 30% dice sí

enc17 <- enc17 |>
  mutate(p14 = ifelse(p14 == 'Si', 1, 0))
sum(enc17$p14) / nrow(enc17)     # 29% dice sí

# ---- Motivos de abandono personal x carrera (no disjuntos: no suma 100%) -----
dimensiones <- c("economico_laboral", "familiar_salud", "vocacion_motivacion",
                 "social_emocional", "distancia")
colMeans(enc24[dimensiones])
colMeans(enc17[dimensiones])

# ---- Moda intrapersonal (ítem más elegido dentro de cada dimensión) ----------
colSums(enc24[c("p4a", "p4k")], na.rm = T)                                  # laborales
colSums(enc24[c("p4h", "p4j")], na.rm = T)                                  # familiares
colSums(enc24[c("p4b", "p4d", "p4e", "p4f", "p4g", "p4m")], na.rm = T)      # p4m vocación otra
colSums(enc24[c("p4n", "p4r", "p4s", "p4t", "p4u")], na.rm = T)             # p4u adaptación

colSums(enc17[c("p3_a", "p3_j")], na.rm = T)                                # laborales
colSums(enc17[c("p3_f", "p3_i")], na.rm = T)                                # familiares
colSums(enc17[c("p3_b", "p3_c3", "p3_d", "p3_e", "p3_g", "p3_h")], na.rm = T) # p3_b otras cosas
colSums(enc17[c("p3_l", "p3_m", "p3_p")], na.rm = T)                        # p3_l no a gusto

# ---- Integración social, vulnerabilidad académica x carrera ------------------
(enc24$integracion_social |> table()) / nrow(enc24)
(enc17$integracion_social |> table()) / nrow(enc17)
(enc24$vulnerabilidad_academica |> table()) / nrow(enc24)
(enc17$vulnerabilidad_academica |> table()) / nrow(enc17)

# ==============================================================================
# EXPORTAR RESULTADOS AGREGADOS  ->  resultados/indicadores_encuesta.csv
# ==============================================================================
# Una fila por (carrera, cohorte, indicador, categoria) con n, N y pct. De acá lee
# 06_graficos.Rmd, en lugar de tener los números escritos a mano.

inst_24 <- c("p4c", "p4i", "p4o", "p4p", "p4q", "p4v")
inst_17 <- c("p3_c2", "p3_n", "p3_o")

resumen <- function(d, inst_vars) {
  N <- nrow(d)
  bin <- function(vars, indicador)
    tibble(indicador = indicador, categoria = vars,
           n = map_int(vars, ~ sum(d[[.x]] == 1, na.rm = TRUE)), N = N)
  niv <- function(var, indicador, niveles)
    tibble(indicador = indicador, categoria = as.character(niveles),
           n = map_int(niveles, ~ sum(d[[var]] == .x, na.rm = TRUE)), N = N)
  excl <- case_when(d$mixto == 1                           ~ "mixto",
                    d$personal == 1 & d$institucional == 0 ~ "solo_personal",
                    d$personal == 0 & d$institucional == 1 ~ "solo_institucional",
                    TRUE                                    ~ "ninguno")
  bind_rows(
    bin(c("instruccional", "sistemico"), "tipo_abandono"),
    bin(c("personal", "institucional"),  "motivo_al_menos_uno"),
    tibble(indicador = "motivo_excluyente", categoria = names(table(excl)),
           n = as.integer(table(excl)), N = N),
    bin(dimensiones,                     "dimension_personal"),
    bin(inst_vars,                       "item_institucional"),
    niv("vulnerabilidad_academica", "vulnerabilidad_academica", 0:4),
    niv("integracion_social",       "integracion_social",       0:6),
    bin("p9_p14",                        "facultad_pudo_evitar"),
    bin("mujer",                         "composicion_sexo"),
    tibble(indicador = "mujer_entre_instruccional", categoria = "mujer",
           n = sum(d$mujer[d$instruccional == 1], na.rm = TRUE), N = sum(d$instruccional == 1)),
    tibble(indicador = "mujer_entre_sistemico", categoria = "mujer",
           n = sum(d$mujer[d$sistemico == 1], na.rm = TRUE), N = sum(d$sistemico == 1))
  )
}

enc24$p9_p14 <- enc24$p9
enc17$p9_p14 <- enc17$p14

indicadores <- bind_rows(
  resumen(enc24, inst_24) |> mutate(carrera = "Contador Público", cohorte = 2019, .before = 1),
  resumen(enc17, inst_17) |> mutate(carrera = "Lic. en Economía", cohorte = 2012, .before = 1)
) |>
  mutate(pct = round(100 * n / N, 2))

write_csv(indicadores, "resultados/indicadores_encuesta.csv")

