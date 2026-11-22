source('scripts/00_librerias.R')

# ---- Ajustar si hace falta (ver names() de cada base) ------------------------
id_2012          <- "estci"         # cédula en act_wide_gen2012 y datos_abandono_gen2012
abandono_2012    <- "v_abandono"    # 1/0 en datos_abandono_gen2012
id_2019          <- "numerodoc"     # cédula en act_wide_gen2019 y en la base de abandono
abandono_2019    <- "v_abandono"     # 1/0 en la base de abandono 2019 (verificar nombre)

# ---- Funciones auxiliares ----------------------------------------------------

# Falla con un mensaje útil si faltan columnas, mostrando las que sí hay
exigir <- function(df, cols, que) {
  faltan <- setdiff(cols, names(df))
  if (length(faltan) > 0)
    stop("En ", que, " faltan las columnas: ", paste(faltan, collapse = ", "),
         "\nColumnas disponibles: ", paste(names(df), collapse = ", "), call. = FALSE)
}

# Tasa de abandono entre quienes aprobaron / no aprobaron la UC.
#   df      base con una fila por estudiante, columna `abandono` (0/1) y las columnas
#           intentos_total_<codigo> y aprobada_act_<codigo> de cada código de la UC
#   codigos códigos de la UC (varios si cambió de código entre planes)
tasa_abandono_uc <- function(df, uc, codigos, carrera, cohorte) {
  cols_int <- paste0("intentos_total_", codigos)
  cols_ap  <- paste0("aprobada_act_", codigos)
  exigir(df, c(cols_int, cols_ap), paste("act_wide", cohorte, "-", uc))
  
  df %>%
    mutate(curso  = rowSums(pick(all_of(cols_int)), na.rm = TRUE) > 0,
           aprobo = rowSums(pick(all_of(cols_ap)),  na.rm = TRUE) > 0) %>%
    filter(curso) %>%
    group_by(aprobacion = if_else(aprobo, "Aprobación", "No aprobación")) %>%
    summarise(n = n(),
              n_abandono = sum(v_abandono, na.rm = T),
              tasa_abandono = round(mean(v_abandono, na.rm=T), 4),
              .groups = "drop") %>%
    mutate(carrera = carrera, cohorte = cohorte, uc = uc, .before = 1)
}

# ==============================================================================
# LECTURA
# ==============================================================================

# ---- Lic. en Economía, gen. 2012 ---------------------------------------------
act12 <- read_dta("data/clean/act_wide_gen2012.dta")
aband12 <- read_dta("data/clean/datos_abandono_gen2012.dta")

exigir(act12,   id_2012,                    "act_wide_gen2012")
exigir(aband12, c(id_2012, abandono_2012),  "datos_abandono_gen2012")

# datos_abandono_gen2012 ya descarta a quienes nunca tuvieron actividad
act12 <- act12 %>%
  inner_join(aband12 %>% select(all_of(id_2012), abandono = all_of(abandono_2012)) %>%
               distinct(),
             by = id_2012)

# ---- Contador Público, gen. 2019 ---------------------------------------------
act19 <- read_dta("data/clean/act_wide_gen2019.dta")
aband19 <- read_dta("data/raw/2025_complemento/Datos abandono_25.dta")

exigir(act19,   id_2019,                    "act_wide_gen2019")
exigir(aband19, c(id_2019, abandono_2019),  "abandono_gen2019")

act19 <- act19 %>%
  inner_join(aband19 %>% select(all_of(id_2019), v_abandono = all_of(abandono_2019)) %>%
               distinct(),
             by = id_2019)

cat("Estudiantes con actividad: 2012 =", nrow(act12), " 2019 =", nrow(act19), "\n")
cat("Abandono global:  2012 =", round(100 * mean(act12$abandono), 1), "%  ",
    "2019 =", round(100 * mean(act19$v_abandono), 1), "%\n")

# ==============================================================================
# TASAS DE ABANDONO POR UC
# ==============================================================================

tasas <- bind_rows(
  # Lic. en Economía 2012
  tasa_abandono_uc(act12, "Intro. a la Microeconomía", "E10",                     "Lic. en Economía", 2012),
  tasa_abandono_uc(act12, "Economía Descriptiva",      "E20",                     "Lic. en Economía", 2012),
  tasa_abandono_uc(act12, "Cálculo",                   c("MC10", "114A", "128A"), "Lic. en Economía", 2012),
  # Contador Público 2019
  tasa_abandono_uc(act19, "Conceptos Contables",       "C10",                     "Contador Público", 2019),
  tasa_abandono_uc(act19, "Contabilidad General I",    "C20",                     "Contador Público", 2019),
  tasa_abandono_uc(act19, "Cálculo",                   c("MC10", "114A", "128A"), "Contador Público", 2019)
)

print(tasas, n = Inf)

# Brecha entre quienes no aprobaron y quienes aprobaron (puntos porcentuales)
brecha <- tasas %>%
  select(carrera, cohorte, uc, aprobacion, tasa_abandono) %>%
  pivot_wider(names_from = aprobacion, values_from = tasa_abandono) %>%
  mutate(brecha_pp = round(100 * (`No aprobación` - `Aprobación`)))
print(brecha)

# ==============================================================================
# EXPORTAR  ->  resultados/tasas_abandono_uc.csv
# ==============================================================================

write_csv(tasas, "resultados/tasas_abandono_uc.csv")

