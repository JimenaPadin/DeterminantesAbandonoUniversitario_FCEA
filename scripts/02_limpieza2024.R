source("scripts/00_librerias.R")

act_gen2019 <- read_dta("data/raw/2025_complemento/Actividades_cdor_25.dta")


cod_mat <- tibble::tibble(
   nombre_mat_original = c(
      "INTRODUCCIÓN_A_LA_MICROECONOMÍA",
      "ANÁLISIS_DE_LAS_INTERACCIONES_ECONÓMICAS", 
      "CÁLCULO_I",
      "CÁLCULO_I/A",
      "CÁLCULO_I-B",
      "CONCEPTOS_CONTABLES",
      "ADMINISTRACIÓN_Y_GESTIÓN_DE_LAS_ORGANIZACIONES_I",
      "ECONOMÍA_DESCRIPTIVA",
      "CONTABILIDAD_GENERAL_I"
   ),
   cod_mat = c("E10","E11","MC10","114A","128A",
               "C10","A10","E20","C20")
)

act_gen2019 <- act_gen2019 %>%
   mutate(nombre_mat = gsub(" ", "_", nombre_mat)) %>% 
   left_join(cod_mat,
             by = c("nombre_mat" = "nombre_mat_original")) %>%
   mutate(nombre_mat = cod_mat) %>%
   select(-cod_mat)

mat_anio1 <- c("E10","E11","MC10","114A","128A",
               "C10","A10","E20","C20")
act_anio1_gen2019 <- act_gen2019 %>% filter(nombre_mat%in%mat_anio1)

#contar eventos
eventos <- act_anio1_gen2019 %>%
   mutate(anio = year(as.Date(fecha_activ))) %>%
   count(numerodoc, anio, name = "eventos") %>%
   filter(anio >= 2019, anio <= 2023) %>%
   pivot_wider(
      names_from = anio,
      names_prefix = "eventos_",
      values_from = eventos,
      values_fill = 0
   )%>%
   mutate(
      eventos_totales = eventos_2019 + eventos_2020 + 
         eventos_2021 +
         eventos_2022 + eventos_2023
   )

act_resumen <- act_anio1_gen2019 %>%
   mutate(
      anio = year(fecha_activ)
   ) %>%
   group_by(numerodoc, nombre_mat) %>%
   arrange(fecha_activ, .by_group = TRUE) %>%
   summarise(
      # Fechas
      fecha_activ_inicial = first(fecha_activ),
      fecha_activ_final   = last(fecha_activ),
      
      # Intentos
      intentos_total = n(),
      
      # Aprobación (sí/no)
      aprobada_act = as.integer(any(aprobada == 1)),
      
      # Año de primera aprobación (sin warnings)
      anio_aprobada = {
         anios_ap <- anio[aprobada == 1]
         if (length(anios_ap) == 0) NA_integer_ else min(anios_ap, na.rm = TRUE)
      },
      
      # Nota con la que aprobó (primera A; debería ser única)
      nota_aprobacion = {
         notas_ap <- nota[aprobada == 1]
         if (length(notas_ap) == 0) NA_real_ else notas_ap[1]
      },
      
      .groups = "drop"
   )

act_wide <- act_resumen %>%
   pivot_wider(
      id_cols    = numerodoc,
      names_from = nombre_mat,
      values_from = c(
         fecha_activ_inicial,
         fecha_activ_final,
         intentos_total,
         aprobada_act,
         anio_aprobada,
         nota_aprobacion
      ),
      names_glue = "{.value}_{nombre_mat}"
   )
act_wide <- act_wide %>% left_join(eventos, by='numerodoc')
indicadores <- c(
   "aprobada_act",
   "anio_aprobada",
   "nota_aprobacion",
   "intentos_total",
   "fecha_activ_inicial",
   "fecha_activ_final"
)

# 2) Orden deseado de materias
materias <- c("E10","E11","MC10","114A","128A",
              "C10","A10","E20","C20")

# 3) Construir vector de nombres en el orden indicador × materia
orden_cols <- c(
   "numerodoc",
   'eventos_totales',
   'eventos_2019','eventos_2020','eventos_2021','eventos_2022',
   'eventos_2023',
   unlist(lapply(materias, function(m) paste0(indicadores, "_", m)))
)

act_wide_ordenado <- act_wide %>%
   select(all_of(orden_cols))

write_dta(act_wide_ordenado, 'data/clean/act_wide_gen2019.dta')

