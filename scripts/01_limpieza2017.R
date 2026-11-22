source("scripts/00_librerias.R")

#matriculados gen2012
Matriculados2017 <- read_dta("data/raw/2017_gen2012/Matriculados Lec_EFI2017.dta") %>% 
   filter(gen==2012)
write_dta(Matriculados2017, 'data/clean/Matriculados_gen2012.dta')

#muestra abandono (marco muestral) gen 2012
MuestraEFI2017 <- read_excel("data/raw/2017_gen2012/Muestra Lec_EFI2017.xlsx") %>% 
   filter(gen==2012)
write_dta(MuestraEFI2017, 'data/clean/Muestra_gen2012.dta')

#actividad original gen2012
activs_orig <- read_dta("data/raw/2017_gen2012/activs y matriculados/activs_orig.dta")
ci <- Matriculados2017$estci
activs_orig_gen2012 <- activs_orig %>% filter(estci%in%ci)
write_dta(activs_orig_gen2012, 'data/clean/activs_orig_gen2012.dta')

#formulario ingreso gen2012
i2012 <- read_csv("data/raw/2017_gen2012/FIngreso_carreras11_12_14/i2012.csv")
i2012 <- i2012 %>% filter(C_I%in%ci) %>% rename(ids=...1)
write_dta(i2012, 'data/clean/form_ingreso_gen2012.dta')

#encuesta gen2012
encuesta_EFI2017 <- read_excel("data/raw/2017_gen2012/encuesta_EFI2017.xls") %>% 
   filter(gen==2012)
write_dta(encuesta_EFI2017, 'data/clean/encuesta_gen2012.dta')

#limpieza actividad gen2012
act_gen2012 <- read_dta('data/clean/activs_orig_gen2012.dta')
#filtramos materias primer año
mat_anio1 <- c('E10','E11', 'MC10', '114A','128A','C10', 'A10', 'E20', 'MC20')
act_anio1_gen2012 <- act_gen2012 %>% filter(mat%in%mat_anio1)
write_dta(act_anio1_gen2012, 'data/clean/act_anio1_gen2012.dta')
#formato wide materias primer año
# 1) Limpiar base: sacar años previos a 2012 y borrar cursadas duplicadas con examen aprobado
act_limpia <- act_anio1_gen2012 %>%
   # Asegurar tipo fecha (ajustar formato si no es "YYYY-MM-DD")
   mutate(fecha = as.Date(fecha)) %>%
   # b) Detectar grupos con examen aprobado en esa misma fecha
   group_by(estci, mat, fecha) %>%
   mutate(hay_examen_aprob = any(tact == "E" & tgen == "A")) %>%
   ungroup() %>%
   # c) Borrar la fila de cursado cuando hay examen aprobado el mismo día
   filter(!(hay_examen_aprob & tact == "C")) %>%
   # d) Ya no necesitamos el flag
   select(-hay_examen_aprob)
#contar eventos
eventos <- act_limpia %>%
   mutate(anio = year(as.Date(fecha))) %>%
   count(estci, anio, name = "eventos") %>%
   filter(anio >= 2012, anio <= 2017) %>%
   pivot_wider(
      names_from = anio,
      names_prefix = "eventos_",
      values_from = eventos,
      values_fill = 0
   )%>%
   mutate(
      eventos_totales = eventos_2012 + eventos_2013 + eventos_2014 +
         eventos_2015 + eventos_2016 + eventos_2017
   )
# 2) RESUMEN por alumno–materia
# ============================================================
act_resumen <- act_limpia %>%
   mutate(
      anio = year(fecha)
   ) %>%
   group_by(estci, mat) %>%
   arrange(fecha, .by_group = TRUE) %>%
   summarise(
      # Fechas
      fecha_activ_inicial = first(fecha),
      fecha_activ_final   = last(fecha),
      
      # Intentos
      cursadas       = sum(tact == "C"),
      examenes       = sum(tact == "E"),
      intentos_total = n(),
      
      # Aprobación (sí/no)
      aprobada_act = as.integer(any(tgen == "A")),
      
      # Año de primera aprobación (sin warnings)
      anio_aprobada = {
         anios_ap <- anio[tgen == "A"]
         if (length(anios_ap) == 0) NA_integer_ else min(anios_ap, na.rm = TRUE)
      },
      
      # Nota con la que aprobó (primera A; debería ser única)
      nota_aprobacion = {
         notas_ap <- nota[tgen == "A"]
         if (length(notas_ap) == 0) NA_real_ else notas_ap[1]
      },
      
      .groups = "drop"
   )


# ============================================================
# 3) FORMATO ANCHO: una fila por estudiante, columnas por materia
# ============================================================
act_wide <- act_resumen %>%
   pivot_wider(
      id_cols    = estci,
      names_from = mat,
      values_from = c(
         fecha_activ_inicial,
         fecha_activ_final,
         cursadas,
         examenes,
         intentos_total,
         aprobada_act,
         anio_aprobada,
         nota_aprobacion
      ),
      names_glue = "{.value}_{mat}"
   )
act_wide <- act_wide %>% left_join(eventos, by='estci')
indicadores <- c(
   "aprobada_act",
   "anio_aprobada",
   "nota_aprobacion",
   "cursadas",
   "examenes",
   "intentos_total",
   "fecha_activ_inicial",
   "fecha_activ_final"
)

# 2) Orden deseado de materias
materias <- c("114A", "128A", "A10", "C10", "E10", "E20", "MC10", "MC20")

# 3) Construir vector de nombres en el orden indicador × materia
orden_cols <- c(
   "estci",
   'eventos_totales',
   'eventos_2012','eventos_2013','eventos_2014','eventos_2015',
   'eventos_2016','eventos_2017',
   unlist(lapply(materias, function(m) paste0(indicadores, "_", m)))
)

# 4) Nos quedamos solo con las columnas que existen y en ese orden
orden_cols <- intersect(orden_cols, names(act_wide))

act_wide_ordenado <- act_wide %>%
   select(all_of(orden_cols))
write_dta(act_wide_ordenado, 'data/clean/act_wide_gen2012.dta')


#datos abandono gen2012
MuestraEFI2017$v_abandono <- 1
ult_act_gen2012 <- act_gen2012 %>% group_by(estci) %>% 
   summarise(ult_fecha=max(as.Date(fecha)))
Datos_abandono_gen2012 <- Matriculados2017 %>% 
   left_join(MuestraEFI2017[,c('estci','v_abandono')],by='estci') %>% 
   mutate(v_abandono = ifelse(is.na(v_abandono),0,v_abandono)) %>% 
   left_join(ult_act_gen2012, by='estci')
#como si no tenia actividad no aparece, se borran
Datos_abandono_gen2012 <- Datos_abandono_gen2012 %>% 
   filter(!is.na(ult_fecha)) %>% 
   mutate(ult_anio = year(ult_fecha))
write_dta(Datos_abandono_gen2012, 'data/clean/Datos_abandono_gen2012.dta')




