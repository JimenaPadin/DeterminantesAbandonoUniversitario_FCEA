# Abandono universitario en FCEA (UdelaR): Lic. en Economía 2012 y Contador Público 2019

Análisis comparativo del abandono en dos generaciones de la Facultad de Ciencias Económicas y de Administración (FCEA, Universidad de la República, Uruguay):

- **Licenciatura en Economía, generación 2012** (encuesta a quienes abandonaron, 2017).
- **Contador Público, generación 2019** (encuesta a quienes abandonaron, 2024).

Combina registros administrativos de trayectoria académica, formularios de ingreso y encuestas a estudiantes que abandonaron, para describir tipos y motivos de abandono, y su relación con la integración social, la vulnerabilidad académica y la aprobación de unidades curriculares (UC) clave de primer año.

El trabajo se desarrolló en el curso *EFI: abandono universitario* (2025) y se publicó como documento de investigación:

> Erosa, C. y Padín, J. (2026). *El abandono universitario en FCEA (UdelaR): Un estudio comparativo entre la Licenciatura en Economía y Contador Público*. Documento de Investigación Estudiantil. Universidad de la República, Facultad de Ciencias Económicas y de Administración. Coordinadora: A. Machado. <!-- TODO: agregar enlace o DOI si existe -->

---

## Disponibilidad de los datos

**Los datos individuales son privados y no están incluidos en este repositorio.** Provienen del Espacio de Formación Integral (EFI) y de la Dirección General de Planeamiento Universitario (DGPLAN) de la Universidad de la República (registros administrativos de actividades y resultados académicos, formularios de ingreso y encuestas), y contienen información personal de estudiantes.

Tal como se declara en el artículo, los conjuntos de datos anonimizados están disponibles a solicitud, a través del equipo de investigación o de la institución.

Lo que sí se publica:

- **El código completo**, para auditar y reproducir el procesamiento si se cuenta con las bases.
- **Las tablas agregadas de resultados** (`resultados/`), de las que salen las figuras. No contienen datos individuales.

Para correr el código se necesitan las bases originales ubicadas como se describe en [Estructura](#estructura-del-repositorio). Quien obtenga acceso autorizado a ellas puede reproducir todo desde cero.

---

## Estructura del repositorio

```
.
├── README.md
├── scripts/                         scripts numerados, en orden de ejecución
│   ├── 01_limpieza_gen2012.R       limpieza Economía 2012 (actividades, abandono, formulario)
│   ├── 02_limpieza_gen2019.R       limpieza Contador Público 2019
│   ├── 03_indicadores_encuesta.R   indicadores de las encuestas -> resultados/
│   ├── 04_caracterizacion_ingreso.R  caracterización al ingreso (formularios) -> resultados/
│   ├── 05_aprobacion_abandono.R    tasa de abandono según aprobación de UC -> resultados/
│   └── 06_graficos.Rmd             figuras 1 a 5, leyendo solo resultados/
├── resultados/                     tablas agregadas y figuras (se publican)
│   ├── indicadores_encuesta.csv
│   ├── caracterizacion_ingreso.csv
│   ├── caracterizacion_tasa_abandono.csv
│   ├── tasas_abandono_uc.csv
│   └── graficos/                   plot1.png ... plot5.png
├── publicacion/                    documento publicado
├── materia_2025/                   entrega original del curso (versión previa)
└── data/                           NO SE PUBLICA (ver .gitignore)
    ├── raw/                        bases originales, por generación
    │   ├── 2017_gen2012/
    │   ├── 2024_gen2019/
    │   └── 2025_complemento/
    └── clean/               salidas de 01 y 02
```

---

## Cómo reproducir

1. Colocar las bases originales en `data/raw/` (ver arriba).
2. Abrir el proyecto de RStudio (`.Rproj`).
3. Ejecutar los scripts en orden:

| Script | Lee | Genera |
|---|---|---|
| `01_limpieza_gen2012.R` | `data/raw/2017_gen2012/` | `data/intermediate/` (actividades en formato ancho y marca de abandono, 2012) |
| `02_limpieza_gen2019.R` | `data/raw/2024_gen2019/`, `2025_complemento/` | `data/intermediate/` (ídem, 2019) |
| `03_indicadores_encuesta.R` | encuestas 2017 y 2024 | `resultados/indicadores_encuesta.csv` |
| `04_caracterizacion_ingreso.R` | formularios de ingreso | `resultados/caracterizacion_*.csv` |
| `05_aprobacion_abandono.R` | salidas de 01 y 02 | `resultados/tasas_abandono_uc.csv` |
| `06_graficos.Rmd` | solo `resultados/*.csv` | `resultados/graficos/plot*.png` |

El script 06 no necesita las bases privadas: solo las tablas de `resultados/`.

**Paquetes de R:** `tidyverse`, `haven`, `readxl`, `here`, `stringi`, `scales`, `viridisLite`, `fmsb`, `shadowtext`. Requiere `dplyr >= 1.1.0`.

---

## De dónde sale cada figura

| Figura | Contenido | Script | Archivo de datos |
|---|---|---|---|
| 1 | Abandono instruccional vs. sistémico | 03 → 06 | `indicadores_encuesta.csv` |
| 2 | Tasa de abandono según aprobación de UC clave | 05 → 06 | `tasas_abandono_uc.csv` |
| 3 | Vulnerabilidad académica | 03 → 06 | `indicadores_encuesta.csv` |
| 4 | Integración social | 03 → 06 | `indicadores_encuesta.csv` |
| 5 | Dimensiones personales del abandono | 03 → 06 | `indicadores_encuesta.csv` |

---

## Definiciones principales

**Abandono (definición operativa).** Un estudiante abandona cuando no realiza ningún tipo de actividad durante al menos un año académico (marzo a febrero), tomando como referencia la última fecha de actividad disponible. En la generación 2012 hay 111 abandonos entre 343 inscriptos (32 %); en la 2019, 375 entre 1.280 (29 %).

**Indicadores** (construidos a partir de ítems equivalentes de ambas encuestas, recodificados a variables dicotómicas):

- **Sistémico / instruccional.** Instruccional: el abandono se debe a un cambio a otra carrera o a otros estudios terciarios. Sistémico: el resto.
- **Motivo personal / institucional / mixto.** Según los motivos declarados; mixto si se marcan ambos tipos.
- **Dimensiones del motivo personal.** Económico/laboral, familiar/salud, vocacional/motivacional, social/emocional y distancia.
- **Integración social** (0 a 5, mayor es más integración): contar con un compañero que ingresó junto, haber hecho amigos, participación gremial, participación en actividades sociales y grupo de estudio.
- **Vulnerabilidad académica** (0 a 4, mayor es más vulnerable): asistencia baja, no sentirse preparado/a, dificultades en matemática o contabilidad, y haber necesitado apoyo en UC clave.

**Aprobación de UC clave.** Se considera solo a quienes tuvieron actividad en la UC. *Economía 2012*: Introducción a la Microeconomía, Economía Descriptiva y Cálculo. *Contador Público 2019*: Conceptos Contables, Contabilidad General I y Cálculo. En las bases de actividades, aprobar la UC coincide con exonerar el curso (nota ≥ 6).

---

## Resultados principales

- Entre quienes abandonaron, el abandono **instruccional** predomina en Economía 2012 (76 %) y es menor en Contador Público 2019 (59 %).
- En ambas generaciones, **no aprobar** una UC clave de primer año se asocia con una tasa de abandono mucho mayor que aprobarla, con brechas de decenas de puntos porcentuales.
- Detalle y discusión en el documento publicado.

---

## Limitaciones y reproducibilidad

- Los cuestionarios de 2017 y 2024 difieren; los indicadores se construyen a partir de ítems conceptualmente equivalentes, no idénticos.
- Las encuestas se aplicaron a quienes abandonaron y respondieron, por lo que los resultados describen a ese grupo y no a toda la generación.

---

## Privacidad

- El repositorio no contiene datos individuales, números de documento ni nombres de estudiantes.
- Los resultados que se publican son frecuencias y porcentajes agregados.
- El historial de git no incluye bases de datos: `.gitignore` excluye `data/`, `*.dta`, `*.xls`, `*.xlsx` y `*.csv` de datos individuales.

---

## Autoría y cita

**Autoras:** Camila Erosa ([ORCID](https://orcid.org/0009-0001-5237-208X)) y Jimena Padín ([ORCID](https://orcid.org/0009-0000-8625-6426)).
**Coordinación:** Alina Machado ([ORCID](https://orcid.org/0000-0002-4223-8477)).
Facultad de Ciencias Económicas y de Administración, Universidad de la República.

**Cómo citar:**

> Erosa, C. y Padín, J. (2026). *El abandono universitario en FCEA (UdelaR): Un estudio comparativo entre la Licenciatura en Economía y Contador Público*. Documento de Investigación Estudiantil. Universidad de la República, Facultad de Ciencias Económicas y de Administración.

## Agradecimientos

El trabajo se enmarca en la sexta edición del Espacio de Formación Integral (EFI) *Determinantes del abandono de los estudiantes universitarios* (FCEA, UdelaR). Se agradece a la Dirección General de Planeamiento Universitario (DGPLAN) por proveer la información administrativa, y al trabajo de recolección de información primaria de las y los estudiantes de las ediciones 2017, 2024 y 2025 del EFI (la lista completa figura en el documento publicado).

## Licencia

- **Código:** MIT
- **Datos:** no se redistribuyen; cualquier uso requiere autorización de la institución que los provee.
