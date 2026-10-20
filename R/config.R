# Configuración del monitor ---------------------------------------------------
#
# Todo lo que depende del cuestionario de la Ola 1 está concentrado en este
# archivo: bloques del flujo de la encuesta, módulos de escalas, etiquetas
# breves de los ítems y marco de cuotas. Si cambia el cuestionario, basta con
# volver a copiar surveys/ola1/_survey/questions.yml a config/questions.yml y
# ajustar las listas de abajo.

ZONA_HORARIA <- "America/Santiago"

# Meta de casos completos y frecuencia de actualización (configurables por
# variables de entorno al desplegar)
META_N_DEFAULT <- as.integer(Sys.getenv("MONITOR_META_N", "3000"))
REFRESCO_SEG <- as.integer(Sys.getenv("MONITOR_REFRESCO_SEG", "60"))

# Variables que solo se responden al final de la encuesta (página id-final):
# un caso cuenta como completo si respondió alguna de ellas.
VARS_FIN <- c("salud_prov", "educ_prov")

# Tope del eje del gráfico de duración (minutos): las encuestas más largas
# (p. ej. sesiones que quedaron abiertas) se agrupan en la última barra.
DURACION_MAX_MIN <- 180

# Valores especiales usados en todo el cuestionario
COD_NS <- "98"
COD_NR <- "99"

# Paleta ----------------------------------------------------------------------

COLORES <- list(
  marca      = "#841414",  # color de la barra de progreso de la encuesta
  serie1     = "#2a78d6",
  serie2     = "#eb6834",
  tinta      = "#0b0b0b",
  tinta2     = "#52514e",
  tenue      = "#898781",
  grilla     = "#e1e0d9",
  eje        = "#c3c2b7",
  superficie = "#ffffff",
  nsnr       = "#b5b3ab",
  bueno      = "#0ca30c",
  alerta     = "#fab219",
  critico    = "#d03b3b"
)

# Escalas de 5 puntos: divergente (acuerdo, justicia) y secuencial (frecuencia).
# Posición 1 = código "1" ... posición 5 = código "5".
PALETA_DIVERGENTE <- c("#b8302f", "#f0a09f", "#d9d8d1", "#86b6ef", "#1c5cab")
PALETA_SECUENCIAL <- c("#cde2fb", "#9ec5f4", "#5598e7", "#256abf", "#0d366b")

# Bloques del flujo de la encuesta (para analizar el abandono) ----------------
#
# Ambas formas parten y terminan igual; entre medio cambia el orden de los
# módulos (ver version_encuesta en surveys/ola1/app.R).

BLOQUES_FLUJO <- list(
  "Información general" = c("sexo", "year_of_birth", "education"),
  "Conjoint"            = c("cbc_practice", "manip_check", paste0("cbc_q", 1:6)),
  "Meritocracia"        = paste0("merit_", 1:8),
  "Actitudes y percepción" = c(
    paste0("mj_", 1:3), paste0("ref_", 1:3), paste0("proy_", 1:2),
    paste0("ranking_politicas_", c("educacion", "salud", "pensiones", "cuidado_ninos", "carreteras")),
    "dec_3", "dec_1", "dec_2", paste0("lucro_", 1:3)
  ),
  "Merecimiento" = c(
    paste0("control_", 1:3), "actitud_1", "actitud_2", paste0("reciproc_", 1:3),
    "actitud_4", paste0("id_", 1:3), paste0("nec_", 1:3), paste0("esf_", 1:5),
    "adc_qstn", paste0("adc_", 5:7)
  ),
  "Cierre" = c("gender", "income", "estatus_subjetivo", "posicion_politica", "salud_prov", "educ_prov")
)

ORDEN_FORMA <- list(
  A = c("Información general", "Conjoint", "Meritocracia", "Actitudes y percepción", "Merecimiento", "Cierre"),
  B = c("Información general", "Conjoint", "Merecimiento", "Actitudes y percepción", "Meritocracia", "Cierre")
)

# Módulos de escalas tipo Likert y etiquetas breves de los ítems ---------------
#
# escala: "divergente" para acuerdo y justicia (polo negativo / punto medio /
# polo positivo), "secuencial" para frecuencia.

MODULOS_ESCALAS <- list(
  "Justicia de mercado" = list(
    escala = "divergente",
    enunciado = "¿En qué medida está de acuerdo o en desacuerdo con las siguientes afirmaciones?",
    items = c(
      mj_1   = "Es justo: altos ingresos, mejor salud",
      mj_2   = "Es justo: altos ingresos, mejor educación para sus hijos",
      mj_3   = "Es justo: altos ingresos, mejores pensiones",
      ref_1  = "Es justo: yo, mejor salud si puedo pagar",
      ref_2  = "Es justo: yo, mejor educación si puedo pagar",
      ref_3  = "Es justo: yo, mejor pensión si puedo pagar",
      proy_1 = "Mis hijos/as, mejor salud si puedo pagar",
      proy_2 = "Mis hijos/as, mejor educación si puedo pagar"
    )
  ),
  "Lucro en servicios" = list(
    escala = "divergente",
    enunciado = "Qué tan justo o injusto le parece que empresas privadas obtengan ganancias monetarias administrando…",
    items = c(
      lucro_1 = "Salud",
      lucro_2 = "Educación",
      lucro_3 = "Pensiones"
    )
  ),
  "Merecimiento" = list(
    escala = "divergente",
    enunciado = "¿En qué medida está de acuerdo o en desacuerdo con las siguientes afirmaciones?",
    items = c(
      control_1  = "Control: pobreza por errores propios, igual derecho a apoyo",
      control_2  = "Control: responsables de sus problemas igual merecen beneficios",
      control_3  = "Control: ayudar sin importar la causa",
      actitud_1  = "Actitud: deberían mostrar más gratitud",
      actitud_2  = "Actitud: está bien que expresen descontento",
      reciproc_1 = "Reciprocidad: reservados para quienes contribuyeron",
      reciproc_2 = "Reciprocidad: no es justo beneficiar a quien no contribuyó",
      reciproc_3 = "Reciprocidad: más aporte, mejores beneficios",
      actitud_4  = "Reciprocidad: mismo acceso, haya cotizado o no",
      id_1       = "Identidad: prioridad a nacidos en Chile",
      id_2       = "Identidad: migrantes, mismos beneficios",
      id_3       = "Identidad: solo con varios años en Chile",
      nec_1      = "Necesidad: solo para pobreza real",
      nec_2      = "Necesidad: no a quien tiene recursos propios",
      nec_3      = "Necesidad: solo extrema necesidad",
      esf_1      = "Esfuerzo: deben hacer algo para mejorar",
      esf_2      = "Esfuerzo: sin esfuerzo, menos apoyo",
      esf_3      = "Esfuerzo: apoyar aunque no haya esfuerzo visible",
      esf_4      = "Esfuerzo: duración según el esfuerzo",
      esf_5      = "Esfuerzo: ayudar igual, busque o no trabajo"
    )
  ),
  "Meritocracia" = list(
    escala = "divergente",
    enunciado = "¿En qué medida está de acuerdo o en desacuerdo con las siguientes afirmaciones?",
    items = c(
      merit_1 = "Percepción: recompensa al esfuerzo",
      merit_2 = "Percepción: recompensa al talento",
      merit_3 = "Percepción: padres ricos, mejor en la vida",
      merit_4 = "Percepción: buenos contactos, mejor en la vida",
      merit_5 = "Preferencia: más esfuerzo, más recompensa",
      merit_6 = "Preferencia: más talento, más recompensa",
      merit_7 = "Preferencia: bien que con padres ricos les vaya mejor",
      merit_8 = "Preferencia: bien que con contactos les vaya mejor"
    )
  ),
  "Inseguridad económica" = list(
    escala = "secuencial",
    enunciado = "¿Con qué frecuencia se siente preocupado por las siguientes situaciones?",
    items = c(
      adc_5 = "Que mi situación financiera empeore",
      adc_6 = "Quedar desempleado",
      adc_7 = "Que las próximas generaciones lo tengan más difícil"
    )
  )
)

# Variables numéricas (sliders) con su casilla "Prefiero no responder"
SLIDERS <- c(
  paste0("cbc_q", 1:6), "cbc_practice", "dec_1", "dec_2", "dec_3",
  "estatus_subjetivo", "posicion_politica"
)

AREAS_RANKING <- c(
  educacion     = "Educación",
  salud         = "Salud",
  pensiones     = "Pensiones",
  cuidado_ninos = "Salas cuna",
  carreteras    = "Carreteras"
)

# Umbrales de los indicadores de calidad --------------------------------------
#
# Referenciales: bueno si el indicador está del lado correcto de `bueno`,
# atención si está entre `bueno` y `alerta`, problema más allá de `alerta`.
# mayor_mejor indica la dirección del indicador.

UMBRALES_CALIDAD <- list(
  atencion     = list(bueno = 0.90, alerta = 0.80, mayor_mejor = TRUE),
  manipulacion = list(bueno = 0.85, alerta = 0.70, mayor_mejor = TRUE),
  rapidos      = list(bueno = 0.05, alerta = 0.10, mayor_mejor = FALSE),
  plano        = list(bueno = 0.03, alerta = 0.06, mayor_mejor = FALSE),
  nsnr_alto    = list(bueno = 0.05, alerta = 0.10, mayor_mejor = FALSE)
)

# Marco de cuotas -------------------------------------------------------------
#
# Proporciones copiadas de output/monitoreo_cuotas/app.R (pre-piloto). Revisar
# que sigan siendo las del diseño muestral de la Ola 1.

MARCO_CUOTAS <- data.frame(
  variable = c(rep("sexo", 2), rep("tramo_edad", 4), rep("educ_cuota", 4)),
  categoria = c(
    "Hombre", "Mujer",
    "18-29", "30-44", "45-59", "60+",
    "Básica o menos", "Media", "Técnica superior", "Universitaria o más"
  ),
  p_meta = c(0.4776, 0.5224, 0.2165, 0.2908, 0.2392, 0.2534, 0.2873, 0.4137, 0.0988, 0.2002)
)

EDUC_CUOTA <- c(
  "0" = "Básica o menos", "1" = "Básica o menos", "2" = "Básica o menos",
  "3" = "Media", "4" = "Media",
  "5" = "Técnica superior", "6" = "Técnica superior",
  "7" = "Universitaria o más", "8" = "Universitaria o más", "9" = "Universitaria o más"
)
