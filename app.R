# Monitor de respuestas · Encuesta JusMer, Ola 1
#
# App Shiny independiente de la encuesta: solo lee las respuestas y muestra el
# avance del campo y los descriptivos básicos de las variables principales.
# Nunca escribe en la base.
#
# Los archivos de R/ se cargan automáticamente al iniciar la app:
#   R/config.R      bloques, módulos, etiquetas, cuotas y umbrales
#   R/preguntas.R   lectura de config/questions.yml (copia del de la encuesta)
#   R/datos.R       fuente de datos: simulada (demo) o Supabase (base)
#   R/simular.R     generador de respuestas simuladas
#   R/procesar.R    variables derivadas y descriptivos
#   R/graficos.R    gráficos
#
# Correr localmente: shiny::runApp()

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(DT)

MODO <- modo_datos()
DESDE <- inicio_conteo()
BASE_DEMO <- if (MODO == "demo") simular_respuestas(430, semilla = 2026) else NULL

# Componentes de interfaz ------------------------------------------------------

tarjeta <- function(titulo, ..., nota = NULL, alto = NULL) {
  card(
    full_screen = TRUE,
    height = alto,
    card_header(titulo),
    card_body(..., fillable = TRUE),
    if (!is.null(nota)) card_footer(nota, class = "nota-tarjeta")
  )
}

tabla_dt <- function(df) {
  datatable(
    df,
    rownames = FALSE,
    options = list(dom = "t", paging = FALSE, ordering = TRUE, autoWidth = FALSE,
                   columnDefs = list(list(className = "dt-right", targets = seq_len(ncol(df) - 1)))),
    class = "compact stripe",
    fillContainer = FALSE
  )
}

estado_calidad <- function(valor, umbral) {
  if (is.na(valor)) return(list(clase = "neutro", texto = "Sin datos", icono = "minus"))
  bueno <- if (umbral$mayor_mejor) valor >= umbral$bueno else valor <= umbral$bueno
  alerta <- if (umbral$mayor_mejor) valor >= umbral$alerta else valor <= umbral$alerta
  if (bueno) list(clase = "bueno", texto = "En rango", icono = "circle-check")
  else if (alerta) list(clase = "alerta", texto = "Revisar", icono = "triangle-exclamation")
  else list(clase = "critico", texto = "Problema", icono = "circle-xmark")
}

estilos <- tags$style(HTML(sprintf("
  body { background: #f9f9f7; }
  .card { border: 1px solid rgba(11,11,11,0.10); box-shadow: none; }
  .card-header { font-weight: 600; background: transparent; border-bottom: 1px solid #ecebe6; }
  .nota-tarjeta { font-size: 0.82rem; color: %s; background: transparent; }
  .bslib-value-box .value-box-title { font-size: 0.9rem; color: %s; }
  .bslib-value-box .value-box-value { font-size: 1.9rem; font-weight: 600; color: %s; }
  .bslib-value-box p { font-size: 0.85rem; color: %s; margin-bottom: 0; }
  .estado-bueno .etiqueta-estado { color: #006300; }
  .estado-alerta .etiqueta-estado { color: #a86d00; }
  .estado-critico .etiqueta-estado { color: %s; }
  .estado-neutro .etiqueta-estado { color: %s; }
  .etiqueta-estado { font-weight: 600; }
  .modo-demo { background: #fff4d6; border: 1px solid #f2d27a; border-radius: 6px; padding: 8px 10px; font-size: 0.85rem; }
  .modo-base { background: #e6f4e6; border: 1px solid #9fd09f; border-radius: 6px; padding: 8px 10px; font-size: 0.85rem; }
  .modo-error { background: #fbe4e4; border: 1px solid #e8a0a0; border-radius: 6px; padding: 8px 10px; font-size: 0.85rem; }
  .progreso { height: 6px; background: #e1e0d9; border-radius: 3px; margin-top: 6px; overflow: hidden; }
  .progreso > div { height: 100%%; background: %s; }
  .enunciado { color: %s; font-size: 0.92rem; }
  table.mini { width: 100%%; font-size: 0.9rem; }
  table.mini td { padding: 3px 6px; border-bottom: 1px solid #ecebe6; }
  table.mini td.num { text-align: right; font-variant-numeric: tabular-nums; }
  table.dataTable { font-size: 0.88rem; font-variant-numeric: tabular-nums; }
",
  COLORES$tenue, COLORES$tinta2, COLORES$tinta, COLORES$tenue,
  COLORES$critico, COLORES$tenue, COLORES$serie1, COLORES$tinta2
)))

panel_resumen <- nav_panel(
  "Resumen",
  icon = icon("gauge"),
  layout_column_wrap(
    width = 1 / 5, fill = FALSE,
    uiOutput("vb_completas"), uiOutput("vb_iniciadas"), uiOutput("vb_tasa"),
    uiOutput("vb_hoy"), uiOutput("vb_duracion")
  ),
  layout_columns(
    col_widths = c(7, 5),
    tarjeta("Sesiones por día", plotOutput("g_diario", height = "300px"),
            nota = "Incluye solo a quienes aceptaron participar. Incompletas: no llegaron a las preguntas finales."),
    tarjeta("Encuestas completas acumuladas", plotOutput("g_acumulado", height = "300px"))
  ),
  layout_columns(
    col_widths = c(7, 5),
    tarjeta("Abandono: último bloque con respuestas", plotOutput("g_abandono", height = "300px"),
            nota = "Sesiones que aceptaron y no terminaron, según el orden de módulos de su forma (A o B)."),
    tarjeta("Formas y dispositivos (encuestas completas)", uiOutput("tabla_formas"))
  )
)

panel_muestra <- nav_panel(
  "Muestra",
  icon = icon("users"),
  layout_column_wrap(
    width = 1 / 3, fill = FALSE, heights_equal = "row",
    tarjeta("Sexo", plotOutput("g_sexo", height = "190px"),
            nota = "Barra: observado. Marca negra: meta del marco muestral."),
    tarjeta("Edad", plotOutput("g_edad", height = "190px"),
            nota = "Barra: observado. Marca negra: meta del marco muestral."),
    tarjeta("Nivel educativo (cuota)", plotOutput("g_educ_cuota", height = "190px"),
            nota = "Barra: observado. Marca negra: meta del marco muestral.")
  ),
  layout_column_wrap(
    width = 1 / 2, fill = FALSE,
    tarjeta("Nivel educativo", plotOutput("g_education", height = "320px")),
    tarjeta("Ingreso mensual del hogar", plotOutput("g_income", height = "320px"))
  ),
  layout_column_wrap(
    width = 1 / 2, fill = FALSE,
    tarjeta("Estatus social subjetivo (0 = más bajo, 10 = más alto)", plotOutput("g_estatus", height = "240px"),
            nota = textOutput("nr_estatus", inline = TRUE)),
    tarjeta("Posición política (0 = izquierda, 10 = derecha)", plotOutput("g_politica", height = "240px"),
            nota = textOutput("nr_politica", inline = TRUE))
  ),
  layout_column_wrap(
    width = 1 / 2, fill = FALSE,
    tarjeta("Situación económica del hogar", plotOutput("g_adc", height = "240px")),
    tarjeta("Identidad de género", plotOutput("g_gender", height = "240px"))
  ),
  layout_column_wrap(
    width = 1 / 2, fill = FALSE,
    tarjeta("Sistema de salud", plotOutput("g_salud", height = "150px")),
    tarjeta("Tipo de colegio", plotOutput("g_colegio", height = "150px"))
  )
)

panel_escalas <- nav_panel(
  "Escalas",
  icon = icon("chart-bar"),
  do.call(navset_card_underline, c(
    list(full_screen = TRUE),
    lapply(seq_along(MODULOS_ESCALAS), function(i) {
      m <- names(MODULOS_ESCALAS)[i]
      nav_panel(
        m,
        tags$p(class = "enunciado", MODULOS_ESCALAS[[m]]$enunciado),
        plotOutput(paste0("g_escala_", i), height = "auto"),
        DTOutput(paste0("t_escala_", i), fill = FALSE)
      )
    })
  ))
)

panel_conjoint <- nav_panel(
  "Conjoint y decisiones",
  icon = icon("scale-balanced"),
  layout_columns(
    col_widths = c(7, 5),
    tarjeta("Reparto del monto de la beca (tareas 1 a 6)", plotOutput("g_conjoint", height = "340px"),
            nota = "Monto asignado al Postulante B, de 0 a $2.000.000. La línea marca el reparto en partes iguales."),
    tarjeta("Tarea repetida (práctica y tarea 6)", plotOutput("g_consistencia", height = "300px"),
            nota = textOutput("txt_consistencia", inline = TRUE))
  ),
  tarjeta("Resumen por tarea", DTOutput("t_conjoint", fill = FALSE),
          nota = "En el punto medio: el slider quedó en $1.000.000, que es su posición inicial."),
  layout_column_wrap(
    width = 1 / 3, fill = FALSE,
    tarjeta("Alumnos que deberían admitirse sin pagar (de 100)", plotOutput("g_dec1", height = "230px")),
    tarjeta("Pago adicional en la mensualidad para apoyarlos", plotOutput("g_dec2", height = "230px"),
            nota = "Solo se pregunta si la respuesta anterior es distinta de 0."),
    tarjeta("Pago adicional en la Isapre para incluir a un 10 %", plotOutput("g_dec3", height = "230px"))
  ),
  tarjeta("Prioridades de inversión pública", plotOutput("g_ranking", height = "260px"),
          nota = "La posición media va de 1 (máxima prioridad) a 5. Cada persona asigna al menos tres prioridades.")
)

panel_calidad <- nav_panel(
  "Calidad",
  icon = icon("clipboard-check"),
  tags$p(class = "enunciado", "Indicadores calculados sobre las encuestas completas, con los filtros de forma y fecha."),
  layout_column_wrap(
    width = 1 / 5, fill = FALSE,
    uiOutput("vb_atencion"), uiOutput("vb_manipulacion"), uiOutput("vb_rapidos"),
    uiOutput("vb_plano"), uiOutput("vb_nsnr")
  ),
  layout_columns(
    col_widths = c(6, 6),
    tarjeta("Duración de la encuesta (minutos)", plotOutput("g_duracion", height = "300px"),
            nota = "La línea discontinua marca un tercio de la mediana, el umbral usado para respuestas muy rápidas."),
    tarjeta("Ítems con más No sé y Prefiero no responder", DTOutput("t_nsnr", fill = FALSE))
  )
)

panel_explorador <- nav_panel(
  "Explorador",
  icon = icon("magnifying-glass"),
  layout_columns(
    col_widths = c(4, 8),
    card(
      card_header("Variable"),
      selectizeInput("var_explorar", NULL, choices = variables_explorador(), width = "100%"),
      tags$p(class = "enunciado", textOutput("txt_enunciado")),
      div(style = "max-height: 470px; overflow-y: auto;", DTOutput("t_explorar", fill = FALSE))
    ),
    card(card_header("Distribución"), plotOutput("g_explorar", height = "520px", fill = FALSE))
  )
)

barra_lateral <- sidebar(
  width = 280,
  uiOutput("estado_fuente"),
  radioButtons("casos", "Casos", c("Solo completas" = "completas", "Todas las que aceptaron" = "todas")),
  helpText("Aplica a Muestra, Escalas, Conjoint y Explorador."),
  radioButtons("forma", "Forma de la encuesta", c("Ambas" = "ambas", "A" = "A", "B" = "B"), inline = TRUE),
  dateRangeInput("rango", "Fechas de inicio", start = NULL, end = NULL,
                 format = "dd/mm/yyyy", language = "es", separator = "a"),
  if (!is.null(DESDE)) helpText(sprintf(
    "Solo se cuentan las sesiones iniciadas desde el %s.",
    format(DESDE, if (format(DESDE, "%H:%M", tz = ZONA_HORARIA) == "00:00") "%d/%m/%Y" else "%d/%m/%Y a las %H:%M",
           tz = ZONA_HORARIA)
  )),
  numericInput("meta", "Meta de encuestas completas", value = META_N_DEFAULT, min = 1, step = 100),
  hr(),
  actionButton("actualizar", "Actualizar ahora", icon = icon("rotate"), class = "btn-outline-secondary"),
  tags$small(textOutput("txt_actualizado")),
  tags$small(class = "text-muted", sprintf("Se actualiza sola cada %d segundos.", REFRESCO_SEG))
)

ui <- page_navbar(
  title = "Monitor JusMer · Ola 1",
  theme = bs_theme(
    version = 5,
    primary = COLORES$marca,
    fg = COLORES$tinta,
    bg = "#ffffff",
    base_font = font_collection("system-ui", "-apple-system", "Segoe UI", "Roboto", "sans-serif")
  ),
  bg = COLORES$marca,
  fillable = FALSE,
  header = estilos,
  sidebar = barra_lateral,
  panel_resumen,
  panel_muestra,
  panel_escalas,
  panel_conjoint,
  panel_calidad,
  panel_explorador
)

# Servidor ---------------------------------------------------------------------

server <- function(input, output, session) {
  fuente <- crear_fuente(MODO, BASE_DEMO, reactive(input$actualizar), session)

  todo <- reactive({
    req(fuente$datos())
    desde_corte(preparar(fuente$datos()), DESDE)
  })

  # El rango de fechas abarca todo el campo: se fija con los primeros datos y,
  # si el usuario no lo ha acotado, se extiende cuando llegan días nuevos.
  ultimo_dia <- reactiveVal(NULL)
  observeEvent(todo(), {
    f <- todo()$fecha
    f <- f[!is.na(f)]
    if (length(f) == 0) return()
    fin_datos <- max(f)
    previo <- ultimo_dia()
    if (is.null(previo)) {
      updateDateRangeInput(session, "rango", start = min(f), end = fin_datos)
    } else if (fin_datos > previo && isTRUE(isolate(input$rango[2]) == previo)) {
      updateDateRangeInput(session, "rango", end = fin_datos)
    }
    ultimo_dia(fin_datos)
  })
  # Hasta que el rango se inicializa con los datos, no se filtra por fecha
  rango <- reactive(if (is.null(ultimo_dia())) NULL else input$rango)

  # Datos para Resumen: todas las sesiones, con filtros de forma y fecha
  base_resumen <- reactive({
    d <- todo()
    if (input$forma != "ambas") d <- d[d$version_encuesta == input$forma, ]
    r <- rango()
    if (length(r) == 2 && !any(is.na(r))) d <- d[is.na(d$fecha) | (d$fecha >= r[1] & d$fecha <= r[2]), ]
    d
  })
  # Datos para descriptivos: con el filtro de casos
  datos <- reactive(filtrar(todo(), input$casos, input$forma, rango()))
  completas <- reactive(filtrar(todo(), "completas", input$forma, rango()))

  output$estado_fuente <- renderUI({
    if (!is.null(fuente$error())) {
      div(class = "modo-error", tags$strong("Error al leer los datos"), tags$br(), fuente$error())
    } else if (MODO == "demo") {
      div(class = "modo-demo", icon("flask"), tags$strong(" Datos simulados"), tags$br(),
          "Las cifras son inventadas. Cambie MONITOR_MODO a 'base' para leer Supabase.")
    } else {
      div(class = "modo-base", icon("database"), tags$strong(" En vivo"), tags$br(),
          sprintf("Tabla %s en Supabase", Sys.getenv("SD_TABLE", "responses")))
    }
  })

  output$txt_actualizado <- renderText({
    req(fuente$actualizado())
    paste("Última lectura:", format(fuente$actualizado(), "%H:%M:%S", tz = ZONA_HORARIA))
  })

  # Resumen ------------------------------------------------------------------
  ind <- reactive(indicadores(base_resumen()))

  output$vb_completas <- renderUI({
    i <- ind()
    avance <- if (isTRUE(input$meta > 0)) i$completas / input$meta else NA
    value_box(
      title = "Encuestas completas", value = num_es(i$completas),
      tags$p(sprintf("%s de la meta (%s)", pct_es(avance, 0.1), num_es(input$meta))),
      div(class = "progreso", div(style = sprintf("width:%s%%", min(100, round(100 * avance, 1)))))
    )
  })
  output$vb_iniciadas <- renderUI({
    i <- ind()
    value_box(
      title = "Sesiones iniciadas", value = num_es(i$iniciadas),
      tags$p(sprintf("%s no aceptaron participar", num_es(i$rechazan)))
    )
  })
  output$vb_tasa <- renderUI({
    i <- ind()
    value_box(
      title = "Tasa de término", value = pct_es(i$tasa, 0.1),
      tags$p("Completas sobre quienes aceptaron")
    )
  })
  output$vb_hoy <- renderUI({
    i <- ind()
    value_box(
      title = "Completas hoy", value = num_es(i$hoy),
      tags$p(sprintf("%s sesiones iniciadas hoy", num_es(i$hoy_iniciadas)))
    )
  })
  output$vb_duracion <- renderUI({
    i <- ind()
    hace <- if (!is.na(i$ultima)) as.numeric(difftime(Sys.time(), i$ultima, units = "mins")) else NA
    texto_hace <- if (is.na(hace)) "Sin sesiones" else if (hace < 60) sprintf("Última sesión hace %d min", round(hace))
                  else sprintf("Última sesión: %s", format(i$ultima, "%d/%m %H:%M", tz = ZONA_HORARIA))
    value_box(
      title = "Duración mediana", value = if (is.na(i$duracion_mediana)) "–" else paste(num_es(i$duracion_mediana, 0.1), "min"),
      tags$p(texto_hace)
    )
  })

  output$g_diario <- renderPlot(g_diario(serie_diaria(base_resumen())), bg = COLORES$superficie, res = 96)
  output$g_acumulado <- renderPlot(g_acumulado(acumulado(base_resumen())), bg = COLORES$superficie, res = 96)
  output$g_abandono <- renderPlot(g_abandono(abandono(base_resumen())), bg = COLORES$superficie, res = 96)

  output$tabla_formas <- renderUI({
    d <- base_resumen()
    d <- d[d$completa, ]
    fila <- function(etiqueta, n, total) {
      tags$tr(tags$td(etiqueta), tags$td(class = "num", num_es(n)),
              tags$td(class = "num", if (total > 0) pct_es(n / total) else "–"))
    }
    total <- nrow(d)
    tagList(
      tags$h6("Forma asignada"),
      tags$table(class = "mini", tags$tbody(
        fila("Forma A: Meritocracia → Actitudes → Merecimiento", sum(d$version_encuesta == "A"), total),
        fila("Forma B: Merecimiento → Actitudes → Meritocracia", sum(d$version_encuesta == "B"), total)
      )),
      tags$h6("Dispositivo", style = "margin-top: 18px;"),
      tags$table(class = "mini", tags$tbody(
        lapply(c("Computador", "Móvil"), function(x) fila(x, sum(d$dispositivo %in% x), total))
      )),
      tags$h6("Sistema operativo", style = "margin-top: 18px;"),
      tags$table(class = "mini", tags$tbody({
        so <- sort(table(d$sistema), decreasing = TRUE)
        lapply(names(so), function(x) fila(x, so[[x]], total))
      }))
    )
  })

  # Muestra -------------------------------------------------------------------
  output$g_sexo <- renderPlot({
    d <- datos(); d$sexo <- d$sexo_cuota
    g_cuotas(cuotas(d, "sexo"))
  }, bg = COLORES$superficie, res = 96)
  output$g_edad <- renderPlot(g_cuotas(cuotas(datos(), "tramo_edad")), bg = COLORES$superficie, res = 96)
  output$g_educ_cuota <- renderPlot(g_cuotas(cuotas(datos(), "educ_cuota")), bg = COLORES$superficie, res = 96)
  output$g_education <- renderPlot(g_barras(frecuencias(datos(), "education"), ancho = 46), bg = COLORES$superficie, res = 96)
  output$g_income <- renderPlot(g_barras(frecuencias(datos(), "income"), ancho = 46), bg = COLORES$superficie, res = 96)
  output$g_estatus <- renderPlot(g_histograma(datos()$estatus_subjetivo_num, 1, xlab = NULL), bg = COLORES$superficie, res = 96)
  output$g_politica <- renderPlot(g_histograma(datos()$posicion_politica_num, 1, xlab = NULL), bg = COLORES$superficie, res = 96)
  texto_nr <- function(v) {
    d <- datos()
    respondio <- respondida(d[[v]]) | d[[paste0(v, "_es_nr")]]
    sprintf("Prefiero no responder: %s (%s de %s)",
            pct_es(mean(d[[paste0(v, "_es_nr")]][respondio])),
            num_es(sum(d[[paste0(v, "_es_nr")]])), num_es(sum(respondio)))
  }
  output$nr_estatus <- renderText(texto_nr("estatus_subjetivo"))
  output$nr_politica <- renderText(texto_nr("posicion_politica"))
  output$g_adc <- renderPlot(g_barras(frecuencias(datos(), "adc_qstn"), ancho = 46), bg = COLORES$superficie, res = 96)
  output$g_gender <- renderPlot(g_barras(frecuencias(datos(), "gender"), ancho = 46), bg = COLORES$superficie, res = 96)
  output$g_salud <- renderPlot(g_barras(frecuencias(datos(), "salud_prov")), bg = COLORES$superficie, res = 96)
  output$g_colegio <- renderPlot(g_barras(frecuencias(datos(), "educ_prov")), bg = COLORES$superficie, res = 96)

  # Escalas -------------------------------------------------------------------
  lapply(seq_along(MODULOS_ESCALAS), function(i) {
    m <- names(MODULOS_ESCALAS)[i]
    n_items <- length(MODULOS_ESCALAS[[m]]$items)
    output[[paste0("g_escala_", i)]] <- renderPlot(
      g_escala(distribucion_escala(datos(), m), m),
      height = function() 110 + 52 * n_items, bg = COLORES$superficie, res = 96
    )
    output[[paste0("t_escala_", i)]] <- renderDT(server = FALSE, {
      r <- resumen_escala(datos(), m)
      alto <- if (MODULOS_ESCALAS[[m]]$escala == "secuencial") "% Regularmente o a menudo" else "% Alto (4-5)"
      tab <- data.frame(
        Ítem = r$item, Descripción = r$etiqueta, N = r$n,
        Media = num_es(r$media, 0.01), DE = num_es(r$de, 0.01),
        alto = pct_es(r$pct_alto), `% No sé` = pct_es(r$pct_ns, 0.1),
        `% Prefiero no responder` = pct_es(r$pct_nr, 0.1), check.names = FALSE
      )
      names(tab)[names(tab) == "alto"] <- alto
      tabla_dt(tab)
    })
  })

  # Conjoint y decisiones -----------------------------------------------------
  output$g_conjoint <- renderPlot(g_conjoint(conjoint_largo(datos())), bg = COLORES$superficie, res = 96)
  cons <- reactive(consistencia_repetida(datos()))
  output$g_consistencia <- renderPlot(g_consistencia(cons()), bg = COLORES$superficie, res = 96)
  output$txt_consistencia <- renderText({
    c <- cons()
    if (c$n == 0) return("Sin casos con ambas tareas respondidas.")
    sprintf(
      "Consistentes (diferencia de hasta $200.000): %s de %s casos. Correlación: %s. Puntos sobre la diagonal indican la misma preferencia en ambas tareas.",
      pct_es(c$pct), num_es(c$n), num_es(c$r, 0.01)
    )
  })
  output$t_conjoint <- renderDT(server = FALSE, {
    r <- resumen_conjoint(datos())
    tabla_dt(data.frame(
      Tarea = r$tarea, N = r$n, `Media al Postulante B` = clp(r$media), DE = clp(r$de),
      `% en el punto medio` = pct_es(r$pct_medio, 0.1), `% todo a uno` = pct_es(r$pct_extremo, 0.1),
      `% Prefiero no responder` = pct_es(r$pct_nr, 0.1), check.names = FALSE
    ))
  })
  output$g_dec1 <- renderPlot(g_histograma(datos()$dec_1_num, 10), bg = COLORES$superficie, res = 96)
  output$g_dec2 <- renderPlot(g_histograma(datos()$dec_2_num, 10, etiquetas = function(x) paste0(x, "%")), bg = COLORES$superficie, res = 96)
  output$g_dec3 <- renderPlot(g_histograma(datos()$dec_3_num, 10, etiquetas = function(x) paste0(x, "%")), bg = COLORES$superficie, res = 96)
  output$g_ranking <- renderPlot(g_ranking(resumen_ranking(datos())), bg = COLORES$superficie, res = 96)

  # Calidad -------------------------------------------------------------------
  cal <- reactive(calidad(completas()))

  caja_calidad <- function(titulo, valor, clave, detalle, icono) {
    est <- estado_calidad(valor, UMBRALES_CALIDAD[[clave]])
    value_box(
      title = titulo, value = pct_es(valor, 0.1),
      class = paste0("estado-", est$clase),
      tags$p(tags$span(class = "etiqueta-estado", icon(est$icono), est$texto), tags$br(), detalle)
    )
  }
  output$vb_atencion <- renderUI(caja_calidad(
    "Chequeo de atención aprobado", cal()$atencion, "atencion",
    "Dejaron en blanco la pregunta de control", "eye"))
  output$vb_manipulacion <- renderUI(caja_calidad(
    "Comprensión de la tarea", cal()$manipulacion, "manipulacion",
    "Respondieron 'repartir un monto'", "lightbulb"))
  output$vb_rapidos <- renderUI(caja_calidad(
    "Respuestas muy rápidas", cal()$rapidos, "rapidos",
    sprintf("Menos de %s min", num_es(cal()$umbral_rapido, 0.1)), "bolt"))
  output$vb_plano <- renderUI(caja_calidad(
    "Respuesta idéntica en merecimiento", cal()$plano, "plano",
    "Misma opción en toda la batería", "equals"))
  output$vb_nsnr <- renderUI(caja_calidad(
    "Muchos No sé / No responde", cal()$nsnr_alto, "nsnr_alto",
    "Más del 25 % de los ítems de escalas", "circle-question"))

  output$g_duracion <- renderPlot({
    d <- completas()
    x <- d$duracion_min[!is.na(d$duracion_min)]
    if (length(x) == 0) return(sin_datos())
    tope <- min(stats::quantile(x, 0.98, names = FALSE), DURACION_MAX_MIN)
    n_sobre <- sum(x > tope)
    eje_x <- if (n_sobre > 0) {
      sprintf("Minutos (%s %s de más de %s min se %s al final)",
              n_sobre, if (n_sobre == 1) "encuesta" else "encuestas", num_es(tope, 1),
              if (n_sobre == 1) "agrupa" else "agrupan")
    } else {
      "Minutos"
    }
    ggplot(data.frame(x = pmin(x, tope)), aes(x = x)) +
      geom_histogram(binwidth = max(1, round(tope / 60)), boundary = 0,
                     fill = COLORES$serie1, colour = COLORES$superficie, linewidth = 0.4) +
      geom_vline(xintercept = stats::median(x), colour = COLORES$tinta, linewidth = 0.6) +
      geom_vline(xintercept = stats::median(x) / 3, colour = COLORES$critico, linewidth = 0.6, linetype = "22") +
      annotate("label", x = stats::median(x), y = Inf, vjust = 1.3,
               label = paste("Mediana:", num_es(stats::median(x), 0.1), "min"),
               size = 3.4, fill = COLORES$superficie, label.size = 0) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
      labs(x = eje_x, y = "Encuestas") +
      tema_monitor()
  }, bg = COLORES$superficie, res = 96)

  output$t_nsnr <- renderDT(server = FALSE, {
    r <- items_mas_nsnr(completas())
    tabla_dt(data.frame(
      Ítem = r$item, Descripción = r$etiqueta, `% No sé` = pct_es(r$pct_ns, 0.1),
      `% Prefiero no responder` = pct_es(r$pct_nr, 0.1), Total = pct_es(r$pct_total, 0.1),
      check.names = FALSE
    ))
  })

  # Explorador ----------------------------------------------------------------
  output$txt_enunciado <- renderText(enunciado_de(req(input$var_explorar)))

  explorar <- reactive({
    v <- req(input$var_explorar)
    d <- datos()
    if (v %in% SLIDERS) {
      x <- d[[paste0(v, "_num")]]
      respondio <- respondida(d[[v]]) | d[[paste0(v, "_es_nr")]]
      x <- x[respondio]
      valores <- sort(unique(x[!is.na(x)]))
      tab <- data.frame(codigo = c(format(valores, scientific = FALSE, trim = TRUE), "NR"),
                        n = c(vapply(valores, function(z) sum(x == z, na.rm = TRUE), 0L), sum(is.na(x))))
      tab$categoria <- ifelse(tab$codigo == "NR", "Prefiero no responder",
                              if (grepl("^cbc", v)) clp(suppressWarnings(as.numeric(tab$codigo))) else tab$codigo)
      tab$pct <- if (sum(tab$n)) tab$n / sum(tab$n) else 0
      list(tipo = "num", x = x, tab = tab, v = v)
    } else {
      niveles <- if (v == "year_of_birth") NULL else niveles_de(v)
      list(tipo = "cat", tab = frecuencias(d, v, niveles = niveles), v = v)
    }
  })

  output$g_explorar <- renderPlot({
    e <- explorar()
    if (e$tipo == "num") {
      paso <- if (grepl("^cbc", e$v)) 1e5 else if (grepl("^dec", e$v)) 10 else 1
      g_histograma(e$x, paso, etiquetas = if (grepl("^cbc", e$v)) function(x) clp(x) else waiver())
    } else if (e$v == "year_of_birth") {
      x <- suppressWarnings(as.numeric(datos()$year_of_birth))
      g_histograma(x, 1)
    } else {
      g_barras(e$tab, ancho = 40)
    }
  }, bg = COLORES$superficie, res = 96)

  output$t_explorar <- renderDT(server = FALSE, {
    t <- explorar()$tab
    if (explorar()$v == "year_of_birth") t <- t[t$n > 0, ]
    tabla_dt(data.frame(Categoría = t$categoria, Código = t$codigo, N = t$n,
                        `%` = pct_es(t$pct, 0.1), check.names = FALSE))
  })
}

shinyApp(ui, server)
