# Metadatos del cuestionario --------------------------------------------------
#
# config/questions.yml es una copia de surveys/ola1/_survey/questions.yml, el
# archivo que surveydown genera al renderizar la encuesta. De ahí salen los
# enunciados y las etiquetas de cada código de respuesta.

leer_preguntas <- function(ruta = "config/questions.yml") {
  # Se lee como UTF-8 sin depender del locale del servidor
  yml <- yaml::yaml.load(paste(readLines(ruta, encoding = "UTF-8", warn = FALSE), collapse = "\n"))
  lapply(yml, function(q) {
    etiqueta <- q$label
    if (is.null(etiqueta) || length(etiqueta) == 0) etiqueta <- ""
    opciones <- q$options
    # options viene como lista etiqueta -> código; se invierte a código -> etiqueta
    codigos <- if (!is.null(opciones)) {
      stats::setNames(names(opciones), vapply(opciones, function(x) as.character(x), ""))
    } else {
      character(0)
    }
    list(
      tipo = q$type,
      indice = q$index,
      enunciado = paste(etiqueta, collapse = " "),
      codigos = codigos
    )
  })
}

PREGUNTAS <- leer_preguntas()

# Etiqueta de un código de respuesta (devuelve el código si no hay etiqueta)
etiquetar <- function(var, valores) {
  codigos <- PREGUNTAS[[var]]$codigos
  if (length(codigos) == 0) return(valores)
  out <- unname(codigos[as.character(valores)])
  ifelse(is.na(out), as.character(valores), out)
}

# Niveles ordenados de una variable categórica, tal como aparecen en la encuesta
niveles_de <- function(var) {
  names(PREGUNTAS[[var]]$codigos)
}

# Enunciados para preguntas sin etiqueta propia en questions.yml (sliders del
# conjoint y filas de la matriz de prioridades)
ENUNCIADOS_EXTRA <- c(
  cbc_practice = "Tarea de práctica: monto asignado al Postulante B (CLP)",
  stats::setNames(
    paste0("Tarea ", 1:6, ": monto asignado al Postulante B (CLP)"),
    paste0("cbc_q", 1:6)
  ),
  stats::setNames(
    paste0("Prioridad de inversión pública: ", AREAS_RANKING, " (1° = máxima)"),
    paste0("ranking_politicas_", names(AREAS_RANKING))
  )
)

enunciado_de <- function(var) {
  if (var %in% names(ENUNCIADOS_EXTRA)) return(unname(ENUNCIADOS_EXTRA[[var]]))
  q <- PREGUNTAS[[var]]
  if (is.null(q) || !nzchar(q$enunciado)) return(var)
  q$enunciado
}

# Variables que el explorador ofrece, agrupadas por bloque del flujo
variables_explorador <- function() {
  excluir <- c("consent_understand", "transicion_ancla", "ranking_politicas")
  lapply(BLOQUES_FLUJO, function(vars) {
    vars <- setdiff(vars, excluir)
    stats::setNames(vars, paste0(vars, " · ", vapply(vars, function(v) {
      txt <- enunciado_de(v)
      if (nchar(txt) > 70) paste0(substr(txt, 1, 67), "…") else txt
    }, "")))
  })
}
