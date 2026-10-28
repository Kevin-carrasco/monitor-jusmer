# Preparación de los datos y descriptivos -------------------------------------

a_fecha <- function(x) {
  x[!nzchar(x)] <- NA
  as.POSIXct(x, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
}

respondida <- function(x) !is.na(x) & nzchar(x)

# Agrega a los datos crudos las variables derivadas que usa el monitor
preparar <- function(crudo) {
  d <- crudo
  for (v in setdiff(columnas_monitor(), names(d))) d[[v]] <- rep("", nrow(d))
  d[] <- lapply(d, function(x) { x <- as.character(x); x[is.na(x)] <- ""; x })

  d$inicio <- a_fecha(d$time_start)
  d$fin <- a_fecha(d$time_end)
  d$fecha <- as.Date(format(d$inicio, tz = ZONA_HORARIA))
  d$acepta <- d$consent_understand == "yes"
  d$completa <- d$acepta & Reduce(`|`, lapply(VARS_FIN, function(v) respondida(d[[v]])))
  d$duracion_min <- ifelse(d$completa & !is.na(d$fin), as.numeric(difftime(d$fin, d$inicio, units = "mins")), NA)

  # Edad y categorías de cuota
  anio <- suppressWarnings(as.integer(d$year_of_birth))
  d$edad <- as.integer(format(d$inicio, "%Y")) - anio
  d$tramo_edad <- cut(d$edad, c(17, 29, 44, 59, Inf), labels = c("18-29", "30-44", "45-59", "60+"))
  d$educ_cuota <- unname(EDUC_CUOTA[d$education])
  d$sexo_cuota <- c("0" = "Hombre", "1" = "Mujer")[d$sexo]

  # Sliders: si se marcó "Prefiero no responder", el valor del slider no cuenta
  for (v in SLIDERS) {
    nr <- d[[paste0(v, "_nr")]] == COD_NR
    valor <- suppressWarnings(as.numeric(d[[v]]))
    valor[nr] <- NA
    d[[paste0(v, "_num")]] <- valor
    d[[paste0(v, "_es_nr")]] <- nr
  }

  # Sistema operativo a partir del navegador ("Chrome v154, Windows")
  so <- trimws(sub(".*,", "", d$browser))
  d$dispositivo <- ifelse(so %in% c("Android", "iOS"), "Móvil",
                          ifelse(nzchar(so), "Computador", NA))
  d$sistema <- ifelse(nzchar(so), so, NA)

  # Bloque más avanzado alcanzado según el orden de la forma asignada
  alcanzado <- vapply(names(BLOQUES_FLUJO), function(b) {
    vars <- intersect(BLOQUES_FLUJO[[b]], names(d))
    rowSums(vapply(vars, function(v) respondida(d[[v]]), logical(nrow(d)))) > 0
  }, logical(nrow(d)))
  if (is.null(dim(alcanzado))) alcanzado <- matrix(alcanzado, nrow = nrow(d), dimnames = list(NULL, names(BLOQUES_FLUJO)))
  d$ultimo_bloque <- vapply(seq_len(nrow(d)), function(i) {
    orden <- ORDEN_FORMA[[d$version_encuesta[i]]]
    if (is.null(orden)) orden <- ORDEN_FORMA$A
    hechos <- orden[alcanzado[i, orden]]
    if (length(hechos) == 0) "Sin respuestas" else hechos[length(hechos)]
  }, "")

  d
}

# Momento desde el que se cuentan las sesiones (NULL: sin corte). Se lee al
# iniciar la app, después de cargar .env.
inicio_conteo <- function() {
  x <- trimws(Sys.getenv("MONITOR_DESDE", INICIO_CONTEO_DEFAULT))
  if (!nzchar(x) || tolower(x) == "todas") return(NULL)
  desde <- as.POSIXct(x, tz = ZONA_HORARIA)
  # Misma zona que d$inicio, para comparar sin avisos
  attr(desde, "tzone") <- "UTC"
  desde
}

# Deja fuera las sesiones iniciadas antes del corte (y las sin hora de inicio)
desde_corte <- function(d, desde) {
  if (is.null(desde)) return(d)
  d[!is.na(d$inicio) & d$inicio >= desde, , drop = FALSE]
}

# Aplica los filtros de la barra lateral
filtrar <- function(d, casos, forma, rango) {
  d <- d[d$acepta, , drop = FALSE]
  if (casos == "completas") d <- d[d$completa, , drop = FALSE]
  if (forma != "ambas") d <- d[d$version_encuesta == forma, , drop = FALSE]
  if (!is.null(rango) && length(rango) == 2 && !any(is.na(rango))) {
    d <- d[!is.na(d$fecha) & d$fecha >= rango[1] & d$fecha <= rango[2], , drop = FALSE]
  }
  d
}

# Indicadores generales ----------------------------------------------------------

indicadores <- function(d) {
  hoy <- as.Date(format(Sys.time(), tz = ZONA_HORARIA))
  acep <- d[d$acepta, , drop = FALSE]
  list(
    iniciadas = nrow(d),
    aceptan = nrow(acep),
    rechazan = sum(d$consent_understand == "no"),
    completas = sum(acep$completa),
    tasa = if (nrow(acep) > 0) mean(acep$completa) else NA,
    hoy = sum(acep$completa & acep$fecha == hoy, na.rm = TRUE),
    hoy_iniciadas = sum(acep$fecha == hoy, na.rm = TRUE),
    duracion_mediana = stats::median(acep$duracion_min, na.rm = TRUE),
    ultima = if (nrow(d) > 0) max(d$inicio, na.rm = TRUE) else NA
  )
}

serie_diaria <- function(d) {
  acep <- d[d$acepta & !is.na(d$fecha), , drop = FALSE]
  if (nrow(acep) == 0) return(data.frame(fecha = as.Date(character(0)), estado = character(0), n = integer(0)))
  dias <- seq(min(acep$fecha), max(acep$fecha), by = "day")
  acep$estado <- ifelse(acep$completa, "Completas", "Incompletas")
  conteo <- dplyr::count(acep, fecha, estado)
  base <- expand.grid(fecha = dias, estado = c("Completas", "Incompletas"), stringsAsFactors = FALSE)
  out <- dplyr::left_join(base, conteo, by = c("fecha", "estado"))
  out$n[is.na(out$n)] <- 0L
  out
}

acumulado <- function(d) {
  comp <- d[d$completa & !is.na(d$fin), , drop = FALSE]
  if (nrow(comp) == 0) return(data.frame(momento = as.POSIXct(character(0)), n = integer(0)))
  comp <- comp[order(comp$fin), ]
  data.frame(momento = comp$fin, n = seq_len(nrow(comp)))
}

abandono <- function(d) {
  inc <- d[d$acepta & !d$completa, , drop = FALSE]
  if (nrow(inc) == 0) return(NULL)
  filas <- lapply(names(ORDEN_FORMA), function(f) {
    bloques <- c("Sin respuestas", ORDEN_FORMA[[f]])
    x <- inc$ultimo_bloque[inc$version_encuesta == f]
    data.frame(
      forma = paste("Forma", f),
      clave = sprintf("%s|%02d|%s", f, seq_along(bloques),
                      ifelse(bloques == "Sin respuestas", "Ninguno (solo consentimiento)", bloques)),
      n = vapply(bloques, function(b) sum(x == b), 0L),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, filas)
}

# Frecuencias ---------------------------------------------------------------------

# Tabla de frecuencias de una variable categórica, con etiquetas
frecuencias <- function(d, var, niveles = NULL, etiquetas = TRUE) {
  x <- d[[var]]
  x <- x[respondida(x)]
  if (is.null(niveles)) niveles <- niveles_de(var)
  niveles <- unique(c(niveles, sort(setdiff(unique(x), niveles))))
  tab <- table(factor(x, levels = niveles))
  out <- data.frame(codigo = names(tab), n = as.integer(tab), stringsAsFactors = FALSE)
  out$categoria <- if (etiquetas) etiquetar(var, out$codigo) else out$codigo
  total <- sum(out$n)
  out$pct <- if (total > 0) out$n / total else 0
  out
}

# Comparación con el marco de cuotas
cuotas <- function(d, var) {
  marco <- MARCO_CUOTAS[MARCO_CUOTAS$variable == var, ]
  x <- d[[var]]
  x <- x[!is.na(x)]
  obs <- table(factor(as.character(x), levels = marco$categoria))
  data.frame(
    categoria = factor(marco$categoria, levels = marco$categoria),
    n = as.integer(obs),
    pct = if (sum(obs) > 0) as.numeric(obs) / sum(obs) else 0,
    p_meta = marco$p_meta,
    sin_dato = sum(is.na(d[[var]]))
  )
}

# Escalas de 5 puntos ---------------------------------------------------------------

resumen_escala <- function(d, modulo) {
  items <- MODULOS_ESCALAS[[modulo]]$items
  filas <- lapply(names(items), function(item) {
    x <- d[[item]]
    x <- x[respondida(x)]
    validos <- suppressWarnings(as.integer(x[!x %in% c(COD_NS, COD_NR)]))
    data.frame(
      item = item,
      etiqueta = unname(items[[item]]),
      n = length(x),
      media = if (length(validos)) mean(validos) else NA,
      de = if (length(validos) > 1) stats::sd(validos) else NA,
      pct_alto = if (length(validos)) mean(validos >= 4) else NA,
      pct_ns = if (length(x)) mean(x == COD_NS) else NA,
      pct_nr = if (length(x)) mean(x == COD_NR) else NA,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, filas)
}

distribucion_escala <- function(d, modulo) {
  items <- MODULOS_ESCALAS[[modulo]]$items
  filas <- lapply(names(items), function(item) {
    x <- d[[item]]
    x <- x[respondida(x) & !x %in% c(COD_NS, COD_NR)]
    tab <- table(factor(x, levels = as.character(1:5)))
    data.frame(
      item = item,
      etiqueta = unname(items[[item]]),
      codigo = names(tab),
      categoria = etiquetar(item, names(tab)),
      n = as.integer(tab),
      pct = if (sum(tab) > 0) as.numeric(tab) / sum(tab) else 0,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, filas)
}

# Conjoint, decisiones y prioridades -------------------------------------------------

resumen_conjoint <- function(d) {
  tareas <- c("cbc_practice", paste0("cbc_q", 1:6))
  nombres <- c("Práctica", paste("Tarea", 1:6))
  filas <- lapply(seq_along(tareas), function(i) {
    v <- tareas[i]
    respondio <- respondida(d[[v]]) | d[[paste0(v, "_es_nr")]]
    x <- d[[paste0(v, "_num")]][respondio]
    data.frame(
      tarea = nombres[i],
      n = sum(respondio),
      media = mean(x, na.rm = TRUE),
      de = stats::sd(x, na.rm = TRUE),
      pct_medio = mean(x == 1e6, na.rm = TRUE),
      pct_extremo = mean(x %in% c(0, 2e6), na.rm = TRUE),
      pct_nr = mean(is.na(x)),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, filas)
}

conjoint_largo <- function(d) {
  tareas <- paste0("cbc_q", 1:6)
  out <- do.call(rbind, lapply(seq_along(tareas), function(i) {
    x <- d[[paste0(tareas[i], "_num")]]
    x <- x[!is.na(x)]
    data.frame(tarea = rep(paste("Tarea", i), length(x)), monto = x)
  }))
  out$tarea <- factor(out$tarea, levels = paste("Tarea", 1:6))
  out
}

# La tarea 6 repite los perfiles de la práctica con los lados invertidos: una
# respuesta consistente cumple práctica + tarea 6 ≈ 2.000.000.
consistencia_repetida <- function(d, tolerancia = 2e5) {
  p <- d$cbc_practice_num
  q6 <- d$cbc_q6_num
  ok <- !is.na(p) & !is.na(q6)
  list(
    n = sum(ok),
    pct = if (sum(ok)) mean(abs(p[ok] - (2e6 - q6[ok])) <= tolerancia) else NA,
    r = if (sum(ok) > 2) suppressWarnings(stats::cor(p[ok], 2e6 - q6[ok])) else NA,
    puntos = data.frame(practica = p[ok], q6_invertida = 2e6 - q6[ok])
  )
}

resumen_ranking <- function(d) {
  filas <- lapply(names(AREAS_RANKING), function(a) {
    x <- suppressWarnings(as.integer(d[[paste0("ranking_politicas_", a)]]))
    respondio <- rowSums(vapply(names(AREAS_RANKING), function(b) {
      respondida(d[[paste0("ranking_politicas_", b)]])
    }, logical(nrow(d)))) > 0
    x <- x[respondio]
    data.frame(
      area = unname(AREAS_RANKING[[a]]),
      n = sum(!is.na(x)),
      media = mean(x, na.rm = TRUE),
      pct_primera = if (length(x)) mean(x %in% 1L) else NA,
      pct_sin = if (length(x)) mean(is.na(x)) else NA,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, filas)
}

# Calidad --------------------------------------------------------------------------

calidad <- function(d) {
  comp <- d[d$completa, , drop = FALSE]
  items_merec <- names(MODULOS_ESCALAS[["Merecimiento"]]$items)
  plano <- if (nrow(comp)) {
    m <- as.matrix(comp[, items_merec, drop = FALSE])
    apply(m, 1, function(r) {
      r <- r[respondida(r) & !r %in% c(COD_NS, COD_NR)]
      length(r) >= 10 && length(unique(r)) == 1
    })
  } else logical(0)
  todos_items <- unlist(lapply(MODULOS_ESCALAS, function(m) names(m$items)), use.names = FALSE)
  nsnr_persona <- if (nrow(comp)) {
    m <- as.matrix(comp[, todos_items, drop = FALSE])
    rowMeans(m == COD_NS | m == COD_NR)
  } else numeric(0)
  med <- stats::median(comp$duracion_min, na.rm = TRUE)
  list(
    n = nrow(comp),
    atencion = if (nrow(comp)) mean(!respondida(comp$attention_check)) else NA,
    manipulacion = if (nrow(comp)) mean(comp$manip_check == "reparto") else NA,
    rapidos = if (nrow(comp)) mean(comp$duracion_min < med / 3, na.rm = TRUE) else NA,
    umbral_rapido = med / 3,
    plano = if (length(plano)) mean(plano) else NA,
    nsnr_alto = if (length(nsnr_persona)) mean(nsnr_persona > 0.25) else NA,
    nsnr_persona = nsnr_persona
  )
}

items_mas_nsnr <- function(d, k = 10) {
  comp <- d[d$completa, , drop = FALSE]
  todos <- unlist(lapply(MODULOS_ESCALAS, function(m) m$items))
  names(todos) <- sub("^.*\\.", "", names(todos))
  extra <- c(income = "Ingreso del hogar", adc_qstn = "Situación económica del hogar")
  vars <- c(todos, extra)
  out <- data.frame(
    item = names(vars),
    etiqueta = unname(vars),
    pct_ns = vapply(names(vars), function(v) mean(comp[[v]] == COD_NS), 0),
    pct_nr = vapply(names(vars), function(v) mean(comp[[v]] == COD_NR), 0),
    stringsAsFactors = FALSE
  )
  sliders_nr <- c(posicion_politica = "Posición política", estatus_subjetivo = "Estatus subjetivo")
  out <- rbind(out, data.frame(
    item = names(sliders_nr), etiqueta = unname(sliders_nr), pct_ns = 0,
    pct_nr = vapply(names(sliders_nr), function(v) mean(comp[[paste0(v, "_es_nr")]]), 0),
    stringsAsFactors = FALSE
  ))
  out$pct_total <- out$pct_ns + out$pct_nr
  out <- out[order(-out$pct_total), ]
  utils::head(out, k)
}
