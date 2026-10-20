# Gráficos --------------------------------------------------------------------
#
# Marcas delgadas, grilla tenue, un solo color para series únicas y etiquetas
# selectivas. Las tablas debajo de cada gráfico entregan los valores exactos.

library(ggplot2)

tema_monitor <- function(base = 11) {
  theme_minimal(base_size = base) +
    theme(
      plot.background = element_rect(fill = COLORES$superficie, colour = NA),
      panel.background = element_rect(fill = COLORES$superficie, colour = NA),
      panel.grid.major = element_line(colour = COLORES$grilla, linewidth = 0.3),
      panel.grid.minor = element_blank(),
      axis.text = element_text(colour = COLORES$tinta2),
      axis.title = element_text(colour = COLORES$tenue, size = base * 0.85),
      plot.title = element_blank(),
      legend.position = "top",
      legend.justification = "left",
      legend.title = element_blank(),
      legend.text = element_text(colour = COLORES$tinta2),
      strip.text = element_text(colour = COLORES$tinta2, face = "bold", hjust = 0),
      plot.margin = margin(8, 12, 8, 8)
    )
}

pct_es <- function(x, acc = 1) {
  ifelse(is.na(x), "–", scales::percent(x, accuracy = acc, decimal.mark = ",", big.mark = "."))
}
num_es <- function(x, acc = 1) {
  ifelse(is.na(x), "–", scales::number(x, accuracy = acc, decimal.mark = ",", big.mark = "."))
}
clp <- function(x) ifelse(is.na(x), "–", paste0("$", num_es(x)))

sin_datos <- function(texto = "Aún no hay respuestas para mostrar con estos filtros") {
  ggplot() +
    annotate("text", x = 0, y = 0, label = texto, colour = COLORES$tenue, size = 4.5) +
    theme_void() +
    theme(plot.background = element_rect(fill = COLORES$superficie, colour = NA))
}

envolver <- function(x, ancho = 38) {
  vapply(x, function(s) paste(strwrap(s, ancho), collapse = "\n"), "")
}

# Barras horizontales de frecuencias (una sola serie)
g_barras <- function(tab, orden = c("cuestionario", "frecuencia"), ancho = 34) {
  if (is.null(tab) || sum(tab$n) == 0) return(sin_datos())
  orden <- match.arg(orden)
  tab$categoria <- envolver(tab$categoria, ancho)
  niveles <- if (orden == "frecuencia") tab$categoria[order(tab$n)] else rev(unique(tab$categoria))
  tab$categoria <- factor(tab$categoria, levels = unique(niveles))
  tope <- max(tab$pct)
  ggplot(tab, aes(x = pct, y = categoria)) +
    geom_col(fill = COLORES$serie1, width = 0.62) +
    geom_text(aes(label = paste0(pct_es(pct), "  (", n, ")")),
              hjust = -0.08, size = 3.1, colour = COLORES$tinta2) +
    scale_x_continuous(limits = c(0, tope * 1.45), expand = expansion(mult = c(0, 0))) +
    labs(x = NULL, y = NULL) +
    tema_monitor() +
    theme(panel.grid.major = element_blank(), axis.text.x = element_blank())
}

# Barras observadas contra la meta del marco muestral (marca vertical)
g_cuotas <- function(tab) {
  if (is.null(tab) || sum(tab$n) == 0) return(sin_datos())
  tab$categoria <- factor(tab$categoria, levels = rev(levels(tab$categoria)))
  tope <- max(c(tab$pct, tab$p_meta))
  ggplot(tab, aes(y = categoria)) +
    geom_col(aes(x = pct), fill = COLORES$serie1, width = 0.62) +
    geom_errorbar(aes(xmin = p_meta, xmax = p_meta), width = 0.85, linewidth = 1,
                  colour = COLORES$tinta) +
    geom_text(aes(x = pmax(pct, p_meta) + tope * 0.04, label = sprintf("%s · meta %s", pct_es(pct), pct_es(p_meta))),
              hjust = 0, size = 3.0, colour = COLORES$tinta2) +
    scale_x_continuous(limits = c(0, tope * 2), expand = expansion(mult = c(0, 0))) +
    labs(x = NULL, y = NULL) +
    tema_monitor() +
    theme(panel.grid.major = element_blank(), axis.text.x = element_blank())
}

# Histograma de una variable numérica discreta (sliders)
g_histograma <- function(x, paso, etiquetas = waiver(), xlab = NULL) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(sin_datos())
  tab <- as.data.frame(table(x = x), stringsAsFactors = FALSE)
  tab$x <- as.numeric(tab$x)
  tab$pct <- tab$Freq / sum(tab$Freq)
  ggplot(tab, aes(x = x, y = pct)) +
    geom_col(fill = COLORES$serie1, width = paso * 0.78) +
    geom_vline(xintercept = stats::median(x), colour = COLORES$tinta, linewidth = 0.6) +
    annotate("label", x = stats::median(x), y = max(tab$pct) * 1.08,
             label = paste("Mediana:", if (is.function(etiquetas)) etiquetas(stats::median(x)) else num_es(stats::median(x))),
             size = 3.0, colour = COLORES$tinta, fill = COLORES$superficie, label.size = 0) +
    scale_y_continuous(labels = pct_es, expand = expansion(mult = c(0, 0.15))) +
    scale_x_continuous(labels = etiquetas, breaks = function(l) {
      paso_eje <- paso * max(1, ceiling(diff(l) / paso / 11))
      seq(ceiling(l[1] / paso_eje) * paso_eje, floor(l[2] / paso_eje) * paso_eje, by = paso_eje)
    }) +
    labs(x = xlab, y = NULL) +
    tema_monitor()
}

# Completas e incompletas por día (barras apiladas con separación)
g_diario <- function(serie) {
  if (is.null(serie) || nrow(serie) == 0 || sum(serie$n) == 0) return(sin_datos())
  serie$estado <- factor(serie$estado, levels = c("Incompletas", "Completas"))
  ggplot(serie, aes(x = fecha, y = n, fill = estado)) +
    geom_col(width = 0.72, colour = COLORES$superficie, linewidth = 0.6) +
    scale_fill_manual(values = c(Completas = COLORES$serie1, Incompletas = "#b7d3f6"),
                      breaks = c("Completas", "Incompletas")) +
    scale_x_date(date_labels = "%d/%m", date_breaks = "1 day") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
    labs(x = NULL, y = "Sesiones") +
    tema_monitor() +
    theme(panel.grid.major.x = element_blank())
}

g_acumulado <- function(acum) {
  if (is.null(acum) || nrow(acum) == 0) return(sin_datos())
  acum$momento <- as.POSIXct(format(acum$momento, tz = ZONA_HORARIA), tz = ZONA_HORARIA)
  ultimo <- acum[nrow(acum), ]
  ggplot(acum, aes(x = momento, y = n)) +
    geom_step(colour = COLORES$serie1, linewidth = 0.8) +
    geom_point(data = ultimo, colour = COLORES$serie1, size = 2.6) +
    geom_text(data = ultimo, aes(label = num_es(n)), vjust = -1, hjust = 1,
              size = 3.3, colour = COLORES$tinta) +
    scale_x_datetime(date_labels = "%d/%m") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15)), labels = num_es) +
    labs(x = NULL, y = "Encuestas completas") +
    tema_monitor()
}

g_abandono <- function(tab) {
  if (is.null(tab) || sum(tab$n) == 0) return(sin_datos("No hay encuestas incompletas"))
  # Cada forma tiene su propio orden de bloques: la clave lo conserva por panel
  tab$clave <- factor(tab$clave, levels = rev(sort(unique(tab$clave))))
  ggplot(tab, aes(x = n, y = clave)) +
    geom_col(fill = COLORES$serie1, width = 0.62) +
    geom_text(aes(label = ifelse(n > 0, n, "")), hjust = -0.25, size = 3.1, colour = COLORES$tinta2) +
    facet_wrap(~forma, scales = "free_y") +
    scale_y_discrete(labels = function(k) sub("^[^|]*[|][^|]*[|]", "", k)) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.18))) +
    labs(x = "Sesiones incompletas", y = NULL) +
    tema_monitor() +
    theme(panel.grid.major.y = element_blank())
}

# Barras apiladas al 100 % para escalas de 5 puntos
g_escala <- function(dist, modulo) {
  if (is.null(dist) || sum(dist$n) == 0) return(sin_datos())
  escala <- MODULOS_ESCALAS[[modulo]]$escala
  paleta <- if (escala == "secuencial") PALETA_SECUENCIAL else PALETA_DIVERGENTE
  # Categorías en el orden de la encuesta: de "1" a "5"
  categorias <- unique(dist$categoria[order(dist$codigo)])
  dist$categoria <- factor(dist$categoria, levels = categorias)
  dist$etiqueta <- factor(envolver(dist$etiqueta, 44), levels = rev(unique(envolver(dist$etiqueta, 44))))
  oscuro <- if (escala == "secuencial") dist$codigo %in% c("4", "5") else dist$codigo %in% c("1", "5")
  dist$color_texto <- ifelse(oscuro, "white", COLORES$tinta)
  ggplot(dist, aes(x = pct, y = etiqueta, fill = categoria, group = categoria)) +
    geom_col(width = 0.7, colour = COLORES$superficie, linewidth = 0.7,
             position = position_stack(reverse = TRUE)) +
    geom_text(aes(label = ifelse(pct >= 0.07, pct_es(pct), ""), colour = color_texto),
              position = position_stack(vjust = 0.5, reverse = TRUE), size = 2.9) +
    scale_fill_manual(values = stats::setNames(paleta, categorias)) +
    scale_colour_identity() +
    scale_x_continuous(labels = pct_es, expand = expansion(mult = c(0, 0.01))) +
    labs(x = "Porcentaje de respuestas válidas (excluye No sé y Prefiero no responder)", y = NULL) +
    guides(fill = guide_legend(nrow = 1)) +
    tema_monitor() +
    theme(panel.grid.major.y = element_blank())
}

g_conjoint <- function(largo) {
  if (is.null(largo) || nrow(largo) == 0) return(sin_datos())
  tab <- dplyr::count(largo, tarea, monto)
  tab <- dplyr::mutate(dplyr::group_by(tab, tarea), pct = n / sum(n))
  ggplot(tab, aes(x = monto, y = pct)) +
    geom_col(fill = COLORES$serie1, width = 78000) +
    geom_vline(xintercept = 1e6, colour = COLORES$tenue, linewidth = 0.4) +
    facet_wrap(~tarea, ncol = 3) +
    scale_x_continuous(breaks = c(0, 1e6, 2e6), labels = c("$0", "$1 M", "$2 M")) +
    scale_y_continuous(labels = pct_es, expand = expansion(mult = c(0, 0.08))) +
    labs(x = "Monto asignado al Postulante B", y = NULL) +
    tema_monitor(10.5) +
    theme(panel.spacing.x = unit(14, "pt"))
}

g_consistencia <- function(cons) {
  if (is.null(cons) || cons$n == 0) return(sin_datos())
  ggplot(cons$puntos, aes(x = practica, y = q6_invertida)) +
    geom_abline(slope = 1, intercept = 0, colour = COLORES$tenue, linewidth = 0.4) +
    geom_jitter(width = 25000, height = 25000, alpha = 0.45, size = 2.2,
                colour = COLORES$serie1, stroke = 0) +
    scale_x_continuous(labels = function(x) paste0("$", num_es(x / 1e3), " mil")) +
    scale_y_continuous(labels = function(x) paste0("$", num_es(x / 1e3), " mil")) +
    labs(x = "Práctica: monto al Postulante B",
         y = "Tarea 6 (lados invertidos): monto equivalente") +
    coord_equal() +
    tema_monitor(10.5)
}

g_ranking <- function(rk) {
  if (is.null(rk) || sum(rk$n) == 0) return(sin_datos())
  rk$area <- factor(rk$area, levels = rk$area[order(-rk$media)])
  ggplot(rk, aes(x = pct_primera, y = area)) +
    geom_col(fill = COLORES$serie1, width = 0.62) +
    geom_text(aes(label = sprintf("%s  ·  posición media %s", pct_es(pct_primera), num_es(media, 0.1))),
              hjust = -0.05, size = 3.1, colour = COLORES$tinta2) +
    scale_x_continuous(labels = pct_es, limits = c(0, max(rk$pct_primera, na.rm = TRUE) * 1.9),
                       expand = expansion(mult = c(0, 0))) +
    labs(x = "Porcentaje que la elige como 1° prioridad", y = NULL) +
    tema_monitor() +
    theme(panel.grid.major.y = element_blank())
}
