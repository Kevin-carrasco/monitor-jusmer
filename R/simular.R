# Respuestas simuladas --------------------------------------------------------
#
# Genera una base con la misma estructura que la tabla `responses` que
# surveydown escribe en Supabase (una fila por sesión, todas las columnas como
# texto, celdas vacías para lo no respondido). Sirve para ver la app
# funcionando antes de conectarla a los datos reales. Los valores son
# inventados: no tienen ningún valor sustantivo.

# Media simulada de cada ítem en la escala 1-5
MEDIAS_SIM <- c(
  mj_1 = 2.0, mj_2 = 1.9, mj_3 = 2.1, ref_1 = 2.6, ref_2 = 2.5, ref_3 = 2.7,
  proy_1 = 2.8, proy_2 = 2.7, lucro_1 = 2.0, lucro_2 = 2.1, lucro_3 = 1.7,
  control_1 = 3.5, control_2 = 3.2, control_3 = 3.7, actitud_1 = 3.4,
  actitud_2 = 3.8, reciproc_1 = 3.3, reciproc_2 = 3.4, reciproc_3 = 3.6,
  actitud_4 = 3.3, id_1 = 3.0, id_2 = 3.1, id_3 = 3.2, nec_1 = 3.2,
  nec_2 = 3.4, nec_3 = 2.9, esf_1 = 4.2, esf_2 = 3.8, esf_3 = 2.6,
  esf_4 = 3.6, esf_5 = 3.0, merit_1 = 2.4, merit_2 = 2.6, merit_3 = 4.3,
  merit_4 = 4.4, merit_5 = 4.0, merit_6 = 3.7, merit_7 = 2.0, merit_8 = 2.1,
  adc_5 = 3.2, adc_6 = 2.7, adc_7 = 3.8
)

simular_respuestas <- function(n, desde = Sys.time() - 9 * 86400,
                               hasta = Sys.time(), semilla = NULL) {
  if (!is.null(semilla)) set.seed(semilla)
  vacia <- as.data.frame(
    matrix(character(0), nrow = 0, ncol = 0),
    stringsAsFactors = FALSE
  )
  if (n <= 0) return(vacia)

  pick <- function(valores, prob = NULL, size = n) {
    sample(valores, size, replace = TRUE, prob = prob)
  }
  a_pasos <- function(x, paso, min, max) {
    as.character(pmin(max, pmax(min, round(x / paso) * paso)))
  }

  d <- data.frame(
    session_id = vapply(seq_len(n), function(i) {
      paste(sample(c(0:9, letters[1:6]), 32, replace = TRUE), collapse = "")
    }, ""),
    stringsAsFactors = FALSE
  )

  # Inicio: más sesiones hacia el final del periodo, como un campo que acelera
  inicio <- desde + as.numeric(difftime(hasta, desde, units = "secs")) * sqrt(stats::runif(n))
  d$time_start <- format(inicio, "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")
  d$version_encuesta <- pick(c("A", "B"))
  d$browser <- pick(
    c("Chrome v154, Windows", "Chrome v154, Android", "Safari v18, iOS",
      "Safari v18, Mac OS", "Edge v140, Windows", "Firefox v143, Windows"),
    prob = c(0.30, 0.27, 0.22, 0.08, 0.09, 0.04)
  )
  d$consent_understand <- pick(c("yes", "no"), prob = c(0.97, 0.03))

  # Información general
  d$sexo <- pick(c("0", "1", "99"), prob = c(0.475, 0.515, 0.01))
  edad <- round(18 + stats::rbeta(n, 1.4, 1.9) * 67)
  d$year_of_birth <- ifelse(stats::runif(n) < 0.02, "prefer_not_say", as.character(2026 - edad))
  d$education <- pick(
    c(as.character(0:9), "prefer_not_to_say"),
    prob = c(0.01, 0.07, 0.08, 0.10, 0.27, 0.05, 0.09, 0.08, 0.19, 0.05, 0.01)
  )

  # Conjoint: monto (CLP) asignado al Postulante B, de 0 a 2.000.000
  slider_cbc <- function() {
    x <- stats::rnorm(n, 1e6, 450000)
    x[stats::runif(n) < 0.15] <- 1e6  # quienes dejan el slider al medio
    a_pasos(x, 1e5, 0, 2e6)
  }
  d$cbc_practice <- slider_cbc()
  d$manip_check <- pick(c("reparto", "eleccion", "universidad"), prob = c(0.88, 0.08, 0.04))
  for (q in 1:5) d[[paste0("cbc_q", q)]] <- slider_cbc()
  # La tarea 6 repite la de práctica con los lados invertidos
  consistente <- stats::runif(n) < 0.75
  q6 <- 2e6 - as.numeric(d$cbc_practice) + stats::rnorm(n, 0, 150000)
  d$cbc_q6 <- ifelse(consistente, a_pasos(q6, 1e5, 0, 2e6), slider_cbc())
  for (v in c("cbc_practice", paste0("cbc_q", 1:6))) {
    d[[paste0(v, "_nr")]] <- ifelse(stats::runif(n) < 0.02, COD_NR, "")
  }

  # Escalas de 5 puntos con un rasgo latente por módulo
  latentes <- lapply(MODULOS_ESCALAS, function(m) stats::rnorm(n))
  invertidos <- c("mj_1", "mj_2", "mj_3", "ref_1", "ref_2", "ref_3", "proy_1", "proy_2",
                  "control_1", "control_2", "control_3", "actitud_2", "actitud_4",
                  "id_2", "esf_3", "esf_5", "merit_3", "merit_4")
  for (modulo in names(MODULOS_ESCALAS)) {
    for (item in names(MODULOS_ESCALAS[[modulo]]$items)) {
      signo <- if (item %in% invertidos) -1 else 1
      x <- MEDIAS_SIM[[item]] + 0.8 * signo * latentes[[modulo]] + stats::rnorm(n, 0, 0.8)
      x <- as.character(pmin(5, pmax(1, round(x))))
      u <- stats::runif(n)
      x[u < 0.03] <- COD_NS
      x[u >= 0.03 & u < 0.045] <- COD_NR
      d[[item]] <- x
    }
  }
  # Una fracción responde igual a toda la batería de merecimiento
  plano <- stats::runif(n) < 0.03
  valor_plano <- pick(c("3", "4", "5"))
  for (item in names(MODULOS_ESCALAS[["Merecimiento"]]$items)) {
    d[[item]][plano] <- valor_plano[plano]
  }

  d$attention_check <- ifelse(
    stats::runif(n) < 0.9, "",
    pick(c("muy_acuerdo", "acuerdo", "desacuerdo", "muy_desacuerdo"))
  )

  # Prioridades de inversión: un orden por persona; algunas asignan solo 3
  pesos <- c(educacion = 0.30, salud = 0.34, pensiones = 0.22, cuidado_ninos = 0.10, carreteras = 0.04)
  ordenes <- t(vapply(seq_len(n), function(i) {
    orden <- sample(names(pesos), 5, prob = pesos)
    pos <- stats::setNames(as.character(1:5), orden)[names(pesos)]
    if (stats::runif(1) < 0.15) pos[pos %in% c("4", "5")] <- ""
    pos
  }, character(5)))
  for (a in names(pesos)) d[[paste0("ranking_politicas_", a)]] <- ordenes[, a]

  # Decisiones
  d$dec_3 <- a_pasos(stats::rexp(n, 1 / 15), 10, 0, 100)
  d$dec_1 <- a_pasos(stats::rgamma(n, 2, 1 / 12), 10, 0, 100)
  d$dec_2 <- ifelse(d$dec_1 == "0", "", a_pasos(stats::rexp(n, 1 / 12), 10, 0, 100))
  for (v in c("dec_3", "dec_1")) {
    d[[paste0(v, "_nr")]] <- ifelse(stats::runif(n) < 0.03, COD_NR, "")
  }
  d$dec_2[d$dec_1_nr == COD_NR] <- ""
  d$dec_2_nr <- ifelse(d$dec_2 != "" & stats::runif(n) < 0.03, COD_NR, "")

  # Situación económica y cierre
  d$adc_qstn <- pick(c("4", "3", "2", "1", COD_NS, COD_NR), prob = c(0.14, 0.38, 0.33, 0.11, 0.01, 0.03))
  d$gender <- ifelse(d$sexo == "0", "male", ifelse(d$sexo == "1", "female", "non_binary"))
  d$gender[stats::runif(n) < 0.02] <- COD_NR
  d$income <- pick(
    c(as.character(1:10), COD_NS, COD_NR),
    prob = c(0.07, 0.10, 0.11, 0.11, 0.10, 0.09, 0.08, 0.08, 0.07, 0.07, 0.05, 0.07)
  )
  d$estatus_subjetivo <- a_pasos(stats::rnorm(n, 4.8, 1.7), 1, 0, 10)
  d$posicion_politica <- a_pasos(stats::rnorm(n, 5, 2.6), 1, 0, 10)
  d$estatus_subjetivo_nr <- ifelse(stats::runif(n) < 0.02, COD_NR, "")
  d$posicion_politica_nr <- ifelse(stats::runif(n) < 0.09, COD_NR, "")
  d$salud_prov <- pick(c("fonasa", "isapre"), prob = c(0.78, 0.22))
  d$educ_prov <- pick(c("privado", "subvencionado", "municipal"), prob = c(0.12, 0.50, 0.38))

  # Duración y término: mediana ~17 minutos, con algunos casos muy rápidos
  duracion <- stats::rlnorm(n, log(17 * 60), 0.45)
  rapidos <- stats::runif(n) < 0.04
  duracion[rapidos] <- stats::runif(sum(rapidos), 4 * 60, 6 * 60)
  termino <- inicio + duracion

  # Abandono: quién no termina y en qué bloque de su forma se detiene
  abandona <- stats::runif(n) < 0.20 | termino > hasta
  bloque_corte <- sample(1:6, n, replace = TRUE, prob = c(0.18, 0.34, 0.16, 0.14, 0.12, 0.06))
  fraccion <- stats::runif(n, 0, 0.8)

  for (i in which(abandona | d$consent_understand == "no")) {
    bloques <- ORDEN_FORMA[[d$version_encuesta[i]]]
    if (d$consent_understand[i] == "no") {
      borrar <- unlist(BLOQUES_FLUJO, use.names = FALSE)
    } else {
      b <- bloque_corte[i]
      vars_b <- BLOQUES_FLUJO[[bloques[b]]]
      conservar <- floor(fraccion[i] * length(vars_b))
      borrar <- c(
        vars_b[seq_along(vars_b) > conservar],
        unlist(BLOQUES_FLUJO[bloques[seq_along(bloques) > b]], use.names = FALSE)
      )
    }
    borrar <- c(borrar, paste0(intersect(borrar, SLIDERS), "_nr"))
    borrar <- intersect(borrar, names(d))
    d[i, borrar] <- ""
  }

  d$time_end <- ifelse(
    abandona | d$consent_understand == "no", "",
    format(termino, "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")
  )
  d
}
