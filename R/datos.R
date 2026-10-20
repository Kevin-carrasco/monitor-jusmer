# Fuente de datos -------------------------------------------------------------
#
# El monitor funciona en dos modos, elegidos con la variable de entorno
# MONITOR_MODO:
#
#   demo  (por defecto) respuestas simuladas con R/simular.R; cada
#         actualización agrega algunas sesiones nuevas para ver la app "en vivo".
#   base  lectura directa de la tabla de respuestas en Supabase (Postgres),
#         la misma que escribe la encuesta. No se descarga nada a mano: la app
#         consulta la base cada REFRESCO_SEG segundos.
#
# Credenciales del modo base: las mismas variables SD_* que usa surveydown
# (SD_HOST, SD_PORT, SD_DBNAME, SD_USER, SD_PASSWORD, SD_TABLE), leídas desde
# el entorno o desde un archivo .env en la carpeta de la app. Se recomienda un
# usuario de Postgres con permiso de solo lectura (ver README).

modo_datos <- function() {
  if (file.exists(".env")) readRenviron(".env")
  modo <- tolower(Sys.getenv("MONITOR_MODO", "demo"))
  if (!modo %in% c("demo", "base")) modo <- "demo"
  modo
}

# Columnas que el monitor necesita. Se excluyen a propósito ip_address,
# cbc_profiles, los *_hist y los time_q_*: no se usan y no conviene moverlas.
columnas_monitor <- function() {
  unique(c(
    "session_id", "time_start", "time_end", "version_encuesta", "browser",
    "consent_understand",
    unlist(BLOQUES_FLUJO, use.names = FALSE),
    "attention_check",
    paste0(SLIDERS, "_nr")
  ))
}

conectar_base <- function() {
  for (pkg in c("DBI", "RPostgres")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Falta el paquete ", pkg, " para el modo base: install.packages(\"", pkg, "\")")
    }
  }
  faltan <- c("SD_HOST", "SD_DBNAME", "SD_USER", "SD_PASSWORD")
  faltan <- faltan[!nzchar(Sys.getenv(faltan))]
  if (length(faltan) > 0) {
    stop("Faltan variables de entorno: ", paste(faltan, collapse = ", "))
  }
  DBI::dbConnect(
    RPostgres::Postgres(),
    host       = Sys.getenv("SD_HOST"),
    port       = as.integer(Sys.getenv("SD_PORT", "5432")),
    dbname     = Sys.getenv("SD_DBNAME"),
    user       = Sys.getenv("SD_USER"),
    password   = Sys.getenv("SD_PASSWORD"),
    sslmode    = Sys.getenv("SD_SSLMODE", "require"),
    gssencmode = "disable"
  )
}

# Lee la tabla completa (solo las columnas necesarias). Una conexión por
# lectura: con el pooler de Supabase y una consulta por minuto es lo más simple
# y evita conexiones colgadas.
leer_base <- function() {
  con <- conectar_base()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  tabla <- Sys.getenv("SD_TABLE", "jusmer_ola1_responses")
  existentes <- DBI::dbListFields(con, tabla)
  cols <- intersect(columnas_monitor(), existentes)
  consulta <- sprintf(
    "SELECT %s FROM %s",
    paste(DBI::dbQuoteIdentifier(con, cols), collapse = ", "),
    DBI::dbQuoteIdentifier(con, tabla)
  )
  d <- DBI::dbGetQuery(con, consulta)
  d[] <- lapply(d, function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    x
  })
  d
}

# Crea, para una sesión de Shiny, un reactive con los datos crudos que se
# refresca solo cada REFRESCO_SEG segundos o al apretar `boton`.
crear_fuente <- function(modo, base_demo, boton, session) {
  datos <- reactiveVal(NULL)
  error <- reactiveVal(NULL)
  actualizado <- reactiveVal(NULL)

  leer <- function() {
    if (modo == "demo") {
      previo <- datos()
      if (is.null(previo)) return(base_demo)
      ultima <- max(as.POSIXct(previo$time_start, tz = "UTC", format = "%Y-%m-%d %H:%M:%S"))
      nuevas <- simular_respuestas(stats::rpois(1, 1.5), desde = ultima, hasta = Sys.time())
      if (nrow(nuevas) == 0) return(previo)
      dplyr::bind_rows(previo, nuevas)
    } else {
      leer_base()
    }
  }

  refrescar <- function() {
    resultado <- tryCatch(leer(), error = function(e) e)
    if (inherits(resultado, "error")) {
      error(conditionMessage(resultado))
    } else {
      error(NULL)
      datos(resultado)
      actualizado(Sys.time())
    }
  }

  observe({
    invalidateLater(REFRESCO_SEG * 1000, session)
    isolate(refrescar())
  })
  observeEvent(boton(), refrescar(), ignoreInit = TRUE)

  list(datos = datos, error = error, actualizado = actualizado)
}
