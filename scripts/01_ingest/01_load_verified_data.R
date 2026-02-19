# ==============================================================================
# 01_ingest/01_load_verified_data.R
# Carga robusta de datos verificados FIP (incluye normalización GAO)
# ==============================================================================

suppressPackageStartupMessages({
  library(fs)
  library(readr)
  library(dplyr)
  library(stringr)
  library(janitor)
  library(logger)
  library(rlang)
})

# ------------------------------------------------------------------------------
# Helpers
# ------------------------------------------------------------------------------

.fail <- function(...) stop(paste0(...), call. = FALSE)

# Devuelve primer match de una lista de candidatos presente en names(df)
.find_col <- function(df, candidates) {
  nms <- names(df)
  hit <- intersect(candidates, nms)
  if (length(hit) == 0) return(NA_character_)
  hit[[1]]
}

# Renombra si encuentra candidato
.rename_if_found <- function(df, target, candidates) {
  hit <- .find_col(df, candidates)
  if (!is.na(hit) && hit != target) {
    df <- dplyr::rename(df, !!target := !!sym(hit))
  }
  df
}

# Normaliza GAO: lower, trim, colapsa espacios
.norm_gao <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_trim() %>%
    stringr::str_squish() %>%
    stringr::str_to_lower()
}

# ------------------------------------------------------------------------------
# API
# ------------------------------------------------------------------------------

#' load_verified_data
#' Lee data/verified y produce lista con tablas estandarizadas.
#' Requisito mínimo GAO: gao, year, total.
load_verified_data <- function(
    verified_dir = NULL,
    gao_file = NULL
) {
  # 1) Resolver rutas
  project_root <- getwd()
  
  if (is.null(verified_dir)) {
    verified_dir <- fs::path(project_root, "data", "verified")
  }
  if (!fs::dir_exists(verified_dir)) {
    .fail("No existe la carpeta verified: ", verified_dir)
  }
  
  log_info("Iniciando carga de datos verificados FIP")
  log_info("Ruta verified: {verified_dir}")
  
  # 2) Detectar archivo GAO (si no viene explícito)
  if (is.null(gao_file)) {
    # patrones típicos
    candidates <- fs::dir_ls(verified_dir, regexp = "(?i)gao.*\\.(csv|xlsx)$")
    if (length(candidates) == 0) {
      # fallback: cualquier csv con 'gao' en header no lo vamos a leer (caro)
      .fail("No encontré archivo GAO en verified (esperaba algo tipo gao*.csv).")
    }
    gao_file <- candidates[[1]]
  } else {
    gao_file <- fs::path(verified_dir, gao_file)
  }
  
  if (!fs::file_exists(gao_file)) {
    .fail("No existe el archivo GAO: ", gao_file)
  }
  
  log_info("Leyendo GAO desde: {gao_file}")
  
  # 3) Leer GAO (csv)
  #    (si tienes GAO en xlsx, lo ajustamos luego; por ahora tu error dice CSV)
  gao_raw <- readr::read_csv(gao_file, show_col_types = FALSE) %>%
    janitor::clean_names()
  
  log_info("Columnas GAO (clean_names): {paste(names(gao_raw), collapse = ', ')}")
  
  # 4) Mapear aliases → gao/year/total
  gao <- gao_raw
  
  # --- year aliases
  gao <- .rename_if_found(gao, "year", c(
    "year", "anio", "ano", "a_o", "vigencia", "periodo", "period", "fecha_year"
  ))
  
  # --- total aliases
  gao <- .rename_if_found(gao, "total", c(
    "total", "count", "n", "casos", "eventos", "events", "conteo", "num", "numero", "cantidad"
  ))
  
  # --- gao aliases
  gao <- .rename_if_found(gao, "gao", c(
    "gao", "grupo", "actor", "organizacion", "organizacion_criminal", "organization",
    "grupo_armado", "grupo_armado_organizado", "grupo_armado_organizado_gao"
  ))
  
  # 5) Validar columnas mínimas (después de renombrar)
  needed <- c("gao", "year", "total")
  missing <- setdiff(needed, names(gao))
  if (length(missing) > 0) {
    .fail(
      "GAO no trae columnas mínimas: gao, year, total.\n",
      "Faltan: ", paste(missing, collapse = ", "), "\n",
      "Columnas disponibles (clean_names): ", paste(names(gao_raw), collapse = ", "), "\n",
      "Solución rápida: renombra columnas en el CSV o añade el alias en 01_load_verified_data.R."
    )
  }
  
  # 6) Limpieza y tipado
  gao <- gao %>%
    mutate(
      gao  = .norm_gao(.data$gao),
      year = suppressWarnings(as.integer(.data$year)),
      total = suppressWarnings(as.numeric(.data$total))
    ) %>%
    filter(!is.na(.data$gao), .data$gao != "", !is.na(.data$year), !is.na(.data$total))
  
  # 7) Chequeos mínimos
  if (nrow(gao) == 0) {
    .fail("GAO quedó vacío tras limpieza. Revisa year/total/gao y formatos numéricos.")
  }
  
  if (any(is.na(gao$year)) || any(is.na(gao$total))) {
    .fail("GAO tiene NA en year/total tras conversión. Revisa separadores y formatos.")
  }
  
  # 8) Dejar listo para el pipeline
  out <- list(
    gao = gao,
    meta = list(
      verified_dir = verified_dir,
      gao_file = gao_file,
      loaded_at = Sys.time()
    )
  )
  
  assign("FIP_VERIFIED", out, envir = .GlobalEnv)
  
  log_info("GAO OK: filas={nrow(gao)} | años={length(unique(gao$year))} | gaos={length(unique(gao$gao))}")
  invisible(out)
}

library(readr)
library(janitor)
library(dplyr)
library(stringr)

f <- "data/verified/TU_ARCHIVO_GAO.csv"  # <-- pon el nombre real

x_raw <- read_csv(f, show_col_types = FALSE)
cat("\n--- NOMBRES RAW ---\n")
print(names(x_raw))

x <- x_raw |> clean_names()
cat("\n--- NOMBRES clean_names() ---\n")
print(names(x))

cat("\n--- HEAD (clean) ---\n")
print(head(x, 3))


