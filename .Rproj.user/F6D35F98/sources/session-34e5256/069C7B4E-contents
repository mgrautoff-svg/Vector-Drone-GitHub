# ==============================================================================
# 00_run_all.R — DEFENSE ANALYTICS PIPELINE (FIP-native) — ROBUSTO
# Fixes:
#  - source_safe()
#  - load_verified_data(): GAO opcional + auto-mapeo de columnas a gao/year/total
#  - No bloquea modelos FIP-native si GAO no cumple contrato
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(fs)
  library(logger)
  library(readr)
  library(janitor)
  library(stringr)
})

# ------------------------------------------------------------------------------
# 0) Helpers
# ------------------------------------------------------------------------------

source_safe <- function(file) {
  if (!fs::file_exists(file)) {
    stop("No existe el script: ", file)
  }
  source(file, local = .GlobalEnv)
  invisible(TRUE)
}

stopif_missing <- function(objs, env = .GlobalEnv) {
  miss <- objs[!vapply(objs, exists, logical(1), envir = env)]
  if (length(miss) > 0) stop("Faltan objetos requeridos en .GlobalEnv: ", paste(miss, collapse = ", "))
  invisible(TRUE)
}

# ------------------------------------------------------------------------------
# 1) main_setup(): asume que tú ya lo tienes en tu setup (si no, lo definimos mínimo)
#    Si ya existe main_setup() en tu setup script, no lo pisa.
# ------------------------------------------------------------------------------

if (!exists("main_setup", envir = .GlobalEnv)) {
  main_setup <- function() {
    # Setup mínimo: asume que estás en raíz del proyecto (getwd())
    project_root <- getwd()
    
    DIRS <- list(
      root = project_root,
      verified_data = fs::path(project_root, "data", "verified"),
      derived_data  = fs::path(project_root, "data", "derived"),
      tables        = fs::path(project_root, "outputs", "tables"),
      figures       = fs::path(project_root, "outputs", "figures"),
      logs          = fs::path(project_root, "logs"),
      scripts       = fs::path(project_root, "scripts")
    )
    
    fs::dir_create(DIRS$derived_data, recurse = TRUE)
    fs::dir_create(DIRS$tables, recurse = TRUE)
    fs::dir_create(DIRS$figures, recurse = TRUE)
    fs::dir_create(DIRS$logs, recurse = TRUE)
    
    assign("DIRS", DIRS, envir = .GlobalEnv)
    
    # logger básico
    log_formatter(formatter_glue)
    log_layout(layout_glue_colors)
    log_file <- fs::path(DIRS$logs, paste0("defense_analytics_", format(Sys.Date(), "%Y%m%d"), ".log"))
    log_appender(appender_tee(log_file))
    
    log_info("main_setup() listo. Root: {project_root}")
    return(list(project_root = project_root, dirs = DIRS, log_file = log_file))
  }
}

# ------------------------------------------------------------------------------
# 2) Loader robusto de verificados (FIP + GAO opcional)
# ------------------------------------------------------------------------------

load_verified_data <- function() {
  stopifnot(exists("DIRS", envir = .GlobalEnv))
  DIRS <- get("DIRS", envir = .GlobalEnv)
  
  log_info("Iniciando carga de datos verificados")
  log_info("Ruta verified: {DIRS$verified_data}")
  
  # ---- FIP (crítico)
  fip_file <- fs::path(DIRS$verified_data, "base_datos_fip_2025_CORREGIDA.csv")
  if (!fs::file_exists(fip_file)) {
    stop("FIP crítico faltante: ", fip_file)
  }
  fip_raw <- readr::read_csv(fip_file, show_col_types = FALSE) |> janitor::clean_names()
  assign("FIP_VERIFIED", fip_raw, envir = .GlobalEnv)
  log_info("FIP cargado: {basename(fip_file)} | n={nrow(fip_raw)}")
  
  # ---- GAO (opcional para FIP-native)
  gao_file <- fs::path(DIRS$verified_data, "fip_verified_gao_counts_2024_2025.csv")
  gao_ok <- FALSE
  gao_msg <- NA_character_
  gao_tbl <- NULL
  
  if (fs::file_exists(gao_file)) {
    x_raw <- readr::read_csv(gao_file, show_col_types = FALSE)
    x <- janitor::clean_names(x_raw)
    
    # Auto-map: detecta columnas plausibles
    nms <- names(x)
    
    pick_first <- function(patterns) {
      hit <- nms[str_detect(nms, paste(patterns, collapse = "|"))]
      if (length(hit) > 0) hit[[1]] else NA_character_
    }
    
    col_gao  <- pick_first(c("^gao$", "grupo", "actor", "organizacion", "estructura"))
    col_year <- pick_first(c("^year$", "^anio$", "^ano$", "vigencia", "fecha"))
    col_tot  <- pick_first(c("^total$", "integrantes", "miembros", "size", "capacidad", "conteo", "count"))
    
    if (all(is.finite(match(c(col_gao, col_year, col_tot), nms)))) {
      gao_tbl <- x |>
        transmute(
          gao  = as.character(.data[[col_gao]]),
          year = as.integer(.data[[col_year]]),
          total = suppressWarnings(as.numeric(.data[[col_tot]]))
        ) |>
        filter(!is.na(gao), !is.na(year))
      
      # sanity
      if (nrow(gao_tbl) > 0) {
        gao_ok <- TRUE
        assign("GAO_VERIFIED", gao_tbl, envir = .GlobalEnv)
        gao_msg <- "GAO cargado y normalizado a (gao, year, total)."
        log_info("{gao_msg} n={nrow(gao_tbl)}")
      } else {
        gao_msg <- "GAO existe pero quedó vacío tras normalizar."
        log_warn("{gao_msg}")
      }
    } else {
      gao_msg <- paste0(
        "GAO no cumple contrato mínimo. Detectado: ",
        "gao=", col_gao, ", year=", col_year, ", total=", col_tot,
        ". Se marca como opcional y se omite."
      )
      log_warn("{gao_msg}")
    }
  } else {
    gao_msg <- "GAO no encontrado (opcional)."
    log_warn("{gao_msg}")
  }
  
  return(list(
    fip_file = fip_file,
    gao_file = if (fs::file_exists(gao_file)) gao_file else NA_character_,
    gao_loaded = gao_ok,
    gao_message = gao_msg
  ))
}

# ------------------------------------------------------------------------------
# 3) Builder mínimo: 01_fip_clean.csv desde FIP_VERIFIED
#    (Si tú ya tienes script de limpieza, este bloque NO lo pisa si existe build_fip_clean())
# ------------------------------------------------------------------------------

if (!exists("build_fip_clean", envir = .GlobalEnv)) {
  build_fip_clean <- function() {
    stopif_missing(c("DIRS", "FIP_VERIFIED"))
    
    DIRS <- get("DIRS", envir = .GlobalEnv)
    fip  <- get("FIP_VERIFIED", envir = .GlobalEnv)
    
    # Intento robusto: detecta columnas típicas (ajústalo si tu FIP ya está estandarizado)
    nms <- names(fip)
    pick <- function(patterns) {
      hit <- nms[str_detect(nms, paste(patterns, collapse = "|"))]
      if (length(hit) > 0) hit[[1]] else NA_character_
    }
    
    col_cat  <- pick(c("^categoria$", "categoria_"))
    col_ind  <- pick(c("^indicador$", "indicador_"))
    col_year <- pick(c("^anio$", "^year$", "^ano$"))
    col_val  <- pick(c("^valor$", "value"))
    col_uni  <- pick(c("^unidad$", "unit"))
    col_var  <- pick(c("variacion", "pct", "porcent"))
    col_note <- pick(c("nota", "fuente", "documento"))
    col_miss <- pick(c("missing", "imput", "tipo"))
    
    # Mínimos
    if (any(is.na(c(col_cat, col_ind, col_year, col_val)))) {
      stop("FIP_VERIFIED no tiene columnas mínimas detectables (categoria/indicador/año/valor). ",
           "Nombres disponibles: ", paste(nms, collapse = ", "))
    }
    
    fip_clean <- fip |>
      transmute(
        categoria = as.character(.data[[col_cat]]),
        indicador = as.character(.data[[col_ind]]),
        año       = as.integer(.data[[col_year]]),
        valor     = suppressWarnings(as.numeric(.data[[col_val]])),
        unidad    = if (!is.na(col_uni)) as.character(.data[[col_uni]]) else NA_character_,
        variacion_porcentual = if (!is.na(col_var)) suppressWarnings(as.numeric(.data[[col_var]])) else NA_real_,
        notas_documento = if (!is.na(col_note)) as.character(.data[[col_note]]) else NA_character_,
        missing_type = if (!is.na(col_miss)) as.character(.data[[col_miss]]) else NA_character_
      ) |>
      arrange(categoria, indicador, año)
    
    out <- fs::path(DIRS$derived_data, "01_fip_clean.csv")
    readr::write_csv(fip_clean, out)
    assign("FIP_CLEAN", fip_clean, envir = .GlobalEnv)
    log_info("01_fip_clean.csv generado: {out} | n={nrow(fip_clean)}")
    return(fip_clean)
  }
}

# ------------------------------------------------------------------------------
# 4) run_all(): orquesta el pipeline FIP-native
# ------------------------------------------------------------------------------

run_all <- function() {
  
  cat("\n", strrep("=", 70), "\n", sep = "")
  cat("DEFENSE ANALYTICS PIPELINE — RUN ALL (FIP-native)\n")
  cat(strrep("=", 70), "\n\n", sep = "")
  
  log_info("STEP A: main_setup()")
  setup_results <- main_setup()
  
  log_info("STEP B: load_verified_data()")
  ingest_results <- load_verified_data()
  
  log_info("STEP C: build_fip_clean()")
  build_fip_clean()
  
  # A partir de aquí: si tus modelos ya están en scripts, sourcéalos aquí
  # Ajusta nombres exactos según tu carpeta real:
  SCRIPTS_DIR <- get("DIRS", envir = .GlobalEnv)$scripts
  
  # ---- Model scripts (ajusta a tus nombres reales)
  candidates <- c(
    fs::path(SCRIPTS_DIR, "03_models", "02_build_fip_panels.R"),
    fs::path(SCRIPTS_DIR, "03_models", "03_P3_decomposition_growth.R"),
    fs::path(SCRIPTS_DIR, "03_models", "05_P5_governance_proxy.R"),
    fs::path(SCRIPTS_DIR, "03_models", "08_P8_red_year_score_fip.R")
  )
  
  existing <- candidates[fs::file_exists(candidates)]
  if (length(existing) > 0) {
    log_info("Sourcing {length(existing)} scripts de modelos...")
    for (f in existing) source_safe(f)
  } else {
    log_warn("No se encontraron scripts en rutas candidatas. Si tus modelos ya están cargados, ok.")
  }
  
  # ---- Requeridos para seguir
  stopif_missing(c("build_fip_panels", "model_red_year_score_fip", "model_growth_decomposition", "model_governance_proxy"))
  
  log_info("STEP D: build_fip_panels()")
  panels <- build_fip_panels()
  
  log_info("STEP E: P3 decomposition")
  p3 <- model_growth_decomposition()
  
  log_info("STEP F: P5 proxy")
  p5 <- model_governance_proxy()
  
  log_info("STEP G: P8 classifier")
  p8 <- model_red_year_score_fip()
  
  log_info("RUN ALL completado")
  cat("\n✓ RUN ALL COMPLETADO\n")
  cat("  • GAO cargado: ", ingest_results$gao_loaded, "\n", sep = "")
  cat("  • Nota GAO: ", ingest_results$gao_message, "\n", sep = "")
  cat("  • Outputs tables: ", get("DIRS", envir = .GlobalEnv)$tables, "\n", sep = "")
  # --------------------------------------------------------------------------
  # STEP H: Render HTML desde MD existentes en /docs
  # --------------------------------------------------------------------------
  log_info("STEP H: Render HTML desde MD existentes en /docs")
  
  # root del proyecto
  root_dir <- if (exists("DIRS", envir = .GlobalEnv) && !is.null(get("DIRS", envir = .GlobalEnv)$root)) {
    get("DIRS", envir = .GlobalEnv)$root
  } else {
    getwd()
  }
  
  # guarda resultados del run (solo objetos, sin ejecución)
  assign("RUN_ALL_RESULTS", list(
    setup  = setup_results,
    ingest = ingest_results,
    panels = panels,
    p3     = p3,
    p5     = p5,
    p8     = p8
  ), envir = .GlobalEnv)
  
  docs_dir <- fs::path(root_dir, "docs")
  fs::dir_create(docs_dir)
  
  md_files <- c(
    "escenarios_SOLO_FIP.md",
    "informe_ejecutivo_SOLO_FIP.md",
    "resumen_tablas_SOLO_FIP.md"
  )
  
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("Falta paquete rmarkdown. Instala con: install.packages('rmarkdown')")
  }
  
  for (md in md_files) {
    md_path <- fs::path(docs_dir, md)
    if (!fs::file_exists(md_path)) stop("No existe MD: ", md_path)
    
    out_html <- fs::path(docs_dir, sub("\\.md$", ".html", md))
    
    tmp_rmd <- tempfile(fileext = ".Rmd")
    writeLines(c(
      "---",
      "output: html_document",
      "---",
      "",
      "```{r, echo=FALSE}",
      sprintf("cat(readLines('%s', warn = FALSE), sep = '\\n')", md_path),
      "```"
    ), tmp_rmd)
    
    rmarkdown::render(tmp_rmd, output_file = out_html, quiet = TRUE)
    log_info("✓ Render: {basename(out_html)}")
  }
  

  
  
  # --------------------------------------------------------------------------
  # STEP H3: Run report de ejecución -> outputs/reports/run_report_*.html
  # --------------------------------------------------------------------------
  log_info("STEP H3: Run report de ejecución (outputs/reports)")
  
  report_script <- fs::path(root_dir, "scripts", "reports", "07_run_report_html.R")
  if (!fs::file_exists(report_script)) stop("No existe el script: ", report_script)
  
  source(report_script, local = .GlobalEnv)
  log_info("✓ Run report generado")
  
  
  invisible(list(setup = setup_results, ingest = ingest_results, panels = panels, p3 = p3, p5 = p5, p8 = p8))
}




# ------------------------------------------------------------------------------
# Ejecutar si se llama directo
# ------------------------------------------------------------------------------
if (sys.nframe() == 0) {
  results <- run_all()
}

