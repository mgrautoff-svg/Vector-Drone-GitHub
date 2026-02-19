# ==============================================================================
# SETUP ROBUSTO - DEFENSE ANALYTICS PIPELINE
# Senior Data Scientist & Strategic Analyst
# ==============================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("DEFENSE ANALYTICS PIPELINE - CONFIGURACIÓN ROBUSTA\n")
cat("Standard: RAND Corporation / Brookings Institution\n")
cat(strrep("=", 70), "\n\n", sep = "")

# ------------------------------------------------------------------------------
# 0. DETERMINAR DIRECTORIO RAÍZ DEL PROYECTO (ANCLADO)
# ------------------------------------------------------------------------------
cat("[0] DETERMINANDO DIRECTORIO RAÍZ DEL PROYECTO\n")

# Estrategia 1: Buscar .Rproj hacia arriba desde el directorio actual
find_project_root <- function() {
  current_dir <- getwd()
  max_depth <- 10  # Límite de niveles hacia arriba
  
  for (i in 1:max_depth) {
    # Verificar si existe .Rproj o estructura esperada
    rproj_files <- list.files(current_dir, pattern = "\\.Rproj$", full.names = TRUE)
    scripts_dir <- file.path(current_dir, "scripts")
    
    if (length(rproj_files) > 0) {
      cat("  • Encontrado .Rproj en:", current_dir, "\n")
      return(current_dir)
    }
    
    if (dir_exists(scripts_dir)) {
      cat("  • Encontrada carpeta scripts/ en:", current_dir, "\n")
      return(current_dir)
    }
    
    # Subir un nivel
    parent_dir <- dirname(current_dir)
    if (parent_dir == current_dir) {
      break  # Llegamos a la raíz del sistema de archivos
    }
    current_dir <- parent_dir
  }
  
  # Si no se encuentra, usar el directorio actual como fallback
  cat("  • No se encontró .Rproj ni scripts/, usando directorio actual\n")
  return(getwd())
}

project_root <- find_project_root()
cat("  • Directorio raíz anclado:", project_root, "\n")

# Verificar que la estructura mínima existe
scripts_dir <- file.path(project_root, "scripts")
if (!dir_exists(scripts_dir)) {
  stop("ERROR: No se encuentra carpeta scripts/ en el directorio raíz.\n",
       "Ejecutar desde la raíz del proyecto o crear estructura mínima.")
}

# ------------------------------------------------------------------------------
# 1. CARGAR PAQUETES CRÍTICOS
# ------------------------------------------------------------------------------
cat("\n[1] VERIFICANDO Y CARGANDO PAQUETES\n")

# Paquetes esenciales
required_packages <- c(
  "tidyverse",    # dplyr, tidyr, ggplot2, readr, purrr, stringr
  "fs",           # Manejo de archivos multiplataforma
  "logger",       # Logging estructurado
  "lubridate",    # Manejo de fechas
  "scales",       # Formateo de escalas para gráficos
  "jsonlite",     # Para guardar resultados en JSON
  "validate"      # Validación de datos (para ingesta)
)

# Verificar paquetes faltantes
missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

if (length(missing_packages) > 0) {
  cat("⚠  Instalando paquetes faltantes:", paste(missing_packages, collapse = ", "), "\n")
  
  # Verificar si estamos en un entorno controlado
  if (Sys.getenv("CI") == "true" || Sys.getenv("DOCKER") == "true") {
    cat("  • Entorno controlado detectado, instalando sin prompts...\n")
    install.packages(missing_packages, dependencies = TRUE, quiet = TRUE)
  } else {
    # Modo interactivo normal
    install.packages(missing_packages, dependencies = TRUE)
  }
  
  cat("✓ Paquetes instalados\n")
} else {
  cat("✓ Todos los paquetes requeridos están instalados\n")
}

# Cargar paquetes silenciosamente
suppressPackageStartupMessages({
  library(tidyverse)
  library(fs)
  library(logger)
  library(lubridate)
  library(scales)
  library(jsonlite)
  library(validate)
})

cat("  • tidyverse cargado (dplyr, ggplot2, readr, etc.)\n")
cat("  • Paquetes especializados: fs, logger, lubridate, scales, jsonlite, validate\n")
cat("  • Total paquetes cargados:", length(.packages()), "\n\n")

# ------------------------------------------------------------------------------
# 2. CREAR ESTRUCTURA DE DIRECTORIOS
# ------------------------------------------------------------------------------
cat("[2] CREANDO ESTRUCTURA DE DIRECTORIOS\n")

# Directorios críticos (deben existir después del setup)
critical_dirs <- c(
  "data/verified",      # Datos verificados FIP
  "data/derived",       # Datos derivados
  "outputs/tables",     # Tablas de resultados
  "outputs/figures",    # Gráficos
  "outputs/reports",    # Reportes
  "logs"                # Logs de ejecución
)

# Directorios opcionales (se crean si no existen)
optional_dirs <- c(
  "data/raw",
  "data/external",
  "data/temp",
  "outputs/maps",
  "outputs/presentations",
  "outputs/models",
  "outputs/scenarios",
  "checks"
)

# Crear directorios críticos (fail fast si falla)
for (dir in critical_dirs) {
  dir_path <- file.path(project_root, dir)
  if (!dir_exists(dir_path)) {
    success <- tryCatch({
      dir_create(dir_path, recursive = TRUE)
      TRUE
    }, error = function(e) FALSE)
    
    if (success) {
      cat("  ✓ Creado:", dir, "\n")
    } else {
      stop("ERROR: No se pudo crear directorio crítico: ", dir)
    }
  }
}

# Crear directorios opcionales (con tolerancia a errores)
for (dir in optional_dirs) {
  dir_path <- file.path(project_root, dir)
  if (!dir_exists(dir_path)) {
    tryCatch({
      dir_create(dir_path, recursive = TRUE)
      cat("  ✓ Creado:", dir, "(opcional)\n")
    }, error = function(e) {
      cat("  ⚠ No se pudo crear:", dir, "(omitiendo)\n")
    })
  }
}

# ------------------------------------------------------------------------------
# 3. VERIFICAR ARCHIVOS CRÍTICOS (FAIL FAST)
# ------------------------------------------------------------------------------
cat("\n[3] VERIFICANDO ARCHIVOS DE DATOS CRÍTICOS (FAIL FAST)\n")

verified_path <- file.path(project_root, "data/verified")

# Archivos críticos que DEBEN existir para que el pipeline funcione
critical_files <- c(
  "fip_verified_gao_counts_2024_2025.csv",
  "base_datos_fip_2025_CORREGIDA.csv"
)

# Archivos recomendados (fallback disponible)
recommended_files <- c(
  "fip_verified_aggregates_2024_2025.csv"
)

# Archivos opcionales (no críticos)
optional_files <- c(
  "fip_verification_checks.csv"
)

# Verificar archivos críticos (FAIL FAST)
missing_critical <- c()
for (file in critical_files) {
  file_path <- file.path(verified_path, file)
  if (file_exists(file_path)) {
    cat("  ✓ ", file, "\n", sep = "")
  } else {
    cat("  ✗ ", file, " (CRÍTICO - FALTANTE)\n", sep = "")
    missing_critical <- c(missing_critical, file)
  }
}

# Si faltan archivos críticos, DETENER
if (length(missing_critical) > 0) {
  cat("\n❌ ERROR CRÍTICO: Faltan archivos esenciales:\n")
  for (file in missing_critical) {
    cat("   • ", file, "\n", sep = "")
  }
  cat("\nSolución:\n")
  cat("   1. Colocar archivos en: ", verified_path, "\n")
  cat("   2. Archivos requeridos:\n")
  cat("      - fip_verified_gao_counts_2024_2025.csv (datos GAO 2024-2025)\n")
  cat("      - base_datos_fip_2025_CORREGIDA.csv (indicadores FIP)\n")
  stop("Pipeline abortado: archivos críticos faltantes.")
}

# Verificar archivos recomendados
for (file in recommended_files) {
  file_path <- file.path(verified_path, file)
  if (file_exists(file_path)) {
    cat("  ✓ ", file, " (recomendado)\n", sep = "")
  } else {
    cat("  ⚠ ", file, " (no encontrado - se usará fallback)\n", sep = "")
  }
}

# Verificar archivos opcionales
for (file in optional_files) {
  file_path <- file.path(verified_path, file)
  if (file_exists(file_path)) {
    cat("  ✓ ", file, " (opcional)\n", sep = "")
  }
}

# Listar todos los archivos disponibles
available_files <- list.files(verified_path, full.names = FALSE)
if (length(available_files) > 0) {
  cat("\n  Archivos disponibles en data/verified:\n")
  for (file in available_files) {
    size <- file.size(file.path(verified_path, file))
    cat(sprintf("    • %-40s (%s)\n", 
                file, 
                ifelse(is.na(size), "N/A", paste(round(size/1024, 1), "KB"))))
  }
}

cat("\n✓ Todos los archivos críticos están presentes\n")

# ------------------------------------------------------------------------------
# 4. CONFIGURAR VARIABLES GLOBALES (CONSISTENTES)
# ------------------------------------------------------------------------------
cat("\n[4] CONFIGURANDO VARIABLES GLOBALES\n")

# Crear objeto DIRS estructurado
DIRS <- list(
  # Directorio raíz (anclado)
  root = project_root,
  
  # Inputs
  raw_data = file.path(project_root, "data/raw"),
  verified_data = file.path(project_root, "data/verified"),
  external_data = file.path(project_root, "data/external"),
  derived_data = file.path(project_root, "data/derived"),
  temp_data = file.path(project_root, "data/temp"),
  
  # Outputs
  tables = file.path(project_root, "outputs/tables"),
  figures = file.path(project_root, "outputs/figures"),
  maps = file.path(project_root, "outputs/maps"),
  reports = file.path(project_root, "outputs/reports"),
  presentations = file.path(project_root, "outputs/presentations"),
  models = file.path(project_root, "outputs/models"),
  scenarios = file.path(project_root, "outputs/scenarios"),
  
  # Logs y checks
  logs = file.path(project_root, "logs"),
  checks = file.path(project_root, "checks")
)

# Parámetros estratégicos (CONSISTENTES)
PARAMS <- list(
  # Configuración general
  general = list(
    baseline_year = 2025,
    forecast_horizon = 5,
    analysis_years = c(2024, 2025)
  ),
  
  # Umbrales estratégicos
  thresholds = list(
    crisis_growth = 0.20,           # 20% crecimiento = crisis
    hhi_oligopoly = 0.25,           # HHI > 0.25 = oligopolio
    hhi_fragmented = 0.15,          # HHI < 0.15 = fragmentado
    capacity_crisis = 40000,        # Capacidad crítica
    capacity_high_risk = 30000,     # Alto riesgo
    capacity_medium_risk = 25000    # Riesgo medio
  ),
  
  # Modelos - ESCENARIOS CONSISTENTES
  scenarios = list(
    names = c("Continuidad", "Contención Parcial", "Estabilización", "Saturación"),
    rates = c(0.235, 0.10, 0.00, NA_real_),  # ÚLTIMO: NA para saturación
    descriptions = c(
      "Tendencia actual (+23.5% anual)",
      "Estrategia moderadamente exitosa (+10%)",
      "Meta de política pública (0%)",
      "Límite ecológico del conflicto (modelo logístico)"
    ),
    colors = c("#D32F2F", "#FF9800", "#4CAF50", "#9C27B0"),
    saturation_params = list(K = 35000, r = 0.35, t0 = 2025)
  ),
  
  # Clasificación de riesgo
  risk_classification = list(
    red = 7,     # ≥7 puntos = Año Rojo (crisis)
    amber = 4,   # 4-6 puntos = Año Ámbar (deterioro)
    green = 0    # 0-3 puntos = Año Verde (estable)
  ),
  
  # Visualización
  visuals = list(
    colors_strategic = c(
      "#4CAF50",  # Verde - Bajo riesgo
      "#FFC107",  # Amarillo - Medio
      "#FF9800",  # Naranja - Alto
      "#F44336"   # Rojo - Crítico
    ),
    colors_groups = viridis::viridis(8),
    theme_strategic = theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(face = "bold"),
        legend.position = "bottom",
        panel.grid.minor = element_blank()
      )
  )
)

# Verificar consistencia interna
if (length(PARAMS$scenarios$names) != length(PARAMS$scenarios$rates)) {
  warning("ADVERTENCIA: Número de escenarios y tasas no coinciden")
}

# Exportar al entorno global
assign("PROJECT_ROOT", project_root, envir = .GlobalEnv)
assign("DIRS", DIRS, envir = .GlobalEnv)
assign("PARAMS", PARAMS, envir = .GlobalEnv)

cat("✓ Variables exportadas: PROJECT_ROOT, DIRS, PARAMS\n")

# ------------------------------------------------------------------------------
# 5. CONFIGURAR LOGGING ESTRUCTURADO (CORREGIDO)
# ------------------------------------------------------------------------------
cat("\n[5] CONFIGURANDO SISTEMA DE LOGGING\n")

# Configurar formato de logs
log_formatter(formatter_glue)
log_layout(layout_glue_colors)

# Archivo de log principal
log_file <- file.path(DIRS$logs, paste0("defense_analytics_", format(Sys.Date(), "%Y%m%d"), ".log"))
log_appender(appender_file(log_file))

# Configurar también log a consola para debugging
log_appender(appender_tee(log_file))

# Mensaje de inicio estructurado (CORREGIDO - usando strrep)
log_info(strrep("=", 60))
log_info("DEFENSE ANALYTICS PIPELINE - INICIALIZACIÓN")
log_info("Timestamp: {Sys.time()}")
log_info("Project Root: {project_root}")
log_info("R Version: {R.version.string}")
log_info("Platform: {R.version$platform}")
log_info(strrep("=", 60))

# Información del sistema
log_info("Sistema: {Sys.info()['sysname']} {Sys.info()['release']}")
log_info("Usuario: {Sys.info()['user']}")
log_info("Directorios críticos creados: {length(critical_dirs)}")
log_info("Archivos críticos verificados: {length(critical_files) - length(missing_critical)}/{length(critical_files)}")

cat("  • Archivo de log principal:", log_file, "\n")
cat("  • Nivel de log: INFO (consola + archivo)\n")

# ------------------------------------------------------------------------------
# 6. VERIFICACIÓN DE RECURSOS DEL SISTEMA
# ------------------------------------------------------------------------------
cat("\n[6] VERIFICANDO RECURSOS DEL SISTEMA\n")

# Verificar espacio en disco en el directorio del proyecto
check_disk_space <- function(path) {
  if (.Platform$OS.type == "windows") {
    tryCatch({
      # Windows - usar shell
      drive <- substr(path, 1, 1)
      cmd <- sprintf('wmic logicaldisk where "DeviceID=\'%s:\'" get Size,FreeSpace', drive)
      disk_info <- system(cmd, intern = TRUE)
      if (length(disk_info) > 2) {
        # Parsear resultados
        values <- strsplit(disk_info[2], "\\s+")[[1]]
        if (length(values) >= 2) {
          free_gb <- round(as.numeric(values[2]) / (1024^3), 1)
          total_gb <- round(as.numeric(values[1]) / (1024^3), 1)
          return(list(available = TRUE, free_gb = free_gb, total_gb = total_gb))
        }
      }
      return(list(available = FALSE))
    }, error = function(e) {
      return(list(available = FALSE))
    })
  } else {
    # Unix/Linux/Mac - usar df
    tryCatch({
      disk_info <- system(paste("df -h", shQuote(path)), intern = TRUE)
      if (length(disk_info) > 1) {
        values <- strsplit(disk_info[2], "\\s+")[[1]]
        return(list(available = TRUE, info = disk_info[2]))
      }
      return(list(available = FALSE))
    }, error = function(e) {
      return(list(available = FALSE))
    })
  }
}

disk_check <- check_disk_space(project_root)
if (disk_check$available) {
  if (.Platform$OS.type == "windows") {
    cat(sprintf("  • Disco: %.0f GB libres de %.0f GB\n", 
                disk_check$free_gb, disk_check$total_gb))
    log_info("Espacio en disco: {disk_check$free_gb} GB libres de {disk_check$total_gb} GB")
  } else {
    cat("  • Disco:", disk_check$info, "\n")
    log_info("Espacio en disco: {disk_check$info}")
  }
} else {
  cat("  • Disco: información no disponible\n")
  log_warn("No se pudo verificar espacio en disco")
}

# Verificar capacidad de procesamiento
cores_physical <- parallel::detectCores(logical = FALSE)
cores_logical <- parallel::detectCores(logical = TRUE)
cat(sprintf("  • CPU: %d núcleos físicos, %d lógicos\n", cores_physical, cores_logical))
log_info("CPU: {cores_physical} núcleos físicos, {cores_logical} lógicos")

# Verificar memoria (aproximación)
mem_limit <- NA
try({
  if (.Platform$OS.type == "windows") {
    mem_info <- system("wmic OS get TotalVisibleMemorySize", intern = TRUE)
    if (length(mem_info) > 2) {
      mem_bytes <- as.numeric(gsub("[^0-9]", "", mem_info[2])) * 1024
      mem_limit <- round(mem_bytes / (1024^3), 1)
    }
  }
}, silent = TRUE)

if (!is.na(mem_limit)) {
  cat(sprintf("  • Memoria: ~%.1f GB disponibles\n", mem_limit))
  log_info("Memoria estimada: {mem_limit} GB")
}

# ------------------------------------------------------------------------------
# 7. CONFIGURACIÓN FINAL Y RESUMEN
# ------------------------------------------------------------------------------
cat("\n", strrep("=", 70), "\n", sep = "")
cat("✓ CONFIGURACIÓN COMPLETADA EXITOSAMENTE\n")
cat(strrep("=", 70), "\n\n", sep = "")

# Resumen ejecutivo
cat("RESUMEN DE CONFIGURACIÓN:\n")
cat("-------------------------\n")
cat("  • Directorio raíz anclado:", PROJECT_ROOT, "\n")
cat("  • Archivos críticos:", length(critical_files) - length(missing_critical), 
    "/", length(critical_files), "presentes ✓\n")
cat("  • Directorios críticos:", length(critical_dirs), "creados/verificados\n")
cat("  • Archivo de log:", log_file, "\n")
cat("  • Paquetes cargados:", length(search()), "\n")
cat(sprintf("  • Recursos: %d núcleos, ~%s GB memoria\n", 
            cores_logical, ifelse(is.na(mem_limit), "N/D", mem_limit)))
cat("  • Escenarios configurados:", length(PARAMS$scenarios$names), "\n")
cat("  • Hora del sistema:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# Verificación final de consistencia
cat("VERIFICACIÓN DE CONSISTENCIA INTERNA:\n")
cat("-------------------------------------\n")
cat("  • Escenarios vs tasas:", 
    ifelse(length(PARAMS$scenarios$names) == length(PARAMS$scenarios$rates),
           "✓ Coinciden", "✗ NO COINCIDEN"), "\n")
cat("  • Umbrales configurados:", length(PARAMS$thresholds), "parámetros\n")
cat("  • Clasificación riesgo:", 
    paste(names(PARAMS$risk_classification), 
          unlist(PARAMS$risk_classification), sep = "=", collapse = ", "), "\n\n")

# Log final (CORREGIDO - usando strrep)
log_info(strrep("=", 60))
log_info("SETUP COMPLETADO EXITOSAMENTE")
log_info("Resumen: {length(critical_dirs)} dirs, {cores_logical} cores, {ifelse(is.na(mem_limit), 'N/D', paste(mem_limit, 'GB'))} mem")
log_info("Escenarios: {length(PARAMS$scenarios$names)} configurados")
log_info(strrep("=", 60))

# ------------------------------------------------------------------------------
# 8. FUNCIÓN PRINCIPAL
# ------------------------------------------------------------------------------
main_setup <- function() {
  return(list(
    project_root = PROJECT_ROOT,
    dirs = DIRS,
    params = PARAMS,
    log_file = log_file,
    system_info = list(
      r_version = R.version.string,
      platform = R.version$platform,
      cores_physical = cores_physical,
      cores_logical = cores_logical,
      memory_gb = mem_limit,
      disk_check = disk_check$available
    ),
    verification = list(
      critical_files_present = length(missing_critical) == 0,
      critical_files_missing = missing_critical,
      critical_dirs_created = length(critical_dirs)
    )
  ))
}

# Mensaje para el usuario
cat("Para usar la configuración en el pipeline:\n")
cat("  setup_results <- main_setup()\n")
cat("  # O acceder directamente a PROJECT_ROOT, DIRS, PARAMS\n\n")

# Si se ejecuta directamente, ejecutar main_setup()
if (sys.nframe() == 0) {
  setup_results <- main_setup()
  cat("Setup ejecutado. Resultados disponibles en setup_results\n")
}