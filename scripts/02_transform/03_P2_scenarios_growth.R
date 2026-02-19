# ==============================================================================
# MODELO P2: ESCENARIOS DE CRECIMIENTO ESTRATÉGICO
# Defense Analytics: Strategic Scenario Modeling (CORREGIDO - FIP NATIVE)
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(fs)
  library(logger)
})

cat("\n[MODEL P2] ESCENARIOS DE CRECIMIENTO ESTRATÉGICO (FIP NATIVE - V3.2)\n")
cat("----------------------------------------------------------------------\n")

# ------------------------------------------------------------------------------
# 1. FUNCIÓN PRINCIPAL DEL MODELO (CORREGIDA - FIP NATIVE)
# ------------------------------------------------------------------------------
model_growth_scenarios <- function() {
  
  log_info("Iniciando modelo de escenarios de crecimiento (FIP native v3.2)")
  
  # --------------------------------------------------------------------------
  # 1.1. BASE CAPACITY DESDE FIP CLEAN (fuente única) - CORREGIDO: condicional mejorado
  # --------------------------------------------------------------------------
  fip_file <- fs::path(DIRS$derived_data, "01_fip_clean.csv")
  if (!file_exists(fip_file)) {
    log_error("Archivo FIP no encontrado: {fip_file}")
    stop("ERROR: No se puede encontrar 01_fip_clean.csv en data/derived/")
  }
  
  fip <- readr::read_csv(fip_file, show_col_types = FALSE)
  
  # Buscar capacidad total 2025
  base_capacity_fip <- fip %>%
    filter(indicador == "Total_Integrantes", año == 2025) %>%
    pull(valor)
  
  # Fallback si no se encuentra en fip_clean - CORREGIDO: length != 1
  if (length(base_capacity_fip) != 1 || is.na(base_capacity_fip)) {
    log_warn("No se encontró Total_Integrantes en 01_fip_clean.csv, usando valor hardcoded")
    base_capacity_fip <- 27121  # Valor del informe FIP
  }
  
  stopifnot(length(base_capacity_fip) == 1, !is.na(base_capacity_fip))
  base_capacity <- as.numeric(base_capacity_fip)
  
  log_info("Capacidad base 2025 (FIP): {base_capacity}")
  
  # --------------------------------------------------------------------------
  # 2. PARÁMETROS DEL MODELO (CORREGIDOS)
  # --------------------------------------------------------------------------
  params <- list(
    
    # Año base
    base_year = 2025,
    base_capacity = base_capacity,  # Usando valor dinámico de FIP
    
    # Horizonte de proyección
    projection_years = 2026:2030,
    
    # Escenarios estratégicos (CORREGIDO: growth_rate → scenario_growth_rate)
    scenarios = tribble(
      ~scenario_id, ~scenario_name, ~scenario_growth_rate, ~description, ~color,
      1, "Continuidad", 0.235, "Tendencia actual (+23.5% anual)", "#D32F2F",
      2, "Contención Parcial", 0.10, "Estrategia moderadamente exitosa (+10%)", "#FF9800",
      3, "Estabilización", 0.00, "Meta de política pública (0%)", "#4CAF50",
      4, "Reversión", -0.05, "Escenario optimista (-5%)", "#2196F3",
      5, "Saturación", NA_real_, "Límite ecológico del conflicto", "#9C27B0"
    ),
    
    # Parámetros de saturación (modelo logístico)
    saturation_params = list(
      K = 35000,    # Capacidad máxima (carrying capacity)
      r = 0.35,     # Tasa intrínseca de crecimiento
      t0 = 2025     # Año inicial
    ),
    
    # Umbrales estratégicos
    thresholds = list(
      crisis = 40000,     # Capacidad crítica
      high_risk = 30000,  # Alto riesgo
      medium_risk = 25000 # Riesgo medio
    )
  )
  
  log_info("Parámetros cargados: {nrow(params$scenarios)} escenarios")
  
  # Asegurar carpeta de tablas
  fs::dir_create(DIRS$tables, recurse = TRUE)
  
  # --------------------------------------------------------------------------
  # 3. FUNCIONES DE PROYECCIÓN (CORREGIDAS - USANDO BASE_YEAR CORRECTAMENTE)
  # --------------------------------------------------------------------------
  
  # 3.1. Proyección exponencial simple - CORREGIDO: base_year
  project_exponential <- function(N0, r, years, base_year = 2025) {
    tibble(
      year = years,
      capacity = round(N0 * (1 + r)^(year - base_year)),
      growth_rate_effective = r
    )
  }
  
  # 3.2. Proyección logística (saturación) - CORREGIDO: base_year
  project_logistic <- function(N0, K, r, years, base_year = 2025) {
    t <- years - base_year
    tibble(
      year = years,
      capacity = round(K / (1 + ((K - N0)/N0) * exp(-r * t))),
      growth_rate_effective = NA_real_
    )
  }
  
  # --------------------------------------------------------------------------
  # 4. GENERAR PROYECCIONES POR ESCENARIO (CORREGIDO CON BASE_YEAR)
  # --------------------------------------------------------------------------
  log_info("Generando proyecciones por escenario (corregidas con base_year)")
  
  projections_list <- list()
  
  # 4.1. Escenario 1: Continuidad (exponencial) - CORREGIDO
  projections_list[[1]] <- project_exponential(
    N0 = params$base_capacity,
    r = params$scenarios$scenario_growth_rate[1],
    years = params$projection_years,
    base_year = params$base_year
  ) %>%
    mutate(scenario_id = 1)
  
  # 4.2. Escenario 2: Contención Parcial (exponencial moderado) - CORREGIDO
  projections_list[[2]] <- project_exponential(
    N0 = params$base_capacity,
    r = params$scenarios$scenario_growth_rate[2],
    years = params$projection_years,
    base_year = params$base_year
  ) %>%
    mutate(scenario_id = 2)
  
  # 4.3. Escenario 3: Estabilización (crecimiento cero) - CORREGIDO
  projections_list[[3]] <- project_exponential(
    N0 = params$base_capacity,
    r = params$scenarios$scenario_growth_rate[3],
    years = params$projection_years,
    base_year = params$base_year
  ) %>%
    mutate(scenario_id = 3)
  
  # 4.4. Escenario 4: Reversión (decrecimiento) - CORREGIDO
  projections_list[[4]] <- project_exponential(
    N0 = params$base_capacity,
    r = params$scenarios$scenario_growth_rate[4],
    years = params$projection_years,
    base_year = params$base_year
  ) %>%
    mutate(scenario_id = 4)
  
  # 4.5. Escenario 5: Saturación (logístico) - CORREGIDO
  projections_list[[5]] <- project_logistic(
    N0 = params$base_capacity,
    K = params$saturation_params$K,
    r = params$saturation_params$r,
    years = params$projection_years,
    base_year = params$base_year
  ) %>%
    mutate(scenario_id = 5)
  
  # 4.6. Combinar todas las proyecciones
  projections_all <- bind_rows(projections_list) %>%
    left_join(params$scenarios, by = "scenario_id") %>%
    select(
      scenario_id, 
      scenario_name, 
      year, 
      capacity, 
      growth_rate_effective,
      scenario_growth_rate,
      description, 
      color
    )
  
  log_info("Proyecciones generadas: {nrow(projections_all)} observaciones")
  
  # --------------------------------------------------------------------------
  # 5. ANÁLISIS DE IMPLICACIONES ESTRATÉGICAS
  # --------------------------------------------------------------------------
  log_info("Analizando implicaciones estratégicas")
  
  # 5.1. Calcular puntos de cruce con umbrales
  threshold_crossings <- projections_all %>%
    group_by(scenario_id, scenario_name) %>%
    summarise(
      # Primer año que supera cada umbral
      crosses_crisis = if (any(capacity >= params$thresholds$crisis)) 
        min(year[capacity >= params$thresholds$crisis]) else NA_real_,
      crosses_high_risk = if (any(capacity >= params$thresholds$high_risk)) 
        min(year[capacity >= params$thresholds$high_risk]) else NA_real_,
      crosses_medium_risk = if (any(capacity >= params$thresholds$medium_risk)) 
        min(year[capacity >= params$thresholds$medium_risk]) else NA_real_,
      
      # Crecimiento acumulado
      total_growth_pct = (last(capacity[order(year)]) / params$base_capacity - 1) * 100,
      
      # Tasa promedio anual
      avg_annual_growth = ifelse(
        all(is.na(growth_rate_effective)),
        NA_real_,
        mean(growth_rate_effective, na.rm = TRUE) * 100
      ),
      
      # Capacidad final
      final_capacity = last(capacity[order(year)]),
      .groups = "drop"
    )
  
  # 5.2. Calcular diferencias entre escenarios
  scenario_comparison <- projections_all %>%
    filter(year == max(year)) %>%
    select(scenario_name, final_capacity = capacity) %>%
    mutate(
      diff_vs_continuity = final_capacity - 
        final_capacity[scenario_name == "Continuidad"],
      pct_vs_continuity = (final_capacity / 
                             final_capacity[scenario_name == "Continuidad"] - 1) * 100
    )
  
  # 5.3. Análisis de sensibilidad - CORREGIDO: usar base_year
  sensitivity_analysis <- expand_grid(
    base_rate = seq(0.15, 0.30, by = 0.01),
    year = params$projection_years
  ) %>%
    mutate(
      capacity = round(params$base_capacity * (1 + base_rate)^(year - params$base_year))
    ) %>%
    group_by(base_rate) %>%
    summarise(
      final_capacity = max(capacity),
      .groups = "drop"
    ) %>%
    mutate(
      threshold_crisis = final_capacity >= params$thresholds$crisis,
      threshold_high = final_capacity >= params$thresholds$high_risk
    )
  
  # --------------------------------------------------------------------------
  # 6. GUARDAR RESULTADOS DEL MODELO
  # --------------------------------------------------------------------------
  log_info("Guardando resultados del modelo")
  
  # 6.1. Guardar proyecciones principales
  write_csv(projections_all, fs::path(DIRS$tables, "P2_scenarios_projections.csv"))
  write_csv(threshold_crossings, fs::path(DIRS$tables, "P2_threshold_crossings.csv"))
  write_csv(scenario_comparison, fs::path(DIRS$tables, "P2_scenario_comparison.csv"))
  write_csv(sensitivity_analysis, fs::path(DIRS$tables, "P2_sensitivity_analysis.csv"))
  
  # 6.2. Guardar parámetros del modelo
  params_df <- tibble(
    parameter = c(
      "base_year", "base_capacity",
      "projection_years",
      "thresholds.crisis", "thresholds.high_risk", "thresholds.medium_risk",
      "saturation.K", "saturation.r", "saturation.t0"
    ),
    value = c(
      params$base_year,
      params$base_capacity,
      paste(params$projection_years, collapse = ";"),
      params$thresholds$crisis,
      params$thresholds$high_risk,
      params$thresholds$medium_risk,
      params$saturation_params$K,
      params$saturation_params$r,
      params$saturation_params$t0
    ) |> as.character()
  )
  write_csv(params_df, fs::path(DIRS$tables, "P2_model_parameters.csv"))
  
  # 6.3. Guardar metadatos del modelo
  model_metadata <- list(
    model_name = "P2_Growth_Scenarios_FIP_Native",
    timestamp = Sys.time(),
    base_year = params$base_year,
    base_capacity = params$base_capacity,
    scenarios_generated = n_distinct(projections_all$scenario_id),
    projection_years = length(params$projection_years),
    threshold_crisis = params$thresholds$crisis,
    model_version = "3.2"  # Versión corregida con base_year
  )
  
  write_csv(
    as_tibble(model_metadata),
    fs::path(DIRS$tables, "P2_model_metadata.csv")
  )
  
  # --------------------------------------------------------------------------
  # 7. REPORTE DEL MODELO CON VALORES CORREGIDOS
  # --------------------------------------------------------------------------
  log_info("Generando reporte del modelo")
  
  cat("\n✓ MODELO P2 COMPLETADO (FIP NATIVE - V3.2 CORREGIDO)\n")
  cat("  • Escenarios generados: ", model_metadata$scenarios_generated, "\n")
  cat("  • Horizonte: 2026-2030 (", model_metadata$projection_years, " años)\n")
  cat("  • Capacidad base 2025: ", format(params$base_capacity, big.mark = ","), "\n")
  cat("  • Umbral de crisis: ", format(params$thresholds$crisis, big.mark = ","), "\n")
  
  # Mostrar resumen ejecutivo
  cat("\nRESUMEN EJECUTIVO - CAPACIDAD 2030:\n")
  cat("------------------------------------\n")
  for (i in 1:nrow(scenario_comparison)) {
    sc <- scenario_comparison[i, ]
    cat(sprintf("  • %-20s: %6s personas (%+5.1f%% vs Continuidad)\n",
                sc$scenario_name,
                format(sc$final_capacity, big.mark = ","),
                sc$pct_vs_continuity))
  }
  
  # Mostrar cruces de umbrales críticos
  cat("\nCRUCES DE UMBRALES CRÍTICOS:\n")
  cat("-----------------------------\n")
  crisis_scenarios <- threshold_crossings %>%
    filter(!is.na(crosses_crisis))
  
  if (nrow(crisis_scenarios) > 0) {
    for (i in 1:nrow(crisis_scenarios)) {
      cs <- crisis_scenarios[i, ]
      cat(sprintf("  • %-20s: Supera 40k en %4.0f\n",
                  cs$scenario_name, cs$crosses_crisis))
    }
  } else {
    cat("  • Ningún escenario supera 40k en el horizonte\n")
  }
  
  # Mostrar sanity check de valores corregidos
  cat("\nSANITY CHECK - VALORES CORREGIDOS (V3.2):\n")
  cat("-----------------------------------------\n")
  sanity_data <- projections_all %>%
    filter(scenario_name %in% c("Continuidad", "Reversión"), year == 2026) %>%
    select(scenario_name, year, capacity)
  
  for (i in 1:nrow(sanity_data)) {
    sd <- sanity_data[i, ]
    cat(sprintf("  • %-20s 2026: %6s (corregido)\n",
                sd$scenario_name, 
                format(sd$capacity, big.mark = ",")))
  }
  
  cat("\n")
  
  # --------------------------------------------------------------------------
  # 8. EXPORTAR RESULTADOS
  # --------------------------------------------------------------------------
  assign("P2_PROJECTIONS", projections_all, envir = .GlobalEnv)
  assign("P2_THRESHOLDS", threshold_crossings, envir = .GlobalEnv)
  assign("P2_COMPARISON", scenario_comparison, envir = .GlobalEnv)
  assign("P2_SENSITIVITY", sensitivity_analysis, envir = .GlobalEnv)
  assign("P2_PARAMS", params, envir = .GlobalEnv)
  
  log_info("Resultados exportados al entorno global")
  
  return(list(
    projections = projections_all,
    thresholds = threshold_crossings,
    comparison = scenario_comparison,
    sensitivity = sensitivity_analysis,
    params = params,
    metadata = model_metadata
  ))
}

# ------------------------------------------------------------------------------
# 9. FUNCIÓN DE COMPATIBILIDAD (para scripts existentes)
# ------------------------------------------------------------------------------
model_growth_scenarios_fip <- function() {
  log_warn("Usando función de compatibilidad: model_growth_scenarios_fip()")
  log_warn("Se recomienda usar model_growth_scenarios() directamente")
  return(model_growth_scenarios())
}

# ------------------------------------------------------------------------------
# 10. EJECUTAR MODELO
# ------------------------------------------------------------------------------
if (!interactive()) {
  # Ejecutar automáticamente
  p2_results <- model_growth_scenarios()
} else {
  cat("\nPara ejecutar el modelo de escenarios, correr: model_growth_scenarios()\n")
  cat("Para compatibilidad FIP: model_growth_scenarios_fip()\n")
}