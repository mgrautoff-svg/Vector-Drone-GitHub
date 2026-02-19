# ==============================================================================
# MODELO P2: ESCENARIOS DE CRECIMIENTO ESTRATÉGICO
# Defense Analytics: Strategic Scenario Modeling
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(forecast)
  library(scales)
  library(logger)
})

cat("\n[MODEL P2] ESCENARIOS DE CRECIMIENTO ESTRATÉGICO\n")
cat("--------------------------------------------------\n")

# ------------------------------------------------------------------------------
# 1. FUNCIÓN PRINCIPAL DEL MODELO
# ------------------------------------------------------------------------------
model_growth_scenarios <- function() {
  
  log_info("Iniciando modelo de escenarios de crecimiento")
  
  # Verificar datos necesarios
  if (!exists("PANEL_ACTOR_YEAR")) {
    stop("ERROR: Panel actor-año no disponible. Ejecutar primero 02_build_derived_panels.R")
  }
  
  # --------------------------------------------------------------------------
  # 2. PARÁMETROS DEL MODELO
  # --------------------------------------------------------------------------
  params <- list(
    
    # Año base
    base_year = 2025,
    base_capacity = 27121,  # Total confirmado por FIP
    
    # Horizonte de proyección
    projection_years = 2026:2030,
    
    # Escenarios estratégicos
    scenarios = tribble(
      ~scenario_id, ~scenario_name, ~growth_rate, ~description, ~color,
      1, "Continuidad", 0.235, "Tendencia actual (+23.5% anual)", "#D32F2F",
      2, "Contención Parcial", 0.10, "Estrategia moderadamente exitosa (+10%)", "#FF9800",
      3, "Estabilización", 0.00, "Meta de política pública (0%)", "#4CAF50",
      4, "Reversión", -0.05, "Escenario optimista (-5%)", "#2196F3",
      5, "Saturación", NA, "Límite ecológico del conflicto", "#9C27B0"
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
  
  log_info("Parámetros del modelo cargados: {length(params$scenarios$scenario_id)} escenarios")
  
  # --------------------------------------------------------------------------
  # 3. FUNCIONES DE PROYECCIÓN
  # --------------------------------------------------------------------------
  
  # 3.1. Proyección exponencial simple
  project_exponential <- function(N0, r, years) {
    tibble(
      year = years,
      capacity = round(N0 * (1 + r)^(year - min(years))),
      growth_rate = r
    )
  }
  
  # 3.2. Proyección logística (saturación)
  project_logistic <- function(N0, K, r, years) {
    t <- years - min(years)
    tibble(
      year = years,
      capacity = round(K / (1 + ((K - N0)/N0) * exp(-r * t))),
      growth_rate = NA_real_
    )
  }
  
  # 3.3. Proyección con desaceleración
  project_deceleration <- function(N0, r_base, years, decel_factor = 0.15) {
    result <- list()
    capacity <- N0
    
    for (i in seq_along(years)) {
      if (i == 1) {
        result[[i]] <- tibble(
          year = years[i],
          capacity = round(N0 * (1 + r_base)),
          growth_rate = r_base
        )
        capacity <- result[[i]]$capacity
      } else {
        # Desaceleración progresiva
        effective_rate <- max(0, r_base * (1 - decel_factor)^(i-1))
        new_capacity <- round(capacity * (1 + effective_rate))
        
        result[[i]] <- tibble(
          year = years[i],
          capacity = new_capacity,
          growth_rate = effective_rate
        )
        capacity <- new_capacity
      }
    }
    
    bind_rows(result)
  }
  
  # --------------------------------------------------------------------------
  # 4. GENERAR PROYECCIONES POR ESCENARIO
  # --------------------------------------------------------------------------
  log_info("Generando proyecciones por escenario")
  
  projections_list <- list()
  
  # 4.1. Escenario 1: Continuidad (exponencial)
  projections_list[[1]] <- project_exponential(
    N0 = params$base_capacity,
    r = params$scenarios$growth_rate[1],
    years = params$projection_years
  ) %>%
    mutate(scenario_id = 1)
  
  # 4.2. Escenario 2: Contención Parcial (exponencial moderado)
  projections_list[[2]] <- project_exponential(
    N0 = params$base_capacity,
    r = params$scenarios$growth_rate[2],
    years = params$projection_years
  ) %>%
    mutate(scenario_id = 2)
  
  # 4.3. Escenario 3: Estabilización (crecimiento cero)
  projections_list[[3]] <- project_exponential(
    N0 = params$base_capacity,
    r = params$scenarios$growth_rate[3],
    years = params$projection_years
  ) %>%
    mutate(scenario_id = 3)
  
  # 4.4. Escenario 4: Reversión (decrecimiento)
  projections_list[[4]] <- project_exponential(
    N0 = params$base_capacity,
    r = params$scenarios$growth_rate[4],
    years = params$projection_years
  ) %>%
    mutate(scenario_id = 4)
  
  # 4.5. Escenario 5: Saturación (logístico)
  projections_list[[5]] <- project_logistic(
    N0 = params$base_capacity,
    K = params$saturation_params$K,
    r = params$saturation_params$r,
    years = params$projection_years
  ) %>%
    mutate(scenario_id = 5)
  
  # 4.6. Combinar todas las proyecciones
  projections_all <- bind_rows(projections_list) %>%
    left_join(params$scenarios, by = "scenario_id") %>%
    select(scenario_id, scenario_name, year, capacity, growth_rate, description, color)
  
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
      total_growth_pct = (max(capacity) / params$base_capacity - 1) * 100,
      avg_annual_growth = mean(growth_rate, na.rm = TRUE) * 100,
      
      # Capacidad final
      final_capacity = max(capacity),
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
  
  # 5.3. Análisis de sensibilidad
  sensitivity_analysis <- expand_grid(
    base_rate = seq(0.15, 0.30, by = 0.01),
    year = params$projection_years
  ) %>%
    mutate(
      capacity = round(params$base_capacity * (1 + base_rate)^(year - 2025))
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
    parameter = names(unlist(params)),
    value = as.character(unlist(params))
  )
  write_csv(params_df, fs::path(DIRS$tables, "P2_model_parameters.csv"))
  
  # 6.3. Guardar metadatos del modelo
  model_metadata <- list(
    model_name = "P2_Growth_Scenarios",
    timestamp = Sys.time(),
    base_year = params$base_year,
    base_capacity = params$base_capacity,
    scenarios_generated = n_distinct(projections_all$scenario_id),
    projection_years = length(params$projection_years),
    threshold_crisis = params$thresholds$crisis,
    model_version = "2.0"
  )
  
  write_csv(
    as_tibble(model_metadata),
    fs::path(DIRS$tables, "P2_model_metadata.csv")
  )
  
  # --------------------------------------------------------------------------
  # 7. REPORTE DEL MODELO
  # --------------------------------------------------------------------------
  log_info("Generando reporte del modelo")
  
  cat("\n✓ MODELO P2 COMPLETADO\n")
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
# 9. EJECUTAR MODELO
# ------------------------------------------------------------------------------
if (!interactive()) {
  # Ejecutar automáticamente
  p2_results <- model_growth_scenarios()
} else {
  cat("\nPara ejecutar el modelo de escenarios, correr: model_growth_scenarios()\n")
}