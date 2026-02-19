# ==============================================================================
# MODELO P4: CONCENTRACIÓN DE LA AMENAZA (HHI)
# Defense Analytics: Market Concentration Analysis
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
  library(logger)
})


cat("\n[MODEL P4] CONCENTRACIÓN DE LA AMENAZA - ANÁLISIS HHI\n")
cat("-------------------------------------------------------\n")

# ------------------------------------------------------------------------------
# 1. FUNCIÓN PRINCIPAL DEL MODELO
# ------------------------------------------------------------------------------
model_concentration_hhi <- function() {
  
  log_info("Iniciando modelo de concentración HHI")
  
  # Verificar datos necesarios
  if (!exists("PANEL_ACTOR_YEAR")) {
    stop("ERROR: Panel actor-año no disponible. Ejecutar primero 02_build_derived_panels.R")
  }
  
  # --------------------------------------------------------------------------
  # 2. PARÁMETROS DEL MODELO
  # --------------------------------------------------------------------------
  params <- list(
    
    # Definición de mercados (capacidades)
    markets = c("total", "armed", "support"),
    
    # Años de análisis
    analysis_years = c(2024, 2025),
    
    # Umbrales HHI (Department of Justice / FTC)
    hhi_thresholds = tribble(
      ~market_type, ~unconcentrated, ~moderate, ~highly,
      "Traditional", 1500, 2500, 2500,
      "Defense Adjusted", 1000, 1800, 2500
    ),
    
    # Métricas de concentración adicionales
    concentration_metrics = c(
      "CR3",    # Concentration Ratio - Top 3
      "CR5",    # Concentration Ratio - Top 5
      "Gini",   # Gini coefficient
      "Palma"   # Palma ratio
    ),
    
    # Escalas de interpretación
    interpretation_scale = tribble(
      ~hhi_range, ~interpretation, ~strategic_implication, ~color,
      "0-1000", "Mercado no concentrado", "Alta competencia, baja coordinación", "#4CAF50",
      "1001-1800", "Concentración moderada", "Competencia con actores dominantes", "#FFC107",
      "1801-2500", "Mercado concentrado", "Oligopolio emergente", "#FF9800",
      "2500+", "Alta concentración", "Cuasi-monopolio o cartel", "#F44336"
    )
  )
  
  # --------------------------------------------------------------------------
  # 3. CÁLCULO DE ÍNDICES DE CONCENTRACIÓN
  # --------------------------------------------------------------------------
  log_info("Calculando índices de concentración")
  
  # 3.1. Preparar datos para análisis
  concentration_data <- PANEL_ACTOR_YEAR %>%
    filter(year %in% params$analysis_years) %>%
    select(gao, year, total, armed, support) %>%
    pivot_longer(
      cols = c(total, armed, support),
      names_to = "market",
      values_to = "capacity"
    )
  
  # 3.2. Calcular HHI para cada mercado y año
  hhi_results <- concentration_data %>%
    group_by(year, market) %>%
    summarise(
      # Herfindahl-Hirschman Index
      hhi_index = sum((capacity / sum(capacity))^2) * 10000,
      
      # Número efectivo de competidores (1/HHI)
      effective_competitors = 1 / (hhi_index / 10000),
      
      # Shares del mercado
      market_shares = list(
        tibble(
          gao = gao,
          share = capacity / sum(capacity)
        )
      ),
      .groups = "drop"
    )
  
  # 3.3. Calcular ratios de concentración (CR3, CR5)
  cr_results <- concentration_data %>%
    group_by(year, market) %>%
    arrange(desc(capacity)) %>%
    mutate(
      rank = row_number(),
      cumul_share = cumsum(capacity / sum(capacity))
    ) %>%
    summarise(
      cr3 = sum((capacity / sum(capacity))[rank <= 3]) * 100,
      cr5 = sum((capacity / sum(capacity))[rank <= 5]) * 100,
      .groups = "drop"
    )
  
  # 3.4. Calcular Gini y Palma
  inequality_results <- concentration_data %>%
    group_by(year, market) %>%
    summarise(
      gini_index = reldist::gini(capacity),
      # Palma ratio = share top 10% / share bottom 40%
      palma_ratio = {
        sorted <- sort(capacity, decreasing = TRUE)
        n <- length(sorted)
        top_10 <- sum(sorted[1:max(1, floor(n * 0.1))]) / sum(sorted)
        bottom_40 <- sum(sorted[ceiling(n * 0.6):n]) / sum(sorted)
        if (bottom_40 > 0) top_10 / bottom_40 else NA_real_
      },
      .groups = "drop"
    )
  
  # 3.5. Combinar todos los índices
  concentration_indices <- hhi_results %>%
    left_join(cr_results, by = c("year", "market")) %>%
    left_join(inequality_results, by = c("year", "market")) %>%
    mutate(
      # Interpretación HHI
      hhi_category = case_when(
        hhi_index < 1000 ~ "No concentrado",
        hhi_index < 1800 ~ "Moderadamente concentrado",
        hhi_index < 2500 ~ "Concentrado",
        TRUE ~ "Altamente concentrado"
      ),
      
      # Tendencia (si hay datos de múltiples años)
      hhi_trend = if (n() > 1) {
        c(NA, diff(hhi_index))
      } else {
        NA_real_
      },
      
      # Implicación estratégica
      strategic_implication = case_when(
        hhi_index < 1000 ~ "Alta fragmentación - dificultad de negociación",
        hhi_index < 1800 ~ "Competencia con líderes - oportunidad para 'divide y vencerás'",
        hhi_index < 2500 ~ "Oligopolio emergente - riesgo de coordinación criminal",
        TRUE ~ "Cuasi-monopolio - requiere estrategia de desarticulación prioritaria"
      )
    )
  
  # --------------------------------------------------------------------------
  # 4. ANÁLISIS DE SHARES DE MERCADO
  # --------------------------------------------------------------------------
  log_info("Analizando shares de mercado")
  
  # 4.1. Desanidar shares para análisis detallado
  market_shares_detail <- hhi_results %>%
    select(year, market, market_shares) %>%
    unnest(market_shares) %>%
    group_by(year, market) %>%
    mutate(
      rank = rank(-share, ties.method = "min"),
      category = case_when(
        rank == 1 ~ "Market Leader",
        rank <= 3 ~ "Top 3",
        rank <= 5 ~ "Top 5",
        rank <= 10 ~ "Top 10",
        TRUE ~ "Others"
      )
    ) %>%
    ungroup()
  
  # 4.2. Evolución de shares (solo para 2024-2025)
  share_evolution <- market_shares_detail %>%
    filter(year %in% c(2024, 2025)) %>%
    select(gao, market, year, share) %>%
    pivot_wider(
      id_cols = c(gao, market),
      names_from = year,
      values_from = share,
      names_prefix = "share_"
    ) %>%
    mutate(
      share_change = share_2025 - share_2024,
      change_direction = ifelse(share_change > 0, "Gained", "Lost"),
      change_magnitude = abs(share_change)
    )
  
  # 4.3. Movilidad de mercado (cambios en ranking)
  if (2024 %in% market_shares_detail$year && 2025 %in% market_shares_detail$year) {
    market_mobility <- market_shares_detail %>%
      filter(year %in% c(2024, 2025)) %>%
      select(gao, market, year, rank) %>%
      pivot_wider(
        id_cols = c(gao, market),
        names_from = year,
        values_from = rank,
        names_prefix = "rank_"
      ) %>%
      mutate(
        rank_change = rank_2024 - rank_2025,  # Positivo = mejoró ranking
        mobility_category = case_when(
          rank_change > 2 ~ "Significant Improvement",
          rank_change > 0 ~ "Slight Improvement",
          rank_change == 0 ~ "Stable",
          rank_change > -2 ~ "Slight Decline",
          TRUE ~ "Significant Decline"
        )
      )
  } else {
    market_mobility <- tibble(
      gao = character(),
      market = character(),
      rank_2024 = numeric(),
      rank_2025 = numeric(),
      rank_change = numeric(),
      mobility_category = character()
    )
  }
  
  # --------------------------------------------------------------------------
  # 5. ANÁLISIS DE DOMINANCIA DE MERCADO
  # --------------------------------------------------------------------------
  log_info("Analizando dominancia de mercado")
  
  # 5.1. Test de dominancia (criterio de la UE: >40% share)
  market_dominance <- market_shares_detail %>%
    filter(year == max(year)) %>%
    group_by(market) %>%
    mutate(
      is_dominant = share > 0.40,
      dominant_gap = if (sum(is_dominant) > 0) {
        max(share[!is_dominant], na.rm = TRUE) / max(share[is_dominant], na.rm = TRUE)
      } else {
        NA_real_
      }
    ) %>%
    ungroup()
  
  # 5.2. Barreras a la entrada (proxy: concentración)
  entry_barriers <- concentration_indices %>%
    filter(year == max(year)) %>%
    mutate(
      entry_barrier_level = case_when(
        hhi_index > 2500 ~ "Very High",
        hhi_index > 1800 ~ "High",
        hhi_index > 1000 ~ "Moderate",
        TRUE ~ "Low"
      ),
      barrier_implication = case_when(
        entry_barrier_level == "Very High" ~ "Mercado cerrado a nuevos actores",
        entry_barrier_level == "High" ~ "Alto costo de entrada",
        entry_barrier_level == "Moderate" ~ "Entrada posible con recursos",
        TRUE ~ "Mercado abierto a competencia"
      )
    )
  
  # --------------------------------------------------------------------------
  # 6. GUARDAR RESULTADOS DEL MODELO
  # --------------------------------------------------------------------------
  log_info("Guardando resultados del modelo")
  
  # 6.1. Guardar índices principales
  write_csv(concentration_indices, fs::path(DIRS$tables, "P4_concentration_indices.csv"))
  write_csv(market_shares_detail, fs::path(DIRS$tables, "P4_market_shares_detail.csv"))
  write_csv(share_evolution, fs::path(DIRS$tables, "P4_share_evolution.csv"))
  write_csv(market_mobility, fs::path(DIRS$tables, "P4_market_mobility.csv"))
  
  # 6.2. Guardar análisis de dominancia
  write_csv(market_dominance, fs::path(DIRS$tables, "P4_market_dominance.csv"))
  write_csv(entry_barriers, fs::path(DIRS$tables, "P4_entry_barriers.csv"))
  
  # 6.3. Guardar parámetros del modelo
  params_df <- tibble(
    parameter = names(unlist(params)),
    value = as.character(unlist(params))
  )
  write_csv(params_df, fs::path(DIRS$tables, "P4_model_parameters.csv"))
  
  # 6.4. Guardar metadatos
  model_metadata <- list(
    model_name = "P4_Concentration_HHI",
    timestamp = Sys.time(),
    analysis_years = paste(params$analysis_years, collapse = "-"),
    markets_analyzed = length(params$markets),
    total_observations = nrow(concentration_indices),
    hhi_range = paste(range(concentration_indices$hhi_index, na.rm = TRUE), collapse = "-"),
    model_version = "2.0"
  )
  
  write_csv(
    as_tibble(model_metadata),
    fs::path(DIRS$tables, "P4_model_metadata.csv")
  )
  
  # --------------------------------------------------------------------------
  # 7. REPORTE DEL MODELO
  # --------------------------------------------------------------------------
  log_info("Generando reporte del modelo")
  
  cat("\n✓ MODELO P4 COMPLETADO\n")
  cat("  • Mercados analizados: ", model_metadata$markets_analyzed, "\n")
  cat("  • Años: ", model_metadata$analysis_years, "\n")
  cat("  • Rango HHI: ", model_metadata$hhi_range, "\n")
  
  # Mostrar resumen ejecutivo por mercado (2025)
  current_year <- max(params$analysis_years)
  current_data <- concentration_indices %>%
    filter(year == current_year)
  
  cat("\nCONCENTRACIÓN DE MERCADO ", current_year, ":\n")
  cat("---------------------------------\n")
  for (i in 1:nrow(current_data)) {
    cd <- current_data[i, ]
    cat(sprintf("  • %-15s: HHI = %4.0f (%s)\n",
                cd$market,
                cd$hhi_index,
                cd$hhi_category))
    cat(sprintf("     Competidores efectivos: %3.1f\n", cd$effective_competitors))
    cat(sprintf("     CR3: %4.1f%%, CR5: %4.1f%%\n", cd$cr3, cd$cr5))
    cat(sprintf("     Implicación: %s\n", cd$strategic_implication))
  }
  
  # Mostrar dominancia de mercado
  cat("\nDOMINANCIA DE MERCADO (share >40%):\n")
  cat("-------------------------------------\n")
  dominant_actors <- market_dominance %>%
    filter(is_dominant) %>%
    arrange(market, desc(share))
  
  if (nrow(dominant_actors) > 0) {
    for (i in 1:nrow(dominant_actors)) {
      da <- dominant_actors[i, ]
      cat(sprintf("  • %-15s: %-20s (%4.1f%% share)\n",
                  da$market,
                  da$gao,
                  da$share * 100))
    }
  } else {
    cat("  • Ningún actor domina individualmente (>40% share)\n")
  }
  
  # Mostrar barreras a la entrada
  cat("\nBARRERAS A LA ENTRADA:\n")
  cat("-----------------------\n")
  for (i in 1:nrow(entry_barriers)) {
    eb <- entry_barriers[i, ]
    cat(sprintf("  • %-15s: %-15s - %s\n",
                eb$market,
                eb$entry_barrier_level,
                eb$barrier_implication))
  }
  
  cat("\n")
  
  # --------------------------------------------------------------------------
  # 8. EXPORTAR RESULTADOS
  # --------------------------------------------------------------------------
  assign("P4_CONCENTRATION", concentration_indices, envir = .GlobalEnv)
  assign("P4_SHARES", market_shares_detail, envir = .GlobalEnv)
  assign("P4_EVOLUTION", share_evolution, envir = .GlobalEnv)
  assign("P4_DOMINANCE", market_dominance, envir = .GlobalEnv)
  assign("P4_BARRIERS", entry_barriers, envir = .GlobalEnv)
  assign("P4_PARAMS", params, envir = .GlobalEnv)
  
  log_info("Resultados exportados al entorno global")
  
  return(list(
    concentration = concentration_indices,
    shares = market_shares_detail,
    evolution = share_evolution,
    dominance = market_dominance,
    barriers = entry_barriers,
    params = params,
    metadata = model_metadata
  ))
}

# ------------------------------------------------------------------------------
# 9. EJECUTAR MODELO
# ------------------------------------------------------------------------------
if (!interactive()) {
  # Ejecutar automáticamente
  p4_results <- model_concentration_hhi()
} else {
  cat("\nPara ejecutar el modelo de concentración, correr: model_concentration_hhi()\n")
}