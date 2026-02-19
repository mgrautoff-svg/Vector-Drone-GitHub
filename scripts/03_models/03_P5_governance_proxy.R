# ==============================================================================
# MODELO P5: PROXY DE “GOBERNANZA CRIMINAL” (FIP native - por indicador)
# Corrección robusta:
#  - NO depende de PANEL_ACTOR_YEAR
#  - Usa 02_panel_fip_2024_2025.csv (tu input real)
#  - safe_rescale() para evitar NA masivo cuando el rango es 0 o todo NA
#  - Manejo explícito de indicadores con solo variación / sin nivel
#  - Outputs consistentes en outputs/tables/
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(fs)
  library(logger)
  library(scales)
})

cat("\n[MODEL P5] PROXY DE GOBERNANZA (FIP native - por indicador)\n")
cat("----------------------------------------------------------\n")

# ------------------------------------------------------------------------------
# 0) UTIL: RESCALE SEGURO (evita NA cuando rango=0 o todo NA)
# ------------------------------------------------------------------------------
safe_rescale <- function(x, to = c(0, 1)) {
  x <- as.numeric(x)
  x[!is.finite(x)] <- NA_real_
  
  if (length(x) == 0) return(numeric(0))
  if (all(is.na(x))) return(rep(0, length(x)))
  
  rng <- range(x, na.rm = TRUE)
  if (!is.finite(rng[1]) || !is.finite(rng[2]) || diff(rng) == 0) {
    return(rep(0, length(x)))
  }
  
  scales::rescale(x, to = to, na.rm = TRUE)
}

# ------------------------------------------------------------------------------
# 1) FUNCIÓN PRINCIPAL
# ------------------------------------------------------------------------------
model_governance_proxy <- function() {
  
  log_info("Iniciando P5 (FIP native)")
  
  # --------- Verificaciones de rutas (respeta tu setup)
  stopifnot(exists("DIRS"))
  stopifnot(dir_exists(DIRS$derived_data))
  
  # Asegurar carpeta de tablas (tu estructura real usa outputs/tables/)
  # Si ya tienes DIRS$tables, úsala; si no, fallback a outputs/tables
  if (!("tables" %in% names(DIRS))) {
    DIRS$tables <- fs::path(DIRS$root, "outputs", "tables")
  }
  fs::dir_create(DIRS$tables, recurse = TRUE)
  
  in_file <- fs::path(DIRS$derived_data, "02_panel_fip_2024_2025.csv")
  if (!fs::file_exists(in_file)) {
    stop("ERROR: No existe 02_panel_fip_2024_2025.csv en: ", DIRS$derived_data,
         "\nEjecuta primero el builder del panel (P2).")
  }
  
  # --------- Parámetros del índice (ajustables)
  params <- list(
    # pesos (suma 1)
    weights = list(
      control_social    = 0.40,  # coerción sobre civiles / control social
      innovation_tactic = 0.25,  # adaptación / innovación táctica
      dispute_compete   = 0.20,  # competencia / disputa inter-actor
      shock_growth      = 0.15   # magnitud del shock (Δ o Δ%)
    ),
    
    # umbrales para tipología (heurísticos)
    thresholds = list(
      high_control = 0.60,
      high_innov   = 0.50,
      high_dispute = 0.50
    )
  )
  
  # ----------------------------------------------------------------------------
  # 2) CARGA Y NORMALIZACIÓN DEL PANEL (FIP 2024 vs 2025)
  # ----------------------------------------------------------------------------
  p <- readr::read_csv(in_file, show_col_types = FALSE)
  
  # Contrato mínimo esperado del panel
  required_cols <- c("categoria", "indicador", "unidad", "valor_2024", "valor_2025", "delta_abs", "delta_pct")
  missing_cols <- setdiff(required_cols, names(p))
  if (length(missing_cols) > 0) {
    stop("ERROR: 02_panel_fip_2024_2025.csv no trae columnas requeridas: ",
         paste(missing_cols, collapse = ", "))
  }
  
  p <- p %>%
    mutate(
      categoria  = as.character(categoria),
      indicador  = as.character(indicador),
      unidad     = as.character(unidad),
      valor_2024 = as.numeric(valor_2024),
      valor_2025 = as.numeric(valor_2025),
      delta_abs  = as.numeric(delta_abs),
      delta_pct  = as.numeric(delta_pct)
    )
  
  # ----------------------------------------------------------------------------
  # 3) FEATURES: separar “nivel”, “shock”, “crecimiento” y “posición”
  # ----------------------------------------------------------------------------
  p <- p %>%
    mutate(
      # crecimiento positivo
      delta_pos = if_else(is.finite(delta_abs), pmax(delta_abs, 0), 0),
      
      # shock: usa Δabs si existe; si no, Δpct; si no, NA
      shock_ratio = case_when(
        is.finite(delta_abs) ~ abs(delta_abs),
        is.finite(delta_pct) ~ abs(delta_pct),
        TRUE ~ NA_real_
      ),
      
      # stock 2025 (nivel)
      stock_2025 = if_else(is.finite(valor_2025), valor_2025, NA_real_)
    )
  
  # Shares (con protección sum=0)  --- FIX if_else vectorial
  sum_stock_2025 <- sum(p$stock_2025, na.rm = TRUE)
  sum_growth_pos <- sum(p$delta_pos, na.rm = TRUE)
  
  p <- p %>%
    mutate(
      stock_share_2025 = if_else(
        sum_stock_2025 > 0 & is.finite(stock_2025),
        stock_2025 / sum_stock_2025,
        0
      ),
      growth_share_pos = if_else(
        sum_growth_pos > 0 & is.finite(delta_pos),
        delta_pos / sum_growth_pos,
        0
      )
    )
  
  # ----------------------------------------------------------------------------
  # 4) MAPEO HEURÍSTICO: qué mide cada indicador (control / innovación / disputa)
  # ----------------------------------------------------------------------------
  p <- p %>%
    mutate(
      indicator_key = str_to_lower(indicador),
      
      # flags por dimensión (0/1)
      is_control_social = if_else(
        categoria %in% c("Seguridad_Ciudadana","Impacto_Humanitario") |
          str_detect(indicator_key, "extors|amenaz|secuest|homicid|confin|desplaz|misiones|lideres"),
        1, 0
      ),
      is_innovation = if_else(
        str_detect(indicator_key, "dron|drones"),
        1, 0
      ),
      is_dispute = if_else(
        categoria %in% c("Conflicto") &
          str_detect(indicator_key, "enfrent|disputa|zonas"),
        1, 0
      ),
      
      # severidad: prioriza Δabs; si no, nivel; si no, Δpct
      severity = case_when(
        is.finite(delta_abs)  ~ abs(delta_abs),
        is.finite(valor_2025) ~ abs(valor_2025),
        is.finite(delta_pct)  ~ abs(delta_pct),
        TRUE ~ NA_real_
      ),
      
      score_control_raw = is_control_social * severity,
      score_innov_raw   = is_innovation     * severity,
      score_dispute_raw = is_dispute        * severity
    )
  
  # ----------------------------------------------------------------------------
  # 5) NORMALIZACIÓN ROBUSTA (sin NA masivo)
  # ----------------------------------------------------------------------------
  p <- p %>%
    mutate(
      component_control = safe_rescale(score_control_raw),
      component_innov   = safe_rescale(score_innov_raw),
      component_dispute = safe_rescale(score_dispute_raw),
      component_shock   = safe_rescale(shock_ratio),
      component_stock   = safe_rescale(stock_share_2025),
      component_growth  = safe_rescale(growth_share_pos)
    )
  
  # ----------------------------------------------------------------------------
  # 6) ÍNDICE COMPUESTO P5 (por indicador)
  # ----------------------------------------------------------------------------
  w <- params$weights
  params$weights$stock_level <- 0.05   # peso stock (solo cuando no hay shock)
  params$thresholds$cap_no_delta <- 0.08
  
  p5 <- p %>%
    mutate(
      has_delta = is.finite(delta_abs) | is.finite(delta_pct),
      
      governance_index = if_else(
        has_delta,
        # con evidencia dinámica: índice completo (sin stock)
        coalesce(component_control, 0) * w$control_social +
          coalesce(component_innov,   0) * w$innovation_tactic +
          coalesce(component_dispute, 0) * w$dispute_compete +
          coalesce(component_shock,   0) * w$shock_growth,
        # sin evidencia dinámica: solo stock, con techo
        pmin(
          coalesce(component_stock, 0) * params$weights$stock_level,
          params$thresholds$cap_no_delta
        )
      ),
      
      governance_type = case_when(
        component_control >= params$thresholds$high_control & component_shock >= 0.30 ~ "Control social + shock",
        component_innov   >= params$thresholds$high_innov                           ~ "Innovación táctica",
        component_dispute >= params$thresholds$high_dispute                         ~ "Competencia/Disputa",
        component_stock   >= 0.50                                                   ~ "Dominante por nivel (stock)",
        TRUE ~ "Mixto"
      ),
      
      strategic_priority = case_when(
        governance_index >= 0.70 ~ "Alta Prioridad",
        governance_index >= 0.40 ~ "Media Prioridad",
        TRUE ~ "Baja Prioridad"
      )
    ) %>%
    arrange(desc(governance_index))
  
  
  # ----------------------------------------------------------------------------
  # 7) TABLAS RESUMEN + DIAGNÓSTICOS
  # ----------------------------------------------------------------------------
  top_table <- p5 %>%
    transmute(
      categoria,
      indicador,
      idx = round(governance_index, 3),
      tipo = governance_type,
      delta_abs = delta_abs,
      delta_pct = delta_pct
    )
  
  summary_type <- p5 %>%
    group_by(governance_type, strategic_priority) %>%
    summarise(
      n = n(),
      sum_stock_2025 = sum(stock_2025, na.rm = TRUE),
      sum_delta_net  = sum(delta_abs, na.rm = TRUE),
      sum_delta_pos  = sum(delta_pos, na.rm = TRUE),
      avg_idx = mean(governance_index, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(sum_delta_pos), desc(sum_stock_2025))
  
  diagnostics <- p5 %>%
    summarise(
      n_indicators = n(),
      n_na_idx = sum(is.na(governance_index)),
      n_na_delta_abs = sum(is.na(delta_abs)),
      n_na_2024 = sum(is.na(valor_2024)),
      n_na_2025 = sum(is.na(valor_2025)),
      sum_net = sum(delta_abs, na.rm = TRUE),
      sum_pos = sum(delta_pos, na.rm = TRUE)
    )
  
  # ----------------------------------------------------------------------------
  # 8) EXPORTS
  # ----------------------------------------------------------------------------
  out_main   <- fs::path(DIRS$tables, "P5_governance_proxy.csv")
  out_top    <- fs::path(DIRS$tables, "P5_governance_top_table.csv")
  out_sum    <- fs::path(DIRS$tables, "P5_governance_summary.csv")
  out_diag   <- fs::path(DIRS$tables, "P5_governance_diagnostics.csv")
  out_params <- fs::path(DIRS$tables, "P5_model_parameters.csv")
  
  readr::write_csv(p5, out_main)
  readr::write_csv(top_table, out_top)
  readr::write_csv(summary_type, out_sum)
  readr::write_csv(diagnostics, out_diag)
  
  params_df <- tibble(
    parameter = c(
      "weights.control_social","weights.innovation_tactic","weights.dispute_compete","weights.shock_growth",
      "thresholds.high_control","thresholds.high_innov","thresholds.high_dispute"
    ),
    value = c(
      w$control_social, w$innovation_tactic, w$dispute_compete, w$shock_growth,
      params$thresholds$high_control, params$thresholds$high_innov, params$thresholds$high_dispute
    )
  )
  readr::write_csv(params_df, out_params)
  
  # ----------------------------------------------------------------------------
  # 9) REPORTE CONSOLA (compacto)
  # ----------------------------------------------------------------------------
  cat("\nTOP DINÁMICO (solo indicadores con Δ):\n")
  cat("-------------------------------------\n")
  
  top_dyn <- p5 %>%
    dplyr::filter(is.finite(delta_abs) | is.finite(delta_pct)) %>%
    dplyr::transmute(
      categoria,
      indicador,
      idx = round(governance_index, 3),
      tipo = governance_type,
      delta_abs = delta_abs
    ) %>%
    dplyr::arrange(desc(idx)) %>%
    dplyr::slice_head(n = 10)
  
  for (i in seq_len(nrow(top_dyn))) {
    r <- top_dyn[i, ]
    cat(sprintf("  %2d) %s :: %s | idx=%s | tipo=%s | Δ=%s\n",
                i,
                r$categoria,
                r$indicador,
                format(r$idx, nsmall = 3),
                r$tipo,
                ifelse(is.na(r$delta_abs), "NA", sprintf("%+g", r$delta_abs))))
  }
  
  cat("\nNOTA: El panel trae Δ para ", nrow(top_dyn),
      " indicadores; el resto no reporta variación.\n", sep = "")
  
  
  
  
  cat("\nMODELO P5 COMPLETADO (FIP native - por indicador)\n")
  cat("  • Input:  ", in_file, "\n", sep = "")
  cat("  • Output: ", out_main, "\n", sep = "")
  cat("  • Indicadores analizados: ", nrow(p5), "\n", sep = "")
  
  diag_row <- diagnostics %>% slice(1)
  cat("  • Crecimiento NETO (Δ abs): ", diag_row$sum_net, "\n", sep = "")
  cat("  • Crecimiento POSITIVO (+): +", diag_row$sum_pos, "\n", sep = "")
  
  cat("\nTOP 10 POR ÍNDICE (P5):\n")
  cat("----------------------\n")
  top10 <- top_table %>% slice_head(n = 10)
  for (i in seq_len(nrow(top10))) {
    r <- top10[i, ]
    cat(sprintf("  %2d) %s :: %s | idx=%s | tipo=%s | Δ=%s\n",
                i,
                r$categoria,
                r$indicador,
                ifelse(is.na(r$idx), "NA", format(r$idx, nsmall = 3)),
                r$tipo,
                ifelse(is.na(r$delta_abs), "NA", sprintf("%+g", r$delta_abs))))
  }
  
  # Exportar objetos
  assign("P5_PROXY", p5, envir = .GlobalEnv)
  assign("P5_PROXY_SUMMARY", summary_type, envir = .GlobalEnv)
  assign("P5_PROXY_DIAGNOSTICS", diagnostics, envir = .GlobalEnv)
  assign("P5_PARAMS", params, envir = .GlobalEnv)
  
  log_info("P5 exportado: P5_PROXY, P5_PROXY_SUMMARY, P5_PROXY_DIAGNOSTICS, P5_PARAMS")
  
  return(list(
    governance   = p5,
    top_table    = top_table,
    summary      = summary_type,
    diagnostics  = diagnostics,
    params       = params,
    files        = list(main = out_main, top = out_top, summary = out_sum, diag = out_diag, params = out_params)
  ))
}



# ------------------------------------------------------------------------------
# EJECUCIÓN (manual)
# ------------------------------------------------------------------------------
p5_results <- model_governance_proxy()
