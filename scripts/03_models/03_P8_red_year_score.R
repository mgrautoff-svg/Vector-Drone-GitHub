# ==============================================================================
# MODELO P8 (FIP-NATIVE): CLASIFICADOR DE RIESGO SISTÉMICO - "AÑO ROJO"
# Consistente con tu pipeline real (P3/P5): usa 02_panel_fip_2024_2025.csv
#
# Input:
#   - data/derived/02_panel_fip_2024_2025.csv
# Output:
#   - outputs/tables/P8_score_components.csv
#   - outputs/tables/P8_score_contribution.csv
#   - outputs/tables/P8_final_results.csv
#   - outputs/tables/P8_sensitivity_analysis.csv
#   - outputs/tables/P8_sensitivity_stats.csv
#   - outputs/tables/P8_model_parameters.csv
#   - outputs/tables/P8_model_metadata.csv
#   - outputs/tables/P8_strategic_implications.csv
#
# Exporta a .GlobalEnv:
#   - P8_RISK_SCORE, P8_CLASSIFICATION, P8_COMPONENTS, P8_CONTRIBUTION,
#     P8_SENSITIVITY, P8_IMPLICATIONS, P8_PARAMS
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
  library(fs)
  library(readr)
  library(logger)
})

cat("\n[MODEL P8] CLASIFICADOR DE RIESGO SISTÉMICO - AÑO ROJO (FIP-NATIVE)\n")
cat("-------------------------------------------------------------------\n")

# ------------------------------------------------------------------------------
# UTIL: %Δ robusto (en %). Si base <=0 o NA => NA
# ------------------------------------------------------------------------------
safe_pct <- function(v2024, v2025) {
  v2024 <- as.numeric(v2024); v2025 <- as.numeric(v2025)
  out <- rep(NA_real_, length(v2024))
  ok <- is.finite(v2024) & is.finite(v2025) & v2024 > 0
  out[ok] <- (v2025[ok] - v2024[ok]) / v2024[ok] * 100
  out
}

# ------------------------------------------------------------------------------
# FUNCIÓN PRINCIPAL
# ------------------------------------------------------------------------------
model_red_year_score_fip <- function() {
  
  log_info("Iniciando P8 (FIP-native)")
  
  # --- Guardrails DIRS
  if (!exists("DIRS", envir = .GlobalEnv)) stop("ERROR: DIRS no existe. Ejecuta SETUP.")
  DIRS <- get("DIRS", envir = .GlobalEnv)
  
  if (!("tables" %in% names(DIRS)))  DIRS$tables <- fs::path(DIRS$root, "outputs", "tables")
  if (!("derived_data" %in% names(DIRS))) stop("ERROR: DIRS$derived_data no existe.")
  fs::dir_create(DIRS$tables, recurse = TRUE)
  
  in_file <- fs::path(DIRS$derived_data, "02_panel_fip_2024_2025.csv")
  if (!fs::file_exists(in_file)) {
    stop("ERROR: No existe 02_panel_fip_2024_2025.csv en: ", DIRS$derived_data)
  }
  
  # --------------------------------------------------------------------------
  # 1) PARÁMETROS
  # --------------------------------------------------------------------------
  params <- list(
    years = c(2024, 2025),
    
    # Indicadores clave (ajusta nombres/pesos si quieres)
    key_indicators = tibble::tribble(
      ~indicator_code, ~indicator_name,                       ~weight, ~direction,
      "ENF",           "Enfrentamientos entre grupos",         1.0,     "+",
      "DRN",           "Ataques con drones",                   1.5,     "+",
      "SEQ",           "Secuestros",                           1.2,     "+",
      "DES",           "Desplazamiento forzado",               1.0,     "+",
      "HLS",           "Homicidios líderes sociales",          0.8,     "-",  # bajar es mejor
      "ATC",           "Ataques a misiones médicas",           0.9,     "+"
    ),
    
    # Umbrales de clasificación (puntos ponderados)
    thresholds = list(
      red = 7,
      amber = 4
    ),
    
    # Grilla de sensibilidad (variar 3 pesos)
    sensitivity_grid = list(
      ENF = seq(0.8, 1.2, by = 0.1),
      DRN = seq(1.3, 1.7, by = 0.1),
      SEQ = seq(1.0, 1.4, by = 0.1)
    )
  )
  
  max_possible <- sum(params$key_indicators$weight * 3)
  
  # --------------------------------------------------------------------------
  # 2) CARGA PANEL + MAPEO A CÓDIGOS (usa "indicador" del panel)
  # --------------------------------------------------------------------------
  p <- readr::read_csv(in_file, show_col_types = FALSE)
  
  required_cols <- c("categoria","indicador","unidad","valor_2024","valor_2025","delta_abs","delta_pct")
  miss <- setdiff(required_cols, names(p))
  if (length(miss) > 0) stop("ERROR: faltan columnas en panel: ", paste(miss, collapse = ", "))
  
  p <- p %>%
    mutate(
      indicador  = as.character(indicador),
      categoria  = as.character(categoria),
      valor_2024 = as.numeric(valor_2024),
      valor_2025 = as.numeric(valor_2025),
      delta_abs  = as.numeric(delta_abs),
      delta_pct  = as.numeric(delta_pct)
    ) %>%
    mutate(ind_key = str_to_lower(indicador)) %>%
    mutate(
      indicator_code = case_when(
        str_detect(ind_key, "enfrent") ~ "ENF",
        str_detect(ind_key, "dron")    ~ "DRN",
        str_detect(ind_key, "secuest") ~ "SEQ",
        str_detect(ind_key, "desplaz") ~ "DES",
        str_detect(ind_key, "homicid") & str_detect(ind_key, "lider") ~ "HLS",
        str_detect(ind_key, "mision") & str_detect(ind_key, "medic")  ~ "ATC",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(indicator_code %in% params$key_indicators$indicator_code)
  
  if (nrow(p) == 0) stop("ERROR: no se pudieron mapear indicadores del panel a los códigos P8.")
  
  # Si hay duplicados por código (p.ej. 2 filas mapean a DRN), agregamos (suma niveles y deltas)
  p8_base <- p %>%
    group_by(indicator_code) %>%
    summarise(
      indicador = first(indicador),
      categoria = first(categoria),
      valor_2024 = sum(valor_2024, na.rm = TRUE),
      valor_2025 = sum(valor_2025, na.rm = TRUE),
      # si delta_abs viene NA masivo, lo recalculamos al final desde niveles
      delta_abs  = sum(delta_abs, na.rm = TRUE),
      delta_pct  = NA_real_,
      .groups = "drop"
    )
  
  # Recalcular delta_abs si salió 0 por NA-sum pero hay niveles válidos
  p8_base <- p8_base %>%
    mutate(
      delta_abs = if_else(
        (!is.finite(delta_abs) | delta_abs == 0) & is.finite(valor_2024) & is.finite(valor_2025),
        valor_2025 - valor_2024,
        delta_abs
      ),
      pct_change = safe_pct(valor_2024, valor_2025)
    )
  
  # --------------------------------------------------------------------------
  # 3) SCORE
  # --------------------------------------------------------------------------
  risk_changes <- params$key_indicators %>%
    left_join(p8_base, by = "indicator_code") %>%
    mutate(
      adjusted_change = case_when(
        direction == "-" ~ -pct_change,
        TRUE ~ pct_change
      ),
      raw_score = case_when(
        is.na(adjusted_change) ~ 0,
        adjusted_change >= 100 ~ 3,
        adjusted_change >= 50  ~ 2,
        adjusted_change >= 20  ~ 1,
        TRUE ~ 0
      ),
      weighted_score = raw_score * weight
    )
  
  total_score <- sum(risk_changes$weighted_score, na.rm = TRUE)
  
  risk_classification <- dplyr::case_when(
    total_score >= params$thresholds$red ~ "ALTO (ROJO)",
    total_score >= params$thresholds$amber ~ "MEDIO (ÁMBAR)",
    TRUE ~ "BAJO (VERDE)"
  )
  
  score_components <- risk_changes %>%
    transmute(
      indicator_code, indicator_name,
      categoria, indicador,
      valor_2024, valor_2025,
      pct_change,
      raw_score, weight, weighted_score
    ) %>%
    arrange(desc(weighted_score), desc(raw_score))
  
  score_contribution <- score_components %>%
    mutate(contribution_pct = ifelse(total_score > 0, weighted_score / total_score * 100, 0))
  
  
  
  # --------------------------------------------------------------------------
  # 4) SENSIBILIDAD (3 pesos)
  # --------------------------------------------------------------------------
  base_weights <- params$key_indicators %>% select(indicator_code, weight) %>% deframe()
  raw_by_code  <- risk_changes %>% select(indicator_code, raw_score) %>% deframe()
  
  sensitivity_weights <- tidyr::expand_grid(
    weight_ENF = params$sensitivity_grid$ENF,
    weight_DRN = params$sensitivity_grid$DRN,
    weight_SEQ = params$sensitivity_grid$SEQ
  ) %>%
    mutate(
      scenario_id = row_number(),
      total_score_varied = pmap_dbl(
        list(weight_ENF, weight_DRN, weight_SEQ),
        function(w_enf, w_drn, w_seq) {
          w_map <- base_weights
          w_map[["ENF"]] <- w_enf
          w_map[["DRN"]] <- w_drn
          w_map[["SEQ"]] <- w_seq
          sum(raw_by_code[names(w_map)] * unname(w_map), na.rm = TRUE)
        }
      ),
      classification_varied = case_when(
        total_score_varied >= params$thresholds$red ~ "ROJO",
        total_score_varied >= params$thresholds$amber ~ "ÁMBAR",
        TRUE ~ "VERDE"
      )
    )
  
  sensitivity_stats <- sensitivity_weights %>%
    summarise(
      min_score = min(total_score_varied, na.rm = TRUE),
      max_score = max(total_score_varied, na.rm = TRUE),
      mean_score = mean(total_score_varied, na.rm = TRUE),
      sd_score = sd(total_score_varied, na.rm = TRUE),
      pct_red = mean(classification_varied == "ROJO") * 100,
      pct_amber = mean(classification_varied == "ÁMBAR") * 100,
      pct_green = mean(classification_varied == "VERDE") * 100,
      .groups = "drop"
    )
  
  # --------------------------------------------------------------------------
  # 5) IMPLICACIONES ESTRATÉGICAS (simple y usable)
  # --------------------------------------------------------------------------
  strategic_implications <- list(
    red = list(
      immediate_actions = c(
        "Activar protocolo de crisis estratégica",
        "Revisión urgente de doctrina operativa",
        "Aumento de inteligencia y vigilancia",
        "Coordinación interagencial prioritaria"
      ),
      recommended_changes = c(
        "Reasignación de recursos a zonas críticas",
        "Refuerzo de capacidades de respuesta rápida",
        "Diplomacia de crisis con actores regionales"
      ),
      warning_indicators = score_components %>%
        filter(weighted_score >= 2) %>%
        pull(indicator_name)
    ),
    amber = list(
      immediate_actions = c(
        "Reforzar monitoreo de indicadores",
        "Ajustes operativos preventivos",
        "Coordinación interinstitucional"
      ),
      recommended_changes = c(
        "Mejora de capacidades de inteligencia",
        "Fortalecimiento de respuesta temprana",
        "Diálogo preventivo con comunidades"
      )
    ),
    green = list(
      immediate_actions = c(
        "Mantener monitoreo rutinario",
        "Continuar estrategias actuales",
        "Fortalecer capacidades preventivas"
      ),
      recommended_changes = c(
        "Inversión en capacidades a largo plazo",
        "Mejora de sistemas de alerta temprana",
        "Diplomacia preventiva"
      )
    )
  )
  
  key <- str_extract(risk_classification, "ROJO|ÁMBAR|VERDE")
  current_implications <- switch(
    key,
    "ROJO" = strategic_implications$red,
    "ÁMBAR" = strategic_implications$amber,
    "VERDE" = strategic_implications$green
  )
  
  # --------------------------------------------------------------------------
  # 6) EXPORTS
  # --------------------------------------------------------------------------
  out_components    <- fs::path(DIRS$tables, "P8_score_components.csv")
  out_contribution  <- fs::path(DIRS$tables, "P8_score_contribution.csv")
  out_final         <- fs::path(DIRS$tables, "P8_final_results.csv")
  out_sens          <- fs::path(DIRS$tables, "P8_sensitivity_analysis.csv")
  out_sens_stats    <- fs::path(DIRS$tables, "P8_sensitivity_stats.csv")
  out_params        <- fs::path(DIRS$tables, "P8_model_parameters.csv")
  out_meta          <- fs::path(DIRS$tables, "P8_model_metadata.csv")
  out_impl          <- fs::path(DIRS$tables, "P8_strategic_implications.csv")
  
  readr::write_csv(score_components, out_components)
  readr::write_csv(score_contribution, out_contribution)
  readr::write_csv(sensitivity_weights, out_sens)
  readr::write_csv(sensitivity_stats, out_sens_stats)
  
  params_df <- params$key_indicators %>%
    mutate(parameter = paste0("key_indicators.", indicator_code)) %>%
    select(parameter, indicator_name, weight, direction)
  readr::write_csv(params_df, out_params)
  
  final_results <- tibble::tibble(
    metric = c("total_score", "risk_classification", "max_possible", "input_panel"),
    value  = as.character(c(round(total_score, 3), risk_classification, round(max_possible, 3), basename(in_file)))
  )
  readr::write_csv(final_results, out_final)
  
  implications_df <- tibble::tibble(
    classification = risk_classification,
    immediate_actions = paste(current_implications$immediate_actions, collapse = "; "),
    recommended_changes = paste(current_implications$recommended_changes, collapse = "; "),
    critical_indicators = if (risk_classification == "ALTO (ROJO)" && !is.null(current_implications$warning_indicators)) {
      paste(current_implications$warning_indicators, collapse = "; ")
    } else {
      NA_character_
    }
  )
  readr::write_csv(implications_df, out_impl)
  
  model_metadata <- tibble::tibble(
    model_name = "P8_Red_Year_Score_FIP",
    timestamp = as.character(Sys.time()),
    years = paste(params$years, collapse = "-"),
    total_score = total_score,
    risk_classification = risk_classification,
    indicators_used = nrow(params$key_indicators),
    sensitivity_range = paste(round(sensitivity_stats$min_score, 1), round(sensitivity_stats$max_score, 1), sep = " - "),
    model_version = "1.0"
  )
  readr::write_csv(model_metadata, out_meta)
  
  # --------------------------------------------------------------------------
  # 7) REPORTE CONSOLA
  # --------------------------------------------------------------------------
  cat("\n✓ MODELO P8 COMPLETADO (FIP-native)\n")
  cat("  • Input:  ", in_file, "\n", sep = "")
  cat("  • Score total: ", round(total_score, 1), " / ", round(max_possible, 1), "\n", sep = "")
  cat("  • Clasificación: ", risk_classification, "\n", sep = "")
  cat("  • Rango sensibilidad: ", model_metadata$sensitivity_range, "\n", sep = "")
  
  cat("\nDESGLOSE DEL SCORE POR INDICADOR:\n")
  cat("-----------------------------------\n")
  for (i in seq_len(nrow(score_components))) {
    sc <- score_components[i, ]
    pct_print <- ifelse(is.na(sc$pct_change), "NA", sprintf("%+.0f%%", sc$pct_change))
    cat(sprintf("  • %-25s: %7s → %1.0f puntos (ponderado: %3.1f)\n",
                sc$indicator_name, pct_print, sc$raw_score, sc$weighted_score))
  }
  
  # --------------------------------------------------------------------------
  # 8) EXPORTAR A .GlobalEnv
  # --------------------------------------------------------------------------
  assign("P8_RISK_SCORE", total_score, envir = .GlobalEnv)
  assign("P8_CLASSIFICATION", risk_classification, envir = .GlobalEnv)
  assign("P8_COMPONENTS", score_components, envir = .GlobalEnv)
  assign("P8_CONTRIBUTION", score_contribution, envir = .GlobalEnv)
  assign("P8_SENSITIVITY", sensitivity_stats, envir = .GlobalEnv)
  assign("P8_IMPLICATIONS", current_implications, envir = .GlobalEnv)
  assign("P8_PARAMS", params, envir = .GlobalEnv)
  
  log_info("P8 exportado a .GlobalEnv")
  
  return(list(
    total_score = total_score,
    classification = risk_classification,
    components = score_components,
    contribution = score_contribution,
    sensitivity = sensitivity_stats,
    implications = current_implications,
    params = params,
    metadata = model_metadata,
    files = list(
      components = out_components,
      contribution = out_contribution,
      final = out_final,
      sensitivity = out_sens,
      sensitivity_stats = out_sens_stats,
      params = out_params,
      metadata = out_meta,
      implications = out_impl
    )
  ))
}

# ------------------------------------------------------------------------------
# EJECUCIÓN (manual)
# ------------------------------------------------------------------------------
 p8_results <- model_red_year_score_fip()

