# ==============================================================================
# MODELO P4 (FIP NATIVE): CONCENTRACIÓN DE LA AMENAZA POR INDICADOR (HHI)
# Defense Analytics: Concentration Analysis (HHI / CR3 / CR5 / Gini / Palma)
#
# Input:
#   - data/derived/02_panel_fip_2024_2025.csv
#
# “Mercados” (interpretación):
#   1) stock_2024  = valor_2024 (peso relativo por indicador)
#   2) stock_2025  = valor_2025
#   3) growth_pos  = max(delta_abs, 0)  (motores del deterioro)
#
# Outputs:
#   - outputs/tables/P4_concentration_indices.csv
#   - outputs/tables/P4_shares_detail.csv
#   - outputs/tables/P4_top_contributors_by_market.csv
#   - outputs/tables/P4_model_metadata.csv
#
# Exporta a .GlobalEnv:
#   - P4_CONCENTRATION, P4_SHARES, P4_TOP, P4_METADATA
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(fs)
  library(logger)
  library(readr)
  library(scales)
})

cat("\n[MODEL P4] CONCENTRACIÓN DE LA AMENAZA (FIP NATIVE - POR INDICADOR)\n")
cat("-------------------------------------------------------------------\n")

# ------------------------------------------------------------------------------
# Utilidades robustas (sin depender de paquetes externos)
# ------------------------------------------------------------------------------

gini_nonneg <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  x <- pmax(x, 0)
  s <- sum(x)
  if (s <= 0) return(0)
  x <- sort(x)
  n <- length(x)
  (2 * sum(seq_len(n) * x) / (n * s)) - (n + 1) / n
}

palma_ratio_nonneg <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  x <- pmax(x, 0)
  s <- sum(x)
  if (s <= 0) return(NA_real_)
  x <- sort(x, decreasing = TRUE)
  n <- length(x)
  # Top 10% vs Bottom 40%
  k_top <- max(1, floor(n * 0.10))
  idx_bottom_start <- ceiling(n * 0.60)
  top_10 <- sum(x[1:k_top]) / s
  bottom_40 <- sum(x[idx_bottom_start:n]) / s
  if (bottom_40 > 0) top_10 / bottom_40 else NA_real_
}

hhi_index <- function(shares) {
  shares <- shares[is.finite(shares)]
  if (length(shares) == 0) return(NA_real_)
  if (sum(shares) <= 0) return(NA_real_)
  sum(shares^2) * 10000
}

effective_competitors <- function(hhi) {
  if (!is.finite(hhi) || hhi <= 0) return(NA_real_)
  1 / (hhi / 10000)
}

hhi_category <- function(hhi) {
  if (!is.finite(hhi)) return(NA_character_)
  dplyr::case_when(
    hhi < 1000 ~ "No concentrado",
    hhi < 1800 ~ "Moderadamente concentrado",
    hhi < 2500 ~ "Concentrado",
    TRUE ~ "Altamente concentrado"
  )
}

hhi_implication <- function(hhi) {
  if (!is.finite(hhi)) return(NA_character_)
  dplyr::case_when(
    hhi < 1000 ~ "Alta fragmentación - difícil negociar/contener por un solo vector",
    hhi < 1800 ~ "Concentración moderada - hay vectores líderes explotables",
    hhi < 2500 ~ "Oligopolio emergente - riesgo de dominancia de pocos vectores",
    TRUE ~ "Cuasi-monopolio - un shock domina; respuesta debe ser prioritaria y quirúrgica"
  )
}

# ------------------------------------------------------------------------------
# FUNCIÓN PRINCIPAL
# ------------------------------------------------------------------------------
model_concentration_hhi <- function() {
  
  log_info("Iniciando P4 HHI (FIP native)")
  
  # Guardrails: DIRS
  if (!exists("DIRS")) stop("ERROR: DIRS no está definido. Ejecuta el SETUP primero.")
  if (is.null(DIRS$derived_data) || is.null(DIRS$tables)) {
    stop("ERROR: DIRS debe contener derived_data y tables. Revisa tu SETUP.")
  }
  
  in_file <- fs::path(DIRS$derived_data, "02_panel_fip_2024_2025.csv")
  if (!fs::file_exists(in_file)) {
    stop("ERROR: No existe 02_panel_fip_2024_2025.csv en: ", DIRS$derived_data,
         "\nEjecuta build_fip_panels() primero.")
  }
  
  df <- readr::read_csv(in_file, show_col_types = FALSE)
  
  needed <- c("categoria","indicador","unidad","valor_2024","valor_2025","delta_abs","delta_pct")
  miss <- setdiff(needed, names(df))
  if (length(miss) > 0) {
    stop("ERROR: Faltan columnas en 02_panel_fip_2024_2025.csv: ", paste(miss, collapse = ", "))
  }
  
  # Normalizar tipos
  base <- df %>%
    mutate(
      categoria  = as.character(categoria),
      indicador  = as.character(indicador),
      unidad     = as.character(unidad),
      valor_2024 = as.numeric(valor_2024),
      valor_2025 = as.numeric(valor_2025),
      delta_abs  = as.numeric(delta_abs),
      delta_pct  = as.numeric(delta_pct)
    ) %>%
    # al menos un valor existente
    filter(!(is.na(valor_2024) & is.na(valor_2025))) %>%
    ungroup()
  
  # “Mercados” (capacidades)
  markets_long <- base %>%
    transmute(
      categoria, indicador, unidad,
      stock_2024 = replace_na(valor_2024, 0),
      stock_2025 = replace_na(valor_2025, 0),
      growth_pos = pmax(replace_na(delta_abs, 0), 0)
    ) %>%
    pivot_longer(
      cols = c(stock_2024, stock_2025, growth_pos),
      names_to = "market",
      values_to = "capacity"
    ) %>%
    # Si todo es cero en un mercado, igual lo dejamos y el índice saldrá NA/0
    mutate(capacity = pmax(as.numeric(capacity), 0))
  
  # Shares detallados
  shares_detail <- markets_long %>%
    group_by(market) %>%
    mutate(
      total_market = sum(capacity, na.rm = TRUE),
      share = if_else(total_market > 0, capacity / total_market, 0),
      rank = dense_rank(desc(share))
    ) %>%
    ungroup() %>%
    mutate(
      bucket = case_when(
        rank == 1 ~ "Leader",
        rank <= 3 ~ "Top 3",
        rank <= 5 ~ "Top 5",
        rank <= 10 ~ "Top 10",
        TRUE ~ "Others"
      )
    )
  
  # Índices por mercado
  concentration_indices <- shares_detail %>%
    group_by(market) %>%
    summarise(
      n_indicators = n(),
      total_capacity = sum(capacity, na.rm = TRUE),
      hhi = hhi_index(share),
      effective_competitors = effective_competitors(hhi),
      cr3 = sum(share[rank <= 3], na.rm = TRUE) * 100,
      cr5 = sum(share[rank <= 5], na.rm = TRUE) * 100,
      gini = gini_nonneg(capacity),
      palma = palma_ratio_nonneg(capacity),
      category = hhi_category(hhi),
      strategic_implication = hhi_implication(hhi),
      .groups = "drop"
    ) %>%
    mutate(
      market_label = recode(
        market,
        stock_2024 = "Stock 2024 (nivel)",
        stock_2025 = "Stock 2025 (nivel)",
        growth_pos = "Crecimiento positivo 2024–2025 (motores)"
      )
    ) %>%
    select(market, market_label, everything())
  
  # Top contribuyentes por mercado
  top_by_market <- shares_detail %>%
    group_by(market) %>%
    arrange(desc(share)) %>%
    slice_head(n = 10) %>%
    ungroup() %>%
    select(market, categoria, indicador, unidad, capacity, share, rank, bucket) %>%
    arrange(market, rank)
  
  # Guardar outputs
  fs::dir_create(DIRS$tables, recurse = TRUE)
  
  out_idx   <- fs::path(DIRS$tables, "P4_concentration_indices.csv")
  out_share <- fs::path(DIRS$tables, "P4_shares_detail.csv")
  out_top   <- fs::path(DIRS$tables, "P4_top_contributors_by_market.csv")
  
  readr::write_csv(concentration_indices, out_idx)
  readr::write_csv(shares_detail, out_share)
  readr::write_csv(top_by_market, out_top)
  
  # Metadata
  model_metadata <- list(
    model_name = "P4_Concentration_HHI_FIP_Native",
    timestamp = as.character(Sys.time()),
    input_file = as.character(in_file),
    markets = "stock_2024;stock_2025;growth_pos",
    n_indicators = n_distinct(base$indicador),
    model_version = "1.0"
  )
  out_meta <- fs::path(DIRS$tables, "P4_model_metadata.csv")
  readr::write_csv(as_tibble(model_metadata), out_meta)
  
  # Reporte consola (quirúrgico)
  cat("\n✓ MODELO P4 COMPLETADO (FIP native)\n")
  cat("  • Input:  ", in_file, "\n", sep = "")
  cat("  • Output: ", out_idx, "\n", sep = "")
  cat("  • Indicadores: ", n_distinct(base$indicador), "\n", sep = "")
  
  cat("\nRESUMEN POR MERCADO:\n")
  cat("----------------------\n")
  for (i in 1:nrow(concentration_indices)) {
    x <- concentration_indices[i, ]
    cat(sprintf("  • %-35s | HHI=%4.0f (%s)\n",
                x$market_label, x$hhi, x$category))
    cat(sprintf("     Comp. efectivos: %3.1f | CR3=%4.1f%% | CR5=%4.1f%% | Gini=%0.3f | Palma=%0.2f\n",
                x$effective_competitors, x$cr3, x$cr5, x$gini, x$palma))
    cat(sprintf("     Implicación: %s\n", x$strategic_implication))
  }
  
  # Mensaje interpretativo especial para growth_pos (tu caso P3)
  gx <- concentration_indices %>% filter(market == "growth_pos")
  if (nrow(gx) == 1) {
    cat("\nLECTURA CLAVE (motores del deterioro):\n")
    cat("-------------------------------------\n")
    cat("  • Este mercado mide quién explica el crecimiento positivo 2024–2025.\n")
    cat("  • Si HHI es alto y CR3≈100%, tienes un shock concentrado en pocos indicadores.\n")
  }
  
  # Exportar
  assign("P4_CONCENTRATION", concentration_indices, envir = .GlobalEnv)
  assign("P4_SHARES", shares_detail, envir = .GlobalEnv)
  assign("P4_TOP", top_by_market, envir = .GlobalEnv)
  assign("P4_METADATA", model_metadata, envir = .GlobalEnv)
  
  log_info("P4 outputs guardados: {out_idx}, {out_share}, {out_top}, {out_meta}")
  log_info("P4 exportado a .GlobalEnv: P4_CONCENTRATION, P4_SHARES, P4_TOP, P4_METADATA")
  
  return(list(
    concentration = concentration_indices,
    shares = shares_detail,
    top = top_by_market,
    metadata = model_metadata,
    outputs = list(indices = out_idx, shares = out_share, top = out_top, meta = out_meta)
  ))
}

# ------------------------------------------------------------------------------
# EJECUCIÓN (manual)
# ------------------------------------------------------------------------------
if (!interactive()) {
  p4_results <- model_concentration_hhi()
} else {
  cat("\nPara ejecutar:\n  p4_results <- model_concentration_hhi()\n")
}

# Para ejecutar:
  p4_results <- model_concentration_hhi()
