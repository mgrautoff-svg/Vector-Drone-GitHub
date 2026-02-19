# ==============================================================================
# CONSTRUCCIÓN DE PANELES DERIVADOS - FIP
# Paneles consistentes con base: 01_fip_clean.csv
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(fs)
  library(logger)
})

cat("\n[PHASE 3] CONSTRUCCIÓN DE PANELES ANALÍTICOS (FIP)\n")
cat("---------------------------------------------------\n")

build_fip_panels <- function() {
  
  in_file <- fs::path(DIRS$derived_data, "01_fip_clean.csv")
  stopifnot(fs::file_exists(in_file))
  
  fip <- readr::read_csv(in_file, show_col_types = FALSE)
  
  # --------------------------------------------------------------------------
  # 1) PANEL LONG (ya limpio, con llave única)
  # --------------------------------------------------------------------------
  panel_long <- fip %>%
    select(categoria, indicador, año, valor, unidad, variacion_porcentual, notas_documento, missing_type) %>%
    arrange(categoria, indicador, año)
  
  # --------------------------------------------------------------------------
  # 2) PANEL WIDE 2024 vs 2025 (comparativo)
  #    Nota: solo para indicadores que tengan al menos uno de los dos años
  # --------------------------------------------------------------------------
  panel_2425 <- panel_long %>%
    filter(año %in% c(2024, 2025)) %>%
    select(categoria, indicador, unidad, año, valor) %>%
    pivot_wider(names_from = año, values_from = valor, names_prefix = "valor_") %>%
    mutate(
      delta_abs = valor_2025 - valor_2024,
      delta_pct = if_else(!is.na(valor_2024) & valor_2024 != 0,
                          (valor_2025 - valor_2024) / valor_2024,
                          NA_real_)
    ) %>%
    arrange(categoria, indicador)
  
  # --------------------------------------------------------------------------
  # 3) SCOREBOARD POR CATEGORÍA (resumen ejecutivo)
  #    - cuenta indicadores
  #    - cuenta cuántos tienen 2024 y 2025
  #    - conteo de NAs
  # --------------------------------------------------------------------------
  scoreboard <- panel_long %>%
    group_by(categoria) %>%
    summarise(
      n_indicators = n_distinct(indicador),
      n_rows = n(),
      n_years = n_distinct(año),
      share_valor_na = mean(is.na(valor)),
      share_var_na   = mean(is.na(variacion_porcentual)),
      .groups = "drop"
    ) %>%
    arrange(desc(n_indicators))
  
  # --------------------------------------------------------------------------
  # 4) Guardar outputs
  # --------------------------------------------------------------------------
  fs::dir_create(DIRS$derived_data, recurse = TRUE)
  
  out1 <- fs::path(DIRS$derived_data, "02_panel_fip_long.csv")
  out2 <- fs::path(DIRS$derived_data, "02_panel_fip_2024_2025.csv")
  out3 <- fs::path(DIRS$derived_data, "02_panel_fip_scoreboard.csv")
  
  readr::write_csv(panel_long, out1)
  readr::write_csv(panel_2425, out2)
  readr::write_csv(scoreboard, out3)
  
  log_info("Panel long guardado: {out1}")
  log_info("Panel 2024-2025 guardado: {out2}")
  log_info("Scoreboard guardado: {out3}")
  
  cat("\n✓ PANELES FIP CONSTRUIDOS\n")
  cat("  • Long:", nrow(panel_long), "filas\n")
  cat("  • 2024-2025:", nrow(panel_2425), "indicadores\n")
  cat("  • Scoreboard:", nrow(scoreboard), "categorías\n\n")
  
  if (interactive()) {
    assign("FIP_PANEL_LONG", panel_long, envir = .GlobalEnv)
    assign("FIP_PANEL_2425", panel_2425, envir = .GlobalEnv)
    assign("FIP_SCOREBOARD", scoreboard, envir = .GlobalEnv)
  }
  
  return(list(long = panel_long, panel_2425 = panel_2425, scoreboard = scoreboard))
}

if (interactive() && sys.nframe() == 0) {
  cat("\nPara ejecutar:\n  panels <- build_fip_panels()\n")
}
