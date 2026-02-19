# ==============================================================================
# VISUALIZACIONES ESTRATÉGICAS - DEFENSE ANALYTICS (FIP-NATIVE)
# Compatible con pipeline real: P3_DECOMPOSITION, P5_PROXY, P8_COMPONENTS
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
  library(ggplot2)
  library(ggrepel)
  library(patchwork)
  library(viridis)
  library(plotly)
  library(htmlwidgets)
  library(fs)
  library(logger)
})

cat("\n[PHASE 6] GENERANDO VISUALIZACIONES ESTRATÉGICAS (FIP-NATIVE)\n")
cat("------------------------------------------------------------\n")

# ------------------------------------------------------------------------------
# 1) ESTILOS
# ------------------------------------------------------------------------------
setup_strategic_theme <- function() {
  
  strategic_colors <- list(
    risk_scale = c("#4CAF50", "#FFC107", "#FF9800", "#F44336"),
    scenario_colors = c(
      "Continuidad" = "#D32F2F",
      "Contención Parcial" = "#FF9800",
      "Estabilización" = "#4CAF50",
      "Reversión" = "#2196F3",
      "Saturación" = "#9C27B0"
    ),
    classification_colors = c(
      "ALTO (ROJO)"  = "#F44336",
      "MEDIO (ÁMBAR)" = "#FF9800",
      "BAJO (VERDE)" = "#4CAF50"
    )
  )
  
  theme_strategic <- theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0, margin = margin(b = 10)),
      plot.subtitle = element_text(size = 12, hjust = 0, color = "gray40", margin = margin(b = 15)),
      plot.caption = element_text(size = 9, color = "gray60", hjust = 1, margin = margin(t = 10)),
      axis.title = element_text(face = "bold", size = 11),
      axis.text = element_text(size = 10),
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 10),
      legend.text = element_text(size = 9),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
  
  list(colors = strategic_colors, theme = theme_strategic)
}

# ------------------------------------------------------------------------------
# 2) FUNCIÓN PRINCIPAL
# ------------------------------------------------------------------------------
generate_strategic_visualizations <- function() {
  
  log_info("Iniciando generación de visualizaciones (FIP-native)")
  styles <- setup_strategic_theme()
  
  # --- Guardrails DIRS
  if (!exists("DIRS", envir = .GlobalEnv)) stop("ERROR: DIRS no existe. Ejecuta SETUP.")
  DIRS <- get("DIRS", envir = .GlobalEnv)
  
  if (!("figures" %in% names(DIRS))) DIRS$figures <- fs::path(DIRS$root, "outputs", "figures")
  fs::dir_create(DIRS$figures, recurse = TRUE)
  
  # --- Verificar objetos CORE (FIP-native)
  needed_core <- c("P3_DECOMPOSITION", "P5_PROXY", "P8_COMPONENTS", "P8_RISK_SCORE", "P8_CLASSIFICATION")
  missing_core <- needed_core[!vapply(needed_core, exists, logical(1), envir = .GlobalEnv)]
  if (length(missing_core) > 0) {
    stop("Faltan objetos core (ejecuta modelos antes): ", paste(missing_core, collapse = ", "))
  }
  
  # Cargar desde GlobalEnv (evita scoping raro)
  P3_DECOMPOSITION <- get("P3_DECOMPOSITION", envir = .GlobalEnv)
  P5_PROXY         <- get("P5_PROXY", envir = .GlobalEnv)
  P8_COMPONENTS    <- get("P8_COMPONENTS", envir = .GlobalEnv)
  P8_RISK_SCORE    <- get("P8_RISK_SCORE", envir = .GlobalEnv)
  P8_CLASSIFICATION<- get("P8_CLASSIFICATION", envir = .GlobalEnv)
  
  plots <- list()
  
  # --------------------------------------------------------------------------
  # P2 (OPCIONAL) — si existe P2_PROJECTIONS
  # --------------------------------------------------------------------------
  if (exists("P2_PROJECTIONS", envir = .GlobalEnv)) {
    
    log_info("Generando P2 (escenarios) — encontrado P2_PROJECTIONS")
    P2_PROJECTIONS <- get("P2_PROJECTIONS", envir = .GlobalEnv)
    
    # contrato mínimo esperado
    req_p2 <- c("year", "capacity", "scenario_name")
    if (all(req_p2 %in% names(P2_PROJECTIONS))) {
      
      p2_plot <- ggplot(P2_PROJECTIONS, aes(x = year, y = capacity, color = scenario_name)) +
        geom_line(linewidth = 1.2) +
        geom_point(size = 2.5) +
        geom_hline(yintercept = 40000, linetype = "dashed",
                   color = styles$colors$risk_scale[4], alpha = 0.6, linewidth = 0.8) +
        annotate("text",
                 x = min(P2_PROJECTIONS$year, na.rm = TRUE),
                 y = 41000,
                 label = "Umbral de Crisis (40k)",
                 hjust = 0, vjust = 0,
                 color = styles$colors$risk_scale[4],
                 size = 3.5, fontface = "bold") +
        scale_color_manual(values = styles$colors$scenario_colors) +
        scale_y_continuous(labels = label_number(scale = 1e-3, suffix = "k")) +
        labs(
          title = "P2 — Escenarios Estratégicos de Capacidad (2026–2030)",
          subtitle = "Proyección de integrantes totales bajo supuestos de política",
          x = "Año", y = "Integrantes (miles)", color = "Escenario",
          caption = "Fuente: FIP | Modelo: Defense Analytics"
        ) +
        styles$theme +
        theme(legend.position = "right")
      
      ggsave(fs::path(DIRS$figures, "P2_growth_scenarios.png"),
             p2_plot, width = 14, height = 8, dpi = 300)
      
      plots$p2 <- p2_plot
    } else {
      log_warn("P2_PROJECTIONS existe pero no trae columnas mínimas (year, capacity, scenario_name). Se omite.")
    }
    
  } else {
    log_warn("No existe P2_PROJECTIONS. Se omite P2.")
  }
  
  # --------------------------------------------------------------------------
  # P3 — DESCOMPOSICIÓN DEL CRECIMIENTO (FIP-native: por indicador)
  # --------------------------------------------------------------------------
  log_info("Generando P3 (descomposición)")
  
  # Asegurar nombres
  # Tu P3 usa: categoria, indicador, delta_abs (y delta_pos en el csv exportado)
  p3_data <- P3_DECOMPOSITION %>%
    mutate(
      delta_abs = as.numeric(delta_abs),
      delta_pos = pmax(delta_abs, 0),
      label = if_else(is.finite(delta_abs), sprintf("%+g", delta_abs), "NA")
    ) %>%
    # top 12 por magnitud (para que sea legible)
    mutate(abs_delta = abs(delta_abs)) %>%
    arrange(desc(abs_delta)) %>%
    slice_head(n = 12) %>%
    # ya vienes con p3_data ordenado; fija niveles explícitos
    mutate(indicador = factor(indicador, levels = unique(indicador)))
  
  
  p3_plot <- ggplot(p3_data, aes(x = delta_abs, y = indicador)) +
    geom_vline(xintercept = 0, color = "gray40", linewidth = 0.6) +
    geom_col(aes(fill = delta_abs > 0), width = 0.7, alpha = 0.9) +
    geom_text(aes(label = label),
              hjust = ifelse(p3_data$delta_abs >= 0, -0.1, 1.1),
              size = 3.4, fontface = "bold") +
    scale_fill_manual(values = c("TRUE" = styles$colors$risk_scale[4],
                                 "FALSE" = styles$colors$risk_scale[1]),
                      guide = "none") +
    scale_x_continuous(labels = label_number(big.mark = ","), expand = expansion(mult = c(0.1, 0.2))) +
    labs(
      title = "P3 — Descomposición del cambio (2024→2025)",
      subtitle = "Top indicadores por magnitud de Δ (positivos y negativos)",
      x = "Δ (valor absoluto reportado en panel)",
      y = NULL,
      caption = "Fuente: Panel FIP 2024–2025 | Modelo: P3 (FIP-native)"
    ) +
    styles$theme +
    theme(axis.text.y = element_text(face = "bold"),
          panel.grid.major.y = element_blank())
  
  ggsave(fs::path(DIRS$figures, "P3_growth_decomposition_fip.png"),
         p3_plot, width = 12, height = 7, dpi = 300)
  
  plots$p3 <- p3_plot
  
  # --------------------------------------------------------------------------
  # P5 — PROXY DE “GOBERNANZA” (FIP-native: por indicador)
  # --------------------------------------------------------------------------
  log_info("Generando P5 (proxy gobernanza)")
  
  p5_data <- P5_PROXY %>%
    mutate(governance_index = as.numeric(governance_index)) %>%
    arrange(desc(governance_index)) %>%
    slice_head(n = 12) %>%
    mutate(indicador = fct_reorder(indicador, governance_index))
  
  p5_plot <- ggplot(p5_data, aes(x = governance_index, y = indicador)) +
    geom_col(fill = styles$colors$risk_scale[3], alpha = 0.9, width = 0.7) +
    geom_text(aes(label = sprintf("%.3f", governance_index)),
              hjust = -0.1, size = 3.4, fontface = "bold") +
    scale_x_continuous(limits = c(0, max(p5_data$governance_index, na.rm = TRUE) * 1.15),
                       expand = expansion(mult = c(0, 0.02))) +
    labs(
      title = "P5 — Proxy de gobernanza criminal (por indicador)",
      subtitle = "Top 12 por índice compuesto P5",
      x = "Índice P5 (0–1, ponderado)",
      y = NULL,
      caption = "Fuente: Panel FIP 2024–2025 | Modelo: P5 (FIP-native)"
    ) +
    styles$theme +
    theme(axis.text.y = element_text(face = "bold"),
          panel.grid.major.y = element_blank())
  
  ggsave(fs::path(DIRS$figures, "P5_governance_proxy_top12.png"),
         p5_plot, width = 12, height = 7, dpi = 300)
  
  plots$p5 <- p5_plot
  
  # --------------------------------------------------------------------------
  # P8 — SCORE “AÑO ROJO” (FIP-native)
  # --------------------------------------------------------------------------
  log_info("Generando P8 (score año rojo)")
  
  p8_data <- P8_COMPONENTS %>%
    mutate(
      indicator_short = str_trunc(indicator_name, 28),
      weighted_score = as.numeric(weighted_score)
    ) %>%
    arrange(weighted_score) %>%
    mutate(indicator_short = factor(indicator_short, levels = indicator_short))
  
  p8_plot <- ggplot(p8_data, aes(x = indicator_short, y = weighted_score)) +
    geom_col(aes(fill = weighted_score), width = 0.7) +
    coord_flip() +
    # umbrales (líneas grises, no colores vectoriales para evitar warnings)
    geom_hline(yintercept = c(2, 4, 7), linetype = "dashed",
               color = "gray40", alpha = 0.6, linewidth = 0.7) +
    annotate("text",
             x = 1,
             y = max(p8_data$weighted_score, na.rm = TRUE) + 0.3,
             label = paste0("SCORE TOTAL: ", round(P8_RISK_SCORE, 1), "  |  ", P8_CLASSIFICATION),
             hjust = 0,
             color = styles$colors$classification_colors[[P8_CLASSIFICATION]],
             fontface = "bold",
             size = 4.5) +
    scale_fill_gradientn(colors = styles$colors$risk_scale) +
    scale_y_continuous(limits = c(0, max(9, max(p8_data$weighted_score, na.rm = TRUE) + 1)),
                       breaks = 0:10,
                       expand = expansion(mult = c(0, 0.05))) +
    labs(
      title = "P8 — Clasificador de riesgo sistémico: 'Año Rojo'",
      subtitle = "Score ponderado por indicador (2024→2025)",
      x = NULL,
      y = "Puntuación (ponderada)",
      caption = "Fuente: Panel FIP 2024–2025 | Score ≥7: Crisis (ROJO)"
    ) +
    styles$theme +
    theme(legend.position = "none",
          axis.text.y = element_text(face = "bold"),
          panel.grid.major.x = element_blank())
  
  ggsave(fs::path(DIRS$figures, "P8_red_year_score.png"),
         p8_plot, width = 10, height = 6, dpi = 300)
  
  plots$p8 <- p8_plot
  
  # --------------------------------------------------------------------------
  # DASHBOARD (solo con lo disponible)
  # --------------------------------------------------------------------------
  log_info("Generando dashboard (solo plots disponibles)")
  
  # Construir mosaico dinámico
  available <- names(plots)
  dashboard <- NULL
  
  if (length(available) >= 2) {
    # Orden preferente
    order_pref <- c("p2", "p3", "p5", "p8")
    available <- intersect(order_pref, available)
    
    # Layout simple: arriba P2 si existe, luego P3+P5, abajo P8
    if ("p2" %in% available) {
      top <- plots$p2 + theme(legend.position = "bottom")
      mid <- (plots$p3 | plots$p5)
      bot <- plots$p8
      dashboard <- top / mid / bot
    } else {
      # sin P2
      dashboard <- (plots$p3 | plots$p5) / plots$p8
    }
    
    dashboard <- dashboard +
      plot_annotation(
        title = "DASHBOARD ESTRATÉGICO — CAPACIDADES Y RIESGO (FIP-native)",
        subtitle = "Colombia 2024–2025 | Defense Analytics",
        caption = "Estándar editorial: RAND/Brookings | Pipeline: P3/P5/P8",
        theme = theme(
          plot.title = element_text(face = "bold", size = 18, hjust = 0.5, margin = margin(b = 6)),
          plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40", margin = margin(b = 10)),
          plot.caption = element_text(size = 9, color = "gray60", hjust = 0.5, margin = margin(t = 10)),
          plot.background = element_rect(fill = "white", color = NA)
        )
      )
    
    ggsave(fs::path(DIRS$figures, "STRATEGIC_DASHBOARD.png"),
           dashboard, width = 16, height = 18, dpi = 300)
  } else {
    log_warn("No hay suficientes plots para dashboard (se requieren >=2). Se omite.")
  }
  
  # --------------------------------------------------------------------------
  # INTERACTIVO (OPCIONAL) — si existe P2_PROJECTIONS
  # --------------------------------------------------------------------------
  p2_interactive <- NULL
  if (exists("P2_PROJECTIONS", envir = .GlobalEnv)) {
    P2_PROJECTIONS <- get("P2_PROJECTIONS", envir = .GlobalEnv)
    if (all(c("year","capacity","scenario_name") %in% names(P2_PROJECTIONS))) {
      p2_interactive <- plot_ly(
        data = P2_PROJECTIONS,
        x = ~year, y = ~capacity,
        color = ~scenario_name,
        type = "scatter", mode = "lines+markers",
        line = list(width = 3),
        marker = list(size = 8),
        hoverinfo = "text",
        text = ~paste0(
          "<b>", scenario_name, "</b><br>",
          "Año: ", year, "<br>",
          "Capacidad: ", format(capacity, big.mark = ",")
        )
      ) %>%
        layout(
          title = list(text = "<b>Escenarios Estratégicos (P2)</b>", x = 0.05),
          xaxis = list(title = "Año"),
          yaxis = list(title = "Integrantes", tickformat = ",.0f"),
          legend = list(orientation = "h", x = 0.5, y = -0.2, xanchor = "center")
        )
      
      htmlwidgets::saveWidget(
        p2_interactive,
        fs::path(DIRS$figures, "P2_interactive_scenarios.html"),
        selfcontained = TRUE
      )
    }
  }
  
  # --------------------------------------------------------------------------
  # REPORTE
  # --------------------------------------------------------------------------
  figures_generated <- fs::dir_ls(DIRS$figures, glob = "*.png")
  
  cat("\n✓ VISUALIZACIONES GENERADAS (FIP-NATIVE)\n")
  cat("  • Total PNG: ", length(figures_generated), "\n", sep = "")
  cat("  • Directorio: ", DIRS$figures, "\n", sep = "")
  cat("  • Archivos:\n")
  for (fig in basename(figures_generated)) cat("    - ", fig, "\n", sep = "")
  if (!is.null(p2_interactive)) cat("  • Interactivo: P2_interactive_scenarios.html\n")
  
  # Metadatos
  viz_metadata <- list(
    generation_time = as.character(Sys.time()),
    total_png = length(figures_generated),
    dashboard_created = !is.null(dashboard),
    interactive_created = !is.null(p2_interactive),
    theme_version = "FIP-native-1.0"
  )
  
  readr::write_csv(
    tibble::as_tibble(viz_metadata),
    fs::path(DIRS$figures, "visualization_metadata.csv")
  )
  
  invisible(list(
    plots = plots,
    dashboard = dashboard,
    interactive = p2_interactive,
    metadata = viz_metadata
  ))
}

# ------------------------------------------------------------------------------
# EJECUCIÓN
# ------------------------------------------------------------------------------
 visualizations <- generate_strategic_visualizations()
