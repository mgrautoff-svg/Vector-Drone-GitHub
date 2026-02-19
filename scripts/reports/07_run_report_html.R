# =========================================================
# DEFENSE ANALYTICS — HTML RUN REPORT (ACCORDION + PREVIEW CSV)
# Genera: outputs/reports/run_report_YYYYMMDD_HHMMSS.html
# =========================================================

suppressPackageStartupMessages({
  library(fs)
  library(glue)
  library(readr)
  library(dplyr)
})

# -------- Paths --------
panel_path  <- "data/derived/02_panel_fip_2024_2025.csv"
tables_dir  <- "outputs/tables"
figs_dir    <- "outputs/figures"
reports_dir <- "outputs/reports"
logs_dir    <- "outputs/logs"

dir_create(reports_dir)

stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
out_html <- path(reports_dir, glue("run_report_{stamp}.html"))

# -------- Helpers --------
list_outputs_full <- function(dir, exts) {
  if (!dir_exists(dir)) return(character())
  files <- dir_ls(dir, recurse = TRUE, type = "file")
  files[tolower(path_ext(files)) %in% tolower(exts)]
}

safe_tail <- function(p, n = 200) {
  if (!file_exists(p)) return("")
  x <- readLines(p, warn = FALSE, encoding = "UTF-8")
  if (length(x) == 0) return("")
  paste(tail(x, min(n, length(x))), collapse = "\n")
}

escape_html <- function(s) {
  s <- gsub("&", "&amp;", s, fixed = TRUE)
  s <- gsub("<", "&lt;", s, fixed = TRUE)
  s <- gsub(">", "&gt;", s, fixed = TRUE)
  s
}

rel_from_report <- function(abs_path, report_file) {
  rel <- path_rel(abs_path, start = path_dir(report_file))
  gsub("\\\\", "/", rel)
}

details_block <- function(title, inner_html, open = FALSE) {
  o <- if (isTRUE(open)) " open" else ""
  paste0('<details', o, '><summary>', title, '</summary><div class="details-body">', inner_html, '</div></details>')
}

pretty_num <- function(x) if (is.na(x)) "NA" else format(round(as.numeric(x), 1), nsmall = 1)

# ---- CSV -> HTML table preview ----
df_to_html_table <- function(df, max_cols = 16) {
  if (nrow(df) == 0) return("<p>No hay filas para previsualizar.</p>")
  if (ncol(df) > max_cols) df <- df[, seq_len(max_cols), drop = FALSE]
  
  th <- paste0("<th>", escape_html(names(df)), "</th>", collapse = "")
  head_row <- paste0("<tr>", th, "</tr>")
  
  body_rows <- apply(df, 1, function(row) {
    tds <- paste0("<td>", escape_html(as.character(row)), "</td>", collapse = "")
    paste0("<tr>", tds, "</tr>")
  })
  paste0(
    '<div class="tablewrap"><table class="tbl"><thead>', head_row,
    '</thead><tbody>', paste0(body_rows, collapse = ""), '</tbody></table></div>'
  )
}

preview_csv_block <- function(csv_abs, report_file, n_preview = 12) {
  nm <- path_file(csv_abs)
  rel <- rel_from_report(csv_abs, report_file)
  
  df <- tryCatch(
    suppressWarnings(read_csv(csv_abs, show_col_types = FALSE, n_max = n_preview)),
    error = function(e) NULL
  )
  
  meta <- if (!is.null(df)) {
    glue('<div class="meta2">Preview: {min(n_preview, nrow(df))} filas • {ncol(df)} columnas</div>')
  } else {
    '<div class="meta2 warn">No se pudo leer el CSV (encoding o formato).</div>'
  }
  
  tbl <- if (!is.null(df)) df_to_html_table(df) else ""
  
  inner <- paste0(
    '<div class="rowlinks">',
    glue('<a href="{rel}" target="_blank" rel="noopener">Abrir CSV</a>'),
    '<span class="sep">•</span>',
    glue('<span class="meta2">{escape_html(rel)}</span>'),
    '</div>',
    meta,
    tbl
  )
  
  details_block(escape_html(nm), inner, open = FALSE)
}

csv_accordion <- function(csv_abs_vec, report_file) {
  if (length(csv_abs_vec) == 0) return("<p>No hay CSV.</p>")
  
  # ordenar por nombre
  csv_abs_vec <- csv_abs_vec[order(tolower(path_file(csv_abs_vec)))]
  
  blocks <- vapply(csv_abs_vec, function(p) preview_csv_block(p, report_file), character(1))
  paste0(blocks, collapse = "")
}

ul_links <- function(files_abs, report_file) {
  if (length(files_abs) == 0) return("<p>No hay archivos.</p>")
  items <- vapply(files_abs, function(p) {
    r <- rel_from_report(p, report_file)
    nm <- path_file(p)
    glue('<li><a href="{r}" target="_blank" rel="noopener">{nm}</a></li>')
  }, character(1))
  paste0("<ul>", paste0(items, collapse = ""), "</ul>")
}

gallery_png <- function(png_abs, report_file, max_show = 40) {
  if (length(png_abs) == 0) return("<p>No hay figuras.</p>")
  png_abs <- png_abs[seq_len(min(length(png_abs), max_show))]
  cards <- vapply(png_abs, function(p) {
    r <- rel_from_report(p, report_file)
    nm <- path_file(p)
    glue('
      <div class="imgcard">
        <div class="imgtitle">{escape_html(nm)}</div>
        <a href="{r}" target="_blank" rel="noopener">
          <img src="{r}" alt="{escape_html(nm)}">
        </a>
      </div>
    ')
  }, character(1))
  paste0('<div class="grid">', paste0(cards, collapse = ""), '</div>')
}

# -------- KPIs P8 --------
p8_score <- NA
p8_class <- NA

if (exists("P8_RESULT", inherits = TRUE)) {
  p8 <- get("P8_RESULT", inherits = TRUE)
  if ("score_total" %in% names(p8)) p8_score <- p8$score_total[1]
  if ("classification" %in% names(p8)) p8_class <- p8$classification[1]
} else if (file_exists(path(tables_dir, "P8_final_results.csv"))) {
  p8f <- suppressWarnings(read_csv(path(tables_dir, "P8_final_results.csv"), show_col_types = FALSE))
  cand_score <- intersect(names(p8f), c("score_total","score","p8_score","total_score"))
  cand_class <- intersect(names(p8f), c("classification","class","p8_class","label"))
  if (length(cand_score)) p8_score <- p8f[[cand_score[1]]][1]
  if (length(cand_class)) p8_class <- p8f[[cand_class[1]]][1]
}

# -------- Outputs --------
csv_abs  <- list_outputs_full(tables_dir, c("csv"))
png_abs  <- list_outputs_full(figs_dir, c("png"))
html_abs <- list_outputs_full("outputs", c("html"))

# -------- Log más reciente --------
log_tail <- ""
latest_log <- NA_character_

if (dir_exists(logs_dir)) {
  logs <- dir_ls(logs_dir, glob = "*.log")
  if (length(logs) > 0) {
    info <- file_info(logs) |> arrange(desc(modification_time))
    latest_log <- info$path[1]
    log_tail <- safe_tail(latest_log, 200)
  }
}

# -------- Secciones --------
sec_outputs <- paste0(
  details_block(glue("Tablas (CSV) — {length(csv_abs)}"),
                csv_accordion(csv_abs, out_html),
                open = TRUE),
  details_block(glue("Figuras (PNG) — {length(png_abs)}"),
                gallery_png(png_abs, out_html),
                open = TRUE),
  details_block(glue("Interactivos (HTML) — {length(html_abs)}"),
                ul_links(html_abs, out_html),
                open = FALSE)
)

sec_log <- if (nzchar(log_tail)) {
  paste0(
    '<div class="box"><div class="meta2">Último log: ',
    ifelse(is.na(latest_log), "NA", escape_html(path_file(latest_log))),
    '</div></div>',
    "<pre>", escape_html(log_tail), "</pre>"
  )
} else {
  '<div class="box">No se encontró log.</div>'
}

# -------- HTML --------
html <- paste0(
  '<!DOCTYPE html>
<html lang="es">
<head>
<meta charset="utf-8">
<title>Defense Analytics — Reporte de ejecución</title>
<style>
body{font-family:Arial,Helvetica,sans-serif;max-width:1100px;margin:40px auto;line-height:1.45;color:#111}
h1{font-size:28px;margin-bottom:0}
.meta{color:#555;margin-top:6px}
h2{margin-top:26px;font-size:18px;border-top:1px solid #eee;padding-top:16px}
.box{background:#fafafa;border:1px solid #eee;border-radius:10px;padding:14px 16px}
.kpi{display:flex;gap:14px;flex-wrap:wrap;margin-top:10px}
.kpi .card{border:1px solid #eee;border-radius:12px;padding:12px 14px;min-width:260px;background:#fff}
.kpi .val{font-size:22px;margin-top:6px}
details{border:1px solid #eee;border-radius:12px;background:#fff;margin:10px 0;overflow:hidden}
summary{cursor:pointer;padding:12px 14px;font-weight:700;list-style:none}
summary::-webkit-details-marker{display:none}
.details-body{padding:14px 14px 18px 14px;border-top:1px solid #eee;background:#fafafa}
.grid{display:grid;grid-template-columns:repeat(auto-fill,minmax(260px,1fr));gap:12px}
.imgcard{border:1px solid #eee;border-radius:12px;background:#fff;padding:10px}
.imgtitle{font-size:12px;color:#444;margin-bottom:8px;word-break:break-word}
img{width:100%;height:auto;border-radius:10px;border:1px solid #f0f0f0}
pre{background:#0b0b0b;color:#eee;padding:14px;border-radius:12px;overflow:auto}
.meta2{color:#666;font-size:12px;margin-top:6px}
.warn{color:#b85}
a{color:#0b57d0;text-decoration:none}
a:hover{text-decoration:underline}
.rowlinks{display:flex;align-items:center;gap:10px;flex-wrap:wrap;margin-bottom:6px}
.sep{color:#bbb}
.tablewrap{overflow:auto;border:1px solid #eee;border-radius:10px;background:#fff}
table.tbl{border-collapse:collapse;width:100%;font-size:12px}
.tbl th,.tbl td{border-bottom:1px solid #eee;padding:8px 10px;text-align:left;vertical-align:top;white-space:nowrap}
.tbl th{background:#fafafa;font-weight:700}
</style>
</head>
<body>

<h1>Defense Analytics — Reporte de ejecución</h1>
<div class="meta">Fecha: ', format(Sys.Date(), "%Y-%m-%d"), ' | Hora: ', format(Sys.time(), "%H:%M:%S"), '</div>

<h2>1) Fuente de datos</h2>
<div class="box">
<p>Panel principal: ', escape_html(normalizePath(panel_path, winslash="/", mustWork = FALSE)),
  if (file_exists(panel_path)) ' ✅' else ' ⚠️ (no encontrado)',
  '</p>
</div>

<h2>2) Resultados clave (KPIs)</h2>
<div class="kpi">
  <div class="card"><div>P8 — Score Año Rojo</div><div class="val">', pretty_num(p8_score), '</div></div>
  <div class="card"><div>P8 — Clasificación</div><div class="val">', ifelse(is.na(p8_class), "NA", escape_html(as.character(p8_class))), '</div></div>
</div>

<h2>3) Outputs generados</h2>
', sec_outputs, '

<h2>4) Log (últimas 200 líneas)</h2>
', sec_log, '

</body>
</html>'
)

writeLines(html, out_html)
cat("OK -> ", out_html, "\n", sep = "")

