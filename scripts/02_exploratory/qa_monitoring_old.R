# =============================================================================
# QA FUNCTION — ROUTINE DATAFRAME
# Outputs: (1) Excel workbook — one consolidated "Tous les problèmes" sheet
#          (2) Standalone interactive HTML report
# =============================================================================
# install.packages(c("openxlsx", "dplyr", "tidyr", "purrr"))
# =============================================================================

library(openxlsx)
library(dplyr)
library(tidyr)
library(purrr)

# =============================================================================
# CONFIGURATION — adapt to your protocol
# =============================================================================

VALID_VALUES <- list(
  ptZscore   = c("inf-3", "inf-2", "inf-1", "0", "sup1", "sup2", "sup3"),
  pbOrPc     = c("pb", "pc"),
  oedemes    = 0:3,
  phase      = as.character(1:4),
  nRepJr     = c(8L, 10L, 12L),
  typeAlim   = c("F75", "F75F", "F100", "F100d", "ATPE"),
  aPlats     = c("LM", "non"),
  repas_vals = c("1", "1_2", "3_4", "sonde", "vomit")
)

REPAS_COLS <- paste0("repasH", 1:10)

NUMERIC_BOUNDS <- list(
  taille          = c(30, 130),
  poids           = c(100, 5000),
  pbOrPcV         = c(60, 200),
  oedemes         = c(0, 3),
  nRepJr          = c(6, 16),
  mlRepas         = c(10, 500),
  selles          = c(0, 20),
  vomissCorrige   = c(0, 20),
  bouillieCorrige = c(0, 500)
)

NA_WARN_PCT  <- 20   # yellow
NA_ERROR_PCT <- 50   # red


# =============================================================================
# INTERNAL HELPERS
# =============================================================================

# Write a labelled block into a worksheet; returns next available start_row
.write_block <- function(wb, ws, df, start_row, title, fill_hex) {
  navy  <- "#1A3A5C"
  blue  <- "#2E6DA4"

  hdr_s <- createStyle(fontColour = "#FFFFFF", fgFill = navy,
                       textDecoration = "Bold", fontName = "Calibri",
                       fontSize = 10, border = "TopBottomLeftRight",
                       borderColour = "#0D2137")
  col_s <- createStyle(fontColour = "#FFFFFF", fgFill = blue,
                       textDecoration = "Bold", fontName = "Calibri",
                       fontSize = 10, border = "TopBottomLeftRight",
                       borderColour = navy)
  dat_s <- createStyle(fgFill = fill_hex, fontName = "Calibri",
                       fontSize = 10, border = "TopBottomLeftRight",
                       borderColour = "#CCCCCC")
  ok_s  <- createStyle(fgFill = "#D4EDDA", fontColour = "#1E7F34",
                       fontName = "Calibri", fontSize = 10, textDecoration = "Bold",
                       border = "TopBottomLeftRight", borderColour = "#C3E6CB")

  # Section title row
  writeData(wb, ws, title, startRow = start_row, startCol = 1)
  addStyle(wb, ws, hdr_s, rows = start_row, cols = 1:max(ncol(df), 4),
           gridExpand = TRUE)

  if (nrow(df) == 0) {
    writeData(wb, ws, "  ✓  Aucun problème détecté",
              startRow = start_row + 1, startCol = 1)
    addStyle(wb, ws, ok_s, rows = start_row + 1,
             cols = 1:max(ncol(df), 4), gridExpand = TRUE)
    return(start_row + 4)
  }

  # Column headers
  writeData(wb, ws, df, startRow = start_row + 1, startCol = 1, colNames = TRUE)
  addStyle(wb, ws, col_s, rows = start_row + 1, cols = 1:ncol(df))
  # Data rows
  addStyle(wb, ws, dat_s,
           rows = (start_row + 2):(start_row + 1 + nrow(df)),
           cols = 1:ncol(df), gridExpand = TRUE)

  return(start_row + nrow(df) + 4)
}


# =============================================================================
# MAIN FUNCTION
# =============================================================================

run_qa <- function(df,
                   excel_path = "QA_rapport_routine.xlsx",
                   html_path  = "QA_rapport_routine.html") {

  n_rows   <- nrow(df)
  run_date <- format(Sys.Date(), "%d/%m/%Y")

  # ── 1. MISSING VALUES ──────────────────────────────────────────────────────
  na_sum <- df %>%
    summarise(across(everything(), ~ sum(is.na(.)))) %>%
    pivot_longer(everything(), names_to = "Variable", values_to = "NA_n") %>%
    mutate(
      Pct    = round(NA_n / n_rows * 100, 1),
      Statut = case_when(
        Pct == 0           ~ "OK",
        Pct < NA_WARN_PCT  ~ "Faible",
        Pct < NA_ERROR_PCT ~ "Modéré",
        TRUE               ~ "Élevé"
      )
    ) %>%
    arrange(desc(NA_n))

  na_issues <- filter(na_sum, Statut %in% c("Modéré", "Élevé"))

  # ── 2. DUPLICATES ──────────────────────────────────────────────────────────
  dups <- df %>%
    mutate(.row = row_number()) %>%
    group_by(id, dateVis) %>%
    filter(n() > 1) %>%
    ungroup() %>%
    arrange(id, dateVis) %>%
    select(.row, index, id, dateVis)

  # ── 3. NUMERIC OUTLIERS ────────────────────────────────────────────────────
  outliers <- map_dfr(names(NUMERIC_BOUNDS), function(var) {
    if (!var %in% names(df)) return(NULL)
    lo <- NUMERIC_BOUNDS[[var]][1]; hi <- NUMERIC_BOUNDS[[var]][2]
    df %>%
      mutate(.row = row_number()) %>%
      filter(!is.na(.data[[var]]),
             .data[[var]] < lo | .data[[var]] > hi) %>%
      transmute(.row, index, id, dateVis,
                Variable = var,
                Valeur   = .data[[var]],
                Min_attendu = lo, Max_attendu = hi,
                Problème = ifelse(.data[[var]] < lo, "< minimum", "> maximum"))
  })

  # ── 4. CATEGORICAL VALIDATION ──────────────────────────────────────────────
  cat_issues <- map_dfr(
    c("ptZscore", "pbOrPc", "phase", "typeAlim", "aPlats", REPAS_COLS),
    function(var) {
      if (!var %in% names(df)) return(NULL)
      key     <- if (var %in% REPAS_COLS) "repas_vals" else var
      allowed <- VALID_VALUES[[key]]
      df %>%
        mutate(.row = row_number()) %>%
        filter(!is.na(.data[[var]]), !(.data[[var]] %in% allowed)) %>%
        transmute(.row, index, id, dateVis,
                  Variable = var,
                  Valeur   = as.character(.data[[var]]),
                  Attendu  = paste(allowed, collapse = " | "))
    })

  # ── 5. LOGIC CHECKS ────────────────────────────────────────────────────────
  logic_checks <- df %>%
    mutate(.row = row_number(),
           l1 = !is.na(pbOrPc) & is.na(pbOrPcV),
           l2 = phase %in% c("2","3") & typeAlim %in% c("F75","F75F") &
                !is.na(phase) & !is.na(typeAlim),
           l3 = !is.na(nRepJr) & !(nRepJr %in% VALID_VALUES[["nRepJr"]]),
           l4 = !is.na(mlRepas) & mlRepas == 0) %>%
    filter(l1 | l2 | l3 | l4) %>%
    mutate(Règle = case_when(
      l1 ~ "pbOrPc renseigné sans pbOrPcV",
      l2 ~ "Phase 2/3 avec alimentation F75/F75F",
      l3 ~ "nRepJr hors valeurs autorisées (8/10/12)",
      l4 ~ "mlRepas = 0",
      TRUE ~ "Autre")) %>%
    select(.row, index, id, dateVis,
           pbOrPc, pbOrPcV, phase, typeAlim, nRepJr, mlRepas, Règle)

  # ── TOTALS ─────────────────────────────────────────────────────────────────
  totals <- c(na    = nrow(na_issues),
              dups  = nrow(dups),
              out   = nrow(outliers),
              cat   = nrow(cat_issues),
              logic = nrow(logic_checks))
  grand_total <- sum(totals)


  # ===========================================================================
  # EXCEL EXPORT
  # ===========================================================================
  wb <- createWorkbook()

  # ── Sheet A: KPI Summary ───────────────────────────────────────────────────
  addWorksheet(wb, "Résumé QA")
  setColWidths(wb, "Résumé QA", cols = 1:3, widths = c(38, 16, 14))

  title_s <- createStyle(fontSize = 15, textDecoration = "Bold",
                         fontName = "Calibri", fontColour = "#1A3A5C")
  writeData(wb, "Résumé QA",
            paste0("RAPPORT QA — Routine  |  ", run_date,
                   "  |  ", n_rows, " lignes"),
            startRow = 1, startCol = 1)
  addStyle(wb, "Résumé QA", title_s, rows = 1, cols = 1)

  kpi <- tibble(
    Contrôle = c("Valeurs manquantes (variables >20%)", "Doublons (id × dateVis)",
                 "Valeurs numériques hors bornes", "Catégories invalides",
                 "Incohérences logiques", "TOTAL"),
    `Nb problèmes` = c(totals[["na"]], totals[["dups"]], totals[["out"]],
                       totals[["cat"]], totals[["logic"]], grand_total),
    Statut = c(
      ifelse(totals[["na"]]    == 0, "✓ OK", "⚠ Attention"),
      ifelse(totals[["dups"]]  == 0, "✓ OK", "✗ Erreur"),
      ifelse(totals[["out"]]   == 0, "✓ OK", "⚠ Attention"),
      ifelse(totals[["cat"]]   == 0, "✓ OK", "✗ Erreur"),
      ifelse(totals[["logic"]] == 0, "✓ OK", "⚠ Attention"),
      ifelse(grand_total       == 0, "✓ TOUT OK", "✗ PROBLÈMES DÉTECTÉS")
    )
  )

  hdr_s <- createStyle(fontColour = "#FFFFFF", fgFill = "#1A3A5C",
                       textDecoration = "Bold", fontName = "Calibri",
                       fontSize = 10, halign = "CENTER")
  writeData(wb, "Résumé QA", kpi, startRow = 3, colNames = TRUE)
  addStyle(wb, "Résumé QA", hdr_s, rows = 3, cols = 1:3)

  for (i in seq_len(nrow(kpi))) {
    fill <- if (grepl("✓", kpi$Statut[i])) "#D4EDDA"
            else if (grepl("⚠", kpi$Statut[i])) "#FFF3CD"
            else "#F8D7DA"
    s <- createStyle(fgFill = fill, fontName = "Calibri",
                     textDecoration = if (i == nrow(kpi)) "Bold" else "normal",
                     border = "TopBottomLeftRight", borderColour = "#CCCCCC")
    addStyle(wb, "Résumé QA", s, rows = 3 + i, cols = 1:3, gridExpand = TRUE)
  }

  # ── Sheet B: ALL ISSUES CONSOLIDATED ──────────────────────────────────────
  addWorksheet(wb, "Tous les problèmes")
  setColWidths(wb, "Tous les problèmes", cols = 1:12,
               widths = c(7, 10, 10, 14, rep(14, 8)))

  writeData(wb, "Tous les problèmes",
            paste0("SYNTHÈSE DE TOUS LES PROBLÈMES — ", run_date,
                   "  |  ", n_rows, " lignes analysées"),
            startRow = 1, startCol = 1)
  addStyle(wb, "Tous les problèmes",
           createStyle(fontSize = 13, textDecoration = "Bold",
                       fontName = "Calibri", fontColour = "#1A3A5C"),
           rows = 1, cols = 1)

  cur <- 3
  cur <- .write_block(wb, "Tous les problèmes",
                      rename(na_issues, `NA (n)` = NA_n, `NA (%)` = Pct),
                      cur, paste0("► VALEURS MANQUANTES  (", nrow(na_issues), " variables >20%)"),
                      "#FFF3CD")
  cur <- .write_block(wb, "Tous les problèmes", dups, cur,
                      paste0("► DOUBLONS  (", nrow(dups), " lignes)"), "#F8D7DA")
  cur <- .write_block(wb, "Tous les problèmes", outliers, cur,
                      paste0("► VALEURS HORS BORNES  (", nrow(outliers), " lignes)"), "#FFF3CD")
  cur <- .write_block(wb, "Tous les problèmes", cat_issues, cur,
                      paste0("► CATÉGORIES INVALIDES  (", nrow(cat_issues), " lignes)"), "#F8D7DA")
  .write_block(wb, "Tous les problèmes", logic_checks, cur,
               paste0("► INCOHÉRENCES LOGIQUES  (", nrow(logic_checks), " lignes)"), "#FFF3CD")

  # ── Sheet C: Full NA table ─────────────────────────────────────────────────
  addWorksheet(wb, "NA par variable")
  setColWidths(wb, "NA par variable", cols = 1:4, widths = c(24, 12, 10, 12))
  writeDataTable(wb, "NA par variable", na_sum, startRow = 1,
                 tableStyle = "TableStyleMedium9", withFilter = TRUE)

  saveWorkbook(wb, file = excel_path, overwrite = TRUE)
  message("Excel exporté : ", excel_path)


  # ===========================================================================
  # HTML REPORT
  # ===========================================================================

  html_escape <- function(x) {
    x <- gsub("&", "&amp;", x); x <- gsub("<", "&lt;", x); x <- gsub(">", "&gt;", x); x
  }

  df_to_html_tbl <- function(df) {
    if (nrow(df) == 0)
      return('<p class="ok-msg">&#10003; Aucun problème détecté</p>')
    hdrs <- paste0("<th>", names(df), "</th>", collapse = "")
    rows <- paste(apply(df, 1, function(r) {
      cells <- paste0("<td>", html_escape(as.character(r)), "</td>", collapse = "")
      paste0("<tr>", cells, "</tr>")
    }), collapse = "\n")
    paste0('<div class="tbl-wrap"><table><thead><tr>', hdrs,
           '</tr></thead><tbody>', rows, '</tbody></table></div>')
  }

  bar_items <- {
    top_na <- na_sum %>% filter(Pct > 0) %>% arrange(desc(Pct)) %>% head(20)
    paste(sapply(seq_len(nrow(top_na)), function(i) {
      v <- top_na$Variable[i]; pct <- top_na$Pct[i]
      cls <- if (pct >= NA_ERROR_PCT) "bar-hi" else if (pct >= NA_WARN_PCT) "bar-md" else "bar-lo"
      paste0('<div class="bar-row"><span class="bar-lbl">', v, '</span>',
             '<div class="bar-track"><div class="bar-fill ', cls,
             '" style="width:', pct, '%"></div></div>',
             '<span class="bar-pct">', pct, '%</span></div>')
    }), collapse = "\n")
  }

  scard <- function(n, lbl, cls)
    paste0('<div class="scard ', cls, '"><div class="snum">', n,
           '</div><div class="slbl">', lbl, '</div></div>')

  sec <- function(id, dot_cls, badge_cls, title, badge_txt, body)
    paste0('<div class="sec" id="', id, '">',
           '<div class="sec-hdr" onclick="tog(this)">',
           '<div class="sec-ttl"><span class="dot ', dot_cls, '"></span>', title, '</div>',
           '<div class="sec-right"><span class="badge ', badge_cls, '">', badge_txt, '</span>',
           '<span class="chev">&#9660;</span></div></div>',
           '<div class="sec-body">', body, '</div></div>')

  html_doc <- paste0(
'<!DOCTYPE html>
<html lang="fr">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<title>Rapport QA Routine</title>
<link href="https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:wght@400;500&family=IBM+Plex+Sans:wght@300;400;600;700&display=swap" rel="stylesheet">
<style>
*,*::before,*::after{box-sizing:border-box;margin:0;padding:0}
:root{
  --bg:#EEF1F6;--surf:#FFF;--navy:#1A3A5C;--blue:#2C6FAC;
  --ok:#1A7F4B;--ok-bg:#D6F5E6;--warn:#8A6000;--warn-bg:#FFF3CD;
  --err:#9B1C2A;--err-bg:#FDDDE0;--txt:#1C2333;--mut:#6B7A99;
  --bdr:#DDE2EC;--mono:"IBM Plex Mono",monospace;--sans:"IBM Plex Sans",sans-serif;
}
body{font-family:var(--sans);background:var(--bg);color:var(--txt);font-size:14px;line-height:1.6}

/* header */
.hdr{background:var(--navy);padding:28px 48px;display:flex;align-items:center;justify-content:space-between;gap:16px}
.hdr h1{color:#fff;font-size:20px;font-weight:700;letter-spacing:-.2px}
.hdr p{color:#8AAECB;font-size:12px;margin-top:3px;font-family:var(--mono)}
.hdr-badge{background:', ifelse(grand_total==0,'#1A7F4B','#9B1C2A'), ';color:#fff;
  font-family:var(--mono);font-size:13px;padding:8px 20px;border-radius:4px;font-weight:500;white-space:nowrap}

/* layout */
.wrap{max-width:1080px;margin:0 auto;padding:28px 20px 60px}

/* stat cards */
.cards{display:grid;grid-template-columns:repeat(5,1fr);gap:10px;margin-bottom:28px}
.scard{background:var(--surf);border-radius:8px;padding:16px;border-top:3px solid var(--bdr);text-align:center}
.scard.ok{border-top-color:var(--ok)}.scard.warn{border-top-color:#E8A020}.scard.err{border-top-color:var(--err)}
.snum{font-size:28px;font-weight:700;font-family:var(--mono);color:var(--navy);line-height:1}
.slbl{font-size:11px;color:var(--mut);text-transform:uppercase;letter-spacing:.5px;margin-top:4px}

/* sections */
.sec{background:var(--surf);border-radius:8px;margin-bottom:16px;border:1px solid var(--bdr);overflow:hidden}
.sec-hdr{display:flex;align-items:center;justify-content:space-between;padding:14px 20px;
  cursor:pointer;user-select:none;border-bottom:1px solid var(--bdr)}
.sec-hdr:hover{background:#F7F9FC}
.sec-ttl{display:flex;align-items:center;gap:9px;font-weight:600;font-size:14px;color:var(--navy)}
.dot{width:8px;height:8px;border-radius:50%;flex-shrink:0}
.dot-ok{background:var(--ok)}.dot-warn{background:#E8A020}.dot-err{background:var(--err)}
.sec-right{display:flex;align-items:center;gap:10px}
.badge{font-family:var(--mono);font-size:11px;padding:2px 10px;border-radius:20px;font-weight:500}
.badge-ok{background:var(--ok-bg);color:var(--ok)}
.badge-warn{background:var(--warn-bg);color:var(--warn)}
.badge-err{background:var(--err-bg);color:var(--err)}
.chev{font-size:14px;color:var(--mut);transition:transform .2s}
.sec-body{padding:18px 20px;display:none}
.sec-body.open{display:block}

/* tables */
.tbl-wrap{overflow-x:auto;border-radius:6px;border:1px solid var(--bdr)}
table{border-collapse:collapse;width:100%;font-size:12px}
thead tr{background:var(--navy)}
thead th{padding:8px 12px;text-align:left;font-weight:600;font-size:11px;
  letter-spacing:.4px;white-space:nowrap;font-family:var(--mono);color:#fff}
tbody tr{border-bottom:1px solid var(--bdr)}
tbody tr:last-child{border-bottom:none}
tbody tr:hover{background:#F7F9FC}
tbody td{padding:7px 12px;font-family:var(--mono);color:var(--txt);white-space:nowrap}
.ok-msg{color:var(--ok);font-weight:600;padding:10px 2px;font-size:13px}

/* bar chart */
.bar-row{display:flex;align-items:center;gap:9px;margin-bottom:6px}
.bar-lbl{font-family:var(--mono);font-size:11px;width:140px;flex-shrink:0}
.bar-track{flex:1;height:13px;background:#EEF1F6;border-radius:3px;overflow:hidden}
.bar-fill{height:100%;border-radius:3px}
.bar-lo{background:var(--ok)}.bar-md{background:#E8A020}.bar-hi{background:var(--err)}
.bar-pct{font-family:var(--mono);font-size:11px;width:44px;text-align:right;color:var(--mut)}
.legend{display:flex;gap:18px;font-size:11px;color:var(--mut);margin-bottom:14px;flex-wrap:wrap}
.leg-dot{width:9px;height:9px;border-radius:50%;display:inline-block;margin-right:4px;vertical-align:middle}

.footer{text-align:center;color:var(--mut);font-size:11px;margin-top:36px;font-family:var(--mono)}
</style>
</head>
<body>
<div class="hdr">
  <div>
    <h1>Rapport de Contrôle Qualité &mdash; Données Routine</h1>
    <p>Généré le ', run_date, ' &nbsp;&middot;&nbsp; ', n_rows, ' lignes analysées</p>
  </div>
  <div class="hdr-badge">', grand_total, ' problème(s) détecté(s)</div>
</div>
<div class="wrap">
  <div class="cards">',
  scard(totals[["na"]],    "Var. manquantes<br>&gt;20%", ifelse(totals[["na"]]   ==0,"ok","warn")),
  scard(totals[["dups"]],  "Doublons",                   ifelse(totals[["dups"]] ==0,"ok","err")),
  scard(totals[["out"]],   "Hors bornes",                ifelse(totals[["out"]]  ==0,"ok","warn")),
  scard(totals[["cat"]],   "Catégories<br>invalides",    ifelse(totals[["cat"]]  ==0,"ok","err")),
  scard(totals[["logic"]], "Incohérences<br>logiques",   ifelse(totals[["logic"]]==0,"ok","warn")),
'</div>

',
  # Section 1: Missing
  sec("s-na",
      ifelse(totals[["na"]]==0,"dot-ok","dot-warn"),
      ifelse(totals[["na"]]==0,"badge-ok","badge-warn"),
      "Valeurs manquantes",
      paste0(nrow(na_issues), " variable(s) &gt;20%"),
      paste0('<div class="legend">',
             '<span><span class="leg-dot" style="background:var(--ok)"></span>&lt;20% OK</span>',
             '<span><span class="leg-dot" style="background:#E8A020"></span>20-50% attention</span>',
             '<span><span class="leg-dot" style="background:var(--err)"></span>&gt;50% élevé</span>',
             '</div>', bar_items)),

  # Section 2: Duplicates
  sec("s-dup",
      ifelse(nrow(dups)==0,"dot-ok","dot-err"),
      ifelse(nrow(dups)==0,"badge-ok","badge-err"),
      "Doublons (id &times; dateVis)",
      paste0(nrow(dups), " ligne(s)"),
      df_to_html_tbl(dups)),

  # Section 3: Outliers
  sec("s-out",
      ifelse(nrow(outliers)==0,"dot-ok","dot-warn"),
      ifelse(nrow(outliers)==0,"badge-ok","badge-warn"),
      "Valeurs numériques hors bornes",
      paste0(nrow(outliers), " ligne(s)"),
      df_to_html_tbl(outliers)),

  # Section 4: Categories
  sec("s-cat",
      ifelse(nrow(cat_issues)==0,"dot-ok","dot-err"),
      ifelse(nrow(cat_issues)==0,"badge-ok","badge-err"),
      "Catégories invalides",
      paste0(nrow(cat_issues), " ligne(s)"),
      df_to_html_tbl(cat_issues)),

  # Section 5: Logic
  sec("s-log",
      ifelse(nrow(logic_checks)==0,"dot-ok","dot-warn"),
      ifelse(nrow(logic_checks)==0,"badge-ok","badge-warn"),
      "Incohérences logiques",
      paste0(nrow(logic_checks), " ligne(s)"),
      df_to_html_tbl(logic_checks)),

'  <div class="footer">QA Routine &nbsp;&middot;&nbsp; ', run_date,
' &nbsp;&middot;&nbsp; ', n_rows, ' lignes &nbsp;&middot;&nbsp; Généré avec R</div>
</div>
<script>
function tog(hdr){
  var b=hdr.nextElementSibling, c=hdr.querySelector(".chev");
  b.classList.toggle("open");
  c.style.transform=b.classList.contains("open")?"rotate(180deg)":"";
}
</script>
</body>
</html>')

  writeLines(html_doc, html_path)
  message("HTML exporté : ", html_path)

  invisible(list(
    na_summary         = na_sum,
    duplicates         = dups,
    outliers           = outliers,
    invalid_categories = cat_issues,
    logic_issues       = logic_checks,
    total_issues       = grand_total
  ))
}


# =============================================================================
# USAGE
# =============================================================================
#
#   source("qa_routine.R")
#
#   qa <- run_qa(
#     df         = routine,
#     excel_path = "QA_rapport_routine.xlsx",
#     html_path  = "QA_rapport_routine.html"
#   )
#
#   # Access results programmatically:
#   qa$na_summary
#   qa$duplicates
#   qa$outliers
#   qa$invalid_categories
#   qa$logic_issues
#
