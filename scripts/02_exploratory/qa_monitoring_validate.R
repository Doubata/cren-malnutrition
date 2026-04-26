# =============================================================================
# QA FUNCTION — ROUTINE DATAFRAME
# Uses {validate} for validation, {writexl} + {rmarkdown} for export
# install.packages(c("validate", "writexl", "dplyr", "tidyr"))
# =============================================================================

library(validate)
library(writexl)
library(dplyr)
library(tidyr)

# =============================================================================
# CONFIGURATION — adapt to your protocol
# =============================================================================

REPAS_COLS <- paste0("repasH", 1:10)

VALID_PTZS   <- c("inf-3", "inf-2", "inf-1", "0", "sup1", "sup2", "sup3")
VALID_PHASE  <- c("1", "2", "3", "4")
VALID_ALIM   <- c("F75", "F75F", "F100", "F100d", "ATPE")
VALID_APLATS <- c("LM", "non")
VALID_REPAS  <- c("1", "1_2", "3_4", "sonde", "vomit")

NA_WARN_PCT  <- 20
NA_ERROR_PCT <- 50

# =============================================================================
# VALIDATION RULES
# =============================================================================

make_rules <- function() {
  validator(

    # ── 1. Missing values: key columns ───────────────────────────────────────
    id_not_missing       = !is.na(id),
    dateVis_not_missing  = !is.na(dateVis),
    phase_not_missing    = !is.na(phase),
    oedemes_not_missing  = !is.na(oedemes),
    poids_not_missing    = !is.na(poids),
    taille_not_missing   = !is.na(taille),

    # ── 2. Duplicates ─────────────────────────────────────────────────────────
    no_duplicate_visit   = !duplicated(paste(id, dateVis)),

    # ── 3. Date format ────────────────────────────────────────────────────────
    date_format          = grepl("^\\d{2}/\\d{2}/\\d{4}$", dateVis),

    # ── 4. Numeric ranges ─────────────────────────────────────────────────────
    taille_range         = is.na(taille)   | taille   %in% 30:130,
    poids_range          = is.na(poids)    | poids    %in% 100:5000,
    pbOrPcV_range        = is.na(pbOrPcV)  | pbOrPcV  %in% 60:200,
    oedemes_range        = is.na(oedemes)  | oedemes  %in% 0:3,
    nRepJr_range         = is.na(nRepJr)   | nRepJr   %in% c(8, 10, 12),
    mlRepas_range        = is.na(mlRepas)  | (mlRepas >= 1 & mlRepas <= 500),
    selles_range         = is.na(selles)   | selles   %in% 0:20,
    vomissCorrige_range  = is.na(vomissCorrige)    | vomissCorrige    %in% 0:20,
    bouillieCorrige_range= is.na(bouillieCorrige)  | bouillieCorrige  %in% 0:500,

    # ── 5. Categorical values ─────────────────────────────────────────────────
    ptZscore_valid       = is.na(ptZscore) | ptZscore %in% c("inf-3","inf-2","inf-1","0","sup1","sup2","sup3"),
    pbOrPc_valid         = is.na(pbOrPc)   | pbOrPc   %in% c("pb", "pc"),
    phase_valid          = is.na(phase)    | phase     %in% c("1","2","3","4"),
    typeAlim_valid       = is.na(typeAlim) | typeAlim  %in% c("F75","F75F","F100","F100d","ATPE"),
    aPlats_valid         = is.na(aPlats)   | aPlats    %in% c("LM","non"),
    repasH1_valid        = is.na(repasH1)  | repasH1   %in% c("1","1_2","3_4","sonde","vomit"),
    repasH2_valid        = is.na(repasH2)  | repasH2   %in% c("1","1_2","3_4","sonde","vomit"),
    repasH3_valid        = is.na(repasH3)  | repasH3   %in% c("1","1_2","3_4","sonde","vomit"),
    repasH4_valid        = is.na(repasH4)  | repasH4   %in% c("1","1_2","3_4","sonde","vomit"),
    repasH5_valid        = is.na(repasH5)  | repasH5   %in% c("1","1_2","3_4","sonde","vomit"),
    repasH6_valid        = is.na(repasH6)  | repasH6   %in% c("1","1_2","3_4","sonde","vomit"),
    repasH7_valid        = is.na(repasH7)  | repasH7   %in% c("1","1_2","3_4","sonde","vomit"),
    repasH8_valid        = is.na(repasH8)  | repasH8   %in% c("1","1_2","3_4","sonde","vomit"),
    repasH9_valid        = is.na(repasH9)  | repasH9   %in% c("1","1_2","3_4","sonde","vomit"),
    repasH10_valid       = is.na(repasH10) | repasH10  %in% c("1","1_2","3_4","sonde","vomit"),

    # ── 6. Logic checks ───────────────────────────────────────────────────────
    pbOrPc_implies_pbOrPcV  = is.na(pbOrPc) | !is.na(pbOrPcV),
    phase23_not_F75         = is.na(phase)  | is.na(typeAlim) |
                              !(phase %in% c("2","3") & typeAlim %in% c("F75","F75F")),
    mlRepas_not_zero        = is.na(mlRepas) | mlRepas > 0
  )
}

# =============================================================================
# MAIN FUNCTION
# =============================================================================

run_qa <- function(df,
                   excel_path = "QA_report_routine.xlsx",
                   html_path  = "QA_report_routine.html") {

  n_rows   <- nrow(df)
  run_date <- format(Sys.Date(), "%Y-%m-%d")

  rules  <- make_rules()
  result <- confront(df, rules)

  # ── Summary table ──────────────────────────────────────────────────────────
  sum_df <- summary(result) |>
    mutate(
      pct_fail = round(fails / (passes + fails + nNA) * 100, 2),
      category = case_when(
        grepl("_not_missing$",   name) ~ "Missing values",
        name == "no_duplicate_visit"   ~ "Duplicates",
        name == "date_format"          ~ "Date format",
        grepl("_range$",         name) ~ "Numeric range",
        grepl("_valid$",         name) ~ "Categorical values",
        TRUE                           ~ "Logic check"
      ),
      status = case_when(
        fails == 0 ~ "PASS",
        pct_fail < 5  ~ "WARN",
        TRUE          ~ "FAIL"
      )
    ) |>
    select(rule = name, category, items, passes, fails, nNA,
           pct_fail, status, expression) |>
    arrange(category, desc(fails))

  # ── Flagged rows: one row per rule violation ───────────────────────────────
  flagged <- violating(df, result) |>
    mutate(.row = row_number()) |>
    select(.row, index, id, dateVis, everything())

  # ── Missing values overview ────────────────────────────────────────────────
  na_overview <- df |>
    summarise(across(everything(), ~ sum(is.na(.)))) |>
    pivot_longer(everything(), names_to = "variable", values_to = "n_missing") |>
    mutate(
      pct_missing = round(n_missing / n_rows * 100, 1),
      status      = case_when(
        pct_missing == 0             ~ "OK",
        pct_missing < NA_WARN_PCT    ~ "Low",
        pct_missing < NA_ERROR_PCT   ~ "Moderate",
        TRUE                         ~ "High"
      )
    ) |>
    arrange(desc(n_missing))

  # ── Excel export ───────────────────────────────────────────────────────────
  write_xlsx(
    list(
      Summary      = sum_df,
      Flagged_Rows = as.data.frame(flagged),
      NA_Overview  = na_overview
    ),
    path = excel_path
  )
  message("Excel report saved: ", excel_path)

  # ── HTML report ────────────────────────────────────────────────────────────
  n_fail  <- sum(sum_df$fails > 0)
  n_pass  <- sum(sum_df$fails == 0)
  n_rules <- nrow(sum_df)

  # Build summary rows HTML
  rows_html <- sum_df |>
    mutate(
      row_class = case_when(
        status == "PASS" ~ "pass",
        status == "WARN" ~ "warn",
        TRUE             ~ "fail"
      ),
      badge = case_when(
        status == "PASS" ~ '<span class="badge badge-pass">PASS</span>',
        status == "WARN" ~ '<span class="badge badge-warn">WARN</span>',
        TRUE             ~ '<span class="badge badge-fail">FAIL</span>'
      )
    ) |>
    rowwise() |>
    mutate(html = paste0(
      '<tr class="', row_class, '">',
      "<td>", category,   "</td>",
      "<td>", rule,       "</td>",
      "<td>", items,      "</td>",
      "<td>", passes,     "</td>",
      "<td>", fails,      "</td>",
      "<td>", pct_fail,   "%</td>",
      "<td>", badge,      "</td>",
      "</tr>"
    )) |>
    pull(html) |>
    paste(collapse = "\n")

  # NA bar chart rows
  na_bars <- na_overview |>
    filter(pct_missing > 0) |>
    head(20) |>
    rowwise() |>
    mutate(html = paste0(
      '<div class="bar-row">',
      '<span class="bar-lbl">', variable, '</span>',
      '<div class="bar-track">',
      '<div class="bar-fill bar-',
      ifelse(pct_missing >= NA_ERROR_PCT, "hi",
             ifelse(pct_missing >= NA_WARN_PCT, "md", "lo")),
      '" style="width:', pct_missing, '%"></div></div>',
      '<span class="bar-pct">', pct_missing, '%</span>',
      '</div>'
    )) |>
    pull(html) |>
    paste(collapse = "\n")

  html_doc <- paste0('<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<title>QA Report — Routine</title>
<link href="https://fonts.googleapis.com/css2?family=JetBrains+Mono:wght@400;500&family=Syne:wght@400;600;700&display=swap" rel="stylesheet">
<style>
*,*::before,*::after{box-sizing:border-box;margin:0;padding:0}
:root{
  --bg:#F2F4F8;--surf:#FFF;--navy:#162032;--blue:#1D5FA6;
  --ok:#17784A;--ok-bg:#DCF5EA;--warn:#7A5500;--warn-bg:#FEF3C7;
  --err:#8B1A28;--err-bg:#FDE8EB;--txt:#1C2333;--mut:#64748B;
  --bdr:#E2E8F0;--mono:"JetBrains Mono",monospace;--sans:"Syne",sans-serif;
}
body{font-family:var(--sans);background:var(--bg);color:var(--txt);font-size:14px}
.hdr{background:var(--navy);padding:28px 48px;display:flex;align-items:center;justify-content:space-between}
.hdr h1{color:#fff;font-size:20px;font-weight:700}
.hdr p{color:#7A94B0;font-size:12px;margin-top:4px;font-family:var(--mono)}
.hdr-badge{background:', ifelse(n_fail == 0, '#17784A', '#8B1A28'), ';color:#fff;
  font-family:var(--mono);font-size:13px;padding:8px 20px;border-radius:4px;font-weight:500}
.wrap{max-width:1100px;margin:0 auto;padding:28px 20px 60px}
.cards{display:grid;grid-template-columns:repeat(4,1fr);gap:12px;margin-bottom:28px}
.card{background:var(--surf);border-radius:8px;padding:18px;border-top:3px solid var(--bdr);text-align:center}
.card.ok{border-top-color:var(--ok)}.card.err{border-top-color:var(--err)}
.cnum{font-size:30px;font-weight:700;font-family:var(--mono);color:var(--navy)}
.clbl{font-size:11px;color:var(--mut);text-transform:uppercase;letter-spacing:.5px;margin-top:4px}
.panel{background:var(--surf);border-radius:8px;margin-bottom:20px;border:1px solid var(--bdr);overflow:hidden}
.panel-hdr{padding:14px 20px;background:var(--navy);color:#fff;font-weight:600;font-size:14px}
.panel-body{padding:20px}
table{border-collapse:collapse;width:100%;font-size:12px}
thead tr{background:#F8FAFC}
thead th{padding:9px 12px;text-align:left;font-weight:600;font-size:11px;
  letter-spacing:.4px;border-bottom:2px solid var(--bdr);font-family:var(--mono);color:var(--mut);white-space:nowrap}
tbody tr{border-bottom:1px solid var(--bdr)}
tbody tr:last-child{border-bottom:none}
tbody tr:hover{background:#F8FAFC}
tbody td{padding:8px 12px;font-family:var(--mono);font-size:12px}
tbody tr.pass td:first-child{border-left:3px solid var(--ok)}
tbody tr.warn td:first-child{border-left:3px solid #F59E0B}
tbody tr.fail td:first-child{border-left:3px solid var(--err)}
.badge{font-family:var(--mono);font-size:11px;padding:2px 9px;border-radius:20px;font-weight:500}
.badge-pass{background:var(--ok-bg);color:var(--ok)}
.badge-warn{background:var(--warn-bg);color:var(--warn)}
.badge-fail{background:var(--err-bg);color:var(--err)}
.bar-row{display:flex;align-items:center;gap:10px;margin-bottom:7px}
.bar-lbl{font-family:var(--mono);font-size:11px;width:160px;flex-shrink:0}
.bar-track{flex:1;height:13px;background:#EEF1F6;border-radius:3px;overflow:hidden}
.bar-fill{height:100%;border-radius:3px}
.bar-lo{background:var(--ok)}.bar-md{background:#F59E0B}.bar-hi{background:var(--err)}
.bar-pct{font-family:var(--mono);font-size:11px;width:44px;text-align:right;color:var(--mut)}
.legend{display:flex;gap:16px;font-size:11px;color:var(--mut);margin-bottom:14px}
.ldot{width:9px;height:9px;border-radius:50%;display:inline-block;margin-right:4px;vertical-align:middle}
.footer{text-align:center;color:var(--mut);font-size:11px;margin-top:36px;font-family:var(--mono)}
</style>
</head>
<body>
<div class="hdr">
  <div>
    <h1>QA Report &mdash; Routine Data</h1>
    <p>Generated ', run_date, ' &nbsp;&middot;&nbsp; ', n_rows, ' rows &nbsp;&middot;&nbsp; ', n_rules, ' rules</p>
  </div>
  <div class="hdr-badge">', n_fail, ' rule(s) failing</div>
</div>
<div class="wrap">
  <div class="cards">
    <div class="card ', ifelse(n_pass==n_rules,"ok","err"), '">
      <div class="cnum">', n_pass, '</div><div class="clbl">Rules passing</div></div>
    <div class="card ', ifelse(n_fail==0,"ok","err"), '">
      <div class="cnum">', n_fail, '</div><div class="clbl">Rules failing</div></div>
    <div class="card">
      <div class="cnum">', nrow(flagged), '</div><div class="clbl">Flagged rows</div></div>
    <div class="card">
      <div class="cnum">', sum(na_overview$n_missing > 0), '</div><div class="clbl">Columns with NAs</div></div>
  </div>

  <div class="panel">
    <div class="panel-hdr">Validation Summary</div>
    <div class="panel-body">
      <table>
        <thead><tr>
          <th>Category</th><th>Rule</th><th>Items</th>
          <th>Pass</th><th>Fail</th><th>% Fail</th><th>Status</th>
        </tr></thead>
        <tbody>', rows_html, '</tbody>
      </table>
    </div>
  </div>

  <div class="panel">
    <div class="panel-hdr">Missing Values (top 20 columns)</div>
    <div class="panel-body">
      <div class="legend">
        <span><span class="ldot" style="background:var(--ok)"></span>&lt;', NA_WARN_PCT, '% OK</span>
        <span><span class="ldot" style="background:#F59E0B"></span>', NA_WARN_PCT, '–', NA_ERROR_PCT, '% Moderate</span>
        <span><span class="ldot" style="background:var(--err)"></span>&gt;', NA_ERROR_PCT, '% High</span>
      </div>',
      na_bars,
    '</div>
  </div>

  <div class="footer">
    validate ', as.character(packageVersion("validate")),
    ' &nbsp;&middot;&nbsp; QA Routine &nbsp;&middot;&nbsp; ', run_date,
  '</div>
</div>
</body>
</html>')

  writeLines(html_doc, html_path)
  message("HTML report saved: ", html_path)

  invisible(list(
    result      = result,
    summary     = sum_df,
    flagged     = flagged,
    na_overview = na_overview
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
#     excel_path = "QA_report_routine.xlsx",
#     html_path  = "QA_report_routine.html"
#   )
#
#   # Access results programmatically:
#   qa$summary      # one row per rule: passes, fails, pct_fail, status
#   qa$flagged      # all rows violating at least one rule
#   qa$na_overview  # missingness per column
#   qa$result       # raw validate confrontation object
#
#   # Re-examine rules interactively:
#   summary(qa$result)
#   plot(qa$result)   # bar chart of pass/fail per rule
#
