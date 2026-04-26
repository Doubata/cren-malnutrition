# =============================================================================
# QA FUNCTION — ROUTINE DATAFRAME
# Uses {pointblank} for validation + HTML/Excel export
# install.packages(c("pointblank", "dplyr", "writexl"))
# =============================================================================

library(pointblank)
library(dplyr)
library(writexl)

# =============================================================================
# CONFIGURATION — adapt to your protocol
# =============================================================================

REPAS_COLS <- paste0("repasH", 1:10)

VALID_REPAS  <- c("1", "1_2", "3_4", "sonde", "vomit")
VALID_PTZS   <- c("inf-3", "inf-2", "inf-1", "0", "sup1", "sup2", "sup3")
VALID_PHASE  <- c("1", "2", "3", "4")
VALID_ALIM   <- c("F75", "F75F", "F100", "F100d", "ATPE")
VALID_APLATS <- c("LM", "non")

# =============================================================================
# MAIN FUNCTION
# =============================================================================

run_monitoring_qa <- function(df,
                   html_path  = "QA_report_routine.html",
                   excel_path = "QA_report_routine.xlsx") {
  
  # ── Build the pointblank agent ─────────────────────────────────────────────
  agent <- create_agent(
    tbl       = df,
    tbl_name  = "routine",
    label     = paste0("Routine QA — ", format(Sys.Date(), "%Y-%m-%d"))
  ) |>
    
    # ── 1. MISSING VALUES: key clinical variables should not be all-NA --------
    col_vals_not_null(columns = vars(id),      label = "id: no missing values") |>
    col_vals_not_null(columns = vars(dateVis), label = "dateVis: no missing values") |>
    col_vals_not_null(columns = vars(phase),   label = "phase: no missing values",
                      actions = warn_on_fail(warn_at = 0.05)) |>
    col_vals_not_null(columns = vars(oedemes), label = "oedemes: no missing values",
                      actions = warn_on_fail(warn_at = 0.05)) |>
    col_vals_not_null(columns = vars(poids),   label = "poids: no missing values",
                      actions = warn_on_fail(warn_at = 0.10)) |>
    col_vals_not_null(columns = vars(taille),  label = "taille: no missing values",
                      actions = warn_on_fail(warn_at = 0.10)) |>
    
    # ── 2. DUPLICATES: each id×dateVis should appear only once ---------------
  rows_distinct(
    columns = vars(id, dateVis),
    label   = "No duplicate id × dateVis"
  ) |>
    
    # ── 3. DATE FORMAT: DD/MM/YYYY -------------------------------------------
  col_vals_regex(
    columns = vars(dateVis),
    regex   = "^\\d{2}/\\d{2}/\\d{4}$",
    label   = "dateVis: format DD/MM/YYYY"
  ) |>
    
    # ── 4. NUMERIC RANGES ─────────────────────────────────────────────────────
    col_vals_between(
      columns = vars(taille), left = 30, right = 130,
      label   = "taille: between 30–130 cm", na_pass = TRUE
    ) |>
    col_vals_between(
      columns = vars(poids), left = 100, right = 5000,
      label   = "poids: between 100–5000 g", na_pass = TRUE
    ) |>
    col_vals_between(
      columns = vars(pbOrPcV), left = 60, right = 200,
      label   = "pbOrPcV: between 60–200 mm", na_pass = TRUE
    ) |>
    col_vals_between(
      columns = vars(oedemes), left = 0, right = 3,
      label   = "oedemes: between 0–3", na_pass = TRUE
    ) |>
    col_vals_between(
      columns = vars(nRepJr), left = 6, right = 16,
      label   = "nRepJr: between 6–16", na_pass = TRUE
    ) |>
    col_vals_between(
      columns = vars(mlRepas), left = 1, right = 500,
      label   = "mlRepas: between 1–500 ml", na_pass = TRUE
    ) |>
    col_vals_between(
      columns = vars(selles), left = 0, right = 20,
      label   = "selles: between 0–20", na_pass = TRUE
    ) |>
    col_vals_between(
      columns = vars(vomissCorrige), left = 0, right = 20,
      label   = "vomissCorrige: between 0–20", na_pass = TRUE
    ) |>
    col_vals_between(
      columns = vars(bouillieCorrige), left = 0, right = 500,
      label   = "bouillieCorrige: between 0–500", na_pass = TRUE
    ) |>
    
    # ── 5. CATEGORICAL VALUES ─────────────────────────────────────────────────
    col_vals_in_set(
      columns       = vars(ptZscore), set = VALID_PTZS,
      label         = "ptZscore: valid z-score category",
      preconditions = function(x) x |> filter(!is.na(ptZscore))
    ) |>
    col_vals_in_set(
      columns       = vars(pbOrPc), set = c("pb", "pc"),
      label         = "pbOrPc: 'pb' or 'pc'",
      preconditions = function(x) x |> filter(!is.na(pbOrPc))
    ) |>
    col_vals_in_set(
      columns       = vars(phase), set = VALID_PHASE,
      label         = "phase: valid phase (1–4)",
      preconditions = function(x) x |> filter(!is.na(phase))
    ) |>
    col_vals_in_set(
      columns       = vars(typeAlim), set = VALID_ALIM,
      label         = "typeAlim: valid food type",
      preconditions = function(x) x |> filter(!is.na(typeAlim))
    ) |>
    col_vals_in_set(
      columns       = vars(aPlats), set = VALID_APLATS,
      label         = "aPlats: 'LM' or 'non'",
      preconditions = function(x) x |> filter(!is.na(aPlats))
    ) |>
    col_vals_in_set(
      columns       = starts_with("repasH"), set = VALID_REPAS,
      label         = "repasH*: valid meal intake code",
      preconditions = function(x) x |> filter(if_any(starts_with("repasH"), ~ !is.na(.)))
    ) |>
    
    # ── 6. LOGIC CHECKS ───────────────────────────────────────────────────────
    # pbOrPc filled → pbOrPcV must also be filled
    col_vals_expr(
      expr  = ~ is.na(pbOrPc) | !is.na(pbOrPcV),
      label = "Logic: pbOrPc filled implies pbOrPcV filled"
    ) |>
    # Phase 2/3 should not have F75/F75F diet
    col_vals_expr(
      expr  = ~ is.na(phase) | is.na(typeAlim) |
        !(phase %in% c("2","3") & typeAlim %in% c("F75","F75F")),
      label = "Logic: phase 2/3 not combined with F75/F75F diet"
    ) |>
    # mlRepas must not be zero
    col_vals_expr(
      expr  = ~ is.na(mlRepas) | mlRepas > 0,
      label = "Logic: mlRepas must be > 0"
    ) |>
    # nRepJr must be one of the allowed values
    col_vals_in_set(
      columns       = vars(nRepJr), set = c(8, 10, 12),
      label         = "Logic: nRepJr must be 8, 10 or 12",
      preconditions = function(x) x |> filter(!is.na(nRepJr))
    ) |>
    
    # ── Run all checks ────────────────────────────────────────────────────────
    interrogate()
  
  # ── HTML report (pointblank native) ───────────────────────────────────────
  export_report(agent, filename = html_path)
  message("HTML report saved: ", html_path)
  
  # ── Excel export: one row per failing step + all_passed flag ──────────────
  report_df <- get_agent_report(agent, display_table = FALSE)
  
  summary_tbl <- report_df |>
    mutate(
      n_fail   = units - n_pass,
      pct_fail = round((1 - f_pass) * 100, 2),
      status   = case_when(
        n_fail == 0 ~ "PASS",
        W == TRUE   ~ "WARN",
        S == TRUE   ~ "FAIL",
        TRUE        ~ "FAIL"
      )
    ) |>
    select(
      step      = i,
      type,
      column    = columns,
      n_tested  = units,
      n_pass,
      n_fail,
      pct_fail,
      status,
      warn      = W,
      stop      = S
    )
  
  # Flagged rows per failing step (for traceability)
  flagged_rows <- get_data_extracts(agent) |>
    imap_dfr(function(tbl, step_i) {
      if (is.null(tbl) || nrow(tbl) == 0) return(NULL)
      mutate(tbl, .step = as.integer(step_i),
             .type = report_df$type[report_df$i == as.integer(step_i)],
             .column = as.character(report_df$columns[report_df$i == as.integer(step_i)]))
    }) |>
    select(.step, .type, .column, everything())
  
  write_xlsx(
    list(
      Summary      = summary_tbl,
      Flagged_Rows = flagged_rows
    ),
    path = excel_path
  )
  message("Excel report saved: ", excel_path)
  
  invisible(list(
    agent       = agent,
    summary     = summary_tbl,
    flagged     = flagged_rows
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
#     html_path  = "QA_report_routine.html",
#     excel_path = "QA_report_routine.xlsx"
#   )
#
#   # Interrogate results programmatically:
#   qa$summary        # data frame: one row per validation step
#   qa$flagged        # data frame: all rows that failed at least one check
#   qa$agent          # full pointblank agent object for further inspection
#
#   # Re-print the HTML report in the viewer:
#   get_agent_report(qa$agent)
#