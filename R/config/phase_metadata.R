# =============================================================================
# phase_metadata.R — extract a full phase's variable sets from the codebook's
# role-flag matrix in one call, instead of one codebook_variables_for_role()
# call per role column.
#
# Assumes role columns follow the "<stage>_<phase>" naming convention already
# used in the codebook (baseline_p1, cat_var_baseline_p1, outcome_p1, ...),
# alongside phase-agnostic columns with no suffix (e.g. "baseline" applying
# regardless of phase).
# =============================================================================

# Detect which role columns belong to a given phase suffix (e.g. "p1", "pt").
.phase_role_columns <- function(codebook, phase) {
  available <- codebook_role_columns(codebook)
  suffix <- paste0("_", phase)
  matched <- available[endsWith(available, suffix)]
  if (length(matched) == 0) {
    stop("No role columns found for phase '", phase, "'. Available role columns: ",
         paste(available, collapse = ", "), call. = FALSE)
  }
  matched
}

# Strip the phase suffix to get a clean, phase-agnostic list name
# (e.g. "cat_var_baseline_p1" -> "cat_var_baseline")
.strip_phase_suffix <- function(role_col, phase) {
  sub(paste0("_", phase, "$"), "", role_col)
}

# One call, one phase, every role -> a named list of variable-name vectors.
# Empty roles (no variable flagged TRUE) are kept as character(0) rather than
# dropped, so downstream code can rely on every expected name being present.
load_phase_metadata <- function(codebook, phase) {
  role_cols <- .phase_role_columns(codebook, phase)
  stage_names <- purrr::map_chr(role_cols, .strip_phase_suffix, phase = phase)

  purrr::map(role_cols, ~ codebook_variables_for_role(codebook, .x)) |>
    rlang::set_names(stage_names)
}

# Scoped validation: checks only the variables THIS phase actually needs,
# rather than validate_codebook()'s default of checking every codebook row.

validate_phase_metadata <- function(metadata, codebook, data, domain_registry,
                                     subgroup_registry = NULL) {
  vars <- unique(unlist(metadata, use.names = FALSE))
  validate_codebook(codebook, data, domain_registry, subgroup_registry, vars = vars)
}

# Quick sanity check before running a full pipeline stage: counts per role,
# flags any role with zero variables (likely a codebook gap, not intentional).
phase_metadata_summary <- function(metadata) {
  tibble::tibble(
    role     = names(metadata),
    n_vars   = purrr::map_int(metadata, length),
    is_empty = n_vars == 0
  )
}
