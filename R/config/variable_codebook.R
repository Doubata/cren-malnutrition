# =============================================================================
# variable_codebook.R — three-tier codebook, all CSV:
#
#   domains.csv            canonical domain list + display order
#                            cols: domain, label, domain_display_order, description(optional)
#
#   domain_subgroups.csv   optional finer grouping within a domain
#                            cols: subgroup, label, subgroup_display_order, domain (FK -> domains$domain)
#
#   variable_codebook.csv  one row per variable
#                            cols: variable, label, description(optional), domain (FK),
#                                        domain_subgroup (FK, optional), type_override (optional),
#                                        geom_override (optional)
#                            + any number of open-ended boolean ROLE columns
#                              (e.g. baseline, baseline_p1, univariate_pt, regression_p1, ...)
#                              marking which analysis step(s)/phase(s) a variable feeds into.
#                              Role columns are auto-detected: anything not in the fixed set
#                              above is treated as a role flag.
#
# `domain` and `subgroup` are KEYS (stable, used for joins/filters). `label`
# is the human-readable display text. Never the same column.
# =============================================================================

CODEBOOK_COLS <- c("variable", "label", "description", "domain",
                   "domain_subgroup", "type_override", "geom_override")

# ---- loaders (CSV only) -----------------------------------------------

load_domain_registry <- function(registry) {
  # registry <- readr::read_csv(path, show_col_types = FALSE, na = c("", "NA"))

  required_cols <- c("domain", "label", "domain_display_order")
  missing_cols  <- setdiff(required_cols, names(registry))
  if (length(missing_cols) > 0) {
    stop("Domain registry is missing required column(s): ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  dupes <- registry$domain[duplicated(registry$domain)]
  if (length(dupes) > 0) {
    stop("Duplicate domain key(s) in registry: ", paste(unique(dupes), collapse = ", "),
         call. = FALSE)
  }

  registry
}

load_domain_subgroup_registry <- function(registry) {
  # registry <- readr::read_csv(path, show_col_types = FALSE, na = c("", "NA"))

  required_cols <- c("subgroup", "label", "subgroup_display_order", "domain")
  missing_cols  <- setdiff(required_cols, names(registry))
  if (length(missing_cols) > 0) {
    stop("Domain subgroup registry is missing required column(s): ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  dupes <- registry$subgroup[duplicated(registry$subgroup)]
  if (length(dupes) > 0) {
    stop("Duplicate subgroup key(s) in registry: ", paste(unique(dupes), collapse = ", "),
         call. = FALSE)
  }

  registry
}

load_variable_codebook <- function(codebook) {
  # codebook <- readr::read_csv(path, show_col_types = FALSE, na = c("", "NA"))

  required_cols <- c("variable", "label", "domain")
  missing_cols  <- setdiff(required_cols, names(codebook))
  if (length(missing_cols) > 0) {
    stop("Variable codebook is missing required column(s): ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  codebook
}

# ---- truthy handling for role-flag columns (values: TRUE/FALSE/blank) ------

.is_truthy <- function(x) {
  if (is.logical(x)) return(!is.na(x) & x)
  !is.na(x) & toupper(trimws(as.character(x))) == "TRUE"
}

.is_valid_boolean_like <- function(x) {
  is.na(x) | toupper(trimws(as.character(x))) %in% c("TRUE", "FALSE")
}

# Anything not in the fixed metadata set is treated as a role-flag column.
codebook_role_columns <- function(codebook) {
  setdiff(names(codebook), CODEBOOK_COLS)
}

# Variables where the given role column is TRUE.
codebook_variables_for_role <- function(codebook, role) {
  available <- codebook_role_columns(codebook)
  if (!role %in% available) {
    suggestion <- .suggest_close_match(role, available)
    hint <- if (!is.na(suggestion)) paste0(" Did you mean '", suggestion, "'?") else ""
    stop("Unknown role column: '", role, "'.", hint,
         " Available: ", paste(available, collapse = ", "), call. = FALSE)
  }
  codebook$variable[.is_truthy(codebook[[role]])]
}

# ---- typo-suggestion helper (edit distance) --------------------------------

.suggest_close_match <- function(bad, choices, max_dist = 3) {
  if (length(choices) == 0) return(NA_character_)
  dists <- utils::adist(bad, choices)[1, ]
  best  <- choices[which.min(dists)]
  if (min(dists) <= max_dist) best else NA_character_
}

.format_unknown_values <- function(unknown, allowed, label) {
  suggestions <- purrr::map_chr(unknown, .suggest_close_match, choices = allowed)
  purrr::map2_chr(unknown, suggestions, function(bad, sugg) {
    if (is.na(sugg)) paste0("  '", bad, "' -- no close match among ", label)
    else              paste0("  '", bad, "' -- did you mean '", sugg, "'?")
  }) |> paste(collapse = "\n")
}

# ---- validation -------------------------------------------------------

# Fails fast, before any plotting:
#  - required columns present
#  - every codebook variable exists in `data`; every requested var has a codebook row
#  - no duplicate variable rows
#  - every domain is a known key in domain_registry (typo-suggested if not)
#  - every non-blank domain_subgroup is a known key in subgroup_registry (if supplied)
#  - each variable's domain_subgroup (if any) actually belongs to that variable's domain
#  - role-flag columns only contain TRUE/FALSE/blank
validate_codebook <- function(codebook, data, domain_registry,
                               subgroup_registry = NULL, vars = NULL) {

  problems <- character(0)
  vars <- vars %||% codebook$variable

  not_in_data <- setdiff(intersect(vars, codebook$variable), names(data))
  if (length(not_in_data) > 0)
    problems <- c(problems, paste0("Codebook variables not found in data: ", paste(not_in_data, collapse = ", ")))

  not_in_codebook <- setdiff(vars, codebook$variable)
  if (length(not_in_codebook) > 0)
    problems <- c(problems, paste0("Vars requested but missing from codebook: ", paste(not_in_codebook, collapse = ", ")))

  dupes <- codebook$variable[duplicated(codebook$variable)]
  if (length(dupes) > 0)
    problems <- c(problems, paste0("Duplicate variable entries in codebook: ", paste(unique(dupes), collapse = ", ")))

  unknown_domains <- setdiff(unique(codebook$domain), domain_registry$domain)
  if (length(unknown_domains) > 0)
    problems <- c(problems, paste0("Domain key(s) not in registry:\n",
                                    .format_unknown_values(unknown_domains, domain_registry$domain, "registry")))

  if ("domain_subgroup" %in% names(codebook)) {
    used_subgroups <- unique(stats::na.omit(codebook$domain_subgroup))

    if (length(used_subgroups) > 0 && is.null(subgroup_registry)) {
      problems <- c(problems, paste0(
        "Codebook uses domain_subgroup values (", paste(used_subgroups, collapse = ", "),
        ") but no subgroup_registry was supplied to validate_codebook()."
      ))
    } else if (length(used_subgroups) > 0) {
      unknown_subgroups <- setdiff(used_subgroups, subgroup_registry$subgroup)
      if (length(unknown_subgroups) > 0)
        problems <- c(problems, paste0("Subgroup key(s) not in registry:\n",
                                        .format_unknown_values(unknown_subgroups, subgroup_registry$subgroup, "registry")))

      # Consistency check: a variable's domain must match its subgroup's domain
      rows_with_subgroup <- codebook |> dplyr::filter(!is.na(.data$domain_subgroup))
      mismatches <- rows_with_subgroup |>
        dplyr::left_join(
          subgroup_registry |> dplyr::select(subgroup, registry_domain = domain),
          by = c("domain_subgroup" = "subgroup")
        ) |>
        dplyr::filter(!is.na(.data$registry_domain), .data$domain != .data$registry_domain)

      if (nrow(mismatches) > 0) {
        msg <- paste0(
          "  '", mismatches$variable, "': domain='", mismatches$domain,
          "' but subgroup '", mismatches$domain_subgroup, "' belongs to domain='",
          mismatches$registry_domain, "'"
        ) |> paste(collapse = "\n")
        problems <- c(problems, paste0("Variable/subgroup domain mismatch:\n", msg))
      }
    }
  }

  for (role in codebook_role_columns(codebook)) {
    bad <- !.is_valid_boolean_like(codebook[[role]])
    if (any(bad)) {
      problems <- c(problems, paste0(
        "Role column '", role, "' has non-boolean value(s) for: ",
        paste(codebook$variable[bad], collapse = ", ")
      ))
    }
  }

  if (length(problems) > 0) {
    stop("Codebook validation failed:\n- ", paste(problems, collapse = "\n- "), call. = FALSE)
  }

  invisible(TRUE)
}

# ---- extraction into maps used by plot_catalog() / auto_plot_catalog() -----

codebook_to_domain_map <- function(codebook) {
  rlang::set_names(codebook$domain, codebook$variable)
}

codebook_to_label_map <- function(codebook) {
  rlang::set_names(codebook$label, codebook$variable)
}

# Writes codebook labels onto `data` itself via labelled::var_label<-, so
# gtsummary::tbl_summary() (which reads that attribute automatically) and
# the plot catalog (via .safe_var_label()'s label_map override) draw from
# the SAME source instead of two independently-maintained label sets.
#
# Only touches variables that are both labelled in the codebook AND present
# in `data` -- variables missing from data are silently skipped here (not
# an error: apply_codebook_labels() isn't a validation step, use
# validate_codebook()/validate_phase_metadata() for that). Pass `vars` to
# scope labelling to one phase/analysis stage if you don't want to label the
# whole dataset at once.
apply_codebook_labels <- function(data, codebook, vars = NULL) {
  label_map <- codebook_to_label_map(codebook)
  label_map <- label_map[!is.na(label_map)]
  
  if (!is.null(vars)) label_map <- label_map[intersect(names(label_map), vars)]
  label_map <- label_map[intersect(names(label_map), names(data))]
  
  if (length(label_map) == 0) {
    warning("No matching labelled variables found to apply.", call. = FALSE)
    return(data)
  }
  
  labelled::var_label(data) <- as.list(label_map)
  data
}

codebook_to_description_map <- function(codebook) {
  if (!"description" %in% names(codebook)) return(NULL)
  ov <- codebook |> dplyr::filter(!is.na(.data$description))
  if (nrow(ov) == 0) return(NULL)
  rlang::set_names(ov$description, ov$variable)
}

codebook_to_subgroup_map <- function(codebook) {
  if (!"domain_subgroup" %in% names(codebook)) return(NULL)
  ov <- codebook |> dplyr::filter(!is.na(.data$domain_subgroup))
  if (nrow(ov) == 0) return(NULL)
  rlang::set_names(ov$domain_subgroup, ov$variable)
}

codebook_to_type_map <- function(codebook) {
  if (!"type_override" %in% names(codebook)) return(NULL)
  ov <- codebook |> dplyr::filter(!is.na(.data$type_override))
  if (nrow(ov) == 0) return(NULL)
  rlang::set_names(ov$type_override, ov$variable)
}

codebook_to_geom_map <- function(codebook) {
  # NOTE: auto_plot_catalog()'s geom_map is keyed by chart_type (one geom
  # choice applies to a whole detected-type group), not by individual var.
  # For true per-variable geom overrides, rebuild those rows via
  # plot_catalog(..., geom = ...) and splice them into the combined catalog.
  if (!"geom_override" %in% names(codebook)) return(NULL)
  ov <- codebook |> dplyr::filter(!is.na(.data$geom_override))
  if (nrow(ov) == 0) return(NULL)
  rlang::set_names(ov$geom_override, ov$variable)
}

# ---- display helpers (key <-> label, ordering) -----------------------------

domain_display_order <- function(domain_registry) {
  domain_registry |> dplyr::arrange(.data$domain_display_order) |> dplyr::pull(.data$domain)
}

domain_label_lookup <- function(domain_registry) {
  rlang::set_names(domain_registry$label, domain_registry$domain)
}

subgroup_display_order <- function(subgroup_registry, domain = NULL) {
  out <- subgroup_registry
  if (!is.null(domain)) out <- dplyr::filter(out, .data$domain == .env$domain)
  out |> dplyr::arrange(.data$subgroup_display_order) |> dplyr::pull(.data$subgroup)
}

subgroup_label_lookup <- function(subgroup_registry) {
  rlang::set_names(subgroup_registry$label, subgroup_registry$subgroup)
}
