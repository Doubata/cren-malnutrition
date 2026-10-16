#' Compute Phase-Specific Nutritional Estimates per Child
#'
#' @description
#' Enriches a child-level data frame (\code{child_df}) with nutritional
#' indicators derived from a longitudinal monitoring data frame
#' (\code{monitoring_df}), restricted to a specific phase run
#' (\code{run_idx}). All output columns are suffixed with \code{_p{phase}}
#' (e.g. \code{nutri_treat_p1}, \code{prop_familymeals_p2}) to allow the
#' function to be called iteratively across multiple phases without column
#' name collisions. The function is designed to be used within a
#' \pkg{dplyr} pipeline.
#'
#' @param child_df      A child-level data frame (one row per child). Must
#'                      contain:
#'   \itemize{
#'     \item \code{id} — unique child identifier matched against
#'           \code{monitoring_df}.
#'   }
#' @param monitoring_df A longitudinal monitoring data frame (multiple rows
#'                      per child). Must contain:
#'   \itemize{
#'     \item \code{id}                   — child identifier (numeric).
#'     \item \code{phase}                — phase label (character), used as
#'           the run-length reference series.
#'     \item \code{typeAlim}             — nutritional product given (character).
#'     \item \code{rutf}                 — RUTF attribute column (character).
#'     \item \code{f75x_freq_validation} — frequency adherence label
#'           (\code{"less"} / \code{"within"} / \code{"more"}).
#'     \item \code{f75x_qty_validation}  — quantity adherence label.
#'     \item \code{f75x_adh_validation}  — daily intake adherence label.
#'     \item \code{plaFamilCorrige}      — family meal consumption
#'           (\code{"oui"} / \code{"non"}).
#'     \item \code{food_intolerance}     — food intolerance flag
#'           (\code{"TRUE"} / \code{"FALSE"}).
#'   }
#' @param phase   Character scalar. The phase label used to suffix all output
#'                column names (e.g. \code{phase = "P1"} produces
#'                \code{nutri_treat_P1}, \code{prop_familymeals_P1}, etc.).
#'                Must be a non-empty string. Typically matches a value found
#'                in the \code{phase} column of \code{monitoring_df} but is
#'                not validated against it.
#' @param run_idx Positive integer. The run index passed to \code{run_order}
#'                in all \code{\link{summarise_event}()} and
#'                \code{\link{get_matching_series_str}()} calls. Typically
#'                corresponds to the positional index of \code{phase} in the
#'                sequence of phases but is kept separate to allow flexible
#'                phase-to-run mappings.
#'
#' @return
#' A data frame identical to \code{child_df} with the following new columns
#' appended, each suffixed with \code{_p{phase}}:
#' \describe{
#'   \item{\code{nutri_treat_series_p{phase}}}{Character. Dash-separated
#'         run-length encoded string of nutritional products given during
#'         the \code{run_idx}-th phase run, with RUTF attribute appended as
#'         \code{"product+rutf"} where present (e.g. \code{"F75-F75F"}).}
#'   \item{\code{nutri_treat_p{phase}}}{Character. Generalised nutritional
#'         treatment category. One of: \code{"F75"}, \code{"F75F"},
#'         \code{"F75-F75F"}, \code{"F75F-F75"}, \code{"F75+RUTF"},
#'         \code{"F75F+RUTF"}, \code{"F100"}, \code{"F100d"},
#'         \code{"Others"}, or \code{NA} (when series is \code{"MISSING"}).
#'         \strong{Note:} \code{"Others"} is a catch-all — inspect these
#'         rows if they appear in output.}
#'   \item{\code{prop_f75x_freq_less_p{phase}}}{Numeric \code{[0,1]}.
#'         Proportion of phase-run rows where F75/F75F frequency was below
#'         WHO recommendation.}
#'   \item{\code{prop_f75x_freq_more_p{phase}}}{Numeric \code{[0,1]}.
#'         Proportion of phase-run rows where F75/F75F frequency exceeded
#'         WHO recommendation.}
#'   \item{\code{prop_f75x_qty_less_p{phase}}}{Numeric \code{[0,1]}.
#'         Proportion of phase-run rows where F75/F75F quantity was below
#'         WHO recommendation.}
#'   \item{\code{prop_f75x_qty_more_p{phase}}}{Numeric \code{[0,1]}.
#'         Proportion of phase-run rows where F75/F75F quantity exceeded
#'         WHO recommendation.}
#'   \item{\code{prop_f75x_adh_less_p{phase}}}{Numeric \code{[0,1]}.
#'         Proportion of phase-run rows where actual daily intake was below
#'         WHO recommendation.}
#'   \item{\code{prop_f75x_adh_more_p{phase}}}{Numeric \code{[0,1]}.
#'         Proportion of phase-run rows where actual daily intake exceeded
#'         WHO recommendation.}
#'   \item{\code{prop_familymeals_p{phase}}}{Numeric \code{[0,1]}.
#'         Proportion of phase-run rows where family meals were consumed.}
#'   \item{\code{prop_food_intolerance_p{phase}}}{Numeric \code{[0,1]}.
#'         Proportion of phase-run rows where food intolerance was recorded.}
#' }
#'
#' @details
#' Processing pipeline:
#' \enumerate{
#'   \item \strong{Validation} — asserts both data frames are non-empty and
#'         that all required columns are present in \code{monitoring_df}.
#'   \item \strong{Column registry} — \code{phase_cols} lists every new
#'         column to be suffixed, decoupled from the \code{mutate()} block
#'         so that additions or removals only require editing one vector.
#'   \item \strong{Enrichment} — each indicator is computed via
#'         \code{\link{get_matching_series_str}()} or
#'         \code{\link{summarise_event}(type = "proportion")}, all
#'         restricted to \code{run_order = run_idx}.
#'   \item \strong{Column suffixing} — \code{\link[dplyr]{rename_with}()}
#'         appends \code{_p{phase}} to every column listed in
#'         \code{phase_cols}, enabling collision-free multi-phase iteration.
#' }
#'
#' The \code{nutri_treat} classification uses ordered regex matching:
#' \enumerate{
#'   \item \code{"MISSING"} only → \code{NA}.
#'   \item Only \code{F75} (+ gaps)        → \code{"F75"}.
#'   \item Only \code{F75F} (+ gaps)       → \code{"F75F"}.
#'   \item \code{F75 → F75F}, no F100/F100d → \code{"F75-F75F"}.
#'   \item \code{F75F → F75}, no F100/F100d → \code{"F75F-F75"}.
#'   \item Only \code{F75+RUTF} (+ gaps)   → \code{"F75+RUTF"}.
#'   \item Only \code{F75F+RUTF} (+ gaps)  → \code{"F75F+RUTF"}.
#'   \item Only \code{F100} (+ gaps)       → \code{"F100"}.
#'   \item Only \code{F100d} (+ gaps)      → \code{"F100d"}.
#'   \item Anything else                   → \code{"Others"}.
#' }
#'
#' @note
#' \code{ids = id} inside each \code{\link{summarise_event}()} call resolves
#' row-wise from \code{child_df} at compute time within \code{mutate()} —
#' this is the tidy-evaluation mechanism that links each child row to its
#' corresponding monitoring records without an explicit join. \cr\cr
#' To compute indicators for all phases, call this function iteratively
#' using \code{\link[purrr]{reduce}()}:
#' \preformatted{
#' phases  <- c("P1", "P2")
#' run_ids <- c(1, 2)
#'
#' df_full <- purrr::reduce2(
#'   phases, run_ids,
#'   ~ add_nutrition_estimates(..1, monitoring_df,
#'                             phase   = ..2,
#'                             run_idx = ..3),
#'   .init = child_df
#' )
#' }

#' @examples
#' # Single phase
#' enriched_p1 <- add_nutrition_estimates(child_df, monitoring_df,
#'                                        phase = "P1", run_idx = 1)
#' names(enriched_p1)
#' #> ... "nutri_treat_series_P1" "nutri_treat_P1" "prop_f75x_freq_less_P1" ...
#'
#' # Inspect treatment category distribution
#' table(enriched_p1$nutri_treat_P1, useNA = "ifany")
#'
#' # Flag children with frequent under-feeding in phase 1
#' enriched_p1[enriched_p1$prop_f75x_qty_less_P1 > 0.5, ]
#' 
#' @family pipeline
#'
#' @seealso
#' \code{\link{summarise_event}}, \code{\link{get_matching_series_str}},
#' \code{\link{validate_f75x_adh}}, \code{\link{validate_f75x_qty}}
#'
#' @importFrom dplyr mutate case_when rename_with all_of
#' @importFrom stringr str_detect str_starts
#'
#' @export
add_nutrition_estimates <- function(child_df, monitoring_df, cohort=NULL, phase=NULL, run_idx=NULL) {
  
  # --- 1. Validation -------------------------------------------------------
  assert_data_frame(child_df,      min.rows = 1)
  assert_data_frame(monitoring_df, min.rows = 1)
  assert_names(names(monitoring_df),
               must.include = c("id", "phase", "typeAlim", "rutf",
                                "f75x_freq_validation", "f75x_qty_validation",
                                "f75x_adh_validation", "plaFamilCorrige",
                                "food_intolerance"))
  # assert_count(phase,   positive = TRUE)
  # assert_count(run_idx, positive = TRUE)
  
  df_enriched <- child_df %>%
    mutate(
      is_breastfed = case_when(
        age_month < 25 ~ "Yes",
        TRUE ~ is_breastfed
      )
    )
  
  if(cohort %in% c("c659_noedema", "c659_edema")){
    
    # --- 2. Column Registry --------------------------------------------------
    # Centralised list of all new columns to be suffixed with _p{phase}.
    # Add or remove columns here only — the mutate() block handles computation,
    # rename_with() handles suffixing.
    phase_cols <- c(
      "nutri_treat_series",
      "nutri_treat",
      "prop_f75x_freq_less",
      "prop_f75x_freq_more",
      "prop_f75x_qty_less",
      "prop_f75x_qty_more",
      "prop_f75x_adh_less",
      "prop_f75x_adh_more",
      "prop_familymeals",
      "prop_food_intolerance"
    )
    
    # --- 3. Enrichment Pipeline ----------------------------------------------
    df_enriched <- df_enriched %>%
      mutate(
        
        # --- Nutritional Treatment Series ------------------------------------
        # Run-length encoded string of products given during run_idx phase run.
        # RUTF attribute appended as "product+rutf" where present.
        nutri_treat_series = get_matching_series_str(
          df         = monitoring_df,
          id_col     = "id",
          target_col = "typeAlim",
          ref_col    = "phase",
          attr_col   = "rutf",
          run_order  = run_idx,
          ids        = id
        ),

        # --- Generalised Treatment Category ----------------------------------
        nutri_treat = generalize_nutri_treatement(nutri_treat_series),
        
        # --- F75/F75F Frequency Validation -----------------------------------
        # Proportion of phase-run rows where prescribed frequency < WHO target
        prop_f75x_freq_less = summarise_event(
          df          = monitoring_df,
          id_col      = "id",
          phase_col   = "phase",
          event       = "less",
          target_cols = "f75x_freq_validation",
          ids         = id,
          run_order   = run_idx,
          type        = "proportion"
        ),
        prop_f75x_freq_less = coalesce(prop_f75x_freq_less, 0),
        
        # Proportion of phase-run rows where prescribed frequency > WHO target
        prop_f75x_freq_more = summarise_event(
          df          = monitoring_df,
          id_col      = "id",
          phase_col   = "phase",
          event       = "more",
          target_cols = "f75x_freq_validation",
          ids         = id,
          run_order   = run_idx,
          type        = "proportion"
        ),
        prop_f75x_freq_more = coalesce(prop_f75x_freq_more, 0),
        
        # --- F75/F75F Quantity Validation ------------------------------------
        # Proportion of phase-run rows where prescribed quantity < WHO target
        prop_f75x_qty_less = summarise_event(
          df          = monitoring_df,
          id_col      = "id",
          phase_col   = "phase",
          event       = "less",
          target_cols = "f75x_qty_validation",
          ids         = id,
          run_order   = run_idx,
          type        = "proportion"
        ),
        prop_f75x_qty_less = coalesce(prop_f75x_qty_less, 0),
        
        # Proportion of phase-run rows where prescribed quantity > WHO target
        prop_f75x_qty_more = summarise_event(
          df          = monitoring_df,
          id_col      = "id",
          phase_col   = "phase",
          event       = "more",
          target_cols = "f75x_qty_validation",
          ids         = id,
          run_order   = run_idx,
          type        = "proportion"
        ),
        prop_f75x_qty_more = coalesce(prop_f75x_qty_more, 0),
        
        # --- F75/F75F Adherence Validation -----------------------------------
        # Proportion of phase-run rows where actual daily intake < WHO target
        prop_f75x_adh_less = summarise_event(
          df          = monitoring_df,
          id_col      = "id",
          phase_col   = "phase",
          event       = "less",
          target_cols = "f75x_adh_validation",
          ids         = id,
          run_order   = run_idx,
          type        = "proportion"
        ),
        prop_f75x_adh_less = coalesce(prop_f75x_adh_less, 0),
        
        # Proportion of phase-run rows where actual daily intake > WHO target
        prop_f75x_adh_more = summarise_event(
          df          = monitoring_df,
          id_col      = "id",
          phase_col   = "phase",
          event       = "more",
          target_cols = "f75x_adh_validation",
          ids         = id,
          run_order   = run_idx,
          type        = "proportion"
        ),
        prop_f75x_adh_more = coalesce(prop_f75x_adh_more, 0),
        
        # --- Family Meal Consumption -----------------------------------------
        # Proportion of phase-run rows where family meals were consumed
        prop_familymeals = summarise_event(
          df          = monitoring_df,
          id_col      = "id",
          phase_col   = "phase",
          event       = "oui",
          target_cols = "plaFamilCorrige",
          ids         = id,
          run_order   = run_idx,
          type        = "proportion"
        ),
        prop_familymeals = coalesce(prop_familymeals, 0),
        
        # --- Food Intolerance ------------------------------------------------
        # Proportion of phase-run rows where food intolerance was recorded
        prop_food_intolerance = summarise_event(
          df          = monitoring_df,
          id_col      = "id",
          phase_col   = "phase",
          event       = "TRUE",
          target_cols = "food_intolerance",
          ids         = id,
          run_order   = run_idx,
          type        = "proportion"
        ),
        prop_food_intolerance = coalesce(prop_food_intolerance, 0),
        
      ) %>%
      
      # --- 4. Column Suffixing -----------------------------------------------
    # Append _{phase} to every column in phase_cols, enabling collision-free
    # multi-phase iteration when this function is called repeatedly.
    # e.g. phase = "1" → nutri_treat_p1, prop_familymeals_p1, etc.
    rename_with(
      .fn   = ~ paste0(.x, "_p", tolower(phase)),
      .cols = all_of(phase_cols)
    )
    
    if(phase=="T"){
      
      phaseT_c659_cols <- c(
        "prop_f100_freq_less",
        "prop_f100_freq_more",
        "prop_f100_qty_less",
        "prop_f100_qty_more",
        "prop_f100_adh_less",
        "prop_f100_adh_more"
      )
      
      df_enriched <- df_enriched %>%
        mutate(
          # --- F100 Frequency Validation -----------------------------------
          # Proportion of phase-run rows where prescribed frequency < WHO target
          prop_f100_freq_less = summarise_event(
            df          = monitoring_df,
            id_col      = "id",
            phase_col   = "phase",
            event       = "less",
            target_cols = "f100_freq_validation",
            ids         = id,
            run_order   = run_idx,
            type        = "proportion"
          ),
          prop_f100_freq_less = coalesce(prop_f100_freq_less, 0),
          
          # Proportion of phase-run rows where prescribed frequency > WHO target
          prop_f100_freq_more = summarise_event(
            df          = monitoring_df,
            id_col      = "id",
            phase_col   = "phase",
            event       = "more",
            target_cols = "f100_freq_validation",
            ids         = id,
            run_order   = run_idx,
            type        = "proportion"
          ),
          prop_f100_freq_more = coalesce(prop_f100_freq_more, 0),
          
          # --- F100 Quantity Validation ------------------------------------
          # Proportion of phase-run rows where prescribed quantity < WHO target
          prop_f100_qty_less = summarise_event(
            df          = monitoring_df,
            id_col      = "id",
            phase_col   = "phase",
            event       = "less",
            target_cols = "f100_qty_validation",
            ids         = id,
            run_order   = run_idx,
            type        = "proportion"
          ),
          prop_f100_qty_less = coalesce(prop_f100_qty_less, 0),
          
          # Proportion of phase-run rows where prescribed quantity > WHO target
          prop_f100_qty_more = summarise_event(
            df          = monitoring_df,
            id_col      = "id",
            phase_col   = "phase",
            event       = "more",
            target_cols = "f100_qty_validation",
            ids         = id,
            run_order   = run_idx,
            type        = "proportion"
          ),
          prop_f100_qty_more = coalesce(prop_f100_qty_more, 0),
          
          # --- F100 Adherence Validation -----------------------------------
          # Proportion of phase-run rows where actual daily intake < WHO target
          prop_f100_adh_less = summarise_event(
            df          = monitoring_df,
            id_col      = "id",
            phase_col   = "phase",
            event       = "less",
            target_cols = "f100_adh_validation",
            ids         = id,
            run_order   = run_idx,
            type        = "proportion"
          ),
          prop_f100_adh_less = coalesce(prop_f100_adh_less, 0),
          
          # Proportion of phase-run rows where actual daily intake > WHO target
          prop_f100_adh_more = summarise_event(
            df          = monitoring_df,
            id_col      = "id",
            phase_col   = "phase",
            event       = "more",
            target_cols = "f100_adh_validation",
            ids         = id,
            run_order   = run_idx,
            type        = "proportion"
          ),
          prop_f100_adh_more = coalesce(prop_f100_adh_more, 0)
          
        ) %>%
        
        rename_with(
          .fn   = ~ paste0(.x, "_p", tolower(phase)),
          .cols = all_of(phaseT_c659_cols)
        )
      
    }
  }
  
  if(cohort == "under6"){
    
    # phaseT_under6_cols <- c(
    #   "prop_f100d_freq_less",
    #   "prop_f100d_freq_more",
    #   "prop_f100d_qty_less",
    #   "prop_f100d_qty_more",
    #   "prop_f100d_adh_less",
    #   "prop_f100d_adh_more"
    # )
    
    df_enriched <- df_enriched %>%
      
      mutate(
        
        # --- Nutritional Treatment Series ------------------------------------
        # Run-length encoded string of products given during run_idx phase run.
        # RUTF attribute appended as "product+rutf" where present.
        nutri_treat_series_under6 = get_matching_series_str(
          df         = monitoring_df,
          id_col     = "id",
          target_col = "typeAlim",
          ref_col    = "phase",
          attr_col   = "rutf",
          # run_order  = run_idx,
          ids        = id
        ),
        
        nutri_treat_under6 = generalize_nutri_treatement(nutri_treat_series_under6),
        
        # --- F100d Frequency Validation -----------------------------------
        # Proportion of phase-run rows where prescribed frequency < WHO target
        prop_f100d_freq_less = summarise_event(
          df          = monitoring_df,
          id_col      = "id",
          phase_col   = "phase",
          event       = "less",
          target_cols = "f100d_freq_validation",
          ids         = id,
          # run_order   = run_idx,
          type        = "proportion"
        ),
        prop_f100d_freq_less = coalesce(prop_f100d_freq_less, 0),
        
        # Proportion of phase-run rows where prescribed frequency > WHO target
        prop_f100d_freq_more = summarise_event(
          df          = monitoring_df,
          id_col      = "id",
          phase_col   = "phase",
          event       = "more",
          target_cols = "f100d_freq_validation",
          ids         = id,
          # run_order   = run_idx,
          type        = "proportion"
        ),
        prop_f100d_freq_more = coalesce(prop_f100d_freq_more, 0),
        
        # --- F100d Quantity Validation ------------------------------------
        # Proportion of phase-run rows where prescribed quantity < WHO target
        prop_f100d_qty_less = summarise_event(
          df          = monitoring_df,
          id_col      = "id",
          phase_col   = "phase",
          event       = "less",
          target_cols = "f100d_qty_validation",
          ids         = id,
          # run_order   = run_idx,
          type        = "proportion"
        ),
        prop_f100d_qty_less = coalesce(prop_f100d_qty_less, 0),
        
        # Proportion of phase-run rows where prescribed quantity > WHO target
        prop_f100d_qty_more = summarise_event(
          df          = monitoring_df,
          id_col      = "id",
          phase_col   = "phase",
          event       = "more",
          target_cols = "f100d_qty_validation",
          ids         = id,
          # run_order   = run_idx,
          type        = "proportion"
        ),
        prop_f100d_qty_more = coalesce(prop_f100d_qty_more, 0),
        
        # --- F100d Adherence Validation -----------------------------------
        # Proportion of phase-run rows where actual daily intake < WHO target
        prop_f100d_adh_less = summarise_event(
          df          = monitoring_df,
          id_col      = "id",
          phase_col   = "phase",
          event       = "less",
          target_cols = "f100d_adh_validation",
          ids         = id,
          # run_order   = run_idx,
          type        = "proportion"
        ),
        prop_f100d_adh_less = coalesce(prop_f100d_adh_less, 0),
        
        # Proportion of phase-run rows where actual daily intake > WHO target
        prop_f100d_adh_more = summarise_event(
          df          = monitoring_df,
          id_col      = "id",
          phase_col   = "phase",
          event       = "more",
          target_cols = "f100d_adh_validation",
          ids         = id,
          # run_order   = run_idx,
          type        = "proportion"
        ),
        prop_f100d_adh_more = coalesce(prop_f100d_adh_more, 0)
        
      )
      
      # rename_with(
      #   .fn   = ~ paste0(.x, "_p", tolower(phase)),
      #   .cols = all_of(phaseT_under6_cols)
      # )
  }
  
  return(df_enriched)
}