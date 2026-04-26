
#' Get Row Indices of a Specific Run in a Vector
#'
#' @description
#' Applies run-length encoding to \code{value_col} and returns the row indices
#' (start:end) corresponding to the \code{run_order}-th consecutive run.
#'
#' @param value_col A vector to run-length encode.
#' @param run_order Positive integer. Which run to locate
#'                  (1 = first run, 2 = second run, etc.).
#'
#' @return
#' An integer vector of row indices for the \code{run_order}-th run, or
#' \code{NA} if \code{value_col} is empty or \code{run_order} exceeds the
#' total number of runs.
#'
#' @seealso \code{\link{get_matching_series_str}}, \code{\link{rle}}
#'
#' @keywords internal
.get_series_idx <- function(value_col, run_order) {
  
  # Guard: empty or all-NA input → NA
  if (length(value_col) == 0) return(NA)
  
  runs <- rle(as.character(value_col))
  
  # Guard: run_order exceeds number of runs → NA
  if (run_order > length(runs$values)) return(NA)
  
  # Derive start / end positions via cumulative lengths
  ends   <- cumsum(runs$lengths)           # end index of each run
  starts <- c(1, head(ends, -1) + 1)      # start index of each run
  
  return(starts[run_order]:ends[run_order])
}



#' Split Target Column Values into a Named List by ID
#'
#' @description
#' Subsets \code{df} by \code{id_col} and returns a named list of
#' \code{val_col} values keyed by ID. When \code{attr_col} is provided,
#' values are combined with their attribute as \code{"value+attr"}, enabling
#' downstream callers to carry both pieces of information in a single vector.
#' \code{NA} values in either column are replaced with the sentinel
#' \code{"MISSING"} before splitting to prevent \code{\link{split}()} from
#' silently dropping \code{NA} groups.
#'
#' @param df      A data frame containing the columns to split.
#' @param val_col Character. Name of the primary value column to extract.
#' @param id_col  Character. Name of the grouping ID column.
#' @param ...     Reserved for future use.
#' @param attr_col Optional character. Name of an attribute column to append
#'                 to each value as \code{"value+attr"}. Defaults to \code{NULL}.
#'
#' @return
#' A named list where each element is a character vector of values (or
#' combined \code{"value+attr"} strings) for one ID group.
#'
#' @note
#' Both \code{val_col} and \code{attr_col} are coerced to character before
#' splitting. The \code{"MISSING"} sentinel ensures \code{NA} positions
#' are preserved as a distinct run value in downstream \code{\link{rle}()}
#' calls.
#'
#' @seealso \code{\link{get_series_str}}, \code{\link{get_matching_series_str}},
#'   \code{\link{count_by_id}}
#'
#' @keywords internal
.get_values_by_id <- function(df, val_col, id_col, ..., attr_col = NULL) {
  
  # Coerce to character and sentinel-fill NAs to preserve run positions
  df[[val_col]] <- as.character(df[[val_col]])
  df[is.na(df[[val_col]]), val_col] <- "MISSING"
  
  id_factor <- as.character(df[[id_col]])
  
  if (is.null(attr_col)) {
    # Simple split: target column only
    values_list <- split(df[[val_col]], id_factor)
    
  } else {
    # Combine target + attribute into a single "value+attr" column,
    # keeping the plain value when attr is MISSING (i.e. was originally NA)
    df[[attr_col]] <- as.character(df[[attr_col]])
    df[is.na(df[[attr_col]]), attr_col] <- "MISSING"
    
    df[[new_val_col]] <- ifelse(
      df[[attr_col]] == "MISSING",
      df[[val_col]],
      paste0(df[[val_col]], "+", df[[attr_col]])
    )
    
    values_list <- split(df[[new_val_col]], id_factor)
  }
  
  return(values_list)
}



#' Map Patient Weights to WHO Lookup Table Row Indices
#'
#' @description
#' For each value in \code{vals}, finds the index of the nearest ceiling row
#' in the sorted \code{weights} reference vector using
#' \code{\link{findInterval}()}. Values outside the lookup range are returned
#' as \code{NA}.
#'
#' @param vals    Numeric vector of patient weights to look up.
#' @param weights Numeric vector of reference weight breakpoints from the
#'                WHO lookup table (need not be pre-sorted).
#'
#' @return
#' An integer vector the same length as \code{vals}. Each element is the
#' ceiling row index into \code{weights}, or \code{NA} for values that are
#' \code{NA}, below \code{min(weights)}, or above \code{max(weights)}.
#'
#' @details
#' A small offset (\code{1e-8}) is subtracted from each value before calling
#' \code{\link{findInterval}()}, then 1 is added to the result. This ensures
#' that a value exactly equal to a breakpoint is assigned to that breakpoint's
#' row (ceiling behaviour) rather than the row below it.
#'
#' @seealso \code{\link{assign_who_rutf}}, \code{\link{assign_who_f75x}},
#'   \code{\link{findInterval}}
#'
#' @keywords internal
.lookup_who_idx <- function(vals, weights) {
  
  # Guard: empty reference table → all NA
  if (length(weights) == 0) return(rep(NA, length(vals)))
  
  sorted_weights <- sort(round(weights, 3))
  
  # Subtract tiny offset before findInterval to achieve ceiling behaviour,
  # then +1 shifts the result from "interval below" to "ceiling row"
  idx <- findInterval(round(vals, 3) - 1e-8, sorted_weights) + 1
  
  # Mark out-of-range values as NA (findInterval returns 0 for below-min,
  # which becomes 1 after +1 — indistinguishable without this guard)
  out_of_bounds <- is.na(vals) | vals < min(weights) | vals > max(weights)
  idx[out_of_bounds] <- NA
  
  return(idx)
}



#' Map Values to WHO F100d Lookup Table Row Indices
#'
#' @description
#' For each value in \code{values}, finds the matching row index in the WHO
#' F100d lookup table by testing whether the value falls within each row's
#' \code{[weight_min, weight_max]} interval. Values outside all intervals are
#' returned as \code{NA}.
#'
#' @param values      Numeric vector of patient weights to look up.
#' @param weight_mins Numeric vector of lower weight bounds from the lookup table.
#' @param weight_maxs Numeric vector of upper weight bounds from the lookup table.
#'
#' @return
#' An integer vector the same length as \code{values}. Each element is the
#' matched row index, or \code{NA} for values below \code{min(weight_mins)},
#' above the corresponding upper bound, or \code{NA} in \code{values}.
#'
#' @note
#' Unlike \code{.lookup_who_idx()} which uses a single sorted breakpoint
#' vector, this helper matches against explicit lower/upper bound pairs —
#' necessary for the F100d tables which define non-overlapping weight bands
#' rather than single breakpoints.
#'
#' @seealso \code{\link{assign_who_f100d_qty}}, \code{.lookup_who_idx()},
#'   \code{\link{findInterval}}
#'
#' @keywords internal
.lookup_who_f100d_idx <- function(values, weight_mins, weight_maxs) {
  
  # Guard: empty input → all NA
  if (length(values) == 0) return(rep(NA_integer_, length(values)))
  
  # findInterval locates the lower-bound interval; returns 0 if below minimum
  idx <- findInterval(values, weight_mins)
  
  # Zero means below minimum weight → NA
  idx[idx == 0] <- NA_integer_
  
  # Validate that value also falls within the matched row's upper bound
  valid      <- !is.na(idx) & values <= weight_maxs[idx]
  idx[!valid] <- NA_integer_
  
  return(idx)
}



#' Compute Daily Food Intake per Patient from Meal Records
#'
#' @description
#' Converts a matrix of categorical meal consumption codes into a numeric
#' daily intake (ml) per patient by mapping each code to a proportion
#' multiplier, then summing \code{multiplier × food_qty} across all meals.
#'
#' @param food_qty     Numeric vector of prescribed quantity per feed (ml/feed),
#'                     one value per patient. Must have the same length as
#'                     \code{nrow(meals_matrix)}.
#' @param meals_matrix A character matrix of meal consumption codes
#'                     (\code{nrow = patients}, \code{ncol = meals}). Recognised
#'                     codes and their multipliers:
#'   \itemize{
#'     \item \code{"1"}     → 1.00 (full portion)
#'     \item \code{"3_4"}   → 0.75 (three-quarters)
#'     \item \code{"1_2"}   → 0.50 (half)
#'     \item \code{"1_4"}   → 0.25 (quarter)
#'     \item \code{"refus"} → 0.00 (refused)
#'     \item \code{"sonde"} → 1.00 (tube-fed, full)
#'     \item \code{"vomit"} → 0.00 (vomited)
#'   }
#'
#' @return
#' A numeric vector the same length as \code{food_qty}. Each element is the
#' total daily intake (ml) for that patient, computed as
#' \code{sum(multipliers × food_qty)} across all meals. Returns \code{NA} for
#' rows where all meal codes are unrecognised or \code{NA}.
#'
#' @details
#' Processing pipeline:
#' \enumerate{
#'   \item Validates that \code{length(food_qty) == nrow(meals_matrix)}.
#'   \item Maps every cell of \code{meals_matrix} to its multiplier via
#'         vectorised named-vector lookup.
#'   \item Reshapes the flat lookup result back to the original matrix
#'         dimensions.
#'   \item Multiplies each row of the weight matrix by \code{food_qty[i]}.
#'   \item Sums across columns with \code{\link{rowSums}(na.rm = TRUE)};
#'         rows where all weights were \code{NA} (unrecognised codes) are
#'         set to \code{NA}.
#' }
#'
#' @seealso \code{\link{validate_f75x_adh}}, \code{\link{rowSums}}
#'
#' @keywords internal
.get_daily_intake_batch <- function(food_qty, meals_matrix) {
  
  # Guard: row count must match qty vector length
  if (length(food_qty) != nrow(meals_matrix)) {
    stop("Mismatch: 'food_qty' must have the same length as the number of rows in 'meals_matrix'.")
  }
  
  # Proportion multiplier lookup: categorical code → fraction of full portion
  multipliers <- c(
    "1"     = 1.00,
    "3_4"   = 0.75,
    "1_2"   = 0.50,
    "1_4"   = 0.25,
    "refus" = 0.00,
    "sonde" = 1.00,
    "vomit" = 0.00
  )
  
  # Vectorised lookup: flatten matrix → map codes → reshape to original dims
  weights_vec    <- multipliers[meals_matrix]
  weights_matrix <- matrix(weights_vec,
                           nrow = nrow(meals_matrix),
                           ncol = ncol(meals_matrix))
  
  # Row-wise: multiply each meal weight by the prescribed quantity for that patient
  intake_matrix <- weights_matrix * food_qty
  
  results <- rowSums(intake_matrix, na.rm = TRUE)
  
  # Rows where all codes were unrecognised → no valid intake data → NA
  all_na_rows        <- rowSums(!is.na(weights_matrix)) == 0
  results[all_na_rows] <- NA
  
  return(results)
}

