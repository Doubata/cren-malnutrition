
#' Assign WHO Weight-for-Height Z-Score Category
#'
#' @description
#' For each valid row in \code{df}, classifies the patient's weight-for-height
#' status into a WHO z-score category by comparing their weight against
#' sex- and age-specific WHO reference thresholds. Four lookup tables cover
#' boys and girls aged 0–24 months and 25–60 months respectively. Returns a
#' labelled category per row (e.g. \code{"-3"}, \code{"-2"}, \code{"-1"},
#' \code{"Mediane"}).
#'
#' @param df                 A data frame containing patient-level data.
#' @param weight_col         Column name (quoted or unquoted) for patient weight in kg.
#' @param height_col         Column name (quoted or unquoted) for patient height in cm.
#' @param sex_col            Column name (quoted or unquoted) for patient sex.
#'                           Expected values: \code{"M"} (male) and \code{"F"} (female).
#' @param age_col            Column name (quoted or unquoted) for patient age in months.
#'                           Used to route patients to the correct age-band lookup table
#'                           (0–24 months vs. 25–60 months).
#' @param labels             Character vector of category labels in ascending order,
#'                           corresponding to z-score thresholds
#'                           \code{SD3neg}, \code{SD2neg}, \code{SD1neg}, \code{SD0}.
#'                           Defaults to \code{c("-3", "-2", "-1", "Mediane")}.
#' @param wfh_boys02_lkp_df  WHO WFH z-score lookup table for boys aged 0–24 months.
#'                            Must contain columns \code{Height}, \code{SD3neg},
#'                            \code{SD2neg}, \code{SD1neg}, \code{SD0}.
#' @param wfh_boys25_lkp_df  WHO WFH z-score lookup table for boys aged 25–60 months.
#'                            Same column structure as \code{wfh_boys02_lkp_df}.
#' @param wfh_girls02_lkp_df WHO WFH z-score lookup table for girls aged 0–24 months.
#'                            Same column structure as \code{wfh_boys02_lkp_df}.
#' @param wfh_girls25_lkp_df WHO WFH z-score lookup table for girls aged 25–60 months.
#'                            Same column structure as \code{wfh_boys02_lkp_df}.
#'
#' @return
#' An unnamed character vector the same length as \code{nrow(df)}. Each element
#' is one of the values in \code{labels}, or \code{NA} for rows where:
#' \itemize{
#'   \item Any of \code{weight_col}, \code{height_col}, \code{sex_col}, or
#'         \code{age_col} is \code{NA}.
#'   \item Patient height falls outside the lookup table range.
#'   \item All threshold values in the lookup row are \code{NA}.
#' }
#'
#' @details
#' Processing pipeline:
#' \enumerate{
#'   \item \strong{Symbol capture} — column name arguments accept both quoted
#'         strings and bare (unquoted) names via \code{\link{substitute}()}.
#'   \item \strong{Validation} — delegates to
#'         \code{.check_assign_who_whz_args()}.
#'   \item \strong{Group masks} — valid rows are partitioned into four groups
#'         by sex × age band: boys 0–24m, boys 25–60m, girls 0–24m, girls 25–60m.
#'   \item \strong{Height lookup} — \code{.lookup_who_idx()} maps each
#'         patient's height to the nearest ceiling row in the relevant lookup table.
#'   \item \strong{Threshold matrix} — a \code{[n × 4]} matrix of
#'         \code{SD3neg}, \code{SD2neg}, \code{SD1neg}, \code{SD0} values is
#'         built per group.
#'   \item \strong{Classification} — \code{rowSums(weight >= threshold_matrix)}
#'         counts how many thresholds the patient's weight meets or exceeds.
#'         The count (0–4) is mapped to \code{labels} via
#'         \code{pmin(count + 1, length(labels))}, clamping to the highest label.
#'   \item \strong{NA handling} — rows with missing weight or an all-\code{NA}
#'         threshold row are set to \code{NA} after classification.
#' }
#'
#' @note
#' 
#'
#' @examples
#' assign_who_whz(
#'   df,
#'   weight_col = "weight_kg", height_col = "height_cm",
#'   sex_col    = "sex",       age_col    = "age_months",
#'   wfh_boys02_lkp_df  = who_wfh_boys02,
#'   wfh_boys25_lkp_df  = who_wfh_boys25,
#'   wfh_girls02_lkp_df = who_wfh_girls02,
#'   wfh_girls25_lkp_df = who_wfh_girls25
#' )
#' #> [1] "-3" "-2" "Mediane" NA ...
#'
#' @family WHO assignment
#'
#' @seealso
#' \code{.check_assign_who_whz_args()}, \code{.lookup_who_idx()},
#' \code{\link{assign_who_rutf}}, \code{\link{assign_who_f75x}}
#'
#' @export
assign_who_whz <- function(df, weight_col, height_col, sex_col, age_col,
                           labels = c("-4", "-3", "-2", "-1", "Mediane"),
                           wfh_boys02_lkp_df,  wfh_boys25_lkp_df,
                           wfh_girls02_lkp_df, wfh_girls25_lkp_df) {
  
  # --- 1. Symbol Capture ---------------------------------------------------
  # Allows callers to pass bare column names (unquoted) or quoted strings.
  if (is.symbol(substitute(weight_col))) weight_col <- deparse(substitute(weight_col))
  if (is.symbol(substitute(height_col))) height_col <- deparse(substitute(height_col))
  if (is.symbol(substitute(sex_col)))    sex_col    <- deparse(substitute(sex_col))
  if (is.symbol(substitute(age_col)))    age_col    <- deparse(substitute(age_col))
  
  # --- 2. Validation -------------------------------------------------------
  .check_assign_who_whz_args(df, weight_col, height_col, sex_col, age_col)
  
  # --- 3. Vector Extraction ------------------------------------------------
  # Pre-extract columns for performance — avoids repeated df[[col]] calls
  weight <- df[[weight_col]]
  height <- df[[height_col]]
  sex    <- df[[sex_col]]
  age    <- df[[age_col]]
  
  # --- 4. Initialisation ---------------------------------------------------
  results <- rep(NA, nrow(df))
  
  # --- 5. Valid Row Filter -------------------------------------------------
  # All four measurements must be present for a row to be classified
  is_valid <- !is.na(weight) & !is.na(height) & !is.na(sex) & !is.na(age)
  if (!any(is_valid)) return(results)
  
  # --- 6. Group Masks ------------------------------------------------------
  # Partition valid rows by sex × age band (0-24m vs 25-60m)
  is_boy  <- is_valid & sex == "M"
  is_girl <- is_valid & sex == "F"
  
  boys_0_2  <- is_boy  & age <= 24
  boys_2_5  <- is_boy  & age >  24 & age <= 60
  girls_0_2 <- is_girl & age <= 24
  girls_2_5 <- is_girl & age >  24 & age <= 60
  
  # --- 7. Internal Classification Helper ----------------------------------
  classify_zscore <- function(mask, lkp) {
    if (!any(mask)) return(NULL)
    
    idx <- which(mask)
    
    # Map patient heights to the ceiling row in the lookup table
    h_idx <- .lookup_who_idx(height[idx], lkp[["Height"]])
    
    # Build a [n × 4] threshold matrix: SD3neg, SD2neg, SD1neg, SD0
    val_matrix <- cbind(lkp[["SD4neg"]][h_idx],
                        lkp[["SD3neg"]][h_idx],
                        lkp[["SD2neg"]][h_idx],
                        lkp[["SD1neg"]][h_idx],
                        lkp[["SD0"]][h_idx])
    
    input_vals <- weight[idx]
    
    # Count how many thresholds each patient's weight meets or exceeds (0–4)
    # NA thresholds are treated as not met (na.rm = TRUE)
    thresholds_met <- rowSums(input_vals >= val_matrix, na.rm = TRUE)
    
    # Map count to label, clamping to the highest available label
    label_idx <- pmin(thresholds_met + 1, length(labels))
    z_cat     <- labels[label_idx]
    
    # Set NA where weight is missing or all lookup thresholds are NA
    all_na <- rowSums(!is.na(val_matrix)) == 0
    z_cat[is.na(input_vals) | all_na] <- NA
    
    list(idx = idx, values = z_cat)
  }
  
  # --- 8. Apply Classification ---------------------------------------------
  groups <- list(
    list(mask = boys_0_2,  lkp = wfh_boys02_lkp_df),
    list(mask = boys_2_5,  lkp = wfh_boys25_lkp_df),
    list(mask = girls_0_2, lkp = wfh_girls02_lkp_df),
    list(mask = girls_2_5, lkp = wfh_girls25_lkp_df)
  )
  
  for (g in groups) {
    res <- classify_zscore(g$mask, g$lkp)
    if (!is.null(res)) results[res$idx] <- res$values
  }
  
  return(unname(results))
}
