# =============================================================================
# plot_utils.R — small shared helpers used across the plot catalog system
# =============================================================================

`%||%` <- function(a, b) if (is.null(a)) b else a

# labelled::var_label() returns NULL (not NA/"") for any variable with no
# label attribute. unlist()-ing that list DROPS the NULLs, silently
# shortening the result and misaligning it against `vars` -- this is what
# breaks plot_catalog()'s tibble() call when only some variables are
# labelled. This helper guarantees a same-length, same-order character
# vector.
#
# Resolution order: explicit label_map (e.g. from a variable codebook) >
# the data's own labelled::var_label() attribute > the raw variable name.
.safe_var_label <- function(data, vars, label_map = NULL) {
  purrr::map_chr(vars, function(v) {
    if (!is.null(label_map) && v %in% names(label_map) && !is.na(label_map[[v]])) {
      return(label_map[[v]])
    }
    l <- labelled::var_label(data[[v]])
    if (is.null(l) || is.na(l) || !nzchar(l)) v else l
  })
}
