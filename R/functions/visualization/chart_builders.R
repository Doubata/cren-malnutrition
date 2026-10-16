# =============================================================================
# chart_builders.R — registry mapping chart_type -> panel-building function.
#
# NOTE: this file only defines the registry and its accessor. Entries are
# added by panel_*.R files calling register_chart_builder() at the bottom of
# their own file (self-registering pattern). Do not add entries directly here
# — add a new panel_*.R file instead.
# =============================================================================

chart_builders <- list()

register_chart_builder <- function(type, fn) {
  chart_builders[[type]] <<- fn
  invisible(NULL)
}
