# =============================================================================
# load_plot_catalog.R — manual loader, only needed if this project does NOT
# use {targets}::tar_source("R"). Order matters: registries before the
# panel_*.R files that register into them.
#
# If using {targets}, skip this file entirely and add tar_source("R") to
# _targets.R instead — it sources every .R file here automatically and
# re-traces dependent targets when any of them change.
# =============================================================================


# source("R/functions/visualization/plot_utils.R")
# source("R/functions/visualization/outcome_resolution.R")
# source("R/functions/config/variable_codebook.R")     # standalone: reads config/*.xlsx, no plotting dependency
# source("R/functions/visualization/geom_registry.R")
# source("R/functions/visualization/chart_builders.R")
# 
# source("R/functions/visualization/panel_categorical.R")   # registers "categorical" into chart_builders
# source("R/functions/visualization/panel_continuous.R")    # registers "continuous" into chart_builders
# # source("R/functions/visualization/panel_ordinal.R")     # add future chart types here, same pattern
# 
# source("R/functions/visualization/plot_catalog.R")
# source("R/functions/visualization/variable_type_detection.R")
# source("R/functions/visualization/auto_catalog.R")          # depends on plot_catalog() and chart_builders
# source("R/functions/visualization/plot_accessors.R")
# source("R/functions/visualization/plot_reports.R")


# Once catalog_p1 is built (via tar_read(catalog_p1) in your Rmd, or directly in console),
# you have a few ways to pull plots out and display them, depending on how much you want to see at once.

# One variable
# rcatalog_p1 <- tar_read(catalog_p1)

# pull_plots(catalog_p1, vars = "prop_whz_error_p1")[[1]]
# A specific subset
# 
# rcompose_plot_grid(
#   pull_plots(catalog_p1, vars = c("gender", "locality", "vaccination_status")),
#   ncol = 3
# )

# By domain or subgroup
# compose_plot_grid(pull_plots(catalog_p1, domain = "morbidity_comorbidities"), ncol = 3)
# compose_plot_grid(pull_plots(catalog_p1, subgroup = "malaria"), ncol = 2)

# Everything, organized by domain → subgroup, in report order
# This is the pattern that ties together domain_display_order() / subgroup_display_order() from earlier — use it in an Rmd chunk with results = 'asis':

# for (d in domain_display_order(domain_registry)) {
#   cat("\n\n## ", domain_label_lookup(domain_registry, d), "\n\n")
#   
#   subgroups <- subgroup_display_order(subgroup_registry, domain = d)
#   
#   if (length(subgroups) == 0) {
#     # domain has no subgroups -- show everything in it as one grid
#     plots <- pull_plots(catalog_p1, domain = d)
#     if (length(plots) > 0) print(compose_plot_grid(plots, ncol = 3))
#   } else {
#     for (sg in subgroups) {
#       plots <- pull_plots(catalog_p1, domain = d, subgroup = sg)
#       if (length(plots) > 0) {
#         cat("\n\n### ", sg, "\n\n")
#         print(compose_plot_grid(plots, ncol = 3))
#       }
#     }
#   }
# }

# Everything, auto-paginated without domain grouping
# If you just want all 20+ P1 baseline plots dumped in batches (from the batching logic in plot_reports.R):

# n <- nrow(catalog_p1)
# batches <- split(seq_len(n), ceiling(seq_len(n) / 9))   # 9 per page, 3x3

# for (b in batches) {
#   print(compose_plot_grid(catalog_p1$plot[b], ncol = 3))
# }

# Quick sanity check before rendering the full report

# nrow(catalog_p1)              # how many plots you actually have
# table(catalog_p1$domain)      # distribution across domains
# table(catalog_p1$chart_type)  # categorical vs continuous split






