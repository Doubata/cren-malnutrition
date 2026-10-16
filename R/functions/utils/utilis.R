
view_report <- function(target_name) {
  path <- targets::tar_read_raw(target_name)
  if (length(path) > 1) path <- path[1]
  tmp <- file.path(tempdir(), basename(path))
  file.copy(path, tmp, overwrite = TRUE)
  rstudioapi::viewer(tmp)
}
