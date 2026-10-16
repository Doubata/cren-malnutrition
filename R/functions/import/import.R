
read_csv_utf8 <- function(path, cols = NULL, guess_max = NULL) {
  
  message(paste("Reading:", path))
  
  readr::read_csv2(
    file = path,
    locale = locale(encoding = "UTF-8"),
    col_select = cols,
    guess_max = guess_max,
    show_col_types = FALSE
  )
  
}