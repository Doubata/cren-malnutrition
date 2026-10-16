
# 1. Helper function to clean and compress the treatment string
clean_treat_series <- function(x) {
  
  map_chr(x, function(str) {
    if (is.na(str) || str == "") return(NA_character_)
    
    # 1. Split string by hyphens into individual tokens
    tokens <- str_split(str, "-")[[1]]
    
    # 2. Filter out any MISSING or empty elements
    tokens <- tokens[!str_detect(tokens, "MISSING") & tokens != ""]
    
    if (length(tokens) == 0) return("MISSING")
    
    # 3. Collapse consecutive identical tokens (e.g., c("F75", "F75") -> "F75")
    tokens <- rle(tokens)$values
    
    # 4. Rejoin with single hyphens
    paste(tokens, collapse = "-")
  })
  
  # x %>%
  #   # 1. Replace MISSING variants with a placeholder hyphen so tokens don't merge
  #   str_replace_all("MISSING(\\+RUTF)?", "-") %>%
  #   
  #   # 2. Collapse multiple hyphens down to one
  #   str_replace_all("-+", "-") %>%
  #   
  #   # 3. Strip leading and trailing hyphens
  #   str_remove("^-") %>%
  #   str_remove("-$") %>%
  #   
  #   # 4. Iteratively remove adjacent duplicated tokens separated by hyphens
  #   # Fixes both standard tokens (F75) and tokens with '+' (F75+RUTF)
  #   str_replace_all("([A-Za-z0-9\\+]+)(-\\1)+", "\\1") %>%
  #   
  #   # Second pass for nested/overlapping repeat cases
  #   str_replace_all("([A-Za-z0-9\\+]+)(-\\1)+", "\\1")
  
}
