
library(skimr)
df_list <- list(p1_data_c659_noedema, pt_data_c659_noedema,
                p1_data_c659_oedema, pt_data_c659_oedema,
                under6_data)

names(df_list) <- c(
  "p1_data_c659_noedema", 
  "pt_data_c659_noedema",
  "p1_data_c659_edema", 
  "pt_data_c659_edema",
  "under6_data"
)

lapply(df_list, skim)

# Safe export using capture.output instead of sink
output_text <- capture.output({
  for (name in names(df_list)) {
    cat("========================================\n")
    cat("SUMMARY FOR DATAFRAME:", name, "\n")
    cat("========================================\n\n")
    print(skimr::skim(df_list[[name]]))
    cat("\n\n")
  }
})

# Write directly to text file
writeLines(output_text, "C:/Users/DELL/Documents/CREN/crenMalnutrition/output/tables/dataframe_summaries.txt")

### 1
library(targets)
library(dplyr)

# Extract the network data
net <- tar_network()

# 1. View all targets, their types, and status
net$nodes |> 
  select(name, type, status, seconds)

# 2. View relationships (Who depends on what)
net$edges |> 
  select(from, to)

write.csv(net$edges, "C:/Users/DELL/Documents/CREN/crenMalnutrition/output/tables/tar_network.txt", row.names = FALSE)

### 2

library(targets)
library(dplyr)
library(purrr)
library(tibble)

# targets::tar_meta() %>%
#   select(name, command, seconds, bytes, format)

targets_to_keep <- c(
  "p1_data_c659_noedema", 
  "pt_data_c659_noedema",
  "p1_data_c659_edema", 
  "pt_data_c659_edema",
  "under6_data"
)

target_names <- targets::tar_manifest()$name

target_names_sub <- target_names[target_names %in% targets_to_keep]
print(target_names_sub)

# Final target inventory
inventory <- map_dfr(target_names_sub, function(x) {
  dat <- targets::tar_read_raw(x) # or targets::tar_read(!!rlang::sym(x))
  
  # Use simple character variable x:
  dat <- targets::tar_read_raw(x)
  
  tibble(
    target = x,
    class = class(dat)[1],
    n_rows = ifelse(is.data.frame(dat), nrow(dat), NA_integer_),
    n_cols = ifelse(is.data.frame(dat), ncol(dat), NA_integer_),
    columns = ifelse(is.data.frame(dat), paste(names(dat), collapse = "; "), NA_character_)
  )
})
write.csv(inventory, "C:/Users/DELL/Documents/CREN/crenMalnutrition/output/tables/targets_dataframe_inventory.csv", row.names = FALSE)



# Variable structure of each final dataframe
variable_inventory <- map_dfr(target_names_sub, function(x) {
  
  dat <- targets::tar_read_raw(x)
  
  tibble(
    target = x,
    variable = names(dat),
    class = map_chr(dat, ~ class(.x)[1]),
    n_missing = map_int(dat, ~ sum(is.na(.x))),
    n_unique = map_int(dat, ~ dplyr::n_distinct(.x))
  )
})

write.csv(
  variable_inventory,
  "C:/Users/DELL/Documents/CREN/crenMalnutrition/output/tables/targets_variable_inventory.csv",
  row.names = FALSE
)







