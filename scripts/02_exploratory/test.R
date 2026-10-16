files <- list.files("R/functions/", pattern = "\\.[Rr]$", full.names = TRUE, recursive = TRUE)
lapply(files, source)
library(checkmate)

test_data <- monitoring_clean %>% 
  left_join(child_clean |> select(id, sexe, ageMois, allait), by = "id") %>%
  # Ordering
  arrange(id, dateVis)


test_data$who_f75x_freq <- assign_who_f75x(
  test_data, "poids", "typeAlim", "oedemes", "mlRepas",
  type_food = c("F75", "F75F"),
  measure_type = "frequency",
  who_f75_data = who_std_raw[["F75"]],
  who_f75_oed3_data = who_std_raw[["F75_oed3"]]
)

who_f75x_qty  = assign_who_f75x(
  ., "poids", "typeAlim", "oedemes", "nRepJr",
  type_food = c("F75", "F75F") ,
  measure_type = "quantity",
  f75_lkp_df = who_std_raw[["F75"]],
  f75_oed3_lkp_df = who_std_raw[["F75_oed3"]]
)
  

summary(test_data$who_f75x_freq)
summary(test_data$who_f75x_qty)
table(test_data$typeAlim)


