# --- paste into your R console, from the dashboard project root ---
source("global.R")

target <- "cohort_p1_data_c659_noedema"   # <- the cohort/pathway you were on
domain <- "sociodemographic"               # <- the domain code you selected

df <- load_target_data(target)
cat("class of returned object:", class(df), "\n")
cat("is data.frame:", is.data.frame(df), "\n")
cat("n columns:", if (is.data.frame(df)) ncol(df) else NA, "\n\n")

codebook_vars <- CODEBOOK |> dplyr::filter(domain == !!domain) |> dplyr::pull(variable) |> unique()
cat("Variables the codebook expects for this domain:\n")
print(codebook_vars)

cat("\nColumns actually present in your real data:\n")
print(names(df))

cat("\nMissing (in codebook, not in your data):\n")
print(setdiff(codebook_vars, names(df)))

# --- paste into your R console, from the dashboard project root ---
source("global.R")

target <- "cohort_p1_data_c659_noedema"   # <- the cohort/pathway you were on
domain <- "sociodemographic"               # <- the domain code you selected

df <- load_target_data(target)
cat("class of returned object:", class(df), "\n")
cat("is data.frame:", is.data.frame(df), "\n")
cat("n columns:", if (is.data.frame(df)) ncol(df) else NA, "\n\n")

codebook_vars <- CODEBOOK |> dplyr::filter(domain == !!domain) |> dplyr::pull(variable) |> unique()
cat("Variables the codebook expects for this domain:\n")
print(codebook_vars)

cat("\nColumns actually present in your real data:\n")
print(names(df))

cat("\nMissing (in codebook, not in your data):\n")
print(setdiff(codebook_vars, names(df)))

