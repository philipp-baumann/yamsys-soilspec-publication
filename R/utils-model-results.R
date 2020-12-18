# Cross-validation row selection helper; only extracts model evaluation
# statistics on held-out samples
select_cv_rows <- function(df) {
  df[df$dataType == "Cross-validation", ]
}

# Function to extract predicted vs. observed from a named simplerspec model
# output list (multiple models)
extract_predobs <- function(model_list) {
  predobs <- map(model_list, ~ .[["predobs"]])
  predobs_cv <- map(predobs, ~ select_cv_rows(.))
  predobs_list <- imap(predobs_cv, function(df, nm) {
    m <- rep(nm, nrow(df)); df$model <- m; tibble::as_tibble(df)})
  dplyr::bind_rows(predobs_list)
}

extract_stats <- function(model_list) {
  stats <- map(model_list, ~ .[["stats"]])
  stats_cv <- map(stats, ~ select_cv_rows(.))
  stats_list <- imap(stats_cv, function(df, nm) {
    m <- rep(nm, nrow(df)); df$model <- m; tibble::as_tibble(df)})
   suppressWarnings(dplyr::bind_rows(stats_list))
}