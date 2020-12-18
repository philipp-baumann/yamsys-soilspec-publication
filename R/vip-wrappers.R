# Extract VIP from multiple models at final number of components
# uses extract_pls_vip from simplerspec
extract_multi_pls_vip <- function(mout_list) {
  vip_df_list <- lapply(mout_list, simplerspec::extract_pls_vip)
  name_list <- names(mout_list)
  nrow_list <- lapply(vip_df_list, nrow)
  model <- flatten_chr(map2(name_list, nrow_list, function(nm, i) rep(nm, i)))
  vip_df <- dplyr::bind_rows(vip_df_list)
  tibble::add_column(vip_df, model = model) 
}

# Extract PLS regression coefficients from single model at final number of 
# components
extract_pls_coef <- function(mout) {
  final_model <- mout$model$finalModel
  ncomp <- mout$model$finalModel$ncomp
  coef <- final_model$coefficients[, , ncomp] # coef is an 3D array
  wn <- as.numeric(colnames(mout$data$calibration$spc_pre[[1]]))
  tibble::tibble(
    wavenumber = wn,
    coef = coef
  )
}

# Extract PLS regression coefficients from multiple models at respective
# final number of components
extract_multi_pls_coef <- function(mout_list) {
  coef_df_list <- lapply(mout_list, extract_pls_coef)
  name_list <- names(mout_list)
  nrow_list <- lapply(coef_df_list, nrow)
  model <- flatten_chr(map2(name_list, nrow_list, function(nm, i) rep(nm, i)))
  coef_df <- dplyr::bind_rows(coef_df_list)
  tibble::add_column(coef_df, model = model)
}

# Combine VIP and PLS regression coefficients for multiple models
extract_multi_pls_vip_coef <- function(mout_list) {
  vip_df <- extract_multi_pls_vip(mout_list = mout_list)
  coef_df <- extract_multi_pls_coef(mout_list = mout_list)
  dplyr::inner_join(x = vip_df, y = coef_df, by = c("wavenumber", "model"))
}