################################################################################
## Author: Philipp Baumann (philipp.baumann@usys.ethz.ch)
## License: GPL-3.0
## Project: Biophysical, institutional and economic drivers of sustainable 
##   soil use in yam systems for improved food security in West Africa (YAMSYS)
## Description: Develop spectroscopic reference models for the YAMSYS pilot
##   landscapes; apply PLS regression
################################################################################

# Install all required packages (uncomment the lines below if you run this)

# Install the simplerspec package from the github repository;
# Installs all the required packages
# (https://github.com/philipp-baumann/simplerspec)
# devtools::install_github("philipp-baumann/simplerspec")

################################################################################
## Part 1: Read and pre-process spectra, Read chemical data, and join
## spectral and chemical data sets
################################################################################

## Read spectra in list ========================================================

# List of OPUS files obtained from Bruker ALPHA spectrometer at ETH Zürich
lf <- dir("data/spectra", full.names = TRUE)

# Read files: ETH
spc_list <- read_opus_univ(fnames = lf, extract = "spc",
  parallel = TRUE)

## Spectral data processing pipe ===============================================

# ETH Alpha files
spc_tbl <- spc_list %>%
  gather_spc() %>% 
  resample_spc(wn_lower = 500, wn_upper = 3996, wn_interval = 2) %>%
  average_spc() %>%
  preprocess_spc(select = "sg_1_w21") %>%
  select_spc_vars(every = 4)

## Read chemical reference data and join with spectral data ====================

# Read chemical reference analysis data
soilchem_tbl <- read_csv(file = here("data", "soilchem/soilchem_yamsys.csv"))

# Join spectra tibble and chemical reference analysis tibble
spec_chem <- join_spc_chem(
  spc_tbl = spc_tbl, chem_tbl = soilchem_tbl, by = "sample_id")

# Save tibble with spectra and chemical data on disk
# saveRDS(x = spec_chem, path = "out/data/spec_chem.Rds")


################################################################################
## Part 2: Run PLS regression models for all measured soil properties
## Use 5 times repeated 10-fold cross-validation for model tuning and
## use by resampling repeat averaged (mean) held-out predictions at
## finally chosen of PLS regression components for model evaluation;
## argument `resampling_method` = "rep_kfold_cv" creates 5 times repeated
## cross-validation; always exclude missing values for samples that have missing
## values in the target soil property variable
################################################################################

# Default value for <resampling_seed> argument in simplerspec::fit_pls is 
# resampling_seed = 123; this ensures that same resampling indices are created
# and that the model results are always the same when re-running code below


## =============================================================================
## 1: Soil properties in the group related to "Mineralogy":
## Total Fe, total Si, total Al, total K, total Zn, total Cu,
## total Mn
## =============================================================================


## Total element concentration (XRF) ===========================================

# Total Fe
pls_Fe_total <- fit_pls(
  # Remove rows with the NA in the data
  spec_chem = spec_chem[!is.na(spec_chem$Fe_tot), ],
  response = Fe_tot,
  evaluation_method = "resampling",
  tuning_method = "resampling",
  resampling_method = "rep_kfold_cv",
  pls_ncomp_max = 10
)

# Total Si
pls_Si_total <- fit_pls(
  spec_chem = spec_chem[!is.na(spec_chem$Si_tot), ],
  response = Si_tot,
  evaluation_method = "resampling",
  tuning_method = "resampling",
  resampling_method = "rep_kfold_cv",
  pls_ncomp_max = 10
)

# Total Al
pls_Al_total <- fit_pls(
  spec_chem = spec_chem[!is.na(spec_chem$Al_tot), ],
  response = Al_tot,
  evaluation_method = "resampling",
  tuning_method = "resampling",
  resampling_method = "rep_kfold_cv",
  pls_ncomp_max = 10
)

# Total K
pls_K_total <- fit_pls(
  spec_chem = spec_chem[!is.na(spec_chem$K_tot), ],
  response = K_tot,
  evaluation_method = "resampling",
  tuning_method = "resampling",
  resampling_method = "rep_kfold_cv",
  pls_ncomp_max = 10
)

# Total Zn
# remark: default model has only 1 PLS component;
# with increasing PLS component RMSE of cross-validation increases!
# model is not better if removing sample with n tot > 60
pls_Zn_total <- fit_pls(
  spec_chem = spec_chem[!is.na(spec_chem$Zn_tot), ],
  response = Zn_tot,
  evaluation_method = "resampling",
  tuning_method = "resampling",
  resampling_method = "rep_kfold_cv",
  pls_ncomp_max = 10
)

# Total Cu
# Default number of PLS components (caret selection) is 1,
# but cross-validation indicates that 3 components has a slightly lower
# RMSE
pls_Cu_total <- fit_pls(
  spec_chem = spec_chem[!is.na(spec_chem$Cu_tot), ],
  response = Cu_tot,
  evaluation_method = "resampling",
  tuning_method = "resampling",
  resampling_method = "rep_kfold_cv",
  pls_ncomp_max = 10
)

# Total Mn
pls_Mn_total <- fit_pls(
  spec_chem = spec_chem[!is.na(spec_chem$Mn_tot), ],
  response = Mn_tot,
  evaluation_method = "resampling",
  tuning_method = "resampling",
  resampling_method = "rep_kfold_cv",
  pls_ncomp_max = 10
)

# Total Ca
pls_Ca_total <- fit_pls(
  spec_chem = spec_chem[!is.na(spec_chem$Ca_tot), ],
  response = Ca_tot,
  evaluation_method = "resampling",
  tuning_method = "resampling",
  resampling_method = "rep_kfold_cv",
  pls_ncomp_max = 10
)

# Exchangeable K
pls_exch_K <- fit_pls(
  spec_chem = spec_chem[!is.na(spec_chem$ex_K), ],
  response = ex_K,
  evaluation_method = "resampling",
  tuning_method = "resampling",
  resampling_method = "rep_kfold_cv",
  pls_ncomp_max = 10
)

# Exchangeable Ca
# pc = 5 lead to much worse predictions
pls_exch_Ca <- fit_pls(
  spec_chem = spec_chem[!is.na(spec_chem$ex_Ca), ],
  response = ex_Ca,
  evaluation_method = "resampling",
  tuning_method = "resampling",
  resampling_method = "rep_kfold_cv",
  pls_ncomp_max = 10
)

# Exchangeable Mg
pls_exch_Mg <- fit_pls(
  spec_chem = spec_chem[!is.na(spec_chem$ex_Mg), ],
  response = ex_Mg,
  evaluation_method = "resampling",
  tuning_method = "resampling",
  resampling_method = "rep_kfold_cv",
  pls_ncomp_max = 10
)

# Exchangeable Al
pls_exch_Al <- fit_pls(
  spec_chem = spec_chem[!is.na(spec_chem$ex_Al), ],
  response = ex_Al,
  evaluation_method = "resampling",
  tuning_method = "resampling",
  resampling_method = "rep_kfold_cv",
  pls_ncomp_max = 10
)

# CEC_eff
pls_CEC <- fit_pls(
  spec_chem = spec_chem[!is.na(spec_chem$CEC_eff), ],
  response = CEC_eff,
  evaluation_method = "resampling",
  tuning_method = "resampling",
  resampling_method = "rep_kfold_cv",
  pls_ncomp_max = 10
)

# BS_eff
pls_BS <- fit_pls(
  spec_chem = spec_chem[!is.na(spec_chem$BS_eff), ],
  response = BS_eff,
  evaluation_method = "resampling",
  tuning_method = "resampling",
  resampling_method = "rep_kfold_cv",
  pls_ncomp_max = 10
)

# pH
pls_pH <- fit_pls(
  spec_chem = spec_chem[!is.na(spec_chem$pH), ],
  response = pH,
  evaluation_method = "resampling",
  tuning_method = "resampling",
  resampling_method = "rep_kfold_cv",
  pls_ncomp_max = 10
)


## =============================================================================
## 3: Soil properties in the group related to
## "Organic Matter":
## Total C, total N, total S, total P
## =============================================================================

# Total C
pls_C <- fit_pls(
  spec_chem = spec_chem[!is.na(spec_chem$C), ],
  response = C,
  evaluation_method = "resampling",
  tuning_method = "resampling",
  resampling_method = "rep_kfold_cv",
  pls_ncomp_max = 10
)

# Total N
pls_N <- fit_pls(
  spec_chem = spec_chem[!is.na(spec_chem$N), ],
  response = N,
  evaluation_method = "resampling",
  tuning_method = "resampling",
  resampling_method = "rep_kfold_cv",
  pls_ncomp_max = 10
)

# Total S
pls_S <- fit_pls(
  spec_chem = spec_chem[!is.na(spec_chem$S), ],
  response = S,
  evaluation_method = "resampling",
  tuning_method = "resampling",
  resampling_method = "rep_kfold_cv",
  pls_ncomp_max = 10
)

# Total P
pls_P <- fit_pls(
  spec_chem = spec_chem[!is.na(spec_chem$P_tot), ],
  response = P_tot,
  evaluation_method = "resampling",
  tuning_method = "resampling",
  resampling_method = "rep_kfold_cv",
  pls_ncomp_max = 10
)

## =============================================================================
## 4: Soil properties in the group related to
## "Plant Nutrition":
## Resin extractable P, DTPA Fe, DTPA Zn, DTPA Cu, DTPA Mn
## =============================================================================

# Resin-extractable P
# Log-transform gives better prediction
# pls_resin_P_log <- fit_pls(
#   spec_chem = spec_chem[!is.na(spec_chem$P_resin), ],
#   response = log(P_resin),
#   evaluation_method = "resampling",
#   tuning_method = "resampling",
#   resampling_method = "rep_kfold_cv",
#   pls_ncomp_max = 10
# )

## Models for DTPA-extractable micronutrients ==================================

# DTPA-extractable Fe
pls_Fe_DTPA_log <- fit_pls(
  spec_chem = spec_chem[!is.na(spec_chem$Fe_DTPA), ],
  response = log(Fe_DTPA),
  evaluation_method = "resampling",
  tuning_method = "resampling",
  resampling_method = "rep_kfold_cv",
  pls_ncomp_max = 10
)

# DTPA-extractable Zn
# Default optimal number of components is 3 -> might be too low
pls_Zn_DTPA <- fit_pls(
  spec_chem = spec_chem[!is.na(spec_chem$Zn_DTPA), ],
  response = Zn_DTPA,
  evaluation_method = "resampling",
  tuning_method = "resampling",
  resampling_method = "rep_kfold_cv",
  pls_ncomp_max = 10
)

# DTPA-extractable Cu
# Be careful with selection of pc = 8 -> predictions much worse!!!
pls_Cu_DTPA <- fit_pls(
  spec_chem = spec_chem[!is.na(spec_chem$Cu_DTPA), ],
  response = Cu_DTPA,
  evaluation_method = "resampling",
  tuning_method = "resampling",
  resampling_method = "rep_kfold_cv",
  pls_ncomp_max = 10
)

# DTPA-extractable Mn
# Selects per default 2 principal components -> Check how to specify more
# Components
pls_Mn_DTPA <- fit_pls(
  spec_chem = spec_chem[!is.na(spec_chem$Mn_DTPA), ],
  response = Mn_DTPA,
  evaluation_method = "resampling",
  tuning_method = "resampling",
  resampling_method = "rep_kfold_cv",
  pls_ncomp_max = 10
)


## =============================================================================
## 5: Texture models: data are measured by IITA in Cameroon
## =============================================================================

# Model for sand
pls_sand <- fit_pls(
  spec_chem = spec_chem[!is.na(spec_chem$sand), ],
  response = sand,
  evaluation_method = "resampling",
  tuning_method = "resampling",
  resampling_method = "rep_kfold_cv",
  pls_ncomp_max = 10
)

# Model for clay
pls_clay <- fit_pls(
  spec_chem = spec_chem[!is.na(spec_chem$clay), ],
  response = clay,
  evaluation_method = "resampling",
  tuning_method = "resampling",
  resampling_method = "rep_kfold_cv",
  pls_ncomp_max = 10
)

# Model for silt
pls_silt <- fit_pls(
  spec_chem = spec_chem[!is.na(spec_chem$silt), ],
  response = silt,
  evaluation_method = "resampling",
  tuning_method = "resampling",
  resampling_method = "rep_kfold_cv",
  pls_ncomp_max = 10
)


################################################################################
## Write all pls models (R objects) into separate files
## using the saveRDS function
## .Rds files are R data files that contain a single R object, in our case
## it is a list from the output of the fit_pls() function
## These files can be directly loaded into a new R session where e.g. calibrated
## properties are predicted based on new sample spectra
################################################################################

## =============================================================================
## 1: Soil properties in the group related to "Mineralogy":
## Total Fe, total Si, total Al, total K, total Zn, total Cu,
## total Mn
## =============================================================================

pls_Fe_total_rds <- 
  saveRDS(pls_Fe_total, "models/rep-kfold-cv/pls_Fe_total.Rds")
pls_Si_total_rds <-
  saveRDS(pls_Si_total, "models/rep-kfold-cv/pls_Si_total.Rds")
pls_Al_total_rds <-
  saveRDS(pls_Al_total, "models/rep-kfold-cv/pls_Al_total.Rds")
pls_K_total_rds <-
  saveRDS(pls_K_total, "models/rep-kfold-cv/pls_K_total.Rds")
pls_Ca_total_rds <-
  saveRDS(pls_Ca_total, "models/rep-kfold-cv/pls_Ca_total.Rds")
pls_Zn_total_rds <-
  saveRDS(pls_Zn_total, "models/rep-kfold-cv/pls_Zn_total.Rds")
pls_Cu_total_rds <-
  saveRDS(pls_Cu_total, "models/rep-kfold-cv/pls_Cu_total.Rds")
pls_Mn_total <-
  saveRDS(pls_Mn_total, "models/rep-kfold-cv/pls_Mn_total.Rds")

## =============================================================================
## 2: Soil properties in the group related to
## "Mineralogy/Plant Nutrition":
## pH, exchangeable K, exchangeable Ca, exchangeable Mg,
## exchangeable Al, CEC, base saturation
## =============================================================================

pls_pH_rds <- 
  saveRDS(pls_pH, "models/rep-kfold-cv/pls_pH.Rds")
pls_exch_K_rds <-
  saveRDS(pls_exch_K, "models/rep-kfold-cv/pls_exch_K.Rds")
pls_exch_Ca_rds <-
  saveRDS(pls_exch_Ca, "models/rep-kfold-cv/pls_exch_Ca.Rds")
pls_exch_Mg_rds <-
  saveRDS(pls_exch_Mg, "models/rep-kfold-cv/pls_exch_Mg.Rds")
pls_exch_Al_rds <-
  saveRDS(pls_exch_Al, "models/rep-kfold-cv/pls_exch_Al.Rds")
pls_CEC_rds <-
  saveRDS(pls_CEC, "models/rep-kfold-cv/pls_CEC.Rds")
pls_BS_rds <-
  saveRDS(pls_BS, "models/rep-kfold-cv/pls_BS.Rds")

## =============================================================================
## 3: Soil properties in the group related to
## "Organic Matter":
## Total C, total N, total S, total P
## =============================================================================

pls_N_rds <-
  saveRDS(pls_N, "models/rep-kfold-cv/pls_N.Rds")
pls_C_rds <-
  saveRDS(pls_C, "models/rep-kfold-cv/pls_C.Rds")
pls_S_rds <-
  saveRDS(pls_S, "models/rep-kfold-cv/pls_S.Rds")
pls_P_rds <-
  saveRDS(pls_P, "models/rep-kfold-cv/pls_P.Rds")

## =============================================================================
## 4: Soil properties in the group related to
## "Plant Nutrition":
## Resin extractable P, DTPA Fe, DTPA Zn, DTPA Cu, DTPA Mn
## =============================================================================

pls_resin_P_log_rds <-
  saveRDS(pls_resin_P_log, "models/rep-kfold-cv/pls_resin_P_log.Rds")
pls_Fe_DTPA_log_rds <-
  saveRDS(pls_Fe_DTPA_log, "models/rep-kfold-cv/pls_Fe_DTPA_log.Rds")
pls_Zn_DTPA_rds <-
  saveRDS(pls_Zn_DTPA, "models/rep-kfold-cv/pls_Zn_DTPA.Rds")
pls_Cu_DTPA_rds <-
  saveRDS(pls_Cu_DTPA, "models/rep-kfold-cv/pls_Cu_DTPA.Rds")
pls_Mn_DTPA_rds <-
  saveRDS(pls_Mn_DTPA, "models/rep-kfold-cv/pls_Mn_DTPA.Rds")

## =============================================================================
## 5: Texture: Sand, clay, and silt percentage
## =============================================================================

pls_sand_rds <-
  saveRDS(pls_sand, "models/rep-kfold-cv/pls_sand.Rds")
pls_silt_rds <-
  saveRDS(pls_silt, "models/rep-kfold-cv/pls_silt.Rds")
pls_clay_rds <-
  saveRDS(pls_clay, "models/rep-kfold-cv/pls_clay.Rds")

# Check if models have been written
check_models_dir <- dir("models/rep-kfold-cv")

# Check model sizes ($model is an object of class "train" (list))
# object.size(pls_resin_P_log$model)
# ca. 30MB

################################################################################
## Save only caret output (element $model of simplerspec::fit_pls output list)
################################################################################

# This saves disk space when using models only for new predictions

# First save model outputs within a list ---------------------------------------

# Simplerspec model ouput list
simplerspec_mout <- list(
  "pls_Fe_total" = pls_Fe_total,
  "pls_Si_total" = pls_Si_total,
  "pls_Al_total" = pls_Al_total,
  "pls_K_total" = pls_K_total,
  "pls_Ca_total" = pls_Ca_total,
  "pls_Zn_total" = pls_Zn_total,
  "pls_Cu_total" = pls_Cu_total,
  "pls_Mn_total" = pls_Mn_total,
  "pls_pH" = pls_pH,
  "pls_exch_K" = pls_exch_K,
  "pls_exch_Ca" = pls_exch_Ca,
  "pls_exch_Mg" = pls_exch_Mg,
  "pls_exch_Al" = pls_exch_Al,
  "pls_CEC" = pls_CEC,
  "pls_BS" = pls_BS,
  "pls_C" = pls_C,
  "pls_N" = pls_N,
  "pls_S" = pls_S,
  "pls_P" = pls_P,
  "pls_resin_P_log" = pls_resin_P_log,
  "pls_Fe_DTPA_log" = pls_Fe_DTPA_log,
  "pls_Zn_DTPA" = pls_Zn_DTPA,
  "pls_Cu_DTPA" = pls_Cu_DTPA,
  "pls_Mn_DTPA" = pls_Mn_DTPA,
  "pls_sand" = pls_sand,
  "pls_silt" = pls_silt,
  "pls_clay" = pls_clay
)

# Extract only $model (object of class "train" for each model) -----------------

models_train <- map(simplerspec_mout, "model")

# Check size of train list
# object.size(models_train)

# Save train list as Rds
models_train_rds <-
  saveRDS(
    models_train,
    "models/rep-kfold-cv-plsr-caret/pls-models-train-list.Rds")

# Read list of trained models
# models_train <- readRDS(
#  file =  "models/rep-kfold-cv-caret/pls-models-train-list.Rds")
