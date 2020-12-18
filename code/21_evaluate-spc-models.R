################################################################################
## Author: Philipp Baumann (philipp.baumann@usys.ethz.ch)
## License: GPL-3.0
## Project: Biophysical, institutional and economic drivers of sustainable 
##   soil use in yam systems for improved food security in West Africa (YAMSYS)
## Description: Evaluate spectroscopic reference models for the YAMSYS pilot
##   landscapes; PLS regression; Build model evaluation summary 
##   (predicted vs. observed) graphs for models with RPD > 2 (R-squared > 0.75);
##   polish graphs for publication
################################################################################

## Collect predicted vs. observed values 
## from simplerspec modeling output lists ======================================

# Read model outputs -----------------------------------------------------------

# ## 1: Soil properties in the group related to "Mineralogy":
# ## Total Fe, total Si, total Al, total K, total Zn, total Cu,
# ## total Mn
# pls_Fe_total <- readRDS("models/rep-kfold-cv/pls_Fe_total.Rds")
# pls_Si_total <- readRDS("models/rep-kfold-cv/pls_Si_total.Rds")
# pls_Al_total <- readRDS("models/rep-kfold-cv/pls_Al_total.Rds")
# pls_K_total <- readRDS("models/rep-kfold-cv/pls_K_total.Rds")
# pls_Ca_total <- readRDS("models/rep-kfold-cv/pls_Ca_total.Rds")
# pls_Zn_total <- readRDS("models/rep-kfold-cv/pls_Zn_total.Rds")
# pls_Cu_total <- readRDS("models/rep-kfold-cv/pls_Cu_total.Rds")
# pls_Mn_total <- readRDS("models/rep-kfold-cv/pls_Mn_total.Rds")
# ## 2: Soil properties in the group related to
# ## "Mineralogy/Plant Nutrition":
# ## Sand, silt, clay; pH, exchangeable K, exchangeable Ca, exchangeable Mg,
# ## exchangeable Al, CEC, base saturation
# pls_sand <- readRDS("models/rep-kfold-cv/pls_sand.Rds")
# pls_silt <- readRDS("models/rep-kfold-cv/pls_silt.Rds")
# pls_clay <- readRDS("models/rep-kfold-cv/pls_clay.Rds")
# pls_pH <- readRDS("models/rep-kfold-cv/pls_pH.Rds")
# pls_exch_K <- readRDS("models/rep-kfold-cv/pls_exch_K.Rds")
# pls_exch_Ca <- readRDS("models/rep-kfold-cv/pls_exch_Ca.Rds")
# pls_exch_Mg <- readRDS("models/rep-kfold-cv/pls_exch_Mg.Rds")
# pls_exch_Al <- readRDS("models/rep-kfold-cv/pls_exch_Al.Rds")
# pls_CEC <- readRDS("models/rep-kfold-cv/pls_CEC.Rds")
# pls_BS <- readRDS("models/rep-kfold-cv/pls_BS.Rds")
# ## 3: Soil properties in the group related to
# ## "Organic Matter":
# ## Total C, total N, total S, total P
# pls_C <- readRDS("models/rep-kfold-cv/pls_C.Rds")
# pls_N <- readRDS("models/rep-kfold-cv/pls_N.Rds")
# pls_S <- readRDS("models/rep-kfold-cv/pls_S.Rds")
# pls_P <- readRDS("models/rep-kfold-cv/pls_P.Rds")
# ## 4: Soil properties in the group related to
# ## "Plant Nutrition":
# ## Resin extractable P, DTPA Fe, DTPA Zn, DTPA Cu, DTPA Mn
# pls_resin_P_log <- readRDS("models/rep-kfold-cv/pls_resin_P_log.Rds")
# pls_Fe_DTPA_log <- readRDS("models/rep-kfold-cv/pls_Fe_DTPA_log.Rds")
# pls_Zn_DTPA <- readRDS("models/rep-kfold-cv/pls_Zn_DTPA.Rds")
# pls_Cu_DTPA <- readRDS("models/rep-kfold-cv/pls_Cu_DTPA.Rds")
# pls_Mn_DTPA <- readRDS("models/rep-kfold-cv/pls_Mn_DTPA.Rds")

## Nest model output lists in one list
models <- list(
  "pls_Fe_total" = pls_Fe_total,
  "pls_Si_total" = pls_Si_total,
  "pls_Al_total" = pls_Al_total,
  "pls_K_total" = pls_K_total,
  "pls_Ca_total" = pls_Ca_total,
  "pls_Zn_total" = pls_Zn_total,
  "pls_Cu_total" = pls_Cu_total,
  "pls_Mn_total" = pls_Mn_total,
  "pls_sand" = pls_sand,
  "pls_silt" = pls_silt,
  "pls_clay" = pls_clay,
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
  # "pls_resin_P_log" = pls_resin_P_log,
  "pls_Fe_DTPA_log" = pls_Fe_DTPA_log,
  "pls_Zn_DTPA" = pls_Zn_DTPA,
  "pls_Cu_DTPA" = pls_Cu_DTPA,
  "pls_Mn_DTPA" = pls_Mn_DTPA
)

# Extract cross-validated predicted vs. observed values
predobs_cv <- extract_predobs(model_list = models)


# Extract model evaluation statistics for plot annotations =====================

# Extract model evaluation statistics (cross-validation only)
stats_cv_raw <- extract_stats(model_list = models)

stats_cv_csv <- write_csv(x = stats_cv_raw,
  path = here("out", "files", "yamsys-data-model-stats.csv"))

## Select only models for soil properties that have RPD_cv > 2 =================

models_rpd_bigger2 <- stats_cv_raw %>% 
  filter(rpd > 2) %>%
  .$model

stats_cv <- stats_cv_raw %>%
  filter(rpd > 2)

predobs_cv <- predobs_cv %>%
  filter(model %in% models_rpd_bigger2)
# Check filtering
# unique(predobs_cv$model) # worked!

## Reorder factor levels to realize custom panel order in plot =================

# Reorder levels of soil attributes (variable)
var_levels <- c("pls_C",  "pls_N", "pls_S",  "pls_clay", 
  "pls_Fe_total", "pls_Al_total", "pls_Ca_total", "pls_K_total", 
  "pls_exch_Ca", "pls_CEC", "pls_Fe_DTPA_log"
)
predobs_cv$model <- factor(predobs_cv$model, levels = var_levels,
  ordered = TRUE)
stats_cv$model <- factor(stats_cv$model, levels = var_levels,
  ordered = TRUE)


## Modify model evaluation annotation data frame ===============================

annotation <- dplyr::mutate(stats_cv,
  rmse = as.character(as.expression(paste0("RMSE == ",
    ifelse(rmse < 1, round(rmse, 2), round(rmse, 1))))),
  r2 = as.character(as.expression(paste0("italic(R)^2 == ",
    round(r2, 2)))),
  rpd = as.character(as.expression(paste("RPD == ",
    round(rpd, 2)))),
  n = as.character(as.expression(paste0("italic(n) == ", n))),
  ncomp = as.character(as.expression(paste0("ncomp == ",
    ncomp))),
  one_one = "1:1"
)


## Create labeller for panels (soil property and unit) =========================

lbl_eval <- as_labeller(
  x = c(
    `pls_Fe_total` = "Total~Fe~'['*g~kg^-1*']'",
    `pls_Al_total` = "Total~Al~'['*g~kg^-1*']'",
    `pls_K_total` = "Total~K~'['*g~kg^-1*']'",
    `pls_Ca_total` = "Total~Ca~'['*g~kg^-1*']'",
    `pls_C` = "Total~C~'['*g~kg^-1*']'",
    `pls_N` = "Total~N~'['*g~kg^-1*']'",
    `pls_S` = "Total~S~'['*g~kg^-1*']'",
    `pls_exch_Ca` = "Ca~(exch.)~'['*mg~kg^-1*']'",
    `pls_CEC` = "CEC[eff]~'[cmol(+)'~kg^-1*']'",
    `pls_Fe_DTPA_log` = "Fe~(DTPA)~'['*mg~kg^-1*']'",
    `pls_clay` = "Clay~'[%]'"
    ),
  default = label_parsed
)

## Plot cross-validation model evaluation graphs ===============================


p_model <- ggplot(data = predobs_cv) +
  geom_point(ggplot2::aes(x = obs, y = pred),
    shape = 1, size = 2, alpha = 1/2, data = predobs_cv) +
  facet_wrap(~ model, scales = "free", ncol = 4, labeller = lbl_eval) +
  geom_text(data = annotation,
    aes(x = Inf, y = -Inf, label = ncomp), size = 3,
    hjust = 1.15, vjust = -4.5, parse = TRUE) + # !!! additional label
  geom_text(data = annotation,
    aes(x = Inf, y = -Inf, label = r2), size = 3,
    hjust = 1.15, vjust = -3, parse = TRUE) +
  geom_text(data = annotation,
    aes(x = Inf, y = -Inf, label = rmse), size = 3,
    hjust = 1.15, vjust = -1.25, parse = TRUE) +
  geom_text(data = annotation,
    ggplot2::aes(x = Inf, y = -Inf, label = rpd), size = 3,
    hjust = 1.12, vjust = -2.5, parse = TRUE) +
  ggplot2::geom_errorbar(aes(x = obs, ymin = pred - pred_sem_ci,
    ymax = pred + pred_sem_ci),
    width = 0, data = predobs_cv, inherit.aes = FALSE) +
  theme_bw() +
  geom_abline(col = "red") +
  geom_text(data = annotation,
    aes(x = Inf, y = Inf, label = one_one), size = 3,
    hjust = 2.5, vjust = 2, colour = "red") +
  xlab("Measured") +
  ylab("Predicted") +
  theme(strip.background = element_rect(fill = "white"))

## Save graph to disk
p_model_pdf <- ggsave(filename = "evaluation-accurate-models.pdf",
  plot = p_model, path = "out/figs", width = 9, height = 7)
## Save a version for the publication
p_model_pdf_pub <- ggsave(filename = "Figure3.pdf", plot = p_model,
  path = "manuscript/figs", width = 9, height = 7)


## Create table with summary of model evaluation ===============================

# Rename soil property names in response column data_model_summary
soil_attributes <- c(
  "C" = "Total C [\\unit{}{g\\,kg$^{-1}$}]",
  "N" = "Total N [\\unit{}{g\\,kg$^{-1}$}]",
  "S" = "Total S [\\unit{}{mg\\,kg$^{-1}$}]",
  "sand" = "Sand [\\%]",
  "silt" = "Silt [\\%]",
  "clay" = "Clay [\\%]",
  "P_tot" =  "Total P [\\unit{}{mg\\,kg$^{-1}$}]",
  "Fe_tot" = "Total Fe [\\unit{}{g\\,kg$^{-1}$}]",
  "Al_tot" = "Total Al [\\unit{}{g\\,kg$^{-1}$}]",
  "Si_tot" = "Total Si [\\unit{}{g\\,kg$^{-1}$}]",
  "Ca_tot" = "Total Ca [\\unit{}{g\\,kg$^{-1}$}]",
  "Zn_tot" = "Total Zn [\\unit{}{mg\\,kg$^{-1}$}]",
  "Cu_tot" = "Total Cu [\\unit{}{mg\\,kg$^{-1}$}]",
  "K_tot" = "Total K [\\unit{}{g\\,kg$^{-1}$}]",
  "Mn_tot" = "Total Mn [\\unit{}{mg\\,kg$^{-1}$}]",
  # "log(P_resin)" = "log(P resin) [\\unit{}{mg\\,kg$^{-1}$}]",
  "log(Fe_DTPA)" = "log(Fe(DTPA)) [\\unit{}{mg\\,kg$^{-1}$}]",
  "Zn_DTPA" = "Zn\\,(DTPA) [\\unit{}{mg\\,kg$^{-1}$}]",
  "Cu_DTPA" = "Cu\\,(DTPA) [\\unit{}{mg\\,kg$^{-1}$}]",
  "Mn_DTPA" = "Mn\\,(DTPA) [\\unit{}{mg\\,kg$^{-1}$}]",
  "pH" = "pH\\textsubscript{H\\textsubscript{2}0}",
  "ex_Ca" = "Ca (exch.) [\\unit{}{mg\\,kg$^{-1}$}]",
  "ex_Mg" = "Mg (exch.) [\\unit{}{mg\\,kg$^{-1}$}]",
  "ex_K" = "K (exch.) [\\unit{}{mg\\,kg$^{-1}$}]",
  "ex_Al" = "Al (exch.) [\\unit{}{mg\\,kg$^{-1}$}]",
  "CEC_eff" = "\\textsc{cec}\\textsubscript{eff} [\\unit{}{cmol(+)\\,kg$^{-1}$}]",
  "BS_eff" = "BS\\textsubscript{eff} [\\%]"
)
# Selection of columns in data_model_summary
model_summary_vars <- c("response", "n",
  "min_obs", "max_obs", "median_obs", "mean_obs",
  "CV", "dataType", "ncomp", "rmse", "r2", "rpd")

# data_model_summary <- read_csv(
#   file = here::here("out", "files", "yamsys-data-model-stats.csv"))

  # omit: # "SB_prop", "NU_prop", "LC_prop"
# Select only set of evaluation statistics for cross-validation
model_summary_unoredered <- stats_cv_raw[, model_summary_vars] %>%
  filter(dataType == "Cross-validation") %>%
  select(- dataType)

model_summary_unoredered$response <- factor(model_summary_unoredered$response, 
  levels = names(soil_attributes), ordered = TRUE)

# Order rows by factor level of values of response column
model_summary <-
  model_summary_unoredered[order(model_summary_unoredered$response), ]

# Revalue with new soil attribute names
model_summary$response <- plyr::revalue(
  model_summary$response, soil_attributes
)
# Reset column names for publication table
colnames(model_summary) <- c("Soil attribute", "$n$",
  "Min\\textsubscript{meas.}", "Max\\textsubscript{meas.}",
  "Med\\textsubscript{meas.}", "Mean\\textsubscript{meas.}",
  "\\textsc{cv}\\textsubscript{meas.}", "ncomp",
  "\\textsc{rmse}\\textsubscript{cv}",
  "$R^2\\textsubscript{cv}$", "\\textsc{rpd}\\textsubscript{cv}")
# Set number of digits per column and row
# Specifiy digits for each row and column
# https://stackoverflow.com/questions/14404241/set-digits-row-wise-with-print-xtable-in-r
mdat <- matrix(
  c(0, 0, 0, 1, 1, 1, 1, 0, 0, 1, 2, 1), # use recycling
  nrow = nrow(model_summary), ncol = ncol(model_summary) + 1,
  byrow = TRUE # change values
)
## Some more custom formatting specific for soil attributes (rows)
# Total Fe, Total Si, Total Al, Total K, Total Zn, Total Cu, Total Mn
mdat[8:10, c(4:7, 10)] <- rep(0, 5)
# Total C
mdat[c(1, 4:6), c(4:7, 10)] <- rep(1, 5)
# Total N
mdat[2, c(4:7, 10)] <- rep(2, 5)
# K (exch.), Ca (exch.), Mg (exch.), Al (exch.), BS_eff
mdat[c(21:24, 26), c(4:7, 10)] <- rep(0, 5)
# CEC_eff
mdat[25, c(4:7, 10)] <- rep(1, 5)
# Total S, Total P
mdat[c(3, 7), c(4:7, 10)] <- rep(0, 5)

model_summary_xtable <- print(
  xtable(x = model_summary, digits = mdat,
    align = c("l", "l", rep("c", ncol(model_summary) - 1))),
  type = "latex", # this uses tabular environment
  # file = here("out", "tables", "spc-model-eval.tex"),
  # tabular.environment = "tabularx",
  include.rownames = FALSE,
  sanitize.colnames.function = identity,
  sanitize.text.function = identity,
  # http://christophj.github.io/replicating/r/how-to-produce-nice-tables-in-pdfs-using-knitr-sweave-and-r/
  hline.after = NULL, # see link above
  booktabs = TRUE,
  add.to.row = list(
    pos = list(-1, 0, nrow(model_summary)),
    command = c("\\tophline{}", "\\middlehline{}", "\\bottomhline{}")
  )
)