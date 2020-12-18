################################################################################
## Author: Philipp Baumann (philipp.baumann@usys.ethz.ch)
## License: GPL-3.0
## Project: Biophysical, institutional and economic drivers of sustainable 
##   soil use in yam systems for improved food security in West Africa (YAMSYS)
## Description: Aggregate chemical reference analysis data prior to
##   developing spectroscopic reference models for the YAMSYS pilot
##   landscapes
################################################################################

## (01) Read and transform pH data =============================================

dat_pH_raw <- read_csv(file = "data/soilchem/data-raw/data-pH.csv")

# Calculate mean per sample_ID for repetitions
dat_pH <- dat_pH_raw %>%
  group_by(sample_ID, country) %>%
  summarize(pH = mean(pH))

## (02) Read and transform total carbon (C), nitrogen (N) and sulfur (S) data ==

# Read CNS data for soils
dat_CNS_raw <- read_csv(file = "data/soilchem/data-raw/data-cns-soil.csv")

# Calculate mean of N, C, and S
dat_CNS <- dat_CNS_raw %>% 
  group_by(sample_ID, country, site, material) %>%
  summarize(
    N_perc = mean(N_perc),
    C_perc = mean(C_perc),
    S_perc = mean(S_perc)
  )

# Check standard deviations of repetition samples (only 2 samples are repeated
# per landscape)
dat_CNS_raw %>% 
  group_by(sample_ID) %>% 
  summarize(
    N_perc = sd(N_perc),
    C_perc = sd(C_perc),
    S_perc = sd(S_perc)
  )

# Convert numbers in percent to g and mg per kg soil
# Express S_perc (percent sulfur) in mg sulfur per kg soil
dat_CNS$S <- dat_CNS$S_perc / 100 * 1000 * 1000
# Express C in g C per kg soil
dat_CNS$C <- dat_CNS$C_perc * 10
# Express N in g C per kg soil
dat_CNS$N <- dat_CNS$N_perc * 10

## (03) Read and transform resin phosphorus (P) data ===========================

# Read data for resin P --------------------------------------------------------

# Series 1: No dilution
dat_resin_P1_raw <- read_csv("data/soilchem/data-raw/data-resin-P-cal1.csv")
# Series 2: 1:1 and 1:3 dilution
dat_resin_P2_raw <- read_csv("data/soilchem/data-raw/data-resin-P-cal2.csv")
# Series 2: 1:10 dilution (2 samples; measured by Eva)
dat_resin_P3_raw <- read_csv("data/soilchem/data-raw/data-resin-P-cal3.csv")


# Calculate resin P values for samples from series 1 (no dilution) -------------

# Function to calculate resin P in mg P per kg soil 
cal_P <- function(df, P_stnd, h2o, m1, m2, abs) {
  df_select <- subset(df, site == "cal1")
  # calculate in mu g P / cuvette
  df_select <- transform(df_select, P_conc_stnd = 2 * P_stnd) 
  stnd_curve <- lm(abs ~ P_conc_stnd, data = df_select)
  # plot(df_select$P_conc_stnd, df_select$abs)
  # summary(stnd_curve)
  # abline(stnd_curve)
  int <- stnd_curve$coef[1]
  slope <- stnd_curve$coef[2]
  df_out <- subset(df, type == "sample")
  df_out <- transform(df_out, 
    P_resin = (abs - int) / slope * (30 / eluate) / m_soil)
}

cal_P3 <- function(df, P_stnd, h2o, m1, m2, abs) {
  df_select <- subset(df, site == "cal1")
  # Calculate in mu g P / cuvette
  df_select <- transform(df_select, P_conc_stnd = P_stnd) 
  stnd_curve <- lm(abs ~ P_conc_stnd, data = df_select)
  # plot(df_select$P_conc_stnd, df_select$abs)
  # summary(stnd_curve)
  # abline(stnd_curve)
  int <- stnd_curve$coef[1]
  slope <- stnd_curve$coef[2]
  df_out <- subset(df, type == "sample")
  df_out <- transform(df_out, 
    P_resin = (abs - int) / slope * (30 / eluate) / m_soil)
}

# Predict P_measured for series 1 (no dilution)
dat_resin_P1 <- cal_P(dat_resin_P1_raw)

# Predict P_measured for series 2 (1:1 and 1:3 dilution)
dat_resin_P2 <- cal_P(dat_resin_P2_raw)

# Predict P_measured for series 3 (1:10 dilution of samples;
# standard and measurement of 2 samples by Eva)
dat_resin_P3 <- cal_P3(dat_resin_P3_raw)

# Combine data
dat_resin_P_all <- rbind(dat_resin_P1, dat_resin_P2, dat_resin_P3)
# dat_resin_P[duplicated(dat_resin_P$sample_ID),] 
# any(duplicated(dat_resin_P$sample_ID))

# Calculate mean resin_P
dat_resin_P <- dat_resin_P_all %>%
  group_by(sample_ID, country, site) %>%
  summarize(
    P_resin = mean(P_resin)
  )

## (04) Read and transform relevant total element concentration data obtained
## by XRF spectroscopy =========================================================

dat_element_total <- read_csv("data/soilchem/data-raw/data-xrf.csv")

# Convert total Ca and K from percent to g per kg soil
dat_element_total$Ca_tot <- 10 * dat_element_total$Ca_tot
dat_element_total$Al_tot <- 10 * dat_element_total$Al_tot
dat_element_total$Si_tot <- 10 * dat_element_total$Si_tot
dat_element_total$K_tot <- 10 * dat_element_total$K_tot
dat_element_total$Fe_tot <- 10 * dat_element_total$Fe_tot

# Delete total S (There is data from dry combustion)
dat_element_total$S_tot <- NULL

# Convert total P from percent to mg per kg soil
dat_element_total$P_tot <- 10000 * dat_element_total$P_tot
dat_element_total$Mn_tot <- 10000 * dat_element_total$Mn_tot


## (05) Read and transform data for DTPA-extractable micronutrients ============

dat_dtpa_raw <- read_csv("data/soilchem/data-raw/data-dtpa-extraction-icp.csv")

# check coefficient of variation of replicates
dat_dtpa_cov <- dat_dtpa_raw %>% 
  group_by(sample_ID, country, site) %>% 
  summarize(
    Zn_DTPA_var = sd(Zn_per_kg) / mean(Zn_per_kg),
    Cu_DTPA_var = sd(Cu_per_kg) / mean(Cu_per_kg),
    Fe_DTPA_var = sd(Fe_per_kg) / mean(Fe_per_kg),
    Mn_DTPA_var = sd(Fe_per_kg) / mean(Fe_per_kg)
  )

# Calculate mean per sample_ID for repetitions
dat_dtpa <- dat_dtpa_raw %>% 
  group_by(sample_ID, country, site) %>% 
  summarize(
    Zn_DTPA = mean(Zn_per_kg),
    Cu_DTPA = mean(Cu_per_kg),
    Fe_DTPA = mean(Fe_per_kg),
    Mn_DTPA = mean(Mn_per_kg)
  )


## (06) Read and transform data for exchangable cations ========================

dat_exch_cations_raw <- read_csv(
  "data/soilchem/data-raw/data-exch-cations-icp.csv")

# Check standard deviation
dat_exch_cations_sd <- dat_exch_cations_raw %>% 
  group_by(sample_ID, country, site) %>% 
  summarize(
    ex_Ca = sd(ex_Ca)
  )

# Calculate mean per sample_ID for repetitions
dat_exch_cations <- dat_exch_cations_raw %>% 
  group_by(sample_ID, country, site) %>% 
  summarize(
    ex_Ca = mean(ex_Ca),
    ex_Mg = mean(ex_Mg),
    ex_K = mean(ex_K),
    ex_Al = mean(ex_Al),
    ex_Na = mean(ex_Na),
    ex_Fe = mean(ex_Fe),
    ex_Mn = mean(ex_Mn),
    ex_Al = mean(ex_Al),
    pH_BaCl2 = mean(pH_BaCl2)
  )

# Calculate CEC_eff using the summation method ---------------------------------

# Function that calculates CEC_eff
calculate_CEC <- function(ex_Ca, ex_Mg, ex_K, ex_Na, ex_Al, pH_BaCl2, ...) {
  # calculate conversion factor ... !!! comment better
  # also calculate Base saturation
  factor_Ca <- 2 / 40.078 / 100 * 10
  factor_Mg <- 2 / 24.305 / 100 * 10
  factor_K <- 1 / 39.098 / 100 * 10
  factor_Na <- 1 / 22.990 / 100 * 10
  factor_H <- 1 / 1 / 100 * 10
  factor_Al <- 3 / 26.98 / 100 * 10
  # Calculate CEC_eff as sum of exchangable cations in 
  # centimoles of cation charge per kilogram
  charge_Ca <- factor_Ca * ex_Ca
  charge_Mg <- factor_Mg * ex_Mg
  charge_K <- factor_K * ex_K
  charge_Na <- factor_Na * ex_Na
  charge_H <- factor_H * 10^(- pH_BaCl2) * 0.03 / 2 * 1000
  charge_Al <- factor_Al * ex_Al
  charge_basic <- charge_Ca + charge_Mg + charge_K + charge_Na
  CEC_eff <- charge_basic + charge_H + charge_Al
  # Calculate effective Base Saturation in %
  BS_eff <- charge_basic / CEC_eff * 100
  tibble::tibble(
    CEC_eff = CEC_eff,
    BS_eff = BS_eff)
}

# Calculate CEC_eff and BS_eff
dat_CEC <- plyr::ddply(dat_exch_cations, "sample_ID",
  plyr::splat(calculate_CEC)
)

# Merge exchangeable cations with CEC_eff and BS data frame
dat_exch_cations <- merge(dat_exch_cations, dat_CEC, 
  by = "sample_ID"
)


## (07) Read texture data obtained from IITA ===================================

dat_texture <- read_csv("data/soilchem/data-raw/data-texture-iita.csv")

## Aggregate all laboratory reference analysis data into one data frame ========

# YAMSYS Site names
site_names <- c(
  'lo' = "Léo (Burkina Faso)",
  'sb' = "Liliyo (Côte d'Ivoire)",
  'mo' = "Midebdo (Burkina Faso)",
  'tb' = "Tiéningboué (Côte d'Ivoire)"
)

# YAMSYS site names including LDSF samples obtained from ICRAF
site_names_all <- c(
  'lo' = "Léo (Burkina Faso)",
  'sb' = "Liliyo YAMSYS (Ivory Coast)",
  'mo' = "Midebdo (Burkina Faso)",
  'tb' = "Tiéningboué (Ivory Coast)",
  'sb_icraf' = "Liliyo LDSF (Ivory Coast)"
)

# Merge all reference analysis data frames to a list
list_data_soilchem <- list(
  dat_CNS,
  dat_exch_cations,
  dat_pH,
  # dat_resin_P,
  dat_element_total,
  dat_dtpa,
  dat_texture
)

# Use Reduce() to join all data frames
# Merge data frames into one new data frame, join by sample_ID
# http://stackoverflow.com/questions/8091303/simultaneously-merge-multiple-data-frames-in-a-list

dat_soilchem_reduced <-
  Reduce(function(...) merge(..., all = TRUE), list_data_soilchem)

# Remove prot_est, N_perc, C_perc, and S_perc
dat_soilchem <- subset(dat_soilchem_reduced, 
  select = - c(N_perc, C_perc, S_perc)
)

# Remove level <sb_icraf> from site and
# create new vector site_comb with 4 levels (1 per YAMSYS site)
dat_soilchem$site_comb <- as.factor(
  gsub("sb_icraf", "sb", as.character(dat_soilchem$site))
)

# Export combined soil chemical data to csv file; write entire data frame
soilchem_yamsys_csv <-
  write_csv(dat_soilchem, path = "data/soilchem/soilchem_yamsys.csv")
