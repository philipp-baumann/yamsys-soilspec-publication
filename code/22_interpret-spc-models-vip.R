################################################################################
## Author: Philipp Baumann (philipp.baumann@usys.ethz.ch)
## License: GPL-3.0
## Project: Biophysical, institutional and economic drivers of sustainable 
##   soil use in yam systems for improved food security in West Africa (YAMSYS)
## Description: Spectroscopic model interpretation plot; 
##   manuscript Fig. S5. Variable Importance in the Projection (vip) scores of
##   PLS regression models for total soil C, total N and % clay,
##   including overlaid raw and preprocessed spectra.
################################################################################

scripts_interpret <- c(here("R", "vip-wrappers.R"))
# walk(scripts_interpret, source)

## Read simplerspec model output ===============================================

# Load selection of final models 
# (previously tuned by repeated k-fold cross-validation)
# pls_C <- readRDS(file = here("models", "rep-kfold-cv", "pls_C.Rds"))
# pls_N <- readRDS(file = here("models", "rep-kfold-cv", "pls_N.Rds"))
# pls_clay <- readRDS(file = here("models", "rep-kfold-cv", "pls_clay.Rds"))

# Create a list of models
mout_list <- list(
  "PLSR C" = pls_C,
  "PLSR N" = pls_N,
  "PLSR clay" = pls_clay
)

## Transform data for plotting =================================================

# Extract raw spectra (list-column <spc>) and preprocessessed spectra 
# (list-column <spc_pre>) from total C model (spectra in all other models
# are the same) into a list containing two data.tables as separate elements ----

dts <- bind_lcols_dts(spc_tbl = pls_C$data$calibration, 
  lcols = c("spc", "spc_pre"))

# Convert list of data.tables to long form -------------------------------------

dts_long <- purrr::imap(dts, function(dt, nm) {
  dt <- data.table::melt(dt, 
    id.vars = c("spc_id", "lcol_type", "group_id"),
    variable.factor = FALSE,
    variable.name = "wavenumber");
  dt[, wavenumber := as.numeric(wavenumber)]
  })

# Annotate peaks in raw spectra based on peak picking --------------------------

# Read spectra
spc_tbl <- readRDS(file = "out/data/spec_chem.Rds")
spc_tbl_sliced <- spc_tbl %>%
  group_by(sample_id) %>% 
  slice(1L)
spc <- do.call(rbind, spc_tbl_sliced$spc) # This is a data frame and data.table
# Extract first spectrum in data.table as vector
spc_1 <- as.matrix(spc)[1, ]

# Find possible peaks
peaks_init <- pick.peaks(spc_1, 10)
# Filter spectral noise from prominent peaks (manually)
len <- length(peaks_init)
peaks <- peaks_init[- (len - c(2, 13, 14, 16:19, 22, 25, 26, 34, 37:57, 61:len))]

# Check visually
# plot(spc_1, type = "l")
# abline(v = peaks, col = "blue")

# Return wavenumbers of peaks
wn <- spc_tbl$wavenumbers[[1]]
wn_peaks <- wn[peaks]
round(wn_peaks, 0)

# Get absorbance offset for average maximum absorbance spectrum
# peaks[1]
peak_pos149 <- as.matrix(spc)[, 149]
idx_max <- seq_along(peak_pos149)[peak_pos149 == max(peak_pos149)][1] # [1]
# because three spectra measurements per sample_id with identical mean
# spectrum
spc_max <- as.matrix(spc)[idx_max, ]
# Maximum peak absorbance of spectra
spc_max_peaks <- spc_max[peaks]

# Merge soil component abbreviations to peaks
constituents <- rep("", length(wn_peaks))

constituents[1] <- " O-H (K)"
constituents[3] <- " O-H (K)"
constituents[4] <- " Alkyl C-H"
constituents[5] <- " Alkyl C-H"
constituents[10:13] <- rep(" Si-O (Q)", 4)
constituents[22] <- " Al-OH (K)"
constituents[23] <- " Al-OH (K)"

annotation_const <- paste0(round(wn_peaks, 0), constituents, sep = "")

df_peak <- tibble(
  wavenumber = wn_peaks,
  annotation = annotation_const,
  y_peaks = spc_max_peaks
)

## Plot spectra and VIP scores =================================================

# Prepare plotting data --------------------------------------------------------

# General graph settings
alpha <- 0.15
xlab <- expression(paste("Wavenumber [", cm^-1, "]"))
ylab1 <-  "Absorbance (Abs.)"; ylab2 <- "Preproc. Abs."
group_id <- "sample_id"

# Pretty Axis breaks
brk <- pretty(as.numeric(names(dts[["spc"]])[!names(dts[["spc"]]) %in% 
  c("spc_id", "lcol_type", "group_id")]))
maxmin <- function(x) {c(max(x), min(x))}
x_lim <- maxmin(as.numeric(names(dts[["spc"]])[!names(dts[["spc"]]) %in%
  c("spc_id", "lcol_type", "group_id")]))

# Extract VIP (variable importance in projection) scores and
# PLS regression coefficients
df_vip_pls <- extract_multi_pls_vip_coef(mout_list = mout_list)
df_vip_pls <- df_vip_pls %>%
  mutate(coef_bigger0 = as.integer(coef > 0)) %>% 
  mutate(coef_bigger0 = coef_bigger0 / 10) %>% 
  mutate(coef_bigger0 = coef_bigger0 - 0.05)

# Determine highlighted regions above VIP = 1
rects <- create_vip_rects(df_vip_pls[df_vip_pls$model == "PLSR C", ])


# Plot mean replicate spectra --------------------------------------------------
p_spc <- ggplot(dts_long[["spc"]], 
    aes(x = wavenumber, y = value)) +
  geom_rect(data = rects, inherit.aes = FALSE,
    aes(xmin = start, xmax = end, ymin = min(dts_long[["spc"]]$value),
      ymax = max(dts_long[["spc"]]$value), group = group), 
      color = "transparent",
      fill = "orange", alpha = 0.3) +
  geom_line(data = dts_long[["spc"]], inherit.aes = FALSE,
    aes(x = wavenumber, y = value, group = group_id),
      alpha = alpha, size = 0.2) +
  geom_vline(data = df_peak, aes(xintercept = wavenumber), color = "#e41a1c",
    linetype = "dotted", size = 0.4, alpha = 0.6) +
  geom_text_repel(data = df_peak, aes(x = wavenumber, y = y_peaks, 
    label = annotation_const), size = 2, angle = 90, col = "#e41a1c", 
    fontface = "bold", arrow = arrow(length = unit(0.02, "npc")),
    direction = "both", fill = "white", nudge_y = 0.45) +
  scale_x_reverse(limits = x_lim, breaks = brk) +
  ylim(c(0, 2.2)) +
  labs(x = xlab, y = ylab1) +
  theme_bw() +
  theme(plot.margin = unit(c(1, 5, -30, 6),
    units = "points"), axis.text.x = element_blank())

# Plot preprocessed spectra --------------------------------------------------
p_spc_pre <- ggplot(dts_long[["spc_pre"]],
    aes(wavenumber, value)) +
  geom_rect(data = rects, inherit.aes = FALSE,
    aes(xmin = start,
      xmax = end, ymin = min(dts_long[["spc_pre"]]$value),
      ymax = max(dts_long[["spc_pre"]]$value), group = group),
      color = "transparent",
      fill = "orange", alpha = 0.3) +
  geom_line(aes(group = group_id),
    alpha = alpha, size = 0.2) +
  labs(x = xlab, y = ylab2) +
  theme_bw() +
  theme(plot.margin = unit(c(0, 5, 1, 1),
    units = "points")) +
  scale_x_reverse(limits = x_lim, breaks = brk) +
  theme(plot.margin = unit(c(1, 5, -30, 6),
    units = "points"),
    axis.title.y = element_text(vjust = 0.25),
    axis.text.x = element_blank())

# Plot VIP ---------------------------------------------------------------------
p_vip <- ggplot(data = df_vip_pls,
    aes(x = wavenumber, y = vip)) +
  geom_rect(data = rects, inherit.aes = FALSE,
    aes(xmin = start, xmax = end, ymin = min(df_vip_pls$vip),
    ymax = max(df_vip_pls$vip), group = group), color = "transparent",
    fill = "orange", alpha = 0.3) +
  geom_hline(yintercept = 1, colour = "black") +
  geom_line(aes(colour = model), size = 0.55) +
  geom_point(aes(x = wavenumber, y = coef_bigger0),
    alpha = 1/5, size = 0.7, colour = "black", shape = 124) +
  xlab(xlab) +
  ylab("VIP") +
  scale_x_reverse(limits = x_lim, breaks = brk) +
  theme_bw() +
  theme(plot.margin = unit(c(0, 5, 1, 1),
    units = "points"), axis.title.y = element_text(vjust = 0.25),
    legend.position = "bottom") +
  guides(colour = guide_legend(title = "Model/soil attribute"))

# Arrange plots in panels without margins --------------------------------------
# No margins in between
p_spc_interpr_comb <- cowplot::plot_grid(
  p_spc, p_spc_pre, p_vip, rel_heights = c(0.35, 0.3, 0.6),
  ncol = 1, align = "v")

# Save graph 
p_spc_interpr_comb_pdf <- ggsave(filename = "spc-pls-vip.pdf",
  plot = p_spc_interpr_comb,
  path = here("out", "figs"),
  width = 6.5, height = 5)
  
# Save version for manuscript
p_spc_interpr_comb_pdf_manuscr <- ggsave(filename = "Figure4.pdf",
  plot = p_spc_interpr_comb, 
  path = here("manuscript", "figs"),
  width = 6.5, height = 5)
