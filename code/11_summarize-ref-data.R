################################################################################
## Author: Philipp Baumann (philipp.baumann@usys.ethz.ch)
## License: GPL-3.0
## Project: Biophysical, institutional and economic drivers of sustainable 
##   soil use in yam systems for improved food security in West Africa (YAMSYS)
## Description: Create summary plot of chemical reference analysis data, which
##   are later used for developing spectroscopic reference models for the YAMSYS
##   pilot landscapes
################################################################################

# Boxplot summary utilities
# scripts_summary <- source(here("R", "utils-graph.R"))

## Make Boxplots soil chemical data summary ====================================

# Read complete YamSys soil data
data_yamsys <- read_csv(file = "data/soilchem/soilchem_yamsys.csv")
# Filter soil chemical data
vars_rm <- c("ex_Na", "ex_Fe", "ex_Mn", "pH_BaCl2")
data_yamsys <- data_yamsys %>% 
  select(- one_of(vars_rm))

# Melt complete YAMSYS data for ggplot 
# from wide to long data frame form -----------------------------
id_vars <- c("sample_ID", "country", "site", "site_comb", "material")
# Omit metadata variables when gathering (melting) variables from 
# long into wide form
cols_gather <- colnames(data_yamsys)[! colnames(data_yamsys) %in% id_vars]
yamsys_long <- tidyr::gather(data_yamsys, 
  cols_gather,
  key = variable,
  value = "value"
)

# Reorder levels of soil attributes (variable)
var_levels <- c(
  "C", "N", "S",
  "sand", "silt", "clay",
  "P_tot",
  "Fe_tot", "Al_tot", "Si_tot", "Ca_tot", "Zn_tot", 
  "Cu_tot", "K_tot", "Mn_tot",
  # "P_resin",
  "Fe_DTPA", "Zn_DTPA", "Cu_DTPA", "Mn_DTPA",
  "pH", "ex_Ca", "ex_Mg", "ex_K", "ex_Al",
  "CEC_eff", "BS_eff"
)
yamsys_long$variable <- factor(yamsys_long$variable, levels = var_levels,
  ordered = TRUE)

# Create full names for sites that will be used for the legend -----------------
site_names <- c(
  'lo' = "Léo (Burkina Faso)",
  'sb' = "Liliyo (Côte d'Ivoire)",
  'mo' = "Midebdo (Burkina Faso)",
  'tb' = "Tiéningboué (Côte d'Ivoire)"
)

# Prepare custom labels for the variables and units in facets ------------------

# Spaces in labels don't work, use "~" instead
# https://stackoverflow.com/questions/37418211/how-to-add-mathematical-symbols-in-ggplot-facet-wrap-text
# Avoid errors wheen adding special mathematical signs:
# https://stackoverflow.com/questions/28666794/add-less-than-symbol-to-a-ggplot-via-geom-text-in-r/28666905#28666905
# x is a lookup vector which is matched by the variable column used 
# in facet_wrap when creating the plot. Consider above links when plotting
# mathematical expressions; e.g. * is needed for connecting the label bits
lbl <- as_labeller(
  x = c(
    `Fe_tot` = "Total~Fe~'['*g~kg^-1*']'",
    `Si_tot` = "Total~Si~'['*g~kg^-1*']'",
    `Al_tot` = "Total~Al~'['*g~kg^-1*']'",
    `K_tot` = "Total~K~'['*g~kg^-1*']'",
    `Ca_tot` = "Total~Ca~'['*g~kg^-1*']'",
    `Zn_tot` = "Total~Zn~'['*mg~kg^-1*']'",
    `Cu_tot` = "Total~Cu~'['*mg~kg^-1*']'",
    `Mn_tot` = "Total~Mn~'['*mg~kg^-1*']'",
    `C` = "Total~C~'['*g~kg^-1*']'",
    `N` = "Total~N~'['*g~kg^-1*']'",
    `S` = "Total~S~'['*g~kg^-1*']'",
    `P_tot` = "Total~P~'['*g~kg^-1*']'",
    `ex_Ca` = "Ca~(exch.)~'['*mg~kg^-1*']'",
    `ex_Mg` = "Mg~(exch.)~'['*mg~kg^-1*']'",
    `ex_K` = "K~(exch.)~'['*mg~kg^-1*']'",
    `ex_Al` = "Al~(exch.)~'['*mg~kg^-1*']'",
    `CEC_eff` = "CEC[eff]~'[cmol(+)'~kg^-1*']'",
    `BS_eff` = "BS[eff]~'[%]'",
    `pH` = "'pH('*H[2]*'O)'",
    # `P_resin` = "P~resin~'['*mg~kg^-1*']'",
    `Fe_DTPA` = "Fe~(DTPA)~'['*mg~kg^-1*']'",
    `Zn_DTPA` = "Zn~(DTPA)~'['*mg~kg^-1*']'",
    `Cu_DTPA` = "Cu~(DTPA)~'['*mg~kg^-1*']'",
    `Mn_DTPA` = "Mn~(DTPA)~'['*mg~kg^-1*']'",
    `sand` = "Sand~'[%]'",
    `silt` = "Silt~'[%]'",
    `clay` = "Clay~'[%]'"
    ),
  default = label_parsed
)


# Create boxplot of all soil chemical properties -------------------------------

p_soilchem <- ggplot(data = yamsys_long) +
  geom_boxplot(aes(x = site_comb, y = value, colour = site_comb), width = 0.6) +
  facet_wrap(~ variable, scales = "free_y", ncol = 6, labeller = lbl) + 
  scale_colour_discrete(name = "Location",
    labels = site_names) +
  stat_summary(aes(x = site_comb, y = value),
    fun.data = give_n, geom = "text", 
    fun = function(x) median(x, na.rm = TRUE), size = 2.75,
    parse = TRUE, vjust = -1.15,
    position = position_dodge(width = 0.75)) +
  xlab("") +
  ylab("") +
  scale_x_discrete(labels = c("lo" = "Léo", "mo" = "Midebdo",
    "sb" = "Liliyo", "tb" = "Tiéningboué")) +
  theme_minimal() +
    # theme(legend.position = c(1, 0), 
    # legend.direction = "vertical", legend.justification = c(0.93, 0.3))  +
  theme(
    strip.background = element_rect(fill = "white", colour = NA),
    strip.text = element_text(hjust = 0),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.ticks = element_line(colour = "black"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.position = "none"
  )

# Save graph to file
p_soilchem_pdf <- ggsave(filename = "summary-soilchem-yamsys.pdf",
  plot = p_soilchem,
  path = here("out", "figs"),
  width = 10, height = 9)

# Save graph also within <publication> folder
p_soilchem_pdf_pub <- ggsave(filename = "S3.pdf",
  plot = p_soilchem,
  path = here("manuscript", "figs"),
  width = 10, height = 9)

# Save graph also within <publication> folder
p_soilchem_pdf_pub_final <- ggsave(filename = "Figure1.pdf",
  plot = p_soilchem,
  path = here("manuscript", "figs"),
  width = 10, height = 9)


## Numerical summary of measured soil properties ===============================

# Group and summarize chemical reference analysis data by site
soilchem_summary <- 
  dat_soilchem %>%
  group_by(site_comb) %>% # group by combined site
  summarize_if(.predicate = is.numeric,
    .funs = funs(mean, median, diff(range(.)), sd), na.rm = TRUE)

# Summarize per country
soilchem_country <-
  dat_soilchem %>%
  group_by(country) %>%
  summarize_if(.predicate = is.numeric,
    .funs = funs(mean, sd), na.rm = TRUE)


## Create correlation matrix heatmap for soil properties =======================
  
 yamsys_cormat <-
  data_yamsys %>%
  select(-c(sample_ID, country, site, material, site_comb)) %>%
  cor(use = "pairwise.complete.obs") %>%
  round(1) %>%
  reorder_cormat() %>%
  get_upper_tri()

yamsys_cormat_long <- yamsys_cormat %>%
  reshape2::melt()

x_labels_unordered <- c(
  `Fe_tot` = "Total~Fe",
  `Si_tot` = "Total~Si",
  `Al_tot` = "Total~Al",
  `K_tot` = "Total~K",
  `Ca_tot` = "Total~Ca",
  `Zn_tot` = "Total~Zn",
  `Cu_tot` = "Total~Cu",
  `Mn_tot` = "Total~Mn",
  `C` = "Total~C",
  `N` = "Total~N",
  `S` = "Total~S",
  `P_tot` = "Total~P",
  `ex_Ca` = "Ca~(exch.)",
  `ex_Mg` = "Mg~(exch.)",
  `ex_K` = "K~(exch.)",
  `ex_Al` = "Al~(exch.)",
  `CEC_eff` = "CEC[eff]",
  `BS_eff` = "BS[eff]",
  `pH` = "pH~(water)",
  # `P_resin` = "P~resin",
  `Fe_DTPA` = "Fe~(DTPA)",
  `Zn_DTPA` = "Zn~(DTPA)",
  `Cu_DTPA` = "Cu~(DTPA)",
  `Mn_DTPA` = "Mn~(DTPA)",
  `sand` = "Sand",
  `silt` = "Silt",
  `clay` = "Clay"
)

x_levels <- levels(yamsys_cormat_long$Var2)
x_labels_ordered <- x_labels_unordered[x_levels]

y_levels <- levels(yamsys_cormat_long$Var1)
y_labels_ordered <- x_labels_unordered[y_levels]

# Create a ggheatmap
p_cormat_heatmap <- ggplot(yamsys_cormat_long,
    aes(x = Var2, y = Var1, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
    midpoint = 0, limit = c(-1, 1), na.value = "white", space = "Lab", 
    name = "Pearson\nCorrelation") +
  scale_x_discrete(position = "top", labels = parse(text = x_labels_ordered)) + 
  scale_y_discrete(labels = parse(text = y_labels_ordered)) +
  coord_fixed() +
  geom_text(aes(Var2, Var1,
    label = case_when(map_lgl(value, ~ !is.na(.x)) ~ sprintf("%.1f", value)
   )),
    color = "black", size = 3) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(colour = "black", angle = 90, hjust = 0, vjust = 0.5,
      size = 10),
    axis.text.y = element_text(colour = "black", size = 10),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.7, 0.4),
    legend.direction = "horizontal") +
    guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
      title.position = "top", title.hjust = 0.5))

cormat_heatmap_pdf <- ggsave(filename = "soilchem-cormat-heatmap.pdf",
  plot = p_cormat_heatmap, path = here("out", "figs"), width = 9, height = 9)

cormat_heatmap_pdf_pub <- ggsave(filename = "Figure2.pdf",
  plot = p_cormat_heatmap, path = here("manuscript", "figs"),
  width = 9, height = 9)
