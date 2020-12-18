################################################################################
## Author: Philipp Baumann (philipp.baumann@usys.ethz.ch)
## License: GPL-3.0
## Project: Biophysical, institutional and economic drivers of sustainable 
##   soil use in yam systems for improved food security in West Africa (YAMSYS)
## Description: Create maps using UTM easting and northing coordinates; depict
##   position of sampled fields within the four pilot landscapes
################################################################################

# Read field metadata
data_field <- read_csv(file = "data/metadata-field/metadata-field-yamsys.csv")

## Plot sampling distribution ==================================================

# Function that returns upper and limits of coordinates per site (atomic vector)
lim_sitemap <- function(df, site, column, diff = 14000) {
  min_easting <- min(data_field[data_field$site == site, ][[column]])
  c(min_easting, min_easting + diff)
}

# Set x-limits for landscapes
# "Tiéningboué" (Côte d'Ivoire)
xlim_tb <- lim_sitemap(df = data_field, site = "tb", column = "UTM_easting")
ylim_tb <- lim_sitemap(df = data_field, site = "tb", column = "UTM_northing")
# "Liliyo" (Côte d'Ivoire)
xlim_sb <- lim_sitemap(df = data_field, site = "sb", column = "UTM_easting")
ylim_sb <- lim_sitemap(df = data_field, site = "sb", column = "UTM_northing")
# Léo (Burkina Faso)
xlim_lo <- lim_sitemap(df = data_field, site = "lo", column = "UTM_easting")
ylim_lo <- lim_sitemap(df = data_field, site = "lo", column = "UTM_northing")
# Midebdo (Burkina Faso)
xlim_mo <- lim_sitemap(df = data_field, site = "mo", column = "UTM_easting")
ylim_mo <- lim_sitemap(df = data_field, site = "mo", column = "UTM_northing")


# Create graphs (ggplot2 objects) ----------------------------------------------

# Tiéningboué
# Rename labels on the fly with a lookup character vector
# See http://docs.ggplot2.org/current/as_labeller.html
to_string_tb <- as_labeller(c(`tb` = "Tiéningboué, Côte d'Ivoire"))
map_tb <- filter(data_field, site == "tb") %>%
  ggplot(data = ., aes(x = UTM_easting, y = UTM_northing)) +
  coord_fixed(ratio = 1) +
  geom_point(shape = 1) +
  facet_wrap("site", labeller = to_string_tb) +
  xlab("E (UTM) [m]") +
  ylab("N (UTM) [m]") +
  xlim(xlim_tb) +
  ylim(ylim_tb) +
  guides(colour = "none", shape = "none") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_text(size = rel(0.8)), 
    axis.title.y = element_text(size = rel(0.8)))

# Liliyo
# Rename labels on the fly with a lookup character vector
# See http://docs.ggplot2.org/current/as_labeller.html
to_string_sb <- as_labeller(c(`sb` = "Liliyo, Côte d'Ivoire"))
map_sb <- filter(data_field, site == "sb") %>%
  ggplot(data = ., aes(x = UTM_easting, y = UTM_northing)) +
  coord_fixed(ratio = 1) +
  geom_point(shape = 1) +
  facet_wrap("site", labeller = to_string_sb) +
  xlab("E (UTM) [m]") +
  ylab("N (UTM) [m]") +
  xlim(xlim_sb) +
  ylim(ylim_sb) +
  guides(colour = "none", shape = "none") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_text(size = rel(0.8)), 
    axis.title.y = element_text(size = rel(0.8)))
  

# Léo
# Rename labels on the fly with a lookup character vector
# See http://docs.ggplot2.org/current/as_labeller.html
to_string_lo <- as_labeller(c(`lo` = "Léo, Burkina Faso"))
map_lo <- filter(data_field, site == "lo") %>%
  ggplot(data = ., aes(x = UTM_easting, y = UTM_northing)) +
  coord_fixed(ratio = 1) +
  geom_point(shape = 1) +
  facet_wrap("site", labeller = to_string_lo) +
  xlab("E (UTM) [m]") +
  ylab("N (UTM) [m]") +
  xlim(xlim_lo) +
  ylim(ylim_lo) +
  guides(colour = "none", shape = "none") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_text(size = rel(0.8)),
    axis.title.y = element_text(size = rel(0.8)))

# Midebdo
# Rename labels on the fly with a lookup character vector
# See http://docs.ggplot2.org/current/as_labeller.html
to_string_mo <- as_labeller(c(`mo` = "Midebdo, Burkina Faso"))
map_mo <- filter(data_field, site == "mo") %>%
  ggplot(data = ., aes(x = UTM_easting, y = UTM_northing)) +
  coord_fixed(ratio = 1) +
  geom_point(shape = 1) +
  facet_wrap("site", labeller = to_string_mo) +
  xlab("E (UTM) [m]") +
  ylab("N (UTM) [m]") +
  xlim(xlim_mo) +
  ylim(ylim_mo) +
  guides(colour = "none", shape = "none") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_text(size = rel(0.8)), 
    axis.title.y = element_text(size = rel(0.8)))

# Arrange maps per landscape into one graph ------------------------------------

p_map <- plot_grid(map_lo, map_mo, map_tb, map_sb, ncol = 2)

# Save in generic figure output
p_map_pdf <- ggsave(filename = "field-distribution-per-site.pdf",  plot = p_map, 
  path = here(c("out", "figs"), width = 5, height = 5.25)

p_map_pdf_pub <- ggsave(filename = "S2.pdf",  
  plot = p_map, 
  path = here(c("out", "manuscript"), width = 5, height = 5.25)


## Use tmap and sf to create a nice map ========================================

# library(sf)

# data_sf <- 
#   data_field %>%
#   filter(site %in% c("tb", "lo", "mo")) %>%
#   st_as_sf(x = ., coords = c("UTM_easting", "UTM_northing"),
#   crs = 4326)

# ggplot(data = data_sf) + geom_sf()

# df.SP <- st_as_sf(df, coords = c("LONG", "LAT"), crs = 4326)

