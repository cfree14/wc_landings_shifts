
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(sf)
library(tidyverse)

# Directories
indir <- "data/meows/raw"
outdir <- "data/meows/processed"
plotdir <- "figures"

# Read data
data_orig <- sf::st_read(dsn=indir, layer="meow_ecos")


# Format data
################################################################################

# Format  data
data <- data_orig %>%
  # Rename columns
  janitor::clean_names("snake") %>%
  rename(ecoregion_id=eco_code,
         province_id=prov_code,
         realm_id=rlm_code) %>%
  # Arrange (eliminating useles columns)
  select(realm_id, realm, province_id, province, ecoregion_id, ecoregion, lat_zone, everything()) %>%
  select(-c(alt_code, eco_code_x))

# Inspect data
head(data)

# Export data
saveRDS(data, file=file.path(outdir, "meows.Rds"))


# Plot data
################################################################################

# My theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.text.y = element_text(angle = 90, hjust = 0.5),
                   axis.title=element_blank(),
                   plot.title=element_blank(),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"))

# MEOWS use
data_use <- data %>%
  filter(realm %in% c("Temperate Northern Pacific", "Tropical Eastern Pacific") &
           province %in% c("Cold Temperate Northeast Pacific",
                           "Warm Temperate Northeast Pacific",
                           "Tropical East Pacific"))

# Land
states <- rnaturalearth::ne_states(country=c("Mexico", "United States of America"), returnclass="sf")
countries <- rnaturalearth::ne_countries(returnclass="sf", scale = "large")

# Plot data
g <- ggplot( ) +
  # Plot MEOWs
  geom_sf(data=data_use, mapping=aes(fill=ecoregion), show.legend = F) +
  # Plot land
  geom_sf(data=mex_states, fill="grey80", color="white", lwd=0.2) +
  geom_sf(data=countries, fill=NA, color="grey30", lwd=0.3) +
  # Label MEOWs
  geom_sf_label(data=data_use, mapping=aes(label=ecoregion), show.legend = F, size=2.5) +
  # Crop
  coord_sf(xlim=c(-127, -91), ylim=c(11, 49)) +
  # Labels
  labs(x="", y="") +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "meows_map.png"),
       width=6, height=6.5, units="in", dpi=600)


