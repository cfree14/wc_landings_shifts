
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(sf)
library(rgeos)
library(tidyverse)

# Directories
meowdir <- "data/meows/processed"
datadir <- "data"
plotdir <- "figures"

# Read MEOW shapefile
meows_orig <- readRDS(file.path(meowdir, "meows.Rds"))
meows_sp <- as(object = meows_orig, Class="Spatial")

# Read fishery office key
office_key_orig <- readRDS(file.path(datadir, "mexico_fishery_office_key.Rds"))


# Build data
################################################################################

# Convert office to sf then sp
office_key_sf <- sf::st_as_sf(office_key_orig, coords=c("long_dd", "lat_dd"))
st_crs(office_key_sf) <- "+proj=longlat +datum=WGS84"
office_key_sp <- as(object = office_key_sf, Class="Spatial")


# Distanc to each LME (cols = offices, rows= meows)
dist_mat <- gDistance(office_key_sp[,1], meows_sp, byid=T)

dist_df <- dist_mat %>%
  # Convert to tibble
  as.tibble() %>%
  # Add MEOW id
  rownames_to_column(var="meow_id") %>%
  # Gather
  gather(key="office_id", value="dist", 2:ncol(.)) %>%
  # Determine closest
  group_by(office_id) %>%
  arrange(office_id, dist) %>%
  slice(1) %>%
  ungroup() %>%
  # Convert to numeric
  mutate(meow_id=as.numeric(meow_id),
         office_id=as.numeric(office_id)) %>%
  # Add fishery office
  mutate(office=office_key_orig$office[office_id],
         ecoregion=meows_orig$ecoregion[meow_id])


# Add MEOW to office key
office_key <- office_key_orig %>%
  left_join(dist_df %>% select(office, ecoregion)) %>%
  # Fix one
  mutate(ecoregion=recode(ecoregion, "Southern Gulf of Mexico"="Mexican Tropical Pacific"))

# Export key
saveRDS(office_key, file=file.path(datadir, "mexico_fishery_office_key_with_ecoregion.Rds"))

# MEOWs
meows_use <- c("Southern California Bight",
               "Magdalena Transition",
               "Cortezian",
               "Mexican Tropical Pacific",
               "Chiapas-Nicaragua")

# Format MEOWs
meows <- meows_orig %>%
  filter(ecoregion %in% meows_use)



# Plot data
################################################################################

# Land
states <- rnaturalearth::ne_states(country=c("Mexico"), returnclass="sf")
countries <- rnaturalearth::ne_countries(returnclass="sf", scale = "large")

# Plot data
g1 <- ggplot( ) +
  # Plot MEOWs
  geom_sf(data=meows, fill=NA) +
  # Plot land
  geom_sf(data=countries, fill="grey90", color=NA) +
  geom_sf(data=states, fill="grey70", color="white", lwd=0.2) +
  geom_sf(data=countries, fill=NA, color="grey30", lwd=0.3) +
 # Plot fishery offices
  geom_point(data=office_key, mapping = aes(x=long_dd, y=lat_dd, color=ecoregion), size=2, show.legend = F) +
  # Label MEOWs
  geom_sf_text(data=meows, mapping=aes(label=ecoregion), show.legend = F, size=2.5) +
  # Crop
  coord_sf(xlim=c(-120, -91), ylim=c(11, 34)) +
  # Labels
  labs(x="", y="") +
  # Theme
  theme_bw() +
  theme(axis.text=element_text(size=6),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.title=element_blank(),
        plot.title=element_blank(),
        # Gridlines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
g1

# Export plot
ggsave(g1, filename=file.path(plotdir, "fishery_office_by_ecoregion.png"),
       width=6.5, height=5, units="in", dpi=600)
