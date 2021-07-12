


# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(raster)
library(tidyverse)

# Directories
sstdir <- "data/cobe"
meowdir <- "data/meows/processed"
plotdir <- "figures"

# Read SST data
sst_orig <- raster::brick(file.path(sstdir, "COBE_1891_2021_annual_sst_mean.grd"))

# Read MEOWs
meows_orig <- readRDS(file.path(meowdir, "meows.Rds"))


# Build SST time series
################################################################################

# MEOWs
meows_use <- c("Oregon, Washington, Vancouver Coast and Shelf",
               "Northern California",
               "Southern California Bight",
               "Magdalena Transition",
               "Cortezian",
               "Mexican Tropical Pacific",
               "Chiapas-Nicaragua")

# Format MEOWs
meows <- meows_orig %>%
  filter(ecoregion %in% meows_use)

# Mask SST data by MEOWs of interest
sst <- raster::mask(sst_orig, mask=meows)

# Convert to data frame
sst_df <- sst %>%
  as.data.frame(xy=T)

# Calculate average
data <- sst_df %>%
  # Reshape
  gather(key="year", value="sst_c", 3:ncol(.)) %>%
  filter(!is.na(sst_c)) %>%
  # Format year
  mutate(year=gsub("X", "", year) %>% as.numeric) %>%
  # Average by year and latitude
  rename(long_dd=x, lat_dd=y) %>%
  group_by(year, lat_dd) %>%
  summarize(sst_c=mean(sst_c)) %>%
  ungroup()

# Export data
write.csv(data, file=file.path(sstdir, "COBE_1891_2021_annual_sst_mean_by_lat.csv"), row.names=F)


# Plot data
################################################################################

# Plot data
g1 <- ggplot(data, aes(x=year, y=sst_c, color=lat_dd, group=lat_dd)) +
  geom_line() +
  # Labels
  labs(x="Year", y="Sea surface temperature (°C)") +
  scale_x_continuous(breaks=seq(1890,2020,10)) +
  # Legend
  scale_color_gradientn(name="Latitude (°N)", colors=RColorBrewer::brewer.pal(9, "RdYlBu")) +
  guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() +
  theme(axis.text=element_text(size=9),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title=element_text(size=10),
        legend.text = element_text(size=9),
        legend.title = element_text(size=10),
        plot.title=element_blank(),
        # Gridlines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
g1

# Export plot
ggsave(g1, filename=file.path(plotdir, "figure_sst_by_lat.png"),
       width=6.5, height=3, units="in", dpi=600)


# Plot data
g2 <- ggplot(data, aes(x=year, y=lat_dd, z=sst_c, fill=sst_c)) +
  geom_tile() +
  geom_contour(color="black") +
  # Labels
  labs(x="Year", y="Latitude (°N)") +
  scale_x_continuous(breaks=seq(1890,2020,10)) +
  # Legend
  scale_fill_gradientn(name="SST (°C)", colors=RColorBrewer::brewer.pal(9, "RdYlBu")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() +
  theme(axis.text=element_text(size=9),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title=element_text(size=10),
        legend.text = element_text(size=9),
        legend.title = element_text(size=10),
        plot.title=element_blank(),
        # Gridlines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
g2

# Export plot
ggsave(g2, filename=file.path(plotdir, "figure_sst_by_lat2.png"),
       width=6.5, height=3, units="in", dpi=600)
