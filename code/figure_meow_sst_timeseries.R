
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(sf)
library(tidyverse)

# Directories
sstdir <- "data/cobe"
meowdir <- "data/meows/processed"
plotdir <- "figures"

# Read MEOW shapefile
meows_orig <- readRDS(file.path(meowdir, "meows.Rds"))

# Read MEOW time series
sst_orig <- read.csv(file.path(sstdir, "COBE_1891_2020_sst_by_ecoregion.csv"))

# Plot data
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




# Plot data
################################################################################

# Land
states <- rnaturalearth::ne_states(country=c("Mexico", "United States of America"), returnclass="sf")
countries <- rnaturalearth::ne_countries(returnclass="sf", scale = "large")

# Plot data
g1 <- ggplot( ) +
  # Plot MEOWs
  geom_sf(data=meows, mapping=aes(fill=ecoregion), show.legend = F) +
  # Plot land
  geom_sf(data=countries, fill="grey90", color=NA) +
  geom_sf(data=states, fill="grey70", color="white", lwd=0.2) +
  geom_sf(data=countries, fill=NA, color="grey30", lwd=0.3) +
  # Label MEOWs
  geom_sf_label(data=meows, mapping=aes(label=ecoregion), show.legend = F, size=2.5) +
  # Crop
  coord_sf(xlim=c(-127, -91), ylim=c(11, 49)) +
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

# Function to plot time series
plot_ts <- function(ecoregion){

  # Format data
  ecoregion_do <- ecoregion
  edata <- sst_orig %>%
    filter(ecoregion==ecoregion_do)

  # Plot data
  g <- ggplot(edata, aes(x=year, y=sst_c)) +
    geom_line(size=0.5) +
    # Labels
    labs(y="SST (°C)", x="", title=ecoregion_do) +
    scale_x_continuous(breaks=seq(1880, 2020, 20)) +
    # Theme
    theme_bw() +
    theme(axis.text=element_text(size=6),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          axis.title.x=element_blank(),
          axis.title.y=element_text(size=7),
          plot.title=element_text(size=7),
          # Gridlines
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"))
  g

  # Return
  return(g)

}

# Plots
g2 <- plot_ts(ecoregion="Oregon, Washington, Vancouver Coast and Shelf")
g3 <- plot_ts(ecoregion="Northern California")
g4 <- plot_ts(ecoregion="Southern California Bight")
g5 <- plot_ts(ecoregion="Magdalena Transition")
g6 <- plot_ts(ecoregion="Cortezian")
g7 <- plot_ts(ecoregion="Mexican Tropical Pacific")
g8 <- plot_ts(ecoregion="Chiapas-Nicaragua")

# Merge plots
g <- gridExtra::grid.arrange(g1, g2, g3, g4, g5, g6, g7, g8,
                             layout_matrix=matrix(data=c(1,1,2,
                                                          1,1,3,
                                                          1,1,4,
                                                          1,1,5,
                                                          8,7,6), ncol=3, byrow=T))

# Export plot
ggsave(g, filename=file.path(plotdir, "figure_meow_sst_timeseries.png"),
       width=6.5, height=6.5, units="in", dpi=600)



