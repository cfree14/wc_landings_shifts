


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

# Read SST data
sst_ras <- raster::brick(file.path(sstdir, "COBE_1891_2021_annual_sst_mean.grd"))

# Read MEOWs
meows <- readRDS(file.path(meowdir, "meows.Rds"))


# Build SST time series
################################################################################

# Calculate mean annual SST within MEOWS
sst_ts_mat <- raster::extract(x=sst_ras, y=meows, method="simple", fun="mean", na.rm=T)


# Format data
sst_ts_df <- sst_ts_mat %>%
  # Convert to df
  as.data.frame() %>%
  # Add MEOW
  mutate(ecoregion=meows$ecoregion) %>%
  select(ecoregion, everything()) %>%
  # Gather
  gather(key="year", value="sst_c", 2:ncol(.)) %>%
  # Convert year
  mutate(year=gsub("X", "", year) %>% as.numeric()) %>%
  # Arrange
  arrange(ecoregion, year) %>%
  # Filter
  filter(year!=2021)

# Plot data
g <- ggplot(sst_ts_df, aes(x=year, y=sst_c, color=ecoregion)) +
  geom_line() +
  labs(x="Year", y='SST (°C)', title="SST by ecoregion") +
  theme_bw() +
  theme(legend.position = "none")
g

# Export data
################################################################################

# Export data
write.csv(sst_ts_df, file=file.path(sstdir, "COBE_1891_2020_sst_by_ecoregion.csv"), row.names=F)








