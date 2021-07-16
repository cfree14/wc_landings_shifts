library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(ggthemes)
library(extrafont)
library(gridExtra)
library(extrafontdb)
library(plotly)
font_import()
loadfonts(device = "win")

# Data Import #
# Directories #
datadir <- "data"
plotdir <- "figures"
tabledir <- "tables"
sstdir <- "data/cobe"

# Read File #
mexdata <- readRDS(file.path(datadir, "2001_2020_mexico_landings_datamares.Rds"))
officedata <- readRDS(file.path(datadir, "mexico_fishery_office_key_with_ecoregion.Rds"))
spp_key <- readRDS(file.path(datadir, "mexico_species_key.Rds"))
sst <- read.csv(file.path(sstdir, "COBE_1891_2020_sst_by_ecoregion.csv"))

# Join Data #
data <- mexdata %>%
  # Add Location and Family #
  left_join(officedata, by=c("state", "office")) %>%
  left_join(spp_key %>% select(comm_name_orig, family))

sdata <- data %>%
  # Filter to species, region, and years of interest
  filter(sci_name=="Opisthonema sp" & prod_type=="Capture") %>%
  filter(year %in% years) %>%
  # Calculate total annual revenues by office
  group_by(year, office, ecoregion, lat_dd) %>%
  summarize(value_mxn=sum(value_mxn), landings_kg = sum(landings_kg)) %>%
  ungroup() %>%
  # Calculate revenue-weighted average latitude
  group_by(year) %>%
  summarize(lat_dd=weighted.mean(x=lat_dd, w=landings_kg)) %>%
  ungroup()

  # Create SST Data #
  sdata2 <- sdata %>%
    # Assign Ecoregion #
    mutate(ecoregion="Mexican Tropical Pacific") %>%
    # Add SST #
    left_join(sst, by=c("ecoregion", "year"))

  # Create Volume Data #
  sdata3 <- data %>%
    # Filter #
    filter(sci_name=="Opisthonema sp" & prod_type=="Capture") %>%
    filter(fishery_type == "Industrial") %>%
    filter(year %in% years) %>%
    # Calculate total annual revenues by office
    group_by(year, office, ecoregion, lat_dd) %>%
    summarize(value_mxn=sum(value_mxn),
              landings_kg=sum(landings_kg)) %>%
    ungroup() %>%
    # Calculate revenue-weighted average latitude
    group_by(year) %>%
    summarize(lat_dd=weighted.mean(x=lat_dd, w=landings_kg),
              landings_kg=sum(landings_kg)) %>%
    ungroup() %>%
    # Assign ecoregion
    mutate(ecoregion="Mexican Tropical Pacific") %>%
    # Add SST for that ecoregion
    left_join(sst, by=c("ecoregion", "year"))

view(data)
years <- 2001:2019

# Plot Data #
  # SST and Mean Latitude #
  g3 <- ggplot(sdata2, aes(x=sst_c, y=lat_dd)) +
        geom_point() +
        geom_smooth(method="lm") +
        labs(x="SST (°C)", y="Latitude (°N)") +
        theme_bw()
  # Pearson Correlation #
  # Mean Latitude and Landings #
  g4 <- ggplot(sdata3, aes(x=lat_dd, y=landings_kg/1000/1000)) +
        geom_point() +
        geom_smooth(method="lm") +
        labs(x="Landings-weighted latitude (°N)", y="Landings (1000s mt)") +
        theme_bw()
  # Pearson Correlation #

# Combine Graphs #
  grid.arrange(g3, g4, ncol = 2, heights = c(3, 3))

  lmfit <- lm(landings_kg ~ lat_dd, sdata3)
  summary(lmfit)

  cor(sdata2$lat_dd, sdata2$sst_c, method="pearson")




