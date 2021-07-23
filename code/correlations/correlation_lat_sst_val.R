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

years <- 2001:2019
# Join Data #
data <- mexdata %>%
  # Add Location and Family #
  left_join(officedata, by=c("state", "office")) %>%
  left_join(spp_key %>% select(comm_name_orig, family))

sdata <- data %>%
  # Filter to species, region, and years of interest
  filter(sci_name == "Sardinops sagax") %>%
  filter(year %in% years) %>%
  # Calculate total annual revenues by office
  group_by(year, office, ecoregion, lat_dd) %>%
  summarize(value_mxn=sum(value_mxn), landings_kg = sum(landings_kg)) %>%
  ungroup() %>%
  # Calculate revenue-weighted average latitude
  group_by(year) %>%
  summarize(lat_dd=weighted.mean(x=lat_dd, w=landings_kg),
            landings_kg=sum(landings_kg)) %>%
  ungroup()

  # Create SST Data #
  sdata2 <- sdata %>%
    # Assign Ecoregion #
    mutate(ecoregion="Southern California Bight") %>%
    # Add SST #
    left_join(sst, by=c("ecoregion", "year"))

  # Create Volume Data #
  sdata3 <- data %>%
    # Filter #
    # Assign ecoregion
    mutate(ecoregion="Southern California Bight") %>%
    # Add SST for that ecoregion
    left_join(sst, by=c("ecoregion", "year")) %>%
    filter(prod_type=="Capture") %>%
    filter(fishery_type == "Artisanal") %>%
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
    ungroup()


# New plot for paper
g <- ggplot(sdata2, aes(x=lat_dd, y=landings_kg/1000/1000, fill=sst_c)) +
  geom_point(pch=21, size=3) +
  geom_smooth(method="lm", color="black", fill=NA) +
  # Limits
  lims(y=c(0,NA)) +
  # Labels
  labs(x="Latitude (°N)", y="Landings (1000s mt)") +
  # Legend
  scale_fill_gradientn(name="SST (°C)", colors=rev(RColorBrewer::brewer.pal(9, "RdBu"))) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() +
  theme(axis.text=element_text(size=6),
         axis.title=element_text(size=8),
         legend.text=element_text(size=6),
         legend.title=element_text(size=8),
         strip.text=element_text(size=8),
         plot.title=element_text(size=10),
         # Gridlines
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.background = element_blank(),
         axis.line = element_line(colour = "black"))
g

# Export plot
ggsave(g, filename=file.path(plotdir, "sardine_landings_vs_lat_by_sst.png"),
       width=4.5, height=4, units="in", dpi=600)


# Plot Data #
# SST and Mean Latitude #
g3 <- ggplot(sdata2, aes(x=sst_c, y=lat_dd)) +
      geom_point() +
      geom_smooth(method="lm") +
      labs(x="SST (°C)", y="Latitude (°N)") +
      theme_bw()
g3

  # Pearson Correlation #
  # Mean Latitude and Landings #
  g4 <- ggplot(sdata3, aes(x=lat_dd, y=landings_kg/1000/1000)) +
        geom_point() +
        geom_smooth(method="lm") +
        labs(x="Landings-weighted latitude (°N)", y="Landings (1000s mt)") +
        theme_bw()
  g4
  # Pearson Correlation #

# Combine Graphs #
grid.arrange(g3, g4, ncol = 2, heights = c(3, 3))
  ggsave(g5, filename=file.path(plotdir, "nematistius_pectoralis_artisanal_up_1.png"),
         units="in", width=6.5, height=8.0, dpi=600)


  lmfit <- lm(landings_kg ~ lat_dd, sdata3)
  summary(lmfit)

  cor(sdata2$lat_dd, sdata2$sst_c, method="pearson")




