# Load Packages #
library(tidyverse)
library(ggthemes)
library(extrafont)
library(extrafontdb)
font_import()
loadfonts(device = "win")

# Data Import #
  # Directories #
dataDir <- "data"
plotDir <- "figures"
tableDir <- "tables"

# Read File #
mexicoLandings = readRDS(file.path(dataDir, "2001_2020_mexico_landings_datamares.Rds"))

# Inspect Data #
view(mexicoLandings)
summary(mexicoLandings)
years <- 2001:2019

# Data Build #
artdata <- mexicoLandings %>%
  # Eliminate Columns and Years #
  filter(year %in% years) %>%
  filter(fishery_type == "Artisanal") %>%
  # filter(level != "species") %>%
  select(year, fishery_type, sci_name, landings_kg) %>%
  # Group by Fishery Type and Sort #
  group_by(year, sci_name) %>%
    # Calculate Sums #
    summarize(landings_kg_tot = sum(landings_kg))
  # Pick Top Species #
  artdata <- artdata[order(-artdata$landings_kg_tot),] %>%
  slice(1:5)

inddata <- mexicoLandings %>%
  # Eliminate Columns and Years #
  filter(year %in% years) %>%
  filter(fishery_type == "Industrial") %>%
  # filter(level != "species") %>%
  select(year, fishery_type, sci_name, landings_kg) %>%
  # Group by Fishery Type and Sort #
  group_by(year, sci_name) %>%
  # Calculate Sums #
  summarize(landings_kg_tot = sum(landings_kg))
  # Pick Top Species #
  inddata <- inddata[order(-inddata$landings_kg_tot),] %>%
  slice(1:5)

view(artdata)
view(inddata)

# Plot Artisanal Data #
gart <- ggplot(data = artdata, aes(x = year, y = landings_kg_tot / 1000, fill = sci_name)) +
  geom_col() +
  labs(title = "Species Targeted by Artisanal Fisheries",
       subtitle = "What are the top five species targeted annually by artisanal fisheries based on landings?",
       x ="Year", y = "Landings Volume (1000s of metric tons)", fill = "Scientific Name") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), text = element_text(size = 10, family = "Segoe UI"))
  # Save #
  ggsave(gart, filename=file.path(plotdir, "artisanal_species_landings.png"),
       units="in", width=10, height=8.0, dpi=600)

# Plot Industrial Data #
gind <- ggplot(data = inddata, aes(x = year, y = landings_kg_tot / 1000, fill = sci_name)) +
  geom_col() +
  labs(title = "Species Targeted by Industrial Fisheries",
       subtitle = "What are the top five species targeted annually by industrial fisheries based on landings?",
       x ="Year", y = "Landings Volume (1000s of metric tons)", fill = "Scientific Name") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), text = element_text(size = 10, family = "Segoe UI"))

  # Save #
  ggsave(gind, filename=file.path(plotdir, "industrial_species_landings.png"),
         units="in", width=10, height=8.0, dpi=600)
