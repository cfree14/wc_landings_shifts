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
  select(year, fishery_type, sci_name, landings_kg, value_mxn) %>%
  # Group by Fishery Type and Sort #
  group_by(year, sci_name) %>%
    # Calculate Sums #
    summarize(landings_kg_tot=sum(landings_kg),
              value_mxn_tot=sum(value_mxn))
  # Pick Top Species #
  artdata <- artdata[order(-artdata$landings_kg_tot, -artdata$value_mxn_tot),] %>%
  slice(1:10)

inddata <- mexicoLandings %>%
  # Eliminate Columns and Years #
  filter(year %in% years) %>%
  filter(fishery_type == "Industrial") %>%
  # filter(level != "species") %>%
  select(year, fishery_type, sci_name, landings_kg, value_mxn) %>%
  # Group by Fishery Type and Sort #
  group_by(sci_name) %>%
  # Calculate Sums #
  summarize(landings_kg_tot=sum(landings_kg),
            value_mxn_tot=sum(value_mxn))
  # Pick Top Species #
  inddata <- inddata[order(-inddata$landings_kg_tot, -inddata$value_mxn_tot),] %>%
  slice(1:5)

# Plot Data #
ggplot(data = inddata, aes(x = sci_name)) +
  geom_bar() +
  labs(title = "Species Targeted by Artisinal Fisheries",
       subtitle = "What types of species and groups do artisinal fisheries target?",
       x ="Year", y = "Proportion of Landings", color = "Species")



view(artdata)
view(inddata)

