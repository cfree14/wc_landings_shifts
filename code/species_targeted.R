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
mexicoLandings = readRDS(file.path(datadir, "2001_2020_mexico_landings_datamares.Rds"))

# Inspect Data #
view(mexicoLandings)
summary(mexicoLandings)
years <- 2001:2019

# Data Build #
data <- mexicoLandings %>%
  # Eliminate Columns and Years #
  filter(year %in% years) %>%
  filter(fishery_type == "Artisinal") %>%
  filter(level != "species") %>%
  select(year, fishery_type, sci_name, level, landings_kg, value_mxn) %>%
  # Group by Fishery Type and Sort #
  group_by(year, fishery_type) %>%
    # Calculate Sums #
    summarize(landings_kg_tot=sum(landings_kg),
              value_mxn_tot=sum(value_mxn) / 10e6)

view(data)

