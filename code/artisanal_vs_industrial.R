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
  # Eliminate Columns #
  filter(year %in% years) %>%
  filter(fishery_type != "Unknown") %>%
  select(year, fishery_type, sci_name, landings_kg, value_mxn) %>%
  # Calculate Total Landings for Fishery Type #
  group_by(fishery_type, year) %>%
  summarize(landings_kg_tot=sum(landings_kg),
          value_mxn_tot=sum(value_mxn) / 10e6) %>%
  ungroup()

  view(data)

# Data Visualization #
  # Revenue Graph #
g1 <- ggplot(data, mapping = aes(x = year, y = value_mxn_tot, color = fishery_type)) +
  geom_line(size = 1.5, alpha = 0.6) +
  labs(title = "Fishery Landing Values Over Time",
       subtitle = "How do artisanal and industrial fishery revenues differ?",
        x ="Year", y = "Value in Pesos (Millions)", color = "Fishery Type") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), text = element_text(size = 14, family = "Segoe UI"))
  # Export Graph 1 #
ggsave(g1, filename=file.path(plotdir, "artisanal_vs_industrial_value.png"),
       units="in", width=6.5, height=8.0, dpi=600)

  # Quantity Graph #
g2 <- ggplot(data, mapping = aes(x = year, y = landings_kg_tot/10e9, color = fishery_type)) +
  geom_line(size = 1.5, alpha = 0.6) +
  labs(title = "Fishery Landing Amounts Over Time",
       subtitle = "How do artisanal and industrial fishery landings differ?",
       x ="Year", y = "Value in Megatons", color = "Fishery Type") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), text = element_text(size = 14, family = "Segoe UI"))

  # Export Graph 2 #
ggsave(g2, filename=file.path(plotdir, "artisanal_vs_industrial_kg.png"),
       units="in", width=6.5, height=8.0, dpi=600)

