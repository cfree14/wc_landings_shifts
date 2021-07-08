library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(rgeos)
library(ggthemes)
library(extrafont)
library(extrafontdb)
library(plotly)
font_import()
loadfonts(device = "win")

# Data Import #
  # Directories #
  dataDir <- "data"
  plotDir <- "figures"
  tableDir <- "tables"

# Read File #
mexdata <- readRDS(file.path(datadir, "2001_2020_mexico_landings_datamares.Rds"))
officedata <- readRDS(file.path(datadir, "mexico_fishery_office_key.Rds"))

view(officedata)
years <- 2019

# Format Data #
data <- mexdata %>%
  # Add Location #
  left_join(officedata, by = c("state", "office"))

mexico <- rnaturalearth::ne_states(country="Mexico", returnclass="sf")

view(data)

ggplot(mexico) +
  geom_sf()

# Data Build #
locdata <- data %>%
  select(year, state, office, fishery_type, landings_kg, value_mxn, lat_dd, long_dd) %>%
  #filter(year %in% years) %>%
  filter(fishery_type != "Unknown") %>%
  group_by(year, state, office, fishery_type, lat_dd, long_dd) %>%
  summarize(landings_kg_tot = sum(landings_kg),
            value_mxn_tot=sum(value_mxn)) %>%
  ungroup()
  # Sort by Most Productive #
  locdata <- locdata[order(-locdata$landings_kg_tot),] %>%

view(locdata1)

# Plot Map #
gmap <- ggplot() +
  geom_sf(data = mexico, color = "white", fill="grey80", lwd = 0.4) +
  geom_point(data = locdata, mapping = aes(x = long_dd, y = lat_dd, color = fishery_type), alpha = 0.5, size = 5) +
  scale_color_manual(values = c('Artisanal', 'Industrial'))+
  labs(title="Mexican Fishery Locations", subtitles = "Where are artisanal and industrial fisheries located in Mexico?",
       x="Longitude", y="Latitude") +
  scale_color_discrete(name="Fishery Type") +
  theme_fivethirtyeight() +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5), text = element_text(size = 14, family = "Segoe UI"))
  # Save #
  ggsave(gmap, filename=file.path(plotdir, "fishery_map.png"),
       units="in", width=10, height=8.0, dpi=600)





