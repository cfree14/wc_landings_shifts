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
  dataDir <- "data"
  plotDir <- "figures"
  tableDir <- "tables"

# Read File #
mexdata <- readRDS(file.path(datadir, "2001_2020_mexico_landings_datamares.Rds"))
officedata <- readRDS(file.path(datadir, "mexico_fishery_office_key.Rds"))
spp_key <- readRDS(file.path(datadir, "mexico_species_key.Rds"))

# Join Data #
data <- data_orig %>%
  # Add Location and Family #
  left_join(office_key, by=c("state", "office")) %>%
  left_join(spp_key %>% select(comm_name_orig, family))

view(data)
years <- 2001:2019

# Build Data #
latdata <- data %>%
filter(year %in% years) %>%
filter(sci_name == "Penaeidae") %>%
select(year, sci_name, office, landings_kg, value_mxn, lat_dd) %>%
group_by(year, sci_name, lat_dd) %>%
summarize(landings_mt=sum(landings_kg)/1000, value_mxn_sum = sum(value_mxn)) %>%
ungroup() %>%
group_by(year) %>%
summarize(lat_dd=weighted.mean(x=lat_dd, w=landings_mt), landings_mt_tot = sum(landings_mt), value_mxn_tot = sum(value_mxn_sum)) %>%
ungroup()

view(latdata)

# Generate Graphs #
g1 <- ggplot(latdata, aes(x = year, y = lat_dd)) +
  geom_line() +
  theme(axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(angle = 90), text = element_text(size = 10, family = "Segoe UI")) +
  labs(title = "Penaeidae Landings, Revenues, and Latitudes over time.",
       subtitle = "How do Penaeidae statistics vary over time?",
       y = "Mean Latitude")

g2 <- ggplot(latdata, aes(x = year, y = landings_mt_tot)) +
geom_line() +
  theme(axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(angle = 90), text = element_text(size = 10, family = "Segoe UI")) +
  labs(y = "Landings in Megatons")

g3 <- ggplot(latdata, aes(x = year, y = value_mxn_tot)) +
geom_line() +
  theme(axis.text.y = element_text(angle = 90), text = element_text(size = 10, family = "Segoe UI")) +
  labs(x = "Year", y = "Value in Pesos (Millions)")

g <- grid.arrange(g1, g2, g3, ncol = 1, heights = c(3, 1.5, 1.5))

ggsave(g, filename=file.path(plotdir, "sample_correlation.png"),
       units="in", width=6.5, height=8.0, dpi=600)
