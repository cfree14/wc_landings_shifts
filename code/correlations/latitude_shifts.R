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
theme_set(theme_bw(base_size = 12))

# Data Import #
# Directories #
  datadir <- "data"
  plotdir <- "figures"
  tabledir <- "tables"

# Read File #
mexdata <- readRDS(file.path(datadir, "2001_2020_mexico_landings_datamares.Rds"))
office_key <- readRDS(file.path(datadir, "mexico_fishery_office_key.Rds"))
spp_key <- readRDS(file.path(datadir, "mexico_species_key.Rds"))

# Join Data #
data <- mexdata %>%
  # Add Location and Family #
  left_join(office_key, by=c("state", "office")) %>%
  left_join(spp_key %>% select(comm_name_orig, family))

view(data)
years <- 2001:2019

# Build Data #
latdata <- data %>%
filter(year %in% years) %>%
  # Choose Species and Fishery Type #
filter(sci_name == "Sardinops sagax") %>%
filter(fishery_type == "Industrial") %>%


# Calculate
  #Need to distinguish by fishery type #
select(year, sci_name, office, landings_kg, value_mxn, lat_dd) %>%
group_by(year, sci_name, lat_dd) %>%
summarize(landings_mt=sum(landings_kg)/1000, value_mxn_sum = sum(value_mxn)/1000) %>%
ungroup() %>%
group_by(year) %>%
summarize(lat_dd=weighted.mean(x=lat_dd, w=landings_mt), landings_mt_tot = sum(landings_mt)/1000, value_mxn_tot = sum(value_mxn_sum)/1000) %>%
ungroup()

view(latdata)

# Generate Graphs #
g1 <- ggplot(latdata, aes(x = year, y = lat_dd)) +
  geom_line() +
  geom_smooth(method = "lm",  color="#226462", fill="#3b89ac", alpha=0.2) +
  theme(axis.title.x = element_blank(),
    axis.text.y = element_text(angle = 90),
    text = element_text(size = 12, family = "Calibri")) +
  labs(y = "Mean Latitude (°N)")

g2 <- ggplot(latdata, aes(x = year, y = landings_mt_tot)) +
geom_line() +
  #geom_smooth(method = "lm",  color="#226462", fill="#3b89ac", alpha=0.2) +
  theme(axis.title.x = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_text(angle = 90), text = element_text(size = 12, family = "Calibri")) +
  labs(y = "Volume (Megatons)")

g3 <- ggplot(latdata, aes(x = year, y = value_mxn_tot)) +
geom_line() +
  #geom_smooth(method = "lm",  color="#226462", fill="#3b89ac", alpha=0.2) +
  theme(axis.text.y = element_text(angle = 90), text = element_text(size = 12, family = "Calibri")) +
  labs(x = "Year", y = "Value (Millions of Pesos)")
  #scale_x_continuous(breaks=seq(2000, 2020, 5), lim=c(2000, 2020)) +

g4 <- grid.arrange(g1, g2, g3, ncol = 1, heights = c(1.5, 1, 1))

ggsave(g4, filename=file.path(plotdir, "case_study.png"),
       units="in", width=8, height=6.5, dpi=600)
