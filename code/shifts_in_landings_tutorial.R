
# Packages
library(rnaturalearth)

# Directories
datadir <- "data"
plotdir <- "figures"
codedir <- "code"
years <- 2001:2019
# Import theme
source(file.path(codedir, "my_theme.R"))

# Read data
list.files(datadir)
data_orig <- readRDS(file.path(datadir, "2001_2020_mexico_landings_datamares.Rds"))

# Read fishery office key
office_key <- readRDS(file.path(datadir, "mexico_fishery_office_key.Rds"))
spp_key <- readRDS(file.path(datadir, "mexico_species_key.Rds"))

# Plotting
##################################################

# Make plot of Mexico fishery offices
mexico <- rnaturalearth::ne_states(country="Mexico", returnclass="sf")

# Plot Mexico
ggplot(mexico) +
  geom_sf()

# Plot location of the fishery offices
ggplot() +
  geom_sf(data=mexico, color="white", fill="grey80", lwd=0.4) +
  geom_point(data=office_key, mapping=aes(x=long_dd, y=lat_dd, color=state), alpha=0.5, size=5) +
  labs(title="Mexican fishery office locations", x="Longitude", y="Latitude") +
  scale_color_discrete(name="State") +
  theme_bw() + my_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5))



# Use joins to add meta-data from keys to data
##################################################


# Format data
data <- data_orig %>%
  # Add lat/long to data
  left_join(office_key, by=c("state", "office")) %>%
  # Add family
  left_join(spp_key %>% select(comm_name_orig, family))


# Analyze data
##################################################

top10_spp <- data %>%
  filter(year==2019 & prod_type=="Capture") %>%
  group_by(sci_name, comm_name) %>%
  summarize(landings_kg=landings_kg) %>%
  ungroup() %>%
  arrange(desc(landings_kg)) %>%
  slice(1:10)

# Let's look at sardine: Sardinops sagax
sardine_data <- data %>%
  filter(sci_name=="Penaeidae" & prod_type=="Capture") %>%
  filter(year %in% years) %>%
  group_by(year, office, lat_dd) %>%
  summarize(landings_mt=sum(landings_kg)/1000) %>%
  ungroup() %>%
  group_by(year) %>%
  summarize(lat_dd=weighted.mean(x=lat_dd, w=landings_mt)) %>%
  ungroup() %>%
  filter(year!=2001)

view(sardine_data)

ggplot(sardine_data, aes(x=year, y=lat_dd)) +
  geom_line()









