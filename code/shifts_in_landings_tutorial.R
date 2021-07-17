
# Packages
library(rnaturalearth)

# Directories
datadir <- "data"
sstdir <- "data/cobe"
plotdir <- "figures"
codedir <- "code"

# Import theme
source(file.path(codedir, "my_theme.R"))

# Read landings data
list.files(datadir)
data_orig <- readRDS(file.path(datadir, "2001_2020_mexico_landings_datamares.Rds"))

# Read species / fishery office key
office_key <- readRDS(file.path(datadir, "mexico_fishery_office_key_with_ecoregion.Rds"))
spp_key <- readRDS(file.path(datadir, "mexico_species_key.Rds"))

# Read SST by ecoregion
sst <- read.csv(file.path(sstdir, "COBE_1891_2020_sst_by_ecoregion.csv"))

# Parameters
years <- 2001:2019

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

view(data)
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


# Let's look the shift in Sardinops sagax in Southern California Bight
##################################################

# Build dataset
sdata <- data %>%
  # Filter to species, region, and years of interest
  filter(sci_name=="Penaeidae" & prod_type=="Capture") %>%
  filter(year %in% years) %>%
  # Calculate total annual revenues by office
  group_by(year, office, ecoregion, lat_dd) %>%
  summarize(value_mxn=sum(value_mxn), landings_kg = sum(landings_kg)) %>%
  ungroup() %>%
  # Calculate revenue-weighted average latitude
  group_by(year) %>%
  summarize(lat_dd=weighted.mean(x=lat_dd, w=landings_kg)) %>%
  ungroup()

# Plot raw data
g <- ggplot(sdata, aes(x=year, y=lat_dd)) +
  geom_line() +
  labs(x="Year", y='Latitude (°N)') +
  theme_bw()
g

# Plot data with linear regression fit
g1 <- ggplot(sdata, aes(x=year, y=lat_dd)) +
  geom_line() +
  geom_smooth(method="lm", color="red", fill="grey80") +
  labs(x="Year", y='Latitude (°N)') +
  theme_bw()
g

# Fit a linear regression using the lm()
lm(data=sdata, formula=lat_dd~year)
lmfit <- lm(lat_dd~year, sdata)

# Inspect linear regression results
summary(lmfit)

# Extract values of interest from the linear model fit
coef(lmfit)
slope <- coef(lmfit)[2]
names(lmfit)
str(lmfit)

# r2 and pvalue
r2 <- summary(lmfit)$r.squared
pvalue <- summary(lmfit)$coefficients[2,4]

# Now, add SST to the dataframe and see the correlation with SST
sdata2 <- sdata %>%
  # Assign ecoregion
  mutate(ecoregion="Southern California Bight") %>%
  # Add SST for that ecoregion
  left_join(sst, by=c("ecoregion", "year"))

# Plot the relationship between lat and SST
g2 <- ggplot(sdata2, aes(x=sst_c, y=lat_dd)) +
  geom_point() +
  geom_smooth(method="lm") +
  labs(x="SST (°C)", y="Latitude (°N)") +
  theme_bw()
g

# Correlation coefficient
cor(sdata2$lat_dd, sdata2$sst_c, method="pearson")
lmfit <- lm(lat_dd ~ sst_c, sdata2)
summary(lmfit)


# Volume as a function of latitude
##################################################

# year, lat, volume, sst
sdata3 <- data %>%
  # Filter to species, region, and years of interest
  filter(sci_name=="Selachimorpha") %>%
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
  mutate(ecoregion = "Southern California Bight") %>%
  # Add SST for that ecoregion
  left_join(sst, by=c("ecoregion", "year"))


g1 <- ggplot(sdata3, aes(x=lat_dd, y=landings_kg/1000/1000)) +
  geom_point() +
  geom_smooth(method="lm") +
  labs(x="Landings-weighted latitude (°N)", y="Landings (1000s mt)") +
  theme_bw()
g1

grid.arrange(g1, g2, ncol = 2, heights = c(3, 3))

lmfit <- lm(landings_kg ~ lat_dd, sdata3)
summary(lmfit)


