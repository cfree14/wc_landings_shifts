
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


# Build data
################################################################################

# Format data
data <- data_orig %>%
  # Add lat/long to data
  left_join(office_key, by=c("state", "office")) %>%
  # Add family
  left_join(spp_key %>% select(comm_name_orig, family))



# Build data
################################################################################

# Identify the fishery and species to loop through
fishery_key <- data %>%
  # Identify unique types
  group_by(fishery_type, sci_name) %>%
  summarize(nyrs=n_distinct(year)) %>%
  ungroup() %>%
  filter(nyrs>=5) %>%
  arrange(fishery_type, sci_name) %>%
  # Remove ND fisheries
  filter(sci_name!="ND") %>%
  # Add columns to record stats
  mutate(slope=NA,
         r2=NA,
         pval=NA)

i <- 1
for(i in 1:nrow(fishery_key)){

  # Which fishery and which species are we currently doing
  print(i)
  fishery_do <- fishery_key$fishery_type[i]
  spp_do <- fishery_key$sci_name[i]

  # Format data for analysis
  sdata <- data %>%
    # Filter to years of interest
    filter(prod_type=="Capture" & sci_name==spp_do & fishery_type==fishery_do) %>%
    filter(year %in% years) %>%
    # Calculatate annual landings office
    group_by(year, office, lat_dd) %>%
    summarize(landings_mt=sum(landings_kg)/1000) %>%
    ungroup() %>%
    # Calculate landings weighted latitud
    group_by(year) %>%
    summarize(lat_dd=weighted.mean(x=lat_dd, w=landings_mt)) %>%
    ungroup() %>%
    # Classify starting ecoregion
    mutate(lat01=lat_dd[year==2001],
           ecoregion=cut(lat01,
                         breaks=c(10, 15, 22, 26, 30),
                         labels=c("Chiapas-Nicaragua", "Mexican Tropical Pacific",
                                  "Magdalena Transition", "Southern California Bight"))) %>%
    # Add SST data
    left_join(sst, by=c("year", "ecoregion"))

  # Plot data with linear regression fit
  if(F){
    title_label <- paste(fishery_do, spp_do)
    g <- ggplot(sdata, aes(x=year, y=lat_dd)) +
      geom_line() +
      geom_smooth(method="lm", color="red", fill="grey80") +
      labs(x="Year", y='Latitude (°N)', title=title_label) +
      theme_bw()
    g
    print(g)
  }

  # Fit a linear regression using the lm()
  lmfit <- lm(lat_dd~year, sdata)
  # summary(lmfit)

  # Extract importnat statisitcs
  slope <- coef(lmfit)[2]
  r2 <- summary(lmfit)$r.squared
  pvalue <- summary(lmfit)$coefficients[2,4]

  # Record the statistics in the container
  fishery_key$slope[i] <- slope
  fishery_key$r2[i] <- r2
  fishery_key$pval[i] <- pvalue

}


# Visualize the results
g <- ggplot(fishery_key %>% filter(pval<=0.05), aes(x=fishery_type, y=slope)) +
  geom_boxplot() +
  geom_hline(yintercept = 0) +
  theme_bw()
g












