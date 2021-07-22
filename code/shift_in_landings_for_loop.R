
# Packages
library(tidyverse)
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

view(sst)
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

view(data)
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
         pval=NA,
         r2_sst_latitude=NA,
         pval_sst_latitude=NA,
         r2_latitude_landings=NA,
         pval_latitude_landings=NA,
         r2_latitude_value=NA,
         pval_latitude_value=NA)

view(fishery_key)
i <- 1
#landings exceptions - i == 62 || i == 101 || i == 112 || i == 127 || i == 153 || i == 267 || i == 305 || i == 306
for(i in 1:nrow(fishery_key)){
if(i == 17 || i == 20 || i == 62 || i == 101 || i == 108 || i == 112 || i == 127 || i == 153 || i == 182 || i == 267 || i == 305 || i == 306)
{
  i <- i + 1
}
  # Which fishery and which species are we currently doing
  print(i)
  fishery_do <- fishery_key$fishery_type[i]
  spp_do <- fishery_key$sci_name[i]

  # Format data for analysis
  sdata <- data %>%
    # Filter to years of interest
    filter(prod_type=="Capture" & sci_name==spp_do & fishery_type==fishery_do) %>%
    filter(year %in% years) %>%
    # Calculate annual landings office
    group_by(year, office, lat_dd) %>%
    summarize(landings_mt=sum(landings_kg)/1000, value_mxn_1=sum(value_mxn)) %>%
    ungroup() %>%
    # Calculate landings weighted latitude
    group_by(year) %>%
    summarize(lat_dd=weighted.mean(x=lat_dd, w=value_mxn_1), landings_mt = sum(landings_mt), value_mxn_2=sum(value_mxn_1)) %>%
    ungroup() %>%
    # Classify starting ecoregion
    mutate(lat01 = lat_dd[year==min(year)],
           ecoregion=cut(lat01,
                        breaks=c(2, 9.5, 13, 21.5, 35),
                        labels=c("Chiapas-Nicaragua", "Mexican Tropical Pacific",
                                 "Magdalena Transition", "Southern California Bight"))) %>%
    # Add SST data
    left_join(sst, by=c("year", "ecoregion"))


   # <- ggplot(sdata, aes(x=lat_dd, y=landings_mt)) +
   # geom_line() +
   # geom_smooth(method="lm", color="#226462", fill="#226462", alpha=0.2) +
   # labs(x="Year", y='Latitude (°N)') +
   # theme_bw()
  # g2
  # print(g2)

  # Fit a linear regression using the lm()
  lmfit1 <- lm(lat_dd~year, sdata)
  lmfit2 <- lm(sst_c~lat_dd, sdata)
  lmfit3 <- lm(lat_dd~landings_mt, sdata)
  lmfit4 <- lm(lat_dd~value_mxn_2, sdata)
  # summary(lmfit)
summary(lmfit2)
  # Extract important statistics
  slope <- coef(lmfit1)[2]
  r2 <- summary(lmfit1)$r.squared
  pvalue <- summary(lmfit1)$coefficients[2,4]
  r2_sst_latitude <- summary(lmfit2)$r.squared
  r2_latitude_landings <- summary(lmfit3)$r.squared
  r2_latitude_value <- summary(lmfit4)$r.squared
  pval_sst_latitude <- summary(lmfit2)$coefficients[2,4]
  pval_latitude_landings <- summary(lmfit3)$coefficients[2,4]
  pval_latitude_value <- summary(lmfit4)$coefficients[2,4]

  # Record the statistics in the container
  fishery_key$slope[i] <- slope
  fishery_key$r2[i] <- r2
  fishery_key$pval[i] <- pvalue
  fishery_key$r2_sst_latitude[i] <- r2_sst_latitude
  fishery_key$r2_latitude_landings[i] <- r2_latitude_landings
  fishery_key$r2_latitude_value[i] <-r2_latitude_value
  fishery_key$pval_sst_latitude[i] <- pval_sst_latitude
  fishery_key$pval_latitude_landings[i] <- pval_latitude_landings
  fishery_key$pval_latitude_value[i] <- pval_latitude_value
}

# Extract statistics from output
#filter(fishery_key, fishery_type == "Industrial")
#industrial_r2_vals <- fishery_key %>% filter(fishery_type == "Artisanal") %>%
#  pull(r2_latitude_value)
fishery_key$r2_sst_latitude
summary(industrial_r2_vals)
summary(fishery_key$slope)
summary(fishery_key$r2_sst_latitude)
summary(fishery_key$r2_latitude_landings)
summary(fishery_key$r2_latitude_value)
summary(fishery_key$pval)
quantile(fishery_key$r2_sst_latitude, probs = c(0.5), na.rm=T)

industrial_r2_vals

view(fishery_key)
# Visualize the results
g1 <- ggplot(fishery_key, aes(x=fishery_type, y=slope, fill=fishery_type)) +
  geom_boxplot(show.legend = F, alpha = 0.7) +
  geom_hline(yintercept = 0) +
  labs(x="Fishery Scale", y="Annual Shift in Latitude (°N/year)") +
  scale_fill_manual(values=c("#3b89ac", "#22646e")) +  # "#003851", "#3561af"
  theme_bw() +
  theme(axis.title = element_text(), text = element_text(size = 12, family = "Calibri"))
g1
ggsave(g1, filename=file.path(plotdir, "slope_boxplot.png"),
       units="in", width=6.5, height=8.0, dpi=600)

g2 <- ggplot(fishery_key, aes(x=fishery_type, y=r2_sst_latitude, fill=fishery_type)) +
  geom_boxplot(show.legend = F, alpha = 0.7) +
  labs(x="Fishery Scale", y=expression(paste("SST vs Weighted Mean Latitude ", R^2))) +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values=c("#3b89ac", "#22646e")) +  # "#003851", "#3561af"
  theme_bw() +
  theme(axis.title = element_text(), text = element_text(size = 12, family = "Calibri"))
g2
ggsave(g2, filename=file.path(plotdir, "sst_latitude_boxplot.png"),
       units="in", width=6.5, height=8.0, dpi=600)

g3 <- ggplot(fishery_key, aes(x=fishery_type, y=r2_latitude_landings, fill=fishery_type)) +
  geom_boxplot(show.legend = F, alpha = 0.7) +
  labs(x="Fishery Scale", y=expression(paste("Weighted Mean Latitude vs Annual Landings ", R^2))) +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values=c("#3b89ac", "#22646e")) +  # "#003851", "#3561af"
  theme_bw() +
  theme(axis.title = element_text(), text = element_text(size = 12, family = "Calibri"))
g3
ggsave(g3, filename=file.path(plotdir, "latitude_landings_boxplot.png"),
       units="in", width=6.5, height=8.0, dpi=600)

g4 <- ggplot(fishery_key, aes(x=fishery_type, y=r2_latitude_value, fill=fishery_type)) +
  geom_boxplot(show.legend = F, alpha = 0.7) +
  labs(x="Fishery Scale", y=expression(paste("Weighted Mean Latitude vs Annual Revenue ", R^2))) +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values=c("#3b89ac", "#22646e")) +  # "#003851", "#3561af"
  theme_bw() +
  theme(axis.title = element_text(), text = element_text(size = 12, family = "Calibri"))
g4
ggsave(g4, filename=file.path(plotdir, "latitude_value_boxplot.png"),
       units="in", width=6.5, height=8.0, dpi=600)

g5 <- grid.arrange(g1, g2, g3, g4, ncol = 4)
g5
ggsave(g5, filename=file.path(plotdir, "the_boxplots.png"),
       units="in", width=16, height=7.5, dpi=600)

