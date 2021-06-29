

# Load packages
library(tidyverse)

# Directories
datadir <- "data"
plotdir <- "figures"
tabledir <- "tables"

# Read data
list.files(datadir)
data_orig <- read.csv(file.path(datadir, "1941_2020_ca_landings_cdfw.csv"), as.is=T)
data_orig <- readRDS(file.path(datadir, "1941_2020_ca_landings_cdfw.Rds"))

# Inspect data
nrow(data_orig)
ncol(data_orig)
head(data_orig)
summary(data_orig)
str(data_orig)

library(psych)
psych::describe(data_orig)

# Inspect numeric columns
data_orig$year
range(data_orig$year)
min(data_orig$year)
max(data_orig$year)
summary(data_orig$year)
boxplot(data_orig$year)

# Inspect categorical columns
unique(data_orig$port_complex)
table(data_orig$port_complex)
table(data_orig$type)
sort(unique(data_orig$comm_name))
data_orig$comm_name %>% unique() %>% sort()

# Vector to specify years of interest
years <- c(2010, 2011, 2012, 2013)
years <- 2010:2020
years <- seq(2010, 2020, 1)

# Build data
data <- data_orig %>%
  # Eliminate shipments (landings only)
  filter(type=="Landings") %>%
  # Reduce to 2010-2020
  filter(year %in% years) %>%
  # Convert kg to g
  mutate(landings_g=landings_kg*1000,
         landings_mt=landings_kg/1000) %>%
  # Select columns of interest
  select(port_complex, port, year, comm_name, landings_kg, value_usd) %>%
  # Arrange
  arrange(port_complex, port, desc(year))

# Analyze data
# In what locations, are the most fish caught from 2010-2020?
results <- data %>%
  # Calculate total landings/value of all species by port and year
  group_by(port_complex, port, year) %>%
  summarize(landings_kg_tot=sum(landings_kg),
            value_usd_tot=sum(value_usd)) %>%
  ungroup() %>%
  # Calculate average landings/value by port
  group_by(port_complex, port) %>%
  summarise(landings_kg_avg=mean(landings_kg_tot),
            value_usd_avg=mean(value_usd_tot)) %>%
  # Arrange in order of importance
  arrange(desc(landings_kg_avg))

# Plot data
ggplot(results, mapping=aes(y=port_complex, x=value_usd_avg/1e6, fill=port)) +
  geom_bar(stat="identity") +
  labs(title="Port-based revenue", y="Port complex", x="Value in dollars") +
  theme(legend.position = "none")

# Plot data
ggplot(results, mapping=aes(y=port, x=value_usd_avg/1e6)) +
  facet_grid(port_complex~., scales="free_y", space="free_y") +
  geom_bar(stat="identity") +
  labs(title="Port-based revenue", y="Port complex", x="Value in dollars") +
  theme(legend.position = "none")




