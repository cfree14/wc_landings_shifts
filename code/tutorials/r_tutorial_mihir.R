
# Load packages
library(tidyverse)


#Dir
datadir <- "data"
plotdir <- "figures"
tabledir <- "tables"

#Read data
data_orig <- read.csv(file.path(datadir, "1941_2020_ca_landings_cdfw.csv"), as.is=T)
data_orig <- readRDS(file.path(datadir, "1941_2020_ca_landings_cdfw.Rds"))

#Inspect data
nrow(data_orig)
ncol(data_orig)
head(data_orig)
summary(data_orig)
str(data_orig)
describe(data_orig)

#Inspect Numerical Columns
range(data_orig$year)
min(data_orig$year)
summary(data_orig$year)
plot(data_orig$year)

#Inspect categorical columns
sort(unique(data_orig$comm_name))
table(data_orig$type)

#Vector for years of interest
years <- 2010:2020

#Build data
data <- data_orig %>%
  #Eliminate shipments (landings only)
  filter(type == "Landings") %>%
  #Reduce to 2010-2020
  filter(year %in% years) %>%
  #Convert kg to g
  mutate(landings_g = landings_kg*1000) %>%

#Select Columns of interest
select(port_complex, port, year, comm_name, landings_kg, value_usd) %>%
#Arrange
arrange(port_complex, port, year)

view(data)

# In what locations were the most fish caught from 2010-2020?
results <- data %>%
  #Calculate the total landings/value by port and year
  group_by(port_complex, port, year) %>%
  summarize(landings_kg_tot=sum(landings_kg), values_usd_tot=sum(value_usd)) %>%
  ungroup() %>%
  #Calculate average landings/value by port
  group_by(port_complex, port) %>%
  summarize(landings_kg_ave=mean(landings_kg_tot),
            values_usd_avg = mean(values_usd_tot)) %>%

#Arrange in order of importance
arrange(desc(landings_kg_avg))

view(results)

#Plot data
ggplot(data = results, mapping = aes(y = port_complex, x = values_usd_avg/1e6, color = port)) +
  geom_bar(stat="identity") +
  labs(title="Port-based revenue", y = "Port Complex", x = "Value in $ ") +
  theme(legend.position = "none")

ggplot(data = results, mapping = aes(y = port, x = values_usd_avg/1e6)) +
  facet_grid(port_complex~.) +
  geom_bar(stat="identity") +
  labs(title="Port-based revenue", y = "Port Complex", x = "Value in $ ") +
  theme(legend.position = "none")


