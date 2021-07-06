
# Directories
datadir <- "data"
plotdir <- "figures"

# Read data
list.files(datadir)
data_orig <- readRDS(file.path(datadir, "2001_2020_mexico_landings_datamares.Rds"))

# Read fishery office key
office_key <- read.csv(file.path(datadir, "mexico_fishery_office_key.csv"), as.is=T)
