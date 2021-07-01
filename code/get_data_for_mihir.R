
# Mexico data
###########################################################

# Directories
datadir <- "data"
plotdir <- "figures"

# Read data
mexdir <- "/Users/cfree/Dropbox/Chris/UCSB/projects/wc_cc_synthesis/data/landings/mexico/datamares/confidential/processed/"
data_mex <- readRDS(file.path(mexdir, "2001_2020_mexico_landings_datamares.Rds"))
office_mex <- read.csv(file.path(mexdir, "office_key.csv"), as.is = T)
spp_mex <- read.csv(file.path(mexdir, "species_key.csv"), as.is = T)


# Export data
write.csv(data_mex, file=file.path(datadir, "2001_2020_mexico_landings_datamares.csv"), row.names = F)
write.csv(office_mex, file=file.path(datadir, "mexico_fishery_office_key.csv"), row.names = F)
write.csv(spp_mex, file=file.path(datadir, "mexico_species_key.csv"), row.names = F)


# Export data as RDS files
saveRDS(data_mex, file=file.path(datadir, "2001_2020_mexico_landings_datamares.Rds"))
saveRDS(office_mex, file=file.path(datadir, "mexico_fishery_office_key.csv"))
saveRDS(spp_mex, file=file.path(datadir, "mexico_species_key.csv"))

# California data
###########################################################

# Get data
cdfw_ports <- wcfish::cdfw_ports
pacfin_all6 <- wcfish::pacfin_all6
cdfw_ports_key <- wcfish::ports
pacfin_ports <- wcfish::pacfin_ports

# Export data
write.csv(cdfw_ports, file=file.path(datadir, "1941_2020_ca_landings_cdfw.csv"), row.names = F)
write.csv(pacfin_all6, file=file.path(datadir, "1980_2020_wa_or_ca_landings_pacfin.csv"), row.names = F)
write.csv(cdfw_ports_key, file=file.path(datadir, "cdfw_ports_key.csv"), row.names = F)
write.csv(pacfin_ports, file=file.path(datadir, "pacfin_ports_key.csv"), row.names = F)

# Export data as RDS files
saveRDS(cdfw_ports, file=file.path(datadir, "1941_2020_ca_landings_cdfw.Rds"))
saveRDS(pacfin_all6, file=file.path(datadir, "1980_2020_wa_or_ca_landings_pacfin.Rds"))
saveRDS(cdfw_ports_key, file=file.path(datadir, "cdfw_ports_key.Rds"))
saveRDS(pacfin_ports, file=file.path(datadir, "pacfin_ports_key.Rds"))







