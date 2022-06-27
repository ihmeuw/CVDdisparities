########################################################################################################################
# Reformats GBD draws for use in scaling US R/E mortality envelope
# Updated to use GBD 2020 versions of tools and envelopes
########################################################################################################################

# Date, functions
date <- gsub("-", "_", Sys.Date())
sum.na <- function(x) {sum2 = sum(x, na.rm=T)}

# Run ID information
run <- 434 # consistent with run_id=292 for population 1/10

# Paths
jpath <- "/FILEPATH/"
# mort.draws <- "/FILEPATH/"
mort.draws <- paste0("/FILEPATH/", run, "/FILEPATH/") # no shock, no HIV, estimate_id=7
# scratch <- "/FILEPATH/"
save.dir <- "/FILEPATH/"

# Libraries
suppressMessages(library(R.utils))
library(ggplot2)
library(rhdf5)
library(matrixStats) 

# Load central functions
suppressMessages(sourceDirectory("/FILEPATH/", modifiedOnly=FALSE))

# Get location and age information
locs <- get_location_metadata(location_set_id=105, gbd_round_id=7, decomp_step="usa_re")
locs[,state2:=tstrsplit(local_id, "-", keep=2)]
re <- unlist(locs[location_type=="ethnicity", .(location_id)], use.names=F)
us <- unlist(locs[location_id==102 | parent_id==102, .(location_id)], use.names=F)

age_info <- get_age_metadata(age_group_set_id=19, gbd_round_id=7)
names(age_info) <- gsub("_group_years", "", names(age_info))
babies <- unlist(age_info[age_start<1,.(age_group_id)], use.names=F) #Under 1
kids <- unlist(age_info[age_start>=1 & age_start<5, .(age_group_id)], use.names=F) #1 to 4
old <- unlist(age_info[age_start>=85, .(age_group_id)], use.names=F)
gbd.ages <- c(age_info$age_group_id, 28, 5, 160)

# Set year range
years <- 1980:2022

# POPULATION
# 1/10/21: using population version 292; stored in release_id=15
print("Getting population")
pop <- get_population(age_group_id='all', location_id=us, sex_id=c(1,2), release_id=15, year_id=years)
pop[,c("run_id"):=NULL]

# Generate aggregates
pop.85 <- pop[age_group_id %in% old, lapply(.SD, sum), by=.(location_id,year_id,sex_id), .SDcols=c("population")]
pop.85[, age_group_id:=160]

pop <- rbind(pop, pop.85)
setnames(pop, "population", "population.292")

# DEATHS
# Read in draws for deaths from GBD, convert to mortality rates and save one file per location
years.list <- list()

for (year in years)
	{
		print(year)
		f <- paste0(mort.draws, "combined_env_aggregated_", year, ".h5")
		dt.year <- data.table(h5read(f,"/FILEPATH/"))
		dt.year <- dt.year[age_group_id %in% gbd.ages & sex_id %in% c(1,2) & location_id %in% us] #subset to GBD age groups
		cols <- c(grep("pop", names(dt.year), value=T), grep("env_", names(dt.year), value=T))
		dt.year.85 <- dt.year[age_group_id %in% old, lapply(.SD, sum), by=.(location_id,year_id,sex_id), .SDcols=cols]
		dt.year.85[, age_group_id:=160]
		dt.year <- rbind(dt.year, dt.year.85)
		dt.year <- merge(dt.year, pop, by=c("age_group_id", "sex_id", "location_id", "year_id"))
		setnames(dt.year, "location_id", "parent_id")
		setnames(dt.year, "pop", "parent_population")
		years.list[[year]] <- dt.year
	}
all <- rbindlist(years.list)

for (state in us)
	{
		dt.state <- all[parent_id==state]
		saveRDS(dt.state, file=paste0(save.dir, "FILEPATH/gbd_noshock_nohiv_", run, "_", state, ".rds"))
	}