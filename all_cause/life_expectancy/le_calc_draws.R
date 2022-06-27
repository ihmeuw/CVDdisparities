########################################################################################################################
# Pulls data from ST-GPR model; scales to GBD results and prepares the file for life expectancy calculation
# Updated to use GBD 2020 versions of tools and envelopes
########################################################################################################################
# Arguements for 
args <- commandArgs(trailingOnly = TRUE)

location_path <- args[1]
message(paste0("Location path is: ", location_path))
task_id <- ifelse(is.na(as.integer(Sys.getenv("SGE_TASK_ID"))), 1, as.integer(Sys.getenv("SGE_TASK_ID")))
message(paste0("Task ID is ", task_id))
all_locs <- read.table(location_path)
message(paste0("All years file type is ", class(all_locs)))
location <- all_locs[task_id,]
message(paste0("Location id is ", location))

# Date, functions
date <- gsub("-", "_", Sys.Date())
sum.na <- function(x) {sum2 = sum(x, na.rm=T)}
`%ni%` <- Negate(`%in%`)

# Paths
jpath <- "/FILEPATH/"
plot.dir <- paste0(jpath, "/FILEPATH/")
code.dir <- "/FILEPATH/"
# code.dir <- "/FILEPATH/" 
# file.dir <- paste0("/FILEPATH/", run_id, "/FILEPATH/")
file.dir <- "/FILEPATH/"
scratch <- "/FILEPATH/"
draws.dir <- "/FILEPATH/"

# Libraries
suppressMessages(library(R.utils))
library(ggplot2)
library(rhdf5)
library(matrixStats) 
library(data.table)
library(demogR, lib="/FILEPATH/")

# Load central functions
suppressMessages(sourceDirectory("/FILEPATH/", modifiedOnly=FALSE))
source("/FILEPATH/le_functions_sae.R")

# Get location and age information
locs <- get_location_metadata(location_set_id=105, gbd_round_id=7, decomp_step="usa_re")
locs[,state2:=tstrsplit(local_id, "-", keep=2)]
re <- unlist(locs[location_type=="ethnicity", .(location_id)], use.names=F)
us <- unlist(locs[location_id>105 & location_id<600, .(location_id)], use.names=F)
group <- unlist(locs[parent_id==location, .(location_id)], use.names=F)

age_info <- get_age_metadata(age_group_set_id=19, gbd_round_id=7)
names(age_info) <- gsub("_group_years", "", names(age_info))
babies <- unlist(age_info[age_start<1,.(age_group_id)], use.names=F) #Under 1
kids <- unlist(age_info[age_start>=1 & age_start<5, .(age_group_id)], use.names=F) #1 to 4
old <- unlist(age_info[age_start>=85, .(age_group_id)], use.names=F)

# Set year range
years <- 1990:2019

# Read in correction factor file
cr <- readRDS(paste0(scratch, "classification_ratios_2021_02_02.rds"))
cr <- cr[,.(age_group_id, location_id, year_start, year_end, sex_id, cr.age_sex)]

# Load population file
pop <- readRDS(file=paste0(scratch, "pop_for_le.rds"))
pop <- pop[year_id %in% years]

# Read in GBD file for scaling
gbd <- readRDS(file=paste0(scratch, "FILEPATH/gbd_noshock_nohiv_434_", location, ".rds"))
# setnames(gbd, "population", "parent.population")
	
# Read in draws from dimensional smoothing (post extending to full time series; pre-scaling)
print(location)
print(group)
file_list <- list.files(path=file.dir, pattern="predicted", full.names=T)
file_list <- grep(paste(group,collapse="|"), file_list, value=T)

print("env")
env <- rbindlist(lapply(file_list, readRDS), id=TRUE)
env[,.id:=NULL]
env <- env[year_id %in% years]
	
all <- env[, lapply(.SD, sum), by=.(location_id, year_id, sex_id), .SDcols=patterns("draw_")]
all[,age_group_id:=22]

under1 <- env[age_group_id %in% babies, lapply(.SD, sum), by=.(location_id, year_id, sex_id), .SDcols=patterns("draw_")]
under1[, age_group_id:=28]

under5 <- env[age_group_id %in% kids, lapply(.SD, sum), by=.(location_id,year_id,sex_id), .SDcols=patterns("draw_")]
under5[, age_group_id:=5]

eightyfive <- env[age_group_id %in% old, lapply(.SD, sum), by=.(location_id,year_id,sex_id), .SDcols=patterns("draw_")]
eightyfive[, age_group_id:=160]

env <- rbind(env, all, under1, under5, eightyfive)

# Merge with population file, generate mortality rate
env <- merge(env, pop, by=c("age_group_id", "sex_id", "location_id", "year_id"))
env[, paste0("mortality_", 0:999) := lapply(FUN=function(x) get(paste0("draw_", x))/population, X=0:999), 
			by=c("location_id", "age_group_id", "sex_id", "year_id")]
				
# Merge with classification ratio file, generate corrected mortality rates
# Add year_start, year_end for merging purposes
env[,year_start:=ifelse(year_id<=1998, 1990, ifelse(year_id>=1999, 1999, NA))]
env[,year_end:=ifelse(year_id<=1998, 1998, ifelse(year_id>=1999, 2011, NA))]

env <- merge(env, cr[,.(age_group_id, sex_id, location_id, year_start, year_end, cr.age_sex)], 
			by=c("age_group_id", "sex_id", "location_id", "year_start", "year_end"))
env[, paste0("cr_", 0:999) := lapply(FUN=function(x) get(paste0("mortality_", x))*cr.age_sex, X=0:999), 
			by=c("location_id", "age_group_id", "sex_id", "year_id")]
env[, paste0("cr.death_", 0:999) := lapply(FUN=function(x) get(paste0("cr_", x))*population, X=0:999), 
			by=c("location_id", "age_group_id", "sex_id", "year_id")]
env <- merge(env, locs[,.(location_id, parent_id)], by="location_id")


# Merge with GBD mortality rates and scale
dt <- merge(env, gbd, by=c("age_group_id", "sex_id", "year_id", "parent_id"))
dt[, paste0("scaled.deaths_", 0:999) := lapply(FUN = function(x) get(paste0("cr.death_", x)) * get(paste0("env_", x)) / sum(get(paste0("cr.death_", x))), X = 0:999), 
					by=c("parent_id", "age_group_id", "sex_id", "year_id")]
dt[, paste0("scaled_", 0:999) := lapply(FUN = function(x) get(paste0("scaled.deaths_", x))/population, X = 0:999), 
					by=c("parent_id", "age_group_id", "sex_id", "year_id")]

# Preserve version with everything for further diagnostics and graphing
complete <- copy(dt)
complete <- merge(complete, age_info[,.(age_group_id, age_start)], by="age_group_id", all.x=T)
complete[, age_start:=ifelse(age_group_id==28, 0, ifelse(age_group_id==5, 1, ifelse(age_group_id==160, 85, age_start)))]
complete[, c("mean_deaths_raw", "lower_deaths_raw", "upper_deaths_raw") := list(rowMeans(.SD), rowQuantiles(as.matrix(.SD), probs=0.025), 
			rowQuantiles(as.matrix(.SD), probs=0.975)), .SDcols=patterns("draw_")]
complete[, c("mean_mortality_raw", "lower_mortality_raw", "upper_mortality_raw") := list(rowMeans(.SD), rowQuantiles(as.matrix(.SD), probs=0.025), 
			rowQuantiles(as.matrix(.SD), probs=0.975)), .SDcols=patterns("mortality_")]
complete[, c("mean_mortality_cr", "lower_mortality_cr", "upper_mortality_cr") := list(rowMeans(.SD), rowQuantiles(as.matrix(.SD), probs=0.025), 
			rowQuantiles(as.matrix(.SD), probs=0.975)), .SDcols=patterns("cr_")]
complete[, c("mean_deaths_cr", "lower_deaths_cr", "upper_deaths_cr"):=list(rowMeans(.SD), rowQuantiles(as.matrix(.SD), probs=0.025), 
			rowQuantiles(as.matrix(.SD), probs=0.975)), .SDcols=patterns("cr.death_")]
complete[, c("mean_deaths_scaled", "lower_deaths_scaled", "upper_deaths_scaled"):=list(rowMeans(.SD), rowQuantiles(as.matrix(.SD), probs=0.025), 
			rowQuantiles(as.matrix(.SD), probs=0.975)), .SDcols=patterns("scaled.deaths_")]
complete[, c("mean_mortality_scaled", "lower_mortality_scaled", "upper_mortality_scaled") := list(rowMeans(.SD), rowQuantiles(as.matrix(.SD), probs=0.025), 
			rowQuantiles(as.matrix(.SD), probs=0.975)), .SDcols=patterns("scaled_")]
complete[, c("mean_gbd_deaths", "lower_gbd_deaths", "upper_gbd_deaths") := list(rowMeans(.SD), rowQuantiles(as.matrix(.SD), probs=0.025), 
			rowQuantiles(as.matrix(.SD), probs=0.975)), .SDcols=patterns("env_")]
complete[, type:="mean value"]
complete[, grep("^scaled_", names(complete)) := NULL]
complete[, grep("^draw_", names(complete)):=NULL]
complete[, grep("^mortality_", names(complete)):=NULL]
complete[, grep("^cr_", names(complete)):=NULL]
complete[, grep("^cr.death_", names(complete)):=NULL]
complete[, grep("^scaled.deaths_", names(complete)):=NULL]
complete[, grep("^env_", names(complete)):=NULL]

# Drop unneeded columns
dt[, grep("draw_", names(dt)):=NULL]
dt[, grep("mortality_", names(dt)):=NULL]
dt[, grep("cr_", names(dt)):=NULL]
dt[, grep("cr.death_", names(dt)):=NULL]
dt[, grep("scaled.deaths_", names(dt)):=NULL]
dt[, grep("env_", names(dt)):=NULL]

# Multiply back through to get deaths to sum
dt.agg <- copy(dt)
saveRDS(dt.agg, file=paste0(draws.dir, "mx_draws_", location, ".rds")) # Saves draw files for aggregation to national level race/ethnicity
dt.agg[,paste0("deaths_", 0:999):=lapply(FUN=function(x) get(paste0("scaled_", x)) * population, X=0:999), by=c("age_group_id", "sex_id", "year_id")]
cols <- c(grep("deaths_", names(dt.agg), value=T), "population")
dt.agg <- dt.agg[, lapply(.SD, sum), by=.(year_id, sex_id, age_group_id), .SDcols=cols]
dt.agg[,location_id:=location]
dt.agg[, paste0("scaled_", 0:999):=lapply(FUN=function(x) get(paste0("deaths_", x))/population, X=0:999), by=.(age_group_id, sex_id, location_id, year_id)]
dt.agg[,grep("deaths_", names(dt.agg)):=NULL]

# Bind state-level aggregate onto most-detailed locations
dt <- rbind(dt, dt.agg, fill=T)
dt <- merge(dt, age_info[,.(age_group_id, age_start)], by="age_group_id", all.x=T)
dt[,age_start:=ifelse(age_group_id==28, 0, ifelse(age_group_id==5, 1, ifelse(age_group_id==160, 85, age_start)))]

# Calculate life expectancy from mortality rates
print("Calculating LE")
le.years <- unique(unlist(dt$year_id, use.names=F))
le.sexes <- unique(unlist(dt$sex_id, use.names=F))
le.locations <- unique(unlist(dt$location_id, use.names=F))

dt.years <- list()
dt.sexes <- list()
dt.locs <- list()

for (l in le.locations)
	{
		for (year in le.years)
			{	
				for (s in le.sexes)
					{ 
						print(l)
						print(s)
						print(year)
						dt.le <- dt[sex_id==s & year_id==year & location_id==l & age_group_id %ni% c(2,3,388,389,22,238,34,31,32,235)]
						dt.le2 <- dt[sex_id==s & year_id==year & location_id==l & age_group_id %ni% c(2,3,388,389,22,238,34,160)]
						
						dt.le <- dt.le[order(age_start)]
						dt.le2 <- dt.le2[order(age_start)]
												
						# le.t <- dt.le[, lapply(.SD, function(x) lifetable(unlist(x), sex=s, graduation=T, extrap=T)), .SDcols=patterns("scaled_")]
						# le.t[,settings:="true, 160"]
						le.f <- dt.le[, lapply(.SD, function(x) lifetable(unlist(x), sex=s, graduation=T, extrap=F)), .SDcols=patterns("scaled_")]
						le.f[,settings:="false, 160"]
						
						# le.t2 <- dt.le2[, lapply(.SD, function(x) lifetable(unlist(x), sex=s, graduation=F, extrap=T)), .SDcols=patterns("scaled_")]
						# le.t2[,settings:="true, 235"]
						# le.f2 <- dt.le2[, lapply(.SD, function(x) lifetable(unlist(x), sex=s, graduation=F, extrap=F)), .SDcols=patterns("scaled_")]
						# le.f2[,settings:="false, 235"]
						
						# le <- rbind(le.t,le.f, le.t2, le.f2)
						le <- le.f
						le[,age:=scaled_0.age]
						le <- le[,c("age", "settings", grep("ex", names(le), value=T)), with=F]
						le[,year_id:=year]
						le[,sex_id:=s]
						le[,location_id:=unique(unlist(dt.le$location_id, use.names=F))]
						dt.sexes[[s]] <- le
					}
				sex.bound <- rbindlist(dt.sexes)
				dt.years[[year]] <- sex.bound
			}
		year.bound <- rbindlist(dt.years)
		dt.locs[[l]] <- year.bound
	}
le.complete <- rbindlist(dt.locs)

saveRDS(le.complete[age %in% c(0,25,65)], file=paste0(draws.dir, "le_draws_",location, "_", date, ".rds"))

# Uncertainty calculation here
# Percent change from 1990 to 2017 and 1990 to 2019
le.pc.1990 <- le.complete[age %in% c(0,25,65) & year_id == 1990]
le.pc.2000 <- le.complete[age %in% c(0,25,65) & year_id == 2000]
le.pc.2019 <- le.complete[age %in% c(0,25,65) & year_id == 2019]
indices <- grep("scaled_", names(le.complete))
setnames(le.pc.1990, indices, paste0(names(le.pc.1990)[indices], '.1990'))
setnames(le.pc.2000, indices, paste0(names(le.pc.2000)[indices], '.2000')) 
setnames(le.pc.2019, indices, paste0(names(le.pc.2019)[indices], '.2019'))

# 1990 to 2019
le.pc.first <- merge(le.pc.2019[, -c("year_id")], le.pc.1990[, -c("year_id")], by = c("age", "sex_id", "location_id", "settings"), all = TRUE)

le.pc.first[, paste0("pc_", 0:999):=lapply(FUN=function(x) ((get(paste0("scaled_", x, ".ex.2019")) - get(paste0("scaled_", x, ".ex.1990")))/get(paste0("scaled_", x, ".ex.1990"))), X = 0:999), 
		by = c("age", "sex_id", "location_id", "settings")]
le.pc.first[, grep("scaled_", names(le.pc.first)) := NULL]

le.pc.first[, c("mean", "lower", "upper") := list(rowMeans(.SD), rowQuantiles(as.matrix(.SD), probs=0.025), 
			rowQuantiles(as.matrix(.SD), probs=0.975)), .SDcols=patterns("pc_")]
le.pc.first[, grep("pc_", names(le.pc.first)) := NULL]
le.pc.first[,type:="percent change, 1990 to 2019"]

# 2000 to 2019
le.pc.second <- merge(le.pc.2019[, -c("year_id")], le.pc.2000[, -c("year_id")], by = c("age", "sex_id", "location_id", "settings"), all = TRUE)

le.pc.second[, paste0("pc_", 0:999):=lapply(FUN=function(x) ((get(paste0("scaled_", x, ".ex.2019")) - get(paste0("scaled_", x, ".ex.2000")))/get(paste0("scaled_", x, ".ex.2000"))), X = 0:999), 
		by = c("age", "sex_id", "location_id", "settings")]
le.pc.second[, grep("scaled_", names(le.pc.second)) := NULL]

le.pc.second[, c("mean", "lower", "upper") := list(rowMeans(.SD), rowQuantiles(as.matrix(.SD), probs=0.025), 
			rowQuantiles(as.matrix(.SD), probs=0.975)), .SDcols=patterns("pc_")]
le.pc.second[, grep("pc_", names(le.pc.second)) := NULL]
le.pc.second[,type:="percent change, 2000 to 2019"]

le.complete[, c("mean", "lower", "upper") := list(rowMeans(.SD), rowQuantiles(as.matrix(.SD), probs=0.025), 
			rowQuantiles(as.matrix(.SD), probs=0.975)), .SDcols=patterns("scaled_")]
le.complete[, type:="mean value"]
le.complete[, grep("scaled_", names(le.complete)) := NULL]

le.complete <- rbind(le.complete, le.pc.first, le.pc.second, fill=T)
saveRDS(le.complete, file=paste0(scratch, "FILEPATH/life_expectancy_", location, "_", date, ".rds"))
saveRDS(complete, file=paste0(scratch, "FILEPATH/summaries_", location, "_", date, ".rds"))
