########################################################################################################################
# Takes results of parallel LE jobs and combines into one file for further analysis/plotting
########################################################################################################################

# Date, functions
date <- "2022_02_15"
sum.na <- function(x) {sum2 = sum(x, na.rm=T)}
`%ni%` <- Negate(`%in%`)

# Paths
summaries.dir <- "/FILEPATH/"
draws.dir <- "/FILEPATH/"

# Libraries
suppressMessages(library(R.utils))
library(ggplot2)
library(rhdf5)
library(matrixStats) 

# Load central functions
suppressMessages(sourceDirectory("/FILEPATH/", modifiedOnly=FALSE))
source("/FILEPATH/le_functions_sae.R")

# Pull locations to make groups for aggregation
locs <- get_location_metadata(location_set_id=105, gbd_round_id=7, decomp_step="usa_re")
ages <- get_age_metadata(age_group_set_id=19, gbd_round_id=7)

# Main files; does not include national aggregate by r/e
file_list <- list.files(path = summaries.dir, pattern = date, full.names = T)
if(paste0(summaries.dir, "/life_expectancy_", date, ".rds")%in%file_list & paste0(summaries.dir, "/summaries_", date, ".rds")%in%file_list){
  # if combined file already exists, pull it in
  le.complete <- rbindlist(lapply(paste0(summaries.dir, "/life_expectancy_", date, ".rds"), readRDS))
  le.complete <- le.complete[!location_id%in%c(1021, 1022, 1023, 1024)]
  summary.complete <- rbindlist(lapply(paste0(summaries.dir, "/summaries_", date, ".rds"), readRDS))
} else {
  files.le <- grep("life_expectancy_", file_list, value=T)
  files.summary <- grep("summaries_", file_list, value=T)
  
  le.complete <- rbindlist(lapply(files.le, readRDS))
  summary.complete <- rbindlist(lapply(files.summary, readRDS))
}

# Generate national aggregates by r/e
files.draws <- list.files(path=draws.dir, pattern='mx_draws_', full.names=T)

overall <- list()
for(state in files.draws) {
		print(state)
		dt.overall <- readRDS(state)
		dt.overall <- dt.overall[location_id>600]
		dt.overall[,paste0("deaths_", 0:999):=lapply(FUN=function(x) get(paste0("scaled_", x)) * population, X=0:999), by=c("age_group_id", "sex_id", "year_id")]
		dt.overall[,grep("scaled_", names(dt.overall)):=NULL]

		overall[[state]] <- dt.overall
}
le.agg <- rbindlist(overall)

# Generate aggregates by r/e at the national level
print("Aggregation")
le.agg <- merge(le.agg, locs[,.(location_id, location_name)], by="location_id")
le.agg[,group:=tstrsplit(location_name, "; ", keep=2)]
cols <- c(grep("deaths_", names(le.agg), value=T), "population")
le.agg.national <- le.agg[, lapply(.SD, sum), by=.(year_id, sex_id, age_group_id), .SDcols=cols]
le.agg.national[, `:=` (group = 'National-level', location_id = 1020)]
le.agg <- le.agg[, lapply(.SD, sum), by=.(year_id, sex_id, age_group_id, group), .SDcols=cols]
le.agg[,location_id:=ifelse(group=="Hispanic, Any race", 1021, ifelse(group=="Non-Hispanic, Black", 1022, 
					 ifelse(group=="Non-Hispanic, Other races", 1023, ifelse(group=="Non-Hispanic, White", 1024, NA))))]
le.agg <- rbind(le.agg, le.agg.national)
le.agg[, paste0("scaled_", 0:999):=lapply(FUN=function(x) get(paste0("deaths_", x))/population, X=0:999), by=.(age_group_id, sex_id, group, year_id)]
le.agg[, grep("deaths_", names(le.agg)):=NULL]

# Calculate life expectancy for the national r/e aggregates
# Merge on age group information
print("Calculating life expectancy")
le.agg <- merge(le.agg, ages[,.(age_group_id, age_group_years_start)], by="age_group_id", all.x=T)
setnames(le.agg, "age_group_years_start", "age_start")
le.agg[,age_start:=ifelse(age_group_id==28, 0, ifelse(age_group_id==5, 1, ifelse(age_group_id==160, 85, age_start)))]
le.years <- unique(unlist(le.agg$year_id, use.names=F))
le.sexes <- unique(unlist(le.agg$sex_id, use.names=F))
le.locations <- unique(unlist(le.agg$location_id, use.names=F))

dt.years <- list()
dt.sexes <- list()
dt.locs <- list()

for (l in le.locations)
	{
		for (year in le.years)
			{	
				for (s in le.sexes)
					{ 
						dt.le <- le.agg[sex_id==s & year_id==year & location_id==l & age_group_id %ni% c(2,3,388,389,22,238,34,31,32,235)]
						dt.le2 <- le.agg[sex_id==s & year_id==year & location_id==l & age_group_id %ni% c(2,3,388,389,22,238,34,160)]
						
						dt.le <- dt.le[order(age_start)]
						dt.le2 <- dt.le2[order(age_start)]
												
						le.t <- dt.le[, lapply(.SD, function(x) lifetable(unlist(x), sex=s, graduation=T, extrap=T)), .SDcols=patterns("scaled_")]
						le.t[,settings:="true, 160"]
						le.f <- dt.le[, lapply(.SD, function(x) lifetable(unlist(x), sex=s, graduation=T, extrap=F)), .SDcols=patterns("scaled_")]
						le.f[,settings:="false, 160"]
						
						le.t2 <- dt.le2[, lapply(.SD, function(x) lifetable(unlist(x), sex=s, graduation=F, extrap=T)), .SDcols=patterns("scaled_")]
						le.t2[,settings:="true, 235"]
						le.f2 <- dt.le2[, lapply(.SD, function(x) lifetable(unlist(x), sex=s, graduation=F, extrap=F)), .SDcols=patterns("scaled_")]
						le.f2[,settings:="false, 235"]
						
						le <- rbind(le.t,le.f, le.t2, le.f2)
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
le.agg.complete <- rbindlist(dt.locs)

# Generate percent change
print("Percent change")
le.pc.1990 <- le.agg.complete[age == 0 & year_id == 1990]
le.pc.2019 <- le.agg.complete[age == 0 & year_id == 2019]
indices <- grep("scaled_", names(le.agg.complete))
setnames(le.pc.1990, indices, paste0(names(le.pc.1990)[indices], '.1990')) 
setnames(le.pc.2019, indices, paste0(names(le.pc.2019)[indices], '.2019'))

le.pc.agg <- merge(le.pc.2019[, -c("year_id")], le.pc.1990[, -c("year_id")], by = c("age", "sex_id", "location_id", "settings"), all = TRUE)

le.pc.agg[, paste0("pc_", 0:999):=lapply(FUN=function(x) ((get(paste0("scaled_", x, ".ex.2019")) - get(paste0("scaled_", x, ".ex.1990")))/get(paste0("scaled_", x, ".ex.1990"))), X = 0:999), 
		by = c("age", "sex_id", "location_id", "settings")]
le.pc.agg[, grep("scaled_", names(le.pc.agg)) := NULL]

# Make summaries
print("Summaries")
le.pc.agg[, c("mean", "lower", "upper") := list(rowMeans(.SD), rowQuantiles(as.matrix(.SD), probs=0.025), 
			rowQuantiles(as.matrix(.SD), probs=0.975)), .SDcols=patterns("pc_")]
le.pc.agg[, grep("pc_", names(le.pc.agg)) := NULL]
le.pc.agg[,type:="percent change, 1990 to 2019"]

le.agg.complete[, c("mean", "lower", "upper") := list(rowMeans(.SD), rowQuantiles(as.matrix(.SD), probs=0.025), 
			rowQuantiles(as.matrix(.SD), probs=0.975)), .SDcols=patterns("scaled_")]
le.agg.complete[, type:="mean value"]
le.agg.complete[, grep("scaled_", names(le.agg.complete)) := NULL]

print("Saving final dataset")
final <- rbind(le.complete, le.agg.complete, le.pc.agg, fill=T)
saveRDS(final, file=paste0(summaries.dir, "life_expectancy_", date, ".rds"))

print("Saving summary data file")
saveRDS(summary.complete, file=paste0(summaries.dir, "summaries_", date, ".rds"))
