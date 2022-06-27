# Code to generate drops for LE values

# Date, functions
date <- "2022_02_15"
sum.na <- function(x) {sum2 = sum(x, na.rm=T)}
`%ni%` <- Negate(`%in%`)

# Paths
jpath <- "/FILEPATH/"
data.dir <- paste0(jpath, "/FILEPATH/")
plot.dir <- paste0(jpath, "/FILEPATH/")
code.dir <- "/FILEPATH/" 
scratch <- "/FILEPATH/"

# Libraries
suppressMessages(library(R.utils))
library(openxlsx)
library(ggplot2)
library(rhdf5)
library(matrixStats) 
suppressMessages(library(data.table))

# Source central functions
suppressMessages(sourceDirectory("/FILEPATH/", modifiedOnly=FALSE))
source("/FILEPATH/utility.r")

# ST-GPR run_id
years <- c(1990:2019)

# Pull location and age group information
locs <- get_location_metadata(location_set_id=105, release_id=8)
locs[,state2:=gsub("US-", "", local_id)]
ages <- get_age_metadata(age_group_set_id=19, gbd_round_id=7)
babies <- unlist(ages[age_group_years_end<=1,.(age_group_id)], use.names=F)

#######RAW DATA#######
# Read in raw data
claude <- fread('/FILEPATH/all_deaths_20Sep2021.csv')
claude.all <- claude[, lapply(.SD, sum), by=.(location_id, year_id, sex_id, age_group_id), .SDcols=("deaths")]
claude.all[,c("nid", "extract_type_id"):=NULL]
claude.babies <- claude.all[age_group_id %in% babies, lapply(.SD, sum.na), by=.(location_id, sex_id, year_id), .SDcols="deaths"]
claude.babies[,age_group_id:=28]
claude.all <- rbind(claude.all, claude.babies)
claude.all <- merge(claude.all, locs[,.(location_id, parent_id)], by="location_id")
claude.all <- claude.all[year_id %in% years] #subsets to just the years of interest

# Leaving as potentially useful for the future; right now subsetting to 1990+
# Pull in information about when states started asking about hispanic ethnicity on their death certficates
# year_exclusions <- data.table(read.xlsx(paste0(jpath, "FILEPATH/dc_hisp_missingness.xlsx")))
# year_exclusions <- merge(year_exclusions, locs[, .(location_id, state2)], by="state2")

# Merge and drop years
# claude.drop <- merge(claude.all, year_exclusions, by.x="parent_id", by.y="location_id")
# claude.drop <- claude.drop[year_id>year_last]

# Find max number of deaths in each year across all age groups
max.sex <- claude.all[age_group_id %ni% babies, lapply(.SD, max), by=.(location_id, sex_id), .SDcols="deaths"]
max.sex[,claude10:=ifelse(deaths<10, 1, 0)]
max.sex[,claude15:=ifelse(deaths<15, 1, 0)]

claude.max <- max.sex[,lapply(.SD, sum), by=.(location_id), .SDcols=c("claude10", "claude15")]

# Read in scaled deaths
# scaled <- readRDS(file=paste0(scratch, "death_mort_pop_", run_id, ".rds"))
scaled <- readRDS(file=paste0(scratch, "/FILEPATH/summaries_", date, ".rds"))

scaled[,scaled.deaths.mean:=mean_mortality_scaled*population]
max.scaled <- scaled[age_group_id %ni% c(babies,5,160,22,27), lapply(.SD, max), by=.(location_id, sex_id), .SDcols="scaled.deaths.mean"]
max.scaled[,scaled10:=ifelse(scaled.deaths.mean<10, 1, 0)]
max.scaled[,scaled15:=ifelse(scaled.deaths.mean<15, 1, 0)]

scaled.max <- max.scaled[,lapply(.SD, sum), by=.(location_id), .SDcols=c("scaled10", "scaled15")]

# Merge versions together
to.drop <- merge(claude.max, scaled.max, by=c("location_id"))
to.drop <- merge(to.drop, locs[,.(location_id, location_name)], by="location_id")
to.drop[,state:=tstrsplit(location_name, split="; ", keep=1)]
to.drop[,group:=tstrsplit(location_name, split="; ", keep=2)]

# Write files
file <- paste0("locs_to_drop_2019_", date)
# write.csv(to.drop, paste0(scratch, file, ".csv"))
# write.xlsx(to.drop, paste0(jpath, "/FILEPATH/", file, ".xlsx"))


pop <- get_population(location_set_id=105, location_id="all", age_group_id="all", sex_id=c(1,2), year_id=1990:2019, 
            gbd_round_id=7, decomp_step="usa_re")
max.sex <- pop[age_group_id %ni% babies & age_group_id %ni% kids & age_group_id %ni% c(22,160), lapply(.SD, max), by=.(location_id, sex_id), .SDcols="population"]
max.sex <- merge(max.sex, locs[,.(location_id, location_name)], by="location_id")
max.sex[,state:=tstrsplit(location_name, "; ", keep=1)]
max.sex[,group:=tstrsplit(location_name, "; ", keep=2)]


max.old <- pop[age_group_id==160, lapply(.SD,max), by=.(location_id, sex_id), .SDcols="population"]
max.old <- reshape(max.old, idvar="location_id", timevar="sex_id", direction="wide")

# Read in file based on numbers of deaths
deaths <- fread("/FILEPATH/locs_to_drop_2019_2022_02_15.csv")

# Merge and look at what gets dropped
dt <- merge(deaths, max.old, by=("location_id"))
dt[,group:=tstrsplit(location_name, "; ", keep=2)]

dt[claude10!=0]
dt[population.1<500 | population.2 <500, .(location_name, claude10, claude15, population.1, population.2)]
dt[(population.1<500 | population.2 <500) & claude10==0, .(location_name, claude10, claude15, population.1, population.2)]
dt[(population.1<500 | population.2 <500) | claude10==1, .(location_name, claude10, claude15, population.1, population.2)]
dt[,drop:=ifelse(population.1<500 | population.2 <500 | claude10==1, 1, 0)]

write.xlsx(dt, file=paste0("/FILEPATH/le_drops_", date, ".xlsx"))
