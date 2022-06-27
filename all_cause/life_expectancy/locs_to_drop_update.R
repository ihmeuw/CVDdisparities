# Code to generate drops for LE values

# Date, functions
date <- gsub("-", "_", Sys.Date())
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

# Pull location and age group information
locs <- get_location_metadata(location_set_id=105, release_id=8)
locs[,state2:=gsub("US-", "", local_id)]
re <- unlist(subset(locs, location_type_id==17, select="location_id"), use.names=F)
ages <- get_age_metadata(age_group_set_id=19, gbd_round_id=7)
age_ids <- unlist(ages[,.(age_group_id)], use.names=F)
babies <- unlist(ages[age_group_years_end<=1,.(age_group_id)], use.names=F)
old <- unlist(ages[age_group_years_start>=85, .(age_group_id)], use.names=F)

# Years and sexes
years <- 1990:2019
sexes <- 1:2

#######RAW DATA#######
# Read in raw data
claude <- fread('/FILEPATH/all_deaths_20Sep2021.csv')
claude.all <- claude[, lapply(.SD, sum), by=.(location_id, year_id, sex_id, age_group_id), .SDcols=("deaths")]
# claude.all[,c("nid", "extract_type_id"):=NULL]
claude.old <- claude.all[age_group_id %in% old, lapply(.SD, sum.na), by=.(location_id, sex_id, year_id), .SDcols="deaths"]
claude.old[,age_group_id:=160]

# Square dataset
# dt.all <- data.table(expand.grid(location_id=re, year_id=years, age_group_id=age_ids, sex_id=sexes))
dt.all <- data.table(expand.grid(location_id=re, year_id=years, age_group_id=160, sex_id=sexes))

dt <- merge(claude.old, dt.all, by=c("location_id", "sex_id", "age_group_id", "year_id"), all.y=T)
dt[, deaths:=nafill(deaths, type = "const", fill=0)]

# Main text
max <- dt[age_group_id==160 & year_id %in% c(1990:1999), lapply(.SD, mean), by=.(location_id, sex_id), .SDcols="deaths"]
max <- merge(max, locs[,.(location_id, location_name)], by="location_id")
max[,group:=tstrsplit(location_name, "; ", keep=2)]
max[,claude10:=ifelse(deaths<10, 1, 0)]
max[,claude15:=ifelse(deaths<15, 1, 0)]
max[,claude20:=ifelse(deaths<20, 1, 0)]

claude.max <- max[,lapply(.SD, sum), by=.(location_id), .SDcols=c("claude10", "claude15")]

write.csv(claude.max, file=paste0("/FILEPATH/locs_to_drop_maintext_", date, ".csv"))

# Appendix
dt[,decade:=ifelse(year_id %in% c(1990:1999), "1990-1999", ifelse(year_id %in% c(2000:2009), "2000-2009", 
			ifelse(year_id %in% c(2010:2019), "2010-2019", NA)))]
max.sex <- dt[age_group_id==160, lapply(.SD, mean), by=.(location_id, sex_id, decade), .SDcols="deaths"]
max.sex <- merge(max.sex, locs[,.(location_id, location_name)], by="location_id")
max.sex[,group:=tstrsplit(location_name, "; ", keep=2)]
max.sex[,claude10:=ifelse(deaths<10, 1, 0)]
max.sex[,claude15:=ifelse(deaths<15, 1, 0)]
max.sex[,claude20:=ifelse(deaths<20, 1, 0)]

write.csv(max.sex, file=paste0("/FILEPATH/locs_to_drop_appendix_", date, ".csv"))


