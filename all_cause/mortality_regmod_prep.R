##Generate file with all-cause mortality envelope

rm(list = setdiff(ls(), c(lsf.str(), "jpath", "hpath", "os")))

# Load libraries
library(ggplot2)
suppressMessages(library(dplyr))
library(openxlsx)
suppressMessages(library(R.utils))
library(feather)
library(lme4)
library(binom)

# Date and log information
date <- gsub("-", "_", Sys.Date())
logs <- "/FILEPATH/"

# Directories
jpath <- "/FILEPATH/"
plot_dir <- paste0(jpath, "/FILEPATH/")

# Functions
sum.na <- function(x) {sum2 = sum(x, na.rm=T)}
prop <- function(x) {x/sum(x)}
`%ni%` <- Negate(`%in%`)

# Source central functions
suppressMessages(sourceDirectory("/FILEPATH/", modifiedOnly=FALSE))

# Pull necessary demographic information
locs <- get_location_metadata(location_set_id=105, decomp_step="usa_re", gbd_round_id=7)
locs[,state2:=gsub("US-", "", local_id)]
states <- unlist(locs[location_type=="admin1", "location_id"], use.names=F)
us.all <- unlist(locs[location_type=="admin1" | location_type=="admin0", "location_id"], use.names=F)
re.info <- locs[location_type=="ethnicity", c("location_id", "parent_id", "ihme_loc_id", "location_name")]
re <- unlist(subset(locs, location_type_id==17, select="location_id"))

age_info <- get_age_metadata(age_group_set_id=19, gbd_round_id=7)
ages <- unlist(age_info$age_group_id, use.names=F)

# Pull deaths from CoD team; aggregate to all-cause mortality
# claude.all <- fread("/FILEPATH/usa_r01_disaggregation_phase_data_for_env.csv")
claude <- fread('/FILEPATH/all_deaths_20Sep2021.csv')
claude.all <- claude[, lapply(.SD, sum), by=.(location_id, year_id, sex_id, age_group_id), .SDcols=("deaths")]

# Pull dataset with information on when Hispanic question was added to DC
year_exclusions <- data.table(read.xlsx(paste0(jpath, "/FILEPATH/dc_hisp_missingness.xlsx")))
year_exclusions <- merge(year_exclusions, locs[, .(location_id, state2)], by="state2")

# Dataset for squaring
dt.all <- data.table(location_id=rep(re, each=2150), year_id=rep(1980:2022, each=50), age_group_id=rep(ages, times=17544),
						sex_id=rep(rep(1:2, each=25), times=8772))
dt.all <- dt.all[year_id<=2019] # subset to years available in dataset

#pull population
pop <- fread(paste0(jpath, "/FILEPATH/us_re_upload_interpolated_2021_09_20.csv"))

# file with census region information
census <- setDT(read.xlsx("/FILEPATH/census_region_state.xlsx"))

# Square dataset
dt <- merge(claude.all[,c("age_group_id", "location_id", "sex_id", "year_id", "deaths")], dt.all,
				by=c("location_id", "year_id", "sex_id", "age_group_id"), all=T)
dt[, deaths:=nafill(deaths, type = "const", fill=0)]

dt <- merge(dt, pop[,c("location_id", "sex_id", "age_group_id", "year_id", "population")], 
				   by=c("location_id", "year_id", "sex_id", "age_group_id"))
dt <- merge(dt, locs[,.(location_id, location_name, parent_id)], by="location_id")
dt[,state := sapply(strsplit(as.character(location_name), ";"), `[`, 1)]
dt[,group := sapply(strsplit(as.character(location_name), "; "), `[`, 2)]
dt[,population_group_id:=factor(group, levels=c("Hispanic, Any race", "Non-Hispanic, Other races", "Non-Hispanic, Black", "Non-Hispanic, White"), 
									   labels=c(1:4))]
dt <- merge(dt, census, by.x="state", by.y="state_name")
dt <- merge(dt, year_exclusions, by.x="parent_id", by.y="location_id")
dt <- merge(dt, age_info[,.(age_group_id, age_group_years_start)], by="age_group_id")
setnames(dt, "age_group_years_start", "age")
dt <- dt[year_id>year_last]

# Generate age variable for use in setting additional leaf in RegMod
dt[,age_leaf:=paste0("age_", age_group_id)]

# Write file for saving
write.csv(dt, file=paste0("/FILEPATH/deaths_regmod_", date, ".csv"))
# write.csv(dt, file=paste0("/FILEPATH/deaths_regmod_", date, ".csv"))
