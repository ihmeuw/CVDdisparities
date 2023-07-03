## -------------------------------------------------------------------
## Description: Rake RegMod results from state level to national level 
## -------------------------------------------------------------------

library(ggplot2)
library(data.table)
library(matrixStats)

# Arguments #########################################################
# read in RegMod model output 
model_index <- "89" # specify model id 
variable <- "deaths" # specify births or deaths 
model_output <- fread(paste0('FILEPATH')) 

# Format the data 
model_output[model_output == ""] <- NA
model_output[,level:=ifelse(is.na(region_name)==F & is.na(race_group)==T & is.na(location_name)==T, "region",
                            ifelse(is.na(region_name)==F & is.na(race_group)==F & is.na(location_name)==T, "race_region",
                                   ifelse(is.na(region_name)==F & is.na(race_group)==F & is.na(location_name)==F, "race_region_state", NA)))]

# remove pop and death columns because they are only state-race specific 
if(variable=="deaths"){
  model_output <- subset(model_output, select = -c(pop))
  model_output <- subset(model_output, select = -c(deaths))
}

# Read in population data 
pop_df <- fread('FILEPATH') 

# Get aggregated population data by region, race/region, and national levels 
pop_df.region <- pop_df[,lapply(.SD, sum), by= .(year_id, region_name), .SDcols=c("pop", variable)]
pop_df.region_by_race <- pop_df[,lapply(.SD, sum), by=.(year_id, region_name, race_group), .SDcols=c("pop", variable)]
pop_df.national <- pop_df[,lapply(.SD, sum), by=.(year_id), .SDcols=c("pop", variable)]

# Bind together pop at region, race/region, and region/race/state levels 
pop_df <- rbind(pop_df, pop_df.region_by_race, pop_df.region, pop_df.national, fill=T)
pop_df <- pop_df[is.na(region_name), region_name := "US National"]

# Merge population and model output data frames 
model_output <- merge(model_output, pop_df[,.(year_id, region_name, race_group, location_name, pop, deaths)], by=c("year_id","region_name", "race_group", "location_name"))
model_output[region_name=="US National", country := "US National"]
model_output[country=="US National", region := NA]

# Turn rates into counts for raking 
cols <- grep("value_", names(model_output), value = TRUE)
model_output[, (cols) := lapply(.SD, '*', pop), .SDcols = cols]
model_output[, value := value*pop]

setnames(model_output, "value", "estimate")

# Rake ##############################################################
# Subset data for each cascade level 
model_output <- model_output[country=="US National", level := "national"]
national <- model_output[level=="national"]
region <- model_output[level=="region"]
race <- model_output[level=="race_region"]
state <- model_output[level=="race_region_state"]

# Rake region level to national level 
dt <- merge(region, national, by=c("year_id"), suffixes = c("_regional", "_national"))

dt[, estimate_reg_scaled := estimate_regional * estimate_national/sum(estimate_regional), by="year_id"]
dt[, paste0("value_", 0:999, "_reg_scaled") := lapply(FUN = function(x) get(paste0("value_", x, "_regional")) * get(paste0("value_", x, "_national")) / sum(get(paste0("value_", x, "_regional"))), X = 0:999), 
   by=c("year_id")]

cols <- c("year_id", grep(pattern=c("_regional"), colnames(dt), value = T), grep(pattern=c("_reg_scaled"), colnames(dt), value = T))
dt <- dt[, ..cols]
setnames(dt, cols, gsub("_regional", "", cols)) 

region <- copy(dt)

# Rake race group-region level to region level 
dt <- merge(race, dt, by=c("year_id", "country", "region_name"), suffixes = c("_race", "_regional")) 

dt[, estimate_re_scaled := estimate_race * estimate_reg_scaled/sum(estimate_race), by=c('year_id','country','region_name')]
dt[, paste0("value_", 0:999, "_re_scaled") := lapply(FUN = function(x) get(paste0("value_", x, "_race")) * get(paste0("value_", x, "_reg_scaled")) / sum(get(paste0("value_", x, "_race"))), X = 0:999), 
   by=c("year_id", "country", "region_name")] #look at this - regional scaled 

cols <- c("year_id", "region_name", grep(pattern=c("_race"), colnames(dt), value = T), grep(pattern=c("_re_scaled"), colnames(dt), value = T))
dt <- dt[, ..cols]
setnames(dt, cols, gsub("_race", "", cols)) 

race <- copy(dt)

# Rake state level to race-group region level 
dt <- merge(state, dt, by=c("year_id", "region_name", "race_group"), suffixes = c("_state", "_race")) 

dt[, estimate_st_scaled := estimate_state * estimate_re_scaled/sum(estimate_state), by=c('year_id','region_name','race_group')]
dt[, paste0("value_", 0:999, "_st_scaled") := lapply(FUN = function(x) get(paste0("value_", x, "_state")) * get(paste0("value_", x, "_re_scaled")) / sum(get(paste0("value_", x, "_state"))), X = 0:999), 
   by=c("year_id", "region_name", "race_group")]

cols <- c("year_id", "region_name", "race_group", grep(pattern=c("_state"), colnames(dt), value = T), grep(pattern=c("_st_scaled"), colnames(dt), value = T))
dt <- dt[, ..cols]
setnames(dt, cols, gsub("_state", "", cols)) 

state <- copy(dt)

# Remove unraked draws estimates and uncertainties from the data frame 
state[, paste0('value_', 0:999) := NULL]

# Save ##############################################################
# Write out state-level results to raked_outputs folder 
out_path <- paste0('FILEPATH')
fwrite(state, out_path)
