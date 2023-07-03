##-------------------------------------------------------------------
## Description: Calculate MMR from RegMod births and deaths outputs 
##-------------------------------------------------------------------

library(data.table)
library(matrixStats)

# Arguments ##############################################
collapse <- F

if (collapse){
  deaths_model <- fread('FILEPATH') 
  births_model <- fread('FILEPATH') 
} else {
  deaths_model <- fread('FILEPATH') 
  births_model <- fread('FILEPATH') 
}

# Prep the data ##########################################
# prep to merge births and deaths into one data frame
cols <- grep("value_", names(births_model), value=T)
setnames(births_model, cols, paste0(cols,"_births"))
cols <- grep("value_", names(deaths_model), value=T)
setnames(deaths_model, cols, paste0(cols,"_deaths"))

if (collapse){
  # create year id columns
  births_model[year_1999 == 1, year_id := 1999]
  births_model[year_2010 == 1, year_id := 2010]
  births_model$year_id <- as.integer(births_model$year_id)
  deaths_model[year_1999 == 1, year_id := 1999]
  deaths_model[year_2010 == 1, year_id := 2010]
  deaths_model$year_id <- as.integer(deaths_model$year_id)
  # rename valule column prior to merge 
  setnames(deaths_model, "value", "value_deaths")
  setnames(births_model, "value", "value_births")
  deaths_model[, year_1999 := NULL]
  deaths_model[, year_2010 := NULL]
  births_model[, year_1999 := NULL]
  births_model[, year_2010 := NULL]
  # merge births and deaths models into one data frame 
  combined_model <- merge(deaths_model, births_model, by=c("id", "year_id"))
} else {
  setnames(deaths_model, "estimate", "estimate_deaths")
  setnames(deaths_model, "estimate_st_scaled", "estimate_st_scaled_deaths")
  setnames(births_model, "estimate", "estimate_births")
  setnames(births_model, "estimate_st_scaled", "estimate_st_scaled_births")
  combined_model <- merge(deaths_model, births_model, by=c("id", "year_id", "region", "race_group", "state", "pop"))
}

# Calculate mmr estimates ##################################
# go into log space to calculate MMR 
cols <- grep('value_', names(combined_model), value = TRUE)
combined_model[, (cols) := lapply(.SD, 'log'), .SDcols = cols]

# calculate MMR 
if (collapse){
  combined_model[, paste0("value_", 0:999, "_mmr") := lapply(X = 0:999, FUN = function(x) (get(paste0("value_",x,"_deaths")) - get(paste0("value_",x,"_births"))))]
} else {
  combined_model[, paste0("value_", 0:999, "_mmr") := lapply(X = 0:999, FUN = function(x) (get(paste0("value_",x,"_st_scaled_deaths")) - get(paste0("value_",x,"_st_scaled_births"))))]
}

# get mean, lower, upper
combined_model[, c('mean_mmr', 'lower_mmr', 'upper_mmr') := list(rowMeans(.SD), rowQuantiles(as.matrix(.SD), probs=0.025), 
                                                                 rowQuantiles(as.matrix(.SD), probs=0.975)), .SDcols=patterns('_mmr')]
# remove death and birth columns
if (collapse){
  combined_model[, paste0('value_', 0:999, '_deaths') := NULL]
  combined_model[, paste0('value_', 0:999, '_births') := NULL]
} else {
  combined_model[, paste0('value_', 0:999, '_st_scaled_deaths') := NULL]
  combined_model[, paste0('value_', 0:999, '_st_scaled_births') := NULL]
}

# re-exponentiate 
cols <- grep('value_', names(combined_model), value = TRUE)
combined_model[, (cols) := lapply(.SD, 'exp'), .SDcols = cols]
combined_model[, mean_mmr := exp(mean_mmr)]
combined_model[, lower_mmr := exp(lower_mmr)]
combined_model[, upper_mmr := exp(upper_mmr)]

# multiply by 100000
combined_model[, mean_mmr := 100000*mean_mmr]
combined_model[, lower_mmr := 100000*lower_mmr]
combined_model[, upper_mmr := 100000*upper_mmr]
if (collapse){
  combined_model[, value_mmr := 100000*(value_deaths/value_births)]
} else {
  combined_model[, value_mmr := 100000*(estimate_st_scaled_deaths/estimate_st_scaled_births)] 
}

# remove draws before saving 
combined_model[, paste0('value_', 0:999, '_mmr') := NULL]

# save 
write.csv(combined_model, 'FILEPATH')  
