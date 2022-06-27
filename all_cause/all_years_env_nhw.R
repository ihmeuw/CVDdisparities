####################################################################################################
## Scale draws from Direct Smoothing version of all-cause mortality envelope to w/out shock
## GBD draws
####################################################################################################
rm(list=ls())

# Arguments for parallelization
args <- commandArgs(trailingOnly = TRUE)
grid_path <- args[1]
message(paste0("Array path is: ", grid_path))
task_id <- ifelse(is.na(as.integer(Sys.getenv("SGE_TASK_ID"))), 1, as.integer(Sys.getenv("SGE_TASK_ID")))
message(paste0("Task ID is ", task_id))
array <- read.csv(grid_path)
message(paste0("All years file type is ", class(grid_path)))
location <- array[task_id,1]
message(paste0("Location ID is ", location))
age_id <- array[task_id,2]
message(paste0("Age Group ID is ", age_id))

# Load libraries
suppressMessages(library(R.utils))
library(openxlsx)
suppressMessages(library(data.table))
library(matrixStats)
suppressMessages(library(forecast))
suppressMessages(library(imputeTS, lib="/FILEPATH/"))
suppressMessages(library(GauPro, lib.loc="/FILEPATH/"))

# Date and functions
date <- gsub("-", "_", Sys.Date())
sum.na <- function(x) {sum2 = sum(x, na.rm=T)}
prop <- function(x) {x/sum(x)}
`%ni%` <- Negate(`%in%`)

# Load shared functions
suppressMessages(sourceDirectory("/FILEPATH/", modifiedOnly=FALSE))

# File paths
data.dir <-  "/FILEPATH/"

# Load draws for sigma=0.01 from USERNAME
all <- fread(paste0(data.dir, "FILEPATH/sigma_p01.csv"))
setnames(all, "age", "age_start") #avoids name collision with loop later
all <- all[location_id==location & age_group_id==age_id]
sexes <- c(1,2)
draws <- unique(grep("draw_", names(all), value=T))

# Fix early years, negatives, zeros
dt.complete <- data.table(expand.grid(location_id=location, age_group_id=age_id, sex_id=unique(all$sex_id), year_id=c(1980:2019)))

to.fix <- copy(all)
# to.fix[, c("mean", "min") := list(rowMeans(.SD, na.rm=T), rowMins(as.matrix(.SD), na.rm=T)), .SDcols=patterns("draw_")]
to.fix[,c("state", "deaths", "population", "location_name", "group", "population_group_id", "region_name", "region_id", "parent_id",
          "division_name", "division_id", "fips_code", "state2", "year_last", "age_start", "age_leaf", "group_id", "dimsm_pred"):=NULL]

to.fix <- merge(to.fix, dt.complete, by=c("location_id", "sex_id", "age_group_id", "year_id"), all=T)
# to.fix[mean<=0, paste0("draw_", 0:999):=NA] # sets zeros/negative values to NA
to.fix[to.fix <= 0] <- NA

dt.fixed <- na_interpolation(to.fix, option="stine")

# Predict out 2020 to 2022
years <- c(2020:2022)
# mort.predict <- list()
sex.predict <- list()

for (s in sexes)
  {
   	dt <- data.table(year_id=years, location_id=location, sex_id=s, age_group_id=age_id)
    
    for (d in draws)
      {
        	y <- unlist(dt.fixed[sex_id==s, get(paste0(d))], use.names=F)
        	auto.mod <- auto.arima(y, allowdrift=TRUE)
        	preds <- forecast(auto.mod, length(years))
        	dt[,paste0(d):=unlist(preds$mean[1:length(years)], use.names=F)]
 		  }
		sex.predict[[s]] <- dt
  }
mort.predict <- rbindlist(sex.predict)
	
dt.final <- rbind(dt.fixed, mort.predict)
dt.summarize <- copy(dt.final)
dt.summarize[, c("mean_deaths_raw", "lower_deaths_raw", "upper_deaths_raw") := list(rowMeans(.SD), rowQuantiles(as.matrix(.SD), probs=0.025), 
      rowQuantiles(as.matrix(.SD), probs=0.975)), .SDcols=patterns("draw_")]
dt.summarize[, grep("^draw_", names(dt.summarize)) := NULL]

saveRDS(dt.summarize, file=paste0(data.dir, "FILEPATH/summarize_", location, "_", age_id, ".rds"))
saveRDS(dt.final, file=paste0(data.dir, "FILEPATH/predicted_draws_", location, "_", age_id, ".rds"))
