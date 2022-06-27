########################################################################################################################
# Takes results of parallel LE jobs and combines into one file for further analysis/plotting
########################################################################################################################

table_metrics <- function(settings_val='false, 160', drop_low_deaths=F, drop_col_name=NA, drop_low_death_file=NA, keep_locs=seq(53709, 53912)){
  
  # Date, functions
  date <- "2022_02_15"
  sum.na <- function(x) {sum2 = sum(x, na.rm=T)}
  
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
  
  # Draw files
  message(paste0("Pulling in all files in ", draws.dir))
  files.draws <- list.files(path=draws.dir, pattern="le_draws", full.names=T)
  
  # Read in and combine
  overall <- rbindlist(lapply(files.draws, readRDS))
  overall <- overall[settings==settings_val&location_id%in%keep_locs]
  overall <- merge(overall, locs[,.(location_id, location_name)], by="location_id")
  overall[,state:=tstrsplit(location_name, "; ", keep=1)]
  overall[,group:=tstrsplit(location_name, "; ", keep=2)]
  
  # Merge in locs to drop and subset
  if(drop_low_deaths){
    if(drop_low_death_file%like%'.csv'){
      drop_data <- fread(paste0(scratch, drop_low_death_file))
    } else {
      drop_data <- read.xlsx(paste0(scratch, drop_low_death_file))
      drop_data <- as.data.table(drop_data)
    }
    
    q <- quote(drop_col_name)
    cols <- c("location_id", eval(q))
    overall <- merge(overall, drop_data[, ..cols], by = c("location_id"), all.x = TRUE)
    
    overall <- overall[get(drop_col_name)==0]
    overall[, paste0(drop_col_name) := NULL]
  }

  # # Min
  # message('Calculating minimum')
  # dt.min <- copy(overall)
  # dt.min <- unique(dt.min[, paste0("min_", 0:999):=lapply(FUN=function(x) min(get(paste0("scaled_", x, ".ex"))), X = 0:999), 
  #                         by = c("age", "sex_id", "group", "year_id")])
  # dt.min[, c("mean", "lower", "upper") := list(rowMeans(.SD), rowQuantiles(as.matrix(.SD), probs=0.025), 
  #                                              rowQuantiles(as.matrix(.SD), probs=0.975)), .SDcols=patterns("min_")]
  # dt.min[, grep("min_", names(dt.min)) := NULL]
  # dt.min[, grep("scaled_", names(dt.min)) := NULL]
  # dt.min[,type:="min"]
  # 
  # # Max
  # message('Calculating maximum')
  # dt.max <- copy(overall)
  # dt.max <- unique(dt.max[, paste0("max_", 0:999):=lapply(FUN=function(x) max(get(paste0("scaled_", x, ".ex"))), X = 0:999), 
  #                         by = c("age", "sex_id", "group", "year_id")])
  # dt.max[, c("mean", "lower", "upper") := list(rowMeans(.SD), rowQuantiles(as.matrix(.SD), probs=0.025), 
  #                                              rowQuantiles(as.matrix(.SD), probs=0.975)), .SDcols=patterns("max_")]
  # dt.max[, grep("max_", names(dt.max)) := NULL]
  # dt.max[, grep("scaled_", names(dt.max)) := NULL]
  # dt.max[,type:="max"]
  # 
  # 95th 
  message('Calculating 95th percentile')
  dt.p95 <- copy(overall)
  dt.p95 <- unique(dt.p95[, paste0("p95_", 0:999):=lapply(FUN=function(x) quantile(get(paste0("scaled_", x, ".ex")), 0.95 ), X = 0:999), 
                          by = c("age", "sex_id", "group", "year_id")])
  dt.p95[, c("mean", "lower", "upper") := list(rowMeans(.SD), rowQuantiles(as.matrix(.SD), probs=0.025), 
                                               rowQuantiles(as.matrix(.SD), probs=0.975)), .SDcols=patterns("p95_")]
  dt.p95[, grep("p95_", names(dt.p95)) := NULL]
  dt.p95[, grep("scaled_", names(dt.p95)) := NULL]
  dt.p95[,type:="p95"]
  
  # 5th
  message('Calculating 5th percentile')
  dt.p5 <- copy(overall)
  dt.p5 <- unique(dt.p5[, paste0("p5_", 0:999):=lapply(FUN=function(x) quantile(get(paste0("scaled_", x, ".ex")), 0.05 ), X = 0:999), 
                        by = c("age", "sex_id", "group", "year_id")])
  dt.p5[, c("mean", "lower", "upper") := list(rowMeans(.SD), rowQuantiles(as.matrix(.SD), probs=0.025), 
                                              rowQuantiles(as.matrix(.SD), probs=0.975)), .SDcols=patterns("p5_")]
  dt.p5[, grep("p5_", names(dt.p5)) := NULL]
  dt.p5[, grep("scaled_", names(dt.p5)) := NULL]
  dt.p5[,type:="p5"]
  
  # Mean
  message('Calculating mean')
  dt.mean <- copy(overall)
  dt.mean <- unique(dt.mean[, paste0("mean_", 0:999):=lapply(FUN=function(x) mean(get(paste0("scaled_", x, ".ex"))), X = 0:999), 
                            by = c("age", "sex_id", "group", "year_id")])
  dt.mean[, c("mean", "lower", "upper") := list(rowMeans(.SD), rowQuantiles(as.matrix(.SD), probs=0.025), 
                                                rowQuantiles(as.matrix(.SD), probs=0.975)), .SDcols=patterns("mean_")]
  dt.mean[, grep("mean_", names(dt.mean)) := NULL]
  dt.mean[, grep("scaled_", names(dt.mean)) := NULL]
  dt.mean[,type:="mean"]
  
  # dt <- rbind(dt.mean, dt.min, dt.max, dt.p95, dt.p5)
  dt <- rbind(dt.mean, dt.p95, dt.p5)
  dt[,c("location_id", "settings", "location_name", "state"):=NULL]
  dt <- dt[is.na(group)==F]
  
  dt <- unique(dt)
  
  if (!"sex" %in% names(dt) & "sex_id" %in% names(dt)) {
    sex_info <- get_ids("sex")
    dt <- merge(dt, sex_info, by = "sex_id", all.x = TRUE)
  }
  
  return(dt)
  
}


