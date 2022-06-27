########################################################################################################################
# Calculate absolute change
########################################################################################################################

# Date, functions
date <- gsub("-", "_", Sys.Date())
sum.na <- function(x) {sum2 = sum(x, na.rm=T)}

# Paths
draws.dir <- "/FILEPATH/"

# Libraries
packages <- c("data.table", "R.utils", "openxlsx", "ggplot2", "rhdf5", "matrixStats")
suppressMessages(invisible(lapply(packages, library, character.only = TRUE)))

# Load central functions
suppressMessages(sourceDirectory("/FILEPATH/", modifiedOnly=FALSE))

# Pull locations to make groups for aggregation
locs <- get_location_metadata(location_set_id=105, gbd_round_id=7, decomp_step="usa_re")
ages <- get_age_metadata(age_group_set_id=19, gbd_round_id=7)

# # Main files; does not include national aggregate by r/e
# file_list <- list.files(path = summaries.dir, pattern = date, full.names = T) 
# files.le <- grep("life_expectancy_", file_list, value=T)
# files.summary <- grep("summaries_", file_list, value=T)

# le.complete <- rbindlist(lapply(files.le, readRDS))
# summary.complete <- rbindlist(lapply(files.summary, readRDS))

# Generate national aggregates by r/e
files.draws <- list.files(path=draws.dir, pattern="le_draws*", full.names=T)

overall <- list()
for(state in files.draws)
	{
		print(state)
		dt.overall <- readRDS(state)
		
		# Generate file for absolute change
		le.pc.1990 <- dt.overall[age %in% c(0,25,65) & year_id == 1990]
		le.pc.2000 <- dt.overall[age %in% c(0,25,65) & year_id == 2000]
		le.pc.2019 <- dt.overall[age %in% c(0,25,65) & year_id == 2019]
		indices <- grep("scaled_", names(dt.overall))
		setnames(le.pc.1990, indices, paste0(names(le.pc.1990)[indices], '.1990'))
		setnames(le.pc.2000, indices, paste0(names(le.pc.2000)[indices], '.2000')) 
		setnames(le.pc.2019, indices, paste0(names(le.pc.2019)[indices], '.2019'))

		# 1990 to 2019
		le.pc.first <- merge(le.pc.2019[, -c("year_id")], le.pc.1990[, -c("year_id")], by = c("age", "sex_id", "location_id", "settings"), all = TRUE)

		le.pc.first[, paste0("abs_", 0:999):=lapply(FUN=function(x) (get(paste0("scaled_", x, ".ex.2019")) - get(paste0("scaled_", x, ".ex.1990"))), X = 0:999), 
				by = c("age", "sex_id", "location_id", "settings")]
		le.pc.first[, grep("scaled_", names(le.pc.first)) := NULL]

		le.pc.first[, c("mean", "lower", "upper") := list(rowMeans(.SD), rowQuantiles(as.matrix(.SD), probs=0.025), 
				rowQuantiles(as.matrix(.SD), probs=0.975)), .SDcols=patterns("abs_")]
		le.pc.first[, grep("abs_", names(le.pc.first)) := NULL]
		le.pc.first[,type:="1990 to 2019"]

		# 2000 to 2019
		le.pc.second <- merge(le.pc.2019[, -c("year_id")], le.pc.2000[, -c("year_id")], by = c("age", "sex_id", "location_id", "settings"), all = TRUE)

		le.pc.second[, paste0("abs_", 0:999):=lapply(FUN=function(x) (get(paste0("scaled_", x, ".ex.2019")) - get(paste0("scaled_", x, ".ex.2000"))), X = 0:999), 
				by = c("age", "sex_id", "location_id", "settings")]
		le.pc.second[, grep("scaled_", names(le.pc.second)) := NULL]

		le.pc.second[, c("mean", "lower", "upper") := list(rowMeans(.SD), rowQuantiles(as.matrix(.SD), probs=0.025), 
			rowQuantiles(as.matrix(.SD), probs=0.975)), .SDcols=patterns("abs_")]
		le.pc.second[, grep("abs_", names(le.pc.second)) := NULL]
		le.pc.second[,type:="2000 to 2019"]

		change <- rbind(le.pc.first, le.pc.second)

		overall[[state]] <- change
	}

abs.change <- rbindlist(overall)

saveRDS(abs.change, file=paste0("/FILEPATH/abs_change_", date, ".rds"))