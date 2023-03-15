#######################################
# Author: USERNAME, minor updates by USERNAME
# Purpose: Launch sbatches for all years for the re_cod_scaling.R script, which runs crabcorrect for cvd causes.
#######################################

rm(list = ls())

library(data.table)
library(R.utils)
# source the central functions
source("/FILEPATH/get_demographics.R")
source("/FILEPATH/get_location_metadata.R")
source("/FILEPATH/get_population.R")

#######################################
# Parameters

# GBD CodCorrect version ID to scale to
gbd_codcorrect_ver <- 297
# Whether to run as final results (direct scaling only) or to test out and compare methods (runs both direct and nested)
final <- T
# Whether to pull in the latest R/E envelope, rather than using the envelope that the COD models were run with
new_r01_env <- T

# get choices for years
dem <- get_demographics(gbd_team="cod", gbd_round_id=7)
available_yrs <- dem$year_id

#######################################

# sbatch diagnostic file paths
sbatch_output <- paste0("/FILEPATH/") 
sbatch_errors <- paste0("/FILEPATH/")
do.call(file.remove, list(list.files(sbatch_output, full.names = TRUE))) 
do.call(file.remove, list(list.files(sbatch_errors, full.names = TRUE))) 

years <- expand.grid(year = available_yrs)
year_path <- "/FILEPATH/all_years.csv"
write.csv(years, year_path, row.names=F)

rscript <- "/FILEPATH/re_cod_scaling.R" #child script
n_jobs <- nrow(years)

#######################################

# save population data outside of array job for later use
locs <- get_location_metadata(location_set_id = 105, gbd_round_id = 7, decomp_step = 'usa_re')
locs <- locs[location_id >= 1000, .(location_id)]

r01_pop <- get_population(location_id = locs$location_id,
                             year_id = available_yrs,
                             age_group_id = dem$age_group_id,
                             sex_id = c(1,2),
                             gbd_round_id = 7,
                             decomp_step = 'usa_re',
                             location_set_id = 105)

write.csv(r01_pop, "/FILEPATH/r01_pop.csv", row.names = F)

#######################################

# clean out output folders
if(final){
  do.call(file.remove, list(list.files("/FILEPATH/", full.names = TRUE))) 
  do.call(file.remove, list(list.files("/FILEPATH/", full.names = TRUE))) 
  do.call(file.remove, list(list.files("/FILEPATH/", full.names = TRUE))) 
}

#######################################
## RUNS CHILD SCRIPT IN PARALLELIZATION
#######################################

full_command <- paste0("sbatch --mem=20G -c 8 -t 1:00:00 -p all.q -A PROJECT ",
                       "-J crabcorrect ",
                       "-a ", paste0("1-", n_jobs), "%5 ",
                       "-o ", sbatch_output, "%x_%A_%a.o ",
                       "-e ", sbatch_errors, "%x_%A_%a.e ", 
                       "/FILEPATH/execRscript.sh -s ", rscript, " ", year_path, " ", gbd_codcorrect_ver, " ", final, " ", new_r01_env)
print(full_command)
system(full_command)
