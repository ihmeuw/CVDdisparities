#######################################
## PREP ENVIRONMENT & GLOBAL VAR
#######################################

rm(list=ls())

#Load required libraries
library(data.table)
library(viridis)
library(R.utils)
library(openxlsx)
library(zip)
library(stringr)
library(ggplot2)
library(gridExtra)

# Parameters
task_id <- ifelse(is.na(as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))), 1, as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID")))
args <- commandArgs(trailingOnly = T)
year_filepath <- args[1] #years from launch_array_script
print(year_filepath)
years <- fread(year_filepath)
para_year_id <- years[task_id, year]

gbd_codcorrect_ver <- as.numeric(args[2]) # GBD codcorrect version id from launch_array_script
final <- as.logical(args[3]) # whether to run as final results (direct scaling) or to test out and compare methods (runs both direct and nested)
new_r01_env <- as.logical(args[4]) # whether to pull in the latest R/E envelope, rather than using the envelope that the COD models were run with

# Source functions
suppressMessages(sourceDirectory("/FILEPATH", modifiedOnly = F))
source('/FILEPATH/crabcorrect_function.R')

################################################################################

# Pull in R01 causes
subcauses <- get_cause_metadata(cause_set_id = 15, gbd_round_id = 7, decomp_step = 'usa_re')
subcauses <- subcauses[level >= 2]

cvd_subcauses <- subcauses[acause %like% "cvd",]
noncvd_subcauses <- subcauses[!acause %like% "cvd",]

cvd_subcauses3 <- cvd_subcauses[level==3]
cvd_subcauses4 <- cvd_subcauses[level==4]

# get all subnationals of the country that are admin2 level and get demographics
locs <- get_location_metadata(location_set_id = 105, gbd_round_id = 7, decomp_step = 'usa_re')
states <- locs[parent_id == 102, .(location_id, location_name, location_type)]
setnames(states, 'location_id', 'state_location_id')
state_re <- locs[location_id >= 1000, .(location_id, location_name, location_type)]
demographics <- get_demographics(gbd_team = "cod", gbd_round_id = 7)


if(final){
  ################################################################################
  # Run the scaling function just once for all causes using directly scaling
  ################################################################################
  
  raking(top_lev_loc = states$state_location_id,
         sublocs = state_re$location_id,
         causes = subcauses$cause_id,
         years = para_year_id,
         sum_vars = c("age_group_id", "year_id", "sex_id", "state_location_id", "cause_id"),
         save_filepath = "/FILEPATH",
         method = "direct",
         stat_type = "deaths",
         gbd_codcorrect_ver = gbd_codcorrect_ver,
         final = final,
         new_r01_env = new_r01_env)
  
} else {
  ################################################################################
  # Calling the scaling function for levels of cvd
  ################################################################################
  
  message('running for level 2')
  # Scaling cvd_all with R/E to GBD envelope
  raking(top_lev_loc = states$state_location_id,
         sublocs = state_re$location_id,
         causes = 491,
         years = para_year_id,
         level = 2,
         sum_vars = c("age_group_id", "year_id", "sex_id", "state_location_id"),
         save_filepath = "/FILEPATH",
         method = "direct",
         stat_type = "cause_fraction",
         gbd_codcorrect_ver = gbd_codcorrect_ver,
         final = final)
  
  ################################################################################
  message('now we will do level 3 nested')
  # Scaling level 3 cvd causes with R/E to results of level 2 scaling with R/E (use location_id instead of state_location_id, don't need cause_id since scaling to parent cause)
  raking(sublocs = state_re$location_id,
         causes = cvd_subcauses3$cause_id,
         years = para_year_id,
         level = 3,
         sum_vars = c("age_group_id", "year_id", "sex_id", "location_id"),
         save_filepath = "/FILEPATH",
         method="nested",
         upload_path = paste0("/FILEPATH/scaled_get_draws_lvl_2_", para_year_id, ".rds"),
         stat_type = "cause_fraction",
         final = final)
  
  # Scaling the level 4 causes with R/E into their separate lvl 3 envelopes with R/E (use location_id instead of state_location_id, don't need cause_id since scaling to parent cause)
  message('next up level 4 nested')
  for (i in unique(cvd_subcauses4$parent_id)){
    message(paste0('\nparent id ', i))
    child_causes <- cvd_subcauses4[parent_id==i, cause_id]
    raking(sublocs = state_re$location_id,
           causes = child_causes,
           years = para_year_id,
           level = 4,
           sum_vars = c("age_group_id", "year_id", "sex_id", "location_id"),
           save_filepath = "/FILEPATH",
           method = "nested",
           parent_id = i,
           upload_path = paste0("/FILEPATH/scaled_get_draws_lvl_3_", para_year_id, ".rds"),
           stat_type = "cause_fraction",
           final = final)
  }
  
  ################################################################################
  # Now we do this using direct scaling
  # Scaling level 3 cvd causes w R/E to GBD envelope of level 3 causes
  message('now level 3 direct')
  raking(top_lev_loc = states$state_location_id,
         sublocs = state_re$location_id,
         causes = cvd_subcauses3$cause_id,
         years = para_year_id,
         level = 3,
         sum_vars = c("age_group_id", "year_id", "sex_id", "state_location_id", "cause_id"),
         save_filepath = "/FILEPATH",
         method = "direct",
         stat_type = "cause_fraction",
         gbd_codcorrect_ver = gbd_codcorrect_ver,
         final = final)
  
  # Scaling level 4 cvd causes with R/E to GBD envelope of lvl 4 causes
  message('now level 4 direct')
  for (i in unique(cvd_subcauses4$parent_id)){
    message(paste0('\nparent id ', i))
    child_causes <- cvd_subcauses4[parent_id==i, cause_id]
    raking(top_lev_loc = states$state_location_id,
           sublocs = state_re$location_id,
           causes = child_causes,
           years = para_year_id,
           level = 4,
           sum_vars = c("age_group_id", "year_id", "sex_id", "state_location_id", "cause_id"),
           save_filepath = "/FILEPATH",
           method = "direct",
           stat_type = "cause_fraction",
           parent_id = i,
           gbd_codcorrect_ver = gbd_codcorrect_ver,
           final = final)
  }
}
