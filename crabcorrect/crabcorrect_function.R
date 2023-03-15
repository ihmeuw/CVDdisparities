raking <-function (top_lev_loc = NULL, sublocs, causes, sum_vars, save_filepath, years, level = NA, stat_type, method, gbd_codcorrect_ver = NULL, upload_path = NULL, parent_id=NULL, final, new_r01_env=F){
  #- top_lev_loc should be the highest location level you want to be scaled. Only used for direct scaling, nested scaling scales to above cause level. Example: states
  #- sublocs is the list of locations that you want scaled. They will be scaled to the top_lev_loc/higher cause level. Example: race/ethnicity groups within states
  #- causes should be the list of causes. Can be 1 cause
  #- sum_vars is a list of the variables to sum over for the more granular geography. You should always sum over age_group_id, sex_id. Can also sum over cause_id, year_id, or location_id.
  #- save_filepath: The location you want scaled files and statistics files to be saved to.
  #- years: the years for which this scaling is happening. This argument is there so you can examine one particular year
  #         without needing to launch the whole pipeline, but you could also just specify year in launch_array_script and have the same effect.
  #- level: the GBD cause level of the cause(s) you are scaling. Ex. cvd_all is level 2, cvd_ihd is level 3. Final version usually run with level as NA, since all causes are run
  #         together, regardless of level, using direct scaling
  #- stat_type: "cause_fraction" or "deaths". Deaths is what exists in the raw data. Cause fraction will compute deaths/envelope for output.
  #- method: "direct" or "nested". The direct method scales your data to GBD no matter what level it is. The nested method scales your data to the provided data in the kwarg 'upload_path'.
  #- gbd_codcorrect_ver: GBD CodCorrect version ID that should be used for scaling
  #- upload_path: location of dataset you want to scale to.
  #- parent_id: Only use if you are scaling level 4 to level 3 causes. Tells function which cause is the parent of which subcause.
  #- final: whether function is being run for 'final' results (if so, produces files for all metrics and rows for age-standardized data and saves all causes together, regardless of level)
  #- new_r01_env: Whether to pull in the latest R/E envelope, rather than using the envelope that the COD models were run with
  
  library(data.table)
  library(R.utils)
  library(openxlsx)
  library(zip)
  library(stringr)
  library(ggplot2)
  library(gridExtra)
  
  suppressMessages(sourceDirectory("/FILEPATH", modifiedOnly = F))
  source('/FILEPATH/mean_ui.R')
  source('/FILEPATH/age_standardize.R')
  
  demographics <- get_demographics(gbd_team = "cod", gbd_round_id = 7)
  
  if (years == "all"){
    years <- demographics$year_id
  }
  
  # check which causes have available models
  check_models <- get_best_model_versions(entity = 'cause', ids = causes, gbd_round_id = 7, decomp_step = 'usa_re')
  causes <- causes[causes %in% unique(check_models$cause_id)]
  
  ################################################################################
  # pull in draws from R01 CODem models
  sublevel <- get_draws(gbd_id_type = 'cause_id',
                        gbd_id = causes,
                        source = 'codem',
                        status = 'best',
                        gbd_round_id = 7,
                        decomp_step = 'usa_re',
                        num_workers = 6,
                        measure_id = 1,
                        location_id = sublocs,
                        age_group_id = demographics$age_group_id,
                        year_id = years,
                        sex_id = c(1,2))
  if(!final){
    message(paste0('Done with sublevel get_draws for level ', level, '!'))
  } else {
    message(paste0('Done with sublevel get_draws!'))
  }
  
  # save model version info to be tracked
  run_info <- unique(sublevel[, .(cause_id, sex_id, model_version_id)])
  
  # format draws
  names(sublevel) <- gsub(x=names(sublevel), pattern = "draw", replacement = "sublev_draw")
  draws <- names(sublevel)[grepl("draw_", names(sublevel))]

  # drop columns that are not needed
  sublevel[, c('measure_id', 'pop', 'sex_name', 'metric_id', 'model_version_id') := NULL]
  
  # pull in new R01 envelope and apply if indicated
  if(new_r01_env){
    # pull in latest R01 envelope
    r01_envelope <- get_envelope(location_id = sublocs,
                                    year_id = years,
                                    age_group_id = demographics$age_group_id,
                                    sex_id = c(1,2),
                                    gbd_round_id = 7,
                                    decomp_step = 'usa_re')
    setnames(r01_envelope, 'mean', 'envelope')
    
    # save model version info to be tracked
    run_info[, r01_envelope_id := unique(r01_envelope$run_id)]
    
    # convert deaths to CF in COD results
    sublevel[, paste0(draws) := lapply(.SD, function(x) (x/envelope)), .SDcols = c(draws)]
    sublevel[, envelope := NULL]
    
    # merge in new R01 envelope
    sublevel <- merge(sublevel, r01_envelope[,c('age_group_id', 'location_id', 'sex_id', 'year_id', 'envelope')], 
                             by=c('age_group_id', 'location_id', 'sex_id', 'year_id'))
    
    # convert CF back to deaths in COD results using new R01 envelope
    sublevel[, paste0(draws) := lapply(.SD, function(x) (x * envelope)), .SDcols = c(draws)]
    
  } else {
    run_info[, r01_envelope_id := 'Default']
  }

  # if using cause fraction, calculate using (number of deaths/envelope)
  if (stat_type=="cause_fraction"){
    sublevel[, paste0(draws) := lapply(.SD, function(x) (x/envelope)), .SDcols = c(draws)]
  }
  sublevel <- sublevel[,-c('envelope')]

  # create and save the raw get_draws stats (mean, upper, lower)
  sub_stats <- mean_ui(sublevel)
  if (final){
    save_raw_stats <- paste0(save_filepath, "/FILEPATH/raw_stats_", years, ".rds")
  } else if (level < 4){
    save_raw_stats <- paste0(save_filepath, "/FILEPATH/raw_stats_lvl_", level, "_", years, ".rds")
  } else {
    save_raw_stats <- paste0(save_filepath, "/FILEPATH/raw_stats_lvl_", level,"_", parent_id, "_", years, ".rds")
  }
  saveRDS(sub_stats, file=save_raw_stats)
  message(paste0("Mean, upper, and lower computed for raw get_draws for sublevel. Saved to ", save_raw_stats))

  ################################################################################
  # reads in data from low-granularity source, either from previous round of raking or from codcorrect.
  if (method == "nested"){
    if (is.null(upload_path)){stop('If using the nested method, upload_path is required.')}
    
    # sums sublevel up to toplevel granularity 
    sublevel_sum <- sublevel[, lapply(.SD, sum), by = sum_vars, .SDcols = draws]
    
    # pulls in toplevel results
    top_lev_draws <- readRDS(file=upload_path)
    names(top_lev_draws) <- gsub(x=names(top_lev_draws), pattern = "draw", replacement = "top_lev_draw")
    top_draws <- names(top_lev_draws)[grepl("draw_", names(top_lev_draws))]
    message(paste0("Loaded data found at ", upload_path))
    if (!is.null(parent_id)){
      top_lev_draws <- top_lev_draws[cause_id==parent_id]
    }
    
  } else if (method == "direct"){
    # Add in state loc_id to sum to
    locs <- get_location_metadata(location_set_id = 105, gbd_round_id = 7, decomp_step = 'usa_re')
    locs <- locs[location_id >= 1000, .(location_id, location_name, location_type, parent_id)]
    setnames(locs, 'parent_id', 'state_location_id')
    sublevel <- merge(sublevel, locs[,c('location_id', 'state_location_id')], by=c('location_id'))
    
    # sums sublevel draws up to toplevel granularity 
    sublevel_sum <- sublevel[, lapply(.SD, sum), by = sum_vars, .SDcols = draws]
    
    # pull in GBD CodCorrect results as toplevel results
    top_lev_draws <- get_draws(gbd_id_type = 'cause_id',
                               gbd_id = causes,
                               source = 'codcorrect',
                               version_id = gbd_codcorrect_ver,
                               gbd_round_id = 7,
                               decomp_step = 'step3',
                               num_workers = 6,
                               measure_id = 1,
                               location_id = top_lev_loc,
                               age_group_id = demographics$age_group_id,
                               year_id = years,
                               sex_id = c(1,2))
    message('Done with top level get_draws!')
    
    # save model version info to be tracked
    run_info[, gbd_codcorrect_id := unique(top_lev_draws$version_id)]
    
    # format draws
    names(top_lev_draws) <- gsub(x=names(top_lev_draws), pattern = "draw", replacement = "top_lev_draw")
    top_draws <- names(top_lev_draws)[grepl("draw_", names(top_lev_draws))]
    setnames(top_lev_draws, 'location_id', 'state_location_id')
    
    # drop columns that are not needed
    top_lev_draws[, c('measure_id', 'metric_id', 'version_id') := NULL]
    
    # if using cause fraction, calculate
    if (stat_type=="cause_fraction"){
      # pull in latest GBD envelope
      top_lev_envelope <- get_envelope(location_id = top_lev_loc,
                                       year_id = years,
                                       age_group_id = demographics$age_group_id,
                                       sex_id = c(1,2),
                                       gbd_round_id = 7,
                                       decomp_step = 'step3')
      setnames(top_lev_envelope, c('location_id', 'mean'), c('state_location_id', 'envelope'))
      
      # save run info to be tracked
      run_info[, gbd_envelope_id_death_to_cf := unique(top_lev_envelope$run_id)]
      
      # merge in GBD envelope
      top_lev_draws <- merge(top_lev_draws, top_lev_envelope[,c('age_group_id', 'state_location_id', 'sex_id', 'year_id', 'envelope')], 
                             by=c('age_group_id', 'state_location_id', 'sex_id', 'year_id'))
      
      # convert to CF using (number of deaths/envelope) 
      top_lev_draws[, paste0(top_draws) := lapply(.SD, function(x) (x/envelope)), .SDcols = c(top_draws)]
      
      top_lev_draws <- top_lev_draws[,-c('envelope')]
    }
    
    # computes stats and saves them for the top level get_draws
    top_stats <- mean_ui(top_lev_draws)
    if (final){
      save_codcorrect_stats <- paste0(save_filepath, "/FILEPATH/codcorrect_stats_", years, ".rds")
    } else if (level < 4){
      save_codcorrect_stats <- paste0(save_filepath, "/FILEPATH/codcorrect_stats_lvl_", level, "_", years, ".rds")
    } else {
      save_codcorrect_stats <- paste0(save_filepath, "/FILEPATH/codcorrect_stats_lvl_", level, "_", parent_id, "_", years, ".rds")
    }
    saveRDS(top_stats, file=save_codcorrect_stats)
    message(paste0("Mean, upper, and lower computed for codcorrect get_draws for sublevel. Saved to ", save_codcorrect_stats))
    
  } else {
    stop("Must choose a method, either 'nested' or 'direct'.")
  }
  
  ################################################################################

  # merge the top level and sublevel data together
  sublevel_merge <- merge(top_lev_draws, sublevel_sum, by=sum_vars)
  
  if (nrow(sublevel_merge)==0){
    stop("Merging the top level and sublevel dataframes has 0 rows. Check summing
            variables to make sure all sum vars exist in the top level df")
  }
  
  # calculate the weights
  sublevel_merge[, paste0("weight_draw_", 0:999) := lapply(X = 0:999, FUN = function(x) get(paste0(ifelse(method=='direct',"top_lev_draw_", "scaled_top_lev_draw_"), x)) / get(paste0("sublev_draw_", x)))]
  sublevel_merge[, paste0(c(draws, top_draws)) := NULL]
  sublevel_scaled <- merge(sublevel, sublevel_merge, by = sum_vars)

  # calculates the scaled value
  sublevel_scaled[, paste0("scaled_draw_", 0:999) := lapply(X = 0:999, FUN = function(x) get(paste0("sublev_draw_", x)) * get(paste0("weight_draw_", x)))]
  sublevel_scaled[, paste0(c(draws, paste0("weight_draw_", 0:999))) := NULL]
  
  final_draws <- names(sublevel_scaled)[grepl("draw_", names(sublevel_scaled))]
  
  # creates and saves the mean, upper and lower of the scaled results and saves draw-level results
  if (final){
    scaled_stats <- mean_ui(sublevel_scaled)
    save_scaled_stats <- paste0(save_filepath, "/FILEPATH/scaled_stats_", years, "_", stat_type, ".rds")
    saveRDS(scaled_stats, file=save_scaled_stats)
    message(paste0("Mean, upper, and lower computed for scaled get_draws for sublevel, ", stat_type, ". Saved to ", save_scaled_stats))
    
    save_file <- paste0(save_filepath, "/FILEPATH/scaled_get_draws_", years, "_", stat_type, ".rds")
    saveRDS(sublevel_scaled, file = save_file)
    message(paste0("Result file saved to ", save_file, " for ", stat_type))
    
    # convert to death counts if necessary
    if(stat_type == 'cause_fraction'){
      
      # pull in latest R01 envelope
      sublev_envelope <- get_envelope(location_id = sublocs,
                                       year_id = years,
                                       age_group_id = demographics$age_group_id,
                                       sex_id = c(1,2),
                                       gbd_round_id = 7,
                                       decomp_step = 'usa_re')
      setnames(sublev_envelope, 'mean', 'envelope')
      
      # save model version info to be tracked
      run_info[, r01_envelope_id_cf_to_death := unique(sublev_envelope$run_id)]
      
      # merge in new R01 envelope
      sublevel_scaled <- merge(sublevel_scaled, sublev_envelope[,c('age_group_id', 'location_id', 'sex_id', 'year_id', 'envelope')], 
                             by=c('age_group_id', 'location_id', 'sex_id', 'year_id'))
      # convert CF back to deaths using (CF * envelope)
      sublevel_scaled[, paste0(final_draws) := lapply(.SD, function(x) (x * envelope)), .SDcols = c(final_draws)]
      sublevel_scaled <- sublevel_scaled[,-c('envelope')]
      
      scaled_stats <- mean_ui(sublevel_scaled)
      save_scaled_stats <- paste0(save_filepath, "/FILEPATH/scaled_stats_", years, "_deaths.rds")
      saveRDS(scaled_stats, file=save_scaled_stats)
      message(paste0("Mean, upper, and lower computed for scaled get_draws for sublevel, deaths. Saved to ", save_scaled_stats))
      
      save_file <- paste0(save_filepath, "/FILEPATH/scaled_get_draws_", years, "_deaths.rds")
      saveRDS(sublevel_scaled, file = save_file)
      message(paste0("Result file saved to ", save_file, " for deaths"))
      
    }
    
    # convert to rate and add age-standardized data using latest R01 population
    sublev_pop <- fread("/FILEPATH/r01_pop.csv")
    
    # save model version info to be tracked
    run_info[, r01_population_id := unique(sublev_pop$run_id)]
    
    # convert deaths to rate
    sublevel_scaled <- merge(sublevel_scaled, sublev_pop[,c('age_group_id', 'location_id', 'sex_id', 'year_id', 'population')], 
                             by=c('age_group_id', 'location_id', 'sex_id', 'year_id'))
    sublevel_scaled[, paste0(final_draws) := lapply(.SD, function(x) (x/population)), .SDcols = c(final_draws)]
    sublevel_scaled <- sublevel_scaled[,-c('population')]
    
    # create age-standardized data
    sublevel_scaled_age_standardized <- age_standardize(draw_data = sublevel_scaled, cols = final_draws)
    sublevel_scaled <- rbind(sublevel_scaled, sublevel_scaled_age_standardized)
    
    scaled_stats <- mean_ui(sublevel_scaled)
    save_scaled_stats <- paste0(save_filepath, "/FILEPATH/scaled_stats_", years, "_rates.rds")
    saveRDS(scaled_stats, file=save_scaled_stats)
    message(paste0("Mean, upper, and lower computed for scaled get_draws for sublevel, rates. Saved to ", save_scaled_stats))
    
    save_file <- paste0(save_filepath, "/FILEPATH/scaled_get_draws_", years, "_rates.rds")
    saveRDS(sublevel_scaled, file = save_file)
    message(paste0("Result file saved to ", save_file, " for rates"))
    
    # save 'tracker'
    run_info[, date := paste0(Sys.time())]
    write.csv(run_info, file = paste0(save_filepath, "/FILEPATH/run_info_", years, ".csv"), row.names = F)
    
  } else if (level < 4){
    scaled_stats <- mean_ui(sublevel_scaled)
    save_scaled_stats <- paste0(save_filepath, "/FILEPATH/scaled_stats_lvl_", level, "_", years, ".rds")
    saveRDS(scaled_stats, file=save_scaled_stats)
    message(paste0("Mean, upper, and lower computed for scaled get_draws for sublevel. Saved to ", save_scaled_stats))
  
    save_file <- paste0(save_filepath, "/FILEPATH/scaled_get_draws_lvl_", level, "_", years, ".rds")
    saveRDS(sublevel_scaled, file = save_file)
    message(paste0("Result file saved to ", save_file, "! Successful scaling!"))
    
  } else {
    #for level 4, we also include the parent_cause.
    scaled_stats <- mean_ui(sublevel_scaled)
    save_scaled_stats <- paste0(save_filepath, "/FILEPATH/scaled_stats_lvl_", level, "_", parent_id, "_", years, ".rds")
    saveRDS(scaled_stats, file=save_scaled_stats)
    message(paste0("Mean, upper, and lower computed for scaled get_draws for sublevel. Saved to ", save_scaled_stats))
    
    save_file <- paste0(save_filepath, "/FILEPATH/scaled_get_draws_lvl_", level, "_", parent_id, "_", years, ".rds")
    saveRDS(sublevel_scaled, file = save_file)
    message(paste0("Result file saved to ", save_file, "! Successful scaling!"))

  }

}
