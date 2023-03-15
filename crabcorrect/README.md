## Purpose: To run crabcorrect data pipeline. 

### Contact USERNAME (EMAIL) or USERNAME (EMAIL) with any questions.

--------------------------------------------------------------------------------

**To run code:**

* Run "launch_array_script.R" script. This will do both direct and nested scaling on cvd_all and 2 levels of child causes if final=F or
  just direct scaling if final=T.
* This script runs the script "re_cod_scaling.R" inside a sbatch. Default is to run for all available GBD years, but years 
can be specified by the variable `available_yrs`.
* "re_cod_scaling.R" then runs the crabcorrect function, found in "crabcorrect_function.R", at the cvd_all, individual cause,
and subcause (for cvd causes that have subcauses) level. It does direct scaling as well as nested scaling if final=F. If final=T, only
direct scaling is done.

--------------------------------------------------------------------------------

**launch_array_script.R:**

* No user inputs required. Just run the full script.
* The only thing this does is create a sbatch for each year you want to scale, and launch a job for re_cod_scaling.R for each
year. Year is the only useful thing to change, if you desire a specific set of years. The default is to run for all GBD years.

--------------------------------------------------------------------------------

**re_cod_scaling.R:**

* Runs crabcorrect function for all cvd causes. Configured to only run cvd causes, but the crabcorrect function will work
on any set of causes.
* Scaling:
  * First, scales the US R/E cvd_all data to the GBD, which does not have race/ethnicity. The resulting datatables are saved out to
  /FILEPATH/. This is level 2, which is the same for nested and direct.
  * Nested Scaling:
    * cvd cause level: Results of cvd_all scaling are used to scale individual cvd causes. Resulting datatables are saved out in
    the /FILEPATH/ folder.
    * cvd subcause level: Results of individual cvd causes for which there are child causes are used to scale the subcauses of
    those causes (example: stroke is the cvd causes, subtypes of stroke are scaled to overall stroke envelope.)
  * Direct Scaling:
    * Scaling result of cvd_all is not used. Cvd cause and subcause R/E data is scaled directly to GBD instead of to next-less-
    granular cause level.
    * cvd cause level: Scales modeled cvd causes, which include R/E, directly to GBD values.
    * cvd subcause level: Scales modeled cvd subcauses, which include R/E, directly to GBD values.
    * Results saved out to /FILEPATH/ or, if being run as final=T,
      /FILEPATH/
  
--------------------------------------------------------------------------------

**crabcorrect_function.R:**

* Contains `raking` function.
* raking function:
  * raking(top_lev_loc,
            sublocs,
            causes,
            sum_vars,
            save_filepath,
            years,
            level,
            stat_type,
            method,
            gbd_codcorrect_ver,
            upload_path = NULL,
            parent_id=NULL,
            final,
            new_r01_env)
    * top_lev_loc: should be the highest location level you want to be scaled. Only used for direct scaling, nested scaling scales to above cause level. 
    For example, for the US R/E case, we are scaling state-R/E to states. In our case, each state-R/E combo is a location_id. Each state has 4 location_id's 
    that represent the state-R/E combination.
    * sublocs: The list of locations you want to be scaled into top_lev_loc/higher cause level. For our case, this is the state-R/E locations.
    * causes: A list of causes
    * sum_vars: The variables that you want to retain when summing to compare to the top_lev_loc geography. You should always
    include `age_group_id` and `sex_id`. This should be all variables that will be the SAME between the top level and
    sublevel locations.
    * save_filepath: The location you want scaled files and statistics files to be saved to.
    * years: the years for which this scaling is happening. This argument is there so you can examine one particular year
    without needing to launch the whole pipeline, but you could also just specify year in launch_array_script and have the same
    effect.
    * level: the GBD cause level of the cause(s) you are scaling. Ex. cvd_all is level 2, cvd_ihd is level 3. Final version usually 
    run with level as NA, since all causes are run together, regardless of level, using direct scaling
    * stat_type: "cause_fraction" or "deaths". Deaths is what exists in the raw data. Cause fraction will compute 
    deaths/envelope for output.
    * method: "direct" or "nested". The direct method scales your data to GBD no matter what level it is. The nested method
    scales your data to the provided data in the kwarg 'upload_path'.
    * gbd_codcorrect_ver: GBD CodCorrect version ID that should be used for scaling
    * upload_path: location of dataset you want to scale to.
    * parent_id: Only use if you are scaling level 4 to level 3 causes. Tells function which cause is the parent of which subcause.
    * final: whether function is being run for 'final' results (if so, produces files for all metrics and rows for age-standardized data and saves all causes together, regardless of level)
    * new_r01_env: Whether to pull in the latest R/E envelope, rather than using the envelope that the COD models were run with

    
  * Saves 2 kinds of files. Full scaled results at the draw level, and scaled results at the mean level.
    * Full scaled results are found at `save_filepath/scaled_get_draws...`
    * Scaled results statistics are found at `save_filepath/scaled_stats...`
    * Statistics files are generated to make plotting scaling results easier.

  * Crabcorrect method:
    * Pulls subloc codem results at the draw level
    * Pulls top_lev_loc results (for direct, this is codcorrect data. For nested, this is user-directed data from above cause level to be scaled to)
    at draw level
    * Sums up subloc data to top_lev_loc level of granularity.
    * Calculates scaling factor between summed subloc and top_lev_loc
    * Applies scaling factor to unsummed subloc data. For example, if top_lev_loc has no R/E, applies same weight to all 4 R/E
    groups for a given age/sex/year/state/cause. Result is scaled subloc data.
    
    ----------------------------------------------------------------------------
    
