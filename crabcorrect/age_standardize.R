age_standardize <- function(draw_data, cols){
  
  # pull in age weights
  source("/FILEPATH/get_age_metadata.R")
  wts <- get_age_metadata(age_group_set_id = 19, gbd_round_id = 7)
  
  # apply weights to data
  df <- copy(draw_data)
  df <- merge(df, wts[, c("age_group_weight_value", "age_group_id")], by="age_group_id")
  df[, paste0(cols) := lapply(.SD, function(x) (x * age_group_weight_value)), .SDcols = c(cols)]
  df <- df[ , lapply(.SD, sum), by = c("location_id", "state_location_id", "year_id", "sex_id", "cause_id"), .SDcols = c(cols)]
  df[, age_group_id := 27]
  
  return(df)

}