library(matrixStats)
mean_ui <- function(draws){
  df <- copy(draws)
  draw_cols <- names(df)[grepl("draw_", names(df))]
  df[, c("mean", "lower", "upper") := list(rowMeans(.SD), 
                                           rowQuantiles(as.matrix(.SD), probs=0.025), 
                                           rowQuantiles(as.matrix(.SD), probs=0.975)), .SDcols=draw_cols]
  df <- df[, paste0(draw_cols) := NULL]
  return(df)
}  

