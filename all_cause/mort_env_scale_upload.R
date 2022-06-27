####################################################################################################
## Scale draws from dimsm version of all-cause mortality envelope to w/out shock
## GBD draws
####################################################################################################
rm(list=ls())

# Libraries
suppressMessages(library(data.table))
suppressMessages(library(R.utils))
suppressMessages(library(imputeTS, lib="/FILEPATH/"))
library(matrixStats) 

# Functions
`%ni%` <- Negate(`%in%`)
date <- gsub("-", "_", Sys.Date())

# Filepaths
data.dir <- "/FILEPATH/"
gbd.draws <- "/FILEPATH/"

# Run information
run <- 434

# Load central libraries
suppressMessages(sourceDirectory("/FILEPATH/", modifiedOnly=FALSE))

# Get location and age information
locs <- get_location_metadata(location_set_id=105, release_id=8)
ages <- get_age_metadata(age_group_set_id=19, gbd_round_id=7)

# Vector to parallelize over
states <- unlist(locs[location_id>500 & location_id<600,.(location_id)], use.names=F)
array <- locs[location_id>600, .(location_id, parent_id)]

# For each state, read in GBD information, merge and scale
print(Sys.time())
dt.state <- list()
for (s in states)
	{
		print(s)
		
		sub <- unlist(array[parent_id==s,.(location_id)], use.names=F)
		
		files <- data.table(list.files(path=data.dir, pattern=".rds", full.names = TRUE))
		files[,end:=tstrsplit(V1, "draws_", keep=2)]
		files[,end:=gsub(".rds", "", end)]
		files[,location_id:=tstrsplit(end, "_", keep=1)]
		files[,location_id:=as.numeric(location_id)]
		files[,age_group_id:=tstrsplit(end, "_", keep=2)]
		files[,age_group_id:=as.numeric(age_group_id)]
		files <- files[location_id %in% sub]
		files <- unlist(files[,.(V1)], use.names=F)

		file.list <- list()
		for (f in files)
			{
				dt.file <- readRDS(f)
				file.list[[f]] <- dt.file
			}
		dt.complete <- rbindlist(file.list)
		dt.complete[,parent_id:=s]
		
		gbd.noshock <- readRDS(paste0(gbd.draws, "gbd_noshock_nohiv_",run,"_", s, ".rds"))
		dt.noshock <- merge(dt.complete, gbd.noshock, by=c("age_group_id", "sex_id", "year_id", "parent_id"))
		dt.noshock[, paste0("scaled_", 0:999) := lapply(FUN = function(x) get(paste0("draw_", x)) * get(paste0("env_", x)) / sum(get(paste0("draw_", x))), X = 0:999), 
			by=c("parent_id", "age_group_id", "sex_id", "year_id")]
		
		# aggregates
		dt.noshock.both <- dt.noshock[, lapply(.SD, sum), by=.(location_id, year_id, age_group_id), .SDcols=patterns("scaled_")]
		dt.noshock.both[,sex_id:=3]
		dt.noshock.allages <- dt.noshock[, lapply(.SD, sum), by=.(location_id, year_id, sex_id), .SDcols=patterns("scaled_")]
		dt.noshock.allages[,age_group_id:=22]
		dt.noshock.both.all <- dt.noshock[, lapply(.SD, sum), by=.(location_id, year_id), .SDcols=patterns("scaled_")]
		dt.noshock.both.all[,`:=` (sex_id=3, age_group_id=22)]
		
		# Bind together
		dt.noshock <- rbind(dt.noshock, dt.noshock.both, dt.noshock.allages, dt.noshock.both.all, fill=T)

		# collapse draws to mean/lower/upper
		dt.noshock[, c("mean", "lower", "upper") := list(rowMeans(.SD, na.rm=T), rowQuantiles(as.matrix(.SD), probs=0.025, na.rm=T), 
			rowQuantiles(as.matrix(.SD), probs=0.975, na.rm=T)), .SDcols=patterns("scaled_")]
		dt.noshock <- dt.noshock[,.(age_group_id, location_id, year_id, sex_id, mean, upper, lower)]
		# dt.noshock.both[, c("mean", "lower", "upper") := list(rowMeans(.SD, na.rm=T), rowQuantiles(as.matrix(.SD, na.rm=T), probs=0.025, na.rm), 
		# 	rowQuantiles(as.matrix(.SD), probs=0.975)), .SDcols=patterns("scaled_")]
		# dt.noshock.both <- dt.noshock.both[,.(age_group_id, location_id, year_id, sex_id, mean, upper, lower)]
		# dt.noshock.allages[, c("mean", "lower", "upper") := list(rowMeans(.SD), rowQuantiles(as.matrix(.SD), probs=0.025), 
		# 	rowQuantiles(as.matrix(.SD), probs=0.975)), .SDcols=patterns("scaled_")]
		# dt.noshock.allages <- dt.noshock.allages[,.(age_group_id, location_id, year_id, sex_id, mean, upper, lower)]
		# dt.noshock.both.all[, c("mean", "lower", "upper") := list(rowMeans(.SD), rowQuantiles(as.matrix(.SD), probs=0.025), 
		# 	rowQuantiles(as.matrix(.SD), probs=0.975)), .SDcols=patterns("scaled_")]
		# dt.noshock.both.all <- dt.noshock.both.all[,.(age_group_id, location_id, year_id, sex_id, mean, upper, lower)]
		
		# # rbind together
		# dt.noshock <- rbind(dt.noshock, dt.noshock.both, dt.noshock.allages, dt.noshock.both.all)

		# save to list
		dt.state[[s]] <- dt.noshock
	}
final <- rbindlist(dt.state)
final[,estimate_stage_id:=7]

# Fix problems in most recent years
problems <- unique(final[mean<=0 | lower<=0, .(location_id, age_group_id, sex_id)])

fix.list <- list()
for (i in 1:nrow(array))
  {
    dt.fill <- final[location_id==problems[["location_id"]][i] & sex_id==problems[["sex_id"]][i] & age_group_id==problems[["age_group_id"]][i]]
    dt.fill[,mean:=ifelse(mean<=0, NA, mean)]
    dt.fill[,lower:=ifelse(lower<=0, NA, lower)]
    dt.fill <- na_interpolation(dt.fill, option="stine")
    fix.list[[i]] <- dt.fill
  }
complete <- rbindlist(fix.list)
setnames(complete, c("mean", "lower", "upper"), c("mean.fixed", "lower.fixed", "upper.fixed"))

final <- merge(final, complete, by=c("age_group_id", "location_id", "year_id", "sex_id", "estimate_stage_id"), all.x=T)
final[,mean:=ifelse(is.na(mean.fixed)==F, mean.fixed, mean)]
final[,lower:=ifelse(is.na(lower.fixed)==F, lower.fixed, lower)]
final[,upper:=ifelse(is.na(upper.fixed)==F, upper.fixed, upper)]
final[,c("mean.fixed", "lower.fixed", "upper.fixed"):=NULL]

# to.graph <- merge(complete, locs[,.(location_id, location_name)], by="location_id")
# to.graph[,state:=tstrsplit(location_name, "; ", keep=1)]
# to.graph[,group:=tstrsplit(location_name, "; ", keep=2)]
# state_names <- unlist(unique(to.graph$state), use.names=F)[1:2]

# pdf(file=paste0("/FILEPATH/mort_check_", date, ".pdf"), width=14, height=8.5)
# for (s in state_names)
# 	{
#   		df.graph <- to.graph[state==s]
#   		state_label <- unique(df.graph$state)
# 		df.graph[,sex_name :=ifelse(sex_id==1, "Male", "Female")]
#   		df.graph[,age_group := factor(age_group_id, levels=c(2,3,388,389,238,34,6:20,30,31,32,235))]
  		
#   		p <- ggplot(data=df.graph, aes(x=year_id, y=mean, color=factor(group), linetype=factor(sex_name))) +
#     			geom_line() +
#     			theme_bw() +
#     			facet_wrap(~factor(age_group), scales="free_y") +
#     			theme(legend.title=element_blank()) +
#     			labs(x="Year", y="Deaths") +
# 			    ggtitle(state_label)
  		
#   		q <- ggplot(data=df.graph[group=="Hispanic, Any race"], aes(x=year_id, y=mean, linetype=factor(sex_name))) +
#     			geom_line() +
#     			theme_bw() +
#     			facet_wrap(~factor(age_group), scales="free_y") +
#     			theme(legend.title=element_blank()) +
#     			labs(x="Year", y="Deaths") +
#     			ggtitle(paste0(state_label, ", Hispanic, Any race"))
  		
#   		r <- ggplot(data=df.graph[group=="Non-Hispanic, Black"], aes(x=year_id, y=mean, linetype=factor(sex_name))) +
#     			geom_line() +
#     			theme_bw() +
#     			facet_wrap(~factor(age_group), scales="free_y") +
#     			theme(legend.title=element_blank()) +
#     			labs(x="Year", y="Population") +
#     			ggtitle(paste0(state_label, ", Non-Hispanic, Black"))
  		
#   		s <- ggplot(data=df.graph[group=="Non-Hispanic, Other races"], aes(x=year_id, y=mean, linetype=factor(sex_name))) +
#     			geom_line() +
#     			theme_bw() +
#     			facet_wrap(~factor(age_group), scales="free_y") +
#     			theme(legend.title=element_blank()) +
#     			labs(x="Year", y="Deaths") +
#     			ggtitle(paste0(state_label, ", Non-Hispanic, Other races"))
  
# 		t <- ggplot(data=df.graph[group=="Non-Hispanic, White"], aes(x=year_id, y=mean, linetype=factor(sex_name))) +
#     			geom_line() +
#     			theme_bw() +
#     			facet_wrap(~factor(age_group), scales="free_y") +
#     			theme(legend.title=element_blank()) +
#     			labs(x="Year", y="Deaths") +
#     			ggtitle(paste0(state_label, ", Non-Hispanic, White"))

# 		print(p)
#   		print(q)
#   		print(r)
#   		print(s)
#   		print(t)
# 	}
# dev.off()



# Pull mortality envelope for additional locations for from GBD
gbd.add <- fread(paste0("/FILEPATH/", run, "/FILEPATH/summary_env_no_shock.csv"))
gbd.add <- gbd.add[year_id>=1980]
gbd.add <- gbd.add[age_group_id %in% c(ages$age_group_id, 22)]
gbd.add[,estimate_stage_id:=7]

# Final dataset for upload
complete <- rbind(final, gbd.add)
print(Sys.time())

write.csv(complete, paste0("/FILEPATH/env_", run, "_", date, ".csv"), row.names=F)
