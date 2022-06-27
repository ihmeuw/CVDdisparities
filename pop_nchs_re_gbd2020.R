####################################################################################################
## Generate population estimates for US R/E project
####################################################################################################
rm(list=ls())

suppressMessages(library(R.utils))
suppressMessages(library(ggplot2))
suppressMessages(library(ipumsr))
suppressMessages(library(dplyr))
suppressMessages(library(data.table))
suppressMessages(library(zoo))
suppressMessages(library(Hmisc))
suppressMessages(library(openxlsx))
suppressMessages(library(haven))
#suppressMessages(library(forecast))
tryCatch({
  suppressMessages(library(forecast))
}, error = function(e) {
  suppressMessages(library(forecast, lib.loc="/PATH/"))
})

tryCatch({
  suppressMessages(library(imputeTS, lib.loc="/PATH/")) # R version < 4.0.0
}, error = function(e) {
  suppressMessages(library(imputeTS, lib.loc="/PATH/")) # R version >= 4.0.0
})
tryCatch({
  suppressMessages(library(mlegp, lib.loc="/PATH/")) # R version < 4.0.0
}, error = function(e) {
  suppressMessages(library(mlegp, lib.loc="/PATH/")) # R version >= 4.0.0
})
tryCatch({
  suppressMessages(library(lbfgs, lib.loc="/PATH/")) # R version < 4.0.0
}, error = function(e) {
  suppressMessages(library(lbfgs, lib.loc="/PATH/")) # R version >= 4.0.0
})
tryCatch({
  suppressMessages(library(GauPro, lib.loc="/PATH/")) # R version < 4.0.0
}, error = function(e) {
  suppressMessages(library(GauPro, lib.loc="/PATH/")) # R version >= 4.0.0
})

root <- ifelse(Sys.info()[1]=="Windows", "/PATH/", "/PATH/")
scratch <- paste0("/PATH/")
plot_dir <- paste0(root, "/PATH/")

date <- gsub("-", "_", Sys.Date())
sum.na <- function(x) {sum2 = sum(x, na.rm=T)}
prop <- function(x) {x/sum(x)}
`%ni%` <- Negate(`%in%`)

# Set decomp_step
step <- "step3"

# Load shared functions for locations and populations
suppressMessages(sourceDirectory("/PATH/", modifiedOnly=FALSE))

# Get location information
map <- get_location_metadata(location_set_id=105, gbd_round_id=7, decomp_step="usa_re")
us <- map[location_type=="admin1"]
us.re <- map[location_type=="ethnicity" & level==5]
re <- unlist(subset(map, location_type_id==17, select="location_id"))
other <- map[location_type!="ethnicity" & location_type!="admin1"]
other.locs <- unlist(other$location_id)

# Get GBD age group information
age_info <- get_age_metadata(gbd_round_id=7, age_group_set_id=19)
age_info <- age_info[, c("age_group_id", "age_group_name", "age_group_years_start", "age_group_years_end")]
babies <- data.table(age_group_id=28, age_group_name="0 to 1", age_group_years_start=0, age_group_years_end=1)
age_info <- rbind(age_info, babies)
# age_info[, age_group_years_start := ifelse(age_group_id %in% c(2,3,388,389,28), NA, age_group_years_start)]
ages_gbd <- unlist(age_info[age_group_id!=28, age_group_id], use.names=F) 
ages_agg <- c(ages_gbd, 22)
under1 <- age_info[age_group_years_start<1 & age_group_id!=28, .(age_group_id)]
five <- age_info[age_group_years_start>=1 & age_group_years_end<=5, .(age_group_id)]
eightyfive <- age_info[age_group_years_start>=85, .(age_group_id)]

# Create empty data.table with all possible combinations; GBD 2020 age groups
dt.all <- data.table(location_id=rep(re, each=2150), year_id=rep(1980:2022, each=50), age_group_id=rep(ages_gbd, times=17544),
                     sex_id=rep(rep(1:2, each=25), times=8772))

# Creates empty data.table with 0 to 1, 1 to 4, 5 to 9, 85+ for dealing with problematic data points
# complete.all <- data.table(location_id=rep(re, each=2150), year_id=rep(1980:2022, each=50), age_group_id=rep(ages_gbd, times=17544),
# 						sex_id=rep(rep(1:2, each=25), times=8772))
# complete.all[,age_group_id:=ifelse(age_group_id==2, 28, ifelse(age_group_id==3, 5, ifelse(age_group_id==31, 160, age_group_id)))]

# Creates empty data.table with 0 to 1, 1 to 2, 2 to 4, 85+ for merging after fixing data points before splitting babies and 
# oldest folks into GBD age groups
# pre.split.all <- data.table(location_id=rep(re, each=2150), year_id=rep(1980:2022, each=50), age_group_id=rep(ages_gbd, times=17544),
# 						sex_id=rep(rep(1:2, each=25), times=8772))
# pre.split.all[,age_group_id:=ifelse(age_group_id==2, 28, ifelse(age_group_id==31, 160, age_group_id))]
# pre.split.all <- pre.split.all[age_group_id %ni% c(3,32,235,388,389)]

# Get GBD population information; pulling gbd_round_id=7, decomp_step="step3" 
gbd.pop <- get_population(year_id=c(1980:2022), sex_id=c(1,2,3), age_group_id=ages_agg,
                          location_id=c(unlist(us$location_id, use.names=F), 102), decomp_step=step, gbd_round_id=7)
gbd.pop[,run_id:=NULL]
gbd.pop <- merge(gbd.pop, us[,c("location_id", "location_name")], by=c("location_id"), all.x=T)
gbd.pop[,location_name:=ifelse(location_id==102, "United States", location_name)]
gbd.pop.scale <- copy(gbd.pop)
setnames(gbd.pop.scale, "population", "gbd")

# Create proportions to split under 1 and 85+ age groups from population
under1 <- gbd.pop[age_group_id %in% c(2,3,388,389)]
under1[, state_tot := sum(population), by='location_name,year_id,sex_id'] 
under1[, state_prop := sum(population)/state_tot, by='location_name,year_id,sex_id,age_group_id']

# over85.gbd <- gbd.pop[age_group_id %in% c(31,32,235)]
# over85.gbd[, state_tot := sum(population), by='location_name,year_id,sex_id'] # for counties with no population, use the state proportions
# over85.gbd[, state_prop := sum(population)/state_tot, by='location_name,year_id,sex_id,age_group_id']

# Try using National race-specific estimates, rather than state information
files <- c("icen_2000_09_85to100.sas7bdat", "pcen_v2019_85to100.sas7bdat")
dt.nat <- list()
for (file in files){
  nat <- data.table(read_sas(paste0(scratch, "PATH/", file)))
  nat <- nat[,.(year, month, age, hisp, RACESEX, POP)]
  setnames(nat, "year", "year_id")
  nat[, group:=ifelse(hisp==2, "Hispanic, Any race",
                      ifelse(hisp==1 & RACESEX %in% c(1,2), "Non-Hispanic, White",
                             ifelse(hisp==1 & RACESEX %in% c(3,4), "Non-Hispanic, Black",
                                    ifelse(hisp==1 & RACESEX %in% c(5,6,7,8), "Non-Hispanic, Other races", NA))))]
  nat[, sex_id:=ifelse(RACESEX %in% c(1,3,5,7), 1, ifelse(RACESEX %in% c(2,4,6,8), 2, NA))]
  nat[, age_group_id:=ifelse(age>=85 & age<90, 31, ifelse(age>=90 & age<95, 32,ifelse(age>=95, 235, NA)))]
  
  dt.nat[[file]] <- nat
}
nat.pop <- rbindlist(dt.nat)

over85 <- nat.pop[, lapply(.SD, sum), by=.(group, sex_id, age_group_id, year_id), .SDcols=c("POP")]
over85[, state_tot := sum(POP), by='group,sex_id,year_id']
over85[, state_prop := sum(POP)/state_tot, by='group,sex_id,age_group_id,year_id']

early <- list()
late <- list()
years.early <- c(1980:1999)
years.late <- c(2020:2022)

for (ey in years.early){
  expand.early <- over85[year_id==2000]
  expand.early[,year_id:=NULL]
  expand.early[,year_id:=ey]
  early[[ey]] <- expand.early
}

for (ly in years.late){
  expand.late <- over85[year_id==2019]
  expand.late[,year_id:=NULL]
  expand.late[,year_id:=ly]
  late[[ly]] <- expand.late
}

over85.early <- rbindlist(early)
over85.late <- rbindlist(late)

over85 <- rbind(over85.early, over85, over85.late)

# Load merged counties information file
load(paste0(scratch, "PATH/merged_counties.rdata")) #loc

# Load raw population data for NCHS bridged-race files
# 1990-1999
one <- fread(paste0(scratch, "PATH/icen_stA1.txt"), fill=T, colClasses="character")
one[, demo:=ifelse(nchar(V1)==5 & nchar(V2)==3, paste0(V1, "0", V2), V1)]
one[, state:=substring(demo,1,2)]
one[, age:=as.numeric(substring(demo,6,7))]
one[, race_sex:=substring(demo,8,8)]
one[, hispanic:=ifelse(substring(demo, 9,9)==1, "Non-Hispanic", ifelse(substring(demo, 9,9)==2, "Hispanic", NA))]
one[, sex_id:=ifelse(race_sex %in% c(1,3,5,7), 1, ifelse(race_sex %in% c(2,4,6,8), 2, NA))]
one[, race:=ifelse(hispanic=="Hispanic", 4,
                   ifelse(race_sex %in% c(1,2) & hispanic=="Non-Hispanic", 1,
                          ifelse(race_sex %in% c(3,4) & hispanic=="Non-Hispanic", 2,
                                 ifelse(race_sex %in% c(5,6,7,8) & hispanic=="Non-Hispanic", 3, NA))))]
one[, pop.1990:=ifelse(nchar(V1)==5, V3, V2)]
one[, pop.1991:=ifelse(nchar(V1)==5, V4, V3)]
one[, pop.1992:=ifelse(nchar(V1)==5, V5, V4)]
one[, pop.1993:=ifelse(nchar(V1)==5, V6, V5)]
one[, pop.1994:=ifelse(nchar(V1)==5, V7, V6)]
one[, pop.1995:=ifelse(nchar(V1)==5, V8, V7)]
one[, pop.1996:=ifelse(nchar(V1)==5, V9, V8)]
one[, pop.1997:=ifelse(nchar(V1)==5, V10, V9)]
one[, pop.1998:=ifelse(nchar(V1)==5, V11, V10)]
one[, pop.1999:=ifelse(nchar(V1)==5, V12, V11)]
one[, grep("V", names(one)):=NULL]
one[, c("demo", "race_sex"):=NULL]
one.long = melt(one, id.vars=c("state", "age", "race", "sex_id"), measure.vars=c(paste0("pop.", c(1990:1999))))
one.long[, variable:=gsub("pop.", "", variable)]
setnames(one.long, "variable", "year_id")
setnames(one.long, "value", "pop")
one.long[, pop:=as.numeric(pop)]
one.long[,age:=ifelse(age==0, 0, ifelse(age==1, 1, ifelse(age %in% c(2,3,4), 2, 5*floor(age/5))))]
one.collapse <- one.long[, lapply(.SD, sum), by=.(state, race, year_id, sex_id, age), .SDcols=c("pop")]

# 2000-2004
two <- fread(paste0(scratch, "PATH/icen_2000_09_y0004.txt"), fill=T, colClasses="character")
two[, demo:=ifelse(nchar(V1)==13 & nchar(V2)==3, paste0(V1, "0", V2), V1)]
two[, state:=substring(demo, 9, 10)]
two[, age:=as.numeric(substring(demo, 14,15))]
two[, race_sex:=substring(demo,16,16)]
two[, hispanic:=ifelse(substring(demo,17,17)==1, "Non-Hispanic", ifelse(substring(demo,17,17)==2, "Hispanic", NA))]
two[, sex_id:=ifelse(race_sex %in% c(1,3,5,7), 1, ifelse(race_sex %in% c(2,4,6,8), 2, NA))]
two[, race:=ifelse(hispanic=="Hispanic", 4,
                   ifelse(race_sex %in% c(1,2) & hispanic=="Non-Hispanic", 1,
                          ifelse(race_sex %in% c(3,4) & hispanic=="Non-Hispanic", 2,
                                 ifelse(race_sex %in% c(5,6,7,8) & hispanic=="Non-Hispanic", 3, NA))))]
two[, pop.2000:=ifelse(nchar(V1)==13, V3, V2)]
two[, pop.2001:=ifelse(nchar(V1)==13, V4, V3)]
two[, pop.2002:=ifelse(nchar(V1)==13, V5, V4)]
two[, pop.2003:=ifelse(nchar(V1)==13, V6, V5)]
two[, pop.2004:=ifelse(nchar(V1)==13, V7, V6)]
two[, grep("V", names(two)):=NULL]
two[, c("demo", "race_sex"):=NULL]
two.long = melt(two, id.vars=c("state", "age", "race", "sex_id"), measure.vars=c(paste0("pop.", c(2000:2004))))
two.long[, variable:=gsub("pop.", "", variable)]
setnames(two.long, "variable", "year_id")
setnames(two.long, "value", "pop")
two.long[, pop:=as.numeric(pop)]
two.long[,age:=ifelse(age==0, 0, ifelse(age==1, 1, ifelse(age %in% c(2,3,4), 2, 5*floor(age/5))))]
two.collapse <- two.long[, lapply(.SD, sum), by=.(state, race, year_id, sex_id, age), .SDcols=c("pop")]

# 2005-2009
three <- fread(paste0(scratch, "PATH/icen_2000_09_y0509.txt"), fill=T, colClasses="character")
three[, demo:=ifelse(nchar(V1)==13 & nchar(V2)==3, paste0(V1, "0", V2), V1)]
three[, state:=substring(demo, 9, 10)]
three[, age:=as.numeric(substring(demo, 14,15))]
three[, race_sex:=substring(demo,16,16)]
three[, hispanic:=ifelse(substring(demo,17,17)==1, "Non-Hispanic", ifelse(substring(demo,17,17)==2, "Hispanic", NA))]
three[, sex_id:=ifelse(race_sex %in% c(1,3,5,7), 1, ifelse(race_sex %in% c(2,4,6,8), 2, NA))]
three[, race:=ifelse(hispanic=="Hispanic", 4,
                     ifelse(race_sex %in% c(1,2) & hispanic=="Non-Hispanic", 1,
                            ifelse(race_sex %in% c(3,4) & hispanic=="Non-Hispanic", 2,
                                   ifelse(race_sex %in% c(5,6,7,8) & hispanic=="Non-Hispanic", 3, NA))))]
three[, pop.2005:=ifelse(nchar(V1)==13, V3, V2)]
three[, pop.2006:=ifelse(nchar(V1)==13, V4, V3)]
three[, pop.2007:=ifelse(nchar(V1)==13, V5, V4)]
three[, pop.2008:=ifelse(nchar(V1)==13, V6, V5)]
three[, pop.2009:=ifelse(nchar(V1)==13, V7, V6)]
three[, grep("V", names(three)):=NULL]
three[, c("demo", "race_sex"):=NULL]
three.long = melt(three, id.vars=c("state", "age", "race", "sex_id"), measure.vars=c(paste0("pop.", c(2005:2009))))
three.long[, variable:=gsub("pop.", "", variable)]
setnames(three.long, "variable", "year_id")
setnames(three.long, "value", "pop")
three.long[, pop:=as.numeric(pop)]
three.long[,age:=ifelse(age==0, 0, ifelse(age==1, 1, ifelse(age %in% c(2,3,4), 2, 5*floor(age/5))))]
three.collapse <- three.long[, lapply(.SD, sum), by=.(state, race, year_id, sex_id, age), .SDcols=c("pop")]

# 2010-2019
four <- data.table(read_sas(paste0(scratch, "PATH/pcen_v2019_y1019.sas7bdat")))
setnames(four, "ST_FIPS", "state")
setnames(four, "POP2010_JUL", "POP2010")
four[, race:=ifelse(hisp==2, 4, #"Hispanic, Any race"
                    ifelse(hisp==1 & RACESEX %in% c(1,2), 1, #Non-Hispanic, White"
                           ifelse(hisp==1 & RACESEX %in% c(3,4), 2, #"Non-Hispanic, Black"
                                  ifelse(hisp==1 & RACESEX %in% c(5,6,7,8), 3, NA))))] #"Non-Hispanic, Other races"
four[, sex_id:=ifelse(RACESEX %in% c(1,3,5,7), 1, ifelse(RACESEX %in% c(2,4,6,8), 2, NA))]
four[,c("CO_FIPS", "POP2010_APR", "VINTAGE", "race4","hisp", "RACESEX"):=NULL]
names(four) <- gsub("POP", "pop.", names(four))
four.long = melt(four, id.vars=c("state", "age", "race", "sex_id"), measure.vars=c(paste0("pop.", c(2010:2019))))
four.long[, variable:=gsub("pop.", "", variable)]
setnames(four.long, "variable", "year_id")
setnames(four.long, "value", "pop")
four.long[,pop:=as.numeric(pop)]
four.long[,age:=ifelse(age==0, 0, ifelse(age==1, 1, ifelse(age %in% c(2,3,4), 2, 5*floor(age/5))))]
four.collapse <- four.long[, lapply(.SD, sum), by=.(state, race, year_id, sex_id, age), .SDcols=c("pop")]

# Bind all of NCHS data together
pop <- rbind(one.collapse, two.collapse, three.collapse, four.collapse)
pop[, state:=as.numeric(state)]
pop[, year_id:=as.numeric(year_id)]
pop[, race:=as.numeric(race)]
# Map to state name
pop <- merge(pop, unique(loc[, .(state, state_name)]), by="state")

#fix formatting to merge with GBD population splits
setnames(pop, "state_name", "location_name")
pop <- merge(pop, us[,.(location_id, parent_id, location_name, location_type)], by="location_name")

# Predict out to 2022; still need to de-loop
years <- c(2020:2022)

states <- unique(unlist(pop$location_name, use.names=F))
sexes <- unique(unlist(pop$sex_id, use.names=F))
races <- unique(unlist(pop$race, use.names=F))
ages <- unique(unlist(pop$age, use.names=F))

pop.predict <- list()
age.predict <- list()
race.predict <- list()
sex.predict <- list()
state.predict <- list()

for (i in 1:length(states)){
  for (j in 1:length(sexes)){
    for(k in 1:length(races)){
      for(l in 1:length(ages)){
        y <- unlist(pop[location_name==states[i] & race==races[k] & age==ages[l] & sex_id==sexes[j], "pop"], use.names=F)
        auto.mod <- auto.arima(y, allowdrift=TRUE)
        preds <- forecast(auto.mod, length(years))
        tmp <- data.table(year_id=years, pop=c(unlist(preds$mean[1:length(years)], use.names=F)))
        tmp$state <- states[i]
        tmp$sex_id <- sexes[j]
        tmp$race <- races[k]
        tmp$age <- ages[l]
        age.predict[[l]] <- tmp
      }
      tmp.race <- do.call(rbind, age.predict)
      race.predict[[k]] <- tmp.race
    }
    tmp.sex <- do.call(rbind, race.predict)
    sex.predict[[j]] <- tmp.sex
  }
  tmp.state <- do.call(rbind, sex.predict)
  state.predict[[i]] <- tmp.state
}	
pop.predict <- data.table(do.call(rbind, state.predict))

#Format and fix names
setnames(pop.predict, "state", "location_name")
pop.predict <- merge(pop.predict, us[,.(location_id, parent_id, location_name, location_type)], by="location_name")

#Load census data 
#Read in population data for 1980 - 1989
# df.census <- read.xlsx(paste0(scratch, "PATH/intercensal_population_old.xlsx"), colNames=F)
# names(df.census) <- c("info", "age0", "age5", "age10", "age15", "age20", "age25", "age30", "age35", "age40", "age45", "age50",
# 			   			"age55", "age60", "age65", "age70", "age75", "age80", "age85")
# df.census$info <- formatC(df.census$info, width=5, format="d", flag="0")
# df.census$state <- as.integer(substr(df.census$info, 1, 2))
# df.census$year <- substr(df.census$info, 3, 3)
# df.census$year <- as.integer(paste0("198", df.census$year))
# df.census$detailed_race <- as.integer(substr(df.census$info, 4, 4))
# df.census$sex <- as.integer(substr(df.census$info, 5, 5))
# df.census$info <- NULL
# df.census <- as.data.table(df.census)
# df.melt <- melt(df.census, id.var=c("detailed_race", "sex", "year", "state"), variable.name="age", value.name="pop")
# dt.census <- data.table(df.melt)
# dt.census$age <- as.integer(gsub("age", "", dt.census$age))
# dt.census[, race := ifelse(detailed_race==1, 1, ifelse(detailed_race==2, 2, ifelse(detailed_race %in% c(3,4), 3, 
# 					ifelse(detailed_race %in% c(5,6,7,8), 4, NA))))]
# dt.race <- dt.census[, lapply(.SD, sum.na), by=.(state, race, year, sex, age), .SDcols = c("pop")]
# 
# #fix formatting to merge with GBD population splits
# dt.race <- merge(dt.race, unique(loc[, c("state", "state_name")]), by="state")
# setnames(dt.race, "sex", "sex_id")
# setnames(dt.race, "year", "year_id")
# setnames(dt.race, "state_name", "location_name")
# dt.race <- merge(dt.race, us[,.(location_id, parent_id, location_name, location_type)], by="location_name")

#Read in 1980 data
ddi <- read_ipums_ddi(paste0(scratch, "PATH/usa_00005.xml"))
ipums <- read_ipums_micro(ddi)

#Create working data.frame and generate labelled variables
df <- ipums
df <- subset(df, STATEFIP<=56 & YEAR==1980) #Subset to individual states only, 1980 only
df <- df %>% mutate(state_text = droplevels(as_factor(STATEFIP, levels = "labels")))
df <- df %>% mutate(gq = droplevels(as_factor(GQ, levels = "labels")))
df <- df %>% mutate(region = droplevels(as_factor(REGION, levels = "labels")))
df <- df %>% mutate(race_text = droplevels(as_factor(RACE, levels = "labels")))

dt <- data.table(df)
dt[, age := ifelse(AGE <1, 0, ifelse(AGE >=1 & AGE <2, 1, ifelse(AGE>=2 & AGE<5, 2, ifelse(AGE >=5 & AGE <10, 5, ifelse(AGE >=10 & AGE <15, 10,
                                                                                                                        ifelse(AGE >=15 & AGE <20, 15, ifelse(AGE >=20 & AGE <25, 20, ifelse(AGE >=25 & AGE <30, 25, ifelse(AGE >=30 & AGE <35, 30, 
                                                                                                                                                                                                                            ifelse(AGE >=35 & AGE <40, 35, ifelse(AGE >=40 & AGE <45, 40, ifelse(AGE >=45 & AGE <50, 45, ifelse(AGE >=50 & AGE <55, 50, 
                                                                                                                                                                                                                                                                                                                                ifelse(AGE >=55 & AGE <60, 55, ifelse(AGE >=60 & AGE <65, 60, ifelse(AGE >=65 & AGE <70, 65, ifelse(AGE >=70 & AGE <75, 70, 
                                                                                                                                                                                                                                                                                                                                                                                                                                    ifelse(AGE >=75 & AGE <80, 75, ifelse(AGE >=80 & AGE <85, 80, ifelse(AGE >=85, 85, NA))))))))))))))))))))]
dt[,race := ifelse(HISPAN==0 & RACE==1, 1, ifelse(HISPAN==0 & RACE==2, 2, ifelse(HISPAN==0 & RACE %in% c(3:9), 3, ifelse(HISPAN!=0, 4, NA))))]
dt[,pop := as.integer(PERWT)]
dt[,state := as.integer(STATEFIP)]
dt[,sex := as.integer(SEX)]
dt[,year := as.integer(YEAR)]
dt.1980 <- dt[gq %ni% c("Other group quarters", "Group quarters--Institutions"), 
              lapply(.SD, sum.na), by=.(state, race, year, sex, age), .SDcols = c("pop")]

dt.1980 <- merge(dt.1980, unique(loc[, c("state", "state_name")]), by="state")
setnames(dt.1980, "sex", "sex_id")
setnames(dt.1980, "year", "year_id")
setnames(dt.1980, "state_name", "location_name")
dt.1980 <- merge(dt.1980, us[,.(location_id, parent_id, location_name, location_type)], by="location_name")

# rbind and split young and old age groups
dt.combined <- rbind(dt.1980, pop, pop.predict, fill=T)
dt.combined[, group:=ifelse(race==1, "Non-Hispanic, White", ifelse(race==2, "Non-Hispanic, Black", ifelse(race==3, "Non-Hispanic, Other races",
                                                                                                          ifelse(race==4, "Hispanic, Any race", NA))))]
dt.combined[, c("parent_id", "state", "location_type"):=NULL]
setnames(dt.combined, "location_id", "parent_id")
setnames(dt.combined, "location_name", "parent_name")
dt.combined[,location_name:=paste0(parent_name, "; ", group)]
dt.combined <- merge(dt.combined, map[,.(location_name, location_id)], by="location_name")

# No longer using GBD pops to split oldest age groups
# Split 85+
# old <- merge(dt.combined[age==85, .(location_id, sex_id, year_id, age, race, parent_id, pop)], 
# 			 over85[sex_id!=3 & year_id %ni% c(1981:1989) & location_id==102, .(year_id, sex_id, state_prop, age_group_id)],
# 			 by.x=c("year_id", "sex_id"), by.y=c("year_id", "sex_id"), all=T, allow.cartesian=T)
# old[,population:=pop*state_prop]

old <- merge(dt.combined[age==85, .(location_id, sex_id, year_id, age, race, group, parent_id, pop)], 
             over85[sex_id!=3 & year_id %ni% c(1981:1989), .(year_id, sex_id, state_prop, age_group_id, group)],
             by=c("group", "sex_id", "year_id"), all=T, allow.cartesian=T)
old[,population:=pop*state_prop]

# Split babies 
babies <- merge(dt.combined[age==0, .(location_id, sex_id, year_id, age, race, parent_id, pop)], 
                under1[sex_id!=3 & year_id %ni% c(1981:1989) & location_id==102, .(year_id, sex_id, state_prop, age_group_id)],
                by.x=c("year_id", "sex_id"), by.y=c("year_id", "sex_id"), all=T, allow.cartesian=T)
babies[,population:=pop*state_prop]

dt.split <- merge(dt.combined[age %ni% c(0,85)], 
                  age_info[age_group_id %ni% c(2,3,388,389,31,32,235), .(age_group_id, age_group_name, age_group_years_start)], 
                  by.x="age", by.y="age_group_years_start", all.x=TRUE)
dt.split[,population:=pop]

dt.split <- rbind(babies, dt.split, old, fill=T)
dt.split[,c("location_name", "parent_name", "group", "age_group_name"):=NULL]
dt.split <- merge(dt.split, dt.all, by=c("location_id", "year_id", "age_group_id", "sex_id"), all=T)

# Merge with death data and find problems
claude <- fread("/PATH/all_deaths_09Sep2019.csv")
claude.all <- claude[, lapply(.SD, sum.na), by=.(location_id, year_id, sex_id, age_group_id), .SDcols = c("deaths")]

complete <- merge(dt.split, claude.all, by=c("age_group_id", "location_id", "sex_id", "year_id"), all.x=T)
complete[,rate:=deaths/population]
complete[,rate:=ifelse(is.finite(rate), rate, NA)] #turns all Inf to NA
complete[,max.pop:=max(population, na.rm=T), by=.(age_group_id, location_id, sex_id)]
pop_2000 <- complete[year_id==2000]
setnames(pop_2000, "population", "pop.2000")
complete <- merge(complete, pop_2000[, .(age_group_id, location_id, sex_id, pop.2000)], by=c("age_group_id", "sex_id", "location_id"))
complete[,diff:=(population-pop.2000)/population]

diff.1999 <- complete[year_id==1999]
setnames(diff.1999, "diff", "diff.1999")
setnames(diff.1999, "population", "pop.1999")
diff.2001 <- complete[year_id==2001]
setnames(diff.2001, "diff", "diff.2001")
setnames(diff.2001, "population", "pop.2001")
to.check <- merge(diff.1999[,.(age_group_id, sex_id, location_id, pop.1999, pop.2000, diff.1999)], 
                  diff.2001[,.(age_group_id, sex_id, location_id, pop.2001, diff.2001)], by=c("age_group_id", "sex_id", "location_id"))
# to.check <- to.check[abs(diff.1999)>0.025 & abs(diff.2001)<0.05]
to.fix <- unique(complete[(rate>0.75 & age_group_id %ni% c(2,3)) | 
                            (is.na(population)==T & is.na(deaths)==F & year_id %ni% c(1980:1989)) | 
                            population==0 | 
                            max.pop<80, .(location_id, age_group_id, sex_id)])

# to.fix <- unique(to.check[, .(location_id, age_group_id, sex_id)])
# to.fix <- unique(complete[location_id==53912, .(location_id, age_group_id, sex_id)]) 

# Identify locations that need to be fixed using GP approach
year.all <- c(1980:2022)
na.fix <- list()
for (i in 1:dim(to.fix)[1]){
  test <- complete[location_id==unlist(to.fix[i,1], use.names=F) & age_group_id==unlist(to.fix[i,2], use.names=F) &
                     sex_id==unlist(to.fix[i,3], use.names=F)]
  test[,population:=ifelse(year_id<2000, NA, population)]
  test[,population:=ifelse(population<1, NA, population)]
  test[, c("deaths", "rate"):=NULL]
  test2 <- test[is.na(population)==F]
  print(paste(i, unique(test$location_id), unique(test$age_group_id), unique(test$sex_id)))
  gp <- GauPro_kernel_model$new(test2$year_id, test2$population, parallel=F, trend=trend_0$new(), kernel=Exponential$new(0))
  # gp <- GauPro(test2$year_id, test2$population, parallel=F)
  gp.pred <- predict(gp, year.all)
  final <- cbind(test, gp=gp.pred)
  setnames(final, "gp.V1", "gp")
  na.fix[[i]] <- final
}

# foo <- merge(complete[location_id==53912 & sex_id==1 & age_group_id==235, .(year_id, population)],
# 			 final[, .(year_id, gp)], by="year_id")

fixed <- rbindlist(na.fix)
fixed[,keep:=ifelse(is.na(population)==T & is.na(gp)==F, 1, 0)]

dt.final <- merge(complete, fixed[keep==1, .(location_id, sex_id, age_group_id, year_id, gp)], 
                  by=c("location_id", "sex_id", "age_group_id", "year_id"), all.x=T)
dt.final[,population:=ifelse(is.na(gp)==F, gp, population)]
dt.final <- dt.final[,.(location_id, sex_id, age_group_id, year_id, age, race, parent_id, population)]

# Interpolate 1981-1989 and any missing years not addressed in GP step
combo <- unique(dt.final[,.(location_id, age_group_id, sex_id)])
na.interp <- list()
for (j in 1:dim(combo)[1]){
  tmp <- dt.final[location_id==unlist(combo[j,.(location_id)], use.names=F) & 
                    age_group_id==unlist(combo[j,.(age_group_id)], use.names=F) & 
                    sex_id==unlist(combo[j,.(sex_id)], use.names=F)]
  tmp.interpolate <- na_interpolation(tmp, option="stine")
  na.interp[[j]] <- tmp.interpolate
}
interpolated <- rbindlist(na.interp)

# Address any remaining year/age/sex/location combinations where deaths>pop
pop.final <- copy(interpolated)
pop.final <- merge(pop.final, claude.all, by=c("age_group_id", "location_id", "sex_id", "year_id"), all.x=T)
pop.final[,rate:=deaths/population]
pop.final[,pop:=ifelse(deaths>population & is.na(deaths)==F, ceiling(population), population)]
pop.final[,rate2:=deaths/pop]
pop.final[rate2>1]
pop.final[,pop:=ifelse(rate2>1 & age_group_id!=2 & is.na(deaths)==F, deaths, pop)]
pop.final[,rate3:=deaths/pop]
pop.final[rate3>1]
pop.final[,c("population", "deaths", "rate", "rate2", "rate3"):=NULL]
setnames(pop.final, "pop", "population")

# save the prepped population file
saveRDS(pop.final, file=paste0(scratch, "PATH/population_re_nchs_split_", date, ".rds"))
#pop.final <- readRDS(file=paste0(scratch, "PATH/population_re_nchs_split_2020_07_13.rds"))

#Fix parent_id variable
pop.scaled <- merge(pop.final, gbd.pop.scale, 
                    by.x=c("parent_id", "sex_id", "year_id", "age_group_id"), by.y=c("location_id", "sex_id", "year_id", "age_group_id"))
pop.scaled[, population := population * gbd/sum(population), by='parent_id,year_id,sex_id,age_group_id']

# Save the scaled prepped population file
saveRDS(pop.scaled, file=paste0(scratch, "PATH/population_scaled_re_nchs_split_", date, ".rds"))
#pop.scaled <- readRDS(file=paste0(scratch, "PATH/population_scaled_re_nchs_split_2020_07_13.rds"))

# Compare effects of scaling
pop.final[,type:="unscaled"]
pop.scaled[,type:="scaled"]
pop.scale.comp <- rbind(pop.final, pop.scaled, fill=T)
pop.scale.comp[, c("gbd", "location_name"):=NULL]
pop.scale.comp <- merge(pop.scale.comp, map[, .(location_id, location_name)], by="location_id")
pop.scale.comp[,state:=tstrsplit(location_name, split="; ", keep=1)]
pop.scale.comp[,group:=tstrsplit(location_name, split="; ", keep=2)]
pop.scale.comp <- merge(pop.scale.comp, age_info[,.(age_group_id, age_group_name)], by="age_group_id", all.x=T)
pop.scale.comp[,age_group_name:=ifelse(age_group_id==388, "1 to 5 months", ifelse(age_group_id==389, "6 to 11 months", age_group_name))]
pop.scale.comp[,sex:=ifelse(sex_id==1, "Male", ifelse(sex_id==2, "Female", ifelse(sex_id==3, "Both", NA)))]


states <- unique(unlist(pop.scale.comp$state, use.names=F))
groups <- unique(unlist(pop.scale.comp$group, use.names=F))
detailed <- unique(unlist(pop.scale.comp$location_name, use.names=F))

pdf(file=paste0(plot_dir, "PATH/pop_scaling_comp_", date, ".pdf"), width=14, height=8.5)
for (d in detailed){
  dt.graph <- pop.scale.comp[location_name==d]
  dt.graph[,age_name:=factor(age_group_name, levels=c("Early Neonatal", "Late Neonatal", "1 to 5 months", "6 to 11 months",
                                                      "12 to 23 months", "2 to 4", "5 to 9", "10 to 14", "15 to 19", "20 to 24",
                                                      "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54",
                                                      "55 to 59", "60 to 64", "65 to 69", "70 to 74", "75 to 79", "80 to 84",
                                                      "85 to 89", "90 to 94", "95 plus"))] 
  
  p <- ggplot(data=dt.graph, aes(x=year_id, y=population, color=factor(type), linetype=factor(sex))) +
    geom_line() +
    theme_bw() +
    facet_wrap(~factor(age_name), scales="free_y") +
    theme(legend.title=element_blank()) +
    labs(x="Year", y="Population") +
    ggtitle(d)
  
  print(p)
}
dev.off()

# Pull population information for global/sr/r
pop.add <- get_population(location_id='all', location_set_id=35, year_id=c(1980:2022), sex_id=c(1,2,3), age_group_id=ages_agg, 
                          decomp_step=step, gbd_round_id=7)
pop.add[,run_id:=NULL]

# Create aggregates
pop.agg.age <- pop.scaled[, lapply(.SD, sum.na), by=.(location_id,year_id, sex_id), .SDcols = c("population")]
pop.agg.age[,age_group_id:=22]
pop.agg.agesex <- pop.scaled[, lapply(.SD, sum.na), by=.(location_id,year_id), .SDcols=c("population")]
pop.agg.agesex[,age_group_id:=22]
pop.agg.agesex[,sex_id:=3]
pop.agg.sex <- pop.scaled[, lapply(.SD, sum.na), by=.(location_id,year_id,age_group_id), .SDcols=c("population")]
pop.agg.sex[,sex_id:=3]

# Generate age aggregates for modeling in ST-GPR
pop.agg.under1 <- pop.scaled[age_group_id %in% unique(under1$age_group_id), lapply(.SD, sum.na), by=.(location_id, year_id, sex_id), .SDcols=c("population")]
pop.agg.under1[,age_group_id:=28]
pop.agg.five <- pop.scaled[age_group_id %in% unique(five$age_group_id), lapply(.SD, sum.na), by=.(location_id, year_id, sex_id), .SDcols=c("population")]
pop.agg.five[,age_group_id:=5]
pop.agg.eightyfive <- pop.scaled[age_group_id %in% unique(eightyfive$age_group_id), lapply(.SD, sum.na), by=.(location_id, year_id, sex_id), .SDcols=c("population")]
pop.agg.eightyfive[,age_group_id:=160]

# Rbind everything together
pop.upload <- rbind(pop.scaled[, .(sex_id, year_id, age_group_id, location_id, population)], #US R/E detailed 
                    pop.agg.age, pop.agg.agesex, pop.agg.sex, #US R/E aggregates
                    pop.agg.under1, pop.agg.five, pop.agg.eightyfive,
                    pop.add) #Other locations in hierarchy 
pop.upload[,lower := NA]
pop.upload[,upper := NA]

# Write file for upload
write.csv(pop.upload, file=paste0(root, "PATH/us_re_upload_interpolated_", date, ".csv"), row.names=F)

# Check that everything sums up as it should
re.pop <- pop.upload[location_id>600]
re.pop <- merge(re.pop, map[,.(location_id, parent_id)], by="location_id") 
re.pop.agg <- re.pop[, lapply(.SD, sum.na), by=.(parent_id, year_id, sex_id, age_group_id), .SDcols = c("population")]
setnames(re.pop.agg, "population", "pop.re")

re.pop.agg.us <- re.pop[, lapply(.SD, sum.na), by=.(year_id, sex_id, age_group_id), .SDcols = c("population")]
setnames(re.pop.agg.us, "population", "pop.re")
re.pop.agg.us[,location_id:=102]

#  R/E locations -> state
check <- merge(re.pop.agg, gbd.pop, 
               by.x=c("parent_id", "sex_id", "year_id", "age_group_id"), 
               by.y=c("location_id", "sex_id", "year_id", "age_group_id"))
check[,diff:=abs(pop.re - population)]
max(check$diff)

# R/E locations -> states -> US
check.us <- merge(re.pop.agg.us, pop.add, by=c("sex_id", "year_id", "location_id", "age_group_id"))
check.us[,diff:=abs(pop.re - population)]
max(check.us$diff)
check.us[diff>1e-4]

# Plots
# One page per state; x=year, y=pop, facet_wrap by age
us.re[,race_name:=tstrsplit(location_name, split="; ", keep=2)]
pop.scaled <- merge(pop.scaled, us.re[, c("location_id", "race_name")], by="location_id")

state_names <- unique(unlist(pop.scaled$location_name, use.names=F))

pdf(file=paste0(plot_dir, "PATH/pop_age_", date, ".pdf"), width=14, height=8.5)
for (i in 1:length(state_names)){
  df.graph <- subset(pop.scaled, location_name==state_names[i])
  # df.graph[,race_name :=ifelse(race==1, "Non-Hispanic White", ifelse(race==2, "Non-Hispanic Black", 
  # 					  ifelse(race==3, "Non-Hispanic Other", ifelse(race==4, "Hispanic", NA))))]
  df.graph[,sex_name :=ifelse(sex_id==1, "Male", "Female")]
  df.graph[,age_group := factor(age_group_id, levels=c(2,3,388,389,238,34,6:20,30,31,32,235))]
  p <- ggplot(data=df.graph, aes(x=year_id, y=population, color=factor(race_name), linetype=factor(sex_name))) +
    geom_line() +
    theme_bw() +
    facet_wrap(~factor(age_group), scales="free_y") +
    theme(legend.title=element_blank()) +
    labs(x="Year", y="Population") +
    ggtitle(as.character(state_names[i]))
  q <- ggplot(data=df.graph[race_name=="Hispanic, Any race"], aes(x=year_id, y=population, linetype=factor(sex_name))) +
    geom_line() +
    theme_bw() +
    facet_wrap(~factor(age_group), scales="free_y") +
    theme(legend.title=element_blank()) +
    labs(x="Year", y="Population") +
    ggtitle(paste0(as.character(state_names[i]), ", Hispanic, Any race"))
  r <- ggplot(data=df.graph[race_name=="Non-Hispanic, Black"], aes(x=year_id, y=population, linetype=factor(sex_name))) +
    geom_line() +
    theme_bw() +
    facet_wrap(~factor(age_group), scales="free_y") +
    theme(legend.title=element_blank()) +
    labs(x="Year", y="Population") +
    ggtitle(paste0(as.character(state_names[i]), ", Non-Hispanic, Black"))
  s <- ggplot(data=df.graph[race_name=="Non-Hispanic, Other races"], aes(x=year_id, y=population, linetype=factor(sex_name))) +
    geom_line() +
    theme_bw() +
    facet_wrap(~factor(age_group), scales="free_y") +
    theme(legend.title=element_blank()) +
    labs(x="Year", y="Population") +
    ggtitle(paste0(as.character(state_names[i]), ", Non-Hispanic, Other races"))
  t <- ggplot(data=df.graph[race_name=="Non-Hispanic, White"], aes(x=year_id, y=population, linetype=factor(sex_name))) +
    geom_line() +
    theme_bw() +
    facet_wrap(~factor(age_group), scales="free_y") +
    theme(legend.title=element_blank()) +
    labs(x="Year", y="Population") +
    ggtitle(paste0(as.character(state_names[i]), ", Non-Hispanic, White"))
  print(p)
  print(q)
  print(r)
  print(s)
  print(t)
}
dev.off()

# One plot per state, x=age_start, y=pop, color=year
pdf(file=paste0(plot_dir, "PATH/pop_year_", date, ".pdf"), width=17, height=8.5)
for (i in 1:length(state_names)){
  df.graph <- subset(pop.scaled, location_name==state_names[i])
  df.graph[,sex_name := ifelse(sex_id==1, "Male", "Female")]
  df.graph[,age_group := factor(age_group_id, levels=c(2,3,388,389,238,34,6:20,30,31,32,235))]
  p <- ggplot(data=df.graph, aes(x=factor(age_group), y=population, group = interaction(factor(year_id), factor(race_name)),
                                 color=factor(year_id), linetype=factor(race_name))) +
    geom_line() +
    theme_bw() +
    facet_wrap(~sex_name) +
    theme(legend.title=element_blank()) +
    labs(x="Age Group", y="Population") +
    ggtitle(as.character(state_names[i]))
  print(p)
}
dev.off()

# One plot per state, scatter pop.final vs. pop.scaled by age, color=year
setnames(pop.final, "population", "pop.original")
setnames(pop.scaled, "population", "pop.scaled")
merged <- merge(pop.final[, list(location_id, sex_id, year_id, age_group_id, pop.original)],
                pop.scaled[, list(location_id, location_name, sex_id, year_id, age_group_id, pop.scaled, race_name)],
                by=c("location_id", "sex_id", "year_id", "age_group_id"))

pdf(file=paste0(plot_dir, "PATH/pop_scaling_", date, ".pdf"), width=17, height=8.5)
for (i in 1:length(state_names)){
  df.graph <- subset(merged, location_name==state_names[i])
  df.graph[,sex_name := ifelse(sex_id==1, "Male", "Female")]
  p <- ggplot(data=df.graph, aes(x=pop.original, y=pop.scaled, #group = interaction(factor(year_id), factor(race_name)),
                                 color=factor(year_id), shape=factor(race_name))) +
    geom_point() +
    geom_abline() +
    theme_bw() +
    facet_wrap(~sex_name) +
    theme(legend.title=element_blank()) +
    labs(x="Population, Original", y="Population, Scaled") +
    ggtitle(as.character(state_names[i]))
  print(p)
}
dev.off()

# compare to old population estimates
old_pop <- get_population(decomp_step="usa_re", gbd_round_id=7, location_id=unique(pop.upload$location_id), 
                          year_id=c(1980:2022), sex_id=c(1,2,3), age_group_id=unique(pop.upload$age_group_id))
setnames(old_pop, "population", "old_population")

combined <- merge(pop.upload[, c("age_group_id", "location_id", "year_id", "sex_id", "population")], 
                  old_pop[, c("age_group_id", "location_id", "year_id", "sex_id", "old_population")], 
                  by=c("age_group_id", "location_id", "year_id", "sex_id"), all=T)

combined_complete <- combined[!is.na(population)&!is.na(old_population),]

pdf(paste0(plot_dir, "PATH/compare_to_old_pop_", date, ".pdf"), width=12, height=7)
for(age in unique(combined_complete$age_group_id)){
  p1 <- ggplot(combined_complete[location_id==1&age_group_id==age,], aes(x=old_population, y=population, color=factor(year_id), shape=factor(sex_id, levels=c(1:3), labels=c("Males", "Females", "Both")))) + theme_bw() + geom_point() + 
    geom_abline(slope = 1, intercept = 0) + ggtitle(paste0("Global - age group ", age)) + labs(x="Old Population", y="New Population", color="", shape="")
  p2 <- ggplot(combined_complete[location_id%ni%c(1, 102, unique(us$location_id), unique(us.re$location_id))&age_group_id==age,], aes(x=old_population, y=population, color=factor(year_id), shape=factor(sex_id, levels=c(1:3), labels=c("Males", "Females", "Both")))) + theme_bw() + geom_point() + 
    geom_abline(slope = 1, intercept = 0) + ggtitle(paste0("Non-U.S. locations - age group ", age)) + labs(x="Old Population", y="New Population", color="", shape="")
  p3 <- ggplot(combined_complete[location_id==102&age_group_id==age,], aes(x=old_population, y=population, color=factor(year_id), shape=factor(sex_id, levels=c(1:3), labels=c("Males", "Females", "Both")))) + theme_bw() + geom_point() + 
    geom_abline(slope = 1, intercept = 0) + ggtitle(paste0("National level - age group ", age)) + labs(x="Old Population", y="New Population", color="", shape="")
  p4 <- ggplot(combined_complete[location_id%in%unique(us$location_id)&age_group_id==age,], aes(x=old_population, y=population, color=factor(year_id), shape=factor(sex_id, levels=c(1:3), labels=c("Males", "Females", "Both")))) + theme_bw() + geom_point() + 
    geom_abline(slope = 1, intercept = 0) + ggtitle(paste0("State level - age group ", age)) + labs(x="Old Population", y="New Population", color="", shape="")
  p5 <- ggplot(combined_complete[location_id%in%unique(us.re$location_id)&age_group_id==age,], aes(x=old_population, y=population, color=factor(year_id), shape=factor(sex_id, levels=c(1:3), labels=c("Males", "Females", "Both")))) + theme_bw() + geom_point() + 
    geom_abline(slope = 1, intercept = 0) + ggtitle(paste0("Race/ethnicity level - age group ", age)) + labs(x="Old Population", y="New Population", color="", shape="")
  print(p1)
  print(p2)
  print(p3)
  print(p4)
  print(p5)
}
dev.off()
