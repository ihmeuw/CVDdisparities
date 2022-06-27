# Code to generate classification ratios

# Date, functions
date <- gsub("-", "_", Sys.Date())
sum.na <- function(x) {sum2 = sum(x, na.rm=T)}
`%ni%` <- Negate(`%in%`)

# Paths
jpath <- "/FILEPATH/"
data.dir <- "/FILEPATH/"
plot.dir <- paste0(jpath, "/FILEPATH/")
code.dir <- "/FILEPATH/" 
scratch <- "/FILEPATH/"

# Libraries
suppressMessages(library(R.utils))
library(openxlsx)
library(ggplot2)
library(rhdf5)
library(matrixStats) 
suppressMessages(library(data.table))

# Load central functions
suppressMessages(sourceDirectory("/FILEPATH/", modifiedOnly=FALSE))
source("/FILEPATH/utility.r")

# Get age and location information
age_info <- get_age_metadata(age_group_set_id=19, gbd_round_id=7)
age_info[,age_group_years_end:=ifelse(age_group_years_end>=5, age_group_years_end-0.1, age_group_years_end)]
# names(age_info) <- gsub("_group_years", "", names(age_info))
setkey(age_info,age_group_years_start,age_group_years_end)

locs <- get_location_metadata(location_set_id=105, gbd_round_id=7)

# Load and reshape classification ratios file
ratios <- data.table(read.xlsx(paste0(data.dir, "arias_classification_ratio.xlsx")))
ratios[,sex:=ifelse(sex=="male", "Male", ifelse(sex=="female", "Female", ifelse(sex=="both", "Both", NA)))]
ratios[,sex_id:=ifelse(sex=="Male", 1, ifelse(sex=="Female", 2, ifelse(sex=="Both", 3, NA)))]

age_sex <- ratios[type=="age_sex"]
setnames(age_sex, "classification_ratio", "cr.age_sex")
overall <- ratios[type=="overall"]
setnames(overall, "classification_ratio", "cr.overall")
concentration <- ratios[type=="concentration"]
setnames(concentration, "classification_ratio", "cr.concentration")
region <- ratios[type=="region"]
setnames(region, "classification_ratio", "cr.region")

dt <- merge(age_sex[,.(group, cr.age_sex, sex, sex_id, age, year_start, year_end)], overall[,.(group, cr.overall, sex, sex_id, year_start, year_end)], 
		by=c("group", "sex", "sex_id", "year_start", "year_end"))
dt <- merge(dt, region[,.(group, cr.region, region_name, year_start, year_end)], by=c("group", "year_start", "year_end"), allow.cartesian=TRUE)
dt[,age_start:=tstrsplit(age, split=" to ", keep=1)]
dt[,age_start:=as.numeric(ifelse(age=="75+", 75, age_start))]
dt[,age_end:=tstrsplit(age, split=" to ", keep=2)]
dt[,age_end:=as.numeric(ifelse(age=="75+", 125, age_end))]
dt[,cr.total:=cr.overall * (cr.age_sex/cr.overall) * (cr.region/cr.overall)]

# Load region/state mapping file
map <- data.table(read.xlsx(paste0(data.dir, "census_region_state.xlsx")))

# Merge mapping file and data table
dt <- merge(dt, map, by="region_name", allow.cartesian=T)
setkey(dt, age_start, age_end)

# Merge age information
dt <- foverlaps(dt, age_info, by.x=c("age_start", "age_end"), by.y=c("age_group_years_start", "age_group_years_end"))
dt[,location_name:=paste0(state_name, "; ", group)]

# Duplicate results for aggregate age groups
under1 <- dt[age_group_id==2]
under1[,age_group_id:=28]

under5 <- dt[age_group_id==2]
under5[,age_group_id:=5]

eightyfive <- dt[age_group_id==235]
eightyfive[,age_group_id:=160]

dt <- rbind(dt, under1, under5, eightyfive)

# Merge location information
dt <- merge(dt, locs[,.(location_name, location_id)], by="location_name")

# Add on placeholder for other; set cr.age_sex=1
other <- dt[location_name %like% "Black", .(location_name, sex_id, age_group_id, cr.age_sex, year_start, year_end)]
other[,cr.age_sex:=1]
other[,location_name:=gsub("Non-Hispanic, Black", "Non-Hispanic, Other races", location_name)]
other <- merge(other, locs[,.(location_name, location_id)], by="location_name")

dt <- rbind(dt, other, fill=T)

saveRDS(dt, file=paste0(scratch, "classification_ratios_", date, ".rds"))
