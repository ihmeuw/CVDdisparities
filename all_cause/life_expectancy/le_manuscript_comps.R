##Generate file with all-cause mortality envelope

rm(list = setdiff(ls(), c(lsf.str(), "jpath", "hpath", "os")))

# Load libraries
library(ggplot2)
library(openxlsx)
suppressMessages(library(R.utils))
library(data.table)

# Date and log information
date <- gsub("-", "_", Sys.Date())

# Directories
data.dir <- "/FILEPATH/"

# Functions
sum.na <- function(x) {sum2 = sum(x, na.rm=T)}
prop <- function(x) {x/sum(x)}
`%ni%` <- Negate(`%in%`)

# Source central functions
suppressMessages(sourceDirectory("/FILEPATH/", modifiedOnly=FALSE))

# Read in data file
dt <- fread(paste0(data.dir, "Reference_Data_Appendix_Tables_dimensional_smoothing_2022_02_15_claude10_false_160_2019_with_nho__2022_02_18.csv"))
names(dt) <- c("age", "sex", "group", "state", 
			   "mean.1990", "lower.1990", "upper.1990", 
			   "mean.2019", "lower.2019", "upper.2019", 
			   "mean.pc", "lower.pc", "upper.pc")
dt[,change:=ifelse(lower.pc<0 & upper.pc>0, "none",
			ifelse(mean.pc<0 & upper.pc<0, "decrease",
			ifelse(mean.pc>0 & lower.pc>0, "increase", NA)))]

dt <- dt[age=="At Birth" & group!="Non-Hispanic, Other races"]
dt <- dt[is.na(mean.pc)==F]

dt.wide <- reshape(dt[,.(sex, group, state, change)], idvar=c("sex", "state"), timevar="group", direction="wide")
names(dt.wide) <- c("sex", "state", "hisp", "nhb", "nhw")
dt.wide[rowSums(sapply(dt.wide, '%in%', c("none", "decrease"))) > 0,][order(state,sex)]
dt.wide[rowSums(sapply(dt.wide, '%in%', c("none", "decrease"))) > 0,][sex=="Female"]
dt.wide[rowSums(sapply(dt.wide, '%in%', c("none", "decrease"))) > 0,][sex=="Male"]
with(dt.wide[hisp=="increase" & nhb=="increase" & nhw=="increase"], table(state, sex))