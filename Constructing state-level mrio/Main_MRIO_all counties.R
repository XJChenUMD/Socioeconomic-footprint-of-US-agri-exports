

library(stringr)
library(tidyverse)
library(Matrix)
library(foreach)
library(doParallel)




#Set directory---------------
setwd("/gpfs/data1/fenggp/Xiangjie/IMPLAN")
path <- getwd()
pathdata <- str_c(path,"/Raw data/Trade flows_546 sectors")
pathdata2 <- str_c(path,"/Raw data/SAM tables")
pathscheme <- str_c(path,"/Sector and region agg scheme")

pathcode <- str_c(path,"/Code_MRIO")

pathinc <- str_c(path,"/inc_MRIO");dir.create(pathinc)

source(str_c(pathcode,"/Module 0_Define encrption function.R"))

#Load and clean required raw data----
# source(str_c(pathcode,"/Module 1_Load and clean trade data_all counties_546sectors.R"))
# source(str_c(pathcode,"/Module 2_Load and clean SAM data_all counties_546sectors_v2.R"))


##############Construct MRIO tables###############

#State level_Agri Analysis------
#Load sector, region and instition aggregation scheme
TaskName <- "State level_Agri Analysis"
TargetYEAR <- 2022

Industry_agg <- read.csv(str_c(pathscheme,"/Industry list_546 Aggregated_Agriculture analysis.csv"))
FactorFD_agg <- read.csv(str_c(pathscheme,"/Factor and demand list_23 Aggregated.csv"))
FIP_agg <- read.csv(str_c(pathscheme,"/FIPS_Region aggregation_State.csv"))

source(str_c(pathcode,"/Module 3_Construct MRIO_flexible scheme.R"))
#-----------


#Bay region analysis (for Aixi)------
#Load sector, region and instition aggregation scheme
TaskName <- "Hiybird level_Bay Analysis"
TargetYEAR <- 2018:2021

Industry_agg <- read.csv(str_c(pathscheme,"/Industry list_81 Aggregated.csv"))
FactorFD_agg <- read.csv(str_c(pathscheme,"/Factor and demand list_23 Aggregated.csv"))
FIP_agg <- read.csv(str_c(pathscheme,"/FIPS_Region aggregation_Bay region.csv"))

source(str_c(pathcode,"/Module 3_Construct MRIO_flexible scheme.R"))
#-----------

#County level energy transition impact-----
#Load sector, region and instition aggregation scheme
Industry_agg <- read.csv(str_c(pathscheme,"/Industry list_546 Aggregated_GCAM 14 sector.csv"))
FactorFD_agg <- read.csv(str_c(pathscheme,"/Factor and demand list_23 Aggregated.csv"))
FIP_agg <- read.csv(str_c(pathscheme,"/FIPS_Region aggregation_GCAM.csv"))
source(str_c(pathcode,"/Module 3_Construct MRIO_GCAM.R"))
#-----------

#State level energy transition impact (for Jiaxun)----
#Load sector, region and instition aggregation scheme
TaskName <- "GCAM State Enenrgy Transition"
TargetYEAR <- 2022

Industry_agg <- read.csv(str_c(pathscheme,"/Industry list_54_Aggregated_Jiaxun.csv"))
FactorFD_agg <- read.csv(str_c(pathscheme,"/Factor and demand list_23 Aggregated.csv"))
FIP_agg <- read.csv(str_c(pathscheme,"/FIPS_Region aggregation_State.csv"))
source(str_c(pathcode,"/Module 3_Construct MRIO_flexible scheme.R"))
# source(str_c(pathcode,"/Module 3_Construct MRIO_State level Energy transition GCAM.R"))
#-------------




