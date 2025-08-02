

library(tidyverse)

setwd("H:\\My Drive\\USGS GEOG project\\Trade conflicts and agriculture\\Tradewar_Agrishock")

path <- getwd()

pathcode <- str_c(path,"/code");dir.create(pathcode)
pathdata <- str_c(path,"/data");dir.create(pathdata)
pathinc <- str_c(path,"/inc");dir.create(pathinc)
pathout <- str_c(path,"/out_20250625");dir.create(pathout)


#Module 1. Clean US agricultural export destination based on GTAP data (2017)
# source(str_c(pathcode,"/US agri export destination_GTAP.R"))

#Module 1. Clean US agricultural export destination based on UMCOMTRADE (2022)
source(str_c(pathcode,"/US agri export destination_UMCOMTRADE.R"))


#Module 2. Calculate the impacts of agriculture export.
source(str_c(pathcode,"/Impact of agriculture export_IMPLAN_v3.R"))


#Module 3. Results visualization
source(str_c(pathcode,"/Visualization.R"))