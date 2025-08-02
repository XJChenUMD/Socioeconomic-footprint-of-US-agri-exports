
library(readr)

options(scipen = 200)

#Sector(Institution) classification------
Insti <- read.csv(str_c(path,"/Industry Table_546 Unaggregated.csv"))  %>% 
  mutate(IndustryDescription = gsub("\\d+\\s-\\s|\\s\\(546\\sUnaggregated\\)", "", IndustryDescription)) %>% 
  mutate(IndustryClass = "Industry") %>% 
  mutate(IndustryClass = ifelse(IndustryCode <= 538 & IndustryCode >= 535, "Not an industry", IndustryClass)) %>% 
  mutate(IndustryClass = ifelse(IndustryCode <= 8001 & IndustryCode >= 5001, "Primary factors", IndustryClass)) %>%  
  mutate(IndustryClass = ifelse(IndustryCode <= 10009 & IndustryCode >= 10001, "Household group", IndustryClass)) %>% 
  mutate(IndustryClass = ifelse(IndustryCode <= 12004 & IndustryCode >= 11001, "Government", IndustryClass)) %>% 
  mutate(IndustryClass = ifelse(IndustryCode <= 14002 & IndustryCode >= 13001, "Firms", IndustryClass)) %>% 
  mutate(IndustryClass = ifelse(IndustryCode <= 28001 & IndustryCode >= 25001, "Interregional trade", IndustryClass)) 

N <- dim(Insti)[1]

SectorCode <- Insti$IndustryCode

#Read Countyname name-------
FIPS <- read.delim(str_c(path,"/FIPS national_county2020.txt"), sep = "|",
                   header = T, stringsAsFactors = F)


#Clean Trade data (Domestic)-------
# Year <- c(2005,2014:2020,2022)
Year <- c(2022)

for (y in 1:length(Year)){
  t0 <- Sys.time()
  print(str_c("Begin-----",Year[y]))
  
  file_list <- unzip(str_c(pathdata,"/County level Trade Flows Table 2022 real 2022 price.zip"), list = TRUE)
  Trade_flows <- read_csv(unz(str_c(pathdata,"/County level Trade Flows Table 2022 real 2022 price.zip"), file_list[1]))
  
  Trade_flows %>% mutate(Destination = str_c(as.numeric(`Destination State Fips`),"-",
                                            as.numeric( `Destination County Fips`)),
                         Origin = str_c(as.numeric(`Origin State Fips`),"-",
                                        as.numeric(`Origin County Fips`))) %>% 
    select(Destination,Origin,`Commodity Code`,`Trade Value`) %>% 
    mutate(`Commodity Code` = as.numeric(substr(`Commodity Code`,2,4))) %>% 
    mutate(`Trade Value` = as.numeric(gsub("[$,]", "", `Trade Value`))) -> Trade_flows_minisize#reduce the file size as much as poss.
  
  save(Trade_flows_minisize,FIPS,SectorCode,N,Insti,
       file = str_c(pathinc,"/Trade Flows_County level_",Year[y],".Rdata"))
  
  print(str_c("Time cost for clean trade flows--",Year[y],"----",round(Sys.time()-t0,2)))
}


rm(list = ls()[-which(ls() %in% c("path","pathout","pathdata","pathinc","pathcode"))])
gc()


