
library(readr)
options(scipen = 200)

pathscheme <- "C:/Users/xiang/OneDrive\\IMPLAN/Sector and region agg scheme"
pathdata2 <- "D:/"

#Sector(Institution) classification------
Insti <- read.csv(str_c(pathscheme,"/Industry Table_546 Unaggregated.csv"))  %>% 
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

FIPS <- read.delim(str_c(pathscheme,"/FIPS national_county2020.txt"), sep = "|",
                   header = T, stringsAsFactors = F) %>% 
  mutate(CountyFullFIPS = str_c(STATEFP,"-",COUNTYFP)) #Read Countyname name
#-------------

#Loop by year
#=================
YEAR <- c(2018:2023)

for (y in YEAR) {
  t0 <- Sys.time()
  
  # AllFileList <- unzip(str_c(pathdata2,"/IMPLAN",y,".zip"),list = T)
  AllFileList <- list.files(str_c(pathdata2,"/IMPLAN",y), recursive = TRUE, full.names = TRUE)
  
  SIO_Table <- array(0,dim = c(N,N,length(FIPS$CountyFullFIPS)),
                     dimnames = list(SectorCode,SectorCode,FIPS$CountyFullFIPS))
  
  #Clean Single regional IO tables-------
  FIPS$Exist <- NA
  
  for (i in 1:length(FIPS$CountyFullFIPS)) {
    t1 <- Sys.time()
    print(str_c("Begin--", i, "--",FIPS$COUNTYNAME[i],"--",FIPS$STATE[i]))
    
    ##Constructing IO table--------------------
    a <- which(str_detect(AllFileList, regex(FIPS$STATE[i], ignore_case = F)) == T)
    b <- which(str_detect(AllFileList, regex(str_c("/",FIPS$COUNTYNAME[i]), ignore_case = F)) == T)
    
    if (FIPS$STATE[i] %in% c("AK","HI","IL","IN","MD","NM","PA","LA","IA")) {
      ignore_words <- c(" County", "'s County", " Municipality",
                        " Borough", "Parish", " Island"," District",
                        " City and Borough"," Census Area")
      clean_AllFileList <- substr(str_replace_all(AllFileList, str_c(ignore_words, collapse = "|"), ""),
                                  67,nchar(str_replace_all(AllFileList, str_c(ignore_words, collapse = "|"), "")))
      clean_AllFileList <- str_replace_all(clean_AllFileList, str_c(c(" ","'"), collapse = "|"), "")
      clean_FIPS <- str_replace_all(FIPS$COUNTYNAME, str_c(ignore_words, collapse = "|"), "")
      clean_FIPS <- str_replace_all(clean_FIPS, str_c(c(" ","'"), collapse = "|"), "")
      a <- which(str_detect(clean_AllFileList, regex(FIPS$STATE[i], ignore_case = F)) == T)
      b <- which(str_detect(clean_AllFileList, regex(str_c("/",clean_FIPS[i]), ignore_case = T)) == T)
    }
    
    #Special treatment
    if(FIPS$COUNTYNAME[i] %in% c("Hawaii County","White County","Will County",
                                 "Baltimore County","Jefferson Parish","Clay County")) {
      b <- which(str_detect(AllFileList, regex(str_c("/",FIPS$COUNTYNAME[i]), ignore_case = F)) == T)
    }
    
    if(FIPS$COUNTYNAME[i] %in% c("Doña Ana County")) {
      b <- which(str_detect(AllFileList, regex(str_c("/","Dona Ana"), ignore_case = F)) == T)
    }
    
    tar <- intersect(a,b)#there are may two counties in two states have the same name
    #-----------
    
    ##Constructing IO table--------------------
    if (sum(tar) != 0){
      FIPS$Exist[i] <- "YES"
      
      # Raw_Data <- read.csv(unz(str_c(pathdata2,"/IMPLAN",y,".zip"), AllFileList$Name[tar]))#unit: current year $
      Raw_Data <- read.csv(AllFileList[tar])#unit: current year $
      
      for (j in 1:N) {
        colcode <- as.numeric(Raw_Data$ReceivingCode[which(Raw_Data$PayingCode %in% SectorCode[j])])#n to 1
        elements <- rowsum(as.numeric(Raw_Data$Value[which(Raw_Data$PayingCode %in% SectorCode[j])]),
                           colcode,reorder = T)# not really sum sth, just to ensure same order
        SIO_Table[match( sort(unique(colcode)),SectorCode),j,i] <- elements
      }
      
      ##Checking Balance, 546 sectors--------------
      Z <- SIO_Table[1:546,1:546,i]
      VA <- SIO_Table[547:dim(SIO_Table)[1],1:546,i]
      FD <- SIO_Table[1:546,547:dim(SIO_Table)[2],i]
      Res <- SIO_Table[547:dim(SIO_Table)[1],547:dim(SIO_Table)[1],i]#Payment from household & gov to household & gov
      sum(Res);sum(VA);sum(Z);sum(FD)
      
      if(round(abs(sum(colSums(Z)+colSums(VA)-rowSums(FD)-rowSums(Z)))+ 
               sum(Raw_Data$Value)-sum(SIO_Table),2) > 1) break
    }else{#The county is missing
      FIPS$Exist[i] <- "NO"
    }
    
    print(str_c("Time cost for ----",FIPS$CountyFullFIPS[i],"----",round(Sys.time()-t1,2)))
  }
  
  save(SIO_Table,SectorCode,Insti,N,AllFileList,FIPS,
       file = str_c(pathinc,"/Single Regional IOT_County level_all counties_",y,".Rdata"))
  
  print(str_c("Time cost for reading IO tables--",y,"   :::   ",round(Sys.time()-t0,2)))
}
#=================



rm(list = ls()[-which(ls() %in% c("path","pathout","pathdata","pathinc","pathcode"))])
gc()


#Check Missing Data in the downloads
#=================
Missing_Check <- c()

for (y in YEAR) {
  t0 <- Sys.time()
  AllFileList <- list.files(str_c(pathdata2,"/IMPLAN",y), 
                            recursive = TRUE, full.names = TRUE)
  
  for (i in 1:length(FIPS$CountyFullFIPS)) {
    t1 <- Sys.time()
    print(str_c("Begin--", i, "--",FIPS$COUNTYNAME[i],"--",FIPS$STATE[i],"--",y))
    
    ##Constructing IO table--------------------
    a <- which(str_detect(AllFileList, regex(FIPS$STATE[i], ignore_case = F)) == T)
    b <- which(str_detect(AllFileList, regex(str_c("/",FIPS$COUNTYNAME[i]), ignore_case = F)) == T)
    
    if (FIPS$STATE[i] %in% c("AK","HI","IL","IN","MD","NM","PA","LA","IA")) {
      ignore_words <- c(" County", "'s County", " Municipality",
                        " Borough", "Parish", " Island"," District",
                        " City and Borough"," Census Area")
      clean_AllFileList <- substr(str_replace_all(AllFileList, str_c(ignore_words, collapse = "|"), ""),
                                  67,nchar(str_replace_all(AllFileList, str_c(ignore_words, collapse = "|"), "")))
      clean_AllFileList <- str_replace_all(clean_AllFileList, str_c(c(" ","'"), collapse = "|"), "")
      clean_FIPS <- str_replace_all(FIPS$COUNTYNAME, str_c(ignore_words, collapse = "|"), "")
      clean_FIPS <- str_replace_all(clean_FIPS, str_c(c(" ","'"), collapse = "|"), "")
      a <- which(str_detect(clean_AllFileList, regex(FIPS$STATE[i], ignore_case = F)) == T)
      b <- which(str_detect(clean_AllFileList, regex(str_c("/",clean_FIPS[i]), ignore_case = T)) == T)
    }
    
    #Special treatment
    if(FIPS$COUNTYNAME[i] %in% c("Hawaii County","White County","Will County",
                                 "Baltimore County","Jefferson Parish","Clay County")) {
      b <- which(str_detect(AllFileList, regex(str_c("/",FIPS$COUNTYNAME[i]), ignore_case = F)) == T)
    }
    
    if(FIPS$COUNTYNAME[i] %in% c("Doña Ana County")) {
      b <- which(str_detect(AllFileList, regex(str_c("/","Dona Ana"), ignore_case = F)) == T)
    }
    
    tar <- intersect(a,b)#there are may two counties in two states have the same name
    
    if (sum(tar) != 0){
      Missing_Check[i] <- "YES"
    }else{#The county is missing
      Missing_Check[i] <- "NO"
    }
    
    if(length(tar) > 1) {
      Missing_Check[i] <- "ERROR"
    }
    print(str_c("Time cost for ----",FIPS$CountyFullFIPS[i],"----",
                round(Sys.time()-t1,2),"---",Missing_Check[i]))
  }
  
  FIPS <- cbind(FIPS,Missing_Check)
  print(str_c("Time cost for reading IO tables--",y,"   :::   ",round(Sys.time()-t0,2)))
}


colnames(FIPS)[9:(8+length(YEAR))] <- str_c("Y",YEAR)
miss <- (rowSums(FIPS[,9:(8+length(YEAR))] == "YES")  != length(YEAR))

misscounty <- FIPS[miss,] 
misscounty$Position <- rownames(FIPS[miss,])

write.csv(misscounty %>% filter (!STATE %in% c("AS","GU","MP","PR","UM","VI")),
          str_c(pathdata2,"/Missiong counties_check_2018_2022.csv"))


save(FIPS,file = str_c(pathinc,"/Missing counties check_2018_2022.Rdata"))


