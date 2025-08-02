


UNData <- read.csv(str_c(pathdata,"/DataJobID-2890014_2890014_USexport2024detail4digit.csv")) %>% 
  select(ProductCode,ProductDescription,PartnerISO3,
           PartnerName,TradeValue.in.1000.USD)
# UNData %>% select(ProductCode,ProductDescription) %>% 
#   distinct() %>% write.csv(str_c(pathdata,"/UNDataHS22Code4digit.csv"))

MatchTable <- read.csv(str_c(pathdata,"/UNCOMTRADE HS code match IMPLAN code Agri Sector.csv"))

UNData %>% filter(PartnerISO3 %in% "WLD") %>% 
  rename("TrdWld" = TradeValue.in.1000.USD) %>% 
  select(-c(PartnerISO3,PartnerName)) %>% 
  left_join(UNData %>% filter(!PartnerISO3 %in% "WLD") %>% 
              group_by(ProductCode, ProductDescription) %>% 
              summarise(TradeValue.in.1000.USD = sum(TradeValue.in.1000.USD)) %>% ungroup() %>% 
              select(ProductCode,ProductDescription,TradeValue.in.1000.USD) %>% 
              rename("Trdnowld" = TradeValue.in.1000.USD) ) %>% 
  mutate(TradeValue.in.1000.USD = TrdWld - Trdnowld) %>% 
  mutate(PartnerISO3 = "RoW", PartnerName = "Rest of World") %>% 
  select(ProductCode,ProductDescription,PartnerISO3,
         PartnerName,TradeValue.in.1000.USD) %>% 
  bind_rows(UNData %>% filter(!PartnerISO3 %in% c("CHN","JPN","KOR","MEX","CAN","WLD")) %>% 
              group_by(ProductCode, ProductDescription) %>% 
              summarise(TradeValue.in.1000.USD = sum(TradeValue.in.1000.USD)) %>% 
              mutate(PartnerISO3 = "EU", PartnerName = "European Union")) %>% 
  bind_rows(UNData %>% filter(PartnerISO3 %in% c("CHN","JPN","KOR","MEX","CAN"))) -> UNData_Regagg


UNData_Regagg %>% merge(MatchTable, by = "ProductCode",no.dups = F) %>% 
  group_by(PartnerISO3,PartnerName,IMPLAN_NewCode,IMPLAN_NewDesc) %>% 
  summarise(Trd = sum(TradeValue.in.1000.USD)) %>% group_by(IMPLAN_NewCode, IMPLAN_NewDesc) %>%       
  mutate(total_trd = sum(Trd, na.rm = TRUE),             
         share = Trd / total_trd) %>%
  ungroup() %>% 
  mutate( PartnerName = if_else(PartnerName == "Korea, Rep.", "Korea", PartnerName)) -> TradeClean

gloregnam <- unique(TradeClean$PartnerName)
gloregnamabbr <- unique(TradeClean$PartnerISO3)
G <- length(gloregnam)

secnam <- sort(unique(TradeClean$IMPLAN_NewCode))

TradeClean %>% select(PartnerName,IMPLAN_NewCode,share) %>% 
  pivot_wider(id_cols = IMPLAN_NewCode,names_from = PartnerName,values_from = share) -> Agri_Destin_Implan
Agri_Destin_Implan <- as.data.frame(Agri_Destin_Implan)
rownames(Agri_Destin_Implan) <- Agri_Destin_Implan$IMPLAN_NewCode
Agri_Destin_Implan <- as.matrix(Agri_Destin_Implan[,-1] )
Agri_Destin_Implan[is.na(Agri_Destin_Implan)] <- 0

USexport_Reg <- TradeClean %>% group_by(PartnerName) %>% summarise(Trd = sum(Trd))

save(USexport_Reg,Agri_Destin_Implan,gloregnam,gloregnamabbr,secnam,
     file = str_c(pathinc,"/US export pattern_UNCOMTRADE 2024.Rdata"))

rm(list = ls()[-which(ls() %in% c("path","pathout","pathdata","pathinc","pathcode"))])
gc()




