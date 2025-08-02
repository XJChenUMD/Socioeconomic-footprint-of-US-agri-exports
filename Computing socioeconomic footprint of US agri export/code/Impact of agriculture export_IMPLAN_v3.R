


#Load required data-----
load("C:/Users/xiang/OneDrive/IMPLAN/inc_MRIO/IMPLAN MRIO State level_Agri Analysis_2022-Ver-2025-05-27.Rdata")
A[is.na(A)] <- 0;A[is.infinite(A)] <- 0;A[is.nan(A)] <- 0
Leontief <- solve(diag(G_agg*N_sec_agg) - as.matrix(A))

load("C:/Users/xiang/OneDrive/IMPLAN/inc_MRIO/IMPLAN SAM add element State level_Agri Analysis_2022-Ver-2025-05-27.Rdata")
load(str_c(pathinc,"/US export pattern_UNCOMTRADE 2024.Rdata"))

Industry_agg <- read.csv(str_c(pathdata,"/Industry list_546 Aggregated_Agriculture analysis.csv"))#With sector abbr.

EMP_data <- read.csv(str_c(pathdata,"/State level Industry Employment_2022.csv"))[-1,-1] %>%
  separate(Region, into = c("Code", "Description"), sep = " - ", convert = TRUE) %>% 
  select(-Description) %>% right_join(Industry_agg, by = c("Code" = "IndustryCode"))

fac <- EMP_data$NewCode
EMP_data <- EMP_data %>% select(-c(Code,IndustryDescription,NewCode,NewDesc))
EMP_data <- rowsum(apply(EMP_data, 2, as.numeric),fac,reorder = F,na.rm = T)

dim(EMP_data) <- c(51*N_sec_agg,1)
EMP_data[(51*N_sec_agg+1):(52*N_sec_agg)] <- 0

G <- length(colnames(Agri_Destin_Implan))
regnam_global <- colnames(Agri_Destin_Implan)


#Disaggregate Implan export to country level------
Agri_Export <- array(0, dim = c(G_agg*N_sec_agg,G),
                     dimnames = list(regsecnam_agg,regnam_global))

Export_reliance <- Agri_Export
Agri_Output <- Agri_Export
Agri_GDP <- Agri_Export
Agri_EMP <- Agri_Export

Secnam_agg <- unique(Industry_agg$NewCode)

for (i in 1:dim(Agri_Destin_Implan)[1]) {
  tar <- which(Secnam_agg %in% rownames(Agri_Destin_Implan)[i])
  Agri_Export[(1:G_agg-1)*N_sec_agg+tar,] <- t(diag(Agri_Destin_Implan[i,]) %*% pracma::repmat(t(EX_Global[(1:G_agg-1)*N_sec_agg+tar]),G,1))
  Export_reliance[(1:G_agg-1)*N_sec_agg+tar,] <- Agri_Export[(1:G_agg-1)*N_sec_agg+tar,]/X[(1:G_agg-1)*N_sec_agg+tar]
  Agri_Output[(1:G_agg-1)*N_sec_agg+tar,] <- X[(1:G_agg-1)*N_sec_agg+tar]
  Agri_GDP[(1:G_agg-1)*N_sec_agg+tar,] <- ValueAdded[(1:G_agg-1)*N_sec_agg+tar]
  Agri_EMP[(1:G_agg-1)*N_sec_agg+tar,] <- EMP_data[(1:G_agg-1)*N_sec_agg+tar]
}

#Export reliance national
Export_rel_tot <- rowSums(rowsum(Agri_Export,rep(Secnam_agg,G_agg),reorder = F))/
  rowsum(as.matrix(X),rep(Secnam_agg,G_agg),reorder = F)


#Capital revenue, labor income, government revenue, employment impacts---------
#L*E
LE <- Leontief%*%Agri_Export

VA_Cap_coef <- VA_Cap/X
VA_Cap_coef[is.na(VA_Cap_coef)] <- 0;VA_Cap_coef[is.infinite(VA_Cap_coef)] <- 0

VA_Labor_coef <- VA_LaborCom/X
VA_Labor_coef[is.na(VA_Labor_coef)] <- 0;VA_Labor_coef[is.infinite(VA_Labor_coef)] <- 0

VA_TaxSub_coef <- VA_TaxSub/X
VA_TaxSub_coef[is.na(VA_TaxSub_coef)] <- 0;VA_TaxSub_coef[is.infinite(VA_TaxSub_coef)] <- 0

EMP_coef <- EMP_data/X#k person
EMP_coef[is.na(EMP_coef)] <- 0;EMP_coef[is.infinite(EMP_coef)] <- 0

Cap_Imp <- as.data.frame(as.matrix(diag(VA_Cap_coef)%*%LE))
Lab_Imp <- as.data.frame(as.matrix(diag(VA_Labor_coef)%*%LE))
Tax_Imp <- as.data.frame(as.matrix(diag(VA_TaxSub_coef)%*%LE))
Tot_Imp <- Cap_Imp+Lab_Imp+Tax_Imp
EMP_Imp <- as.data.frame(as.matrix(diag(EMP_coef)%*%LE))


# Household income impact-------------
SAM <- apply(SAM_transfer, 2, as.numeric)
SAM_str <- as.data.frame(t(t(SAM)/colSums(SAM))[-c(1:3,13:14),] )
SAM_str <- SAM_str %>% select(matches("::(1|2)$"))
SAM_str[is.na(SAM_str)] <- 0
rownames(SAM_str) <-  FactorFD_agg$NewDesc[5:13]

LAB_SAM_str <- SAM_str %>% select(matches("::(1)$"))
CAP_SAM_str <- SAM_str %>% select(matches("::(2)$"))

Lab_Imp_agg <- rowsum(Lab_Imp,rep(regnam,each = N_sec_agg),reorder = F)
Cap_Imp_agg <- rowsum(Cap_Imp,rep(regnam,each = N_sec_agg),reorder = F)


#Income impact to households------------
Hou_IMP_byDestin <- as.matrix(LAB_SAM_str) %*% as.matrix(Lab_Imp_agg)+
  as.matrix(CAP_SAM_str) %*% as.matrix(Cap_Imp_agg)

Hou_Exp_Base <- rowSums(rowsum(t(as_tibble(apply(FD, 2, as.numeric))),
                               rep(4:14,length(regnam)),reorder = F))[-c(10:11)]
names(Hou_Exp_Base) <-  FactorFD_agg$NewDesc[5:13]
HOu_ExP_IMPrate <- Hou_IMP_byDestin/Hou_Exp_Base


Hou_Imp_byState <- as.matrix(LAB_SAM_str) * pracma::repmat(rowSums(as.matrix(Lab_Imp_agg)),9,1) +
  as.matrix(CAP_SAM_str) * pracma::repmat(rowSums(as.matrix(Cap_Imp_agg)),9,1)

Hou_Exp_Base_State <- colSums(as_tibble(apply(FD, 2, as.numeric)) %>% select(!grep("::13$|::14$", colnames(FD), value = TRUE)))
dim(Hou_Exp_Base_State) <- dim(Hou_Imp_byState)
dimnames(Hou_Exp_Base_State) <- dimnames(Hou_Imp_byState)
HOu_ExP_State_IMPrate <- Hou_Imp_byState/Hou_Exp_Base_State


#Income impact per household
HouNum <- read.csv("C:\\Users\\xiang\\OneDrive\\IMPLAN\\Raw data\\All States 2022_demographics Households number 2025-07-30T0147.csv")[-(1:3),-2]
state_table <- data.frame(state_name = state.name, state_abbr = state.abb)
HouNum <- left_join(HouNum,state_table,by = c("X" = "state_name"))
colnames(HouNum) <- c("StateNam",Insti$IndustryDescription[551:559],"StateAbbr")
HouNum %>% pivot_longer(-c(StateNam,StateAbbr),names_to = "Household",values_to = "Number") -> HouNum
HouNum$Number <- as.numeric(HouNum$Number)

#Direct and indirect impact------
Direct <- (diag(length(regsecnam_agg))+A)%*%Agri_Export
Indirect <- (Leontief-diag(length(regsecnam_agg))-A)%*%Agri_Export

Cap_Dir <- as.data.frame(as.matrix(diag(VA_Cap_coef)%*%Direct))
Lab_Dir <- as.data.frame(as.matrix(diag(VA_Labor_coef)%*%Direct))
Tax_Dir <- as.data.frame(as.matrix(diag(VA_TaxSub_coef)%*%Direct))
Tot_Dir <- Cap_Dir+Lab_Dir+Tax_Dir

Cap_Indir <- as.data.frame(as.matrix(diag(VA_Cap_coef)%*%Indirect))
Lab_Indir <- as.data.frame(as.matrix(diag(VA_Labor_coef)%*%Indirect))
Tax_Indir <- as.data.frame(as.matrix(diag(VA_TaxSub_coef)%*%Indirect))
Tot_Indir <- Cap_Indir+Lab_Indir+Tax_Indir

EMP_Dir <- as.data.frame(as.matrix(diag(EMP_coef)%*%Direct))
EMP_Indir <- as.data.frame(as.matrix(diag(EMP_coef)%*%Indirect))


#Convert to flat file-----
Cap_Imp$Regsec <- regsecnam_agg
Cap_Imp$Basevalue <- VA_Cap
Cap_Imp <- Cap_Imp %>% pivot_longer(-c(Regsec,Basevalue),names_to = "Partner") %>% 
  separate(Regsec,into = c("State","Secno"),sep = "::",remove = F) %>% 
  mutate(Impact = "Total impacts",Demension = "Capital revenue")

Lab_Imp$Regsec <- regsecnam_agg
Lab_Imp$Basevalue <- VA_LaborCom
Lab_Imp <- Lab_Imp %>% pivot_longer(-c(Regsec,Basevalue),names_to = "Partner") %>% 
  separate(Regsec,into = c("State","Secno"),sep = "::",remove = F) %>% 
  mutate(Impact = "Total impacts",Demension = "Labor compensation")

Tax_Imp$Regsec <- regsecnam_agg
Tax_Imp$Basevalue <- VA_TaxSub
Tax_Imp <- Tax_Imp %>% pivot_longer(-c(Regsec,Basevalue),names_to = "Partner") %>% 
  separate(Regsec,into = c("State","Secno"),sep = "::",remove = F) %>% 
  mutate(Impact = "Total impacts",Demension = "Government revenue")

Tot_Imp$Regsec <- regsecnam_agg
Tot_Imp$Basevalue <- ValueAdded
Tot_Imp <- Tot_Imp %>% pivot_longer(-c(Regsec,Basevalue),names_to = "Partner") %>% 
  separate(Regsec,into = c("State","Secno"),sep = "::",remove = F) %>% 
  mutate(Impact = "Total impacts",Demension = "GDP")

EMP_Imp$Regsec <- regsecnam_agg
EMP_Imp$Basevalue <- EMP_data
EMP_Imp <- EMP_Imp %>% pivot_longer(-c(Regsec,Basevalue),names_to = "Partner") %>% 
  separate(Regsec,into = c("State","Secno"),sep = "::",remove = F) %>% 
  mutate(Impact = "Total impacts",Demension = "Employment")

Cap_Dir$Regsec <- regsecnam_agg
Cap_Dir$Basevalue <- VA_Cap
Cap_Dir <- Cap_Dir %>% pivot_longer(-c(Regsec,Basevalue),names_to = "Partner") %>% 
  separate(Regsec,into = c("State","Secno"),sep = "::",remove = F) %>% 
  mutate(Impact = "Direct impacts",Demension = "Capital revenue")

Lab_Dir$Regsec <- regsecnam_agg
Lab_Dir$Basevalue <- VA_LaborCom
Lab_Dir <- Lab_Dir %>% pivot_longer(-c(Regsec,Basevalue),names_to = "Partner") %>% 
  separate(Regsec,into = c("State","Secno"),sep = "::",remove = F) %>% 
  mutate(Impact = "Direct impacts",Demension = "Labor compensation")

Tax_Dir$Regsec <- regsecnam_agg
Tax_Dir$Basevalue <- VA_TaxSub
Tax_Dir <- Tax_Dir %>% pivot_longer(-c(Regsec,Basevalue),names_to = "Partner") %>% 
  separate(Regsec,into = c("State","Secno"),sep = "::",remove = F) %>% 
  mutate(Impact = "Direct impacts",Demension = "Government revenue")

Tot_Dir$Regsec <- regsecnam_agg
Tot_Dir$Basevalue <- ValueAdded
Tot_Dir <- Tot_Dir %>% pivot_longer(-c(Regsec,Basevalue),names_to = "Partner") %>% 
  separate(Regsec,into = c("State","Secno"),sep = "::",remove = F) %>% 
  mutate(Impact = "Direct impacts",Demension = "GDP")

EMP_Dir$Regsec <- regsecnam_agg
EMP_Dir$Basevalue <- EMP_data
EMP_Dir <- EMP_Dir %>% pivot_longer(-c(Regsec,Basevalue),names_to = "Partner") %>% 
  separate(Regsec,into = c("State","Secno"),sep = "::",remove = F) %>% 
  mutate(Impact = "Direct impacts",Demension = "Employment")

Cap_Indir$Regsec <- regsecnam_agg
Cap_Indir$Basevalue <- VA_Cap
Cap_Indir <- Cap_Indir %>% pivot_longer(-c(Regsec,Basevalue),names_to = "Partner") %>% 
  separate(Regsec,into = c("State","Secno"),sep = "::",remove = F) %>% 
  mutate(Impact = "Indirect impacts",Demension = "Capital revenue")

Lab_Indir$Regsec <- regsecnam_agg
Lab_Indir$Basevalue <- VA_LaborCom
Lab_Indir <- Lab_Indir %>% pivot_longer(-c(Regsec,Basevalue),names_to = "Partner") %>% 
  separate(Regsec,into = c("State","Secno"),sep = "::",remove = F) %>% 
  mutate(Impact = "Indirect impacts",Demension = "Labor compensation")

Tax_Indir$Regsec <- regsecnam_agg
Tax_Indir$Basevalue <- VA_TaxSub
Tax_Indir <- Tax_Indir %>% pivot_longer(-c(Regsec,Basevalue),names_to = "Partner") %>% 
  separate(Regsec,into = c("State","Secno"),sep = "::",remove = F) %>% 
  mutate(Impact = "Indirect impacts",Demension = "Government revenue")

Tot_Indir$Regsec <- regsecnam_agg
Tot_Indir$Basevalue <- ValueAdded
Tot_Indir <- Tot_Indir %>% pivot_longer(-c(Regsec,Basevalue),names_to = "Partner") %>% 
  separate(Regsec,into = c("State","Secno"),sep = "::",remove = F) %>% 
  mutate(Impact = "Indirect impacts",Demension = "GDP")

EMP_Indir$Regsec <- regsecnam_agg
EMP_Indir$Basevalue <- EMP_data
EMP_Indir <- EMP_Indir %>% pivot_longer(-c(Regsec,Basevalue),names_to = "Partner") %>% 
  separate(Regsec,into = c("State","Secno"),sep = "::",remove = F) %>% 
  mutate(Impact = "Indirect impacts",Demension = "Employment")

AllResults <- left_join(bind_rows(Cap_Imp,Lab_Imp,Tax_Imp,Tot_Imp,EMP_Imp,
                                  Cap_Dir,Lab_Dir,Tax_Dir,Tot_Dir,
                                  Cap_Indir,Lab_Indir,Tax_Indir,Tot_Indir,
                                  EMP_Dir,EMP_Indir),
                        Industry_agg %>% select(NewCode,NewDesc) %>% 
                          distinct() %>% mutate(NewCode = as.character(NewCode)), 
                        by = c("Secno"= "NewCode"))


#Other useful information--------
Export_rel_tot <- as.data.frame(Export_rel_tot)
colnames(Export_rel_tot) <- "value"
Export_rel_tot$Secno <-  rownames(Export_rel_tot)

Export_rel_tot <- left_join(Export_rel_tot,
          Industry_agg %>% select(NewCode,NewDesc) %>% 
            distinct() %>% mutate(NewCode = as.character(NewCode)), 
          by = c("Secno"= "NewCode"))

rowSums(Hou_IMP_byDestin)/sum(Hou_IMP_byDestin)


#Lab Cap Tax str
Revenue_str <- as_tibble(rowsum(t(as.matrix(VA)),rep(Secnam_agg,G_agg),reorder = F)/
                           rowSums(rowsum(t(as.matrix(VA)),rep(Secnam_agg,G_agg),reorder = F)))

colnames(Revenue_str) <- c("Labor compensation","Capital revenue","Government tax")
Revenue_str$Secno <- Secnam_agg
Revenue_str$Secnam <- unique(Industry_agg$NewDesc)


save(AllResults,regnam,regsecnam_agg,HouNum,
     Agri_Output,Agri_GDP,Agri_EMP,ValueAdded,EMP_data,
     Export_rel_tot,Agri_Export,Export_reliance,Industry_agg,USexport_Reg,
     SAM,SAM_str,Revenue_str,
     Hou_IMP_byDestin,Hou_Exp_Base,HOu_ExP_IMPrate,
     Hou_Imp_byState,Hou_Exp_Base_State,HOu_ExP_State_IMPrate,
     file = str_c(pathinc,"/Agri Exp impacts_IMPLAN 2022.Rdata"))


rm(list = ls()[-which(ls() %in% c("path","pathout","pathdata","pathinc","pathcode"))])
gc()
