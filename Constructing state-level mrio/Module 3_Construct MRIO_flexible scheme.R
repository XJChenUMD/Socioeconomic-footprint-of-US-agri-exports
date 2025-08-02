
for (year in TargetYEAR) {
  tt <- Sys.time()
  #Set parallel calculation----
  num_cores <- 40
  cl <- makeCluster(num_cores)
  registerDoParallel(cl)
  #-----
  
  
  #Load required data------
  load(str_c(pathinc,"/Trade Flows_County level_",year,".Rdata"))
  load(str_c(pathinc,"/Single Regional IOT_County level_all counties_",year,".Rdata"))
  load(str_c(pathinc,"/US total SAM_",year,".Rdata"))
  
  FIPS <- left_join(FIPS,FIP_agg)
  
  G <- length(FIPS$COUNTYNAME)
  N_sec <- 546
  Secnam <- SectorCode[1:N_sec]
  FDnam <- SectorCode[547:569]
  
  G_agg <- length(unique(FIPS$RegAgg))
  regnam <- unique(FIP_agg$RegAgg)
  N_sec_agg <- length(unique(Industry_agg$NewCode))
  Secnam_agg <- unique(Industry_agg$NewCode)
  
  FDnam_agg <- unique(FactorFD_agg$NewCode)
  N_fd_agg <- length(FDnam_agg)
  
  regsecnam_agg <- str_c(rep(regnam,each = N_sec_agg), "::",rep(Secnam_agg,G_agg))
  regfdnam_agg <- str_c(rep(regnam,each = N_fd_agg), "::",rep(FDnam_agg,G_agg))
  #---------------
  
  #Build an empty framework----------
  Z_local <- sparseMatrix(i = integer(0), j = integer(0),  x = numeric(0),  
                          dim = c(N_sec_agg*G_agg,N_sec_agg*G_agg),
                          dimnames = list(regsecnam_agg,regsecnam_agg))
  Z_trade <- Z_local
  
  VA <- sparseMatrix(i = integer(0), j = integer(0),  x = numeric(0),  
                     dim = c(N_fd_agg,N_sec_agg*G_agg),
                     dimnames = list(FDnam_agg,regsecnam_agg))
  
  FD_local <- sparseMatrix(i = integer(0), j = integer(0),  x = numeric(0),  
                           dim = c(N_sec_agg*G_agg,N_fd_agg*G_agg),
                           dimnames = list(regsecnam_agg,regfdnam_agg))
  
  FD_trade <- FD_local
  
  Output <- rep(0,N_sec_agg*G_agg)
  names(Output) <- regsecnam_agg
  
  FDtot <- rep(0,length(regfdnam_agg))
  names(FDtot) <- regfdnam_agg
  
  Global_import_Inter <- Output
  Domestic_import_Inter <- Output
  
  Global_export <- Output
  Domestic_export <- Output
  
  Global_import_Final <- rep(0,N_fd_agg*G_agg)
  names(Global_import_Final) <- regfdnam_agg
  
  Domestic_import_Final <- Global_import_Final
  
  Global_export_Fourth <- Global_import_Final
  
  Domestic_export_Fourth <- Global_import_Final
  #---------------
  
  
  #Fill the framework with the single-regional SAM tables------------
  for (r in 1:G_agg) {
    print(str_c("Begin ---SIO filling---",r,"--",regnam[r],"--Year--",year))
    
    m = (r-1)*N_sec_agg+1; n = r*N_sec_agg
    p = (r-1)*N_fd_agg+1; q = r*N_fd_agg
    tar <- which(FIPS$RegAgg %in% regnam[r])
    
    if (length(tar) > 1){
      Z_local[m:n,m:n] <- t(rowsum(t(rowsum(apply(SIO_Table[1:N_sec,1:N_sec,tar],1:2,sum),
                                            Industry_agg$NewCode,reorder = F)),
                                   Industry_agg$NewCode,reorder = F))
      
      VA[,m:n] <- t(rowsum(t(rowsum(apply(SIO_Table[547:569,1:N_sec,tar],1:2,sum),
                                    FactorFD_agg$NewCode,reorder = F)),
                           Industry_agg$NewCode,reorder = F))
      
      FD_local[m:n,p:q] <- t(rowsum(t(rowsum(apply(SIO_Table[1:N_sec,547:569,tar],1:2,sum),
                                             Industry_agg$NewCode,reorder = F)),
                                    FactorFD_agg$NewCode,reorder = F))
      
      Output[m:n] <- rowsum(colSums(apply(SIO_Table[,1:N_sec,tar],1:2,sum)),
                            Industry_agg$NewCode,reorder = F)
      
      FDtot[p:q] <- rowsum(as.matrix(colSums(apply(SIO_Table[,547:569,tar],1:2,sum))),
                           FactorFD_agg$NewCode,reorder = F)
      
      Global_import_Inter[m:n] <- rowsum(as.matrix(rowSums(SIO_Table[570,1:N_sec,tar])),
                                         Industry_agg$NewCode,reorder = F)
      Domestic_import_Inter[m:n] <- rowsum(as.matrix(rowSums(SIO_Table[571,1:N_sec,tar])),
                                           Industry_agg$NewCode,reorder = F)
      
      Global_export[m:n] <- rowsum(as.matrix(rowSums(SIO_Table[1:N_sec,570,tar])),
                                   Industry_agg$NewCode,reorder = F)
      Domestic_export[m:n] <- rowsum(as.matrix(rowSums(SIO_Table[1:N_sec,571,tar])),
                                     Industry_agg$NewCode,reorder = F)
      
      Global_import_Final[p:q] <- rowsum(as.matrix(rowSums(SIO_Table[570,547:569,tar])),
                                         FactorFD_agg$NewCode,reorder = F)
      Domestic_import_Final[p:q] <- rowsum(as.matrix(rowSums(SIO_Table[571,547:569,tar])),
                                           FactorFD_agg$NewCode,reorder = F)
      
      Global_export_Fourth[p:q] <- rowsum(as.matrix(rowSums(SIO_Table[547:569,570,tar])),
                                          FactorFD_agg$NewCode,reorder = F)
      Domestic_export_Fourth[p:q] <- rowsum(as.matrix(rowSums(SIO_Table[547:569,571,tar])),
                                            FactorFD_agg$NewCode,reorder = F)
    }else{
      Z_local[m:n,m:n] <- t(rowsum(t(rowsum(SIO_Table[1:N_sec,1:N_sec,tar],Industry_agg$NewCode,reorder = F)),
                                   Industry_agg$NewCode,reorder = F))
      
      VA[,m:n] <- t(rowsum(t(rowsum(SIO_Table[547:569,1:N_sec,tar],FactorFD_agg$NewCode,reorder = F)),
                           Industry_agg$NewCode,reorder = F))
      
      FD_local[m:n,p:q] <- t(rowsum(t(rowsum(SIO_Table[1:N_sec,547:569,tar],Industry_agg$NewCode,reorder = F)),
                                    FactorFD_agg$NewCode,reorder = F))
      
      Output[m:n] <- rowsum(colSums(SIO_Table[,1:N_sec,tar]),Industry_agg$NewCode,reorder = F)
      
      FDtot[p:q] <- rowsum(as.matrix(colSums(SIO_Table[,547:569,tar])),FactorFD_agg$NewCode,reorder = F)
      
      Global_import_Inter[m:n] <- rowsum(SIO_Table[570,1:N_sec,tar],Industry_agg$NewCode,reorder = F)
      Domestic_import_Inter[m:n] <- rowsum(SIO_Table[571,1:N_sec,tar],Industry_agg$NewCode,reorder = F)
      
      Global_export[m:n] <- rowsum(SIO_Table[1:N_sec,570,tar],Industry_agg$NewCode,reorder = F)
      Domestic_export[m:n] <- rowsum(SIO_Table[1:N_sec,571,tar],Industry_agg$NewCode,reorder = F)
      
      Global_import_Final[p:q] <- rowsum(as.matrix(SIO_Table[570,547:569,tar]),FactorFD_agg$NewCode,reorder = F)
      Domestic_import_Final[p:q] <- rowsum(as.matrix(SIO_Table[571,547:569,tar]),FactorFD_agg$NewCode,reorder = F)
      
      Global_export_Fourth[p:q] <- rowsum(as.matrix(SIO_Table[547:569,570,tar]),FactorFD_agg$NewCode,reorder = F)
      Domestic_export_Fourth[p:q] <- rowsum(as.matrix(SIO_Table[547:569,571,tar]),FactorFD_agg$NewCode,reorder = F)
    }
  }
  
  IOM_local <- Z_local%*%(Diagonal(x = 1/Output))
  FDM_local <- FD_local%*%(Diagonal(x = 1/FDtot))
  #---------------
  
  
  #Import pattern based on county-level trade flow data----------
  Trade_flows_minisize$Commodity.Code_agg <- Industry_agg$NewCode[match(Trade_flows_minisize$`Commodity Code`, Industry_agg$IndustryCode)]
  
  county_list <- unique(Trade_flows_minisize$Destination)
  
  Trade_flows_clean_list <- foreach(county = county_list, .combine = bind_rows, .packages = "dplyr") %dopar% {
    Trade_flows_minisize %>%
      filter(Destination == county) %>%
      group_by(Origin, Commodity.Code_agg, Destination) %>%
      summarise(Value = sum(`Trade Value`), .groups = "drop")
  }
  
  Trade_flows_clean <- bind_rows(Trade_flows_clean_list)
  
  Trade_flows_clean <- Trade_flows_clean %>%
    separate(Destination, into = c("Destin_State", "Destin_County"), sep = "-", remove = FALSE) %>%
    separate(Origin, into = c("Origin_State", "Origin_County"), sep = "-", remove = FALSE)
  
  save(Trade_flows_clean,
       file = str_c(pathinc,"/Trade_flows_clean_agg_",TaskName,"_",year,".Rdata"))
  rm(Trade_flows_minisize,SIO_Table);gc()
  # load(str_c(pathinc,"/Trade_flows_clean_agg_",TaskName,"_",year,".Rdata"))
  #----------
  
  #Decompose the domestic trade row--------
  #Use the average pattern (both for intermediate and final goods) in the nation to get a basic idea of how much intermediate goods are imported (and from where)
  #Allocate the import source based on market share in trade flows
  #Loop by county
  Z_ustot <- t(rowsum(t(rowsum(USTotal_SAM[1:N_sec,1:N_sec],Industry_agg$NewCode,reorder = F)),
                      Industry_agg$NewCode,reorder = F))
  Output_ustot <- rowsum(colSums(USTotal_SAM[,1:N_sec]),Industry_agg$NewCode,reorder = F)
  FD_ustot <- t(rowsum(t(rowsum(USTotal_SAM[1:N_sec,547:569],Industry_agg$NewCode,reorder = F)),FactorFD_agg$NewCode,reorder = F))
  FDtot_ustot <- rowsum(as.matrix(colSums(USTotal_SAM[,547:569])),FactorFD_agg$NewCode,reorder = F)
  
  IOM_ustot <- t(t(Z_ustot)/as.vector(Output_ustot))
  IOM_ustot[is.na(IOM_ustot)] <- 0
  FDM_ustot <- t(t(FD_ustot)/as.vector(FDtot_ustot))
  FDM_ustot[is.na(FDM_ustot)] <- 0
  
  for (r in 1:G_agg) {
    t0 <- Sys.time()
    
    print(str_c("Begin trade estimation---",r,"--",regnam[r],"---Year--",year))
    m = (r-1)*N_sec_agg+1; n = r*N_sec_agg
    p = (r-1)*N_fd_agg+1; q = r*N_fd_agg
    tar <- which(FIPS$RegAgg %in% regnam[r])
    
    TRADE_Matrix <- array(0,dim = c(G_agg,N_sec_agg),
                          dimnames = list(regnam,Secnam_agg))
    
    IOM_diff <- pmax(IOM_ustot - IOM_local[m:n, m:n], 10^-8)#avoid negative numbers
    IOM_Import <- IOM_diff%*%diag(Output[m:n])
    
    FDM_diff <- pmax(FDM_ustot - FDM_local[m:n, p:q], 10^-8)#avoid negative numbers
    FDM_Import <- FDM_diff%*%diag(FDtot[p:q])
    
    Trade_flows_clean %>% filter(Destination %in% FIPS$CountyFullFIPS[tar]) %>% 
      group_by(Origin,Origin_State,Origin_County,Commodity.Code_agg) %>% 
      summarise(Value = sum(Value), .groups = "drop") %>% 
      left_join(FIPS, by = c("Origin" = "CountyFullFIPS")) %>% 
      group_by(RegAgg,Commodity.Code_agg) %>%
      summarise(Value = sum(Value), .groups = "drop") -> Trade_flows_clean_county
    
    for (k in 1:N_sec_agg) {
      Trade_flows_clean_county %>% 
        filter(Commodity.Code_agg %in% Secnam_agg[k]) -> INT
      xx <- match(INT$RegAgg,regnam)
      yy <- INT$Value
      if (sum(is.na(xx)) != 0){
        nap <- which(xx %in% NA)
        xx <- xx[-nap]
        yy <- yy[-nap]
      }
      
      TRADE_Matrix[xx,k] <- yy
    }
    
    Trade_Pattern <- t(t(TRADE_Matrix)/colSums(TRADE_Matrix))
    Trade_Pattern[is.na(Trade_Pattern)] <- 0
    
    #Adjust IOM_Import and FDM_Import based on the domestic trade data
    scale <- colSums(IOM_Import)/Domestic_import_Inter[m:n]
    scale[is.na(scale) | is.infinite(scale)] <- 1
    IOM_Import <- IOM_Import/pracma::repmat(scale,N_sec_agg,1)
    
    scale <- colSums(FDM_Import)/Domestic_import_Final[p:q]
    scale[is.na(scale) | is.infinite(scale)] <- 1
    FDM_Import <- FDM_Import/pracma::repmat(scale,N_sec_agg,1)
    
    IOM_Import[is.na(IOM_Import)|is.infinite(IOM_Import)] <- 0
    FDM_Import[is.na(FDM_Import)|is.infinite(FDM_Import)] <- 0
    
    #Adjust the share of trade in intermediate and trade in final goods
    IOM_Import <- IOM_Import*
      (sum(Domestic_import_Inter[m:n])/(sum(Domestic_import_Final[p:q])+sum(Domestic_import_Inter[m:n])))/
      (sum(IOM_Import)/sum(TRADE_Matrix))
    
    FDM_Import <- FDM_Import*
      (sum(Domestic_import_Final[p:q])/(sum(Domestic_import_Final[p:q])+sum(Domestic_import_Inter[m:n])))/
      (sum(FDM_Import)/sum(TRADE_Matrix))
    
    #Save time: only run when the county is exist
    if(sum(Output[m:n]) != 0){
      #Parallel calculation
      match_idx <- lapply(1:N_sec_agg, function(i) which(rep(Secnam_agg, G_agg) %in% Secnam_agg[i]))
      
      results <- foreach(i = 1:N_sec_agg, .combine = 'c', .packages = "Matrix") %dopar% {
        sector_rows <- match_idx[[i]] 
        trade_pattern_vec <- Trade_Pattern[, i]
        
        Z_sub <- sweep(matrix(trade_pattern_vec, nrow = length(sector_rows), 
                              ncol = N_sec_agg, byrow = FALSE), 
                       2, IOM_Import[i, ], "*")
        
        FD_sub <- sweep(matrix(trade_pattern_vec, nrow = length(sector_rows),
                               ncol = N_fd_agg, byrow = FALSE),
                        2, FDM_Import[i, ], "*")
        list(sector_rows = sector_rows, Z_sub = Z_sub, FD_sub = FD_sub)
      }
      
      for (i in 1:N_sec_agg) {
        sector_rows <- results[[(i-1)*3+1]]
        Z_trade[sector_rows, m:n] <- results[[(i-1)*3+2]]
        FD_trade[sector_rows, p:q] <- results[[(i-1)*3+3]]
      }
    }
    
    print(str_c("Time cost for ----",regnam[r],"----",round(Sys.time()-t0,2)))
  }
  #---------------
  
  #Constraints: total domestic import and export by sector, and trade flows between counties#---------
  TRADE_EST <- cbind(Z_trade,FD_trade)
  TRADE_EST[TRADE_EST == 0] <- 10^-8
  
  Constraints_Col <- c(Domestic_import_Inter,Domestic_import_Final)
  Constraints_Col [Constraints_Col  < 0] <- 0#Do not understand why import is negative
  Constraints_Row <- Domestic_export
  Constraints_Row[Constraints_Row < 0] <- 0
  
  #The tot sum of col and row should be equal, the difference is casused by the trade flow in the fourth quadrant
  #Adjust the Total Import data to keep it balance. Ask Customer stuff for explanation
  scale <- sum(Constraints_Row)/sum(Constraints_Col)
  Constraints_Col <- Constraints_Col*scale
  
  save(TRADE_EST,Constraints_Col,Constraints_Row,
       file = str_c(pathinc,"/MRIO Trade Flows_estimated_",TaskName,"_",year,".Rdata"))#Since it is time-consuming
  # load(str_c(pathinc,"/MRIO Trade Flows_estimated_",TaskName,"_",year,".Rdata"))
  #---------
  
  
  #RAS balance----------
  i <- 0
  criteria <- max(abs(Constraints_Row - rowSums(TRADE_EST)))
  
  while(criteria > 1) {
    ttt <- Sys.time()
    i <- i + 1 
    print(str_c(i,"---Diffrence before adjustment:---",
                round(criteria,0)))
    
    ratio <- rowSums(TRADE_EST)/Constraints_Row
    ratio[is.na(ratio)] <- 0
    TRADE_EST <- (Diagonal(x = 1/ratio))%*%TRADE_EST
    TRADE_EST@x[is.na(TRADE_EST@x)] <- 10^-8
    
    ratio <- colSums(TRADE_EST)/Constraints_Col
    ratio[is.na(ratio)] <- 0
    TRADE_EST <- TRADE_EST%*%(Diagonal(x = 1/ratio))
    TRADE_EST@x[is.na(TRADE_EST@x)] <- 10^-8
    
    criteria <- max(abs(Constraints_Row - rowSums(TRADE_EST)))
    print(str_c(i,"---Diffrence after adjustment:---",
                round(criteria,0),"--Time Cost--",round(Sys.time()-ttt,2)))
  }
  #-------
  
  
  #Define save the MRIO elements-----
  #May require balance the whole MRIO again
  Z <- TRADE_EST[,1:(dim(Z_trade)[2])]+Z_local
  Z[Z == 0] <- 10^-8
  
  FD <- TRADE_EST[,(dim(Z_trade)[2]+1):(dim(TRADE_EST)[2])]+FD_local
  FD[FD == 0] <- 10^-8
  FD <- FD[,-which(sub(".*::", "", colnames(FD)) %in% c("1","2","3"))]#Readjust FD
  
  ValueAdded <- colSums(VA[1:3,])#Readjust VA
  
  X <- Output
  IM_Global <- Global_import_Inter
  EX_Global <- Global_export
  #--------
  
  
  #Check balance------
  i <- 0
  
  colConstr <- X-ValueAdded-IM_Global
  range(colConstr)
  colConstr[which(colConstr < 0)] <- 10^-8
  rowConstr <- X-EX_Global
  range(rowConstr)
  
  criteria <-   max(c(abs(rowSums(Z)+rowSums(FD)-rowConstr),
                      abs(colSums(Z)-colConstr)))
  
  
  while(criteria > 1) {
    ttt <- Sys.time()
    i <- i + 1 
    print(str_c(i,"---Diffrence before adjustment:---",round(criteria,0)))
    
    INT <- cbind(Z,FD)
    ratio <- rowSums(INT)/rowConstr
    ratio[is.na(ratio)] <- 0
    INT <- (Diagonal(x = 1/ratio))%*%INT
    INT@x[is.na(INT@x)] <- 10^-8
    Z <- INT[,1:length(regsecnam_agg)]
    FD <- INT[,(length(regsecnam_agg)+1):(dim(INT)[2])]
    
    INT <- Z
    ratio <- colSums(INT)/colConstr
    ratio[is.na(ratio)] <- 0
    INT <- INT%*%(Diagonal(x = 1/ratio))
    INT@x[is.na(INT@x)] <- 10^-8
    Z <- INT
    
    criteria <- max(c(abs(rowSums(Z)+rowSums(FD)-rowConstr),
                      abs(colSums(Z)-colConstr)))
    print(str_c(i,"---Diffrence after adjustment:---",
                round(criteria,0),"--Time Cost--",round(Sys.time()-ttt,2)))
  }
  
  #Set small value back to 0
  Z[Z < 10^-5] <- 0
  FD[FD < 10^-5] <- 0
  
  A <- Z%*%(Diagonal(x = 1/X))
  A@x[is.na(A@x)] <- 10^-8
  A@x[is.infinite(A@x)] <- 10^-8
  
  Leontief <- solve(diag(dim(A)[1])-A)
  Leontief@x[is.na(Leontief@x)] <- 0
  A@x[A@x == 10^-8] <- 0
  
  VA <- VA[1:3,]
  VA_LaborCom <- VA[1,]
  VA_Cap <- VA[2,]
  VA_TaxSub <- VA[3,]
  
  #Data encryption sand save as R data--------
  library(base64enc)
  
  protected_data <- list(
    Z = Z,
    FD = FD,
    X = X,
    ValueAdded = ValueAdded,
    VA = VA,
    VA_LaborCom = VA_LaborCom,
    VA_Cap = VA_Cap,
    VA_TaxSub = VA_TaxSub,
    A = A,
    Leontief = Leontief,
    IM_Global = IM_Global,
    EX_Global = EX_Global,
    FIPS = FIPS,
    Insti = Insti,
    Industry_agg = Industry_agg,
    G_agg = G_agg,
    N_fd_agg = N_fd_agg,
    regnam = regnam,
    FactorFD_agg = FactorFD_agg,
    N_sec_agg = N_sec_agg,
    Secnam_agg = Secnam_agg,
    regsecnam_agg = regsecnam_agg,
    regfdnam_agg = regfdnam_agg
  )
  .encrypted_data <- base64encode(serialize(protected_data, NULL))
  
  
  save(.encrypted_data, unlock_data,
       file = str_c(pathinc,"/IMPLAN MRIO ",TaskName,"_",year,"-Ver-",Sys.Date(),".Rdata"))
  
  # save(Z,FD,X,ValueAdded,VA,VA_LaborCom,VA_Cap,VA_TaxSub,
  #      A,Leontief,IM_Global,EX_Global,
  #      FIPS,Insti,Industry_agg,G_agg,N_fd_agg,regnam,FactorFD_agg,
  #      N_sec_agg,Secnam_agg,regsecnam_agg,regfdnam_agg,
  #      file = str_c(pathinc,"/IMPLAN MRIO ",TaskName,"_",year,"-Ver-",Sys.Date(),".Rdata"))
  #------------
  
  #Stop parallel calculation---
  stopCluster(cl)
  
  print(str_c("Time cost for--",year,"   :::   ",round(Sys.time()-tt,2)))
}







