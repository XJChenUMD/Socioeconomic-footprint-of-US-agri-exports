
library(maps)

load(str_c(pathinc,"/Agri Exp impacts_IMPLAN 2022.Rdata"))

nature_colors <- c("#1b9e77", "#d95f02", "#7570b3", "#e7298a",
                   "#66a61e", "#e6ab02", "#a6761d", "#666666",
                   "#1f78b4", "#33a02c", "#fb9a99", "#fdbf6f",
                   "#cab2d6", "#ff7f00", "#6a3d9a", "#b15928",
                   "#8dd3c7", "#ffffb3", "#bebada", "#fb8072")

theme_format <-   theme_minimal(base_size = 12) +
  theme(axis.text.y = element_text(size = 11, color = "black"),
        axis.text.x = element_text(size = 11, color = "black"),
        axis.title.x = element_text(size = 12, color = "black"),
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5, color = "black"),
        plot.subtitle = element_text(size = 11, hjust = 0.5, color = "black"),
        panel.grid.major.y = element_blank(),  
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(color = "gray90"),
        axis.line.x = element_line(color = "black"),
        panel.border = element_blank())


theme_map <-   theme_minimal(base_size = 12) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        strip.text = element_text(size = 11, face = "bold", color = "black"), 
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 11, hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 10),
        legend.position = c(0.93,0.2))

state_labels <- data.frame(lon = state.center$x,
                           lat = state.center$y,
                           abbr = state.abb) %>% 
  filter(!abbr %in% c("AK","HI"))



#Export/Output, national level-----
#Only show sectors bigger than 10%.
Export_rel_tot %>% arrange(desc(value)) %>% 
  mutate(label_wrapped = str_wrap(NewDesc, width = 25),
         label_wrapped = factor(label_wrapped, levels = rev(label_wrapped))) %>% 
  filter(value > 0.1) %>% 
  ggplot() +
  geom_col(aes(x = label_wrapped, y = value*100),fill = nature_colors[1]) +
  scale_y_continuous(breaks = seq(0,100,20),labels = str_c(seq(0,100,20),"%"))+
  coord_flip() +  
  labs(x = "", y = "Export dependence (export/output)", 
       title = "Export dependence by sector",
       subtitle = "Ranked from highest to lowest. Only show sectors > 10%") +
  theme_format 

ggsave(str_c(pathout,"/Export dependence_national.png"),width = 7,height = 7,dpi = 400)

# load(str_c(pathinc,"/US export pattern_UNCOMTRADE 2024.Rdata"))
library(scales)
library(ggrepel)
pie_data <- USexport_Reg %>%
  mutate(pct = Trd / sum(Trd),label = paste0(PartnerName, "\n", percent(pct, 1)))

ggplot(pie_data, aes(x = "", y = Trd, fill = PartnerName)) +
  geom_col(width = 1, color =  "grey") +
  coord_polar(theta = "y") +
  geom_text(aes(label = label),
            position = position_stack(vjust = .5), size = 3) +
  labs(title = "Agriculture exports by partner") + 
  scale_fill_manual(breaks =pie_data$PartnerName, values = nature_colors[2:8])+
  guides(fill = F)+
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(str_c(pathout,"/Export share by partner.png"),width = 4,height = 4,dpi = 400)


#Impact map----------
Agg_Results <- AllResults %>% 
  group_by(State,Secno,NewDesc,Impact,Demension) %>% 
  summarise(value = sum(value),Basevalue = mean(Basevalue)) %>% #agg destination
  group_by(State,Impact,Demension) %>% 
  summarise(value = sum(value),Basevalue = sum(Basevalue)) %>% #agg sectors
  mutate(ratio = value/Basevalue)

us_map <- map_data("state")
state_df <- data.frame(State = state.abb, region = tolower(state.name))
df_map <- Agg_Results %>% left_join(state_df, by = "State")

#GDP impact measured in USD
Data <- us_map %>% left_join(df_map, by = "region") %>% 
  filter(Impact %in% "Total impacts",Demension %in% "GDP")

Data %>% 
  ggplot() +
  geom_polygon(aes(long, lat, group = group, fill = value/10^6),color = "white", linewidth = 0.2) +
  geom_text(data = state_labels, aes(x = lon, y = lat, label = abbr),
            color = "black", size = 3, inherit.aes = FALSE) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1, name = "GDP (M$)",
                       limits = range(Data$value / 1e6), oob = scales::squish)+
  labs(title = "GDP induced by export",
       subtitle = "measured in million USD") +
  theme_map
ggsave(str_c(pathout,"/GDP impact measure in Dollar.png"),width = 8,height = 6,dpi = 400)


#GDP impact measured in ratio
Data %>% 
  ggplot() +
  geom_polygon(aes(long, lat, group = group, fill = ratio*100),color = "white", linewidth = 0.2) +
  geom_text(data = state_labels, aes(x = lon, y = lat, label = abbr),
            color = "black", size = 3, inherit.aes = FALSE) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1, name = "% of\nGDP",
                       limits = range(Data$ratio*100), oob = scales::squish)+
  labs(title = "GDP induced by export",
       subtitle = "measured in share of GDP") +
  theme_map
ggsave(str_c(pathout,"/GDP impact measure in GDP share.png"),width = 8,height = 6,dpi = 400)


#Labor impacts measured headcount
Data <- us_map %>% left_join(df_map, by = "region") %>% 
  filter(Impact %in% "Total impacts",Demension %in% "Employment")

Data %>% 
  ggplot() +
  geom_polygon(aes(long, lat, group = group, fill = value/1000),color = "white", linewidth = 0.2) +
  geom_text(data = state_labels, aes(x = lon, y = lat, label = abbr),
            color = "black", size = 3, inherit.aes = FALSE) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1, name = "Employment\n(thou)",
                       limits = range(Data$value/1000), oob = scales::squish)+
  labs(title = "Employment induced by export",
       subtitle = "measured in 1000 persons") +
  theme_map
ggsave(str_c(pathout,"/Employment impact measure in 1000 persons.png"),width = 8,height = 6,dpi = 400)


#Labor impacts measured in ratio
Data %>% 
  ggplot() +
  geom_polygon(aes(long, lat, group = group, fill = ratio*100),color = "white", linewidth = 0.2) +
  geom_text(data = state_labels, aes(x = lon, y = lat, label = abbr),
            color = "black", size = 3, inherit.aes = FALSE) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1, name = "Employment\n(%)",
                       limits = range(Data$ratio*100), oob = scales::squish)+
  labs(title = "Employment induced by export",
       subtitle = "measured in share of total employment") +
  theme_map
ggsave(str_c(pathout,"/Employment impact measure in share.png"),width = 8,height = 6,dpi = 400)


#Supply chain effect
Data <- us_map %>% left_join(df_map, by = "region") %>% 
  filter(Demension %in% "GDP") %>% 
  select(-c(Basevalue,ratio)) %>% 
  pivot_wider(c(long,lat,group,order,region,subregion,State,Demension),
               names_from = "Impact",values_from = "value") %>% 
  mutate(indirectrate = `Indirect impacts`/`Total impacts`)

Data %>% 
  ggplot() +
  geom_polygon(aes(long, lat, group = group, fill = indirectrate*100),color = "white", linewidth = 0.2) +
  geom_text(data = state_labels, aes(x = lon, y = lat, label = abbr),
            color = "black", size = 3, inherit.aes = FALSE) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1, name = "% of\nimpacts",
                       limits = range(Data$indirectrate*100), oob = scales::squish)+
  labs(title = "The role of supply chain effect",
       subtitle = "Indirect impact/Total impact") +
  theme_map
ggsave(str_c(pathout,"/The role of supply chain effect.png"),width = 8,height = 6,dpi = 400)


#More details in supply chain effect


#Destination-------
Partner_Results <- AllResults %>% 
  group_by(State,Impact,Demension,Partner) %>% 
  summarise(value = sum(value),Basevalue = sum(Basevalue)) %>% #agg sectors
  mutate(ratio = value/Basevalue) %>% 
  filter(Impact %in% "Total impacts",Demension %in% "GDP") %>% 
  filter(!State %in% c("OtherTerritory","AK","HI")) %>% 
  filter(!Partner %in% "USA") 

# Partner_Results$State <- factor(Partner_Results$State, levels = rev(sort(unique(Partner_Results$State))))
Partner_Results$Partner <- factor(Partner_Results$Partner, 
                                  levels = rev(c("Canada","China","European Union",
                                                 "Japan","Korea","Mexico","Rest of World")))

Partner_Results %>% 
  ggplot(aes(y = Partner, x = State)) +
  geom_point(aes(size = ratio*100, color = value / 1e6), alpha = 0.8) +
  scale_color_gradient(low = "#92c5de", high = "#ca0020", name = "GDP impact\n(M$)") +
  scale_size_continuous(range = c(1, 8), name = "% of\nGDP") +
  labs(title = "State-level GDP impact by trade partner",
    subtitle = "Tile color = GDP impact in million USD, Circle size = ratio to state GDP",
    y = NULL, x = NULL) +
  theme_format+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(str_c(pathout,"/GDP impact by partner and state.png"),width = 10,height = 5,dpi = 400)

#Add a total one
Partner_Results %>% group_by(Impact,Demension,Partner) %>% 
  summarise(value = sum(value),Basevalue = sum(Basevalue))%>% ungroup() %>% 
  mutate(ratio = value/Basevalue) %>% 
  arrange(desc(ratio)) %>% 
  mutate(Partner = factor(Partner, levels = rev(Partner))) %>% 
  ggplot() +
  geom_col(aes(x = Partner, y = ratio*100),fill = nature_colors[1]) +
  scale_y_continuous(breaks = seq(0,1,.05),labels = str_c(seq(0,1,.05),"%"))+
  coord_flip() +  
  labs(x = "", y = "GDP share", 
       title = "National GDP impact by trade partner") +
  theme_format 
ggsave(str_c(pathout,"/GDP impact by partner.png"),width = 5,height = 5,dpi = 400)



#Sector impacts----
Sec_Results <- AllResults %>% 
  group_by(State,Secno,NewDesc,Impact,Demension) %>% 
  summarise(value = sum(value),Basevalue = mean(Basevalue)) %>% 
  mutate(ratio = value/Basevalue)

Data <- Sec_Results %>% filter(Demension %in% "GDP", Impact %in% "Total impacts") %>% 
  filter(!State %in% c("OtherTerritory","AK","HI")) 

tar <- Data %>% filter(ratio > 0.1)
tarsec <- unique(tar$NewDesc)

Data %>% 
  filter(ratio > 0.1) %>%
  mutate(label_wrapped = str_wrap(NewDesc, width = 25)) %>% 
  ggplot(aes(y = label_wrapped, x = State)) +
  geom_point(aes(size = ratio*100, color = value / 1e6), alpha = 0.8, shape = 15) +
  scale_color_gradient(low = "#92c5de", high = "#ca0020", name = "GDP impact\n(M$)") +
  scale_size_continuous(range = c(1, 8), name = "% of\n sectoral GDP") +
  labs(title = "Export-induced sectoral GDP impacts across states",
       subtitle = "Only sectors with mpacts >10% are shown; 
       circle size indicates the share of sectoral GDP induced by export;color reflects absolute GDP gains",
       y = NULL, x = NULL) +
  theme_format+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(str_c(pathout,"/GDP impact by sector.png"),width = 12,height = 11,dpi = 400)


Data %>% group_by(Secno,NewDesc,Impact,Demension) %>% 
  summarise(value = sum(value),Basevalue = sum(Basevalue)) %>% ungroup() %>% 
  mutate(ratio = value/Basevalue) %>% arrange(desc(ratio)) %>% 
  mutate(label_wrapped = str_wrap(NewDesc, width = 25),
         label_wrapped = factor(label_wrapped, levels = rev(label_wrapped))) %>% 
  filter(ratio > 0.1) %>% 
  ggplot() +
  geom_col(aes(x = label_wrapped, y = ratio*100),fill = nature_colors[1]) +
  scale_y_continuous(breaks = seq(0,100,20),labels = str_c(seq(0,100,20),"%"))+
  coord_flip() +  
  labs(x = "", y = "GDP share induced by export", 
       title = "GDP dependence on export by sector",
       subtitle = "Only show sectors > 10%") +
  theme_format 
ggsave(str_c(pathout,"/GDP dependence_national.png"),width = 7,height = 11,dpi = 400)


#Marco implication
Agg_Results %>% 
  filter(Impact %in% "Total impacts") %>% 
  group_by(Impact,Demension) %>% 
  summarise(value = sum(value)/10^6,Basevalue = sum(Basevalue)/10^6) %>% 
  mutate(ratio = value/Basevalue)


#Household impact
housnam <- rownames(HOu_ExP_IMPrate)
df <- as.tibble(apply(HOu_ExP_IMPrate,2,as.numeric) )%>%
  mutate(Household = housnam ) %>%
  relocate(Household) 

df_long <- df %>%
  pivot_longer(cols = -Household, names_to = "Region", values_to = "Value") %>%
  mutate(Household = factor(Household, levels = housnam))

ggplot(df_long, aes(x = Region, y = Household, fill = Value*100)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "darkred", name = "%") +
  theme_minimal() +
  labs(title = "Impact on household income by partner",
       x = NULL,
       y = NULL,
       fill = "%")+
  theme_test(base_size = 12) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1), legend.position = "bottom") +
  guides(fill = guide_colorbar(barwidth = 10, barheight = 0.5))
ggsave(str_c(pathout,"/Impact on household income by destination.png"),width = 8,height = 4,dpi = 400)


HouImp_agg <- rowSums(as.tibble(apply(HOu_ExP_IMPrate,2,as.numeric)))
names(HouImp_agg) <- housnam

df <- as.data.frame(HouImp_agg)  
colnames(df) <- "Value"
df$Household <- rownames(df)

df <- df %>% mutate(Household = factor(Household, levels = housnam))
ggplot(df, aes(x = Household, y = Value*100)) +
  geom_col(fill = "steelblue") +
  theme_minimal() +
  labs(title = "Income impact of agricultural exports",
       subtitle = "Measured in share of income",
       x = NULL,
       y = "Change rate of income (%)") +
  coord_flip() +  theme_format
ggsave(str_c(pathout,"/Impact on household income_agg.png"),width = 5,height = 5,dpi = 400)


int <- as.data.frame(rowSums(Hou_Imp_byState))
int$Household <- rownames(int)
colnames(int) <- c("Impacts","Household")

HouNum %>% group_by(Household) %>% summarise(HouNum = sum(Number)) %>% 
  left_join(int, by = "Household") %>% mutate(PerHouImp = Impacts/HouNum) %>% 
  mutate(Household = factor(Household, levels = housnam)) -> df

ggplot(df, aes(x = Household, y = PerHouImp)) +
  geom_col(fill = "steelblue") +
  theme_minimal() +
  labs(title = "Income impact of agricultural exports",
       subtitle = "Measured in income per household",
       x = NULL,
       y = "Income impact per household ($)") +
  coord_flip() +  theme_format
ggsave(str_c(pathout,"/Impact on household income_agg income perhousehold.png"),width = 5,height = 5,dpi = 400)


ggplot(df, aes(x = Household, y = Impacts/10^6)) +
  geom_col(fill = "steelblue") +
  theme_minimal() +
  labs(title = "Income impact of agricultural exports",
       subtitle = "Measured in total income",
       x = NULL,
       y = "Total income impact (million $)") +
  coord_flip() +  theme_format
ggsave(str_c(pathout,"/Impact on household income_agg total income.png"),width = 5,height = 5,dpi = 400)


df <- as_tibble(HOu_ExP_State_IMPrate)  
df$Household <- rownames(HOu_ExP_State_IMPrate)
df_long <- df %>%
  rename_with(~gsub("::1", "", .x)) %>%
  pivot_longer(cols = -Household, names_to = "State", values_to = "Impact") %>% 
  filter(!State %in% "OtherTerritory")

df_long$Household <- factor(df_long$Household, 
                              levels = c("Households LT15k", "Households 15-30k", "Households 30-40k",
                                         "Households 40-50k", "Households 50-70k", "Households 70-100k",
                                         "Households 100-150k", "Households 150-200k", "Households 200k+"))

ggplot(df_long, aes(x = State, y = Household, fill = Impact*100)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "darkred", name = "%") +
  labs(x = NULL, y = NULL,
       title = "Impact on household income groups across states") +
  theme_test(base_size = 12) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "bottom") +
  guides(fill = guide_colorbar(barwidth = 10, barheight = 0.5))
ggsave(str_c(pathout,"/Impact on household income_across states.png"),width = 8,height = 4,dpi = 400)




#Sector revenue structure--------
library(ggtern)

sector_order <- unique(Revenue_str$Secnam)
Revenue_str %>% 
  mutate(Secnam = factor(Secnam, levels = sector_order)) %>% 
  filter(!Secno %in% c(20:22,69:77)) %>% 
  select(Secnam, `Labor compensation`, `Capital revenue`, `Government tax`) %>%
  pivot_longer(cols = -Secnam, names_to = "Component", values_to = "Value") %>% 
  ggplot(aes(x = Secnam, y = Value*100, fill = Component)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("Labor compensation" = "#1b9e77",
                               "Capital revenue" = "#d95f02",
                               "Government tax" = "#7570b3")) +
  labs(title = "Breakdown of value added components",
       x = NULL, y = "Share of value added (%)",
       fill = "Value added component") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = -325, hjust = 1,vjust = 1), legend.position = "bottom")
ggsave(str_c(pathout,"/Sectoral value added compotents breakdown.png"),width = 13,height = 6,dpi = 400)




#State export share and GDP impact share--------
#Export share
Data <- as.data.frame(rowSums(rowsum(Agri_Export,rep(regnam, each = 77)))/sum(rowSums(rowsum(Agri_Export,rep(regnam, each = 77)))))
colnames(Data) <- "Exportshare"
Data$State <- rownames(Data)

#GDP impact share
AllResults %>% filter(Demension %in% "GDP",Impact %in% "Total impacts") %>% 
  group_by(State) %>% summarise(GDPimp = sum(value)) %>% ungroup() %>% 
  mutate(GDPshare = GDPimp/sum(GDPimp)) %>% 
  left_join(Data) -> df

ggplot(df, aes(x = Exportshare*100, y = GDPshare*100)) +
  geom_point(color = "steelblue", size = 5, alpha = 0.7) +
  geom_text_repel(aes(label = State), hjust = -0.1, vjust = 0.5, size = 3) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") + 
  scale_x_continuous(limits = c(1,13),breaks = seq(1,14,2),labels = str_c(seq(1,14,2),"%"))+
  scale_y_continuous(limits = c(1,13),breaks = seq(1,14,2),labels = str_c(seq(1,14,2),"%"))+
  labs(title = "Export share vs GDP impact share",
       x = "Export share", y = "GDP impact share") +
  theme_test()
ggsave(str_c(pathout,"/Export share vs GDP impact share.png"),width = 7,height = 7,dpi = 400)


#State labor, cap and tax impacts
Agg_Results %>% filter(Demension %in% c("Capital revenue","Labor compensation","Government revenue"),
                      Impact %in% "Total impacts",
                      !State %in% "OtherTerritory")  -> Data

Data$Demension <- factor(Data$Demension, levels = c("Labor compensation", "Capital revenue", "Government revenue"))

ggplot(Data, aes(x = State, y = ratio*100, color = Demension)) +
  geom_point(size = 5, alpha = 0.8) +
  scale_y_continuous(breaks = seq(-8,8,2),labels = str_c(seq(-8,8,2),"%"))+
  labs(title = "Impact of export on value added components",
       x = NULL, y = "Impact share", color = "GDP component") +
  theme_bw(base_size = 12) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "bottom")
ggsave(str_c(pathout,"/Impact of export on value added components.png"),width = 10,height = 5,dpi = 400)


#Additional basic facts
#Export amount and export sturture by state  (Bar chart and heat map)
Export <- as.data.frame(rowSums(Agri_Export))
Export$StateSec <- rownames(Export)
colnames(Export) <- c("Export","StateSec")

Export <- Export %>%
  separate(StateSec, into = c("State", "Sector"), sep = "::", remove = FALSE)

Export %>% group_by(State) %>% summarise(TotExport = sum(Export)) %>% 
  filter(!State %in% "OtherTerritory") -> StateTot

StateTot %>% 
  ggplot(aes(x = reorder(State, TotExport/10^9), y = TotExport/10^9)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Agricultural export by state",
    x = NULL, y = "Agriculture export value (billion $)") + 
  coord_flip()+
  theme_format+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
ggsave(str_c(pathout,"/Agricultural export by state.png"),width = 5,height = 10,dpi = 400)

Export %>% mutate(Sector = as.integer(Sector)) %>% 
  left_join(Industry_agg, by = c("Sector" = "NewCode")) %>% 
  filter(!State %in% "OtherTerritory")  %>%  left_join(StateTot, by = "State") %>%
  mutate(Share = Export / TotExport) %>% 
  select(State,Sector,NewDesc, Share) -> ExportStru


ExportStru %>% filter(Share > 0.1) %>% 
  mutate(label_wrapped = str_wrap(NewDesc, width = 25)) %>% 
  ggplot(aes(x = State, y = label_wrapped, fill = Share*100)) +
  geom_tile() +
  scale_fill_gradient(low = "lightyellow", high = "red", name = "Share (%)") +
  labs(title = "Sectoral structure of agriculture export",
       subtitle = "Only show sectors with >10% export share",
       x = NULL,y = NULL) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(str_c(pathout,"/Agricultural export by sector.png"),width = 12,height = 10,dpi = 400)


#Agricultural value added to GDP  (MAP)
#Agricultural employment to GDP (MAP)
names(EMP_data) <- names(ValueAdded)

Agri_GDP <- as.data.frame(rowMeans(Agri_GDP))
Agri_GDP$StateSec <- rownames(Agri_GDP)
colnames(Agri_GDP) <- c("Agri_GDP","StateSec")
Agri_GDP <- Agri_GDP %>%
  separate(StateSec, into = c("State", "Sector"), sep = "::", remove = FALSE) %>% 
  group_by(State) %>% summarise(Agri_GDP = sum(Agri_GDP))

ValueAdded <- as.data.frame(ValueAdded)
ValueAdded$StateSec <- rownames(ValueAdded)
colnames(ValueAdded) <- c("ValueAdded","StateSec")
ValueAdded <- ValueAdded %>%
  separate(StateSec, into = c("State", "Sector"), sep = "::", remove = FALSE) %>% 
  group_by(State) %>% summarise(ValueAdded = sum(ValueAdded))

Agri_EMP <- as.data.frame(rowMeans(Agri_EMP))
Agri_EMP$StateSec <- rownames(Agri_EMP)
colnames(Agri_EMP) <- c("Agri_EMP","StateSec")
Agri_EMP <- Agri_EMP %>%
  separate(StateSec, into = c("State", "Sector"), sep = "::", remove = FALSE) %>% 
  group_by(State) %>% summarise(Agri_EMP = sum(Agri_EMP))

EMP_data <- as.data.frame(EMP_data)
EMP_data$StateSec <- rownames(EMP_data)
colnames(EMP_data) <- c("EMP","StateSec")
EMP_data <- EMP_data %>%
  separate(StateSec, into = c("State", "Sector"), sep = "::", remove = FALSE) %>% 
  group_by(State) %>% summarise(EMP_data = sum(EMP))


Agri_GDP %>% left_join(Agri_EMP, by = "State") %>% left_join(EMP_data, by = "State") %>% 
  left_join(ValueAdded, by = "State") %>% 
  mutate(GDPshare = Agri_GDP/ValueAdded,
         EMPshare = Agri_EMP/EMP_data) %>% left_join(state_df, by = "State") %>% 
  filter(!State %in% "OtherTerritory") -> df_map

Data <- us_map %>% left_join(df_map, by = "region") 

Data %>% 
  ggplot() +
  geom_polygon(aes(long, lat, group = group, fill = GDPshare*100),color = "white", linewidth = 0.2) +
  geom_text(data = state_labels, aes(x = lon, y = lat, label = abbr),
            color = "black", size = 3, inherit.aes = FALSE) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1, name = "GDP (%)",
                       limits = range(Data$GDPshare), oob = scales::squish)+
  labs(title = "GDP contributed by agricutural production") +
  theme_map
ggsave(str_c(pathout,"/GDP contributed by agricutural production.png"),width = 8,height = 6,dpi = 400)


Data %>% 
  ggplot() +
  geom_polygon(aes(long, lat, group = group, fill = EMPshare*100),color = "white", linewidth = 0.2) +
  geom_text(data = state_labels, aes(x = lon, y = lat, label = abbr),
            color = "black", size = 3, inherit.aes = FALSE) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1, name = "Employment\n(%)",
                       limits = range(Data$EMPshare), oob = scales::squish)+
  labs(title = "Employment contributed by agricutural production") +
  theme_map
ggsave(str_c(pathout,"/EMP contributed by agricutural production.png"),width = 8,height = 6,dpi = 400)



#Trade partner and sectors (heat map)
AllResults %>% filter(Impact %in% "Total impacts", Demension %in% "GDP") %>% 
  group_by(NewDesc,Partner) %>% 
  summarise(value = sum(value)) -> Data

sector_total <- Data %>%
  group_by(NewDesc) %>%
  summarise(total = sum(value, na.rm = TRUE))

trade_share <- Data %>%
  left_join(sector_total, by = "NewDesc") %>%
  mutate(share = value / total) %>% 
  mutate(NewDesc_wrapped = str_wrap(NewDesc, width = 40)) %>% 
  filter(!NewDesc %in% "Others (Not an industry)")


trade_share %>% mutate(NewDesc_wrapped = factor(NewDesc_wrapped,
                                                   levels = rev(str_wrap(sector_order, width = 40)))) %>% 
  ggplot(aes(x = Partner, y = NewDesc_wrapped, fill = share)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "darkblue", labels = scales::percent_format(), name = "Share") +
  labs(title = "Partner-specific contributions to export-driven GDP by sector", x = NULL, y = NULL) +
  theme_format+
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
    axis.text.y = element_text(size = 8))
ggsave(str_c(pathout,"/Partner share in each sector.png"),width = 8,height = 12,dpi = 400)
