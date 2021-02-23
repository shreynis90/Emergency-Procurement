library(cem)
library(readxl)
library(dplyr)
library(ggplot2)
library(gtsummary)
library(openxlsx)
memory.limit(size = 30000)
gc()
italy_match<- read_excel("data_09_20.xlsx")
italy_match<-as.data.frame(italy_match)

##Disaster1 Analysis##----

#Removing contracts from other disaster areas#
italy_disaster1<- italy_match %>%
                  filter(disnumber == "Disaster_001"|is.na(disnumber))
italy_disaster1$treatment_Disaster1 <- ifelse(is.na(italy_disaster1$disnumber)|(italy_disaster1$disnumber != "Disaster_001"),0,1)
##


##Converting to dataframe and Retaining first three digits of the CPV code
italy_disaster1 <- data.frame(italy_disaster1)
italy_disaster1$buyer_buyerType <- as.factor(italy_disaster1$buyer_buyerType) #Declaring buyer_buyerType as a factor
italy_disaster1$tender_mainCpv <- as.integer(italy_disaster1$tender_mainCpv)
italy_disaster1$tender_mainCpv <- sub("^(\\d{3}).*$", "\\1", italy_disaster1$tender_mainCpv) #Retaining only the first three digits of the the CPV code
italy_disaster1$tender_mainCpv <- as.integer(italy_disaster1$tender_mainCpv)
italy_disaster1$tender_mainCpv

##Fixing the dates
italy_disaster1<- italy_disaster1%>% #Dropping contracts which do not have call or award date
  filter(!is.na(tender_publications_firstCallForTenderDate)& !is.na(tender_publications_firstdContractAwardDate)) 
italy_disaster1$contractyear<- ifelse(is.na(italy_disaster1$tender_publications_firstCallForTenderDate), substring(italy_disaster1$tender_publications_firstdContractAwardDate,1,4),substring(italy_disaster1$tender_publications_firstCallForTenderDate,1,4)) #Contract year
italy_disaster1$contractmonth <- ifelse(is.na(italy_disaster1$tender_publications_firstCallForTenderDate), substring(italy_disaster1$tender_publications_firstdContractAwardDate,6,7),substring(italy_disaster1$tender_publications_firstCallForTenderDate,6,7)) #Contract month
italy_disaster1<- italy_disaster1 %>% filter(contractyear>=2007 & contractyear<=2020) #Range of the Data (Full data goes from 2006 to 2020)

##Pretreatment average of the dependent variable
italy_disaster1 <- italy_disaster1 %>% #Dropping the Contracts that are missing the number of bidders 
  filter(!is.na(lot_bidsCount))
italy_disaster1$lot_bidsCount <- ifelse(italy_disaster1$lot_bidsCount > 20, 20, italy_disaster1$lot_bidsCount)
pretreat <-  italy_disaster1%>% filter(contractyear <= 2009 & contractmonth < 10)
pretreat <- pretreat %>% mutate(meanlotbids = mean(lot_bidsCount)) #pretreament average of the number of bidders
italy_disaster1$contractyear <- as.factor(italy_disaster1$contractyear)
italy_disaster1$meanlotbids <- pretreat[1,22]
  
#Matching
vars <- c("treatment_Disaster1","buyer_buyerType", "tender_mainCpv", "lot_bidsCount", "tender_finalPrice_EUR", "meanlotbids", "contractyear")
temp<-italy_disaster1[vars]
vars2<- c("buyer_buyerType", "tender_mainCpv", "tender_finalPrice_EUR", "meanlotbids","contractyear")
temp<- as.data.frame(temp)
imbalance(group=temp$treatment_Disaster1, data=temp[vars2])
summary(temp$tender_finalPrice_EUR)
valuecuts = c(5.620e+05, 1.573e+06, 5.218e+06)
buyer_buyerType.grp<- list(c("REGIONAL_AUTHORITY", "REGIONAL_AGENCY", "UTILITIES"), c("NATIONAL_AUTHORITY"),c("OTHER"), c("PUBLIC_BODY"), c("NA",NA))
mat <- cem(treatment = "treatment_Disaster1", data = temp, drop = "lot_bidsCount", cutpoints = list(tender_finalPrice_EUR = valuecuts), grouping = list(buyer_buyerType= buyer_buyerType.grp))
mat
italy_disaster1$treatcon_d1<-ifelse((italy_disaster1$treatment_Disaster1==1)& (mat$matched == TRUE),1, ifelse(mat$matched==TRUE,0,1000))
italy_disaster1_1<- italy_disaster1%>%
  filter(!is.na(tender_publications_firstCallForTenderDate)| !is.na(tender_publications_firstdContractAwardDate))
disaster1_date <- as.POSIXct("2009-10-01")
italy_disaster1_1$time_d1 <- ifelse(is.na(italy_disaster1_1$tender_publications_firstCallForTenderDate),ifelse(italy_disaster1_1$tender_publications_firstdContractAwardDate < disaster1_date,0,1),ifelse(italy_disaster1_1$tender_publications_firstCallForTenderDate < disaster1_date,0,1))
italy_disaster1_1 <- italy_disaster1_1 %>%
  filter(treatcon_d1 != 1000)
italy_disaster1_1$did <- italy_disaster1_1$treatcon_d1*italy_disaster1_1$time_d1
didreg = lm(lot_bidsCount ~ did + time_d1 + treatcon_d1 + contractyear + contractmonth + tender_finalPrice_EUR + tender_mainCpv, data = italy_disaster1_1)
didreg
summary(didreg)
g<-aggregate(italy_disaster1_1[, 14], list(italy_disaster1_1$treatcon_d1, italy_disaster1_1$contractyear, italy_disaster1_1$contractmonth), mean)
g2 <- cbind(g[,1], paste(g[,2],g[,3]), round(g[,4],0))
g2<- as.data.frame(g2)
ggplot(g2, aes(x=g2[,2], y=g2[,3], color = factor(g2[,1]), group = g2[,1])) +geom_line() + geom_point() + xlab('Year') + ylab('Average Number of Bidders') + labs(subtitle="Blue line depicts the Disaster timing",title="Disaster 1 in 2009-10-01", color = "Treated") +geom_vline(xintercept = 4.9, color = "blue", size=0.5) 

write.xlsx(italy_disaster1, "temp1.xlsx")


 #Disaster2 Analysis ---- 

#Removing contracts from other disaster areas#
italy_disaster2<- italy_match %>%
  filter(disnumber == "Disaster_002"|is.na(disnumber))
italy_disaster2$treatment_Disaster2 <- ifelse(is.na(italy_disaster2$disnumber)|(italy_disaster2$disnumber != "Disaster_002"),0,1)
##

##Converting to dataframe and Retaining first three digits of the CPV code
italy_disaster2$buyer_buyerType <- as.factor(italy_disaster2$buyer_buyerType)
italy_disaster2 <- data.frame(italy_disaster2)
italy_disaster2$buyer_buyerType <- as.factor(italy_disaster2$buyer_buyerType)
italy_disaster2$tender_mainCpv <- as.integer(italy_disaster2$tender_mainCpv)
italy_disaster2$tender_mainCpv <- sub("^(\\d{3}).*$", "\\1", italy_disaster2$tender_mainCpv)
italy_disaster2$tender_mainCpv <- as.integer(italy_disaster2$tender_mainCpv)
italy_disaster2$tender_mainCpv

##Fixing the dates
italy_disaster2<- italy_disaster2%>% #Dropping contracts which do not have call or award date
  filter(!is.na(tender_publications_firstCallForTenderDate)& !is.na(tender_publications_firstdContractAwardDate)) 
italy_disaster2$contractyear<- ifelse(is.na(italy_disaster2$tender_publications_firstCallForTenderDate), substring(italy_disaster2$tender_publications_firstdContractAwardDate,1,4),substring(italy_disaster2$tender_publications_firstCallForTenderDate,1,4)) #Contract year
italy_disaster2$contractmonth <- ifelse(is.na(italy_disaster2$tender_publications_firstCallForTenderDate), substring(italy_disaster2$tender_publications_firstdContractAwardDate,6,7),substring(italy_disaster2$tender_publications_firstCallForTenderDate,6,7)) #Contract month
italy_disaster2<- italy_disaster2 %>% filter(contractyear>=2007 & contractyear<=2020) #Range of the Data (Full data goes from 2006 to 2020)

##Pretreatment average of the dependent variable
italy_disaster2 <- italy_disaster2 %>% #Dropping the Contracts that are missing the number of bidders.
  filter(!is.na(lot_bidsCount))
italy_disaster2$lot_bidsCount <- ifelse(italy_disaster2$lot_bidsCount > 20, 20, italy_disaster2$lot_bidsCount)
pretreat2 <-  italy_disaster2%>% filter(contractyear <= 2012 & contractmonth < 05)
pretreat2 <- pretreat %>% mutate(meanlotbids = mean(lot_bidsCount)) #pretreament average of the number of bidders
italy_disaster2$contractyear <- as.factor(italy_disaster2$contractyear)
italy_disaster2$meanlotbids <- pretreat2[1,22]


#Matching
vars_2 <- c("treatment_Disaster2","buyer_buyerType", "tender_mainCpv", "lot_bidsCount", "tender_finalPrice_EUR", "meanlotbids")
temp2<-italy_disaster2[vars_2]
vars2<- c("buyer_buyerType", "tender_mainCpv", "tender_finalPrice_EUR", "meanlotbids")
temp2<- as.data.frame(temp2)
imbalance(group=temp2$treatment_Disaster2, data=temp2[vars2])
summary(temp2$tender_finalPrice_EUR)
valuecuts2 = c(5.348e+05, 1.543e+06, 5.666e+06)
buyer_buyerType.grp<- list(c("REGIONAL_AUTHORITY", "REGIONAL_AGENCY", "UTILITIES"), c("NATIONAL_AUTHORITY"),c("OTHER"), c("PUBLIC_BODY"), c("NA",NA))
mat2 <- cem(treatment = "treatment_Disaster2", data = temp2, drop = "lot_bidsCount", cutpoints = list(tender_finalPrice_EUR = valuecuts2), grouping = list(buyer_buyerType= buyer_buyerType.grp))
mat2
italy_disaster2$treatcon_d2<-ifelse((italy_disaster2$treatment_Disaster2==1)& (mat2$matched == TRUE),1, ifelse(mat2$matched==TRUE,0,1000))
italy_disaster2_1<- italy_disaster2%>%
  filter(!is.na(tender_publications_firstCallForTenderDate)| !is.na(tender_publications_firstdContractAwardDate))
disaster2_date <- as.POSIXct("2012-05-29")
italy_disaster2_1$time_d2 <- ifelse(is.na(italy_disaster2_1$tender_publications_firstCallForTenderDate),ifelse(italy_disaster2_1$tender_publications_firstdContractAwardDate < disaster2_date,0,1),ifelse(italy_disaster2_1$tender_publications_firstCallForTenderDate < disaster2_date,0,1))
italy_disaster2_1 <- italy_disaster2_1 %>%
  filter(treatcon_d2 != 1000)
italy_disaster2_1$did <- italy_disaster2_1$treatcon_d2*italy_disaster2_1$time_d2
didreg2 = lm(lot_bidsCount ~ did + time_d2 + treatcon_d2, data = italy_disaster2_1)
didreg2
summary(didreg2)
g<-aggregate(italy_disaster2_1[, 14], list(italy_disaster2_1$treatcon_d2, italy_disaster2_1$contractyear, italy_disaster2_1$contractmonth), mean)
g2 <- cbind(g[,1], paste(g[,2],g[,3]), round(g[,4],0))
g2<- as.data.frame(g2)
ggplot(g2, aes(x=g2[,2], y=g2[,3], color = factor(g2[,1]), group = g2[,1])) +geom_line() + geom_point() + xlab('Year') + ylab('Average Number of Bidders') + labs(subtitle="Blue line depicts the Disaster timing",title="Disaster 1 in 2009-10-01", color = "Treated") +geom_vline(xintercept = 11.9, color = "blue", size=0.5) 


#Disaster3 Analysis ----
italy_disaster3<- italy_match %>%
  filter(disnumber == "Disaster_003"|is.na(disnumber))
italy_disaster3$treatment_Disaster3 <- ifelse(is.na(italy_disaster3$disnumber)|(italy_disaster3$disnumber != "Disaster_003"),0,1)
italy_disaster3$buyer_buyerType <- as.factor(italy_disaster3$buyer_buyerType)
italy_disaster3 <- data.frame(italy_disaster3)
italy_disaster3$buyer_buyerType <- as.factor(italy_disaster3$buyer_buyerType)
italy_disaster3$tender_mainCpv <- as.integer(italy_disaster3$tender_mainCpv)
italy_disaster3$tender_mainCpv <- sub("^(\\d{3}).*$", "\\1", italy_disaster3$tender_mainCpv)
italy_disaster3$tender_mainCpv <- as.integer(italy_disaster3$tender_mainCpv)
italy_disaster3$tender_mainCpv
italy_disaster3 <- italy_disaster3 %>%
  filter(!is.na(lot_bidsCount)& !is.na(tender_mainCpv))
italy_disaster3$lot_bidsCount <- ifelse(italy_disaster3$lot_bidsCount > 20, 20, italy_disaster3$lot_bidsCount)
italy_disaster3<- italy_disaster3 %>% group_by(tender_mainCpv) %>% mutate(meanlotbids = mean(lot_bidsCount))
vars_3 <- c("treatment_Disaster3","buyer_buyerType", "tender_mainCpv", "lot_bidsCount", "tender_finalPrice_EUR", "meanlotbids")
temp3<-italy_disaster3[vars_3]
vars2<- c("buyer_buyerType", "tender_mainCpv", "tender_finalPrice_EUR", "meanlotbids")
temp3<- as.data.frame(temp3)
imbalance(group=temp3$treatment_Disaster3, data=temp3[vars2])
summary(temp3$tender_finalPrice_EUR)
valuecuts3 = c(5.348e+05, 1.545e+06, 5.617e+06)
buyer_buyerType.grp<- list(c("REGIONAL_AUTHORITY", "REGIONAL_AGENCY", "UTILITIES"), c("NATIONAL_AUTHORITY"),c("OTHER"), c("PUBLIC_BODY"), c("NA",NA))
mat3 <- cem(treatment = "treatment_Disaster3", data = temp3, drop = "lot_bidsCount", cutpoints = list(tender_finalPrice_EUR = valuecuts3), grouping = list(buyer_buyerType= buyer_buyerType.grp))
mat3
italy_disaster3$treatcon_d3<-ifelse((italy_disaster3$treatment_Disaster3==1)& (mat3$matched == TRUE),1, ifelse(mat3$matched==TRUE,0,1000))
italy_disaster3_1<- italy_disaster3%>%
  filter(!is.na(tender_publications_firstCallForTenderDate)| !is.na(tender_publications_firstdContractAwardDate))
disaster3_date <- as.POSIXct("2013-11-18")
italy_disaster3_1$time_d3 <- ifelse(is.na(italy_disaster3_1$tender_publications_firstCallForTenderDate),ifelse(italy_disaster3_1$tender_publications_firstdContractAwardDate < disaster3_date,0,1),ifelse(italy_disaster3_1$tender_publications_firstCallForTenderDate < disaster3_date,0,1))
italy_disaster3_1 <- italy_disaster3_1 %>%
  filter(treatcon_d3 != 1000)
italy_disaster3_1$did <- italy_disaster3_1$treatcon_d3*italy_disaster3_1$time_d3
italy_disaster3_1$lot_bidsCount <- ifelse(italy_disaster3_1$lot_bidsCount > 20, 20, italy_disaster3_1$lot_bidsCount)
didreg3 = lm(lot_bidsCount ~ did + time_d3 + treatcon_d3, data = italy_disaster3_1)
didreg3
summary(didreg3)
italy_disaster3_1<- italy_disaster3_1 %>%
  filter(!is.na(lot_bidsCount))
years<- ifelse(is.na(italy_disaster3_1$tender_publications_firstCallForTenderDate), substring(italy_disaster3_1$tender_publications_firstdContractAwardDate,1,4),substring(italy_disaster3_1$tender_publications_firstCallForTenderDate,1,4))   
italy_disaster3_1$contractyear<- years
g<-aggregate(italy_disaster3_1[, 14], list(italy_disaster3_1$treatcon_d3, italy_disaster3_1$contractyear), mean)
ggplot(g, aes(x=g[,2], y=g[,3], color = factor(g[,1]), group = g[,1])) +
  geom_line() +
  geom_point()+ xlab('Year') + ylab('Average Number of Bidders')+ labs(color = "Treated")+ labs(subtitle="Blue line depicts the Disaster timing",title="Disaster 3 in 2013-11-18", color = "Treated") +geom_vline(xintercept = 8.8, color = "blue", size=0.5) 

#Disaster4 Analysis ----
italy_disaster4<- italy_match %>%
  filter(disnumber == "Disaster_004"|is.na(disnumber))
italy_disaster4$treatment_Disaster4 <- ifelse(is.na(italy_disaster4$disnumber)|(italy_disaster4$disnumber != "Disaster_004"),0,1)
italy_disaster4$buyer_buyerType <- as.factor(italy_disaster4$buyer_buyerType)
italy_disaster4 <- data.frame(italy_disaster4)
italy_disaster4$buyer_buyerType <- as.factor(italy_disaster4$buyer_buyerType)
italy_disaster4$tender_mainCpv <- as.integer(italy_disaster4$tender_mainCpv)
italy_disaster4$tender_mainCpv <- sub("^(\\d{3}).*$", "\\1", italy_disaster4$tender_mainCpv)
italy_disaster4$tender_mainCpv <- as.integer(italy_disaster4$tender_mainCpv)
italy_disaster4$tender_mainCpv
italy_disaster4 <- italy_disaster4 %>%
  filter(!is.na(lot_bidsCount)& !is.na(tender_mainCpv))
italy_disaster4$lot_bidsCount <- ifelse(italy_disaster4$lot_bidsCount > 20, 20, italy_disaster4$lot_bidsCount)
italy_disaster4<- italy_disaster4 %>% group_by(tender_mainCpv) %>% mutate(meanlotbids = mean(lot_bidsCount))
vars_4 <- c("treatment_Disaster4","buyer_buyerType", "tender_mainCpv", "lot_bidsCount", "tender_finalPrice_EUR", "meanlotbids")
temp4<-italy_disaster4[vars_4]
vars2<- c("buyer_buyerType", "tender_mainCpv", "tender_finalPrice_EUR", "meanlotbids")
temp4<- as.data.frame(temp4)
imbalance(group=temp4$treatment_Disaster4, data=temp4[vars2])
summary(temp4$tender_finalPrice_EUR)
valuecuts4 = c(5.348e+05, 1.543e+06, 5.633e+06)
buyer_buyerType.grp<- list(c("REGIONAL_AUTHORITY", "REGIONAL_AGENCY", "UTILITIES"), c("NATIONAL_AUTHORITY"),c("OTHER"), c("PUBLIC_BODY"), c("NA",NA))
mat4 <- cem(treatment = "treatment_Disaster4", data = temp4, drop = "lot_bidsCount", cutpoints = list(tender_finalPrice_EUR = valuecuts4), grouping = list(buyer_buyerType= buyer_buyerType.grp))
mat4
italy_disaster4$treatcon_d4<-ifelse((italy_disaster4$treatment_Disaster4==1)& (mat4$matched == TRUE),1, ifelse(mat4$matched==TRUE,0,1000))
italy_disaster4_1<- italy_disaster4%>%
  filter(!is.na(tender_publications_firstCallForTenderDate)| !is.na(tender_publications_firstdContractAwardDate))
disaster4_date <- as.POSIXct("2016-08-24")
italy_disaster4_1$time_d4 <- ifelse(is.na(italy_disaster4_1$tender_publications_firstCallForTenderDate),ifelse(italy_disaster4_1$tender_publications_firstdContractAwardDate < disaster4_date,0,1),ifelse(italy_disaster4_1$tender_publications_firstCallForTenderDate < disaster4_date,0,1))
italy_disaster4_1 <- italy_disaster4_1 %>%
  filter(treatcon_d4 != 1000)
italy_disaster4_1$did <- italy_disaster4_1$treatcon_d4*italy_disaster4_1$time_d4
italy_disaster4_1$lot_bidsCount <- ifelse(italy_disaster4_1$lot_bidsCount > 20, 20, italy_disaster4_1$lot_bidsCount)
didreg4 = lm(lot_bidsCount ~ did + time_d4 + treatcon_d4, data = italy_disaster4_1)
didreg4
summary(didreg4)
italy_disaster4_1<- italy_disaster4_1 %>%
  filter(!is.na(lot_bidsCount))
years<- ifelse(is.na(italy_disaster4_1$tender_publications_firstCallForTenderDate), substring(italy_disaster4_1$tender_publications_firstdContractAwardDate,1,4),substring(italy_disaster4_1$tender_publications_firstCallForTenderDate,1,4))   
italy_disaster4_1$contractyear<- years
g<-aggregate(italy_disaster4_1[, 14], list(italy_disaster4_1$treatcon_d4, italy_disaster4_1$contractyear), mean)
ggplot(g, aes(x=g[,2], y=g[,3], color = factor(g[,1]), group = g[,1])) +
  geom_line() +
  geom_point()+ xlab('Year') + ylab('Average Number of Bidders')+ labs(color = "Treated") + labs(subtitle="Blue line depicts the Disaster timing",title="Disaster 4 in 2016-08-24", color = "Treated") +geom_vline(xintercept = 11.8, color = "blue", size=0.5)


#Disaster 5 Analysis ----
italy_disaster5<- italy_match %>%
  filter(disnumber == "Disaster_005"|is.na(disnumber))
italy_disaster5$treatment_Disaster5 <- ifelse(is.na(italy_disaster5$disnumber)|(italy_disaster5$disnumber != "Disaster_005"),0,1)
italy_disaster5$buyer_buyerType <- as.factor(italy_disaster5$buyer_buyerType)
italy_disaster5 <- data.frame(italy_disaster5)
italy_disaster5$buyer_buyerType <- as.factor(italy_disaster5$buyer_buyerType)
italy_disaster5$tender_mainCpv <- as.integer(italy_disaster5$tender_mainCpv)
italy_disaster5$tender_mainCpv <- sub("^(\\d{3}).*$", "\\1", italy_disaster5$tender_mainCpv)
italy_disaster5$tender_mainCpv <- as.integer(italy_disaster5$tender_mainCpv)
italy_disaster5$tender_mainCpv
italy_disaster5 <- italy_disaster5 %>%
  filter(!is.na(lot_bidsCount)& !is.na(tender_mainCpv))
italy_disaster5$lot_bidsCount <- ifelse(italy_disaster5$lot_bidsCount > 20, 20, italy_disaster5$lot_bidsCount)
italy_disaster5<- italy_disaster5 %>% group_by(tender_mainCpv) %>% mutate(meanlotbids = mean(lot_bidsCount))
vars_5 <- c("treatment_Disaster5","buyer_buyerType", "tender_mainCpv", "lot_bidsCount", "tender_finalPrice_EUR", "meanlotbids")
temp5<-italy_disaster5[vars_5]
vars2<- c("buyer_buyerType", "tender_mainCpv", "tender_finalPrice_EUR", "meanlotbids")
temp5<- as.data.frame(temp5)
imbalance(group=temp5$treatment_Disaster5, data=temp5[vars2])
summary(temp5$tender_finalPrice_EUR)
valuecuts5 = c(5.360e+05, 1.543e+06, 5.654e+06)
buyer_buyerType.grp<- list(c("REGIONAL_AUTHORITY", "REGIONAL_AGENCY", "UTILITIES"), c("NATIONAL_AUTHORITY"),c("OTHER"), c("PUBLIC_BODY"), c("NA",NA))
mat5 <- cem(treatment = "treatment_Disaster5", data = temp5, drop = "lot_bidsCount", cutpoints = list(tender_finalPrice_EUR = valuecuts5), grouping = list(buyer_buyerType= buyer_buyerType.grp))
mat5
italy_disaster5$treatcon_d5<-ifelse((italy_disaster5$treatment_Disaster5==1)& (mat5$matched == TRUE),1, ifelse(mat5$matched==TRUE,0,1000))
italy_disaster5_1<- italy_disaster5%>%
  filter(!is.na(tender_publications_firstCallForTenderDate)| !is.na(tender_publications_firstdContractAwardDate))
disaster5_date <- as.POSIXct("2017-01-18")
italy_disaster5_1$time_d5 <- ifelse(is.na(italy_disaster5_1$tender_publications_firstCallForTenderDate),ifelse(italy_disaster5_1$tender_publications_firstdContractAwardDate < disaster5_date,0,1),ifelse(italy_disaster5_1$tender_publications_firstCallForTenderDate < disaster5_date,0,1))
italy_disaster5_1 <- italy_disaster5_1 %>%
  filter(treatcon_d5 != 1000)
italy_disaster5_1$did <- italy_disaster5_1$treatcon_d5*italy_disaster5_1$time_d5
italy_disaster5_1$lot_bidsCount <- ifelse(italy_disaster5_1$lot_bidsCount > 20, 20, italy_disaster5_1$lot_bidsCount)
didreg5 = lm(lot_bidsCount ~ did + time_d5 + treatcon_d5, data = italy_disaster5_1)
didreg5
summary(didreg5)
italy_disaster5_1<- italy_disaster5_1 %>%
  filter(!is.na(lot_bidsCount))
years<- ifelse(is.na(italy_disaster5_1$tender_publications_firstCallForTenderDate), substring(italy_disaster5_1$tender_publications_firstdContractAwardDate,1,4),substring(italy_disaster5_1$tender_publications_firstCallForTenderDate,1,4))   
italy_disaster5_1$contractyear<- years
g<-aggregate((italy_disaster5_1[, 14]), list(italy_disaster5_1$treatcon_d5, italy_disaster5_1$contractyear), mean)
ggplot(g, aes(x=g[,2], y=g[,3], color = factor(g[,1]), group = g[,1])) +
  geom_line() +
  geom_point() + xlab('Year') + ylab('Average Number of Bidders')+ labs(subtitle="Blue line depicts the Disaster timing",title="Disaster 5 on 2017-01-18", color = "Treated") +geom_vline(xintercept = 12.02, color = "blue", size=0.5)

##Visualization ----
tbl_regression(didreg)
