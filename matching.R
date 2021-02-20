library(cem)
library(readxl)
library(dplyr)
library(ggplot2)

italy_match<- read_excel("data_09_20.xlsx")
f<- which(italy_match$treatcon==1)
length(f)
italy_match<-as.data.frame(italy_match)

##Disaster1 Analysis##----

italy_disaster1<- italy_match %>%
                  filter(disnumber == "Disaster_001"|is.na(disnumber))
italy_disaster1$treatment_Disaster1 <- ifelse(is.na(italy_disaster1$disnumber)|(italy_disaster1$disnumber != "Disaster_001"),0,1)
italy_disaster1$buyer_buyerType <- as.factor(italy_disaster1$buyer_buyerType)
italy_disaster1 <- data.frame(italy_disaster1)
italy_disaster1$buyer_buyerType <- as.factor(italy_disaster1$buyer_buyerType)
italy_disaster1$tender_mainCpv <- as.integer(italy_disaster1$tender_mainCpv)
italy_disaster1$tender_mainCpv <- sub("^(\\d{3}).*$", "\\1", italy_disaster1$tender_mainCpv)
italy_disaster1$tender_mainCpv <- as.integer(italy_disaster1$tender_mainCpv)
italy_disaster1$tender_mainCpv
vars <- c("treatment_Disaster1","buyer_buyerType", "tender_mainCpv", "lot_bidsCount", "tender_finalPrice_EUR")
temp<-italy_disaster1[vars]
vars2<- c("buyer_buyerType", "tender_mainCpv", "tender_finalPrice_EUR")
temp<- as.data.frame(temp)
imbalance(group=temp$treatment_Disaster1, data=temp[vars2])
summary(temp$tender_finalPrice_EUR)
valuecuts = c(5.348e+05, 1.543e+06, 5.591e+06)
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
italy_disaster1_1$lot_bidsCount <- ifelse(italy_disaster1_1$lot_bidsCount > 20, 20, italy_disaster1_1$lot_bidsCount)
didreg = lm(lot_bidsCount ~ did + time_d1 + treatcon_d1, data = italy_disaster1_1)
didreg
summary(didreg)


