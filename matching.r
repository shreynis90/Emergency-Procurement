library(cem)
library(readxl)
library(dplyr)
library(ggplot2)
library(gtsummary)
library(openxlsx)
library(data.table)
library(lubridate)
library(compare)
library(stringi)
memory.limit(size = 30000)
gc()
italy<- read_excel("data_09_20.xlsx")
italy<-as.data.frame(italy)

##Fixing the dates ----

italy$tender_publications_firstCallForTenderDate <- as.POSIXct(italy$tender_publications_firstCallForTenderDate,format='%Y/%m/%d')
italy$tender_publications_firstdContractAwardDate <- as.POSIXct(italy$tender_publications_firstdContractAwardDate,format='%Y/%m/%d')
italy$tender_bidDeadline <- as.POSIXct(italy$tender_bidDeadline,format='%Y/%m/%d')

##Fixing the issue with Dates from before 2011----
italy$contractDate<- fifelse(is.na(italy$tender_publications_firstCallForTenderDate),fifelse(is.na(italy$tender_publications_firstdContractAwardDate),as.POSIXct("2000-01-01", "%Y-%m-%d",tz="GMT"),italy$tender_publications_firstdContractAwardDate),italy$tender_publications_firstCallForTenderDate)
italy_temp <- italy %>% filter(contractDate> as.POSIXct("2011-01-01", "%Y-%m-%d",tz="GMT"))
## Median number of days between tenderbiddealine and firstcallfordate
diff<- median(round(difftime(italy_temp$tender_bidDeadline, italy_temp$tender_publications_firstCallForTenderDate, units = "days"),0), na.rm =  TRUE)




##Replacing the date with fresh date for all the contracts before 2011
italy$tender_publications_firstCallForTenderDate <- fifelse(italy$contractDate<"2011-01-01", italy$tender_bidDeadline - days(diff), italy$tender_publications_firstCallForTenderDate)
italy<- italy %>% filter(contractDate> as.POSIXct("2000-01-01", "%Y-%m-%d",tz="GMT"))

italy$buyer_name <- stri_trans_general(italy$buyer_name,id = "latin-ascii")
italy$buyer_name <- stri_trans_tolower(italy$buyer_name,locale = NULL)


##Disaster1 Analysis##----

##Converting to dataframe and Retaining first two digits of the CPV code
##Disaster 1 ----
italy_disaster1<- italy %>%
  filter(disnumber == "Disaster_001"|is.na(disnumber))
##

italy_disaster1 <- data.frame(italy_disaster1)
italy_disaster1$buyer_buyerType <- as.factor(italy_disaster1$buyer_buyerType) #Declaring buyer_buyerType as a factor
italy_disaster1$tender_mainCpv <- as.integer(italy_disaster1$tender_mainCpv)
italy_disaster1$tender_mainCpv <- sub("^(\\d{3}).*$", "\\1", italy_disaster1$tender_mainCpv) #Retaining only the first two digits of the the CPV code
italy_disaster1$tender_mainCpv <- as.integer(italy_disaster1$tender_mainCpv)
italy_disaster1$tender_mainCpv

##Breaking up the dates to extract months and years
italy_disaster1$contractyear<- ifelse(is.na(italy_disaster1$tender_publications_firstCallForTenderDate), substring(italy_disaster1$tender_publications_firstdContractAwardDate,1,4),substring(italy_disaster1$tender_publications_firstCallForTenderDate,1,4)) #Contract year
italy_disaster1$contractmonth <- ifelse(is.na(italy_disaster1$tender_publications_firstCallForTenderDate), substring(italy_disaster1$tender_publications_firstdContractAwardDate,6,7),substring(italy_disaster1$tender_publications_firstCallForTenderDate,6,7)) #Contract month
italy_disaster1$contractday <- ifelse(is.na(italy_disaster1$tender_publications_firstCallForTenderDate), substring(italy_disaster1$tender_publications_firstdContractAwardDate,9,10),substring(italy_disaster1$tender_publications_firstCallForTenderDate,9,10)) #Contract month
italy_disaster1$contractmonth <- as.numeric(italy_disaster1$contractmonth)
italy_disaster1$contractyear <- as.numeric(italy_disaster1$contractyear)
italy_disaster1$contractday <- as.numeric(italy_disaster1$contractday)

italy_disaster1$contractdate_final <- paste(italy_disaster1$contractyear, italy_disaster1$contractmonth, italy_disaster1$contractday, sep="-")
italy_disaster1$contractdate_final <- as.POSIXct(italy_disaster1$contractdate_final)
##
disaster1_date <- as.POSIXct("2009-10-02")

##Dropping those that did not have firstcalldate or tenderbiddeadline before 2011
italy_disaster1 <- subset(italy_disaster1, !(is.na(tender_publications_firstCallForTenderDate) & contractyear<2011)) 


italy_disaster1 <- italy_disaster1 %>% #Dropping the Contracts that are missing the number of bidders 
  filter(!is.na(lot_bidsCount))
italy_disaster1$lot_bidsCount <- ifelse(italy_disaster1$lot_bidsCount > 20, 20, italy_disaster1$lot_bidsCount)


## Disaster 1 Pretreatment ----

disaster1_pretreat0 <-  italy_disaster1 %>% filter(disaster1_date - as.difftime(1, unit="days") > italy_disaster1$contractdate_final)
disaster1_posttreat0 <- anti_join(italy_disaster1, disaster1_pretreat0)

d1name<-as.data.frame(disaster1_pretreat0$buyer_name)
d1name <- distinct(d1name) 

d1namepost<-as.data.frame(disaster1_posttreat0$buyer_name)
d1namepost <- distinct(d1namepost)

names(d1name)[1]<-paste("buyer_name")
names(d1namepost)[1]<-paste("buyer_name")

common1_0 <- intersect(d1name, d1namepost)

disaster1_pretreat_1<- inner_join(common1_0, disaster1_pretreat0, by= "buyer_name")
disaster1_pretreat <- disaster1_pretreat_1 %>% group_by(buyer_name) %>% mutate(meanlotbids = mean(lot_bidsCount)) #pretreament average of the number of bidders
common1_1<- cbind.data.frame(disaster1_pretreat$buyer_name, disaster1_pretreat$meanlotbids)
common1_1<- distinct(common1_1)
names(common1_1)[1]<-paste("buyer_name")
names(common1_1)[2]<-paste("meanlotbids")

disaster1_posttreat<- inner_join(common1_1, disaster1_posttreat0, by= "buyer_name")

disaster1_pretreat$contractyear <- as.factor(disaster1_pretreat$contractyear)
disaster1_pretreat$contractvalue <- ifelse(is.na(disaster1_pretreat$tender_finalPrice_EUR), ifelse(is.na(disaster1_pretreat$tender_estimatedPrice_EUR),"",disaster1_pretreat$tender_estimatedPrice_EUR),disaster1_pretreat$tender_finalPrice_EUR)
disaster1_pretreat$log_contractvalue <- log(as.numeric(as.character(disaster1_pretreat$contractvalue)))

disaster1_pretreat$buyer_buyerType<- as.factor(disaster1_pretreat$buyer_buyerType)
disaster1_pretreat$tender_mainCpv<- as.factor(disaster1_pretreat$tender_mainCpv)
disaster1_pretreat$contractyear<- as.factor(disaster1_pretreat$contractyear)

#Matching
vars <- c("treatcon","buyer_buyerType", "tender_mainCpv", "lot_bidsCount", "log_contractvalue", "meanlotbids", "contractyear")
temp1_0<-disaster1_pretreat[vars]
vars2<- c("buyer_buyerType", "tender_mainCpv", "log_contractvalue","contractyear", "meanlotbids")
temp1_0<- as.data.frame(temp1_0)
imbalance(group=temp1_0$treatcon, data=temp1_0[vars2])
summary(temp1_0$log_contractvalue)
valuecuts = c(13.955)
buyer_buyerType.grp<- list(c("REGIONAL_AUTHORITY", "REGIONAL_AGENCY", "UTILITIES"), c("NATIONAL_AUTHORITY"),c("OTHER"), c("PUBLIC_BODY"), c("NA",NA))
mat1_0 <- cem(treatment = "treatcon", data = temp1_0, drop = "lot_bidsCount",  grouping = list(buyer_buyerType= buyer_buyerType.grp))
mat1_0
mat1_0$w
est1_0 <- att(mat1_0, lot_bidsCount ~ treatcon, data = temp1_0)
est1_0
disaster1_pretreat$aftermatchtreat <- mat1_0$matched
disaster1_pretreat$aftermatchweight <- mat1_0$w
disaster1_pretreat_final <- disaster1_pretreat %>% filter(aftermatchtreat == TRUE)
disaster1_pretreat_final$timing <- 0


###Disaster1 Postreatment ----
disaster1_posttreat$contractyear <- as.factor(disaster1_posttreat$contractyear)
disaster1_posttreat$contractvalue <- ifelse(is.na(disaster1_posttreat$tender_finalPrice_EUR), ifelse(is.na(disaster1_posttreat$tender_estimatedPrice_EUR),"",disaster1_posttreat$tender_estimatedPrice_EUR),disaster1_posttreat$tender_finalPrice_EUR)
disaster1_posttreat$log_contractvalue <- log(as.numeric(as.character(disaster1_posttreat$contractvalue)))

disaster1_posttreat$buyer_buyerType<- as.factor(disaster1_posttreat$buyer_buyerType)
disaster1_posttreat$tender_mainCpv<- as.factor(disaster1_posttreat$tender_mainCpv)
disaster1_posttreat$contractyear<- as.factor(disaster1_posttreat$contractyear)

#Matching
vars <- c("treatcon","buyer_buyerType", "tender_mainCpv", "lot_bidsCount", "log_contractvalue", "meanlotbids", "contractyear")
temp1_1<-disaster1_posttreat[vars]
vars2<- c("buyer_buyerType", "tender_mainCpv", "log_contractvalue", "meanlotbids","contractyear")
temp1_1<- as.data.frame(temp1_1)
imbalance(group=temp1_1$treatcon, data=temp1_1[vars2])
summary(temp1_1$log_contractvalue)
valuecuts = c(14.192)
buyer_buyerType.grp<- list(c("REGIONAL_AUTHORITY", "REGIONAL_AGENCY", "UTILITIES"), c("NATIONAL_AUTHORITY"),c("OTHER"), c("PUBLIC_BODY"), c("NA",NA))
mat1_1 <- cem(treatment = "treatcon", data = temp1_1, drop = "lot_bidsCount", grouping = list(buyer_buyerType= buyer_buyerType.grp))
mat1_1
est1_1 <- att(mat1_1, lot_bidsCount ~ treatcon, data = temp1_1)
est1_1
disaster1_posttreat$aftermatchtreat <- mat1_1$matched
disaster1_posttreat$aftermatchweight <- mat1_1$w
disaster1_posttreat_final <- disaster1_posttreat %>% filter(aftermatchtreat == TRUE)
disaster1_posttreat_final$timing <- 1

disaster1_matched <- rbind(as.data.frame(disaster1_pretreat_final), as.data.frame(disaster1_posttreat_final))
disaster1_matched<- as.data.frame(disaster1_matched)
disaster1_matched$did <- disaster1_matched$treatcon*disaster1_matched$timing
disaster1_matched$matchsource <- "d1"

disaster1_did<- lm(lot_bidsCount ~ treatcon + timing + did, data = disaster1_matched, weights = aftermatchweight)
summary.lm(disaster1_did)


##Disaster 2 ----
italy_disaster2<- italy %>%
  filter(disnumber == "Disaster_002"|is.na(disnumber))
##

italy_disaster2 <- data.frame(italy_disaster2)
italy_disaster2$buyer_buyerType <- as.factor(italy_disaster2$buyer_buyerType) #Declaring buyer_buyerType as a factor
italy_disaster2$tender_mainCpv <- as.integer(italy_disaster2$tender_mainCpv)
italy_disaster2$tender_mainCpv <- sub("^(\\d{3}).*$", "\\1", italy_disaster2$tender_mainCpv) #Retaining only the first three digits of the the CPV code
italy_disaster2$tender_mainCpv <- as.integer(italy_disaster2$tender_mainCpv)
italy_disaster2$tender_mainCpv

##Breaking up the dates to extract months and years
italy_disaster2$contractyear<- ifelse(is.na(italy_disaster2$tender_publications_firstCallForTenderDate), substring(italy_disaster2$tender_publications_firstdContractAwardDate,1,4),substring(italy_disaster2$tender_publications_firstCallForTenderDate,1,4)) #Contract year
italy_disaster2$contractmonth <- ifelse(is.na(italy_disaster2$tender_publications_firstCallForTenderDate), substring(italy_disaster2$tender_publications_firstdContractAwardDate,6,7),substring(italy_disaster2$tender_publications_firstCallForTenderDate,6,7)) #Contract month
italy_disaster2$contractday <- ifelse(is.na(italy_disaster2$tender_publications_firstCallForTenderDate), substring(italy_disaster2$tender_publications_firstdContractAwardDate,9,10),substring(italy_disaster2$tender_publications_firstCallForTenderDate,9,10)) #Contract month
italy_disaster2$contractmonth <- as.numeric(italy_disaster2$contractmonth)
italy_disaster2$contractyear <- as.numeric(italy_disaster2$contractyear)
italy_disaster2$contractday <- as.numeric(italy_disaster2$contractday)

italy_disaster2$contractdate_final <- paste(italy_disaster2$contractyear, italy_disaster2$contractmonth, italy_disaster2$contractday, sep="-")
italy_disaster2$contractdate_final <- as.POSIXct(italy_disaster2$contractdate_final)

##
disaster2_date <- as.POSIXct("2012-05-29")

##Dropping those that did not have firstcalldate or tenderbiddeadline before 2011
italy_disaster2 <- subset(italy_disaster2, !(is.na(tender_publications_firstCallForTenderDate) & contractyear<2011)) 


##Pretreatment average of the dependent variable
italy_disaster2 <- italy_disaster2 %>% #Dropping the Contracts that are missing the number of bidders 
  filter(!is.na(lot_bidsCount))
italy_disaster2$lot_bidsCount <- ifelse(italy_disaster2$lot_bidsCount > 20, 20, italy_disaster2$lot_bidsCount)
italy_disaster2$contractmonth <- as.numeric(italy_disaster2$contractmonth)
italy_disaster2$contractyear <- as.numeric(italy_disaster2$contractyear)

## Disaster 2 Pretreatment ----

disaster2_pretreat0 <-  italy_disaster2 %>% filter(disaster2_date > contractdate_final)
disaster2_posttreat0 <- anti_join(italy_disaster2, disaster2_pretreat0)

d2name<-as.data.frame(disaster2_pretreat0$buyer_name)
d2name <- distinct(d2name) 

d2namepost<-as.data.frame(disaster2_posttreat0$buyer_name)
d2namepost <- distinct(d2namepost)

names(d2name)[1]<-paste("buyer_name")
names(d2namepost)[1]<-paste("buyer_name")

common2_0 <- intersect(d2name, d2namepost)

disaster2_pretreat_1<- inner_join(common2_0, disaster2_pretreat0, by= "buyer_name")
disaster2_pretreat <- disaster2_pretreat_1 %>% group_by(buyer_name) %>% mutate(meanlotbids = mean(lot_bidsCount)) #pretreament average of the number of bidders
common2_1<- cbind.data.frame(disaster2_pretreat$buyer_name, disaster2_pretreat$meanlotbids)
common2_1<- distinct(common2_1)
names(common2_1)[1]<-paste("buyer_name")
names(common2_1)[2]<-paste("meanlotbids")

disaster2_posttreat<- inner_join(common2_1, disaster2_posttreat0, by= "buyer_name")


disaster2_pretreat$contractyear <- as.factor(disaster2_pretreat$contractyear)
disaster2_pretreat$contractvalue <- ifelse(is.na(disaster2_pretreat$tender_finalPrice_EUR), ifelse(is.na(disaster2_pretreat$tender_estimatedPrice_EUR),"",disaster2_pretreat$tender_estimatedPrice_EUR),disaster2_pretreat$tender_finalPrice_EUR)
disaster2_pretreat$log_contractvalue <- log(as.numeric(as.character(disaster2_pretreat$contractvalue)))

disaster2_pretreat$buyer_buyerType<- as.factor(disaster2_pretreat$buyer_buyerType)
disaster2_pretreat$tender_mainCpv<- as.factor(disaster2_pretreat$tender_mainCpv)
disaster2_pretreat$contractyear<- as.factor(disaster2_pretreat$contractyear)

#Matching
vars <- c("treatcon","buyer_buyerType", "tender_mainCpv", "lot_bidsCount", "log_contractvalue", "meanlotbids", "contractyear")
temp2_0<-disaster2_pretreat[vars]
vars2<- c("buyer_buyerType", "tender_mainCpv", "log_contractvalue", "meanlotbids","contractyear")
vars3<- c("log_contractvalue","meanlotbids","contractyear","tender_mainCpv")
temp2_0<- as.data.frame(temp2_0)
imbalance(group=temp2_0$treatcon, data=temp2_0[vars2])
summary(temp2_0$log_contractvalue)
valuecuts = c(14.109)
buyer_buyerType.grp<- list(c("REGIONAL_AUTHORITY", "REGIONAL_AGENCY", "UTILITIES"), c("NATIONAL_AUTHORITY"),c("OTHER"), c("PUBLIC_BODY"), c("NA",NA))
mat2_0 <- cem(treatment = "treatcon", data = temp2_0, drop = "lot_bidsCount", grouping = list(buyer_buyerType= buyer_buyerType.grp))
mat2_0
est2_0 <- att(mat2_0, lot_bidsCount ~ treatcon, data = temp2_0)
est2_0
disaster2_pretreat$aftermatchtreat <- mat2_0$matched
disaster2_pretreat$aftermatchweight <- mat2_0$w
disaster2_pretreat_final <- disaster2_pretreat %>% filter(aftermatchtreat == TRUE)
disaster2_pretreat_final$timing <- 0


###Disaster2 Postreatment ----



disaster2_posttreat$contractyear <- as.factor(disaster2_posttreat$contractyear)
disaster2_posttreat$contractvalue <- ifelse(is.na(disaster2_posttreat$tender_finalPrice_EUR), ifelse(is.na(disaster2_posttreat$tender_estimatedPrice_EUR),"",disaster2_posttreat$tender_estimatedPrice_EUR),disaster2_posttreat$tender_finalPrice_EUR)
disaster2_posttreat$log_contractvalue <- log(as.numeric(as.character(disaster2_posttreat$contractvalue)))

disaster2_posttreat$buyer_buyerType<- as.factor(disaster2_posttreat$buyer_buyerType)
disaster2_posttreat$tender_mainCpv<- as.factor(disaster2_posttreat$tender_mainCpv)
disaster2_posttreat$contractyear<- as.factor(disaster2_posttreat$contractyear)

#Matching
vars <- c("treatcon","buyer_buyerType", "tender_mainCpv", "lot_bidsCount", "log_contractvalue", "meanlotbids", "contractyear")
temp2_1<-disaster2_posttreat[vars]
vars2<- c("buyer_buyerType", "tender_mainCpv", "log_contractvalue", "meanlotbids","contractyear")
temp2_1<- as.data.frame(temp2_1)
imbalance(group=temp2_1$treatcon, data=temp2_1[vars2])
summary(temp2_1$log_contractvalue)
valuecuts = c(14.25)
buyer_buyerType.grp<- list(c("REGIONAL_AUTHORITY", "REGIONAL_AGENCY", "UTILITIES"), c("NATIONAL_AUTHORITY"),c("OTHER"), c("PUBLIC_BODY"), c("NA",NA))
mat2_1 <- cem(treatment = "treatcon", data = temp2_1, drop = "lot_bidsCount", grouping = list(buyer_buyerType= buyer_buyerType.grp))
mat2_1
est2_1 <- att(mat2_1, lot_bidsCount ~ treatcon, data = temp2_1)
est2_1
disaster2_posttreat$aftermatchtreat <- mat2_1$matched
disaster2_posttreat$aftermatchweight <- mat2_1$w
disaster2_posttreat_final <- disaster2_posttreat %>% filter(aftermatchtreat == TRUE)
disaster2_posttreat_final$timing <- 1
disaster2_matched <- rbind(as.data.frame(disaster2_pretreat_final), as.data.frame(disaster2_posttreat_final))
disaster2_matched$did <- disaster2_matched$treatcon*disaster2_matched$timing
disaster2_matched$matchsource <- "d2"

disaster2_did<- lm(lot_bidsCount ~ treatcon + timing + did, data = disaster2_matched, weights = aftermatchweight)
summary.lm(disaster2_did)

##Disaster 3 ----
italy_disaster3<- italy %>%
  filter(disnumber == "Disaster_003"|is.na(disnumber))
##

italy_disaster3 <- data.frame(italy_disaster3)
italy_disaster3$buyer_buyerType <- as.factor(italy_disaster3$buyer_buyerType) #Declaring buyer_buyerType as a factor
italy_disaster3$tender_mainCpv <- as.integer(italy_disaster3$tender_mainCpv)
italy_disaster3$tender_mainCpv <- sub("^(\\d{3}).*$", "\\1", italy_disaster3$tender_mainCpv) #Retaining only the first three digits of the the CPV code
italy_disaster3$tender_mainCpv <- as.integer(italy_disaster3$tender_mainCpv)
italy_disaster3$tender_mainCpv

##Breaking up the dates to extract months and years
italy_disaster3$contractyear<- ifelse(is.na(italy_disaster3$tender_publications_firstCallForTenderDate), substring(italy_disaster3$tender_publications_firstdContractAwardDate,1,4),substring(italy_disaster3$tender_publications_firstCallForTenderDate,1,4)) #Contract year
italy_disaster3$contractmonth <- ifelse(is.na(italy_disaster3$tender_publications_firstCallForTenderDate), substring(italy_disaster3$tender_publications_firstdContractAwardDate,6,7),substring(italy_disaster3$tender_publications_firstCallForTenderDate,6,7)) #Contract month
italy_disaster3$contractday <- ifelse(is.na(italy_disaster3$tender_publications_firstCallForTenderDate), substring(italy_disaster3$tender_publications_firstdContractAwardDate,9,10),substring(italy_disaster3$tender_publications_firstCallForTenderDate,9,10)) #Contract month
italy_disaster3$contractmonth <- as.numeric(italy_disaster3$contractmonth)
italy_disaster3$contractyear <- as.numeric(italy_disaster3$contractyear)
italy_disaster3$contractday <- as.numeric(italy_disaster3$contractday)

italy_disaster3$contractdate_final <- paste(italy_disaster3$contractyear, italy_disaster3$contractmonth, italy_disaster3$contractday, sep="-")
italy_disaster3$contractdate_final <- as.POSIXct(italy_disaster3$contractdate_final)

##
disaster3_date <- as.POSIXct("2013-11-18")

##Dropping those that did not have firstcalldate or tenderbiddeadline before 2011
italy_disaster3 <- subset(italy_disaster3, !(is.na(tender_publications_firstCallForTenderDate) & contractyear<2011)) 


##Pretreatment average of the dependent variable
italy_disaster3 <- italy_disaster3 %>% #Dropping the Contracts that are missing the number of bidders 
  filter(!is.na(lot_bidsCount))
italy_disaster3$lot_bidsCount <- ifelse(italy_disaster3$lot_bidsCount > 20, 20, italy_disaster3$lot_bidsCount)
italy_disaster3$contractmonth <- as.numeric(italy_disaster3$contractmonth)
italy_disaster3$contractyear <- as.numeric(italy_disaster3$contractyear)

## Disaster 3 Pretreatment ----
disaster3_pretreat0 <-  italy_disaster3 %>% filter(disaster3_date > contractdate_final)
disaster3_posttreat0 <- anti_join(italy_disaster3, disaster3_pretreat0)

d3name<-as.data.frame(disaster3_pretreat0$buyer_name)
d3name <- distinct(d3name) 

d3namepost<-as.data.frame(disaster3_posttreat0$buyer_name)
d3namepost <- distinct(d3namepost)

names(d3name)[1]<-paste("buyer_name")
names(d3namepost)[1]<-paste("buyer_name")

common3_0 <- intersect(d3name, d3namepost)

disaster3_pretreat_1<- inner_join(common3_0, disaster3_pretreat0, by= "buyer_name")
disaster3_pretreat <- disaster3_pretreat_1 %>% group_by(buyer_name) %>% mutate(meanlotbids = mean(lot_bidsCount)) #pretreament average of the number of bidders
common3_1<- cbind.data.frame(disaster3_pretreat$buyer_name, disaster3_pretreat$meanlotbids)
common3_1<- distinct(common3_1)
names(common3_1)[1]<-paste("buyer_name")
names(common3_1)[2]<-paste("meanlotbids")

disaster3_posttreat<- inner_join(common3_1, disaster3_posttreat0, by= "buyer_name")


disaster3_pretreat$contractyear <- as.factor(disaster3_pretreat$contractyear)
disaster3_pretreat$contractvalue <- ifelse(is.na(disaster3_pretreat$tender_finalPrice_EUR), ifelse(is.na(disaster3_pretreat$tender_estimatedPrice_EUR),"",disaster3_pretreat$tender_estimatedPrice_EUR),disaster3_pretreat$tender_finalPrice_EUR)
disaster3_pretreat$log_contractvalue <- log(as.numeric(as.character(disaster3_pretreat$contractvalue)))

disaster3_pretreat$buyer_buyerType<- as.factor(disaster3_pretreat$buyer_buyerType)
disaster3_pretreat$tender_mainCpv<- as.factor(disaster3_pretreat$tender_mainCpv)
disaster3_pretreat$contractyear<- as.factor(disaster3_pretreat$contractyear)

#Matching
vars <- c("treatcon","buyer_buyerType", "tender_mainCpv", "lot_bidsCount", "log_contractvalue", "meanlotbids", "contractyear")
temp3_0<-disaster3_pretreat[vars]
vars2<- c("buyer_buyerType", "tender_mainCpv", "log_contractvalue", "meanlotbids","contractyear")
vars3<- c("log_contractvalue","meanlotbids","contractyear","tender_mainCpv")
temp3_0<- as.data.frame(temp3_0)
imbalance(group=temp3_0$treatcon, data=temp3_0[vars2])
summary(temp3_0$log_contractvalue)
valuecuts = c(14.116)
buyer_buyerType.grp<- list(c("REGIONAL_AUTHORITY", "REGIONAL_AGENCY", "UTILITIES"), c("NATIONAL_AUTHORITY"),c("OTHER"), c("PUBLIC_BODY"), c("NA",NA))
mat3_0 <- cem(treatment = "treatcon", data = temp3_0, drop = "lot_bidsCount", grouping = list(buyer_buyerType= buyer_buyerType.grp))
mat3_0
est3_0 <- att(mat3_0, lot_bidsCount ~ treatcon, data = temp3_0)
est3_0
disaster3_pretreat$aftermatchtreat <- mat3_0$matched
disaster3_pretreat$aftermatchweight <- mat3_0$w
disaster3_pretreat_final <- disaster3_pretreat %>% filter(aftermatchtreat == TRUE)
disaster3_pretreat_final$timing <- 0

###Disaster3 Postreatment ----

disaster3_posttreat$contractyear <- as.factor(disaster3_posttreat$contractyear)
disaster3_posttreat$contractvalue <- ifelse(is.na(disaster3_posttreat$tender_finalPrice_EUR), ifelse(is.na(disaster3_posttreat$tender_estimatedPrice_EUR),"",disaster3_posttreat$tender_estimatedPrice_EUR),disaster3_posttreat$tender_finalPrice_EUR)
disaster3_posttreat$log_contractvalue <- log(as.numeric(as.character(disaster3_posttreat$contractvalue)))

disaster3_posttreat$buyer_buyerType<- as.factor(disaster3_posttreat$buyer_buyerType)
disaster3_posttreat$tender_mainCpv<- as.factor(disaster3_posttreat$tender_mainCpv)
disaster3_posttreat$contractyear<- as.factor(disaster3_posttreat$contractyear)

#Matching
vars <- c("treatcon","buyer_buyerType", "tender_mainCpv", "lot_bidsCount", "log_contractvalue", "meanlotbids", "contractyear")
temp3_1<-disaster3_posttreat[vars]
vars2<- c("buyer_buyerType", "tender_mainCpv", "log_contractvalue", "meanlotbids","contractyear")
temp3_1<- as.data.frame(temp3_1)
imbalance(group=temp3_1$treatcon, data=temp3_1[vars2])
summary(temp3_1$log_contractvalue)
valuecuts = c(14.36)
buyer_buyerType.grp<- list(c("REGIONAL_AUTHORITY", "REGIONAL_AGENCY", "UTILITIES"), c("NATIONAL_AUTHORITY"),c("OTHER"), c("PUBLIC_BODY"), c("NA",NA))
mat3_1 <- cem(treatment = "treatcon", data = temp3_1, drop = "lot_bidsCount", grouping = list(buyer_buyerType= buyer_buyerType.grp))
mat3_1
est3_1 <- att(mat3_1, lot_bidsCount ~ treatcon, data = temp3_1)
est3_1
disaster3_posttreat$aftermatchtreat <- mat3_1$matched
disaster3_posttreat$aftermatchweight <- mat3_1$w
disaster3_posttreat_final <- disaster3_posttreat %>% filter(aftermatchtreat == TRUE)
disaster3_posttreat_final$timing <- 1
disaster3_matched <- rbind(as.data.frame(disaster3_pretreat_final), as.data.frame(disaster3_posttreat_final))
disaster3_matched$did <- disaster3_matched$treatcon*disaster3_matched$timing
disaster3_matched$matchsource <- "d3"

disaster3_did<- lm(lot_bidsCount ~ treatcon + timing + did, data = disaster3_matched, weights = aftermatchweight)
summary.lm(disaster3_did)

##Disaster 4 ----
italy_disaster4<- italy %>%
  filter(disnumber == "Disaster_004"|is.na(disnumber))
##

italy_disaster4 <- data.frame(italy_disaster4)
italy_disaster4$buyer_buyerType <- as.factor(italy_disaster4$buyer_buyerType) #Declaring buyer_buyerType as a factor
italy_disaster4$tender_mainCpv <- as.integer(italy_disaster4$tender_mainCpv)
italy_disaster4$tender_mainCpv <- sub("^(\\d{3}).*$", "\\1", italy_disaster4$tender_mainCpv) #Retaining only the first three digits of the the CPV code
italy_disaster4$tender_mainCpv <- as.integer(italy_disaster4$tender_mainCpv)
italy_disaster4$tender_mainCpv

##Breaking up the dates to extract months and years
italy_disaster4$contractyear<- ifelse(is.na(italy_disaster4$tender_publications_firstCallForTenderDate), substring(italy_disaster4$tender_publications_firstdContractAwardDate,1,4),substring(italy_disaster4$tender_publications_firstCallForTenderDate,1,4)) #Contract year
italy_disaster4$contractmonth <- ifelse(is.na(italy_disaster4$tender_publications_firstCallForTenderDate), substring(italy_disaster4$tender_publications_firstdContractAwardDate,6,7),substring(italy_disaster4$tender_publications_firstCallForTenderDate,6,7)) #Contract month
italy_disaster4$contractday <- ifelse(is.na(italy_disaster4$tender_publications_firstCallForTenderDate), substring(italy_disaster4$tender_publications_firstdContractAwardDate,9,10),substring(italy_disaster4$tender_publications_firstCallForTenderDate,9,10)) #Contract month
italy_disaster4$contractmonth <- as.numeric(italy_disaster4$contractmonth)
italy_disaster4$contractyear <- as.numeric(italy_disaster4$contractyear)
italy_disaster4$contractday <- as.numeric(italy_disaster4$contractday)

italy_disaster4$contractdate_final <- paste(italy_disaster4$contractyear, italy_disaster4$contractmonth, italy_disaster4$contractday, sep="-")
italy_disaster4$contractdate_final <- as.POSIXct(italy_disaster4$contractdate_final)

##
disaster4_date <- as.POSIXct("2016-08-24")

##Dropping those that did not have firstcalldate or tenderbiddeadline before 2011
italy_disaster4 <- subset(italy_disaster4, !(is.na(tender_publications_firstCallForTenderDate) & contractyear<2011)) 


##Pretreatment average of the dependent variable
italy_disaster4 <- italy_disaster4 %>% #Dropping the Contracts that are missing the number of bidders 
  filter(!is.na(lot_bidsCount))
italy_disaster4$lot_bidsCount <- ifelse(italy_disaster4$lot_bidsCount > 20, 20, italy_disaster4$lot_bidsCount)
italy_disaster4$contractmonth <- as.numeric(italy_disaster4$contractmonth)
italy_disaster4$contractyear <- as.numeric(italy_disaster4$contractyear)

## Disaster 4 Pretreatment ----
disaster4_pretreat0 <-  italy_disaster4 %>% filter(disaster4_date > contractdate_final)
disaster4_posttreat0 <- anti_join(italy_disaster4, disaster4_pretreat0)

d4name<-as.data.frame(disaster4_pretreat0$buyer_name)
d4name <- distinct(d4name) 

d4namepost<-as.data.frame(disaster4_posttreat0$buyer_name)
d4namepost <- distinct(d4namepost)

names(d4name)[1]<-paste("buyer_name")
names(d4namepost)[1]<-paste("buyer_name")

common4_0 <- intersect(d4name, d4namepost)

disaster4_pretreat_1<- inner_join(common4_0, disaster4_pretreat0, by= "buyer_name")
disaster4_pretreat <- disaster4_pretreat_1 %>% group_by(buyer_name) %>% mutate(meanlotbids = mean(lot_bidsCount)) #pretreament average of the number of bidders
common4_1<- cbind.data.frame(disaster4_pretreat$buyer_name, disaster4_pretreat$meanlotbids)
common4_1<- distinct(common4_1)
names(common4_1)[1]<-paste("buyer_name")
names(common4_1)[2]<-paste("meanlotbids")

disaster4_posttreat<- inner_join(common4_1, disaster4_posttreat0, by= "buyer_name")


disaster4_pretreat$contractyear <- as.factor(disaster4_pretreat$contractyear)
disaster4_pretreat$contractvalue <- ifelse(is.na(disaster4_pretreat$tender_finalPrice_EUR), ifelse(is.na(disaster4_pretreat$tender_estimatedPrice_EUR),"",disaster4_pretreat$tender_estimatedPrice_EUR),disaster4_pretreat$tender_finalPrice_EUR)
disaster4_pretreat$log_contractvalue <- log(as.numeric(as.character(disaster4_pretreat$contractvalue)))

disaster4_pretreat$buyer_buyerType<- as.factor(disaster4_pretreat$buyer_buyerType)
disaster4_pretreat$tender_mainCpv<- as.factor(disaster4_pretreat$tender_mainCpv)
disaster4_pretreat$contractyear<- as.factor(disaster4_pretreat$contractyear)

#Matching
vars <- c("treatcon","buyer_buyerType", "tender_mainCpv", "lot_bidsCount", "log_contractvalue", "meanlotbids", "contractyear")
temp4_0<-disaster4_pretreat[vars]
vars2<- c("buyer_buyerType", "tender_mainCpv", "log_contractvalue", "meanlotbids","contractyear")
vars3<- c("log_contractvalue","meanlotbids","contractyear","tender_mainCpv")
temp4_0<- as.data.frame(temp4_0)
imbalance(group=temp4_0$treatcon, data=temp4_0[vars2])
summary(temp4_0$log_contractvalue)
valuecuts = c(14.174)
buyer_buyerType.grp<- list(c("REGIONAL_AUTHORITY", "REGIONAL_AGENCY", "UTILITIES"), c("NATIONAL_AUTHORITY"),c("OTHER"), c("PUBLIC_BODY"), c("NA",NA))
mat4_0 <- cem(treatment = "treatcon", data = temp4_0, drop = "lot_bidsCount", grouping = list(buyer_buyerType= buyer_buyerType.grp))
mat4_0
est4_0 <- att(mat4_0, lot_bidsCount ~ treatcon, data = temp4_0)
est4_0
disaster4_pretreat$aftermatchtreat <- mat4_0$matched
disaster4_pretreat$aftermatchweight <- mat4_0$w
disaster4_pretreat_final <- disaster4_pretreat %>% filter(aftermatchtreat == TRUE)
disaster4_pretreat_final$timing <- 0

###Disaster4 Postreatment ----



disaster4_posttreat$contractyear <- as.factor(disaster4_posttreat$contractyear)
disaster4_posttreat$contractvalue <- ifelse(is.na(disaster4_posttreat$tender_finalPrice_EUR), ifelse(is.na(disaster4_posttreat$tender_estimatedPrice_EUR),"",disaster4_posttreat$tender_estimatedPrice_EUR),disaster4_posttreat$tender_finalPrice_EUR)
disaster4_posttreat$log_contractvalue <- log(as.numeric(as.character(disaster4_posttreat$contractvalue)))

disaster4_posttreat$buyer_buyerType<- as.factor(disaster4_posttreat$buyer_buyerType)
disaster4_posttreat$tender_mainCpv<- as.factor(disaster4_posttreat$tender_mainCpv)
disaster4_posttreat$contractyear<- as.factor(disaster4_posttreat$contractyear)

#Matching
vars <- c("treatcon","buyer_buyerType", "tender_mainCpv", "lot_bidsCount", "log_contractvalue", "meanlotbids", "contractyear")
temp4_1<-disaster4_posttreat[vars]
vars2<- c("buyer_buyerType", "tender_mainCpv", "log_contractvalue", "meanlotbids","contractyear")
temp4_1<- as.data.frame(temp4_1)
imbalance(group=temp4_1$treatcon, data=temp4_1[vars2])
summary(temp4_1$log_contractvalue)
valuecuts = c(14.31)
buyer_buyerType.grp<- list(c("REGIONAL_AUTHORITY", "REGIONAL_AGENCY", "UTILITIES"), c("NATIONAL_AUTHORITY"),c("OTHER"), c("PUBLIC_BODY"), c("NA",NA))
mat4_1 <- cem(treatment = "treatcon", data = temp4_1, drop = "lot_bidsCount", grouping = list(buyer_buyerType= buyer_buyerType.grp))
mat4_1
est4_1 <- att(mat4_1, lot_bidsCount ~ treatcon, data = temp4_1)
est4_1
disaster4_posttreat$aftermatchtreat <- mat4_1$matched
disaster4_posttreat$aftermatchweight <- mat4_1$w
disaster4_posttreat_final <- disaster4_posttreat %>% filter(aftermatchtreat == TRUE)
disaster4_posttreat_final$timing <- 1
disaster4_matched <- rbind(as.data.frame(disaster4_pretreat_final), as.data.frame(disaster4_posttreat_final))
disaster4_matched$did <- disaster4_matched$treatcon*disaster4_matched$timing
disaster4_matched$matchsource <- "d4"

disaster4_did<- lm(lot_bidsCount ~ treatcon + timing + did, data = disaster4_matched, weights = aftermatchweight)
summary.lm(disaster4_did)

##Disaster 5 ----
italy_disaster5<- italy %>%
  filter(disnumber == "Disaster_005"|is.na(disnumber))
##

italy_disaster5 <- data.frame(italy_disaster5)
italy_disaster5$buyer_buyerType <- as.factor(italy_disaster5$buyer_buyerType) #Declaring buyer_buyerType as a factor
italy_disaster5$tender_mainCpv <- as.integer(italy_disaster5$tender_mainCpv)
italy_disaster5$tender_mainCpv <- sub("^(\\d{3}).*$", "\\1", italy_disaster5$tender_mainCpv) #Retaining only the first three digits of the the CPV code
italy_disaster5$tender_mainCpv <- as.integer(italy_disaster5$tender_mainCpv)
italy_disaster5$tender_mainCpv

##Breaking up the dates to extract months and years
italy_disaster5$contractyear<- ifelse(is.na(italy_disaster5$tender_publications_firstCallForTenderDate), substring(italy_disaster5$tender_publications_firstdContractAwardDate,1,4),substring(italy_disaster5$tender_publications_firstCallForTenderDate,1,4)) #Contract year
italy_disaster5$contractmonth <- ifelse(is.na(italy_disaster5$tender_publications_firstCallForTenderDate), substring(italy_disaster5$tender_publications_firstdContractAwardDate,6,7),substring(italy_disaster5$tender_publications_firstCallForTenderDate,6,7)) #Contract month
italy_disaster5$contractday <- ifelse(is.na(italy_disaster5$tender_publications_firstCallForTenderDate), substring(italy_disaster5$tender_publications_firstdContractAwardDate,9,10),substring(italy_disaster5$tender_publications_firstCallForTenderDate,9,10)) #Contract month
italy_disaster5$contractmonth <- as.numeric(italy_disaster5$contractmonth)
italy_disaster5$contractyear <- as.numeric(italy_disaster5$contractyear)
italy_disaster5$contractday <- as.numeric(italy_disaster5$contractday)

italy_disaster5$contractdate_final <- paste(italy_disaster5$contractyear, italy_disaster5$contractmonth, italy_disaster5$contractday, sep="-")
italy_disaster5$contractdate_final <- as.POSIXct(italy_disaster5$contractdate_final)

##
disaster5_date <- as.POSIXct("2017-01-18")

##Dropping those that did not have firstcalldate or tenderbiddeadline before 2011
italy_disaster5 <- subset(italy_disaster5, !(is.na(tender_publications_firstCallForTenderDate) & contractyear<2011)) 


##Pretreatment average of the dependent variable
italy_disaster5 <- italy_disaster5 %>% #Dropping the Contracts that are missing the number of bidders 
  filter(!is.na(lot_bidsCount))
italy_disaster5$lot_bidsCount <- ifelse(italy_disaster5$lot_bidsCount > 20, 20, italy_disaster5$lot_bidsCount)
italy_disaster5$contractmonth <- as.numeric(italy_disaster5$contractmonth)
italy_disaster5$contractyear <- as.numeric(italy_disaster5$contractyear)


## Disaster 5 Pretreatment ----
disaster5_pretreat0 <-  italy_disaster5 %>% filter(disaster5_date > contractdate_final)
disaster5_posttreat0 <- anti_join(italy_disaster5, disaster5_pretreat0)

d5name<-as.data.frame(disaster5_pretreat0$buyer_name)
d5name <- distinct(d5name) 

d5namepost<-as.data.frame(disaster5_posttreat0$buyer_name)
d5namepost <- distinct(d5namepost)

names(d5name)[1]<-paste("buyer_name")
names(d5namepost)[1]<-paste("buyer_name")

common5_0 <- intersect(d5name, d5namepost)

disaster5_pretreat_1<- inner_join(common5_0, disaster5_pretreat0, by= "buyer_name")
disaster5_pretreat <- disaster5_pretreat_1 %>% group_by(buyer_name) %>% mutate(meanlotbids = mean(lot_bidsCount)) #pretreament average of the number of bidders
common5_1<- cbind.data.frame(disaster5_pretreat$buyer_name, disaster5_pretreat$meanlotbids)
common5_1<- distinct(common5_1)
names(common5_1)[1]<-paste("buyer_name")
names(common5_1)[2]<-paste("meanlotbids")

disaster5_posttreat<- inner_join(common5_1, disaster5_posttreat0, by= "buyer_name")

disaster5_pretreat$contractyear <- as.factor(disaster5_pretreat$contractyear)
disaster5_pretreat$contractvalue <- ifelse(is.na(disaster5_pretreat$tender_finalPrice_EUR), ifelse(is.na(disaster5_pretreat$tender_estimatedPrice_EUR),"",disaster5_pretreat$tender_estimatedPrice_EUR),disaster5_pretreat$tender_finalPrice_EUR)
disaster5_pretreat$log_contractvalue <- log(as.numeric(as.character(disaster5_pretreat$contractvalue)))

disaster5_pretreat$buyer_buyerType<- as.factor(disaster5_pretreat$buyer_buyerType)
disaster5_pretreat$tender_mainCpv<- as.factor(disaster5_pretreat$tender_mainCpv)
disaster5_pretreat$contractyear<- as.factor(disaster5_pretreat$contractyear)

#Matching
vars <- c("treatcon","buyer_buyerType", "tender_mainCpv", "lot_bidsCount", "log_contractvalue", "meanlotbids", "contractyear")
temp5_0<-disaster5_pretreat[vars]
vars2<- c("buyer_buyerType", "tender_mainCpv", "log_contractvalue", "meanlotbids","contractyear")
vars3<- c("log_contractvalue","meanlotbids","contractyear","tender_mainCpv")
temp5_0<- as.data.frame(temp5_0)
imbalance(group=temp5_0$treatcon, data=temp5_0[vars2])
summary(temp5_0$log_contractvalue)
valuecuts = c(14.211)
buyer_buyerType.grp<- list(c("REGIONAL_AUTHORITY", "REGIONAL_AGENCY", "UTILITIES"), c("NATIONAL_AUTHORITY"),c("OTHER"), c("PUBLIC_BODY"), c("NA",NA))
mat5_0 <- cem(treatment = "treatcon", data = temp5_0, drop = "lot_bidsCount", grouping = list(buyer_buyerType= buyer_buyerType.grp))
mat5_0
est5_0 <- att(mat5_0, lot_bidsCount ~ treatcon, data = temp5_0)
est5_0
disaster5_pretreat$aftermatchtreat <- mat5_0$matched
disaster5_pretreat$aftermatchweight <- mat5_0$w
disaster5_pretreat_final <- disaster5_pretreat %>% filter(aftermatchtreat == TRUE)
disaster5_pretreat_final$timing <- 0


###Disaster5 Postreatment ----
disaster5_posttreat$contractyear <- as.factor(disaster5_posttreat$contractyear)
disaster5_posttreat$contractvalue <- ifelse(is.na(disaster5_posttreat$tender_finalPrice_EUR), ifelse(is.na(disaster5_posttreat$tender_estimatedPrice_EUR),"",disaster5_posttreat$tender_estimatedPrice_EUR),disaster5_posttreat$tender_finalPrice_EUR)
disaster5_posttreat$log_contractvalue <- log(as.numeric(as.character(disaster5_posttreat$contractvalue)))

disaster5_posttreat$buyer_buyerType<- as.factor(disaster5_posttreat$buyer_buyerType)
disaster5_posttreat$tender_mainCpv<- as.factor(disaster5_posttreat$tender_mainCpv)
disaster5_posttreat$contractyear<- as.factor(disaster5_posttreat$contractyear)

#Matching
vars <- c("treatcon","buyer_buyerType", "tender_mainCpv", "lot_bidsCount", "log_contractvalue", "meanlotbids", "contractyear")
temp5_1<-disaster5_posttreat[vars]
vars2<- c("buyer_buyerType", "tender_mainCpv", "log_contractvalue","contractyear")
temp5_1<- as.data.frame(temp5_1)
imbalance(group=temp5_1$treatcon, data=temp5_1[vars2])
summary(temp5_1$log_contractvalue)
valuecuts = c(14.34)
buyer_buyerType.grp<- list(c("REGIONAL_AUTHORITY", "REGIONAL_AGENCY", "UTILITIES"), c("NATIONAL_AUTHORITY","OTHER"), c("PUBLIC_BODY"), c("NA",NA))
mat5_1 <- cem(treatment = "treatcon", data = temp5_1, drop = "lot_bidsCount", grouping = list(buyer_buyerType= buyer_buyerType.grp))
mat5_1
est5_1 <- att(mat5_1, lot_bidsCount ~ treatcon, data = temp5_1)
est5_1
disaster5_posttreat$aftermatchtreat <- mat5_1$matched
disaster5_posttreat$aftermatchweight <- mat5_1$w
disaster5_posttreat_final <- disaster5_posttreat %>% filter(aftermatchtreat == TRUE)
disaster5_posttreat_final$timing <- 1
disaster5_matched <- rbind(as.data.frame(disaster5_pretreat_final), as.data.frame(disaster5_posttreat_final))
disaster5_matched$did <- disaster5_matched$treatcon*disaster5_matched$timing
disaster5_matched$matchsource <- "d5"

disaster5_did<- lm(lot_bidsCount ~ treatcon + timing + did, data = disaster5_matched, weights = aftermatchweight)
summary.lm(disaster5_did)

##Full matched data

dv1 <- rbind(disaster1_matched, disaster5_matched, disaster4_matched, disaster3_matched, disaster2_matched)

dv1_did<- lm(lot_bidsCount ~ treatcon + timing + did + buyer_buyerType + log_contractvalue + tender_mainCpv, data = dv1, weights = aftermatchweight)
summary.lm(dv1_did)
table(dv1$treatcon)
