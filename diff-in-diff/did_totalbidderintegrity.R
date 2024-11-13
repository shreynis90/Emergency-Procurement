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
library(modEvA)
library(weights)
library(gdata)

memory.limit(size = 30000)
gc()
italy_main<- read_excel("data_09_20.xlsx")
italy_main<-as.data.frame(italy_main)
keep(italy_main, sure = TRUE)
italy <- italy_main

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

italy<- italy %>% filter(!is.na(lot_bidsCount))
italy$totalbiddersintegrity <- ifelse(italy$lot_bidsCount == 1|italy$lot_bidsCount == 0, 1, 0)
italy$ID <- 1:nrow(italy)

##Disaster1 Analysis##----

##Converting to dataframe and Retaining first two digits of the CPV code
##Disaster 1 ----
italy_disaster1<- italy %>%
  filter(disnumber == "Disaster_001")
##

italy_disaster1 <- data.frame(italy_disaster1)
italy_disaster1$buyer_buyerType <- as.factor(italy_disaster1$buyer_buyerType) #Declaring buyer_buyerType as a factor
italy_disaster1$tender_mainCpv <- as.integer(italy_disaster1$tender_mainCpv)
italy_disaster1$tender_mainCpv <- sub("^(\\d{2}).*$", "\\1", italy_disaster1$tender_mainCpv) #Retaining only the first two digits of the the CPV code
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
  filter(!is.na(totalbiddersintegrity))

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
disaster1_pretreat <- disaster1_pretreat_1 %>% group_by(buyer_name) %>% mutate(meantotalbiddersintegrity = mean(totalbiddersintegrity)) #pretreament average of the number of bidders
common1_1<- cbind.data.frame(disaster1_pretreat$buyer_name, disaster1_pretreat$meantotalbiddersintegrity)
common1_1<- distinct(common1_1)
names(common1_1)[1]<-paste("buyer_name")
names(common1_1)[2]<-paste("meantotalbiddersintegrity")

disaster1_posttreat<- inner_join(common1_1, disaster1_posttreat0, by= "buyer_name")

disaster1_pretreat$contractyear <- as.factor(disaster1_pretreat$contractyear)
disaster1_pretreat$contractvalue <- ifelse(is.na(disaster1_pretreat$tender_finalPrice_EUR), ifelse(is.na(disaster1_pretreat$tender_estimatedPrice_EUR),"",disaster1_pretreat$tender_estimatedPrice_EUR),disaster1_pretreat$tender_finalPrice_EUR)
disaster1_pretreat$log_contractvalue <- log(as.numeric(as.character(disaster1_pretreat$contractvalue)))

disaster1_pretreat$buyer_buyerType<- as.factor(disaster1_pretreat$buyer_buyerType)
disaster1_pretreat$tender_mainCpv<- as.factor(disaster1_pretreat$tender_mainCpv)
disaster1_pretreat$contractyear<- as.factor(disaster1_pretreat$contractyear)

disaster1_posttreat$contractyear <- as.factor(disaster1_posttreat$contractyear)
disaster1_posttreat$contractvalue <- ifelse(is.na(disaster1_posttreat$tender_finalPrice_EUR), ifelse(is.na(disaster1_posttreat$tender_estimatedPrice_EUR),"",disaster1_posttreat$tender_estimatedPrice_EUR),disaster1_posttreat$tender_finalPrice_EUR)
disaster1_posttreat$log_contractvalue <- log(as.numeric(as.character(disaster1_posttreat$contractvalue)))

disaster1_posttreat$buyer_buyerType<- as.factor(disaster1_posttreat$buyer_buyerType)
disaster1_posttreat$tender_mainCpv<- as.factor(disaster1_posttreat$tender_mainCpv)
disaster1_posttreat$contractyear<- as.factor(disaster1_posttreat$contractyear)

disaster1_posttreat$timing <- 1
disaster1_pretreat$timing <- 0

disaster1 <- rbind(as.data.frame(disaster1_pretreat), as.data.frame(disaster1_posttreat))
#Matching
vars_bf <- c("ID","timing", "tender_mainCpv", "totalbiddersintegrity", "log_contractvalue", "meantotalbiddersintegrity","contractmonth", "buyer_buyerType")

temp1_0<-disaster1[vars_bf]
vars2_bf<- c("timing", "tender_mainCpv", "log_contractvalue", "meantotalbiddersintegrity","contractmonth","buyer_buyerType")
temp1_0<- as.data.frame(temp1_0)
#imbalance(group=temp1_0$timing, data=temp1_0[vars_bf])
summary(temp1_0$log_contractvalue)
valuecuts1_0 = c(12.13,13.06,14.22)
buyer_buyerType.grp<- list(c("REGIONAL_AUTHORITY"), c("REGIONAL_AGENCY", "UTILITIES"), c("NATIONAL_AUTHORITY"),c("OTHER"), c("PUBLIC_BODY"), c("NA",NA))
mat1_0 <- cem(treatment = "timing", data = temp1_0, drop = c("ID","totalbiddersintegrity"),cutpoints = list(log_contractvalue=valuecuts1_0), grouping = list(buyer_buyerType= buyer_buyerType.grp), eval.imbalance = TRUE)
mat1_0
disaster1$aftermatchtreat <- mat1_0$matched
disaster1$aftermatchweight <- mat1_0$w
disaster1_final <- disaster1 %>% filter(aftermatchtreat == TRUE)


##Disaster 1 DiD ----
disaster1_final$loc <- 1
italy_disaster1_out<- italy %>%
  filter(treatcon == 0)

italy_disaster1_out <- data.frame(italy_disaster1_out)
italy_disaster1_out$buyer_buyerType <- as.factor(italy_disaster1_out$buyer_buyerType) #Declaring buyer_buyerType as a factor
italy_disaster1_out$tender_mainCpv <- as.integer(italy_disaster1_out$tender_mainCpv)
italy_disaster1_out$tender_mainCpv <- sub("^(\\d{2}).*$", "\\1", italy_disaster1_out$tender_mainCpv) #Retaining only the first two digits of the the CPV code
italy_disaster1_out$tender_mainCpv <- as.integer(italy_disaster1_out$tender_mainCpv)


##Breaking up the dates to extract months and years
italy_disaster1_out$contractyear<- ifelse(is.na(italy_disaster1_out$tender_publications_firstCallForTenderDate), substring(italy_disaster1_out$tender_publications_firstdContractAwardDate,1,4),substring(italy_disaster1_out$tender_publications_firstCallForTenderDate,1,4)) #Contract year
italy_disaster1_out$contractmonth <- ifelse(is.na(italy_disaster1_out$tender_publications_firstCallForTenderDate), substring(italy_disaster1_out$tender_publications_firstdContractAwardDate,6,7),substring(italy_disaster1_out$tender_publications_firstCallForTenderDate,6,7)) #Contract month
italy_disaster1_out$contractday <- ifelse(is.na(italy_disaster1_out$tender_publications_firstCallForTenderDate), substring(italy_disaster1_out$tender_publications_firstdContractAwardDate,9,10),substring(italy_disaster1_out$tender_publications_firstCallForTenderDate,9,10)) #Contract month
italy_disaster1_out$contractmonth <- as.numeric(italy_disaster1_out$contractmonth)
italy_disaster1_out$contractyear <- as.numeric(italy_disaster1_out$contractyear)
italy_disaster1_out$contractday <- as.numeric(italy_disaster1_out$contractday)

italy_disaster1_out$contractdate_final <- paste(italy_disaster1_out$contractyear, italy_disaster1_out$contractmonth, italy_disaster1_out$contractday, sep="-")
italy_disaster1_out$contractdate_final <- as.POSIXct(italy_disaster1_out$contractdate_final)
italy_disaster1_out$contractvalue <- ifelse(is.na(italy_disaster1_out$tender_finalPrice_EUR), ifelse(is.na(italy_disaster1_out$tender_estimatedPrice_EUR),"",italy_disaster1_out$tender_estimatedPrice_EUR),italy_disaster1_out$tender_finalPrice_EUR)
italy_disaster1_out$log_contractvalue <- log(as.numeric(as.character(italy_disaster1_out$contractvalue)))



##
disaster1_date <- as.POSIXct("2009-10-02")

##Dropping those that did not have firstcalldate or tenderbiddeadline before 2011
italy_disaster1_out <- subset(italy_disaster1_out, !(is.na(tender_publications_firstCallForTenderDate) & contractyear<2011)) 
italy_disaster1_out$buyer_buyerType<- as.factor(italy_disaster1_out$buyer_buyerType)
italy_disaster1_out$tender_mainCpv<- as.factor(italy_disaster1_out$tender_mainCpv)
italy_disaster1_out$contractyear<- as.factor(italy_disaster1_out$contractyear)
italy_disaster1_out$contractmonth<- as.factor(italy_disaster1_out$contractmonth)

italy_disaster1_out <- italy_disaster1_out %>% #Dropping the Contracts that are missing the number of bidders 
  filter(!is.na(totalbiddersintegrity))

## Disaster 1_out Pretreatment ----

disaster1_out_pretreat0 <-  italy_disaster1_out %>% filter(disaster1_date - as.difftime(1, unit="days") > italy_disaster1_out$contractdate_final)
disaster1_out_pretreat0$timing <-0
disaster1_out_pretreat0$loc <- 0

disaster1_out_posttreat0 <- anti_join(italy_disaster1_out, disaster1_out_pretreat0)
disaster1_out_posttreat0$timing<- 1
disaster1_out_posttreat0$loc<- 0

##Disaster1 pre-treatment average of DV out----
d1name_out<-as.data.frame(disaster1_out_pretreat0$buyer_name)
d1name_out <- distinct(d1name_out) 

d1namepost_out<-as.data.frame(disaster1_out_posttreat0$buyer_name)
d1namepost_out <- distinct(d1namepost_out)

names(d1name_out)[1]<-paste("buyer_name")
names(d1namepost_out)[1]<-paste("buyer_name")

common1_0_out <- intersect(d1name_out, d1namepost_out)

disaster1_out_pretreat_1<- inner_join(common1_0_out, disaster1_out_pretreat0, by= "buyer_name")

disaster1_out_pretreat <- disaster1_out_pretreat_1 %>% group_by(buyer_name) %>% mutate(meantotalbiddersintegrity = mean(totalbiddersintegrity)) #pretreament average of the number of bidders
common1_1_out<- cbind.data.frame(disaster1_out_pretreat$buyer_name, disaster1_out_pretreat$meantotalbiddersintegrity)
common1_1_out<- distinct(common1_1_out)
names(common1_1_out)[1]<-paste("buyer_name")
names(common1_1_out)[2]<-paste("meantotalbiddersintegrity")

disaster1_out_posttreat<- inner_join(common1_1_out, disaster1_out_posttreat0, by= "buyer_name")

##----


disaster1_in_pretreat0 <- disaster1_final %>% filter(timing == 0)
disaster1_in_pretreat = subset(disaster1_in_pretreat0, select = -c(aftermatchweight,aftermatchtreat))
disaster1_in_pretreat$contractmonth <- as.factor(disaster1_in_pretreat$contractmonth)
disaster1_in_out_pretreat <- rbind(disaster1_out_pretreat,disaster1_in_pretreat) 

vars <- c("ID","loc", "tender_mainCpv", "totalbiddersintegrity", "log_contractvalue","buyer_buyerType","contractyear","contractmonth")
temp1_0_did<-disaster1_in_out_pretreat[vars]
vars2<- c("loc", "tender_mainCpv", "log_contractvalue","contractmonth","buyer_buyerType","meantotalbiddersintegrity")
temp1_0_did<- as.data.frame(temp1_0_did)
#imbalance(group=temp1_0_did$loc, data=temp1_0_did[vars2])
summary(temp1_0_did$log_contractvalue)
summary(temp1_0_did$meantotalbiddersintegrity)
valuecuts1_0_did = c(15)
mat1_0_did <- cem(treatment = "loc", data = temp1_0_did, drop = c("ID","totalbiddersintegrity"),cutpoints = list(log_contractvalue=valuecuts1_0_did), eval.imbalance = TRUE)
mat1_0_did
disaster1_in_out_pretreat$aftermatchtreat <- mat1_0_did$matched
disaster1_in_out_pretreat$aftermatchweight <- mat1_0_did$w
disaster1_did_0 <- disaster1_in_out_pretreat %>% filter(aftermatchtreat == TRUE)

##Disaster 1_Postreatment ----

disaster1_in_posttreat0 <- disaster1_final %>% filter(timing == 1)
disaster1_in_posttreat = subset(disaster1_in_posttreat0, select = -c(aftermatchweight,aftermatchtreat) )

disaster1_in_out_posttreat <- rbind(disaster1_out_posttreat, disaster1_in_posttreat)


temp1_1_did<-disaster1_in_out_posttreat[vars]
temp1_1_did<- as.data.frame(temp1_1_did)
#imbalance(group=temp1_1_did$loc, data=temp1_1_did[vars2])
summary(temp1_1_did$log_contractvalue)
summary(temp1_1_did$meantotalbiddersintegrity)
valuecuts1_1_did = c(13.319,14.331)
mat1_1_did <- cem(treatment = "loc", data = temp1_1_did, drop = c("ID","totalbiddersintegrity"),cutpoints = list(log_contractvalue=valuecuts1_1_did), eval.imbalance = TRUE)
mat1_1_did

disaster1_in_out_posttreat$aftermatchtreat <- mat1_1_did$matched
disaster1_in_out_posttreat$aftermatchweight <- mat1_1_did$w
disaster1_did_1 <- disaster1_in_out_posttreat %>% filter(aftermatchtreat == TRUE)

disaster1_did_final <- rbind(disaster1_did_0,disaster1_did_1)



##Disaster 2 ----
italy_disaster2<- italy %>%
  filter(disnumber == "Disaster_002")
##

italy_disaster2 <- data.frame(italy_disaster2)
italy_disaster2$buyer_buyerType <- as.factor(italy_disaster2$buyer_buyerType) #Declaring buyer_buyerType as a factor
italy_disaster2$tender_mainCpv <- as.integer(italy_disaster2$tender_mainCpv)
italy_disaster2$tender_mainCpv <- sub("^(\\d{2}).*$", "\\1", italy_disaster2$tender_mainCpv) #Retaining only the first three digits of the the CPV code
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
italy_disaster2 <- italy_disaster2 %>% #Dropping the Contracts that are missing the totalbiddersintegrity
  filter(!is.na(totalbiddersintegrity))
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
disaster2_pretreat <- disaster2_pretreat_1 %>% group_by(buyer_name) %>% mutate(meantotalbiddersintegrity = mean(totalbiddersintegrity)) #pretreament average of the number of bidders
common2_1<- cbind.data.frame(disaster2_pretreat$buyer_name, disaster2_pretreat$meantotalbiddersintegrity)
common2_1<- distinct(common2_1)
names(common2_1)[1]<-paste("buyer_name")
names(common2_1)[2]<-paste("meantotalbiddersintegrity")

disaster2_posttreat<- inner_join(common2_1, disaster2_posttreat0, by= "buyer_name")


disaster2_pretreat$contractyear <- as.factor(disaster2_pretreat$contractyear)
disaster2_pretreat$contractvalue <- ifelse(is.na(disaster2_pretreat$tender_finalPrice_EUR), ifelse(is.na(disaster2_pretreat$tender_estimatedPrice_EUR),"",disaster2_pretreat$tender_estimatedPrice_EUR),disaster2_pretreat$tender_finalPrice_EUR)
disaster2_pretreat$log_contractvalue <- log(as.numeric(as.character(disaster2_pretreat$contractvalue)))

disaster2_pretreat$buyer_buyerType<- as.factor(disaster2_pretreat$buyer_buyerType)
disaster2_pretreat$tender_mainCpv<- as.factor(disaster2_pretreat$tender_mainCpv)
disaster2_pretreat$contractyear<- as.factor(disaster2_pretreat$contractyear)

disaster2_posttreat$contractyear <- as.factor(disaster2_posttreat$contractyear)
disaster2_posttreat$contractvalue <- ifelse(is.na(disaster2_posttreat$tender_finalPrice_EUR), ifelse(is.na(disaster2_posttreat$tender_estimatedPrice_EUR),"",disaster2_posttreat$tender_estimatedPrice_EUR),disaster2_posttreat$tender_finalPrice_EUR)
disaster2_posttreat$log_contractvalue <- log(as.numeric(as.character(disaster2_posttreat$contractvalue)))

disaster2_posttreat$buyer_buyerType<- as.factor(disaster2_posttreat$buyer_buyerType)
disaster2_posttreat$tender_mainCpv<- as.factor(disaster2_posttreat$tender_mainCpv)
disaster2_posttreat$contractyear<- as.factor(disaster2_posttreat$contractyear)

disaster2_posttreat$timing <- 1
disaster2_pretreat$timing <- 0

disaster2 <- rbind(as.data.frame(disaster2_pretreat), as.data.frame(disaster2_posttreat))


#Matching

temp2_0<-disaster2[vars_bf]
temp2_0<- as.data.frame(temp2_0)
#imbalance(group=temp2_0$timing, data=temp2_0[vars2_bf])
summary(temp2_0$log_contractvalue)
valuecuts2_0 = c(13.18,14.34,15.62,14)
mat2_0 <- cem(treatment = "timing", data = temp2_0, drop = c("ID","totalbiddersintegrity"), eval.imbalance = TRUE)
mat2_0
disaster2$aftermatchtreat <- mat2_0$matched
disaster2$aftermatchweight <- mat2_0$w
disaster2_final <- disaster2 %>% filter(aftermatchtreat == TRUE)

##Disaster 2 DiD ----
disaster2_final$loc <- 1
italy_disaster2_out<- italy %>%
  filter(treatcon == 0)

italy_disaster2_out <- data.frame(italy_disaster2_out)
italy_disaster2_out$buyer_buyerType <- as.factor(italy_disaster2_out$buyer_buyerType) #Declaring buyer_buyerType as a factor
italy_disaster2_out$tender_mainCpv <- as.integer(italy_disaster2_out$tender_mainCpv)
italy_disaster2_out$tender_mainCpv <- sub("^(\\d{2}).*$", "\\1", italy_disaster2_out$tender_mainCpv) #Retaining only the first two digits of the the CPV code
italy_disaster2_out$tender_mainCpv <- as.integer(italy_disaster2_out$tender_mainCpv)
italy_disaster2_out$tender_mainCpv

##Breaking up the dates to extract months and years
italy_disaster2_out$contractyear<- ifelse(is.na(italy_disaster2_out$tender_publications_firstCallForTenderDate), substring(italy_disaster2_out$tender_publications_firstdContractAwardDate,1,4),substring(italy_disaster2_out$tender_publications_firstCallForTenderDate,1,4)) #Contract year
italy_disaster2_out$contractmonth <- ifelse(is.na(italy_disaster2_out$tender_publications_firstCallForTenderDate), substring(italy_disaster2_out$tender_publications_firstdContractAwardDate,6,7),substring(italy_disaster2_out$tender_publications_firstCallForTenderDate,6,7)) #Contract month
italy_disaster2_out$contractday <- ifelse(is.na(italy_disaster2_out$tender_publications_firstCallForTenderDate), substring(italy_disaster2_out$tender_publications_firstdContractAwardDate,9,10),substring(italy_disaster2_out$tender_publications_firstCallForTenderDate,9,10)) #Contract month
italy_disaster2_out$contractmonth <- as.numeric(italy_disaster2_out$contractmonth)
italy_disaster2_out$contractyear <- as.numeric(italy_disaster2_out$contractyear)
italy_disaster2_out$contractday <- as.numeric(italy_disaster2_out$contractday)

italy_disaster2_out$contractdate_final <- paste(italy_disaster2_out$contractyear, italy_disaster2_out$contractmonth, italy_disaster2_out$contractday, sep="-")
italy_disaster2_out$contractdate_final <- as.POSIXct(italy_disaster2_out$contractdate_final)
italy_disaster2_out$contractvalue <- ifelse(is.na(italy_disaster2_out$tender_finalPrice_EUR), ifelse(is.na(italy_disaster2_out$tender_estimatedPrice_EUR),"",italy_disaster2_out$tender_estimatedPrice_EUR),italy_disaster2_out$tender_finalPrice_EUR)
italy_disaster2_out$log_contractvalue <- log(as.numeric(as.character(italy_disaster2_out$contractvalue)))



##
disaster2_date <- as.POSIXct("2012-05-29")

#Dropping those that did not have firstcalldate or tenderbiddeadline before 2011
italy_disaster2_out <- subset(italy_disaster2_out, !(is.na(tender_publications_firstCallForTenderDate) & contractyear<2011)) 
italy_disaster2_out$buyer_buyerType<- as.factor(italy_disaster2_out$buyer_buyerType)
italy_disaster2_out$tender_mainCpv<- as.factor(italy_disaster2_out$tender_mainCpv)
italy_disaster2_out$contractyear<- as.factor(italy_disaster2_out$contractyear)
italy_disaster2_out$contractmonth<- as.factor(italy_disaster2_out$contractmonth)

italy_disaster2_out <- italy_disaster2_out %>% #Dropping the Contracts that are missing the number of bidders 
  filter(!is.na(totalbiddersintegrity))

## Disaster 2_out Pretreatment ----

disaster2_out_pretreat0 <-  italy_disaster2_out %>% filter(disaster2_date > italy_disaster2_out$contractdate_final)
disaster2_out_pretreat0$timing <-0
disaster2_out_pretreat0$loc <- 0

disaster2_out_posttreat0 <- anti_join(italy_disaster2_out, disaster2_out_pretreat0)
disaster2_out_posttreat0$timing<- 1
disaster2_out_posttreat0$loc<- 0
##Disaster 2_out pretreatment average of DV----
d2name_out<-as.data.frame(disaster2_out_pretreat0$buyer_name)
d2name_out <- distinct(d2name_out) 

d2namepost_out<-as.data.frame(disaster2_out_posttreat0$buyer_name)
d2namepost_out <- distinct(d2namepost_out)

names(d2name_out)[1]<-paste("buyer_name")
names(d2namepost_out)[1]<-paste("buyer_name")

common2_0_out <- intersect(d2name_out, d2namepost_out)

disaster2_out_pretreat_1<- inner_join(common2_0_out, disaster2_out_pretreat0, by= "buyer_name")

disaster2_out_pretreat <- disaster2_out_pretreat_1 %>% group_by(buyer_name) %>% mutate(meantotalbiddersintegrity = mean(totalbiddersintegrity)) #pretreament average of the number of bidders
common2_1_out<- cbind.data.frame(disaster2_out_pretreat$buyer_name, disaster2_out_pretreat$meantotalbiddersintegrity)
common2_1_out<- distinct(common2_1_out)
names(common2_1_out)[1]<-paste("buyer_name")
names(common2_1_out)[2]<-paste("meantotalbiddersintegrity")

disaster2_out_posttreat<- inner_join(common2_1_out, disaster2_out_posttreat0, by= "buyer_name")

##----



disaster2_in_pretreat0 <- disaster2_final %>% filter(timing == 0)
disaster2_in_pretreat = subset(disaster2_in_pretreat0, select = -c(aftermatchweight,aftermatchtreat) )
disaster2_in_pretreat$contractmonth <- as.factor(disaster2_in_pretreat$contractmonth)

disaster2_in_out_pretreat <- rbind(disaster2_out_pretreat,disaster2_in_pretreat) 


temp2_0_did<-disaster2_in_out_pretreat[vars]
temp2_0_did<- as.data.frame(temp2_0_did)
#imbalance(group=temp2_0_did$loc, data=temp2_0_did[vars_x])
summary(temp2_0_did$log_contractvalue)
summary(temp2_0_did$meantotalbiddersintegrity)
valuecuts2_0_did = c(13.265, 14.294,12,11)
mat2_0_did <- cem(treatment = "loc", data = temp2_0_did, drop = c("ID","totalbiddersintegrity"),cutpoints = list(log_contractvalue = valuecuts2_0_did),eval.imbalance = TRUE)
mat2_0_did


disaster2_in_out_pretreat$aftermatchtreat <- mat2_0_did$matched
disaster2_in_out_pretreat$aftermatchweight <- mat2_0_did$w
disaster2_did_0 <- disaster2_in_out_pretreat %>% filter(aftermatchtreat == TRUE)

dis2_mean_in <- disaster2_did_0 %>% filter(loc == 1)
dis2_mean_out <- disaster2_did_0 %>% filter(loc == 0)

t.test(dis2_mean_in$totalbiddersintegrity,dis2_mean_out$totalbiddersintegrity, paired = FALSE, conf.level = 0.90)

##Disaster 2_Postreatment ----

disaster2_in_posttreat0 <- disaster2_final %>% filter(timing == 1)
disaster2_in_posttreat = subset(disaster2_in_posttreat0, select = -c(aftermatchweight,aftermatchtreat) )

disaster2_in_out_posttreat <- rbind(disaster2_out_posttreat, disaster2_in_posttreat)


temp2_1_did<-disaster2_in_out_posttreat[vars]
temp2_1_did<- as.data.frame(temp2_1_did)
#imbalance(group=temp2_1_did$loc, data=temp2_1_did[vars2])
summary(temp2_1_did$log_contractvalue)
valuecuts2_1_did = c(13.46,14.58)
mat2_1_did <- cem(treatment = "loc", data = temp2_1_did, drop = c("ID","totalbiddersintegrity"),cutpoints = list(log_contractvalue=valuecuts2_1_did), eval.imbalance = TRUE)
mat2_1_did

disaster2_in_out_posttreat$aftermatchtreat <- mat2_1_did$matched
disaster2_in_out_posttreat$aftermatchweight <- mat2_1_did$w
disaster2_did_1 <- disaster2_in_out_posttreat %>% filter(aftermatchtreat == TRUE)

disaster2_did_final <- rbind(disaster2_did_0,disaster2_did_1)

##Disaster 3
italy_disaster3<- italy %>%
  filter(disnumber == "Disaster_003")
##

italy_disaster3 <- data.frame(italy_disaster3)
italy_disaster3$buyer_buyerType <- as.factor(italy_disaster3$buyer_buyerType) #Declaring buyer_buyerType as a factor
italy_disaster3$tender_mainCpv <- as.integer(italy_disaster3$tender_mainCpv)
italy_disaster3$tender_mainCpv <- sub("^(\\d{2}).*$", "\\1", italy_disaster3$tender_mainCpv) #Retaining only the first three digits of the the CPV code
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
  filter(!is.na(totalbiddersintegrity))

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
disaster3_pretreat <- disaster3_pretreat_1 %>% group_by(buyer_name) %>% mutate(meantotalbiddersintegrity = mean(totalbiddersintegrity)) #pretreament average of the number of bidders
common3_1<- cbind.data.frame(disaster3_pretreat$buyer_name, disaster3_pretreat$meantotalbiddersintegrity)
common3_1<- distinct(common3_1)
names(common3_1)[1]<-paste("buyer_name")
names(common3_1)[2]<-paste("meantotalbiddersintegrity")

disaster3_posttreat<- inner_join(common3_1, disaster3_posttreat0, by= "buyer_name")


disaster3_pretreat$contractyear <- as.factor(disaster3_pretreat$contractyear)
disaster3_pretreat$contractvalue <- ifelse(is.na(disaster3_pretreat$tender_finalPrice_EUR), ifelse(is.na(disaster3_pretreat$tender_estimatedPrice_EUR),"",disaster3_pretreat$tender_estimatedPrice_EUR),disaster3_pretreat$tender_finalPrice_EUR)
disaster3_pretreat$log_contractvalue <- log(as.numeric(as.character(disaster3_pretreat$contractvalue)))

disaster3_pretreat$buyer_buyerType<- as.factor(disaster3_pretreat$buyer_buyerType)
disaster3_pretreat$tender_mainCpv<- as.factor(disaster3_pretreat$tender_mainCpv)
disaster3_pretreat$contractyear<- as.factor(disaster3_pretreat$contractyear)

disaster3_posttreat$contractyear <- as.factor(disaster3_posttreat$contractyear)
disaster3_posttreat$contractvalue <- ifelse(is.na(disaster3_posttreat$tender_finalPrice_EUR), ifelse(is.na(disaster3_posttreat$tender_estimatedPrice_EUR),"",disaster3_posttreat$tender_estimatedPrice_EUR),disaster3_posttreat$tender_finalPrice_EUR)
disaster3_posttreat$log_contractvalue <- log(as.numeric(as.character(disaster3_posttreat$contractvalue)))

disaster3_posttreat$buyer_buyerType<- as.factor(disaster3_posttreat$buyer_buyerType)
disaster3_posttreat$tender_mainCpv<- as.factor(disaster3_posttreat$tender_mainCpv)
disaster3_posttreat$contractyear<- as.factor(disaster3_posttreat$contractyear)

disaster3_posttreat$timing <- 1
disaster3_pretreat$timing <- 0

disaster3 <- rbind(as.data.frame(disaster3_pretreat), as.data.frame(disaster3_posttreat))



#Matching

temp3_0<-disaster3[vars_bf]
temp3_0<- as.data.frame(temp3_0)
summary(temp3_0$log_contractvalue)
valuecuts3_0 <- c(13.10,13.97, 14.79)
mat3_0 <- cem(treatment = "timing", data = temp3_0, drop = c("ID","totalbiddersintegrity"),cutpoints = list(log_contractvalue=valuecuts3_0), grouping = list(buyer_buyerType= buyer_buyerType.grp), eval.imbalance = TRUE)
mat3_0

disaster3$aftermatchtreat <- mat3_0$matched
disaster3$aftermatchweight <- mat3_0$w
disaster3_final <- disaster3 %>% filter(aftermatchtreat == TRUE)

##Disaster 3 DiD ----
disaster3_final$loc <- 1
italy_disaster3_out<- italy %>%
  filter(treatcon == 0)

italy_disaster3_out <- data.frame(italy_disaster3_out)
italy_disaster3_out$buyer_buyerType <- as.factor(italy_disaster3_out$buyer_buyerType) #Declaring buyer_buyerType as a factor
italy_disaster3_out$tender_mainCpv <- as.integer(italy_disaster3_out$tender_mainCpv)
italy_disaster3_out$tender_mainCpv <- sub("^(\\d{2}).*$", "\\1", italy_disaster3_out$tender_mainCpv) #Retaining only the first two digits of the the CPV code
italy_disaster3_out$tender_mainCpv <- as.integer(italy_disaster3_out$tender_mainCpv)
italy_disaster3_out$tender_mainCpv

##Breaking up the dates to extract months and years
italy_disaster3_out$contractyear<- ifelse(is.na(italy_disaster3_out$tender_publications_firstCallForTenderDate), substring(italy_disaster3_out$tender_publications_firstdContractAwardDate,1,4),substring(italy_disaster3_out$tender_publications_firstCallForTenderDate,1,4)) #Contract year
italy_disaster3_out$contractmonth <- ifelse(is.na(italy_disaster3_out$tender_publications_firstCallForTenderDate), substring(italy_disaster3_out$tender_publications_firstdContractAwardDate,6,7),substring(italy_disaster3_out$tender_publications_firstCallForTenderDate,6,7)) #Contract month
italy_disaster3_out$contractday <- ifelse(is.na(italy_disaster3_out$tender_publications_firstCallForTenderDate), substring(italy_disaster3_out$tender_publications_firstdContractAwardDate,9,10),substring(italy_disaster3_out$tender_publications_firstCallForTenderDate,9,10)) #Contract month
italy_disaster3_out$contractmonth <- as.numeric(italy_disaster3_out$contractmonth)
italy_disaster3_out$contractyear <- as.numeric(italy_disaster3_out$contractyear)
italy_disaster3_out$contractday <- as.numeric(italy_disaster3_out$contractday)

italy_disaster3_out$contractdate_final <- paste(italy_disaster3_out$contractyear, italy_disaster3_out$contractmonth, italy_disaster3_out$contractday, sep="-")
italy_disaster3_out$contractdate_final <- as.POSIXct(italy_disaster3_out$contractdate_final)
italy_disaster3_out$contractvalue <- ifelse(is.na(italy_disaster3_out$tender_finalPrice_EUR), ifelse(is.na(italy_disaster3_out$tender_estimatedPrice_EUR),"",italy_disaster3_out$tender_estimatedPrice_EUR),italy_disaster3_out$tender_finalPrice_EUR)
italy_disaster3_out$log_contractvalue <- log(as.numeric(as.character(italy_disaster3_out$contractvalue)))



##
disaster3_date <- as.POSIXct("2013-11-18")

#Dropping those that did not have firstcalldate or tenderbiddeadline before 2011
italy_disaster3_out <- subset(italy_disaster3_out, !(is.na(tender_publications_firstCallForTenderDate) & contractyear<2011)) 
italy_disaster3_out$buyer_buyerType<- as.factor(italy_disaster3_out$buyer_buyerType)
italy_disaster3_out$tender_mainCpv<- as.factor(italy_disaster3_out$tender_mainCpv)
italy_disaster3_out$contractyear<- as.factor(italy_disaster3_out$contractyear)
italy_disaster3_out$contractmonth<- as.factor(italy_disaster3_out$contractmonth)
italy_disaster3_out$contractday<- as.factor(italy_disaster3_out$contractday)
italy_disaster3_out <- italy_disaster3_out %>% #Dropping the Contracts that are missing the number of bidders 
  filter(!is.na(totalbiddersintegrity))

## Disaster 3_out Pretreatment ----

disaster3_out_pretreat0 <-  italy_disaster3_out %>% filter(disaster3_date > italy_disaster3_out$contractdate_final)
disaster3_out_pretreat0$timing <-0
disaster3_out_pretreat0$loc <- 0

disaster3_out_posttreat0 <- anti_join(italy_disaster3_out, disaster3_out_pretreat0)
disaster3_out_posttreat0$timing<- 1
disaster3_out_posttreat0$loc<- 0

##Disaster 2_out pretreatment average of DV----
d3name_out<-as.data.frame(disaster3_out_pretreat0$buyer_name)
d3name_out <- distinct(d3name_out) 

d3namepost_out<-as.data.frame(disaster3_out_posttreat0$buyer_name)
d3namepost_out <- distinct(d3namepost_out)

names(d3name_out)[1]<-paste("buyer_name")
names(d3namepost_out)[1]<-paste("buyer_name")

common3_0_out <- intersect(d3name_out, d3namepost_out)

disaster3_out_pretreat_1<- inner_join(common3_0_out, disaster3_out_pretreat0, by= "buyer_name")

disaster3_out_pretreat <- disaster3_out_pretreat_1 %>% group_by(buyer_name) %>% mutate(meantotalbiddersintegrity = mean(totalbiddersintegrity)) #pretreament average of the number of bidders
common3_1_out<- cbind.data.frame(disaster3_out_pretreat$buyer_name, disaster3_out_pretreat$meantotalbiddersintegrity)
common3_1_out<- distinct(common3_1_out)
names(common3_1_out)[1]<-paste("buyer_name")
names(common3_1_out)[2]<-paste("meantotalbiddersintegrity")

disaster3_out_posttreat<- inner_join(common3_1_out, disaster3_out_posttreat0, by= "buyer_name")


disaster3_in_pretreat0 <- disaster3_final %>% filter(timing == 0)
disaster3_in_pretreat = subset(disaster3_in_pretreat0, select = -c(aftermatchweight,aftermatchtreat) )
disaster3_in_pretreat$contractmonth <- as.factor(disaster3_in_pretreat$contractmonth)
disaster3_in_pretreat$contractday <- as.factor(disaster3_in_pretreat$contractday)

disaster3_in_out_pretreat <- rbind(disaster3_out_pretreat,disaster3_in_pretreat) 

temp3_0_did<-disaster3_in_out_pretreat[vars]
temp3_0_did<- as.data.frame(temp3_0_did)
summary(temp3_0_did$log_contractvalue)
valuecuts3_0_did = c(18)
month.grp <- list(c("1"),c("2"),c("3","4"),c("5","6"), c("7"),c( "8"),c("9","10"),c( "11","12","NA",NA))
buyer_buyerType.grp<- list(c("REGIONAL_AUTHORITY", "REGIONAL_AGENCY"),c("UTILITIES","NATIONAL_AUTHORITY"), c("OTHER"),c("PUBLIC_BODY","NA"))
mat3_0_did <- cem(treatment = "loc", data = temp3_0_did, drop = c("ID","totalbiddersintegrity"),cutpoints = list(log_contractvalue=valuecuts3_0_did), grouping= list(contractmonth = month.grp, buyer_buyerType = buyer_buyerType.grp), eval.imbalance = TRUE)
mat3_0_did

disaster3_in_out_pretreat$aftermatchtreat <- mat3_0_did$matched
disaster3_in_out_pretreat$aftermatchweight <- mat3_0_did$w
disaster3_did_0 <- disaster3_in_out_pretreat %>% filter(aftermatchtreat == TRUE)

dis3_mean_in <- disaster3_did_0 %>% filter(loc == 1)
dis3_mean_out <- disaster3_did_0 %>% filter(loc == 0)

t.test(dis3_mean_in$totalbiddersintegrity,dis3_mean_out$totalbiddersintegrity, paired = FALSE, conf.level = 0.90)

##Disaster 3_Postreatment ----

disaster3_in_posttreat0 <- disaster3_final %>% filter(timing == 1)
disaster3_in_posttreat = subset(disaster3_in_posttreat0, select = -c(aftermatchweight,aftermatchtreat) )

disaster3_in_out_posttreat <- rbind(disaster3_out_posttreat, disaster3_in_posttreat)
temp3_1_did<-disaster3_in_out_posttreat[vars]
temp3_1_did<- as.data.frame(temp3_1_did)
summary(temp3_1_did$log_contractvalue)
valuecuts3_1_did = c(13.44,14.58)
mat3_1_did <- cem(treatment = "loc", data = temp3_1_did, drop = c("ID","totalbiddersintegrity"),cutpoints = list(log_contractvalue=valuecuts3_1_did), eval.imbalance = TRUE)
mat3_1_did


disaster3_in_out_posttreat$aftermatchtreat <- mat3_1_did$matched
disaster3_in_out_posttreat$aftermatchweight <- mat3_1_did$w
disaster3_did_1 <- disaster3_in_out_posttreat %>% filter(aftermatchtreat == TRUE)

disaster3_did_final <- rbind(disaster3_did_0,disaster3_did_1)


##Disaster 4 ----
italy_disaster4<- italy %>%
  filter(disnumber == "Disaster_004")
##

italy_disaster4 <- data.frame(italy_disaster4)
italy_disaster4$buyer_buyerType <- as.factor(italy_disaster4$buyer_buyerType) #Declaring buyer_buyerType as a factor
italy_disaster4$tender_mainCpv <- as.integer(italy_disaster4$tender_mainCpv)
italy_disaster4$tender_mainCpv <- sub("^(\\d{2}).*$", "\\1", italy_disaster4$tender_mainCpv) #Retaining only the first three digits of the the CPV code
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
  filter(!is.na(totalbiddersintegrity))
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
disaster4_pretreat <- disaster4_pretreat_1 %>% group_by(buyer_name) %>% mutate(meantotalbiddersintegrity = mean(totalbiddersintegrity)) #pretreament average of the number of bidders
common4_1<- cbind.data.frame(disaster4_pretreat$buyer_name, disaster4_pretreat$meantotalbiddersintegrity)
common4_1<- distinct(common4_1)
names(common4_1)[1]<-paste("buyer_name")
names(common4_1)[2]<-paste("meantotalbiddersintegrity")

disaster4_posttreat<- inner_join(common4_1, disaster4_posttreat0, by= "buyer_name")


disaster4_pretreat$contractyear <- as.factor(disaster4_pretreat$contractyear)
disaster4_pretreat$contractvalue <- ifelse(is.na(disaster4_pretreat$tender_finalPrice_EUR), ifelse(is.na(disaster4_pretreat$tender_estimatedPrice_EUR),"",disaster4_pretreat$tender_estimatedPrice_EUR),disaster4_pretreat$tender_finalPrice_EUR)
disaster4_pretreat$log_contractvalue <- log(as.numeric(as.character(disaster4_pretreat$contractvalue)))

disaster4_pretreat$buyer_buyerType<- as.factor(disaster4_pretreat$buyer_buyerType)
disaster4_pretreat$tender_mainCpv<- as.factor(disaster4_pretreat$tender_mainCpv)
disaster4_pretreat$contractyear<- as.factor(disaster4_pretreat$contractyear)

disaster4_posttreat$contractyear <- as.factor(disaster4_posttreat$contractyear)
disaster4_posttreat$contractvalue <- ifelse(is.na(disaster4_posttreat$tender_finalPrice_EUR), ifelse(is.na(disaster4_posttreat$tender_estimatedPrice_EUR),"",disaster4_posttreat$tender_estimatedPrice_EUR),disaster4_posttreat$tender_finalPrice_EUR)
disaster4_posttreat$log_contractvalue <- log(as.numeric(as.character(disaster4_posttreat$contractvalue)))

disaster4_posttreat$buyer_buyerType<- as.factor(disaster4_posttreat$buyer_buyerType)
disaster4_posttreat$tender_mainCpv<- as.factor(disaster4_posttreat$tender_mainCpv)
disaster4_posttreat$contractyear<- as.factor(disaster4_posttreat$contractyear)

disaster4_posttreat$timing <- 1
disaster4_pretreat$timing <- 0

disaster4 <- rbind(as.data.frame(disaster4_pretreat), as.data.frame(disaster4_posttreat))

#Matching

temp4_0<-disaster4[vars_bf]
temp4_0<- as.data.frame(temp4_0)
summary(temp4_0$log_contractvalue)
summary(temp4_0$meantotalbiddersintegrity)
valuecuts4_0 = c(12.81,13.60, 14.12)
buyer_buyerType.grp<- list(c("REGIONAL_AUTHORITY", "REGIONAL_AGENCY", "UTILITIES"), c("NATIONAL_AUTHORITY"),c("OTHER"), c("PUBLIC_BODY"), c("NA",NA))
mat4_0 <- cem(treatment = "timing", data = temp4_0, drop = c("ID","totalbiddersintegrity"),cutpoints = list(log_contractvalue=valuecuts4_0), grouping = list(buyer_buyerType= buyer_buyerType.grp), eval.imbalance = TRUE)
mat4_0
mat4_0
disaster4$aftermatchtreat <- mat4_0$matched
disaster4$aftermatchweight <- mat4_0$w
disaster4_final <- disaster4 %>% filter(aftermatchtreat == TRUE)

##Disaster 4 DiD ----
disaster4_final$loc <- 1
italy_disaster4_out<- italy %>%
  filter(treatcon == 0)

italy_disaster4_out <- data.frame(italy_disaster4_out)
italy_disaster4_out$buyer_buyerType <- as.factor(italy_disaster4_out$buyer_buyerType) #Declaring buyer_buyerType as a factor
italy_disaster4_out$tender_mainCpv <- as.integer(italy_disaster4_out$tender_mainCpv)
italy_disaster4_out$tender_mainCpv <- sub("^(\\d{2}).*$", "\\1", italy_disaster4_out$tender_mainCpv) #Retaining only the first two digits of the the CPV code
italy_disaster4_out$tender_mainCpv <- as.integer(italy_disaster4_out$tender_mainCpv)
italy_disaster4_out$tender_mainCpv

##Breaking up the dates to extract months and years
italy_disaster4_out$contractyear<- ifelse(is.na(italy_disaster4_out$tender_publications_firstCallForTenderDate), substring(italy_disaster4_out$tender_publications_firstdContractAwardDate,1,4),substring(italy_disaster4_out$tender_publications_firstCallForTenderDate,1,4)) #Contract year
italy_disaster4_out$contractmonth <- ifelse(is.na(italy_disaster4_out$tender_publications_firstCallForTenderDate), substring(italy_disaster4_out$tender_publications_firstdContractAwardDate,6,7),substring(italy_disaster4_out$tender_publications_firstCallForTenderDate,6,7)) #Contract month
italy_disaster4_out$contractday <- ifelse(is.na(italy_disaster4_out$tender_publications_firstCallForTenderDate), substring(italy_disaster4_out$tender_publications_firstdContractAwardDate,9,10),substring(italy_disaster4_out$tender_publications_firstCallForTenderDate,9,10)) #Contract month
italy_disaster4_out$contractmonth <- as.numeric(italy_disaster4_out$contractmonth)
italy_disaster4_out$contractyear <- as.numeric(italy_disaster4_out$contractyear)
italy_disaster4_out$contractday <- as.numeric(italy_disaster4_out$contractday)

italy_disaster4_out$contractdate_final <- paste(italy_disaster4_out$contractyear, italy_disaster4_out$contractmonth, italy_disaster4_out$contractday, sep="-")
italy_disaster4_out$contractdate_final <- as.POSIXct(italy_disaster4_out$contractdate_final)
italy_disaster4_out$contractvalue <- ifelse(is.na(italy_disaster4_out$tender_finalPrice_EUR), ifelse(is.na(italy_disaster4_out$tender_estimatedPrice_EUR),"",italy_disaster4_out$tender_estimatedPrice_EUR),italy_disaster4_out$tender_finalPrice_EUR)
italy_disaster4_out$log_contractvalue <- log(as.numeric(as.character(italy_disaster4_out$contractvalue)))



##
disaster4_date <- as.POSIXct("2016-08-24")

#Dropping those that did not have firstcalldate or tenderbiddeadline before 2011
italy_disaster4_out <- subset(italy_disaster4_out, !(is.na(tender_publications_firstCallForTenderDate) & contractyear<2011)) 
italy_disaster4_out$buyer_buyerType<- as.factor(italy_disaster4_out$buyer_buyerType)
italy_disaster4_out$tender_mainCpv<- as.factor(italy_disaster4_out$tender_mainCpv)
italy_disaster4_out$contractyear<- as.factor(italy_disaster4_out$contractyear)
italy_disaster4_out$contractmonth<- as.factor(italy_disaster4_out$contractmonth)

italy_disaster4_out <- italy_disaster4_out %>% #Dropping the Contracts that are missing the number of bidders 
  filter(!is.na(totalbiddersintegrity))

## Disaster 4_out Pretreatment ----

disaster4_out_pretreat0 <-  italy_disaster4_out %>% filter(disaster4_date > italy_disaster4_out$contractdate_final)
disaster4_out_pretreat0$timing <-0
disaster4_out_pretreat0$loc <- 0

disaster4_out_posttreat0 <- anti_join(italy_disaster4_out, disaster4_out_pretreat0)
disaster4_out_posttreat0$timing<- 1
disaster4_out_posttreat0$loc<- 0

##Disaster 4_out pretreatment average of DV----
d4name_out<-as.data.frame(disaster4_out_pretreat0$buyer_name)
d4name_out <- distinct(d4name_out) 

d4namepost_out<-as.data.frame(disaster4_out_posttreat0$buyer_name)
d4namepost_out <- distinct(d4namepost_out)

names(d4name_out)[1]<-paste("buyer_name")
names(d4namepost_out)[1]<-paste("buyer_name")

common4_0_out <- intersect(d4name_out, d4namepost_out)

disaster4_out_pretreat_1<- inner_join(common4_0_out, disaster4_out_pretreat0, by= "buyer_name")

disaster4_out_pretreat <- disaster4_out_pretreat_1 %>% group_by(buyer_name) %>% mutate(meantotalbiddersintegrity = mean(totalbiddersintegrity)) #pretreament average of the number of bidders
common4_1_out<- cbind.data.frame(disaster4_out_pretreat$buyer_name, disaster4_out_pretreat$meantotalbiddersintegrity)
common4_1_out<- distinct(common4_1_out)
names(common4_1_out)[1]<-paste("buyer_name")
names(common4_1_out)[2]<-paste("meantotalbiddersintegrity")

disaster4_out_posttreat<- inner_join(common4_1_out, disaster4_out_posttreat0, by= "buyer_name")

disaster4_in_pretreat0 <- disaster4_final %>% filter(timing == 0)
disaster4_in_pretreat = subset(disaster4_in_pretreat0, select = -c(aftermatchweight,aftermatchtreat) )
disaster4_in_pretreat$contractmonth<- as.factor(disaster4_in_pretreat$contractmonth)

disaster4_in_out_pretreat <- rbind(disaster4_out_pretreat,disaster4_in_pretreat) 
temp4_0_did<-disaster4_in_out_pretreat[vars]
temp4_0_did<- as.data.frame(temp4_0_did)
summary(temp4_0_did$log_contractvalue)
summary(temp4_0_did$meantotalbiddersintegrity)
valuecuts4_0_did = c(13.311,11)
meancuts4_0_did = c(0.07,0.245)
mat4_0_did <- cem(treatment = "loc", data = temp4_0_did, drop = c("ID","totalbiddersintegrity"),cutpoints = list(log_contractvalue=valuecuts4_0_did), eval.imbalance = TRUE)
mat4_0_did


disaster4_in_out_pretreat$aftermatchtreat <- mat4_0_did$matched
disaster4_in_out_pretreat$aftermatchweight <- mat4_0_did$w
disaster4_did_0 <- disaster4_in_out_pretreat %>% filter(aftermatchtreat == TRUE)

dis4_mean_in <- disaster4_did_0 %>% filter(loc == 1)
dis4_mean_out <- disaster4_did_0 %>% filter(loc == 0)

#t.test(dis4_mean_in$totalbiddersintegrity,dis4_mean_out$totalbiddersintegrity, paired = FALSE, conf.level = 0.90)

##Disaster 4_Postreatment ----

disaster4_in_posttreat0 <- disaster4_final %>% filter(timing == 1)
disaster4_in_posttreat = subset(disaster4_in_posttreat0, select = -c(aftermatchweight,aftermatchtreat) )

disaster4_in_out_posttreat <- rbind(disaster4_out_posttreat, disaster4_in_posttreat)

temp4_1_did<-disaster4_in_out_posttreat[vars]
temp4_1_did<- as.data.frame(temp4_1_did)
summary(temp4_1_did$log_contractvalue)
summary(temp4_1_did$meantotalbiddersintegrity)
valuecuts4_1_did = c(13.61,14.82)
meancuts4_1_did = c(0.245)
mat4_1_did <- cem(treatment = "loc", data = temp4_1_did, drop = c("ID","totalbiddersintegrity"),cutpoints = list(log_contractvalue=valuecuts4_1_did), eval.imbalance = TRUE)
mat4_1_did

disaster4_in_out_posttreat$aftermatchtreat <- mat4_1_did$matched
disaster4_in_out_posttreat$aftermatchweight <- mat4_1_did$w
disaster4_did_1 <- disaster4_in_out_posttreat %>% filter(aftermatchtreat == TRUE)

disaster4_did_final <- rbind(disaster4_did_0,disaster4_did_1)



##Disaster 5 ----
italy_disaster5<- italy %>%
  filter(disnumber == "Disaster_005")
##

italy_disaster5 <- data.frame(italy_disaster5)
italy_disaster5$buyer_buyerType <- as.factor(italy_disaster5$buyer_buyerType) #Declaring buyer_buyerType as a factor
italy_disaster5$tender_mainCpv <- as.integer(italy_disaster5$tender_mainCpv)
italy_disaster5$tender_mainCpv <- sub("^(\\d{2}).*$", "\\1", italy_disaster5$tender_mainCpv) #Retaining only the first three digits of the the CPV code
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
italy_disaster5 <- italy_disaster5 %>% 
  filter(!is.na(totalbiddersintegrity))
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
disaster5_pretreat <- disaster5_pretreat_1 %>% group_by(buyer_name) %>% mutate(meantotalbiddersintegrity = mean(totalbiddersintegrity)) #pretreament average of the number of bidders
common5_1<- cbind.data.frame(disaster5_pretreat$buyer_name, disaster5_pretreat$meantotalbiddersintegrity)
common5_1<- distinct(common5_1)
names(common5_1)[1]<-paste("buyer_name")
names(common5_1)[2]<-paste("meantotalbiddersintegrity")

disaster5_posttreat<- inner_join(common5_1, disaster5_posttreat0, by= "buyer_name")

disaster5_pretreat$contractyear <- as.factor(disaster5_pretreat$contractyear)
disaster5_pretreat$contractvalue <- ifelse(is.na(disaster5_pretreat$tender_finalPrice_EUR), ifelse(is.na(disaster5_pretreat$tender_estimatedPrice_EUR),"",disaster5_pretreat$tender_estimatedPrice_EUR),disaster5_pretreat$tender_finalPrice_EUR)
disaster5_pretreat$log_contractvalue <- log(as.numeric(as.character(disaster5_pretreat$contractvalue)))

disaster5_pretreat$buyer_buyerType<- as.factor(disaster5_pretreat$buyer_buyerType)
disaster5_pretreat$tender_mainCpv<- as.factor(disaster5_pretreat$tender_mainCpv)
disaster5_pretreat$contractyear<- as.factor(disaster5_pretreat$contractyear)

disaster5_posttreat$contractyear <- as.factor(disaster5_posttreat$contractyear)
disaster5_posttreat$contractvalue <- ifelse(is.na(disaster5_posttreat$tender_finalPrice_EUR), ifelse(is.na(disaster5_posttreat$tender_estimatedPrice_EUR),"",disaster5_posttreat$tender_estimatedPrice_EUR),disaster5_posttreat$tender_finalPrice_EUR)
disaster5_posttreat$log_contractvalue <- log(as.numeric(as.character(disaster5_posttreat$contractvalue)))

disaster5_posttreat$buyer_buyerType<- as.factor(disaster5_posttreat$buyer_buyerType)
disaster5_posttreat$tender_mainCpv<- as.factor(disaster5_posttreat$tender_mainCpv)
disaster5_posttreat$contractyear<- as.factor(disaster5_posttreat$contractyear)

disaster5_posttreat$timing <- 1
disaster5_pretreat$timing <- 0

disaster5 <- rbind(as.data.frame(disaster5_pretreat), as.data.frame(disaster5_posttreat))

#Matching

temp5_0<-disaster5[vars_bf]
temp5_0<- as.data.frame(temp5_0)
summary(temp5_0$log_contractvalue)
valuecuts5_0 <- c(10,11.85,14.08,14.95,15.56,14,13)
month.grp <- list(c("1"),c("2"),c("3"),c("4"), c("5"),c("6"), c("7", "8","9"),c("10"),c("11"),c("12"))
mat5_0 <- cem(treatment = "timing", data = temp5_0, drop = c("ID","totalbiddersintegrity"), cutpoints = list(log_contractvalue = valuecuts5_0), grouping = list(contractmonth = month.grp), eval.imbalance = TRUE)
mat5_0
est5_0 <- att(mat5_0, totalbiddersintegrity ~ timing, data = temp5_0)
est5_0
disaster5$aftermatchtreat <- mat5_0$matched
disaster5$aftermatchweight <- mat5_0$w
disaster5_final <- disaster5 %>% filter(aftermatchtreat == TRUE)


##Disaster 5 DiD ----
disaster5_final$loc <- 1
italy_disaster5_out<- italy %>%
  filter(treatcon == 0)

italy_disaster5_out <- data.frame(italy_disaster5_out)
italy_disaster5_out$buyer_buyerType <- as.factor(italy_disaster5_out$buyer_buyerType) #Declaring buyer_buyerType as a factor
italy_disaster5_out$tender_mainCpv <- as.integer(italy_disaster5_out$tender_mainCpv)
italy_disaster5_out$tender_mainCpv <- sub("^(\\d{2}).*$", "\\1", italy_disaster5_out$tender_mainCpv) #Retaining only the first two digits of the the CPV code
italy_disaster5_out$tender_mainCpv <- as.integer(italy_disaster5_out$tender_mainCpv)
italy_disaster5_out$tender_mainCpv

##Breaking up the dates to extract months and years
italy_disaster5_out$contractyear<- ifelse(is.na(italy_disaster5_out$tender_publications_firstCallForTenderDate), substring(italy_disaster5_out$tender_publications_firstdContractAwardDate,1,4),substring(italy_disaster5_out$tender_publications_firstCallForTenderDate,1,4)) #Contract year
italy_disaster5_out$contractmonth <- ifelse(is.na(italy_disaster5_out$tender_publications_firstCallForTenderDate), substring(italy_disaster5_out$tender_publications_firstdContractAwardDate,6,7),substring(italy_disaster5_out$tender_publications_firstCallForTenderDate,6,7)) #Contract month
italy_disaster5_out$contractday <- ifelse(is.na(italy_disaster5_out$tender_publications_firstCallForTenderDate), substring(italy_disaster5_out$tender_publications_firstdContractAwardDate,9,10),substring(italy_disaster5_out$tender_publications_firstCallForTenderDate,9,10)) #Contract month
italy_disaster5_out$contractmonth <- as.numeric(italy_disaster5_out$contractmonth)
italy_disaster5_out$contractyear <- as.numeric(italy_disaster5_out$contractyear)
italy_disaster5_out$contractday <- as.numeric(italy_disaster5_out$contractday)

italy_disaster5_out$contractdate_final <- paste(italy_disaster5_out$contractyear, italy_disaster5_out$contractmonth, italy_disaster5_out$contractday, sep="-")
italy_disaster5_out$contractdate_final <- as.POSIXct(italy_disaster5_out$contractdate_final)
italy_disaster5_out$contractvalue <- ifelse(is.na(italy_disaster5_out$tender_finalPrice_EUR), ifelse(is.na(italy_disaster5_out$tender_estimatedPrice_EUR),"",italy_disaster5_out$tender_estimatedPrice_EUR),italy_disaster5_out$tender_finalPrice_EUR)
italy_disaster5_out$log_contractvalue <- log(as.numeric(as.character(italy_disaster5_out$contractvalue)))



##
disaster5_date <- as.POSIXct("2017-01-18")

#Dropping those that did not have firstcalldate or tenderbiddeadline before 2011
italy_disaster5_out <- subset(italy_disaster5_out, !(is.na(tender_publications_firstCallForTenderDate) & contractyear<2011)) 
italy_disaster5_out$buyer_buyerType<- as.factor(italy_disaster5_out$buyer_buyerType)
italy_disaster5_out$tender_mainCpv<- as.factor(italy_disaster5_out$tender_mainCpv)
italy_disaster5_out$contractyear<- as.factor(italy_disaster5_out$contractyear)
italy_disaster5_out$contractmonth<- as.factor(italy_disaster5_out$contractmonth)

italy_disaster5_out <- italy_disaster5_out %>% #Dropping the Contracts that are missing the number of bidders 
  filter(!is.na(totalbiddersintegrity))

## Disaster 5_out Pretreatment ----

disaster5_out_pretreat0 <-  italy_disaster5_out %>% filter(disaster5_date > italy_disaster5_out$contractdate_final)
disaster5_out_pretreat0$timing <-0
disaster5_out_pretreat0$loc <- 0

disaster5_out_posttreat0 <- anti_join(italy_disaster5_out, disaster5_out_pretreat0)
disaster5_out_posttreat0$timing<- 1
disaster5_out_posttreat0$loc<- 0

##Disaster 5_out pretreatment average of DV----
d5name_out<-as.data.frame(disaster5_out_pretreat0$buyer_name)
d5name_out <- distinct(d5name_out) 

d5namepost_out<-as.data.frame(disaster5_out_posttreat0$buyer_name)
d5namepost_out <- distinct(d5namepost_out)

names(d5name_out)[1]<-paste("buyer_name")
names(d5namepost_out)[1]<-paste("buyer_name")

common5_0_out <- intersect(d5name_out, d5namepost_out)

disaster5_out_pretreat_1<- inner_join(common5_0_out, disaster5_out_pretreat0, by= "buyer_name")

disaster5_out_pretreat <- disaster5_out_pretreat_1 %>% group_by(buyer_name) %>% mutate(meantotalbiddersintegrity = mean(totalbiddersintegrity)) #pretreament average of the number of bidders
common5_1_out<- cbind.data.frame(disaster5_out_pretreat$buyer_name, disaster5_out_pretreat$meantotalbiddersintegrity)
common5_1_out<- distinct(common5_1_out)
names(common5_1_out)[1]<-paste("buyer_name")
names(common5_1_out)[2]<-paste("meantotalbiddersintegrity")

disaster5_out_posttreat<- inner_join(common5_1_out, disaster5_out_posttreat0, by= "buyer_name")



disaster5_in_pretreat0 <- disaster5_final %>% filter(timing == 0)
disaster5_in_pretreat = subset(disaster5_in_pretreat0, select = -c(aftermatchweight,aftermatchtreat) )

disaster5_in_pretreat$contractmonth <- as.factor(disaster5_in_pretreat$contractmonth)

disaster5_in_out_pretreat <- rbind(disaster5_out_pretreat,disaster5_in_pretreat) 
vars_x <- c("ID","loc", "totalbiddersintegrity","log_contractvalue", "contractmonth","contractyear","tender_mainCpv","buyer_buyerType")
temp5_0_did<-disaster5_in_out_pretreat[vars_x]
temp5_0_did<- as.data.frame(temp5_0_did)
summary(temp5_0_did$log_contractvalue)
valuecuts5_0_did <- c(13.59,13.58,13.6,11,16,17,11.5,11.1)
month.grp <- list(c("1"),c("2"),c("3","4"),c("5","6"), c("7"),c( "8"),c("9","10"),c( "11","12","NA",NA))
buyer_buyerType.grp<- list(c("REGIONAL_AUTHORITY", "REGIONAL_AGENCY"),c("UTILITIES","NATIONAL_AUTHORITY"), c("OTHER"),c("PUBLIC_BODY","NA"))
#mat5_0_did <- cem(treatment = "loc", data = temp5_0_did, drop = c("ID","totalbiddersintegrity"), cutpoints = list(log_contractvalue = valuecuts5_0_did),grouping = list(contractmonth = month.grp, buyer_buyerType = buyer_buyerType.grp), eval.imbalance = TRUE)
temp5_0_did<- as.data.frame(temp5_0_did)
mat5_0_did <- cem(treatment = "loc", data = temp5_0_did, drop = c("ID","totalbiddersintegrity"), cutpoints = list(log_contractvalue = valuecuts5_0_did),grouping = list(contractmonth = month.grp, buyer_buyerType = buyer_buyerType.grp) , eval.imbalance = TRUE)
mat5_0_did

disaster5_in_out_pretreat$aftermatchtreat <- mat5_0_did$matched
disaster5_in_out_pretreat$aftermatchweight <- mat5_0_did$w
disaster5_did_0 <- disaster5_in_out_pretreat %>% filter(aftermatchtreat == TRUE)

dis5_mean_in <- disaster5_did_0 %>% filter(loc == 1)
dis5_mean_out <- disaster5_did_0 %>% filter(loc == 0)

t.test(dis5_mean_in$totalbiddersintegrity,dis5_mean_out$totalbiddersintegrity, paired = FALSE, conf.level = 0.90)

##Disaster 5_Postreatment ----

disaster5_in_posttreat0 <- disaster5_final %>% filter(timing == 1)
disaster5_in_posttreat = subset(disaster5_in_posttreat0, select = -c(aftermatchweight,aftermatchtreat) )

disaster5_in_out_posttreat <- rbind(disaster5_out_posttreat, disaster5_in_posttreat)


temp5_1_did<-disaster5_in_out_posttreat[vars_x]
temp5_1_did<- as.data.frame(temp5_1_did)

summary(temp5_1_did$log_contractvalue)
valuecuts5_1_did <- c(13.59,13.58,13.6,11,16,17,11.5,11.1)
mat5_1_did <- cem(treatment = "loc", data = temp5_1_did, drop = c("ID","totalbiddersintegrity"), cutpoints = list(log_contractvalue = valuecuts5_1_did),grouping = list(contractmonth = month.grp, buyer_buyerType = buyer_buyerType.grp), eval.imbalance = TRUE)
mat5_1_did

disaster5_in_out_posttreat$aftermatchtreat <- mat5_1_did$matched
disaster5_in_out_posttreat$aftermatchweight <- mat5_1_did$w
disaster5_did_1 <- disaster5_in_out_posttreat %>% filter(aftermatchtreat == TRUE)

disaster5_did_final <- rbind(disaster5_did_0,disaster5_did_1)

##Full matched data

##t-tests----

dv1 <- rbind(disaster1_final, disaster2_final, disaster3_final, disaster4_final, disaster5_final)

dv1_pre <- dv1 %>% filter(timing == 0)
dv1_post <- dv1 %>% filter(timing == 1)


matched_comparison<- t.test(dv1_post$totalbiddersintegrity,dv1_pre$totalbiddersintegrity, paired = FALSE, conf.level = 0.90)

matched_comparison =  matched_comparison %>%
  broom::tidy() %>% 
  mutate(`N (total)` = nrow(dv1_pre) + nrow(dv1_post),
         `N (after disaster)` = nrow(dv1_post))

saveRDS(matched_comparison,'tables/table_8/4_t_test_did_totalbidderintegrity.rds')

##Diff-in-diff ----
disaster3_did_final$contractday <- as.numeric(disaster3_did_final$contractday)

dv2 <- rbind(disaster2_did_final, disaster3_did_final, disaster5_did_final)

dv3<- transform(dv2, freq.loc = ave(seq(nrow(dv2)), buyer_name, FUN=length))
dv4<- dv3 
dv4_in<- dv4 %>% filter(loc==1)
dv4_out <- dv4 %>% filter(loc==0)

dv4_out_sumwt <- dv4_out %>% group_by(ID) %>% mutate(sumwt = sum(aftermatchweight))
dv4_out_sumwt2<- distinct(dv4_out_sumwt)
didreg = lm(totalbiddersintegrity ~ loc*timing +  contractyear + contractmonth + buyer_buyerType + tender_mainCpv + log_contractvalue, data = dv4, weights = aftermatchweight)
summary(didreg)

dv4_pre_in <- dv4 %>% filter(timing == 0 & loc == 1)
dv4_pre_out <- dv4 %>% filter(timing == 0 & loc == 0)

t.test(dv4_pre_in$totalbiddersintegrity,dv4_pre_out$totalbiddersintegrity, paired = FALSE, conf.level = 0.90)

##Computations of graph ----
###matched t-tests for 3, 2, 1 years

disaster1_final$year <- format(as.Date(disaster1_final$contractdate), "%Y")
disaster2_final$year <- format(as.Date(disaster2_final$contractdate), "%Y")
disaster3_final$year <- format(as.Date(disaster3_final$contractdate), "%Y")
disaster4_final$year <- format(as.Date(disaster4_final$contractdate), "%Y")
disaster5_final$year <- format(as.Date(disaster5_final$contractdate), "%Y")
##Oneyear ----
d1_oneyear <- disaster1_final %>% filter(year >= 2008 & year <= 2010) %>% filter(loc == 1)
d2_oneyear <- disaster2_final %>% filter((year >= 2011) & year <= 2013)%>% filter(loc == 1)
d3_oneyear <- disaster3_final %>% filter(year >= 2012 & year <= 2014)%>% filter(loc == 1)
d4_oneyear <- disaster4_final %>% filter(year >= 2015 & year <= 2017)%>% filter(loc == 1)
d5_oneyear <- disaster5_final %>% filter(year >= 2016 & year <= 2018)%>% filter(loc == 1)

d_oneyear <- rbind(d1_oneyear,d2_oneyear,d3_oneyear,d4_oneyear,d5_oneyear)
d_oneyearpost <- d_oneyear %>% filter(timing == 1)
d_oneyearpre <- d_oneyear %>% filter(timing == 0)
matched_comparison_oneyear <- t.test(d_oneyearpost$totalbiddersintegrity,d_oneyearpre$totalbiddersintegrity, paired = FALSE, conf.level = 0.90)

matched_comparison_oneyear =  matched_comparison_oneyear %>%
  broom::tidy() %>% 
  mutate(`N (total)` = nrow(d_oneyearpost) + nrow(d_oneyearpost),
         `N (after disaster)` = nrow(d_oneyearpost))

saveRDS(matched_comparison_oneyear,'tables/table_14/4_t_test_did_totalbidderintegrity.rds')

##Two year ----
d1_twoyear <- disaster1_final %>% filter(year >= 2007 & year <= 2011) %>% filter(loc == 1)
d2_twoyear <- disaster2_final %>% filter((year >= 2010) & year <= 2014)%>% filter(loc == 1)
d3_twoyear <- disaster3_final %>% filter(year >= 2011 & year <= 2015)%>% filter(loc == 1)
d4_twoyear <- disaster4_final %>% filter(year >= 2014 & year <= 2018)%>% filter(loc == 1)
d5_twoyear <- disaster5_final %>% filter(year >= 2015 & year <= 2019)%>% filter(loc == 1)

d_twoyear <- rbind(d1_twoyear,d2_twoyear,d3_twoyear,d4_twoyear,d5_twoyear)
d_twoyearpost <- d_twoyear %>% filter(timing == 1)
d_twoyearpre <- d_twoyear %>% filter(timing == 0)
matched_comparison_twoyear <- t.test(d_twoyearpost$totalbiddersintegrity,d_oneyearpre$totalbiddersintegrity, paired = FALSE, conf.level = 0.90)

##Threeyear

d1_threeyear <- disaster1_final %>% filter(year >= 2006 & year <= 2012)%>% filter(loc == 1)

d2_threeyear <- disaster2_final %>% filter(year >= 2009 & year <= 2015)%>% filter(loc == 1)

d3_threeyear <- disaster3_final %>% filter(year >= 2010 & year <= 2016)%>% filter(loc == 1)
d4_threeyear <- disaster4_final %>% filter(year >= 2013 & year <= 2019)%>% filter(loc == 1)
d5_threeyear <- disaster5_final %>% filter(year >= 2014 & year <= 2020)%>% filter(loc == 1)


d_threeyear <- rbind(d1_threeyear,d2_threeyear,d3_threeyear,d4_threeyear,d5_threeyear)

d_threeyearpre <- d_threeyear %>% filter(timing == 0)
d_threeyearpost <- d_threeyear %>% filter(timing == 1)
matched_comparison_threeyear <- t.test(d_threeyearpost$totalbiddersintegrity,d_threeyearpre$totalbiddersintegrity, paired = FALSE, conf.level = 0.90)

matched_comparison_threeyear =  matched_comparison_threeyear %>%
  broom::tidy() %>% 
  mutate(`N (total)` = nrow(d_threeyearpre) + nrow(d_threeyearpost),
         `N (after disaster)` = nrow(d_threeyearpost))

saveRDS(matched_comparison_threeyear,'tables/table_13/4_t_test_did_totalbidderintegrity.rds')

###Clustering Standard errors:

#install.packages("multiwayvcov")
#install.packages("lmtest")

library(multiwayvcov)
library(lmtest)

cluster_var <- "buyer_nuts"

didreg2 = lm(totalbiddersintegrity ~ loc*timing +  contractyear + contractmonth + buyer_buyerType + tender_mainCpv + log_contractvalue, data = dv4, weights = aftermatchweight)

summary(didreg2)

saveRDS(didreg2, '4_totalbidderintegrity_did.Rds')

clustered_se <- cluster.vcov(didreg2, dv4[[cluster_var]])

summary_coeftest <- coeftest(didreg2, clustered_se)
print(summary_coeftest)

saveRDS(summary_coeftest, '4_totalbidderintegrity_did_clustered_se.Rds')

distinct_rows <- dv4 %>% distinct(buyer_nuts)
print(distinct_rows)





