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

italy$procedureintegrity<- ifelse(italy$tender_indicator_INTEGRITY_PROCEDURE_TYPE == 0,1,0)

##Disaster1 Analysis##----

##Converting to dataframe and Retaining first two digits of the CPV code
##Disaster 1 ----
italy_disaster1<- italy %>%
  filter(disnumber == "Disaster_001")
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
  filter(!is.na(procedureintegrity))

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
disaster1_pretreat <- disaster1_pretreat_1 %>% group_by(buyer_name) %>% mutate(meanprocedureintegrity = mean(procedureintegrity)) #pretreament average of the number of bidders
common1_1<- cbind.data.frame(disaster1_pretreat$buyer_name, disaster1_pretreat$meanprocedureintegrity)
common1_1<- distinct(common1_1)
names(common1_1)[1]<-paste("buyer_name")
names(common1_1)[2]<-paste("meanprocedureintegrity")

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
vars <- c("timing", "tender_mainCpv", "procedureintegrity", "log_contractvalue", "meanprocedureintegrity","contractmonth","buyer_buyerType")
temp1_0<-disaster1[vars]
vars2<- c("log_contractvalue","meanprocedureintegrity")
temp1_0<- as.data.frame(temp1_0)
imbalance(group=temp1_0$timing, data=temp1_0[vars2])
summary(temp1_0$log_contractvalue)
valuecuts1_0 = c(13.05)
mat1_0 <- cem(treatment = "timing", data = temp1_0, drop = "procedureintegrity",cutpoints = list(log_contractvalue=valuecuts1_0), eval.imbalance = TRUE)
mat1_0
mat1_0$w
est1_0 <- att(mat1_0, procedureintegrity ~ timing, data = temp1_0)
est1_0
disaster1$aftermatchtreat <- mat1_0$matched
disaster1$aftermatchweight <- mat1_0$w
disaster1_final <- disaster1 %>% filter(aftermatchtreat == TRUE)

##Disaster 2 ----
italy_disaster2<- italy %>%
  filter(disnumber == "Disaster_002")
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
italy_disaster2 <- italy_disaster2 %>% #Dropping the Contracts that are missing the procedureintegrity
  filter(!is.na(procedureintegrity))
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
disaster2_pretreat <- disaster2_pretreat_1 %>% group_by(buyer_name) %>% mutate(meanprocedureintegrity = mean(procedureintegrity)) #pretreament average of the number of bidders
common2_1<- cbind.data.frame(disaster2_pretreat$buyer_name, disaster2_pretreat$meanprocedureintegrity)
common2_1<- distinct(common2_1)
names(common2_1)[1]<-paste("buyer_name")
names(common2_1)[2]<-paste("meanprocedureintegrity")

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

temp2_0<-disaster2[vars]
vars2<- c("tender_mainCpv", "log_contractvalue", "meanprocedureintegrity")
vars3<- c("log_contractvalue","meanprocedureintegrity","contractyear","tender_mainCpv")
temp2_0<- as.data.frame(temp2_0)
imbalance(group=temp2_0$timing, data=temp2_0[vars2])
summary(temp2_0$log_contractvalue)
valuecuts2_0 = c(14.34)
buyer_buyerType.grp<- list(c("REGIONAL_AUTHORITY", "REGIONAL_AGENCY", "UTILITIES"), c("NATIONAL_AUTHORITY"),c("OTHER"), c("PUBLIC_BODY"), c("NA",NA))
mat2_0 <- cem(treatment = "timing", data = temp2_0, drop = "procedureintegrity",cutpoints = list(log_contractvalue=valuecuts2_0), eval.imbalance = TRUE)
mat2_0
est2_0 <- att(mat2_0, procedureintegrity ~ timing, data = temp2_0)
est2_0
disaster2$aftermatchtreat <- mat2_0$matched
disaster2$aftermatchweight <- mat2_0$w
disaster2_final <- disaster2 %>% filter(aftermatchtreat == TRUE)

##Disaster 3 ----
italy_disaster3<- italy %>%
  filter(disnumber == "Disaster_003")
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
  filter(!is.na(procedureintegrity))

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
disaster3_pretreat <- disaster3_pretreat_1 %>% group_by(buyer_name) %>% mutate(meanprocedureintegrity = mean(procedureintegrity)) #pretreament average of the number of bidders
common3_1<- cbind.data.frame(disaster3_pretreat$buyer_name, disaster3_pretreat$meanprocedureintegrity)
common3_1<- distinct(common3_1)
names(common3_1)[1]<-paste("buyer_name")
names(common3_1)[2]<-paste("meanprocedureintegrity")

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

temp3_0<-disaster3[vars]
vars2<- c("timing", "log_contractvalue", "meanprocedureintegrity")
temp3_0<- as.data.frame(temp3_0)
imbalance(group=temp3_0$timing, data=temp3_0[vars2])
summary(temp3_0$log_contractvalue)
valuecuts3_0 <- c(14.23)
mat3_0 <- cem(treatment = "timing", data = temp3_0, drop = "procedureintegrity",cutpoints = list(log_contractvalue=valuecuts3_0), eval.imbalance = TRUE)
mat3_0
est3_0 <- att(mat3_0, procedureintegrity ~ timing, data = temp3_0)
est3_0
disaster3$aftermatchtreat <- mat3_0$matched
disaster3$aftermatchweight <- mat3_0$w
disaster3_final <- disaster3 %>% filter(aftermatchtreat == TRUE)



##Disaster 4 ----
italy_disaster4<- italy %>%
  filter(disnumber == "Disaster_004")
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
  filter(!is.na(procedureintegrity))
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
disaster4_pretreat <- disaster4_pretreat_1 %>% group_by(buyer_name) %>% mutate(meanprocedureintegrity = mean(procedureintegrity)) #pretreament average of the number of bidders
common4_1<- cbind.data.frame(disaster4_pretreat$buyer_name, disaster4_pretreat$meanprocedureintegrity)
common4_1<- distinct(common4_1)
names(common4_1)[1]<-paste("buyer_name")
names(common4_1)[2]<-paste("meanprocedureintegrity")

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

temp4_0<-disaster4[vars]
vars2<- c("tender_mainCpv", "log_contractvalue", "meanprocedureintegrity")
vars3<- c("log_contractvalue","meanprocedureintegrity","contractyear","tender_mainCpv")
temp4_0<- as.data.frame(temp4_0)
imbalance(group=temp4_0$timing, data=temp4_0[vars2])
summary(temp4_0$log_contractvalue)
valuecuts4_0 = c(13.76)
buyer_buyerType.grp<- list(c("REGIONAL_AUTHORITY", "REGIONAL_AGENCY", "UTILITIES"), c("NATIONAL_AUTHORITY"),c("OTHER"), c("PUBLIC_BODY"), c("NA",NA))
mat4_0 <- cem(treatment = "timing", data = temp4_0, drop = "procedureintegrity",cutpoints = list(log_contractvalue=valuecuts4_0), eval.imbalance = TRUE)
mat4_0
est4_0 <- att(mat4_0, procedureintegrity ~ timing, data = temp4_0)
est4_0
disaster4$aftermatchtreat <- mat4_0$matched
disaster4$aftermatchweight <- mat4_0$w
disaster4_final <- disaster4 %>% filter(aftermatchtreat == TRUE)


##Disaster 5 ----
italy_disaster5<- italy %>%
  filter(disnumber == "Disaster_005")
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
italy_disaster5 <- italy_disaster5 %>% 
  filter(!is.na(procedureintegrity))
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
disaster5_pretreat <- disaster5_pretreat_1 %>% group_by(buyer_name) %>% mutate(meanprocedureintegrity = mean(procedureintegrity)) #pretreament average of the number of bidders
common5_1<- cbind.data.frame(disaster5_pretreat$buyer_name, disaster5_pretreat$meanprocedureintegrity)
common5_1<- distinct(common5_1)
names(common5_1)[1]<-paste("buyer_name")
names(common5_1)[2]<-paste("meanprocedureintegrity")

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

temp5_0<-disaster5[vars]
vars2<- c("tender_mainCpv", "log_contractvalue", "meanprocedureintegrity")
temp5_0<- as.data.frame(temp5_0)
imbalance(group=temp5_0$timing, data=temp5_0[vars2])
summary(temp5_0$log_contractvalue)
valuecuts5_0 = c(14.95)
buyer_buyerType.grp<- list(c("REGIONAL_AUTHORITY", "REGIONAL_AGENCY", "UTILITIES"), c("NATIONAL_AUTHORITY"),c("OTHER"), c("PUBLIC_BODY"), c("NA",NA))
mat5_0 <- cem(treatment = "timing", data = temp5_0, drop = "procedureintegrity",cutpoints = list(log_contractvalue=valuecuts5_0), eval.imbalance = TRUE)
mat5_0
est5_0 <- att(mat5_0, procedureintegrity ~ timing, data = temp5_0)
est5_0
disaster5$aftermatchtreat <- mat5_0$matched
disaster5$aftermatchweight <- mat5_0$w
disaster5_final <- disaster5 %>% filter(aftermatchtreat == TRUE)

##Full matched data

dv1 <- rbind(disaster1_final, disaster2_final, disaster3_final, disaster4_final, disaster5_final)
dv1_pre <- dv1 %>% filter(timing == 0)
dv1_post <- dv1 %>% filter(timing == 1)

matched_comparison<- t.test(dv1_pre$procedureintegrity,dv1_post$procedureintegrity, paired = FALSE, conf.level = 0.90)
matched_comparison
