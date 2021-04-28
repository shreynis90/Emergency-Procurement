library(cem)
library(data.table)
library(readxl)
library(dplyr)
library(ggplot2)
library(gtsummary)
library(lubridate)
library(openxlsx)
library(zoo)
library(scales)
library(forcats)
library(reshape2)
library(stargazer)
library(broom)
library(modEvA)


memory.limit(size = 30000)
gc()

##Reading the Data#----
italy<- read_excel("data_09_20.xlsx")
italy<-as.data.frame(italy)

##Declaring as Dates
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


##Removing Missings 
italy<- italy %>% filter(!is.na(lot_bidsCount))

##Declaring a Trimmed bidder number variable
italy$totalbidders <- ifelse(italy$lot_bidsCount >= 20, 20, italy$lot_bidsCount)

##Disaster 1 ----
italy_disaster1<- italy %>%
  filter(disnumber == "Disaster_001"|is.na(disnumber))
##

italy_disaster1 <- data.frame(italy_disaster1)
italy_disaster1$buyer_buyerType <- as.factor(italy_disaster1$buyer_buyerType) #Declaring buyer_buyerType as a factor
italy_disaster1$tender_mainCpv <- as.integer(italy_disaster1$tender_mainCpv)
italy_disaster1$cpv <- sub("^(\\d{2}).*$", "\\1", italy_disaster1$tender_mainCpv) #Retaining only the first two digits of the the CPV code
italy_disaster1$cpv <- as.integer(italy_disaster1$cpv)

##Breaking up the dates to extract months and years
italy_disaster1$contractyear<- ifelse(is.na(italy_disaster1$tender_publications_firstCallForTenderDate), substring(italy_disaster1$tender_publications_firstdContractAwardDate,1,4),substring(italy_disaster1$tender_publications_firstCallForTenderDate,1,4)) #Contract year
italy_disaster1$contractmonth <- ifelse(is.na(italy_disaster1$tender_publications_firstCallForTenderDate), substring(italy_disaster1$tender_publications_firstdContractAwardDate,6,7),substring(italy_disaster1$tender_publications_firstCallForTenderDate,6,7)) #Contract month
italy_disaster1<- italy_disaster1 %>% filter(contractyear>=2008 & contractyear<2013) #Range of the Data (Full data goes from 2006 to 2020)
##
disaster1_date <- as.POSIXct("2009-10-02")

##Dropping those that did not have firstcalldate or tenderbiddeadline before 2011
italy_disaster1 <- subset(italy_disaster1, !(is.na(tender_publications_firstCallForTenderDate) & contractyear<2011)) 

##Generating only month and year as date
italy_disaster1$Date <- as.yearmon(paste(italy_disaster1$contractmonth, italy_disaster1$contractyear), "%m %Y")

##disaster2---- (Same as disaster 1)
italy_disaster2<- italy %>%
  filter(disnumber == "Disaster_002"|is.na(disnumber))
##

italy_disaster2 <- data.frame(italy_disaster2)
italy_disaster2$buyer_buyerType <- as.factor(italy_disaster2$buyer_buyerType) #Declaring buyer_buyerType as a factor
italy_disaster2$tender_mainCpv <- as.integer(italy_disaster2$tender_mainCpv)
italy_disaster2$cpv <- sub("^(\\d{2}).*$", "\\1", italy_disaster2$tender_mainCpv) #Retaining only the first two digits of the the CPV code
italy_disaster2$cpv <- as.integer(italy_disaster2$cpv)

##Fixing the dates
italy_disaster2$contractyear<- ifelse(is.na(italy_disaster2$tender_publications_firstCallForTenderDate), substring(italy_disaster2$tender_publications_firstdContractAwardDate,1,4),substring(italy_disaster2$tender_publications_firstCallForTenderDate,1,4)) #Contract year
italy_disaster2$contractmonth <- ifelse(is.na(italy_disaster2$tender_publications_firstCallForTenderDate), substring(italy_disaster2$tender_publications_firstdContractAwardDate,6,7),substring(italy_disaster2$tender_publications_firstCallForTenderDate,6,7)) #Contract month
italy_disaster2<- italy_disaster2 %>% filter(contractyear>=2009 & contractyear<2016) #Range of the Data (Full data goes from 2006 to 2020)
##
italy_disaster2 <- subset(italy_disaster2, !(is.na(tender_publications_firstCallForTenderDate) & contractyear<2011))
disaster2_date <- as.POSIXct("2012-05-29")

###Number of Contracts ----
italy_disaster2$Date <- as.yearmon(paste(italy_disaster2$contractmonth, italy_disaster2$contractyear), "%m %Y")

##Disaster 3 ----
#Removing contracts from other disaster areas#
italy_disaster3<- italy %>%
  filter(disnumber == "Disaster_003"|is.na(disnumber))
##

italy_disaster3 <- data.frame(italy_disaster3)
italy_disaster3$buyer_buyerType <- as.factor(italy_disaster3$buyer_buyerType) #Declaring buyer_buyerType as a factor
italy_disaster3$tender_mainCpv <- as.integer(italy_disaster3$tender_mainCpv)
italy_disaster3$cpv <- sub("^(\\d{2}).*$", "\\1", italy_disaster3$tender_mainCpv) #Retaining only the first three digits of the the CPV code
italy_disaster3$cpv <- as.integer(italy_disaster3$cpv)

##Fixing the dates
italy_disaster3$contractyear<- ifelse(is.na(italy_disaster3$tender_publications_firstCallForTenderDate), substring(italy_disaster3$tender_publications_firstdContractAwardDate,1,4),substring(italy_disaster3$tender_publications_firstCallForTenderDate,1,4)) #Contract year
italy_disaster3$contractmonth <- ifelse(is.na(italy_disaster3$tender_publications_firstCallForTenderDate), substring(italy_disaster3$tender_publications_firstdContractAwardDate,6,7),substring(italy_disaster3$tender_publications_firstCallForTenderDate,6,7)) #Contract month
italy_disaster3<- italy_disaster3 %>% filter(contractyear>=2010 & contractyear<2017) #Range of the Data (Full data goes from 2006 to 2020)
##
italy_disaster3 <- subset(italy_disaster3, !(is.na(tender_publications_firstCallForTenderDate) & contractyear<2011))
disaster3_date <- as.POSIXct("2013-11-18")

###Number of Contracts ----
italy_disaster3$Date <- as.yearmon(paste(italy_disaster3$contractmonth, italy_disaster3$contractyear), "%m %Y")


##Disaster 4 ----
#Removing contracts from other disaster areas#
italy_disaster4<- italy %>%
  filter(disnumber == "Disaster_004"|is.na(disnumber))
##

italy_disaster4 <- data.frame(italy_disaster4)
italy_disaster4$buyer_buyerType <- as.factor(italy_disaster4$buyer_buyerType) #Declaring buyer_buyerType as a factor
italy_disaster4$tender_mainCpv <- as.integer(italy_disaster4$tender_mainCpv)
italy_disaster4$cpv <- sub("^(\\d{2}).*$", "\\1", italy_disaster4$tender_mainCpv) #Retaining only the first three digits of the the CPV code
italy_disaster4$cpv <- as.integer(italy_disaster4$cpv)

##Fixing the dates
italy_disaster4$contractyear<- ifelse(is.na(italy_disaster4$tender_publications_firstCallForTenderDate), substring(italy_disaster4$tender_publications_firstdContractAwardDate,1,4),substring(italy_disaster4$tender_publications_firstCallForTenderDate,1,4)) #Contract year
italy_disaster4$contractmonth <- ifelse(is.na(italy_disaster4$tender_publications_firstCallForTenderDate), substring(italy_disaster4$tender_publications_firstdContractAwardDate,6,7),substring(italy_disaster4$tender_publications_firstCallForTenderDate,6,7)) #Contract month
italy_disaster4<- italy_disaster4 %>% filter(contractyear>=2013 & contractyear<2020) #Range of the Data (Full data goes from 2006 to 2020)
##
italy_disaster4 <- subset(italy_disaster4, !(is.na(tender_publications_firstCallForTenderDate) & contractyear<2011))
disaster4_date <- as.POSIXct("2016-08-24")

###Number of Contracts ----
italy_disaster4$Date <- as.yearmon(paste(italy_disaster4$contractmonth, italy_disaster4$contractyear), "%m %Y")


##Disaster 5----
#Removing contracts from other disaster areas#
italy_disaster5<- italy %>%
  filter(disnumber == "Disaster_005"|is.na(disnumber))
##

italy_disaster5 <- data.frame(italy_disaster5)
italy_disaster5$buyer_buyerType <- as.factor(italy_disaster5$buyer_buyerType) #Declaring buyer_buyerType as a factor
italy_disaster5$tender_mainCpv <- as.integer(italy_disaster5$tender_mainCpv)
italy_disaster5$cpv <- sub("^(\\d{2}).*$", "\\1", italy_disaster5$tender_mainCpv) #Retaining only the first three digits of the the CPV code
italy_disaster5$cpv <- as.integer(italy_disaster5$cpv)

##Fixing the dates
italy_disaster5$contractyear<- ifelse(is.na(italy_disaster5$tender_publications_firstCallForTenderDate), substring(italy_disaster5$tender_publications_firstdContractAwardDate,1,4),substring(italy_disaster5$tender_publications_firstCallForTenderDate,1,4)) #Contract year
italy_disaster5$contractmonth <- ifelse(is.na(italy_disaster5$tender_publications_firstCallForTenderDate), substring(italy_disaster5$tender_publications_firstdContractAwardDate,6,7),substring(italy_disaster5$tender_publications_firstCallForTenderDate,6,7)) #Contract month
italy_disaster5<- italy_disaster5 %>% filter(contractyear>=2014 & contractyear<=2020) #Range of the Data (Full data goes from 2006 to 2020)
##
italy_disaster5 <- subset(italy_disaster5, !(is.na(tender_publications_firstCallForTenderDate) & contractyear<2011))
disaster5_date <- as.POSIXct("2017-01-18")

###Number of Contracts
italy_disaster5$Date <- as.yearmon(paste(italy_disaster5$contractmonth, italy_disaster5$contractyear), "%m %Y")



##Treated Contracts by disaster ----
italy_disaster1_1 <- italy_disaster1 %>% filter(treatcon == 1)
italy_disaster1_1$treatmentstatus <- ifelse(is.na(italy_disaster1_1$tender_publications_firstCallForTenderDate), ifelse(italy_disaster1_1$tender_publications_firstdContractAwardDate >= disaster1_date - as.difftime(1, unit="days"),1,0),ifelse(italy_disaster1_1$tender_publications_firstCallForTenderDate>= disaster1_date  - as.difftime(1, unit="days"),1,0))
italy_disaster1_1$ord <- NULL
for(i in 1:nrow(italy_disaster1_1)){
  italy_disaster1_1$ord[i] <- round(as.integer(difftime(italy_disaster1_1$Date[i], as.yearmon(disaster1_date, units = "weeks")))/31,0)
  
}
italy_disaster1_1_3 <- italy_disaster1_1 %>%filter(ord <=36 & ord >= - 36)
italy_disaster1_1_2 <- italy_disaster1_1 %>%filter(ord <=24 & ord >= - 24)
italy_disaster1_1_1 <- italy_disaster1_1 %>%filter(ord <=12 & ord >= - 12)

italy_disaster2_1 <- italy_disaster2 %>% filter(treatcon == 1)
italy_disaster2_1$treatmentstatus <- ifelse(is.na(italy_disaster2_1$tender_publications_firstCallForTenderDate), ifelse(italy_disaster2_1$tender_publications_firstdContractAwardDate >= disaster2_date,1,0),ifelse(italy_disaster2_1$tender_publications_firstCallForTenderDate >= disaster2_date,1,0))
italy_disaster2_1$ord <- NULL
for(i in 1:nrow(italy_disaster2_1)){
  italy_disaster2_1$ord[i] <- round(as.integer(difftime(italy_disaster2_1$Date[i], as.yearmon(disaster2_date, units = "weeks")))/31,0)
  
}
italy_disaster2_1_3 <- italy_disaster2_1 %>%filter(ord <=36 & ord >=- 36)
italy_disaster2_1_2 <- italy_disaster2_1 %>%filter(ord <=24 & ord >= - 24)
italy_disaster2_1_1 <- italy_disaster2_1 %>%filter(ord <=12 & ord >= - 12)


italy_disaster3_1 <- italy_disaster3 %>% filter(treatcon == 1)
italy_disaster3_1$treatmentstatus <- ifelse(is.na(italy_disaster3_1$tender_publications_firstCallForTenderDate), ifelse(italy_disaster3_1$tender_publications_firstdContractAwardDate >= disaster3_date,1,0),ifelse(italy_disaster3_1$tender_publications_firstCallForTenderDate >= disaster3_date,1,0))
italy_disaster3_1$ord <- NULL
for(i in 1:nrow(italy_disaster3_1)){
  italy_disaster3_1$ord[i] <- round(as.integer(difftime(italy_disaster3_1$Date[i], as.yearmon(disaster3_date, units = "weeks")))/31,0)
  
}
italy_disaster3_1_3 <- italy_disaster3_1 %>%filter(ord <=36 & ord >=- 36)
italy_disaster3_1_2 <- italy_disaster3_1 %>%filter(ord <=24 & ord >=- 24)
italy_disaster3_1_1 <- italy_disaster3_1 %>%filter(ord <=12 & ord >=- 12)


italy_disaster4_1 <- italy_disaster4 %>% filter(treatcon == 1)
italy_disaster4_1$treatmentstatus <- ifelse(is.na(italy_disaster4_1$tender_publications_firstCallForTenderDate), ifelse(italy_disaster4_1$tender_publications_firstdContractAwardDate >= disaster4_date,1,0),ifelse(italy_disaster4_1$tender_publications_firstCallForTenderDate >= disaster4_date,1,0))
italy_disaster4_1$ord <- NULL
for(i in 1:nrow(italy_disaster4_1)){
  italy_disaster4_1$ord[i] <- round(as.integer(difftime(italy_disaster4_1$Date[i], as.yearmon(disaster4_date, units = "weeks")))/31,0)
  
}
italy_disaster4_1_3 <- italy_disaster4_1 %>%filter(ord <=36 & ord >=- 36)
italy_disaster4_1_2 <- italy_disaster4_1 %>%filter(ord <=24 & ord >=- 24)
italy_disaster4_1_1 <- italy_disaster4_1 %>%filter(ord <=12 & ord >=- 12)


italy_disaster5_1 <- italy_disaster5 %>% filter(treatcon == 1)
italy_disaster5_1$treatmentstatus <- ifelse(is.na(italy_disaster5_1$tender_publications_firstCallForTenderDate), ifelse(italy_disaster5_1$tender_publications_firstdContractAwardDate >= disaster5_date,1,0),ifelse(italy_disaster5_1$tender_publications_firstCallForTenderDate >= disaster5_date,1,0))
italy_disaster5_1$ord <- NULL
for(i in 1:nrow(italy_disaster5_1)){
  italy_disaster5_1$ord[i] <- round(as.integer(difftime(italy_disaster5_1$Date[i], as.yearmon(disaster5_date, units = "weeks")))/31,0)
  
}
italy_disaster5_1_3 <- italy_disaster5_1 %>%filter(ord <=36 & ord >=- 36)
italy_disaster5_1_2 <- italy_disaster5_1 %>%filter(ord <=24 & ord >=- 24)
italy_disaster5_1_1 <- italy_disaster5_1 %>%filter(ord <=12 & ord >=- 12)

##3 year Regressions ----
italy_reg3<- rbind(italy_disaster1_1_3,italy_disaster2_1_3,italy_disaster3_1_3,italy_disaster4_1_3, italy_disaster5_1_3)
italy_reg3$contractvalue <- ifelse(is.na(italy_reg3$tender_finalPrice_EUR), ifelse(is.na(italy_reg3$tender_estimatedPrice_EUR),"",italy_reg3$tender_estimatedPrice_EUR),italy_reg3$tender_finalPrice_EUR)
italy_reg3$log_contractvalue <- log(as.numeric(as.character(italy_reg3$contractvalue)))

italy_reg3<- italy_reg3 %>% filter(!is.na(log_contractvalue))

model3<- lm(totalbidders ~ treatmentstatus + contractmonth + contractyear + factor(cpv) + log_contractvalue + buyer_buyerType,  data = italy_reg3)
summary.lm(model3)


##2 year Regressions ----
italy_reg2<- rbind(italy_disaster1_1_2,italy_disaster2_1_2,italy_disaster3_1_2,italy_disaster4_1_2, italy_disaster5_1_2)
italy_reg2$contractvalue <- ifelse(is.na(italy_reg2$tender_finalPrice_EUR), ifelse(is.na(italy_reg2$tender_estimatedPrice_EUR),"",italy_reg2$tender_estimatedPrice_EUR),italy_reg2$tender_finalPrice_EUR)
italy_reg2$log_contractvalue <- log(as.numeric(as.character(italy_reg2$contractvalue)))

italy_reg2<- italy_reg2 %>% filter(!is.na(log_contractvalue))

model2_ols<- lm(totalbidders ~ treatmentstatus + contractmonth + contractyear + factor(cpv) + log_contractvalue + buyer_buyerType, data = italy_reg2)
summary.lm(model2_ols)

##1 year Regressions ----
italy_reg1<- rbind(italy_disaster1_1_1,italy_disaster2_1_1,italy_disaster3_1_1,italy_disaster4_1_1, italy_disaster5_1_1)
italy_reg1$contractvalue <- ifelse(is.na(italy_reg1$tender_finalPrice_EUR), ifelse(is.na(italy_reg1$tender_estimatedPrice_EUR),"",italy_reg1$tender_estimatedPrice_EUR),italy_reg1$tender_finalPrice_EUR)
italy_reg1$log_contractvalue <- log(as.numeric(as.character(italy_reg1$contractvalue)))
italy_reg1<- italy_reg1 %>% filter(!is.na(log_contractvalue))

model1_ols<- lm(totalbidders ~ treatmentstatus + contractmonth + contractyear + factor(cpv) + log_contractvalue + buyer_buyerType, data = italy_reg1)
summary.lm(model1_ols)


##Average number of bidders ----
italy_reg3$bidders <- ifelse(italy_reg3$lot_bidsCount >= 20, 20, italy_reg3$lot_bidsCount)
italy_reg2$bidders <- ifelse(italy_reg2$lot_bidsCount >= 20, 20, italy_reg2$lot_bidsCount)
italy_reg1$bidders <- ifelse(italy_reg1$lot_bidsCount >= 20, 20, italy_reg1$lot_bidsCount)

model3_logit_avg<- lm(bidders ~ treatmentstatus + contractmonth + contractyear + factor(cpv) + log_contractvalue + buyer_buyerType, data = italy_reg3)
summary.lm(model3_logit_avg)




