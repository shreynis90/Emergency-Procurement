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

memory.limit(size = 30000)
gc()
italy_main<- read_excel("data_09_20.xlsx")
italy_main<-as.data.frame(italy_main)

##Fixing the issue with dates before 2011----
italy_main$tender_publications_firstCallForTenderDate <- as.POSIXct(italy_main$tender_publications_firstCallForTenderDate,format='%Y/%m/%d')
italy_main$tender_publications_firstdContractAwardDate <- as.POSIXct(italy_main$tender_publications_firstdContractAwardDate,format='%Y/%m/%d')
italy_main$tender_bidDeadline <- as.POSIXct(italy_main$tender_bidDeadline,format='%Y/%m/%d')
italy_main$contractDate<- fifelse(is.na(italy_main$tender_publications_firstCallForTenderDate),fifelse(is.na(italy_main$tender_publications_firstdContractAwardDate),as.POSIXct("2000-01-01", "%Y-%m-%d",tz="GMT"),italy_main$tender_publications_firstdContractAwardDate),italy_main$tender_publications_firstCallForTenderDate)
italy_temp <- italy_main %>% filter(contractDate> as.POSIXct("2011-01-01", "%Y-%m-%d",tz="GMT"))
diff<- median(round(difftime(italy_temp$tender_bidDeadline, italy_temp$tender_publications_firstCallForTenderDate, units = "days"),0), na.rm =  TRUE)
italy_main$tender_publications_firstCallForTenderDate <- fifelse(italy_main$contractDate<"2011-01-01", italy_main$tender_bidDeadline - days(diff), italy_main$tender_publications_firstCallForTenderDate)
italy_main<- italy_main %>% filter(contractDate> as.POSIXct("2000-01-01", "%Y-%m-%d",tz="GMT"))
italy<- italy_main

##Changing the dependent variable so that a higher number indicates greater corruption risk
italy$tender_indicator_INTEGRITY_PROCEDURE_TYPE<- ifelse(italy$tender_indicator_INTEGRITY_PROCEDURE_TYPE == 0,1,0)

#Disaster 1 ----
italy_disaster1<- italy %>%
  filter(disnumber == "Disaster_001"|is.na(disnumber))
##

italy_disaster1 <- data.frame(italy_disaster1)
italy_disaster1$buyer_buyerType <- as.factor(italy_disaster1$buyer_buyerType) #Declaring buyer_buyerType as a factor
italy_disaster1$tender_mainCpv <- as.integer(italy_disaster1$tender_mainCpv)
italy_disaster1$tender_mainCpv <- sub("^(\\d{2}).*$", "\\1", italy_disaster1$tender_mainCpv) #Retaining only the first three digits of the the CPV code
italy_disaster1$tender_mainCpv <- as.integer(italy_disaster1$tender_mainCpv)
italy_disaster1$tender_mainCpv

##Fixing the dates
italy_disaster1$contractyear<- ifelse(is.na(italy_disaster1$tender_publications_firstCallForTenderDate), substring(italy_disaster1$tender_publications_firstdContractAwardDate,1,4),substring(italy_disaster1$tender_publications_firstCallForTenderDate,1,4)) #Contract year
italy_disaster1$contractmonth <- ifelse(is.na(italy_disaster1$tender_publications_firstCallForTenderDate), substring(italy_disaster1$tender_publications_firstdContractAwardDate,6,7),substring(italy_disaster1$tender_publications_firstCallForTenderDate,6,7)) #Contract month
italy_disaster1<- italy_disaster1 %>% filter(contractyear>=2008 & contractyear<2013) #Range of the Data (Full data goes from 2006 to 2020)
##
disaster1_date <- as.POSIXct("2009-10-02")
italy_disaster1 <- subset(italy_disaster1, !(is.na(tender_publications_firstCallForTenderDate) & contractyear<2011))

italy_disaster1$Date <- as.yearmon(paste(italy_disaster1$contractmonth, italy_disaster1$contractyear), "%m %Y")
ncontracts1 <- italy_disaster1 %>% filter(treatcon == 1) %>% group_by(Date, treatcon, tender_indicator_INTEGRITY_PROCEDURE_TYPE)%>% mutate(numberofcontracts = n()) %>% select(Date, numberofcontracts, treatcon, disnumber) %>% distinct()

ncontracts1

##disaster2---- 
italy_disaster2<- italy %>%
  filter(disnumber == "Disaster_002"|is.na(disnumber))
##

italy_disaster2 <- data.frame(italy_disaster2)
italy_disaster2$buyer_buyerType <- as.factor(italy_disaster2$buyer_buyerType) #Declaring buyer_buyerType as a factor
italy_disaster2$tender_mainCpv <- as.integer(italy_disaster2$tender_mainCpv)
italy_disaster2$tender_mainCpv <- sub("^(\\d{2}).*$", "\\1", italy_disaster2$tender_mainCpv) #Retaining only the first three digits of the the CPV code
italy_disaster2$tender_mainCpv <- as.integer(italy_disaster2$tender_mainCpv)
italy_disaster2$tender_mainCpv

##Fixing the dates
italy_disaster2$contractyear<- ifelse(is.na(italy_disaster2$tender_publications_firstCallForTenderDate), substring(italy_disaster2$tender_publications_firstdContractAwardDate,1,4),substring(italy_disaster2$tender_publications_firstCallForTenderDate,1,4)) #Contract year
italy_disaster2$contractmonth <- ifelse(is.na(italy_disaster2$tender_publications_firstCallForTenderDate), substring(italy_disaster2$tender_publications_firstdContractAwardDate,6,7),substring(italy_disaster2$tender_publications_firstCallForTenderDate,6,7)) #Contract month
italy_disaster2<- italy_disaster2 %>% filter(contractyear>=2009 & contractyear<2016) #Range of the Data (Full data goes from 2006 to 2020)
##
italy_disaster2 <- subset(italy_disaster2, !(is.na(tender_publications_firstCallForTenderDate) & contractyear<2011))
disaster2_date <- as.POSIXct("2012-05-29")

###Number of Contracts ----
italy_disaster2$Date <- as.yearmon(paste(italy_disaster2$contractmonth, italy_disaster2$contractyear), "%m %Y")
ncontracts2 <- italy_disaster2 %>% filter(treatcon == 1) %>% group_by(Date,treatcon,tender_indicator_INTEGRITY_PROCEDURE_TYPE
)%>% mutate(numberofcontracts = n()) %>% select(Date, numberofcontracts, treatcon, disnumber) %>% distinct()

##Disaster 3 ----
#Removing contracts from other disaster areas#
italy_disaster3<- italy %>%
  filter(disnumber == "Disaster_003"|is.na(disnumber))
##

italy_disaster3 <- data.frame(italy_disaster3)
italy_disaster3$buyer_buyerType <- as.factor(italy_disaster3$buyer_buyerType) #Declaring buyer_buyerType as a factor
italy_disaster3$tender_mainCpv <- as.integer(italy_disaster3$tender_mainCpv)
italy_disaster3$tender_mainCpv <- sub("^(\\d{2}).*$", "\\1", italy_disaster3$tender_mainCpv) #Retaining only the first three digits of the the CPV code
italy_disaster3$tender_mainCpv <- as.integer(italy_disaster3$tender_mainCpv)
italy_disaster3$tender_mainCpv

##Fixing the dates
italy_disaster3$contractyear<- ifelse(is.na(italy_disaster3$tender_publications_firstCallForTenderDate), substring(italy_disaster3$tender_publications_firstdContractAwardDate,1,4),substring(italy_disaster3$tender_publications_firstCallForTenderDate,1,4)) #Contract year
italy_disaster3$contractmonth <- ifelse(is.na(italy_disaster3$tender_publications_firstCallForTenderDate), substring(italy_disaster3$tender_publications_firstdContractAwardDate,6,7),substring(italy_disaster3$tender_publications_firstCallForTenderDate,6,7)) #Contract month
italy_disaster3<- italy_disaster3 %>% filter(contractyear>=2010 & contractyear<2017) #Range of the Data (Full data goes from 2006 to 2020)
##
italy_disaster3 <- subset(italy_disaster3, !(is.na(tender_publications_firstCallForTenderDate) & contractyear<2011))
disaster3_date <- as.POSIXct("2013-11-18")

###Number of Contracts ----
italy_disaster3$Date <- as.yearmon(paste(italy_disaster3$contractmonth, italy_disaster3$contractyear), "%m %Y")
ncontracts3 <- italy_disaster3 %>% filter(treatcon == 1) %>% group_by(Date, treatcon,tender_indicator_INTEGRITY_PROCEDURE_TYPE
)%>% mutate(numberofcontracts = n()) %>% select(Date, numberofcontracts, treatcon, disnumber) %>% distinct()


##Disaster 4 ----
#Removing contracts from other disaster areas#
italy_disaster4<- italy %>%
  filter(disnumber == "Disaster_004"|is.na(disnumber))
##

italy_disaster4 <- data.frame(italy_disaster4)
italy_disaster4$buyer_buyerType <- as.factor(italy_disaster4$buyer_buyerType) #Declaring buyer_buyerType as a factor
italy_disaster4$tender_mainCpv <- as.integer(italy_disaster4$tender_mainCpv)
italy_disaster4$tender_mainCpv <- sub("^(\\d{2}).*$", "\\1", italy_disaster4$tender_mainCpv) #Retaining only the first three digits of the the CPV code
italy_disaster4$tender_mainCpv <- as.integer(italy_disaster4$tender_mainCpv)
italy_disaster4$tender_mainCpv

##Fixing the dates
italy_disaster4$contractyear<- ifelse(is.na(italy_disaster4$tender_publications_firstCallForTenderDate), substring(italy_disaster4$tender_publications_firstdContractAwardDate,1,4),substring(italy_disaster4$tender_publications_firstCallForTenderDate,1,4)) #Contract year
italy_disaster4$contractmonth <- ifelse(is.na(italy_disaster4$tender_publications_firstCallForTenderDate), substring(italy_disaster4$tender_publications_firstdContractAwardDate,6,7),substring(italy_disaster4$tender_publications_firstCallForTenderDate,6,7)) #Contract month
italy_disaster4<- italy_disaster4 %>% filter(contractyear>=2013 & contractyear<2020) #Range of the Data (Full data goes from 2006 to 2020)
##
italy_disaster4 <- subset(italy_disaster4, !(is.na(tender_publications_firstCallForTenderDate) & contractyear<2011))
disaster4_date <- as.POSIXct("2016-08-24")

###Number of Contracts ----
italy_disaster4$Date <- as.yearmon(paste(italy_disaster4$contractmonth, italy_disaster4$contractyear), "%m %Y")
ncontracts4 <- italy_disaster4 %>% filter(treatcon == 1) %>% group_by(Date, treatcon,tender_indicator_INTEGRITY_PROCEDURE_TYPE)%>% mutate(numberofcontracts = n()) %>% select(Date, numberofcontracts, treatcon, disnumber) %>% distinct()


##Disaster 5----
#Removing contracts from other disaster areas#
italy_disaster5<- italy %>%
  filter(disnumber == "Disaster_005"|is.na(disnumber))
##

italy_disaster5 <- data.frame(italy_disaster5)
italy_disaster5$buyer_buyerType <- as.factor(italy_disaster5$buyer_buyerType) #Declaring buyer_buyerType as a factor
italy_disaster5$tender_mainCpv <- as.integer(italy_disaster5$tender_mainCpv)
italy_disaster5$tender_mainCpv <- sub("^(\\d{2}).*$", "\\1", italy_disaster5$tender_mainCpv) #Retaining only the first three digits of the the CPV code
italy_disaster5$tender_mainCpv <- as.integer(italy_disaster5$tender_mainCpv)
italy_disaster5$tender_mainCpv

##Fixing the dates
italy_disaster5$contractyear<- ifelse(is.na(italy_disaster5$tender_publications_firstCallForTenderDate), substring(italy_disaster5$tender_publications_firstdContractAwardDate,1,4),substring(italy_disaster5$tender_publications_firstCallForTenderDate,1,4)) #Contract year
italy_disaster5$contractmonth <- ifelse(is.na(italy_disaster5$tender_publications_firstCallForTenderDate), substring(italy_disaster5$tender_publications_firstdContractAwardDate,6,7),substring(italy_disaster5$tender_publications_firstCallForTenderDate,6,7)) #Contract month
italy_disaster5<- italy_disaster5 %>% filter(contractyear>=2014 & contractyear<=2020) #Range of the Data (Full data goes from 2006 to 2020)
##
italy_disaster5 <- subset(italy_disaster5, !(is.na(tender_publications_firstCallForTenderDate) & contractyear<2011))
disaster5_date <- as.POSIXct("2017-01-18")

###Number of Contracts ----
italy_disaster5$Date <- as.yearmon(paste(italy_disaster5$contractmonth, italy_disaster5$contractyear), "%m %Y")
ncontracts5 <- italy_disaster5 %>% filter(treatcon == 1) %>% group_by(Date, treatcon,tender_indicator_INTEGRITY_PROCEDURE_TYPE)%>% mutate(numberofcontracts = n()) %>% select(Date, numberofcontracts, treatcon, disnumber) %>% distinct()


ncontracts2$time <- NA
ncontracts3$time <- NA
ncontracts4$time <- NA
ncontracts4$time <- NA
ncontracts1$time <- NA
ncontracts1$ord<-NA
ncontracts2$ord<- NA
ncontracts3$ord<- NA
ncontracts4$ord<- NA
ncontracts5$ord<- NA
ncontracts5$time<- NA
for (i in 1: nrow(ncontracts1)) {
  if(ncontracts1$Date[i]< as.yearmon(disaster1_date, "%Y %m")){
    ncontracts1$time[i]<-paste0("t", round(as.integer(difftime(ncontracts1$Date[i], as.yearmon(disaster1_date, units = "weeks")))/31,0))
    ncontracts1$ord[i] <- round(as.integer(difftime(ncontracts1$Date[i], as.yearmon(disaster1_date, units = "weeks")))/31,0)
  }
  if(ncontracts1$Date[i]> as.yearmon(disaster1_date, "%Y %m")){
    ncontracts1$time[i]<-paste0("t+", round(as.integer(difftime(ncontracts1$Date[i], as.yearmon(disaster1_date, units = "weeks")))/31,0))
    ncontracts1$ord[i] <- round(as.integer(difftime(ncontracts1$Date[i], as.yearmon(disaster1_date, units = "weeks")))/31,0)
  }
  if(ncontracts1$Date[i] == as.yearmon(disaster1_date, "%Y %m")){
    ncontracts1$time[i]<-paste0("t = 0")
    ncontracts1$ord[i] <- "0"}
}

ncontracts1<-ncontracts1[order(ncontracts1$Date),]
ncontracts1

for (i in 1: nrow(ncontracts2)) {
  if(ncontracts2$Date[i]< as.yearmon(disaster2_date, "%Y %m")){
    ncontracts2$time[i]<-paste0("t", round(as.integer(difftime(ncontracts2$Date[i], as.yearmon(disaster2_date, units = "weeks")))/31,0))
    ncontracts2$ord[i] <- round(as.integer(difftime(ncontracts2$Date[i], as.yearmon(disaster2_date, units = "weeks")))/31,0)
  }
  if(ncontracts2$Date[i]> as.yearmon(disaster2_date, "%Y %m")){
    ncontracts2$time[i]<-paste0("t+", round(as.integer(difftime(ncontracts2$Date[i], as.yearmon(disaster2_date, units = "weeks")))/31,0))
    ncontracts2$ord[i] <- round(as.integer(difftime(ncontracts2$Date[i], as.yearmon(disaster2_date, units = "weeks")))/31,0)
  }
  if(ncontracts2$Date[i] == as.yearmon(disaster2_date, "%Y %m")){
    ncontracts2$time[i]<-paste0("t = 0")
    ncontracts2$ord[i] <- "0"}
}
ncontracts2<-ncontracts2[order(ncontracts2$Date),]
ncontracts2

for (i in 1: nrow(ncontracts3)) {
  if(ncontracts3$Date[i]< as.yearmon(disaster3_date, "%Y %m")){
    ncontracts3$time[i]<-paste0("t", round(as.integer(difftime(ncontracts3$Date[i], as.yearmon(disaster3_date, units = "weeks")))/31,0))
    ncontracts3$ord[i] <- round(as.integer(difftime(ncontracts3$Date[i], as.yearmon(disaster3_date, units = "weeks")))/31,0)
  }
  if(ncontracts3$Date[i]> as.yearmon(disaster3_date, "%Y %m")){
    ncontracts3$time[i]<-paste0("t+", round(as.integer(difftime(ncontracts3$Date[i], as.yearmon(disaster3_date, units = "weeks")))/31,0))
    ncontracts3$ord[i] <- round(as.integer(difftime(ncontracts3$Date[i], as.yearmon(disaster3_date, units = "weeks")))/31,0)
  }
  if(ncontracts3$Date[i] == as.yearmon(disaster3_date, "%Y %m")){
    ncontracts3$time[i]<-"t = 0"
    ncontracts3$ord[i] <- "0"}
}
ncontracts3<-ncontracts3[order(ncontracts3$Date),]
ncontracts3

for (i in 1: nrow(ncontracts4)) {
  if(ncontracts4$Date[i]< as.yearmon(disaster4_date, "%Y %m")){
    ncontracts4$time[i]<-paste0("t", round(as.integer(difftime(ncontracts4$Date[i], as.yearmon(disaster4_date, units = "weeks")))/31,0))
    ncontracts4$ord[i] <- round(as.integer(difftime(ncontracts4$Date[i], as.yearmon(disaster4_date, units = "weeks")))/31,0)
  }
  if(ncontracts4$Date[i]> as.yearmon(disaster4_date, "%Y %m")){
    ncontracts4$time[i]<-paste0("t+", round(as.integer(difftime(ncontracts4$Date[i], as.yearmon(disaster4_date, units = "weeks")))/31,0))
    ncontracts4$ord[i] <- round(as.integer(difftime(ncontracts4$Date[i], as.yearmon(disaster4_date, units = "weeks")))/31,0)
  }
  if(ncontracts4$Date[i] == as.yearmon(disaster4_date, "%Y %m")){
    ncontracts4$ord[i] <- "0"
    ncontracts4$time[i]<-"t = 0"}
}
ncontracts4<-ncontracts4[order(ncontracts4$Date),]
ncontracts4

for (i in 1: nrow(ncontracts5)) {
  if(ncontracts5$Date[i]< as.yearmon(disaster5_date, "%Y %m")){
    ncontracts5$time[i]<-paste0("t", round(as.integer(difftime(ncontracts5$Date[i], as.yearmon(disaster5_date, units = "weeks")))/31,0))
    ncontracts5$ord[i] <- round(as.integer(difftime(ncontracts5$Date[i], as.yearmon(disaster5_date, units = "weeks")))/31,0)
  }
  if(ncontracts5$Date[i]> as.yearmon(disaster5_date, "%Y %m")){
    ncontracts5$time[i]<-paste0("t+", round(as.integer(difftime(ncontracts5$Date[i], as.yearmon(disaster5_date, units = "weeks")))/31,0))
    ncontracts5$ord[i] <- round(as.integer(difftime(ncontracts5$Date[i], as.yearmon(disaster5_date, units = "weeks")))/31,0)
  }
  if(ncontracts5$Date[i] == as.yearmon(disaster5_date, "%Y %m")){
    ncontracts5$time[i]<-"t = 0"
    ncontracts5$ord[i] <- "0"
  }
}
ncontracts5<-ncontracts5[order(ncontracts5$Date),]
ncontracts5

##Monthly Analysis ----
ncontracts<- rbind(ncontracts1, ncontracts2, ncontracts3, ncontracts4, ncontracts5)

ncontracts <- ncontracts %>% group_by(time, tender_indicator_INTEGRITY_PROCEDURE_TYPE)%>% mutate(numberofcontracts = sum(numberofcontracts)) %>% select(tender_indicator_INTEGRITY_PROCEDURE_TYPE,time, numberofcontracts, treatcon, ord) %>% distinct()

ncontracts<- as.data.frame(ncontracts)
ncontracts$ord<- as.numeric(as.character(ncontracts$ord))
ncontracts <- ncontracts%>% filter(ord >= -12 & ord<= 12)

ncontracts<-ncontracts[order(ncontracts$ord),]

ggplot(data=ncontracts, aes(x=fct_inorder(time), y=numberofcontracts, fill=as.factor(tender_indicator_INTEGRITY_PROCEDURE_TYPE))) + geom_bar(stat="identity")+  scale_fill_brewer(palette="Set1")+
  theme_minimal()+   theme(axis.text.x=element_text(angle=90,hjust=1)) +  geom_vline(xintercept=ncontracts$ord[25] ,linetype=1, colour="black")+ labs(title="Monthly Number of Contracts By Open Vs Non-open Procedure", x="Dates (unit in Months)", y = "Monthly number of contracts" , subtitle="T = 0 depicts disaster incidence", fill = "1: Non Open Procedure")

#Quarterly Analysis ----

ncontracts<- NULL
ncontracts<- rbind(ncontracts1, ncontracts2, ncontracts3, ncontracts4, ncontracts5)
ncontracts<- as.data.frame(ncontracts)
ncontracts$ord<- as.numeric(as.character(ncontracts$ord))
ncontracts$numberofcontracts<- as.numeric(ncontracts$numberofcontracts)

ncontracts <- ncontracts %>% group_by(time, tender_indicator_INTEGRITY_PROCEDURE_TYPE)%>% mutate(numberofcontracts = sum(numberofcontracts)) %>% select(tender_indicator_INTEGRITY_PROCEDURE_TYPE,time, numberofcontracts, treatcon, ord) %>% distinct()

ncontracts <- ncontracts%>% filter(ord >= -36 & ord<= 36)

k<- table(ncontracts$ord)
k<- as.data.frame(k)
k$Var1<- as.numeric(as.character(k$Var1))
k
a<- NULL
for (j in 1:nrow(k)) {
  
  if(k$Freq[j] == 1)
  {
    if(k$Var1[j]< 0){
      t<- paste0("t", k$Var1[j])  
    }
    if(k$Var1[j]> 0){
      t<- paste0("t+", k$Var1[j])  
    }
    a<- rbind(a, c(tender_indicator_INTEGRITY_PROCEDURE_TYPE = 1, time = t, numberofcontracts = 0,treatcon =1, ord=k$Var1[j]))
  }
}
a<- as.data.frame(a)
a$tender_indicator_INTEGRITY_PROCEDURE_TYPE<- as.numeric(as.character(a$tender_indicator_INTEGRITY_PROCEDURE_TYPE))
a$time<- as.character(a$time)
a$numberofcontracts<- as.numeric(as.character(a$numberofcontracts))
a$treatcon<- as.numeric(as.character(a$treatcon))
a$ord<- as.numeric(as.character(a$ord))
ncontracts<- bind_rows(ncontracts, a)
ncontracts<-ncontracts[order(ncontracts$ord),]
ncontracts<- as.data.frame(ncontracts)
ncontracts$date<- NA

l<- ifelse(ncontracts$ord - 0 == -1, 1, 0)
tr<- max(which(l == 1))

for (i in 1:nrow(ncontracts)) {
  if(ncontracts$ord[i] %% 3 == 1 & ncontracts$ord[i]> 0){
    for(j in i:(i + 6)){
      ncontracts$date[j] <- paste0("q",round(ncontracts$ord[i]/3 +1))
    }
  }
  if(ncontracts$ord[i]==0){
    ncontracts$date[i] <- "Disaster month"
  }
}

for (i in tr:1) {
  if(abs(ncontracts$ord[i]) %% 3 == 1 & ncontracts$ord[i]<0){
    for(j in i:(i - 5)){
      ncontracts$date[j] <- paste0("q-",round(abs(ncontracts$ord[i])/3+1,0))
    }
  }
}

ncontracts <- ncontracts %>% group_by(date, tender_indicator_INTEGRITY_PROCEDURE_TYPE)%>% mutate(numberofcontracts = round(sum(numberofcontracts),3)) %>% select(tender_indicator_INTEGRITY_PROCEDURE_TYPE,date, numberofcontracts, treatcon) %>% distinct()

ncontracts <- ncontracts %>% group_by(date)%>% mutate(share = round(numberofcontracts/sum(numberofcontracts),2)) %>% select(tender_indicator_INTEGRITY_PROCEDURE_TYPE,date, numberofcontracts, treatcon, share) %>% distinct()

ggplot(data=ncontracts, aes(x=fct_inorder(date), y=share, fill=as.factor(tender_indicator_INTEGRITY_PROCEDURE_TYPE))) + geom_bar(stat="identity")+  scale_fill_brewer(palette="Dark2")+
  theme_minimal()+   theme(axis.text.x=element_text(angle=90,hjust=1)) +  geom_vline(xintercept=ncontracts$ord[25] ,linetype=1, colour="black")+ labs(title="Quarterly Share of Contracts By Procedure Type", x="Dates (unit in Quarters)", y = "Quarterly Share of Contracts" , subtitle="Disaster Month depicts the share of contracts in the Disaster Month", fill = "1: Non-Open Procedure")

ggplot(data=ncontracts, aes(x=fct_inorder(date), y=numberofcontracts, fill=as.factor(tender_indicator_INTEGRITY_PROCEDURE_TYPE))) + geom_bar(stat="identity")+  scale_fill_brewer(palette="Dark2")+
  theme_minimal()+   theme(axis.text.x=element_text(angle=90,hjust=1)) +  geom_vline(xintercept=ncontracts$ord[25] ,linetype=1, colour="black")+ labs(title="Quarterly Share of Contracts By Procedure Type", x="Dates (unit in Quarters)", y = "Quarterly Number of Contracts" , subtitle="Disaster Month depicts the share of contracts in the Disaster Month", fill = "1: Non-Open Procedure")


##T-tests ----
ncontracts_before_3 <- ncontracts[c(1:24),]
ncontracts_after_3 <- ncontracts[c(27:50),]
ncontracts_before_3.gr1 <- ncontracts_before_3 %>% filter(tender_indicator_INTEGRITY_PROCEDURE_TYPE == 1)
ncontracts_after_3.gr1 <- ncontracts_after_3 %>% filter(tender_indicator_INTEGRITY_PROCEDURE_TYPE == 1)
mean(ncontracts_before_3.gr1$share)
mean(ncontracts_after_3.gr1$share)

t3<-t.test(ncontracts_before_3.gr1$share , ncontracts_after_3.gr1$share, paired = TRUE, conf.level = 0.95)
t3


ncontracts_before_2 <- ncontracts[c(9:24),]
ncontracts_after_2 <- ncontracts[c(27:42),]
ncontracts_before_2.gr1 <- ncontracts_before_2 %>% filter(tender_indicator_INTEGRITY_PROCEDURE_TYPE == 1)
ncontracts_after_2.gr1 <- ncontracts_after_2 %>% filter(tender_indicator_INTEGRITY_PROCEDURE_TYPE == 1)
mean(ncontracts_before_2.gr1$share)
mean(ncontracts_after_2.gr1$share)

t2<-t.test(ncontracts_before_2.gr1$share , ncontracts_after_2.gr1$share, paired = TRUE, conf.level = 0.95)
t2

ncontracts_before_1 <- ncontracts[c(17:24),]
ncontracts_after_1 <- ncontracts[c(27:34),]

ncontracts_before_1.gr1 <- ncontracts_before_1 %>% filter(tender_indicator_INTEGRITY_PROCEDURE_TYPE == 1)
ncontracts_after_1.gr1 <- ncontracts_after_1 %>% filter(tender_indicator_INTEGRITY_PROCEDURE_TYPE == 1)
mean(ncontracts_before_1.gr1$share)
mean(ncontracts_after_1.gr1$share)

t1<-t.test(ncontracts_before_1.gr1$share , ncontracts_after_1.gr1$share, paired = TRUE, conf.level = 0.95)
t1
