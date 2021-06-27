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
library(modEvA)
library(mfx)


memory.limit(size = 30000)
gc()
italy<- read_excel("data_09_20.xlsx")
italy<-as.data.frame(italy)
italy$tender_publications_firstCallForTenderDate <- as.POSIXct(italy$tender_publications_firstCallForTenderDate,format='%Y/%m/%d')
italy$tender_publications_firstdContractAwardDate <- as.POSIXct(italy$tender_publications_firstdContractAwardDate,format='%Y/%m/%d')
italy$tender_bidDeadline <- as.POSIXct(italy$tender_bidDeadline,format='%Y/%m/%d')

##Fixing the issue with Dates ----
italy$contractDate<- fifelse(is.na(italy$tender_publications_firstCallForTenderDate),fifelse(is.na(italy$tender_publications_firstdContractAwardDate),as.POSIXct("2000-01-01", "%Y-%m-%d",tz="GMT"),italy$tender_publications_firstdContractAwardDate),italy$tender_publications_firstCallForTenderDate)
italy_temp <- italy %>% filter(contractDate> as.POSIXct("2011-01-01", "%Y-%m-%d",tz="GMT"))
diff<- median(round(difftime(italy_temp$tender_bidDeadline, italy_temp$tender_publications_firstCallForTenderDate, units = "days"),0), na.rm =  TRUE)
italy$tender_publications_firstCallForTenderDate <- fifelse(italy$contractDate<"2011-01-01", italy$tender_bidDeadline - days(diff), italy$tender_publications_firstCallForTenderDate)
italy<- italy %>% filter(contractDate> as.POSIXct("2000-01-01", "%Y-%m-%d",tz="GMT"))

#Dropping missings and creating a new variable for advertisement integrity ----
italy<- italy %>% filter(!is.na(tender_indicator_INTEGRITY_ADVERTISEMENT_PERIOD))
italy$advertintegrity <- ifelse(italy$tender_indicator_INTEGRITY_ADVERTISEMENT_PERIOD == 0, 1, 0)

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
ncontracts1 <- italy_disaster1 %>% filter(treatcon == 1) %>% group_by(Date, treatcon, advertintegrity)%>% mutate(numberofcontracts = n()) %>% select(Date, numberofcontracts, treatcon, disnumber, advertintegrity) %>% distinct()

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
ncontracts2 <- italy_disaster2 %>% filter(treatcon == 1) %>% group_by(Date, advertintegrity)%>% mutate(numberofcontracts = n()) %>% select(Date, numberofcontracts, treatcon, disnumber, advertintegrity) %>% distinct()

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
ncontracts3 <- italy_disaster3 %>% filter(treatcon == 1) %>% group_by(Date, treatcon, advertintegrity)%>% mutate(numberofcontracts = n()) %>% select(Date, numberofcontracts, treatcon, disnumber, advertintegrity) %>% distinct()


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
ncontracts4 <- italy_disaster4 %>% filter(treatcon == 1) %>% group_by(Date, treatcon, advertintegrity)%>% mutate(numberofcontracts = n()) %>% select(Date, numberofcontracts, treatcon, disnumber, advertintegrity) %>% distinct()


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
ncontracts5 <- italy_disaster5 %>% filter(treatcon == 1) %>% group_by(Date, treatcon, advertintegrity)%>% mutate(numberofcontracts = n()) %>% select(Date, numberofcontracts, treatcon, disnumber, advertintegrity) %>% distinct()


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

ncontracts<- rbind(ncontracts1, ncontracts2, ncontracts3, ncontracts4, ncontracts5)

ncontracts <- ncontracts %>% group_by(time,advertintegrity)%>% mutate(numberofcontracts = sum(numberofcontracts)) %>% select(time, numberofcontracts, treatcon, ord, advertintegrity) %>% distinct()
ncontracts<- as.data.frame(ncontracts)
ncontracts$ord<- as.numeric(as.character(ncontracts$ord))
ncontracts <- ncontracts%>% filter(ord >= -12 & ord<= 12)

ncontracts<-ncontracts[order(ncontracts$ord),]

ggplot(data=ncontracts, aes(x=fct_inorder(time), y=numberofcontracts)) + geom_bar(stat="identity")+  scale_fill_brewer(palette="Paired")+
  theme_minimal()+   theme(axis.text.x=element_text(angle=90,hjust=1)) +  geom_vline(xintercept=ncontracts$ord[11] ,linetype=1, colour="black")+ labs(title="Monthly Number of Contracts", x="Dates (unit in Months)", y = "Monthly number of contracts" , subtitle="T = 0 depicts disaster incidence", fill = "Share of Each Disaster")


ggplot(data=ncontracts, aes(x=fct_inorder(time), y=numberofcontracts, fill=as.factor(advertintegrity))) + geom_bar(stat="identity")+  scale_fill_brewer(palette="Set2")+
  theme_minimal()+   theme(axis.text.x=element_text(angle=90,hjust=1)) +  geom_vline(xintercept=ncontracts$ord[11] ,linetype=1, colour="black")+ labs(title="Monthly Number of Contracts by Advertisement Integrity", x="Dates (unit in Months)", y = "Monthly number of contracts" , subtitle="T = 0 depicts disaster incidence", fill = "0: Risky Advertisement Period")



ncontracts<- NULL
ncontracts<- rbind(ncontracts1, ncontracts2, ncontracts3, ncontracts4, ncontracts5)
ncontracts<- as.data.frame(ncontracts)
ncontracts$ord<- as.numeric(as.character(ncontracts$ord))
ncontracts$numberofcontracts<- as.numeric(ncontracts$numberofcontracts)

ncontracts <- ncontracts %>% group_by(time, advertintegrity)%>% mutate(numberofcontracts = sum(numberofcontracts)) %>% select(advertintegrity,time, numberofcontracts, treatcon, ord) %>% distinct()

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
    a<- rbind(a, c(advertintegrity = 1, time = t, numberofcontracts = 0,treatcon =1, ord=k$Var1[j]))
  }
}
a<- as.data.frame(a)
a$advertintegrity<- as.numeric(as.character(a$advertintegrity))
a$time<- as.character(a$time)
a$numberofcontracts<- as.numeric(as.character(a$numberofcontracts))
a$treatcon<- as.numeric(as.character(a$treatcon))
a$ord<- as.numeric(as.character(a$ord))
ncontracts<- bind_rows(ncontracts, a)
ncontracts<-ncontracts[order(ncontracts$ord),]
ncontracts<- as.data.frame(ncontracts)
ncontracts$date<- NULL

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
      ncontracts$date[j] <- paste0("q-",round(abs(ncontracts$ord[i])/3+1))
    }
  }
}

ncontracts <- ncontracts %>% group_by(date, advertintegrity)%>% mutate(numberofcontracts = round(sum(numberofcontracts),2)) %>% select(advertintegrity,date, numberofcontracts, treatcon) %>% distinct()

ncontracts <- ncontracts %>% group_by(date)%>% mutate(share = round(numberofcontracts/sum(numberofcontracts),2)) %>% select(advertintegrity,date, numberofcontracts, treatcon, share) %>% distinct()

ggplot(data=ncontracts, aes(x=fct_inorder(date), y=share, fill=as.factor(advertintegrity))) + geom_bar(stat="identity")+  scale_fill_brewer(palette="Dark2")+
  theme_minimal()+   theme(axis.text.x=element_text(angle=90,hjust=1))+ labs(title="Quarterly Share of Contracts By Risky Advertisement", x="Dates (unit in Quarters)", y = "Quarterly Share of Contracts" , subtitle="Disaster Month depicts the share of contracts in the Disaster Month", fill = "1: Risky Advert Period")

ggplot(data=ncontracts, aes(x=fct_inorder(date), y=numberofcontracts, fill=as.factor(advertintegrity))) + geom_bar(stat="identity")+  scale_fill_brewer(palette="Dark2")+
  theme_minimal()+   theme(axis.text.x=element_text(angle=90,hjust=1)) + labs(title="Quarterly Number of Contracts By Risky Advertisement Period", x="Dates (unit in Quarters)", y = "Quarterly Number of Contracts" , subtitle="Disaster Month depicts the share of contracts in the Disaster Month", fill = "1: Risky Advert Period")

ncontracts_before_3 <- ncontracts[c(1:24),]
ncontracts_after_3 <- ncontracts[c(27:50),]
ncontracts_before_3.gr1 <- ncontracts_before_3 %>% filter(advertintegrity == 1)
ncontracts_after_3.gr1 <- ncontracts_after_3 %>% filter(advertintegrity == 1)
mean(ncontracts_before_3.gr1$share)
mean(ncontracts_after_3.gr1$share)

t3<-t.test(ncontracts_before_3.gr1$share , ncontracts_after_3.gr1$share, paired = TRUE, conf.level = 0.95)
t3


ncontracts_before_2 <- ncontracts[c(9:24),]
ncontracts_after_2 <- ncontracts[c(27:42),]
ncontracts_before_2.gr1 <- ncontracts_before_2 %>% filter(advertintegrity == 1)
ncontracts_after_2.gr1 <- ncontracts_after_2 %>% filter(advertintegrity == 1)
mean(ncontracts_before_2.gr1$share)
mean(ncontracts_after_2.gr1$share)

t2<-t.test(ncontracts_before_2.gr1$share , ncontracts_after_2.gr1$share, paired = TRUE, conf.level = 0.95)
t2

ncontracts_before_1 <- ncontracts[c(17:24),]
ncontracts_after_1 <- ncontracts[c(27:34),]

ncontracts_before_1.gr1 <- ncontracts_before_1 %>% filter(advertintegrity == 1)
ncontracts_after_1.gr1 <- ncontracts_after_1 %>% filter(advertintegrity == 1)
mean(ncontracts_before_1.gr1$share)
mean(ncontracts_after_1.gr1$share)

t1<-t.test(ncontracts_before_1.gr1$share , ncontracts_after_1.gr1$share, paired = TRUE, conf.level = 0.95)
t1

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
italy_reg3 <- italy_reg3 %>% filter(!is.na(log_contractvalue))
f<- chisq.test(italy_reg3$treatmentstatus, italy_reg3$advertintegrity, correct = FALSE)
f
f$observed

q3<- as.data.frame(table(italy_reg3$tender_mainCpv))
q3$Freq

cpv_list3 <- as.data.frame(q3$Var1[which(q3$Freq>= 1000)])

italy_reg3$newcpv<- NA
for(i in 1:nrow(italy_reg3)){
  for(j in 1:nrow(cpv_list3)){
    if(italy_reg3$tender_mainCpv[i] == cpv_list3[j,1]){
      italy_reg3$newcpv[i] <- italy_reg3$tender_mainCpv[i]
      break
    }
    if(italy_reg3$tender_mainCpv[i] != cpv_list3[j,1]){
      italy_reg3$newcpv[i] <- "Other"
    }
  }
}

italy_reg3$buyer<- ifelse(italy_reg3$buyer_buyerType=="REGIONAL_AUTHORITY"|italy_reg3$buyer_buyerType=="REGIONAL_AGENCY", "Regional", "Other")

write.csv(italy_reg3,"temp3.csv")

model3_logit<- glm(advertintegrity ~ treatmentstatus + factor(tender_mainCpv) + buyer_buyerType+ log_contractvalue + contractyear + contractmonth, family ="binomial", data = italy_reg3)
summary.glm(model3_logit)
RsqGLM(model3_logit)

model3_logit_small<- glm(advertintegrity ~ treatmentstatus + factor(newcpv) + buyer + log_contractvalue + contractyear + contractmonth, family ="binomial", data = italy_reg3)
summary.glm(model3_logit_small)
RsqGLM(model3_logit_small)

logitor(advertintegrity ~ treatmentstatus + contractmonth + contractyear + log_contractvalue + factor(newcpv) + buyer, data = italy_reg3)


model3_ols<- lm(advertintegrity ~ treatmentstatus + contractmonth + contractyear + factor(tender_mainCpv) + log_contractvalue + buyer_buyerType, data = italy_reg3)
summary.lm(model3_ols)

model3_ols_small<- lm(advertintegrity ~ treatmentstatus + contractmonth + contractyear + factor(newcpv) + log_contractvalue, data = italy_reg3)
summary.lm(model3_ols_small)


##2 year Regressions ----
italy_reg2<- rbind(italy_disaster1_1_2,italy_disaster2_1_2,italy_disaster3_1_2,italy_disaster4_1_2, italy_disaster5_1_2)
italy_reg2$contractvalue <- ifelse(is.na(italy_reg2$tender_finalPrice_EUR), ifelse(is.na(italy_reg2$tender_estimatedPrice_EUR),"",italy_reg2$tender_estimatedPrice_EUR),italy_reg2$tender_finalPrice_EUR)
italy_reg2$log_contractvalue <- log(as.numeric(as.character(italy_reg2$contractvalue)))
italy_reg2 <- italy_reg2 %>% filter(!is.na(log_contractvalue))
f<- chisq.test(italy_reg2$treatmentstatus, italy_reg2$advertintegrity, correct = FALSE)
f
f$observed


q2<- as.data.frame(table(italy_reg2$tender_mainCpv))
q2$Freq

cpv_list2 <- as.data.frame(q2$Var1[which(q2$Freq>= 1000)])

italy_reg2$newcpv<- NA
for(i in 1:nrow(italy_reg2)){
  for(j in 1:nrow(cpv_list2)){
    if(italy_reg2$tender_mainCpv[i] == cpv_list2[j,1]){
      italy_reg2$newcpv[i] <- italy_reg2$tender_mainCpv[i]
      break
    }
    if(italy_reg2$tender_mainCpv[i] != cpv_list2[j,1]){
      italy_reg2$newcpv[i] <- "Other"
    }
  }
}

italy_reg2$buyer<- ifelse(italy_reg2$buyer_buyerType=="REGIONAL_AUTHORITY"|italy_reg2$buyer_buyerType=="REGIONAL_AGENCY", "Regional", "Other")

write.csv(italy_reg2,"temp2.csv")

model2_logit<- glm(advertintegrity ~ treatmentstatus + log_contractvalue + contractyear + contractmonth + factor(tender_mainCpv) + buyer_buyerType, family ="binomial", data = italy_reg2)
summary.glm(model2_logit)
RsqGLM(model2_logit)

model2_ols<- lm(advertintegrity ~ treatmentstatus + factor(tender_mainCpv) + contractyear + contractmonth + log_contractvalue + buyer_buyerType, data = italy_reg2)
summary.lm(model2_ols)

model2_logit_small<- glm(advertintegrity ~ treatmentstatus + log_contractvalue + contractyear + contractmonth + factor(newcpv) + buyer, family ="binomial", data = italy_reg2)
summary.glm(model2_logit_small)
RsqGLM(model2_logit_small)

model2_ols_small<- lm(advertintegrity ~ treatmentstatus + factor(newcpv) + contractyear + contractmonth + log_contractvalue + buyer, data = italy_reg2)
summary.lm(model2_ols_small)

##1 year Regressions ----
italy_reg1<- rbind(italy_disaster1_1_1,italy_disaster2_1_1,italy_disaster3_1_1,italy_disaster4_1_1, italy_disaster5_1_1)
italy_reg1$contractvalue <- ifelse(is.na(italy_reg1$tender_finalPrice_EUR), ifelse(is.na(italy_reg1$tender_estimatedPrice_EUR),"",italy_reg1$tender_estimatedPrice_EUR),italy_reg1$tender_finalPrice_EUR)
italy_reg1$log_contractvalue <- log(as.numeric(as.character(italy_reg1$contractvalue)))
italy_reg1 <- italy_reg1 %>% filter(!is.na(log_contractvalue))
f<- chisq.test(italy_reg1$treatmentstatus, italy_reg1$advertintegrity, correct = FALSE)
f
f$observed

q1<- as.data.frame(table(italy_reg1$tender_mainCpv))
q1$Freq

cpv_list1 <- as.data.frame(q1$Var1[which(q1$Freq>= 500)])

italy_reg1$newcpv<- NA
for(i in 1:nrow(italy_reg1)){
  for(j in 1:nrow(cpv_list1)){
    if(italy_reg1$tender_mainCpv[i] == cpv_list1[j,1]){
      italy_reg1$newcpv[i] <- italy_reg1$tender_mainCpv[i]
      break
    }
    if(italy_reg1$tender_mainCpv[i] != cpv_list1[j,1]){
      italy_reg1$newcpv[i] <- "Other"
    }
  }
}

italy_reg1$buyer<- ifelse(italy_reg1$buyer_buyerType=="REGIONAL_AUTHORITY"|italy_reg1$buyer_buyerType=="REGIONAL_AGENCY", "Regional", "Other")

model1_logit<- glm(advertintegrity ~ treatmentstatus + contractmonth + contractyear + factor(tender_mainCpv) + log_contractvalue + buyer_buyerType, family ="binomial", data = italy_reg1)
summary.glm(model1_logit)
RsqGLM(model1_logit)

model1_ols<- lm(advertintegrity ~ treatmentstatus + contractmonth + contractyear + factor(tender_mainCpv) + log_contractvalue + buyer_buyerType, data = italy_reg1)
summary.lm(model1_ols)

model1_logit_small<- glm(advertintegrity ~ treatmentstatus + contractmonth + contractyear + factor(newcpv) + log_contractvalue + buyer, family ="binomial", data = italy_reg1)
summary.glm(model1_logit_small)
RsqGLM(model1_logit_small)

model1_ols_small<- lm(advertintegrity ~ treatmentstatus + contractmonth + contractyear + factor(newcpv) + log_contractvalue + buyer, data = italy_reg1)
summary.lm(model1_ols_small)


