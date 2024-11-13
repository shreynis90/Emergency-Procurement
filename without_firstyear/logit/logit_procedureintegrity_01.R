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
library(margins)



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

#Dropping missings and creating a new variable for procedure integrity ----
italy<- italy %>% filter(!is.na(tender_indicator_INTEGRITY_PROCEDURE_TYPE))
italy$procedureintegrity<- ifelse(italy$tender_indicator_INTEGRITY_PROCEDURE_TYPE == 0,1,0)

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
##
disaster1_date <- as.POSIXct("2009-10-02")
italy_disaster1 <- subset(italy_disaster1, !(is.na(tender_publications_firstCallForTenderDate) & contractyear<2011))

italy_disaster1$Date <- as.yearmon(paste(italy_disaster1$contractmonth, italy_disaster1$contractyear), "%m %Y")
ncontracts1 <- italy_disaster1 %>% filter(treatcon == 1) %>% group_by(Date, treatcon, procedureintegrity)%>% mutate(numberofcontracts = n()) %>% dplyr::select(Date, numberofcontracts, treatcon, disnumber, procedureintegrity) %>% distinct()

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
##
italy_disaster2 <- subset(italy_disaster2, !(is.na(tender_publications_firstCallForTenderDate) & contractyear<2011))
disaster2_date <- as.POSIXct("2012-05-29")

###Number of Contracts ----
italy_disaster2$Date <- as.yearmon(paste(italy_disaster2$contractmonth, italy_disaster2$contractyear), "%m %Y")
ncontracts2 <- italy_disaster2 %>% filter(treatcon == 1) %>% group_by(Date, procedureintegrity)%>% mutate(numberofcontracts = n()) %>% dplyr::select(Date, numberofcontracts, treatcon, disnumber, procedureintegrity) %>% distinct()

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
##
italy_disaster3 <- subset(italy_disaster3, !(is.na(tender_publications_firstCallForTenderDate) & contractyear<2011))
disaster3_date <- as.POSIXct("2013-11-18")

###Number of Contracts ----
italy_disaster3$Date <- as.yearmon(paste(italy_disaster3$contractmonth, italy_disaster3$contractyear), "%m %Y")
ncontracts3 <- italy_disaster3 %>% filter(treatcon == 1) %>% group_by(Date, treatcon, procedureintegrity)%>% mutate(numberofcontracts = n()) %>% dplyr::select(Date, numberofcontracts, treatcon, disnumber, procedureintegrity) %>% distinct()


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
##
italy_disaster4 <- subset(italy_disaster4, !(is.na(tender_publications_firstCallForTenderDate) & contractyear<2011))
disaster4_date <- as.POSIXct("2016-08-24")

###Number of Contracts ----
italy_disaster4$Date <- as.yearmon(paste(italy_disaster4$contractmonth, italy_disaster4$contractyear), "%m %Y")
ncontracts4 <- italy_disaster4 %>% filter(treatcon == 1) %>% group_by(Date, treatcon, procedureintegrity)%>% mutate(numberofcontracts = n()) %>% dplyr::select(Date, numberofcontracts, treatcon, disnumber, procedureintegrity) %>% distinct()


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
##
italy_disaster5 <- subset(italy_disaster5, !(is.na(tender_publications_firstCallForTenderDate) & contractyear<2011))
disaster5_date <- as.POSIXct("2017-01-18")

###Number of Contracts ----
italy_disaster5$Date <- as.yearmon(paste(italy_disaster5$contractmonth, italy_disaster5$contractyear), "%m %Y")
ncontracts5 <- italy_disaster5 %>% filter(treatcon == 1) %>% group_by(Date, treatcon, procedureintegrity)%>% mutate(numberofcontracts = n()) %>% dplyr::select(Date, numberofcontracts, treatcon, disnumber, procedureintegrity) %>% distinct()


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

#Full period logit regression
italy_reg<- rbind(italy_disaster1_1,italy_disaster2_1,italy_disaster3_1,italy_disaster4_1, italy_disaster5_1)
italy_reg$contractvalue <- ifelse(is.na(italy_reg$tender_finalPrice_EUR), ifelse(is.na(italy_reg$tender_estimatedPrice_EUR),"",italy_reg$tender_estimatedPrice_EUR),italy_reg$tender_finalPrice_EUR)
italy_reg$log_contractvalue <- log(as.numeric(as.character(italy_reg$contractvalue)))
italy_reg <- italy_reg %>% filter(!is.na(log_contractvalue))

italy_reg$buyer<- ifelse(italy_reg$buyer_buyerType=="REGIONAL_AUTHORITY"|italy_reg$buyer_buyerType=="REGIONAL_AGENCY", "Regional", "Other")
italy_reg$newcpv <- ifelse(italy_reg$tender_mainCpv == 33, 33, 100)

italy_reg$contractdate <- as.Date(paste(italy_reg$contractyear, italy_reg$contractmonth, "01", sep = "-"), "%Y-%m-%d")


filter_by_disaster <- function(data) {
  data %>% 
    filter(
      !(disnumber == "Disaster_001" & contractdate >= as.Date("2009-10-01") & contractdate < as.Date("2010-10-01")) &
        !(disnumber == "Disaster_002" & contractdate >= as.Date("2012-05-01") & contractdate < as.Date("2013-05-01")) &
        !(disnumber == "Disaster_003" & contractdate >= as.Date("2013-11-01") & contractdate < as.Date("2014-11-01")) &
        !(disnumber == "Disaster_004" & contractdate >= as.Date("2016-08-01") & contractdate < as.Date("2017-08-01")) &
        !(disnumber == "Disaster_005" & contractdate >= as.Date("2017-01-01") & contractdate < as.Date("2018-01-01"))
    )
}

# Apply the filtering function to the dataframe
italy_reg_filtered <- filter_by_disaster(italy_reg)

model_logit<- glm(procedureintegrity ~ treatmentstatus + factor(newcpv) + buyer+ log_contractvalue + contractyear + contractmonth, family ="binomial", data = italy_reg_filtered)

saveRDS(model_logit, 'tables/robustness/table_21/1_logit_procedureintegrity_01_model.rds')

n_after = sum(italy_reg_filtered$treatmentstatus[italy_reg_filtered$treatmentstatus == 1])

saveRDS(n_after, 'tables/robustness/table_21/1_logit_procedureintegrity_01_n_after.rds')


# summary.glm(model_logit)

saveRDS(RsqGLM(model_logit), 'tables/robustness/table_21/1_logit_procedureintegrity_01_se.rds')

# summary(margins(model_logit))


m <- margins::margins(model_logit, variables = c('treatmentstatus'), type = "response") %>% 
  broom::tidy() %>% 
  mutate(term = 'Non-open Procedure')
saveRDS(m, 'tables/robustness/table_21/1_logit_procedureintegrity_01.rds')

