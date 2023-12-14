#this the the password and username for the aqs api
library(keyring)

keyring::key_set(service = "AQSDatamart", username = "luznya86@gmail.com")

library(RAQSAPI) 

datamartAPI_user <- "luznya86@gmail.com"
server <- "AQSDatamart"

aqs_credentials(username = datamartAPI_user, key = key_get(service = server, username = datamartAPI_user ) )




library(RAQSAPI)

bdate1 <- as.Date("20100101", format = "%Y%m%d")
edate1 <- as.Date("20101231", format = "%Y%m%d")
params <- c("44201", "42602", "81102")
polldata2010 <- data.frame()

state <- sprintf("%02d", 1:50)
for (j in state) {
  report <-aqs_annualsummary_by_state(params,
                                      bdate1 ,  
                                      edate1 ,
                                      stateFIPS = as.character(j))
  polldata2010 <- rbind(polldata2010, report)                                    
}


bdate2 <- as.Date("20110101", format = "%Y%m%d")
edate2 <- as.Date("20111231", format = "%Y%m%d")
params <- c("44201", "42602", "81102")
polldata2011 <- data.frame()

state <- sprintf("%02d", 1:50)
for (j in state) {
  report <-aqs_annualsummary_by_state(params,
                                      bdate2 ,  
                                      edate2 ,
                                      stateFIPS = as.character(j))
  polldata2011 <- rbind(polldata2011, report)                                    
}


bdate3 <- as.Date("20120101", format = "%Y%m%d")
edate3 <- as.Date("20121231", format = "%Y%m%d")
params <- c("44201", "42602", "81102")
polldata2012 <- data.frame()

state <- sprintf("%02d", 1:50)
for (j in state) {
  report <-aqs_annualsummary_by_state(params,
                                      bdate3 ,  
                                      edate3 ,
                                      stateFIPS = as.character(j))
  polldata2012 <- rbind(polldata2012, report)                                    
}

bdate4 <- as.Date("20130101", format = "%Y%m%d")
edate4 <- as.Date("20131231", format = "%Y%m%d")
params <- c("44201", "42602", "81102")
polldata2013 <- data.frame()

state <- sprintf("%02d", 1:50)
for (j in state) {
  report <-aqs_annualsummary_by_state(params,
                                      bdate4 ,  
                                      edate4 ,
                                      stateFIPS = as.character(j))
  polldata2013 <- rbind(polldata2013, report)                                    
}

bdate5 <- as.Date("20140101", format = "%Y%m%d")
edate5 <- as.Date("20141231", format = "%Y%m%d")
params <- c("44201", "42602", "81102")
polldata2014 <- data.frame()

state <- sprintf("%02d", 1:50)
for (j in state) {
  report <-aqs_annualsummary_by_state(params,
                                      bdate5 ,  
                                      edate5 ,
                                      stateFIPS = as.character(j))
  polldata2014 <- rbind(polldata2014, report)                                    
}

bdate6 <- as.Date("20150101", format = "%Y%m%d")
edate6 <- as.Date("20151231", format = "%Y%m%d")
params <- c("44201", "42602", "81102")
polldata2015 <- data.frame()

state <- sprintf("%02d", 1:50)
for (j in state) {
  report <-aqs_annualsummary_by_state(params,
                                      bdate6 ,  
                                      edate6 ,
                                      stateFIPS = as.character(j))
  polldata2015 <- rbind(polldata2015, report)                                    
}

bdate7 <- as.Date("20160101", format = "%Y%m%d")
edate7 <- as.Date("20161231", format = "%Y%m%d")
params <- c("44201", "42602", "81102")
polldata2016 <- data.frame()

state <- sprintf("%02d", 1:50)
for (j in state) {
  report <-aqs_annualsummary_by_state(params,
                                      bdate7 ,  
                                      edate7 ,
                                      stateFIPS = as.character(j))
  polldata2016 <- rbind(polldata2016, report)                                    
}

bdate8 <- as.Date("20170101", format = "%Y%m%d")
edate8 <- as.Date("20171231", format = "%Y%m%d")
params <- c("44201", "42602", "81102")
polldata2017 <- data.frame()

state <- sprintf("%02d", 1:50)
for (j in state) {
  report <-aqs_annualsummary_by_state(params,
                                      bdate8 ,  
                                      edate8 ,
                                      stateFIPS = as.character(j))
  polldata2017 <- rbind(polldata2017, report)                                    
}

bdate9 <- as.Date("20180101", format = "%Y%m%d")
edate9 <- as.Date("20181231", format = "%Y%m%d")
params <- c("44201", "42602", "81102")
polldata2018 <- data.frame()

state <- sprintf("%02d", 1:50)
for (j in state) {
  report <-aqs_annualsummary_by_state(params,
                                      bdate9 ,  
                                      edate9 ,
                                      stateFIPS = as.character(j))
  polldata2018 <- rbind(polldata2018, report)                                    
}

bdate10 <- as.Date("20190101", format = "%Y%m%d")
edate10 <- as.Date("20191231", format = "%Y%m%d")
params <- c("44201", "42602", "81102")
polldata2019 <- data.frame()

state <- sprintf("%02d", 1:50)
for (j in state) {
  report <-aqs_annualsummary_by_state(params,
                                      bdate10 ,  
                                      edate10 ,
                                      stateFIPS = as.character(j))
  polldata2019 <- rbind(polldata2019, report)                                    
}

bdate11 <- as.Date("20200101", format = "%Y%m%d")
edate11 <- as.Date("20201231", format = "%Y%m%d")
params <- c("44201", "42602", "81102")
polldata2020 <- data.frame()

state <- sprintf("%02d", 1:50)
for (j in state) {
  report <-aqs_annualsummary_by_state(params,
                                      bdate11 ,  
                                      edate11 ,
                                      stateFIPS = as.character(j))
  polldata2020 <- rbind(polldata2020, report)                                    
}

bdate12 <- as.Date("20210101", format = "%Y%m%d")
edate12 <- as.Date("20211231", format = "%Y%m%d")
params <- c("44201", "42602", "81102")
polldata2021 <- data.frame()

state <- sprintf("%02d", 1:50)
for (j in state) {
  report <-aqs_annualsummary_by_state(params,
                                      bdate12 ,  
                                      edate12 ,
                                      stateFIPS = as.character(j))
  polldata2021 <- rbind(polldata2021, report)                                    
}

##############################################

library(xml2)
library(rvest)
library(tidyverse)

Asthmadatastate <- data.frame()

years <- 2011:2017

for (i in years) {
  url <- paste0("https://www.cdc.gov/asthma/brfss/", i, "/tableC1.htm")
  src <- read_html(url)
  print(url)
  
  nds1 <- html_elements(src, xpath = '//td | //th')
  
  str(nds1)
  
  pop1 <- html_text(nds1) 
  
  wable1 <- pop1 %>%
    matrix(ncol = 8, byrow = TRUE) %>%
    as.data.frame()
  wable1$Year <- i
  print(wable1)
  
  if (ncol(Asthmadatastate) == 0) {
    Asthmadatastate <- wable1
  } else {
    Asthmadatastate <- rbind(Asthmadatastate, wable1)
  }
}


## adding in 2010
url10 <- paste0("https://www.cdc.gov/asthma/brfss/2010/current/tableC1.htm")
src10 <- read_html(url10)
print(url10)

nds10 <- html_elements(src10, xpath = '//td | //th')

str(nds10)

pop10 <- html_text(nds10) 

wable10 <- pop10 %>%
  matrix(ncol = 8, byrow = TRUE) %>%
  as.data.frame()
wable10$Year <- 2010
print(wable10)

if (ncol(Asthmadatastate) == 0) {
  Asthmadatastate <- wable10
} else {
  Asthmadatastate <- rbind(Asthmadatastate, wable10)
}
#done

library(openxlsx)

brssf2018 <- read.xlsx("~/FinalProject/brssf2018.xlsx")
Asthmadatastate <- rbind(Asthmadatastate, brssf2018)


library(openxlsx)

brssf2019 <- read.xlsx("~/FinalProject/brssf2019.xlsx")
Asthmadatastate <- rbind(Asthmadatastate, brssf2019)

library(openxlsx)

brssf2020 <- read.xlsx("~/FinalProject/brssf2020.xlsx")
Asthmadatastate <- rbind(Asthmadatastate, brssf2020)

#adding in 2021
url21 <- paste0("https://www.cdc.gov/asthma/brfss/2021/tableC1.html")
src21 <- read_html(url21)
print(url21)

nds21 <- html_elements(src21, xpath = '//td | //th')

str(nds21)

pop21 <- html_text(nds21) 

wable21 <- pop21 %>%
  matrix(ncol = 8, byrow = TRUE) %>%
  as.data.frame()
wable21$Year <- 2021
print(wable21)

if (ncol(Asthmadatastate) == 0) {
  Asthmadatastate <- wable21
} else {
  Asthmadatastate <- rbind(Asthmadatastate, wable21)
}
#done





############### This is to get the seperate data for ozone no2 and pm
library(tidyr)
library(dplyr)
library(tidyverse)


newpoll10 <- polldata2010 %>% 
  select(year, state, parameter, arithmetic_mean, first_max_value) %>% 
  group_by(state, parameter) %>% 
  summarise(pollution=mean(arithmetic_mean),
            Max=max(first_max_value),
            Year=mean(year))

newpoll11 <- polldata2011 %>% 
  select(year, state, parameter, arithmetic_mean, first_max_value) %>% 
  group_by(state, parameter) %>% 
  summarise(pollution=mean(arithmetic_mean),
            Max=max(first_max_value),
            Year=mean(year))
newpoll12 <- polldata2012 %>% 
  select(year, state, parameter, arithmetic_mean, first_max_value) %>% 
  group_by(state, parameter) %>% 
  summarise(pollution=mean(arithmetic_mean),
            Max=max(first_max_value),
            Year=mean(year))
newpoll13 <- polldata2013 %>% 
  select(year, state, parameter, arithmetic_mean, first_max_value) %>% 
  group_by(state, parameter) %>% 
  summarise(pollution=mean(arithmetic_mean),
            Max=max(first_max_value),
            Year=mean(year))
newpoll14 <- polldata2014 %>% 
  select(year, state, parameter, arithmetic_mean, first_max_value) %>% 
  group_by(state, parameter) %>% 
  summarise(pollution=mean(arithmetic_mean),
            Max=max(first_max_value),
            Year=mean(year))
newpoll15 <- polldata2015 %>% 
  select(year, state, parameter, arithmetic_mean, first_max_value) %>% 
  group_by(state, parameter) %>% 
  summarise(pollution=mean(arithmetic_mean),
            Max=max(first_max_value),
            Year=mean(year))
newpoll16 <- polldata2016 %>% 
  select(year, state, parameter, arithmetic_mean, first_max_value) %>% 
  group_by(state, parameter) %>% 
  summarise(pollution=mean(arithmetic_mean),
            Max=max(first_max_value),
            Year=mean(year))
newpoll17 <- polldata2017 %>% 
  select(year, state, parameter, arithmetic_mean, first_max_value) %>% 
  group_by(state, parameter) %>% 
  summarise(pollution=mean(arithmetic_mean),
            Max=max(first_max_value),
            Year=mean(year))
newpoll18 <- polldata2018 %>% 
  select(year, state, parameter, arithmetic_mean, first_max_value) %>% 
  group_by(state, parameter) %>% 
  summarise(pollution=mean(arithmetic_mean),
            Max=max(first_max_value),
            Year=mean(year))
newpoll19 <- polldata2019 %>% 
  select(year, state, parameter, arithmetic_mean, first_max_value) %>% 
  group_by(state, parameter) %>% 
  summarise(pollution=mean(arithmetic_mean),
            Max=max(first_max_value),
            Year=mean(year))

newpoll20 <- polldata2020 %>% 
  select(year, state, parameter, arithmetic_mean, first_max_value) %>% 
  group_by(state, parameter) %>% 
  summarise(pollution=mean(arithmetic_mean),
            Max=max(first_max_value),
            Year=mean(year))

newpoll21 <- polldata2021 %>% 
  select(year, state, parameter, arithmetic_mean, first_max_value) %>% 
  group_by(state, parameter) %>% 
  summarise(pollution=mean(arithmetic_mean),
            Max=max(first_max_value),
            Year=mean(year))


allstates <- rbind(newpoll10, newpoll11, newpoll12, newpoll13, newpoll14, newpoll15,
                   newpoll16, newpoll17, newpoll18, newpoll19, newpoll20, newpoll21)

ozonzedata <- allstates %>% 
  filter(parameter=="Ozone")

NO2data <- allstates %>% 
  filter(parameter=="Nitrogen dioxide (NO2)")

PM10 <- allstates %>% 
  filter(parameter=="PM10 Total 0-10um STP")





################cleaning of the athsmadata


Asthmadatastate2 <- Asthmadatastate %>% 
  rename('state' = V1,
         'athsmaprevalence'= V7,
         'Prevalencepercent'=V3) 


library(dplyr)

Asthmadatastate3 <- Asthmadatastate2 %>%
  mutate(state = case_when(
    state == "AL" ~ "Alabama",
    state == "AK" ~ "Alaska",
    state == "AZ" ~ "Arizona",
    state == "AR" ~ "Arkansas",
    state == "CA" ~ "California",
    state == "CO" ~ "Colorado",
    state == "CT" ~ "Connecticut",
    state == "DE" ~ "Delaware",
    state == "FL" ~ "Florida",
    state == "GA" ~ "Georgia",
    state == "HI" ~ "Hawaii",
    state == "ID" ~ "Idaho",
    state == "IL" ~ "Illinois",
    state == "IN" ~ "Indiana",
    state == "IA" ~ "Iowa",
    state == "KS" ~ "Kansas",
    state == "KY" ~ "Kentucky",
    state == "LA" ~ "Louisiana",
    state == "ME" ~ "Maine",
    state == "MD" ~ "Maryland",
    state == "MA" ~ "Massachusetts",
    state == "MI" ~ "Michigan",
    state == "MN" ~ "Minnesota",
    state == "MS" ~ "Mississippi",
    state == "MO" ~ "Missouri",
    state == "MT" ~ "Montana",
    state == "NE" ~ "Nebraska",
    state == "NV" ~ "Nevada",
    state == "NH" ~ "New Hampshire",
    state == "NJ" ~ "New Jersey",
    state == "NM" ~ "New Mexico",
    state == "NY" ~ "New York",
    state == "NC" ~ "North Carolina",
    state == "ND" ~ "North Dakota",
    state == "OH" ~ "Ohio",
    state == "OK" ~ "Oklahoma",
    state == "OR" ~ "Oregon",
    state == "PA" ~ "Pennsylvania",
    state == "RI" ~ "Rhode Island",
    state == "SC" ~ "South Carolina",
    state == "SD" ~ "South Dakota",
    state == "TN" ~ "Tennessee",
    state == "TX" ~ "Texas",
    state == "UT" ~ "Utah",
    state == "VT" ~ "Vermont",
    state == "VA" ~ "Virginia",
    state == "WA" ~ "Washington",
    state == "WV" ~ "West Virginia",
    state == "WI" ~ "Wisconsin",
    state == "WY" ~ "Wyoming",
    TRUE ~ as.character(state)  # Keep other values unchanged
  ))



Asthmadatastate4 <- Asthmadatastate3 %>% 
  select(state, Prevalencepercent, athsmaprevalence, Year) %>% 
  filter(!is.na(as.numeric(Prevalencepercent)))

alldatawider <- allstates %>% 
  select(state, Year, pollution, parameter) %>% 
  group_by(state, Year) %>%
  pivot_wider(names_from = parameter, values_from = pollution)

pollandasthma <- merge(Asthmadatastate4, alldatawider, by = c("Year", "state"))

pollandasthma <- pollandasthma %>% 
  rename(PM10 = `PM10 Total 0-10um STP`,
         NO2 = `Nitrogen dioxide (NO2)`)

########################### merge everything

ozonzedata1 <- merge(Asthmadatastate4, ozonzedata, by = c("Year", "state"))

ozonzedata1$athsmaprevalence <- gsub(",", "", ozonzedata1$athsmaprevalence)

ozonzedata1 <- ozonzedata1 %>% 
  mutate(athsmaprevalence = as.numeric(athsmaprevalence))

PM10_1 <- merge(Asthmadatastate4, PM10, by = c("Year", "state"))

PM10_1$athsmaprevalence <- gsub(",", "", PM10_1$athsmaprevalence)

PM10_1 <- PM10_1 %>% 
  mutate(athsmaprevalence = as.numeric(athsmaprevalence))

N02data1 <- merge(Asthmadatastate4, NO2data, by = c("Year", "state"))

N02data1$athsmaprevalence <- gsub(",", "", N02data1$athsmaprevalence)

N02data1 <- N02data1 %>% 
  mutate(athsmaprevalence = as.numeric(athsmaprevalence))

class(ozonzedata1$athsmaprevalence)
class(PM10_1$athsmaprevalence)
class(N02data1$athsmaprevalence)

class(pollandasthma$PM10)
pollandasthma <- pollandasthma %>% 
  mutate(Prevalencepercent = as.numeric(Prevalencepercent))

ozoneEast <- ozonzedata1 %>% 
  filter(state %in% c('Maine', 'New Hampshire', 'Massachusetts', 'New York', 'Pennsylvania', 'New Jersey', 'Delaware',
                      'Maryland', 'Virginia', 'North Carolina', 'South Carolina', 'Georgia', 'Florida', 'Rhode Island', 
                      'Connecticut')) 



ozoneWest <- ozonzedata1 %>% 
  filter(state %in% c('Washington', 'Oregon', 'California', 'Idaho', 'Nevada', 'Alaska', 'Hawaii'))

ozoneNorth <- ozonzedata1 %>% 
  filter(state %in% c('Montana', 'North Dakota', 'South Dakota', 'Wyoming', 'Nebraska', 'Minnesota', 'Iowa', 'Wisconsin',
                      'Michigan', 'Illinois', 'Indiana', 'Ohio', 'West Virginia', 'Vermont'))

ozoneSouth <- ozonzedata1 %>% 
  filter(state %in% c('Utah', 'Colorado', 'Arizona', 'New Mexico', 'Texas', 'Kansas', 'Missouri', 'Oklahoma','Arkansas',
                      'Kentucky', 'Tennessee', 'Alabama', 'Mississippi', 'Louisiana'))


N02data1eEast <- N02data1 %>% 
  filter(state %in% c('Maine', 'New Hampshire', 'Massachusetts', 'New York', 'Pennsylvania', 'New Jersey', 'Delaware',
                      'Maryland', 'Virginia', 'North Carolina', 'South Carolina', 'Georgia', 'Florida', 'Rhode Island', 
                      'Connecticut'))

N02data1West <- N02data1 %>% 
  filter(state %in% c('Washington', 'Oregon', 'California', 'Idaho', 'Nevada', 'Alaska', 'Hawaii'))

N02data1North <- N02data1 %>% 
  filter(state %in% c('Montana', 'North Dakota', 'South Dakota', 'Wyoming', 'Nebraska', 'Minnesota', 'Iowa', 'Wisconsin',
                      'Michigan', 'Illinois', 'Indiana', 'Ohio', 'West Virginia', 'Vermont'))

N02data1South <- N02data1 %>% 
  filter(state %in% c('Utah', 'Colorado', 'Arizona', 'New Mexico', 'Texas', 'Kansas', 'Missouri', 'Oklahoma','Arkansas',
                      'Kentucky', 'Tennessee', 'Alabama', 'Mississippi', 'Louisiana'))


PM10_1East <- PM10_1 %>% 
  filter(state %in% c('Maine', 'New Hampshire', 'Massachusetts', 'New York', 'Pennsylvania', 'New Jersey', 'Delaware',
                      'Maryland', 'Virginia', 'North Carolina', 'South Carolina', 'Georgia', 'Florida', 'Rhode Island', 
                      'Connecticut'))

PM10_1West <- PM10_1 %>% 
  filter(state %in% c('Washington', 'Oregon', 'California', 'Idaho', 'Nevada', 'Alaska', 'Hawaii'))

PM10_1North <- PM10_1 %>% 
  filter(state %in% c('Montana', 'North Dakota', 'South Dakota', 'Wyoming', 'Nebraska', 'Minnesota', 'Iowa', 'Wisconsin',
                      'Michigan', 'Illinois', 'Indiana', 'Ohio', 'West Virginia', 'Vermont'))

PM10_1South <- PM10_1 %>% 
  filter(state %in% c('Utah', 'Colorado', 'Arizona', 'New Mexico', 'Texas', 'Kansas', 'Missouri', 'Oklahoma','Arkansas',
                      'Kentucky', 'Tennessee', 'Alabama', 'Mississippi', 'Louisiana'))

########################
#Figure 1
library(latticeExtra)
ozonemost <- ozonzedata1 %>% 
  filter(state %in% c('New York', 'Tennessee', 'Georgia', 'Illinois'))

custom_y_range <- c(5, 15)

ozonemostobj1 <- xyplot(pollution ~ Year | factor(state), ozonemost, type = c("l", "g"), col='darkgreen')
ozonemostobj2 <- xyplot(Prevalencepercent ~ Year | factor(state), ozonemost, type = c("l", "g"), col='red', ylim = custom_y_range)
doubleYScale(ozonemostobj1, ozonemostobj2, add.ylab2 = TRUE, use.style=F )

#Figure 2
library(latticeExtra)
ozoneleasr <- ozonzedata1 %>% 
  filter(state %in% c('Maine', 'Alaska', 'Montana'))

custom_y_range <- c(5, 15)

ozone1mostobj1 <- xyplot(pollution ~ Year | factor(state), ozoneleasr, type = c("l", "g"), col='darkgreen')
ozone1mostobj2 <- xyplot(Prevalencepercent ~ Year | factor(state), ozoneleasr, type = c("l", "g"), col='red', ylim = custom_y_range)
doubleYScale(ozone1mostobj1, ozone1mostobj2, add.ylab2 = TRUE, use.style=F )


#Figure 3
library(latticeExtra)
PM10_1most <- PM10_1 %>% 
  filter(state %in% c('California', 'Texas', 'Nevada', 'New Mexico', 'Michigan','New Jersy' ))

custom_y_range <- c(5, 15)

PM10_1mostobj1 <- xyplot(pollution ~ Year | factor(state), PM10_1most, type = c("l", "g"), col='darkgreen')
PM10_1mostobj2 <- xyplot(Prevalencepercent ~ Year | factor(state), PM10_1most, type = c("l", "g"), col='red', ylim = custom_y_range)
doubleYScale(PM10_1mostobj1, PM10_1mostobj2, add.ylab2 = TRUE, use.style=F )

#Figure 4
library(latticeExtra)
PM10_1leasr <- PM10_1 %>% 
  filter(state %in% c('South Dakota', 'North Dakota', 'Vermont', 'Maine' ))

custom_y_range <- c(5, 15)

PM10_2mostobj1 <- xyplot(pollution ~ Year | factor(state), PM10_1leasr, type = c("l", "g"), col='darkgreen')
PM10_2mostobj2 <- xyplot(Prevalencepercent ~ Year | factor(state), PM10_1leasr, type = c("l", "g"), col='red', ylim = custom_y_range)
doubleYScale(PM10_2mostobj1, PM10_2mostobj2, add.ylab2 = TRUE, use.style=F )


#Figure 5

library(latticeExtra)
No2_1most <- N02data1 %>% 
  filter(state %in% c('California', 'New York', 'Nevada', 'Illinois', 'Ohio' ))

custom_y_range <- c(5, 15)

No2_1mostobj1 <- xyplot(pollution ~ Year | factor(state), No2_1most, type = c("l", "g"), col='darkgreen')
No2_1mostobj2 <- xyplot(Prevalencepercent ~ Year | factor(state), No2_1most, type = c("l", "g"), col='red', ylim = custom_y_range)
doubleYScale(No2_1mostobj1, No2_1mostobj2, add.ylab2 = TRUE, use.style=F )


#Figure 6

library(latticeExtra)
No2_1leas <- N02data1 %>% 
  filter(state %in% c('Louisiana', 'Kansas', 'Utah' ))

custom_y_range <- c(5, 15)

No2_2mostobj1 <- xyplot(pollution ~ Year | factor(state), No2_1leas, type = c("l", "g"), col='darkgreen')
No2_2mostobj2 <- xyplot(Prevalencepercent ~ Year | factor(state), No2_1leas, type = c("l", "g"), col='red', ylim = custom_y_range)
doubleYScale(No2_2mostobj1, No2_2mostobj2, add.ylab2 = TRUE, use.style=F )


############################
library(ggplot2)


ggplot(ozonzedata1, aes(x = Year, y = athsmaprevalence, color = state)) +
  geom_line() +
  labs(title = "Scatter Plot of Asthma Number Over Years",
       x = "Year",
       y = "Asthma Prevalence",
       color = "State")

ggplot(ozonzedata1, aes(x = Year, y = pollution, color = state)) +
  geom_line() +
  labs(title = "Ozone Pollution Amount Over Years",
       x = "Year",
       y = "Pollution Amount",
       color = "State")

ggplot(PM10_1, aes(x = Year, y = pollution, color = state)) +
  geom_line() +
  labs(title = "Particulate Matter Pollution Amount Over Years",
       x = "Year",
       y = "Pollution Amount",
       color = "State")

ggplot(N02data1, aes(x = Year, y = pollution, color = state)) +
  geom_line() +
  labs(title = "NO2 Pollution Amount Over Years",
       x = "Year",
       y = "Pollution Amount",
       color = "State")



library(ggplot2)


ggplot(ozoneEast, aes(x = Year, y = athsmaprevalence, color = state)) +
  geom_line() +
  labs(title = "Eastern States of Asthma Number Over Years",
       x = "Year",
       y = "Asthma Prevalence",
       color = "State")

ggplot(ozoneWest, aes(x = Year, y = athsmaprevalence, color = state)) +
  geom_line() +
  labs(title = "Western States of Asthma Number Over Years",
       x = "Year",
       y = "Asthma Prevalence",
       color = "State")
ggplot(ozoneSouth, aes(x = Year, y = athsmaprevalence, color = state)) +
  geom_line() +
  labs(title = "Southern States Asthma Number Over Years",
       x = "Year",
       y = "Asthma Prevalence",
       color = "State")

ggplot(ozoneNorth, aes(x = Year, y = athsmaprevalence, color = state)) +
  geom_line() +
  labs(title = "Northern States Asthma Number Over Years",
       x = "Year",
       y = "Asthma Prevalence",
       color = "State")



library(latticeExtra)

obj1 <- xyplot(pollution ~ Year, ozoneWest, type = "l" , lwd=2, groups = state)
obj2 <- xyplot(athsmaprevalence ~ Year, ozoneWest, type = "l", lwd=2, groups = state)

# --> Make the plot with second y axis:
doubleYScale(Westobj1, Westobj2, add.ylab2 = TRUE, use.style=FALSE )


Westobj1 <- xyplot(pollution ~ Year | factor(state), ozoneWest, type = c("l", "g"), col='darkgreen')
Westobj2 <- xyplot(Prevalencepercent ~ Year | factor(state), ozoneWest, type = c("l", "g"), col='red')
doubleYScale(Westobj1, Westobj2, add.ylab2 = TRUE, use.style=FALSE )

Eastobj1 <- xyplot(pollution ~ Year | factor(state), ozoneEast, type = c("l", "g"), col='darkgreen')
Eastobj2 <- xyplot(Prevalencepercent ~ Year | factor(state), ozoneEast, type = c("l", "g"), col='red')
doubleYScale(Eastobj1, Eastobj2, add.ylab2 = TRUE, use.style=FALSE )

Northobj1 <- xyplot(pollution ~ Year | factor(state), ozoneNorth, type = c("l", "g"), col='darkgreen')
Northobj2 <- xyplot(Prevalencepercent ~ Year | factor(state), ozoneNorth, type = c("l", "g"), col='red')
doubleYScale(Northobj1, Northobj2, add.ylab2 = TRUE, use.style=FALSE )

Southobj1 <- xyplot(pollution ~ Year | factor(state), ozoneSouth, type = c("l", "g"), col='darkgreen')
Southobj2 <- xyplot(Prevalencepercent ~ Year | factor(state), ozoneSouth, type = c("l", "g"), col='red')
doubleYScale(Southobj1, Southobj2, add.ylab2 = TRUE, use.style=FALSE )


Westobj1PM10_1 <- xyplot(pollution ~ Year | factor(state), PM10_1West, type = c("l", "g"), col='darkgreen')
Westobj2PM10_1 <- xyplot(Prevalencepercent ~ Year | factor(state), PM10_1West, type = c("l", "g"), col='red')
doubleYScale(Westobj1PM10_1, Westobj2PM10_1, add.ylab2 = TRUE, use.style=FALSE )

Eastobj1PM10_1 <- xyplot(pollution ~ Year | factor(state), PM10_1East, type = c("l", "g"), col='darkgreen')
Eastobj2PM10_1 <- xyplot(Prevalencepercent ~ Year | factor(state), PM10_1East, type = c("l", "g"), col='red')
doubleYScale(Eastobj1PM10_1, Eastobj2PM10_1, add.ylab2 = TRUE, use.style=FALSE )

Northobj1PM10_1 <- xyplot(pollution ~ Year | factor(state), PM10_1North, type = c("l", "g"), col='darkgreen')
Northobj2PM10_1 <- xyplot(Prevalencepercent ~ Year | factor(state), PM10_1North, type = c("l", "g"), col='red')
doubleYScale(Northobj1PM10_1, Northobj2PM10_1, add.ylab2 = TRUE, use.style=FALSE )

Southobj1PM10_1 <- xyplot(pollution ~ Year | factor(state), PM10_1South, type = c("l", "g"), col='darkgreen')
Southobj2PM10_1 <- xyplot(Prevalencepercent ~ Year | factor(state), PM10_1South, type = c("l", "g"), col='red')
doubleYScale(Southobj1PM10_1, Southobj2PM10_1, add.ylab2 = TRUE, use.style=FALSE )



Westobj1N02data1 <- xyplot(pollution ~ Year | factor(state), N02data1West, type = c("l", "g"), col='darkgreen')
Westobj2N02data1 <- xyplot(Prevalencepercent ~ Year | factor(state), N02data1West, type = c("l", "g"), col='red')
doubleYScale(Westobj1N02data1, Westobj2N02data1, add.ylab2 = TRUE, use.style=FALSE )

Eastobj1N02data1 <- xyplot(pollution ~ Year | factor(state), N02data1eEast, type = c("l", "g"), col='darkgreen')
Eastobj2N02data1 <- xyplot(Prevalencepercent ~ Year | factor(state), N02data1eEast, type = c("l", "g"), col='red')
doubleYScale(Eastobj1N02data1, Eastobj2N02data1, add.ylab2 = TRUE, use.style=FALSE )

Northobj1N02data1 <- xyplot(pollution ~ Year | factor(state), N02data1North, type = c("l", "g"), col='darkgreen')
Northobj2N02data1 <- xyplot(Prevalencepercent ~ Year | factor(state), N02data1North, type = c("l", "g"), col='red')
doubleYScale(Northobj1N02data1, Northobj2N02data1, add.ylab2 = TRUE, use.style=FALSE )

Southobj1N02data1 <- xyplot(pollution ~ Year | factor(state), N02data1South, type = c("l", "g"), col='darkgreen')
Southobj2N02data1 <- xyplot(Prevalencepercent ~ Year | factor(state), N02data1South, type = c("l", "g"), col='red')
doubleYScale(Southobj1N02data1, Southobj2N02data1, add.ylab2 = TRUE, use.style=FALSE )





#########shiny object

library(stats)

states <- unique(pollandasthma$state)

correlation_df <- data.frame(State = character(), Pollutant = character(), Correlation = numeric(), stringsAsFactors = FALSE)


for (state in states) {
  state_data <- subset(pollandasthma, state == state)

    correlation <- cor(state_data$Prevalencepercent, state_data[[pollutant]], use = "complete.obs")
    
    correlation_df <- rbind(correlation_df, data.frame(State = state, Pollutant = 'Ozone', Correlation = correlation))
  }

correlation <- cor(pollandasthma$Prevalencepercent, pollandasthma$Ozone, use = "complete.obs")


correlationtrend <- correlation_df %>% 
  pivot_wider(names_from = Pollutant, values_from = Correlation)

print(correlationtrend )

library(stats)

states <- unique(pollandasthma$state)
pollutants <- c("Ozone", "NO2", "PM10")  # Add other pollutants as needed

correlation_df <- data.frame(State = character(), Asthma = numeric(), Pollutant = character(), Correlation = numeric(), stringsAsFactors = FALSE)

for (state in states) {
  state_data <- subset(pollandasthma, state == state)
  asthmacorr <- cor(state_data$Prevalencepercent, state_data$Year)
  
  for (pollutant in pollutants) {
    correlation <- cor(state_data[[pollutant]],state_data$Year, use = "complete.obs")
    
    correlation_df <- rbind(correlation_df, data.frame(State = state, Asthma = asthmacorr, Pollutant = pollutant, Correlation = correlation))
  }
}

print(correlation_df)

library(stats)

states <- unique(pollandasthma$state)
pollutants <- c("Ozone", "NO2", "PM10")  # Add other pollutants as needed

correlation_df <- data.frame(State = character(), Asthma_Correlation = numeric(), Pollutant = character(), Pollutant_Year_Correlation = numeric(), stringsAsFactors = FALSE)

for (state in states) {
  state_data <- subset(pollandasthma, state == state)
  asthmacorr <- cor(state_data$Prevalencepercent, state_data$Year)
  
  for (pollutant in pollutants) {
    pollutant_year_corr <- cor(state_data[[pollutant]], state_data$Year, use = "complete.obs")
    
    correlation_df <- rbind(correlation_df, data.frame(State = state, Asthma_Correlation = asthmacorr, Pollutant = pollutant, Pollutant_Year_Correlation = pollutant_year_corr))
  }
}

print(correlation_df)

#############################


rsconnect::setAccountInfo(name='luzmaria',
                          token='827C13D9D77C49AE48299CDE3A029BC4',
                          secret='+1xVFU9TS2C0MQbRiaA5XG8MhW1kkQrmGQ9yusQ2')
library(rsconnect)

library(rsconnect)




write.csv(pollandasthma, file = "pollandasthma.csv", row.names = TRUE)
