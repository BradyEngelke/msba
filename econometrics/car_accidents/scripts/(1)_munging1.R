# Author: Minya Na

setwd("~/Downloads")
library(dplyr)
library(tidyverse)
library(data.table)
library(lubridate)
d <- fread('US_Accidents_Dec19.csv',stringsAsFactors = F)

d %>%
  select(year,month)

view(head(d))
d$year <- year(d$Start_Time)
d$month <- month(d$Start_Time)
colnames(d)
head(d)
str(d)
names(d)[names(d) == "Temperature(F)"] <- "Temperature"
names(d)[names(d) == "Humidity(%)"] <- "Humidity"
names(d)[names(d) == "Pressure(in)"] <- "Pressure"
names(d)[names(d) =="Visibility(mi)"] <- "Visibility"
names(d)[names(d) == "Wind_Speed(mph)"] <- "Wind_Speed"
names(d)[names(d) == "Precipitation(in)"] <- "Precipitation"
names(d)[names(d) == "Wind_Chill(F)"] <- "Wind_Chill"


colnames(d)         
new_d1 <- d %>%
  select(State,year,month,Severity,Temperature:Visibility,Wind_Speed,Precipitation)%>%
  group_by(State,month)%>%
  summarise(avg_severity = mean(Severity,na.rm=T),avg_temp = mean(Temperature,na.rm = T),
            avg_wind_chill = mean(Wind_Chill,na.rm = T),
            avg_humi = mean(Humidity,na.rm = T), avg_pres = mean(Pressure,na.rm = T),
            avg_visi = mean(Visibility,na.rm = T),
            avg_wind_speed = mean(Wind_Speed,na.rm = T),avg_preci = mean(Precipitation,na.rm = T))


colnames(d)

summary(new_d1)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

new_d2 <- d %>%
    select(State,year,month,Wind_Direction,Weather_Condition:Astronomical_Twilight)%>%
  group_by(State,year,month)%>%
    summarise(
  major_wind_direction = Mode(Wind_Direction),
  major_weather_condition = Mode(Weather_Condition),major_amenity = Mode(Amenity),
  major_bump = Mode(Bump),major_crossing = Mode(Crossing),major_giveway = Mode(Give_Way),
  major_junction = Mode(Junction), major_noexit = Mode(No_Exit), 
  major_railway = Mode(Roundabout), major_station = Mode(Station),
  major_stop = Mode(Stop),major_traffic_calming = Mode(Traffic_Calming),
  major_traffic_signal = Mode(Traffic_Signal), major_turning_loop = Mode(Turning_Loop),
  major_sunrise = Mode(Sunrise_Sunset),major_civil_twilight = Mode(Civil_Twilight),
  major_nautical = Mode(Nautical_Twilight),major_astro = Mode(Astronomical_Twilight))

write.csv(new_d1,"~/Downloads/new_d1.csv")
write.csv(new_d2,"~/Downloads/new_d2.csv")

head(new_d1)
ncol(new_d1)
colnames(new_d1)
colnames(new_d2)

new_d1 %>%
  select(State,year,month,avg_severity,avg_temp)%>%
  spread(key = month, value = c(avg_severity,avg_temp))


head(new_d2)
-----------------------------------------
# propensity score: weather, by month
install.packages("readxl")
library(readxl)
alcohol <- read_excel('/Users/miana/Downloads/Alcohol Consumption by State.xlsx')


selected_alcohol <- alcohol %>%
  select(State,Year,`All beverages (gallons per capita)`)%>%
  filter(Year %in% c(2014,2015,2016))%>%
  arrange(State,Year)

summary(selected_alcohol$Year)
length(unique(selected_alcohol$State))


selected_alcohol <- selected_alcohol %>%
  spread(key = Year, value = `All beverages (gallons per capita)`)

selected_alcohol$State[selected_alcohol$State == "Louisana"] = "Louisiana"

  
setnames(selected_alcohol, old = c('2014','2015','2016'), 
         new = c('X2014_beverages','X2015_beverages','X2016_beverages'))
write.csv(selected_alcohol,'/Users/miana/Downloads/beverage_aggregate.csv')
-----------------
setwd("~/Downloads")
rev <- read_excel('/Users/miana/Downloads/clean_data_causal/rev_expenses.xlsx')
nrow(rev)
rev <- rev %>%
  arrange(State,Year)
colnames(rev)

rev <- rev %>%
  select(State,Year,Taxes,Total_Revenue,Public_Welfare,
         Hospitals_Expense,Health_Expense,Police_Expense,Correction_Expense)%>%
  mutate(rev_ratio = Taxes/Total_Revenue)%>%
  select(State,Year,rev_ratio,Public_Welfare,
         Hospitals_Expense,Health_Expense,Police_Expense,Correction_Expense)
rev <- rev %>%
  filter(Year %in% c(2014,2015,2016))
view(rev)

rev_1 <- rev%>%
  select(State,Year,rev_ratio)%>%
  spread(key = Year,value = rev_ratio) %>%
  setnames(old = c('2014','2015','2016'), 
           new = c('X2014_rev_ratio','X2015_rev_ratio','X2016_rev_ratio'))

rev_2 <- rev%>%
  select(State,Year,Public_Welfare)%>%
  spread(key = Year,value = Public_Welfare) %>%
  setnames(old = c('2014','2015','2016'), 
           new = c('X2014_Public_Welfare','X2015_Public_Welfare','X2016_Public_Welfare'))

rev_3 <- rev%>%
  select(State,Year,Hospitals_Expense)%>%
  spread(key = Year,value = Hospitals_Expense) %>%
  setnames(old = c('2014','2015','2016'), 
           new = c('X2014_Hospitals_Expense','X2015_Hospitals_Expense','X2016_Hospitals_Expense'))

rev_4 <- rev%>%
  select(State,Year,Health_Expense)%>%
  spread(key = Year,value = Health_Expense) %>%
  setnames(old = c('2014','2015','2016'), 
           new = c('X2014_Health_Expense','X2015_Health_Expense','X2016_Health_Expense'))

rev_5 <- rev%>%
  select(State,Year,Police_Expense)%>%
  spread(key = Year,value = Police_Expense) %>%
  setnames(old = c('2014','2015','2016'), 
           new = c('X2014_Police_Expense','X2015_Police_Expense','X2016_Police_Expense'))

rev_6 <- rev%>%
  select(State,Year,Correction_Expense)%>%
  spread(key = Year,value = Correction_Expense) %>%
  setnames(old = c('2014','2015','2016'), 
         new = c('X2014_Correction_Expense','X2015_Correction_Expense','X2016_Correction_Expense'))
view(rev_combine)
rev_combine <- merge(rev_1,rev_2, by = "State")
rev_combine <- merge(rev_combine,rev_3,by = "State")
rev_combine <- merge(rev_combine,rev_4,by = "State")
rev_combine <- merge(rev_combine,rev_5,by = "State")
rev_combine <- merge(rev_combine,rev_6,by = "State")

write.csv(rev_combine,'/Users/miana/Downloads/clean_data_causal/rev_expense_aggregate.csv')


population <- read_excel('/Users/miana/Downloads/clean_data_causal/pop.xlsx')
view(population)
population$State = sub(".","",population$State)
unique(population$State)

population <- population %>%
  select(State,"2014","2015","2016",Total_Area)%>%
  setnames(old = c('2014','2015','2016'), 
           new = c('X2014_population','X2015_population','X2016_population'))

population <- population %>%
  mutate(X2014_Population_Density = X2014_population/Total_Area,
         X2015_Population_Density = X2015_population/Total_Area,
         X2016_Population_Density = X2016_population/Total_Area)

population <- population %>%
  select(State, X2014_Population_Density,X2015_Population_Density,X2016_Population_Density)
write.csv(population,'/Users/miana/Downloads/clean_data_causal/population_aggregate.csv')


View(agg1)
agg1 <- merge(selected_alcohol, rev_combine,by = "State")
agg1 <- merge(agg1, population, by = "State")
write.csv(agg1,'/Users/miana/Downloads/clean_data_causal/agg1.csv')


a_s <- read.csv('/Users/miana/Downloads/clean_data_causal/age and sex_by_state.csv',header = T)
View(a_s)
colnames(a_s)

setnames(a_s,old = c('POPEST2014_CIV','POPEST2015_CIV','POPEST2016_CIV'), 
         new = c('X2014','X2015','X2016'))
unique(a_s$NAME)
a_s <- a_s %>%
  filter(NAME!="District of Columbia" & NAME != "United States")

a_s <- a_s %>%
  select(NAME,SEX,AGE,"X2014","X2015","X2016")
total_2014<- sum(a_s$X2014)

a_s <- a_s %>%
  mutate(product_2014 = AGE*X2014)%>%
  mutate(avg_2014 = product_2014/total_2014)
