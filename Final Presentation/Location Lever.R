
# Library loading ---------------------------------------------------------
library(data.table)
library(lubridate)
library(tidyverse)
library(dplyr)
library(pROC)

source ("/Users/marcos/OneDrive/ESADE/Term 2/Data Analytics with R/Challenge/Final Presentation/R_FinalScript .R")
source("/Users/marcos/OneDrive/ESADE/Term 2/Data Analytics with R/Challenge/Challenge 1/R Challenge.R")

#From best product presence we take this
td = fread("/Users/marcos/OneDrive/ESADE/Term 2/Data Analytics with R/Challenge/Challenge 1/final_data/transactional_data.csv")
pd = fread("/Users/marcos/OneDrive/ESADE/Term 2/Data Analytics with R/Challenge/Challenge 1/final_data/product_data.csv")

# Fix variable formats ----------------------------------------------------
td[,class(timestamp)]
td[,timestamp:=ymd_hms(timestamp)]
td[,class(timestamp)]

pd[,margin:=((price/(1+tax_rate)) - cost)/price] # price = (cost+margin)*(1+tax)
tdp = merge(td,pd,by='product_name',all.x=T)

mg_mach <- tdp[, .(margin), by= machine]

mg_mach[!duplicated(mg_mach)]

#From the source of Joao's latest code

sales_per_machine <- merged3[, .(Total_sales = length(timestamp)), by = .(machine)]
active <-merged3[, .(active_days_machine=uniqueN(date)), by = .(machine)]
avg_trans <- merge(sales_per_machine, active, by=c('machine'), all.x=TRUE)
avg_trans[, avg_trans_day:= Total_sales/active_days_machine ]

mean(avg_trans$avg_sales_day)

#Now we go to line 316 from challenge 1 and we can build the location model
#We need to take finalds and substitute avg transaction per machine for the one i just calculated

#Before removing NAs, the avg sales per machine per day is correct

data1 = sort(sample(nrow(finalds), nrow(finalds)*.8))
train<-finalds[data1,]
test<-finalds[-data1,]
head(train)
head(test)

#a
model <- lm(avgdaily_trans ~.,data=train)
summary(model) #this shows us two variables with a p-value above 0.05, total_number_of_routes_600 and num_vendex_nearby_300. They do not show statiscal significance, impacting little the model's performance


#b
log_transport <- log10(finalds[,finalds$total_number_of_routes_600])
head(log_transport)
head(finalds$total_number_of_routes_600)
finalds2 <- finalds[,.(small_machine,income_average,num_hotels_45,num_vendex_nearby_300,indicator,avgdaily_trans)]
finalds2$log_transport <- log_transport
head(finalds2)

#I use data1 because the number of rows hasn't changed, and we want to repeat the train test selection
train<-finalds2[data1,]
test<-finalds2[-data1,]
head(train)
head(test)

model <- lm(avgdaily_trans ~.,data=train)
summary(model) #We can see right away that the new variable is statistically significant, and now vendex close by is also statistical significant?
final_model = step(model) #here we do a step, eliminating features with big p-values
summary(final_model)


#e
#if we train it with the whole dataset
model <- lm(avgdaily_trans ~.,data=finalds2)
summary(model) #We can see right away that the new variable is statistically significant, and now vendex close by is also statistical significant?
final_model = step(model) #here we do a step, eliminating features with big p-values
summary(final_model)

# Predictions
pred1 <- predict(final_model, newdata = finalds2, type = "response")
summary(pred1)
head(pred1)

#Checking for BCN

newtrans_bcn <- data.table(small_machine = 0,
                           income_average = 0,
                           num_hotels_45 = 3,
                           num_vendex_nearby_300 = 20,
                           indicator = 1,
                           log_transport= log10(9))
prednewtrans1 <- predict(final_model, newdata = newtrans_bcn, type = "response")
prednewtrans1



#---------------OVERALL SCORE-------------
avg_trans<- avg_trans[,.(machine, avg_sales_day)]
machine_performance = merge(maching_sales_prediction, avg_trans, by = 'machine') #merge avg number of transactions with machine performance

model_pred_items <- avg_trans[, .(machine, avg_sales_day, model_pred_items = predict(final_model, newdata = finalds2, type = "response"))]

model_pred_items <- model_pred_items[complete.cases(model_pred_items),]#removes rows with NAs in any column, if we apply the same logic for the entire dataset


machine_performance = machine_performance[, profit_avg := avg_daily_trans * profit_margin] #daily ACTUAL profit based on average profit marging and avg daily transactions
machine_performance = machine_performance[, pred_profit_avg := model_pred_items * profit_margin] #daily PREDICTED profit based on average profit marginm and avg daily transactions


#--------LOCATION AMSTERDAM V BARCELONA SCORES-------

#feature importance: transport station, machine size, hotels, transport passengers, income average, vendex nearby

ams_metrostations = 58 #https://en.wikipedia.org/wiki/Category:Amsterdam_Metro_stations

ams_total_hotels = 	501 #https://www.iamsterdam.com/en/about-amsterdam/overview/facts-and-figures
ams_hotel_rooms =	34758
ams_stays_total = 1.6e7 #https://www.statista.com/statistics/960733/number-of-overnight-stays-spend-in-amsterdam-by-accommodation-type/
ams_rooms_per_hotel =  ams_hotel_rooms / ams_total_hotels

ams_stays_5s = ams_stays_total * catalunya_stays_5_share
ams_stays_4s = ams_stays_total * catalunya_stays_4_share

ams_hotels_5 = ceiling((ams_stays_5s / 365) / (ams_rooms_per_hotel * .7)) #number of 5s hotels at 70% occupancy rate
ams_hotels_4 = ceiling((ams_stays_4s / 365) / (ams_rooms_per_hotel * .7)) ##number of 4s hotels at 70% occupancy rate
ams_hotels_45 = ams_hotels_4 + ams_hotels_5

metro_bcn = fread('./LocationData/metrostationsBCN.csv')
stay_area_bcn = fread('./LocationData/OvernightstaysAreaBCN.csv')
stay_category_bcn = fread('./LocationData/OvernightstaysCategoryBCN.csv')

ann_stays_catlunya = stay_area_bcn[1,'Value'] * 1e3
ann_stays_bcn = stay_area_bcn[2,'Value'] * 1e3
catalunya_stays_bcnshare = ann_stays_bcn / ann_stays_catlunya #bcn stays as pct of catalunya stays

catalunya_stays_5 = stay_category_bcn[2, 'Value'] * 1e3
catalunya_stays_4 = stay_category_bcn[3, 'Value'] * 1e3
catalunya_stays_5_share = catalunya_stays_5 / (stay_category_bcn[1, 'Value'] * 1e3) #5s stays as share of total stays
catalunya_stays_4_share = catalunya_stays_4 / (stay_category_bcn[1, 'Value'] * 1e3) #4s stays as share of total stays

bcn_stays_5 = catalunya_stays_5 * catalunya_stays_bcnshare #apply bcn share to catalunya stays 5s
bcn_stays_4 = catalunya_stays_4 * catalunya_stays_bcnshare #apply bcn share to catalunya stays 4s

bcn_stays_45 = bcn_stays_4 + bcn_stays_5 #sum stays in 4 and 5 stars
bcn_hotels_45 = ceiling((bcn_stays_45 / 365) / (ams_rooms_per_hotel * .7)) #apply rooms per hotel from ams to bcn stays

stations_bcn = length(metro_bcn$Name) #number of metro stations in bcn

###location scores per city###

ams_metrostations = 58
ams_bus_stops = 900
stations_ams = ams_metrostations + ams_bus_stops
ams_hotels_45 = 189 #Booking

metro_stations_bcn = 161
bus_stops_barcelona = 2606
stations_bcn = metro_stations_bcn + bus_stops_barcelona
bcn_hotels_45 = 345 #Booking

ams_location_score = (stations_ams * 1.699e+00) + (ams_hotels_45 * 3.301e-01)
bcn_location_score = (stations_bcn * 1.699e+00) + (bcn_hotels_45 * 3.301e-01)

newtrans_bcn <- data.table(small_machine = 0,
                           income_average = 0,
                           num_hotels_45 = bcn_hotels_45,
                           num_vendex_nearby_300 = 20,
                           indicator = 1,
                           log_transport= log10(stations_bcn))

prednewtrans1 <- predict(final_model, newdata = newtrans_bcn, type = "response")
prednewtrans1

newtrans_ams <- data.table(small_machine = 0,
                           income_average = 0,
                           num_hotels_45 = ams_hotels_45,
                           num_vendex_nearby_300 = 0,
                           indicator = 1,
                           log_transport= log10(stations_ams))

prednewtrans2 <- predict(final_model, newdata = newtrans_ams, type = "response")
prednewtrans2


#Amsterdam
score_ams <- c()
machines_nearby_ams <- c()

for (i in 1:150){
  newtrans_ams <- data.table(small_machine = 0,
                             income_average = 0,
                             num_hotels_45 = ams_hotels_45,
                             num_vendex_nearby_300 = i,
                             indicator = 1,
                             log_transport= log10(stations_ams))
  
  
  prednewtrans2 <- predict(final_model, newdata = newtrans_ams, type = "response")
  score_ams<- c(score_ams, prednewtrans2)
  machines_nearby_ams <- c(machines_nearby_ams, i)
}

#Barcelona
score_bcn <- c()
machines_nearby_bcn <- c()

for (i in 1:150){
  newtrans_bcn <- data.table(small_machine = 0,
                             income_average = 0,
                             num_hotels_45 = bcn_hotels_45,
                             num_vendex_nearby_300 = i,
                             indicator = 1,
                             log_transport= log10(stations_bcn))
  
  
  prednewtrans2 <- predict(final_model, newdata = newtrans_bcn, type = "response")
  score_bcn<- c(score_bcn, prednewtrans2)
  machines_nearby_bcn <- c(machines_nearby_bcn, i)
}

ams <- data.frame(score_ams, machines_nearby_ams)
bcn <- data.frame(score_bcn, machines_nearby_bcn)
names(ams)[names(ams) == "machines_nearby_ams"] <- "machines_nearby"
names(bcn)[names(bcn) == "machines_nearby_bcn"] <- "machines_nearby"

ams_bcn <- merge(ams, bcn, by=c('machines_nearby'), all.x=TRUE)


y1 <- score_ams
y2 <- score_bcn
x <- ams_bcn$machines_nearby
# in this plot y2 is plotted on what is clearly an inappropriate scale
plot(y1 ~ x, ylim = c(-1, 150), type = "l", xlab = "Machines Nearby", ylab = "Average sales" ,main = "AMS vs BCN decrease in avg sales as machines nearby increase")
points(y2 ~ x, pch = 2, type = "l", col = "dark red")

((ams_bcn$score_ams[150] - ams_bcn$score_ams[1])/ ams_bcn$score_ams[1])*100
((ams_bcn$score_bcn[150] - ams_bcn$score_bcn[1])/ ams_bcn$score_bcn[1])*100

  