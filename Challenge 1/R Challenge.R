#install.packages("tidyverse")

library(data.table)
library(pROC)
library(tidyverse)

transdata <- fread(file = "/Users/marcos/OneDrive/ESADE/Term 2/Data Analytics with R/Challenge/Challenge 1/final_data/transactional_data.csv",
                   header = TRUE,
                   sep = ",",
                   stringsAsFactors = FALSE,
                   dec= ".")

proddata <- fread(file = "/Users/marcos/OneDrive/ESADE/Term 2/Data Analytics with R/Challenge/Challenge 1/final_data/product_data.csv",
                  header = TRUE,
                  sep = ",",
                  stringsAsFactors = FALSE,
                  dec= ".")

machdata <- fread(file = "/Users/marcos/OneDrive/ESADE/Term 2/Data Analytics with R/Challenge/Challenge 1/final_data/machine_data.csv",
                  header = TRUE,
                  sep = ",",
                  stringsAsFactors = FALSE,
                  dec= ".")

# summary of variables types and basic statistical quantities
summary(transdata)
summary(proddata)
summary(machdata)


#Question 1 General Overview Data
print(colSums(is.na(machdata)))#just checking number of NAs in machdata
print(colSums(is.na(proddata)))#just checking number of NAs in proddata
print(colSums(is.na(transdata)))#just checking number of NAs in transdata

#a How many machines?
unmach <- unique(machdata[["machine"]])
print(length(unmach)) #2495 unique machine ids


#b
small <- machdata[,.(.N), by=small_machine] #959 small machines!
print(small)
print(small[[2,"N"]])
smallper <- small[[2,"N"]]/length(unmach)
print(smallper) #0.3843or 38.43% is our number

#c
unloc <- unique(machdata[["location_type"]])
print(unloc)
print(length(unloc))
distr <- table(machdata$location_type)
barplot(distr)
loca <- machdata[,.(.N), by=location_type] # transport = 691; petrol station = 343 ; others = 691 ; NA = 1
print(loca)


#d
unprod <- unique(proddata[["product_name"]])
print(unprod)
print(length(unprod)) #63 unique product names
unprodcat <- unique(proddata[["category"]])
print(unprodcat)
print(length(unprodcat)) #8 unique categories


#e#use this for f but for each machine id retard
library(plyr)
ddply(proddata, .(category), summarize,  avg_price=mean(price))

#                       category avg_price
#1  Carbonates and energy drinks  3.261538
#2               Chocolate based  2.309091
#3 Cookies, pastries and cereals  2.385714
#4      Juice, tea and smoothies  2.862500
#5                    Milk based  3.425000
#6                         Salty  2.725000
#7                   Sugar candy  2.300000
#8                         Water  3.260000

ddply(proddata, .(type_drink_snack), summarize,  avg_price=mean(price))
#   type_drink_snack avg_price
#1            drink  3.176667
#2            snack  2.424242


#f
timeselect <- transdata[transdata$timestamp >= "2017-03-01" & transdata$timestamp <= "2017-03-31",]
#checking if it works
summary(timeselect)
print(length(unique(timeselect[["machine"]]))) #number of machines
undates <- unique(timeselect[["timestamp"]])
print(undates)
daily <- timeselect[, .N, by=date]
#sanity check
print(daily) #gives us 30 days
print(unique(daily[["date"]])) #gives us 30 days
#the daily average of items sold is:
me <- mean(daily$N) #21538.3 is the daily number of transactions... seems to big to be true
print(me/length(unique(timeselect[["machine"]]))) #8.64
#redoing this^
################
#now we take from transactions the number of operations per machine
tpmf <- timeselect[,.N,by=machine]
head(tpmf)
#and for how long did each machine operate?
holderf <- group_by(timeselect, date, machine)
holderf$trans <- 1
head(holderf)
tail(holderf)

opmf <- ddply(holderf, .(date, machine), summarize,  total_trans=sum(trans))
head(opmf)
#what's the average of total trans
avgtransmachf <- ddply(opmf, .(machine), summarize,  avgdaily_trans=mean(total_trans))
head(avgtransmachf)

#now we put this with the other 6 columns, and we will have our column to predict
ds2 <- subset(machdata, select=c(machine,small_machine,income_average,total_number_of_routes_600,num_hotels_45,num_vendex_nearby_300))
withaverage <- merge(ds2,avgtransmachf, by="machine")
head(withaverage)
#by small machine == 1 or 0
smallavg <- ddply(withaverage, .(small_machine), summarize,  avgdaily_trans=mean(avgdaily_trans))
head(smallavg)
##############

#Checking product variety by machine
machineandcategory <- merge(subset(transdata,select=c(machine,product_name)),subset(machdata,select=c(machine,location_type,train_AvgDailyPassengers,small_machine)),by="machine")
machineandcategory2 <- machineandcategory
# using lapply, loop over columns and match values to the look up table. store in "new".
new[] <- lapply(machineandcategory2$product_name, function(x) machineandcategory$category[match(x, machineandcategory$product_name)])

#group1 <- group_by(mach1,machine)
#head(group1)
#cant run these lines, go over 16. Gb limit lol
#secondmerge <- merge(group1, mach1, by="machine")
#head(secondmerge)



##############

#Question 2 Consider #items in pdf plot
#to get the plot he made
join1 <- merge(transdata,proddata,by = "product_name")
join2 <- merge(join1, machdata, by="machine")
countdaysnack <- join2[type_drink_snack == "snack"]
countdaydrink <- join2[type_drink_snack == "drink"]
summary(countdaysnack)
summary(countdaydrink)
daysnack <- countdaysnack[, .N, by=(date)]
daydrink <- countdaydrink[, .N, by=(date)]
sdjoin <- merge(daysnack,daydrink,by = "date")
summary(daysnack)
summary(daydrink)
summary(sdjoin) #Nx is for snacks, Ny is for drinks


#select date to see in detail
timeselect2 <- sdjoin[sdjoin$date >= "2017-01-01" & sdjoin$date <= "2017-01-18",]
timeselect2
plot(timeselect2$date, timeselect2$N.x, type = "l")
plot(timeselect2$date, timeselect2$N.y, type = "l")


#Find Correlation
#join the number of sales per day to the original tables and see if anything moves with them
#a
#no coding here, just a question. My guess its the that with warmer seasons, more people spend more time out, and vending machines sales go up accordingly

#b
#let's check how this compares against the days of the week
timeselect2$date <- as.factor(weekdays(timeselect2$date))
plot(timeselect2$date, timeselect2$N.x, type = "l")
plot(timeselect2$date, timeselect2$N.y, type = "l")
#seems like the short term fluctuations are the weekdays, where people do different activities depending on the day of the



#Question 3 Distribution given on average income
#a
#related to NA the answer is simple: remove them. The complex answer is, if geographical location is available, substitute those NAs for collected data, but since they are only a few of them removing them is a safe bet.
summary(machdata$income_average)
hist(machdata$income_average)
dep <- table(machdata$income_average)
barplot(dep)
#the number of high values, above 60k(around 3rd quartile), is a very small % of the data as we can see from the line below
totalsumtable <- machdata[,.N,by=income_average]
head(totalsumtable)
notrichquartilesumtable <- totalsumtable[totalsumtable$income_average<(180000)]
head(notrichquartilesumtable)

totalsum <- sum(totalsumtable$N)
totalsum
sapply(totalsumtable, sd, na.rm = TRUE)

notrichquartilesum <- sum(notrichquartilesumtable$N)
notrichquartilesum
sapply(thirdquartilesumtable, sd, na.rm = TRUE)

richsumtable <- totalsumtable[totalsumtable$income_average>(180000)]
head(richsumtable)

richsum <- sum(richsumtable$N)
richsum
richsum/totalsum

clean <- machdata[complete.cases(machdata$income_average),]#removes rows with NAs in income_average
summary(clean$income_average) #no NAs
clean2 <- machdata[complete.cases(machdata),]#removes rows with NAs in any column, if we apply the same logic for the entire dataset

clean <- clean[clean$income_average<(14381500/2)] #optional, a way to deal with outliners

summary(clean$income_average)
summary(clean2$income_average)


#Another way to deal with outliers is to seperate them from the original dataset, and investigating them to see if they show any patterns
outlier <- clean[clean$income_average>60148]
summary(outlier)
dep <- table(outlier$income_average)
barplot(dep) #this shows an almost even distribution, with one bin particularly full, between 60k and 62k. Let's investigate further

outlier2 <- outlier[outlier$income_average>60800 & outlier$income_average<61100]
summary(outlier2)
dep <- table(outlier2$income_average)
barplot(dep) #we have here a particular neighborhood that seems to be popular in our dataset, with more than 100 counts.
#if we go back to line 171 and run that plot, we see that this 60k outlier is also the highest value, with 4 other peaks.
#What we probably have here is a greater number of machines in specific neighborhoods. Let's check to see if that is true.
holder1 <- machdata[machdata$income_average==60984] #a specific neighborhood!
holder2 <- holder1[,.N,by=machine]
print(holder2) #yup, we have 117 machine here alone. This means that this outliers might actually be places of specific interest if we want to explore the performance of saturrated places.
#My recomendation is to split the dataset and investigate/create models on top of this 4 areas saturadted with machines.

#b
#So, 3 ways to deal with NAs. First, you remove them. usually the safest choice. The second is to have them be zero, which can be useful if the
#the record has some sort of lazy initialization, that is, information is only updated when there is something to update, and therefore an
#NA in the system is actually the equivelent of NULL/Zero. The third way to deal with it is to have it equal a number, such as the average
#of the instance. This last option requires field/expert knowledge to make that call. My personal choice fo rthe first few models would defnitely
#be to simply remove the rows with NAs since there are so few of them.

#utilizing from 1f, head(withaverage), to compare NAs to avg daily items
head(withaverage)
arcvi_descriptive(withaverage$income_average, withaverage$avgdaily_trans)
#this shows a small trend with number of transscations going up with income.
#This makes sense since neighborhoods with more income have more people with disposable income.
#NAs have a curious performance, with lower avg daily transactions, This may lead one to believe NAs are neighborhoods with low incomes.
#ecause  these neighborhoods have low incomes, consequently, there are less sales. Since the number of neighborhoods which have an NA value for their income is small
#Our recommendation is to still take them off.

#Question 4 Analyze Boxplot
#See that dark line at the bottom? That's the median line squished against the first quartile. This means that in the majority of cases, areas not only have less than
#one hotel, they have zero hotels nearby. Radius of that area for hotels is unknown unfortunately.

#Question 5
#Let's create the dataset then
#from machdata we want small_machine, income_average, total_number_of_routes_600, num_hotels_45, and num_vendex_nearby_300;
#the sixth feature is an 0-1 indicator on whether the machine has train_AvgDailyPassengers infomred, meaning its a petrol station or other type of location

ds <- subset(machdata, select=c(machine,small_machine,income_average,total_number_of_routes_600,num_hotels_45,num_vendex_nearby_300))
ds$indicator <- 0 #creating the indicator column, which will be populated next
#but first let's just check train_AvgDailyPassengers 's distribution
dep <- table(machdata$train_AvgDailyPassengers)
barplot(dep)
#how many zeroes?
print(sum(machdata$train_AvgDailyPassengers==0, na.rm = T))# no values equal zero!
print(sum(is.na(machdata$train_AvgDailyPassengers), na.rm = T))# 1262 NAs
print(sum(!is.na(machdata$train_AvgDailyPassengers), na.rm = T))# 1233 real values!
summary(machdata$train_AvgDailyPassengers) #we can also do this, which i forgot momentarily about

ds$indicator[machdata$train_AvgDailyPassengers>0 & !is.na(machdata$train_AvgDailyPassengers)] = 1
#let's check our creation
summary(ds)
head(ds)
tail(ds)
#just checking there are no repeats bc I'm a noob at R
x <- setDT(ds)[, .N, machine]
print(unique(x$N))#yup, each line is a single machine

#we now need to create the column with what we want to predict. that is, a column with average daily sales for EACH machine.

#now we take from transactions the number of operations per machine
tpm <- transdata[,.N,by=machine]
head(tpm)
#and for how long did each machine operate?
holder <- group_by(transdata, date, machine)
holder$trans <- 1
head(holder)
tail(holder)

opm <- ddply(holder, .(date, machine), summarize,  total_trans=sum(trans))
head(opm)
#what's the average of total trans
avgtransmach <- ddply(opm, .(machine), summarize,  avgdaily_trans=mean(total_trans))
head(avgtransmach)
finalds <- 0
#now we put this with the other 6 columns, and we will have our column to predict
finalds <- merge(ds,avgtransmach, by="machine")
finalds
#now let's remove the NAs
finalds <- finalds[complete.cases(finalds),]#removes rows with NAs in any column, if we apply the same logic for the entire dataset
#let's also remove the machine ids
#finalds %>% select(-machine) #this didn't work?
finalds <- subset(finalds, select=c(small_machine,income_average,total_number_of_routes_600,num_hotels_45,num_vendex_nearby_300,indicator,avgdaily_trans))
finalds

#aight, time to model
#randomize and split in 80%
data1 = sort(sample(nrow(finalds), nrow(finalds)*.8))
train<-finalds[data1,]
test<-finalds[-data1,]
head(train)
head(test)

#a
model <- lm(avgdaily_trans ~.,data=train)
summary(model) #this shows us two variables with a p-value above 0.05, total_number_of_routes_600 and num_vendex_nearby_300. They do not show statiscal significance, impacting little the model's performance
anova(model, test="Chisq") #deviance table
#it's a pretty bad model
model = step(model) #here we do a step, eliminating features with big p-values
summary(model)
anova(model, test="Chisq")

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

#c
#small machines coefficient is -1.718, so if it is 1, we sell almost 2 units less on avg

#d
#All factors remaining equal, having machines nearby decreases only 0.107 avg sales

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
pred1 <- c(pred1)
pred1 <- data.frame(pred1)
head(pred1)
prorder <- pred1[order(pred1, decreasing = TRUE),]
#for 20%, we have 2213 rows, so we want the top 443 and the bottom after 1770
#let's order the real values
dsorder1 <- finalds2[order(avgdaily_trans, decreasing = TRUE),]
dsorder2 <- dsorder1$avgdaily_trans
dsorder2
#first column is the original/real, second column is the prediction
top20 <- data.frame(dsorder2[1:445],prorder[1:445])
bottom20 <- data.frame(dsorder2[1769:2213],prorder[1769:2213])

head(top20)
head(bottom20)
top20
bottom20
#I will use median and standard deviation. The former because it is more telling than the average, which can be skewed by a few values.
#The latter because the standard deviation is a good value to tell us about the spread of data.


#some data bellow
sd(top20$dsorder2.1.445.)
sd(top20$prorder.1.445.)
sd(bottom20$dsorder2.1769.2213.)
sd(bottom20$prorder.1769.2213.)

#starting comparison, always real/prediction
#comparing std top20s
x1<-sd(top20$dsorder2.1.445.)/sd(top20$prorder.1.445.)
x1
#comparing std bottom20s
x2<-sd(bottom20$dsorder2.1769.2213.)/sd(bottom20$prorder.1769.2213.)
x2
#the numbers above show that our predictions have trouble predicting values away from the mean.
#with the bottom values, our model is overestimating the spread.


median(top20$dsorder2.1.445.)
median(top20$prorder.1.445.)
median(bottom20$dsorder2.1769.2213.)
median(bottom20$prorder.1769.2213.)

#comparing median top20s, always real/prediction
y1<-median(top20$dsorder2.1.445.)/median(top20$prorder.1.445.)
y1
#comparing median bottom20s
y2<-median(bottom20$dsorder2.1769.2213.)/median(bottom20$prorder.1769.2213.)
y2
#The median gives us closer values, but the trend is the same.
#With the median, we see that we underestimate high values, and overestimate low values.
#Our model, perhaps due to its linear nature, does so by almost the same absolute amount of 0.31/0.32



#Ratio of Ratios, after agreggating with the median and standard deviation, we are comparing results between top and bottom 20
x1/x2 #this is the std
#For the standard deviation, the top has 3.55 times the std of the bottom, indicating that we have a bigger spread on high values.

y1/y2 #this is the median
#For the median we have a similar issue. Together with these values, one could make the conclusion that this dataset needs more data with high values of average sales.

#These two ratios comparing top to bottom show us that we are better at predicting low values than high values. This indicates that this dataset may require
#more new data, with emphasis on machines with higher average daily sales. It is also possible that a linear model is not suited for this dataset´s distribution.


#f
newtrans <- data.table(small_machine = 0,
                       income_average = 0,
                       num_hotels_45 = 2,
                       num_vendex_nearby_300 = 0,
                       indicator = 0,
                       log_transport= log10(20))

prednewtrans1 <- predict(final_model, newdata = newtrans, type = "response")

newtrans <- data.table(small_machine = 0,
                       income_average = 0,
                       num_hotels_45 = 0,
                       num_vendex_nearby_300 = 3,
                       indicator = 1,
                       log_transport= log10(10))

prednewtrans2 <- predict(final_model, newdata = newtrans, type = "response")

prednewtrans1
prednewtrans2 #no matter income levels, this one gives high average sales

#trying with the stations in bcn
newtrans_bcn <- data.table(small_machine = 0,
                       income_average = 0,
                       num_hotels_45 = 130,
                       num_vendex_nearby_300 = 0,
                       indicator = 1,
                       log_transport= log10(50))

prednewtrans1 <- predict(final_model, newdata = newtrans_bcn, type = "response")
prednewtrans1
