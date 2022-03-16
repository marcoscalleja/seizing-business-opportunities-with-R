library(data.table)
library(pROC)
library(tidyverse)
library(ggplot2)
library(mltools)
library(caret)
rm(list = ls())

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

machfaildata <- fread(file = "/Users/marcos/OneDrive/ESADE/Term 2/Data Analytics with R/Challenge/Challenge 2/machine_failures.csv", 
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


################## PRICE SENSITIVITY SECTION

head(machfaildata)
merged1 <- merge(transdata, machfaildata, by=c('machine','timestamp','column'), all.x=TRUE)
merged1[is.na(failure), failure:=0]
head(merged1)

merged2 <- merge(transdata, proddata, by=c('product_name'), all.x=TRUE)
#merged2[is.na(failure), failure:=0]
head(merged2)

#let's the price distribution of all products and compare with what is actually bought
ddply(proddata, .(category), summarize,  Unique_Prices=length(price),
      Avg_Price = mean(price),
      #Max_Price = min(price),
      #Min_Price = max(price),
      STD   = sd(price)#,
      #SE   = sd / sqrt(length(price))
      )

ddply(merged2, .(category), summarize,  Unique_Prices=length(price),
      Avg_Price = mean(price),
      #Max_Price = min(price),
      #Min_Price = max(price),
      STD   = sd(price)#,
      #SE   = sd / sqrt(length(price))
)

#Let's plot the price distribution from all and sold
#First we get the list of unique categories and unique product names so we can cycle through and produce plots for comparison
unique_cat <- unique(proddata$category)
unique_cat
proddata
un <- unique_cat[1]
#test
distr1 <- table(proddata[category == un]$price)
barplot(distr1)
distr2 <- table(merged2[category == un]$price)
barplot(distr2)


#testing ggplot2
p1 <- ggplot(proddata, aes(x=price, fill=category)) +
  geom_histogram(binwidth=1, position="dodge") +
  ggtitle("price distribution per category") #+
  #geom_text(aes(label = price, hjust = 1))

# Second plot - make this stacked and with percentages, then see how it changes through different settings
p2 <- ggplot(merged2, aes(x=price, fill=category)) +
  geom_histogram(binwidth=1, position="dodge") +
  ggtitle("Fitted growth curve per diet")
p1
p2

#Let's now merge with column so we can also check the positioning
merged3 <- merge(merged2, machdata, by=c('machine'), all.x=TRUE)
summary(merged3)


#merged 3 has all the data we need. now we just slice it to create different models

#to create the target, which is quantity (how many we sell) we have to group by certain things
#whatever we group datasets by, we need a quantity metric for each case and erase duplicates 
#(We have to take out timestamps and etc.)

#phase1 is to take all data and group by daily average not by machine but by each category
#the models below can take merged3 and be used for bins to produce more specific models even

category_daily_average <- merged3[, .(daily_sales = length(timestamp)), by = .(category, price)]
head(category_daily_average) #terrible model
category_daily_average$category <- as.factor(category_daily_average$category)

productname_daily_average <- merged3[, .(daily_sales = length(timestamp)), by = .(product_name, price)]
head(productname_daily_average) #terrible model

column_daily_average <- merged3[, .(daily_sales = length(timestamp)), by = .(column, price)]
head(column_daily_average) #this will need to be one hot enconded
column_daily_average$column <- as.factor(column_daily_average$column) #this model is the first in this list to give coefficient close to significance levels

incavg_daily_average <- merged3[, .(daily_sales = length(timestamp)), by = .(income_average, price)] #the results form this dataset present income 
              #averge as making very little impact on avg daily sales. so i think we need to split it up in bins and se if we get better results
#remove NAs
incavg_daily_average <- incavg_daily_average[complete.cases(incavg_daily_average)]
incavg_daily_average
#unique_inc <- unique(incavg_daily_average$income_average)
#length(unique_inc)

#income + cat
catinc_daily_average <- merged3[, .(daily_sales = length(timestamp)), by = .(category, income_average, price)] #income average + categories is also very good at giving back interesting coefficients
head(catinc_daily_average)
catinc_daily_average$category <- as.factor(catinc_daily_average$category)

#income + col
catinc_daily_average <- merged3[, .(daily_sales = length(timestamp)), by = .(column, income_average, price)] #income average + categories is also very good at giving back interesting coefficients

catinc_daily_average$category <- as.factor(catinc_daily_average$column)
catinc_daily_average <- catinc_daily_average[complete.cases(catinc_daily_average)]
head(catinc_daily_average)

#a general one:
general_daily_avg <- merged3[, .(daily_sales = length(timestamp)), by = .(category, price, column, income_average)]
general_daily_avg <- general_daily_avg[complete.cases(general_daily_avg)]
#general_daily_avg$column <- as.factor(general_daily_avg$column)
#general_daily_avg <- one_hot(general_daily_avg, cols = 'column')
#general_daily_avg
general_daily_avg$category <- as.factor(general_daily_avg$category)
general_daily_avg$column <- as.factor(general_daily_avg$column)

head(general_daily_avg)
general_daily_avg



#splitting by income quartiles
summary(merged3) #2 = 48974, 2 = 54110,3 = 60984
merged3_1 <- merged3[merged3$income_average <= 48974] #-14 incavg #chocolate & milk based, juice/tea/smoothies and water #
merged3_2 <- merged3[merged3$income_average >= 48974 & merged3$income_average <= 54110] #-19 incavg #chocolate & milk based, and water #
merged3_3 <- merged3[merged3$income_average >= 54110 & merged3$income_average <= 60984] #-37 incavg # chocolate based #
merged3_4 <- merged3[merged3$income_average >= 60984] #-25 incavg # chocolate & milk based #

input <- merged3

incavg_daily_average <- input[, .(daily_sales = length(timestamp)), by = .(income_average, price)]
incavg_daily_average <- incavg_daily_average[complete.cases(incavg_daily_average)]
incavg_daily_average
#income + cat #this pretty mjuch tells us that the poorer you are the more variety product you buys from a vending machine
catinc_daily_average <- input[, .(daily_sales = length(timestamp)), by = .(category, income_average, price)] #income average + categories is also very good at giving back interesting coefficients
catinc_daily_average$category <- as.factor(catinc_daily_average$category)
#income + col
colinc_daily_average <- input[, .(daily_sales = length(timestamp)), by = .(column, income_average, price)] #income average + categories is also very good at giving back interesting coefficients
colinc_daily_average$column <- as.factor(colinc_daily_average$column)
colinc_daily_average <- colinc_daily_average[complete.cases(colinc_daily_average)]

#income + product name #useful for bundling
prninc_daily_average <- input[, .(daily_sales = length(timestamp)), by = .(product_name, category, price, income_average)] #income average + categories is also very good at giving back interesting coefficients
prninc_daily_average$product_name <- as.factor(prninc_daily_average$product_name)
#prninc_daily_average <- prninc_daily_average[complete.cases(prninc_daily_average)]
prninc_daily_average

#################
#getting to modelling:
#chose the dataset

finalds <- prninc_daily_average 

data1 = sort(sample(nrow(finalds), nrow(finalds)*.8))
train<-finalds[data1,]
test<-finalds[-data1,]
head(train)
head(test)

#a
#model <- lm(daily_sales ~.,data=train)
model1 <- lm(daily_sales ~., data = prninc_daily_average)
summary(model1) #this shows us two variables with a p-value above 0.05, total_number_of_routes_600 and num_vendex_nearby_300. They do not show statiscal significance, impacting little the model's performance
my_estimates <- model1$coefficients                     # Matrix manipulation to extract estimates
my_estimates
model1 = step(model1) #here we do a step, eliminating features with big p-values
summary(model1)

model2 <- lm(daily_sales ~., data = catinc_daily_average)
summary(model2) #this shows us two variables with a p-value above 0.05, total_number_of_routes_600 and num_vendex_nearby_300. They do not show statiscal significance, impacting little the model's performance
my_estimates <- model2$coefficients                     # Matrix manipulation to extract estimates
my_estimates
model2 = step(model2) #here we do a step, eliminating features with big p-values
summary(model2)

#anova(model, test="Chisq") #deviance table
#it's a pretty bad model
#model = step(model) #here we do a step, eliminating features with big p-values
#summary(model)
#anova(model, test="Chisq")




#do for loop that goes through a pair and store values of predictions. Than use another for loop to create an equation that tells us how much we gain by doing these pairs by percentage of price decrease
#create a table with no duplicates, with only product names, categories and prices

#we have to choose an income average
inc_avg <- 60000

prodcat_price <- merged3[, .(daily_sales = length(timestamp)), by = .(category, product_name, price, income_average, cost)]
prodcat_price <- prodcat_price[complete.cases(prodcat_price)]
prodcat_price <- subset(prodcat_price, select = -c(daily_sales)) 
head(prodcat_price)
prodcat_price$income_average <- inc_avg
prodcat_price <- prodcat_price[!duplicated(prodcat_price)]
prodcat_price <- as.data.frame(prodcat_price)
prodcat_price
nrow(prodcat_price)

pred <- predict(model2, newdata = prodcat_price[1,], type = "response")

summary(pred)
head(pred)
pred1 <- c(pred)
pred1 <- data.frame(pred)
head(pred)
x1 <- pred[1]

x1

#intializing table to hold final differences per i(column) and j(row)
data <- 0
difference <- c()
label1 <- c()
label2 <- c()
price1 <- c()
price2 <- c()
total_nobund1 <- c()
total_nobund2 <- c()
total_yesbund1 <- c()
total_yesbund2 <- c()
revenue_before <- c()
revenue_after <- c()
cost1 <- c()
cost2 <- c()

percentage <- 0.1
mul <- 1 - percentage

for(i in seq_len(nrow(prodcat_price))){
  z1 <- prodcat_price[i,]
  z12 <- z1
  z12$price <- z12$price*mul
  
  #difference <- 0
  
  
  for(j in seq_len(nrow(prodcat_price))){
    z2 <- prodcat_price[j,]
    z22 <- z2
    z22$price <- z22$price*mul
  
    #this calculate based on product name
    Apred11 <- predict(model1, newdata = z1, type = "response") #we take product name
    Apred12 <- predict(model1, newdata = z12, type = "response")
    Apred21 <- predict(model1, newdata = z2, type = "response")
    Apred22 <- predict(model1, newdata = z22, type = "response")
    
    
    #this calculates based on cat and price
    Bpred11 <- predict(model2, newdata = z1, type = "response") #we take the category and price of z1
    Bpred12 <- predict(model2, newdata = z12, type = "response")
    Bpred21 <- predict(model2, newdata = z2, type = "response")
    Bpred22 <- predict(model2, newdata = z22, type = "response")
    
    #save all the daily sales on avg so we have a sales difference and price differences
    x1 <- (Apred11[1] + 3*Bpred11[1])/4 #we trust more model2
    x12 <- (Apred12[1] + 3*Bpred12[1])/4
    x2  <- (Apred21[1] + 3*Bpred21[1])/4
    x22 <- (Apred22[1] + 3*Bpred22[1])/4
    
    total_nobund1 <- c(total_nobund1, x1)
    total_nobund2 <- c(total_nobund2, x2)
    total_yesbund1 <- c(total_yesbund1, x12)
    total_yesbund2 <- c(total_yesbund2, x22)
    
    revenue_before <- z1$price*x1 + z2$price*x2 #(price times avg sales)
    revenue_after <- z12$price*x12 + z22$price*x22 #(price times avg sales)
    diff <- revenue_after - revenue_before
    difference <- c(difference,diff)
    label1 <- c(label1, z1$product_name)
    label2 <- c(label1, z2$product_name)
    price1 <- c(price1, z1$price)
    price2 <- c(price2, z2$price)
    cost1 <- c(cost1, z1$cost) 
    cost2 <- c(cost2, z2$cost)
    #data <- data[, ':=' (nrow(prodcat_price)) = difference]
  }
 
  
}
#product_name <- label1


#stage1 <- data.frame(product_name,price1)
#stage1 <- subset(merge(stage1, proddata, by=c('product_name', 'price'), all.x=TRUE), select = c(product_name, price, cost))
#stage2 <-

class.df <- data.frame(label1,price1,cost1, label2[-1],price2, cost2, total_nobund1, total_nobund2, total_yesbund1, total_yesbund2, difference)
class.df$discount <- mul
head(label1)
head(label2)
class.df

toprint<-sort(class.df$difference, decreasing =TRUE, index.return = TRUE)
head(toprint)

#class.df$profit_change[]
profit_change <- (((class.df$total_yesbund1*(class.df$price1*class.df$discount-class.df$cost1))+((class.df$total_yesbund2*(class.df$price2*class.df$discount-class.df$cost2)))) - ((class.df$total_nobund1*(class.df$price1-class.df$cost1))+((class.df$total_nobund2*(class.df$price2-class.df$cost2)))))
profit_change
class.df$profit_change <- profit_change

#gives us the rows in order of the 
class.df[order(-class.df$difference), ]
df <- class.df[order(-class.df$profit_change), ]

#data <- data[, ':=' (nrow(prodcat_price)) = difference]
#data
head(df)



#co<-model$coef
#b0 <- model$coef[1]
#x1 <- model$coef[2]






###################


phase1 = subset(merged3, select = -c(timestamp,date,tax_rate,machine)) #machine data is already included. we also don't need taxes yet


#for the next part, we create a quantity column which is the target, but the quantity isn't just about repeating rows. the quantity is generated by a range on the regression values.
#we have to create the bins manually bc of our lack of expertize for every value

#distr_phase1 <- table(merged3$num_vendex_nearby_300)
#barplot(distr_phase1) #0,1,2,3,4,5 and above

#distr_phase1 <- table(merged3$small_machine)
#barplot(distr_phase1) #0,1,2,3,4,5 and above

#distr_phase1 <- table(merged3$n_density_5km)
#barplot(distr_phase1) #0 to 400, 400 to 1400, 1400 and above

distr_phase1 <- table(merged3$income_average)
barplot(distr_phase1) #0,1,2,3,4,5 and above
unique_inc <- unique(merged3$income_average)
length(unique_inc)

#distr_phase1 <- table(merged3$small_machine)
#barplot(distr_phase1) #0,1,2,3,4,5 and above

#general model



#column model:


#category model:


#income average model: #we need to make sure bins of income average give similar result

#product name modelS:
#1 by a single product, how price varies with column, income average, num_vendex nearby, trainavgdailypass, total number of routes







  #get %
#merged2 %>% 
#  count(price = factor(price), category = factor(category)) %>% 
#  mutate(pct = prop.table(n))

#ggplot(merged2,aes(x = factor(price), fill = factor(category), 
#                             y = (..count..)/sum(..count..),
#       label = scales::percent(prop.table(stat(count))))) +
#  geom_bar() +
#  stat_bin(geom = "text",
#           aes(label = paste(round((..count..)/sum(..count..)*100), "%")),
#           vjust = 5) +
#  scale_y_continuous(labels = scales::percent)

#total distriubtion
distr <- table(merged2$price)
barplot(distr)


for i in unique_cat{ #here we save the plots so we play with them later
  distr1 <- table(proddata[category == i]$price)
  #barplot(distr1)
  #loca1 <- proddata[,.(.N), by=price] # transport = 691; petrol station = 343 ; others = 691 ; NA = 1
  #print(loca1)
  
  distr2 <- table(merged2[category == i]$price)
  #barplot(distr2)
  #loca2 <- merged2[,.(.N), by=price] # transport = 691; petrol station = 343 ; others = 691 ; NA = 1
  #print(loca2)
}



