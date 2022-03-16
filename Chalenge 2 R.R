library(data.table)

source("/Users/marcos/OneDrive/ESADE/Term 2/Data Analytics with R/Challenge/Challenge 1/R Challenge.R")

failures <- fread(file = "/Users/marcos/OneDrive/ESADE/Term 2/Data Analytics with R/Challenge/Challenge 2/machine_failures.csv", 
                      header = TRUE, 
                      sep = ",",
                      stringsAsFactors = FALSE,
                      dec= ".")
transdata <- fread(file = "/Users/marcos/OneDrive/ESADE/Term 2/Data Analytics with R/Challenge/Challenge 1/final_data/transactional_data.csv", 
      header = TRUE, 
      sep = ",",
      stringsAsFactors = FALSE,
      dec= ".")

#1. Merge the transactional dataset with the machine failures data set setting failure variable to 0
#when no failure is recorded 
dt <- merge( transdata,failures, all=TRUE, sort=FALSE)
dt

unique(dt$failure)

#Adding 0s when there's no failure
dt[is.na(failure),failure:=0] 


#2.  In the transactional data table (I assume it's the new merged dataset), we create a variable called “last_vend” containing the timestamp of
#the previous sale of each machine 

dt = dt[order(machine, timestamp, date)] 

dt[, last_vend:= shift(timestamp)]

#3. Create a new variable in the transactional data table called “deltahours” containing, for every
#sale, the hours that passed since the last sale

#We are interested in hours. Thus, we'll use hours as unit
dt[, deltahours:= difftime(dt$timestamp, dt$last_vend, units = "hours")]

#4. Create an auxiliary data table called “machine_daily_average” with the average daily sales per
#machine. Use this auxiliary table to attach to every row of the transactional data table the the
#average daily sales per machine. You can do this by doing a merge.

machine_daily_average <- avgtransmachf
head(machine_daily_average)

dt <- merge( dt,machine_daily_average, all=TRUE, sort=FALSE)

#5. Create a new variable called “delta” in the transactional data table containing a normalized
#version of deltahours consisting on the deltahours associated with each sale divided by the
#average deltahours of each machine i.e. delta = deltahours /(24/daily_sales_machine). The
#interpretation of delta is the amount of “missed sales” if the machine was selling at a constant
#rate 

dt[, delta:= deltahours /(24/avgdaily_trans)]


#7d
#We define the threshold
delta_threshold <- ?????
#transactions that would have generated an alarm
dt_alarm = dt[,delta > delta_threshold] #We don't have the threshold

conversion_factor <- 24/daily_sales
dt[, threshold_hours := conversion_factor* delta_threshold] #From deltas to hours

#threshold_hours_fixed -> We add 1.5h to the threshold hours
dt[, threshold_hours_fixed := threshold_hours + 1.5]

dt[, delta_fixed:= threshold_hours_fixed * (1/(24/daily_sales_machine))]

#if the current system was able to detect and repair the machine before our EWS system, 
#the won_sales will be negative (actually we lost sales)
dt[, won_sales := failure*(delta-delta_fixed)] #These are hours when we are selling products and were missed before


additional_revenues = sum(as.numeric(data_alarm$won_sales)*1.7) 

#10€ of cost each time a machine fails
ews_cost = 10*(nrow(dt[failure == 0]))

#Cost with the previous system. They had 2.2 false alarms per machine and year which means 2.2*10€ of cost
old_cost <- length(unique(dt$machine))*2.2*10

#The dataset is 3 months so to calculate the profit for a whole year we multiply by 4
profit <- 4*(additional_revenues - ews_cost) - old_cost

precentage_increase <- profit / nrow(dt)*1.7*4

