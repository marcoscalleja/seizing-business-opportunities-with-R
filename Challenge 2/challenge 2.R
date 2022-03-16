#R Challenge Number 2
rm(list = ls())
library(dplyr)
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

machfaildata <- fread(file = "/Users/marcos/OneDrive/ESADE/Term 2/Data Analytics with R/Challenge/Challenge 2/machine_failures.csv", 
                      header = TRUE, 
                      sep = ",",
                      stringsAsFactors = FALSE,
                      dec= ".")
# summary of variables types and basic statistical quantities
summary(transdata)
summary(proddata)
summary(machdata)
summary(machfaildata)


tpm <- transdata[,.N,by=machine]
head(tpm)
#and for how long did each machine operate?
holder <- group_by(transdata, date, machine)
holder$trans <- 1
head(holder)
tail(holder)
library(plyr)
opm <- ddply(holder, .(date, machine), summarize,  total_trans=sum(trans))
tail(opm)
#what's the average of total trans
avgtransmach <- ddply(opm, .(machine, date), summarize,  avgdaily_trans=mean(total_trans))
head(avgtransmach)

#let's first organize trasndata by machine, and then order by timestamp
by_mach <- transdata %>% group_by(machine)
by_mach <- by_mach %>% arrange(machine, date, timestamp) #everything is now perfectly ordered, by machine, by date, and by hour.
# this was done so that we have the option to use the order of the columns to subtract time time between transactions, for the delta
#now creating an index number so we preserve the order after merge
by_mach <- tibble::rowid_to_column(by_mach, "index")

tail(by_mach, 10)

# Merge the transactional dataset with the machine failures data set setting failure variable to 0 when no failure is recorded
# machfaildata[duplicated(machfaildata)] #no duplicates found
#group by machine and sum the failures

#converting machfaildata timestamp to date
head(machfaildata)
machfaildata <- as.data.frame.matrix(machfaildata) 

machfaildata$date <- format(machfaildata$timestamp,'%Y-%m-%d')
head(machfaildata)

class(machfaildata['timestamp'])
summary(machfaildata)

#machfaildata['date'] <- as.Date(machfaildata['timestamp'])
machfaildata['date'] <- as.Date(paste(machfaildata$timestamp), "%m/%d/%Y") 
summary(machfaildata)

sumfail <- summarise_at(group_by(machfaildata,machine),vars(failure),funs(sum(.,na.rm=TRUE))) #https://www.datasciencemadesimple.com/group-by-function-in-r-using-dplyr/
head(sumfail)
phase1 <- merge(by_mach, sumfail, by=c('machine','machine'), all.x=T)
#phase1 <- join(by_mach.machine, sumfail.machine)
phase1 <- phase1 %>% arrange(index)
head(phase1)
tail(phase1)
sum(is.na(phase1['failure']))
sum(is.na(phase1)) #this confirms all nas are in 'failure', expected since not all machines have failures
phase1[is.na(phase1)] <- 0

#In the transactional data table, create a variable called “last_vend” containing the timestamp of the previous sale of each machine
#copy columns with timestamps, dates
holder_time <- phase1['index']
#holder_time['index'] <- phase1['index']
holder_time['last_vend'] <- phase1['timestamp']
holder_time['p_date'] <- phase1['date']
head(holder_time)
holder_time['index']<-holder_time['index'] + 1
head(holder_time['index'])
phase2 <- merge(phase1, holder_time, by=c('index','index'), all.x=T)
head(phase2)

#Create a new variable in the transactional data table called “deltahours” containing, for every sale, the hours that passed since the last sale
#create new column

################### testing area ###################
phase2['deltahours'] <- 0

f1 = function(x, output) {
  #numberrows <- nrow(x) #so we don't apply this in the last row
  print(x['index'])
  #print(x['machine'])
  #print( test[test['index'] == x['index']]['machine'])
  
  x['deltahours'] <- difftime(x['timestamp'], x['last_vend'])
  
}

f3 = function(x, output) {
  #numberrows <- nrow(x) #so we don't apply this in the last row
  print(x['index'])
  print(x['machine'])
  print(test$machine[test['index'] == (as.integer(x['index'])-1)])
  
  if (as.integer(x['index'])!=1) {
    
    
    if ((x['machine'] == test$machine[test['index'] == (as.integer(x['index'])-1)])){ #this allows us to not perform this operation when the machine number changes!
      print("same machine")
      x['deltahours'] <- difftime(x['timestamp'], x['last_vend'])
    } else {
      print("machine has changd")
      x['deltahours'] <- 0}
    
  } else {print("here3")
    x['deltahours'] <- 0}}


#test function
test <- head(phase2, 6)
test$machine[test$index==3] <- 2
test$machine[test$index==4] <- 2
test$machine[test$index==5] <- 2
test


test$machine[test['index'] == 2]
test$machine[test['index'] == (4-1)]

test['deltahours'] <- apply(test, 1, f1)
test
########################

#final function, after tuned in test phase
f2 = function(x, output) {
  #numberrows <- nrow(x) #so we don't apply this in the last row
  print(as.integer(x['index']))
  print(x['machine'])
  print(phase2$machine[phase2['index'] == (as.integer(x['index'])-1)])
  
  if (as.integer(x['index'])!=1) {
    
    
    if ((as.integer(x['machine']) == phase2$machine[phase2['index'] == (as.integer(x['index'])-1)])){ #this allows us to not perform this operation when the machine number changes!
      #print("same machine")
      x['deltahours'] <- difftime(x['timestamp'], x['last_vend'])
    } else {
      #print("machine has changd")
      x['deltahours'] <- 0}
    
  } else {#print("x index is == 1")
    x['deltahours'] <- 0}}

phase2['deltahours'] <- apply(phase2, 1, f1)
head(phase2)
tail(phase2)#have to divide by hours since difftime gives back in minutes
phase3 <- phase2
phase3['deltahours']<-phase3['deltahours']/60
head(phase3)
sum((phase3['deltahours']<0),na.rm=TRUE) #gives 2494negative machines, which makes sense since we do have 2495 machines
phase3$deltahours[phase3['deltahours']<0] <- NA #here we eliminate the negative values and replace them with NA
sum((phase3['deltahours']<0),na.rm=TRUE)

#let's now do the merge with avg sales per machine
#phase4 <- merge(phase3, avgtransmach, 'machine', 'machine', all.x=T)
phase4 <- merge(phase3, avgtransmach, by=c('machine', 'date'))
head(phase4)

#now we do the normalization
phase4['delta']<-phase4['deltahours']/(24/phase4['avgdaily_trans'])
summary(phase4['delta'])
head(phase4)

length(unique(machfaildata[["machine"]]))
summary(phase4)
sum((phase4['failure']!=0),na.rm=TRUE)
sum((phase4['failure']==NA),na.rm=TRUE)
phase4$failure[phase4['failure']!=0] <- 1
summary(phase4['failure'])
phase5 = phase4[,!(names(phase4) %in% c("index","p_date"))]


#eliminate NA rows, bc delta needs previous data to work
phase5 <- na.omit(phase5)
head(phase5)
sum((phase5['failure']!=0),na.rm=TRUE)
sum((phase5['failure']==0),na.rm=TRUE)
summary(phase4['delta'])
summary(phase5['delta'])

#and here comes the model

#6
#creating train and test datasets
data1 = sort(sample(nrow(phase4), nrow(phase4)*.7))
#creating training data set by selecting the output row values
train<-phase5[data1,]
#creating test data set by not selecting the output row values
test<-phase5[-data1,]

summary(phase5)
head(phase5)

m <- glm(failure~delta, data = train, family = 'binomial')
summary(m)

#Question 1a
auc_glm <- auc(train$failure, predict(m, newdata = train, type= "response"))
auc_glm
auc_glm2 <- auc(test$failure, predict(m, newdata = test, type= "response"))
auc_glm2

#Question 1b
m$coef
b0 <- m$coef[1]
x1 <- m$coef[2]
min(phase5$delta)
max(phase5$delta)
X1_range <- seq(from=min(phase5$delta), to=max(phase5$delta), by=.01)
a_logits <- b0 + x1*X1_range
a_probs <- exp(a_logits)/(1 + exp(a_logits))

plot(X1_range, a_probs,
     ylim=c(0,1),
     type="l", 
     lwd=3, 
     lty=2, 
     col="gold", 
     xlab="delta", ylab="P(outcome)", main="Probability of Failure")


#the graph above gives us a good visual for finding both values.
#for a prob of 0.6, we have a number around 4.5.
#for a prob of 0.8, our delta is then around 6.3.
#to find the precise values within R, we run twice a simple function
#to find the root of each.


f<-function (x) -0.6 + (exp(-2.7102890 + 0.6533929*x)/(1+exp(-2.7102890 + 0.6533929*x)))
uniroot(f,lower=0, upper=1, extendInt = "yes")$root

f<-function (x) -0.8 + (exp(-2.7102890 + 0.6533929*x)/(1+exp(-2.7102890 + 0.6533929*x)))
uniroot(f,lower=0, upper=1, extendInt = "yes")$root

#so for 60% we have a delta of exactly 4.7686
#for 80% we have a delta of 6.2697


#How many of these alarms would be fired per day on average according to your model?
p60 <- phase5[phase5$delta >= 4.6, ]
p80 <- phase5[phase5$delta >= 6.1, ]
head(p60)
head(p80)

testp60 <- summarise_at(group_by(phase5,failure,date),vars(delta),funs(mean(.,na.rm=TRUE)))
testp60$failure[testp60$delta>0.3]
tail(testp60,30)

sum(p60$failure)
sum(p80$failure)
#create a trans dummy column
p60['trans']<-1
p80['trans']<-1
head(p60)
head(p80)

#for total number of days
ndays <- length(unique(transdata[["date"]]))


sum(p60$trans)/ndays
sum(p80$trans)/ndays


#What % of these will be “false alarms” i.e. failure variable is equal to 0, 
#for each level of priority?
sum(p60$trans[p60$failure==0])/ndays

#7d)


sum(p80$trans[p80$failure==0])/ndays
