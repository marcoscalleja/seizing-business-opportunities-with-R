
join1 <- merge(transdata,proddata,by = "product_name")
join2 <- merge(join1, machdata, by="machine")

#Snacks v Drinks v Total
countdaysnack <- join2[type_drink_snack == "snack"]
countdaydrink <- join2[type_drink_snack == "drink"]
summary(countdaysnack)
summary(countdaydrink)
daysnack <- countdaysnack[, .N, by=(date)]
daydrink <- countdaydrink[, .N, by=(date)]
sdjoin <- merge(daysnack,daydrink,by = "date")
names(sdjoin)[2] <- "snack"
names(sdjoin)[3] <- "drink"

sdjoin[, Total:= snack + drink]
head(sdjoin)

dfplot <- sdjoin %>% gather(key, value, -date)
ggplot(dfplot, mapping = aes(x = date, y = value, color = key) ) + geom_line()

mean(daysnack$N)
mean(daydrink$N)
mean(sdjoin$Total)

#By Category
unique(join1$category)
daycateg <- join2[, .N, by=.(date, category)]

countcookie <- join2[category == "Cookies, pastries and cereals"]
countchoco <- join2[category == "Chocolate based"]
countsalty <- join2[category == "Salty"]
countcarbonates <- join2[category == "Carbonates and energy drinks"]
countjuice <- join2[category == "Juice, tea and smoothies"]
countmilk <- join2[category == "Milk based"]
countsugar <- join2[category == "Sugar candy"]
countwater <- join2[category == "Water"]

daycookie <- countcookie[, .N, by=(date)]
daychoco <- countchoco[, .N, by=(date)]
daysalty <- countsalty[, .N, by=(date)]
daycarb <- countcarbonates[, .N, by=(date)]
dayjuice <- countjuice[, .N, by=(date)]
daymilk <- countmilk[, .N, by=(date)]
daysugar <- countsugar[, .N, by=(date)]
daywater <- countwater[, .N, by=(date)]

sdjoin <- merge(daycookie,daychoco, by = "date")
names(sdjoin)[2]<- "Cookies, pastries and cereals" 
names(sdjoin)[3]<- "Chocolate based" 
sdjoin <- merge(sdjoin,daysalty, by = "date")
names(sdjoin)[4]<-  "Salty"
sdjoin <- merge(sdjoin,daycarb, by = "date")
names(sdjoin)[5]<-  "Carbonates and energy drinks"
sdjoin <- merge(sdjoin,dayjuice, by = "date")
names(sdjoin)[6]<-  "Juice, tea and smoothies"
sdjoin <- merge(sdjoin,daymilk, by = "date")
names(sdjoin)[7]<-  "Milk based"
sdjoin <- merge(sdjoin,daysugar, by = "date")
names(sdjoin)[8]<-  "Sugar candy"
sdjoin <- merge(sdjoin,daywater, by = "date")
names(sdjoin)[9]<-  "Water"

head(sdjoin)

mean(sdjoin$`Chocolate based`)

dfplot <- sdjoin %>% gather(key, value, -date)
ggplot(dfplot, mapping = aes(x = date, y = value, color = key) ) + geom_line(size=2) + 
  ggsave("/Users/marcos/Desktop/category_plot.png", width = 20, height = 10, dpi = 300)


#Pie Chart Exercise 1

# Create data for the graph.
x <- c(sum(daycookie$N), sum(daychoco$N), sum(daysalty$N), sum(daycarb$N), sum(dayjuice$N)
       ,sum(daymilk$N),sum(daysugar$N),sum(daywater$N))
labels <- unique(join1$category)

# Plot the chart with title and rainbow color pallet.
#pie(x, labels = piepercent, main = "Transactions Per Product Category",col = rainbow(length(x)))
legend("topright", labels, cex = 0.8,
       fill = rainbow(length(x)))


library(plotrix)

pie3D(x,labels = lbl,explode = 0.1, main = "Pie Chart of Countries ")

legend("topright", labels, cex = 0.8,
       fill = rainbow(length(x)))


#In line 71 of Challenge 1's code we define the avg price per category
avg_price_cat <- ddply(proddata, .(category), summarize,  avg_price=mean(price))

# barchart with added parameters
ggplot(avg_price_cat, aes(x=avg_price_cat$avg_price, y=avg_price_cat$category, fill=avg_price_cat$avg_price)) +
  geom_bar(stat="identity")+theme_minimal() + labs(y = "Category", x = "Average Price", fill = "Average Price", face = "bold.italic", color = "black", size = 16)

#barplot(avg_price_cat$avg_price,
#        main = "Average price products per category",
#        xlab = "Average price",
#        ylab = "category",
#        col = "darkblue",
#        horiz = TRUE)


