############################################################
# Example lever: ensure best product presence
############################################################

# Library loading ---------------------------------------------------------
library(data.table)
library(lubridate)
library(tidyverse)

# Data loading ------------------------------------------------------------
td = fread("/Users/marcos/OneDrive/ESADE/Term 2/Data Analytics with R/Challenge/Challenge 1/final_data/transactional_data.csv")
pd = fread("/Users/marcos/OneDrive/ESADE/Term 2/Data Analytics with R/Challenge/Challenge 1/final_data/product_data.csv")

# Fix variable formats ----------------------------------------------------
td[,class(timestamp)]
td[,timestamp:=ymd_hms(timestamp)]
td[,class(timestamp)]

pd[,margin:=(price/(1+tax_rate)) - cost] # price = (cost+margin)*(1+tax)
tdp = merge(td,pd,by='product_name',all.x=T)

# number of active days of each machine (hypothesis: if the machine doesnt sell anything in the whole day is because is broken)
tdp[,active_days_machine:=uniqueN(date),by=machine] 

# daily items per product and machine
daily_products = tdp[,.(daily_items = .N / active_days_machine[1]),
                     by=.(machine,product_name,category,type_drink_snack)] 
daily_products[, .(daily_items= mean(daily_items)), by = product_name][order(-daily_items)]

# average daily snacks/drinks per machine
daily_products[,daily_items_avg_cat:=mean(daily_items),by=.(machine, type_drink_snack)] 
# efficiency of the product in each machine standardizing by the "average snack/drink"
daily_products[,efficiency:=daily_items/daily_items_avg_cat] 
# average product efficiency over all machines and machine presence
efficiency_table = daily_products[,.(efficiency_avg=mean(efficiency),
                                     num_machines_present = uniqueN(machine)),
                                  by=.(product_name,category,type_drink_snack)][order(category,-efficiency_avg)] 
efficiency_table[,pct_machines_present:=num_machines_present/uniqueN(tdp$machine)]

efficiency_table[type_drink_snack == 'snack'][order(-efficiency_avg)][pct_machines_present < 0.7][num_machines_present > 30]
efficiency_table[type_drink_snack == 'drink'][order(-efficiency_avg)][pct_machines_present < 0.7][num_machines_present > 30]

# Top product presence ----------------------------------------------------
# Top5 snacks and drinks
top_snacks_name = efficiency_table[order(-efficiency_avg)][type_drink_snack=='snack'][1:5]$product_name
top_drinks_name = efficiency_table[order(-efficiency_avg)][type_drink_snack=='drink'][1:5]$product_name
# Bottom 5 snacks and drinks
worst_snacks_name =efficiency_table[order(efficiency_avg)][type_drink_snack=='snack'][1:5]$product_name
worst_drinks_name =efficiency_table[order(efficiency_avg)][type_drink_snack=='drink'][1:5]$product_name

tdp = merge(tdp, efficiency_table[,.(product_name,efficiency_avg)],by='product_name')

replacements = tdp[,.(
  num_top_snacks_missing = uniqueN(top_snacks_name) - sum(top_snacks_name %in% unique(product_name)),
  num_top_drinks_missing = uniqueN(top_drinks_name) - sum(top_drinks_name %in% unique(product_name)),
  num_worst_snacks_present = sum(worst_snacks_name %in% unique(product_name)),
  num_worst_drinks_present = sum(worst_drinks_name %in% unique(product_name))
),by=machine]

replacements[,snacks_to_change:=min(num_top_snacks_missing, num_worst_snacks_present),by=1:nrow(replacements)]
replacements[,drinks_to_change:=min(num_top_drinks_missing, num_worst_drinks_present),by=1:nrow(replacements)]
replacements[,products_to_change:=snacks_to_change+drinks_to_change]
replacements[,mean(products_to_change)] # ~1 product to change per machine

# Table of missing good snacks and present bad snacks
missing_good_snacks_machine = tdp[,.(good_snack_name = top_snacks_name[!top_snacks_name %in% unique(product_name)]),by=machine]
missing_good_snacks_machine = merge(missing_good_snacks_machine,
                                    efficiency_table[,.(good_snack_name = product_name,
                                                        efficiency_avg)],
                                    by='good_snack_name',all.x=T)

present_bad_snacks_machine = tdp[,.(bad_snack_name = worst_snacks_name[worst_snacks_name %in% unique(product_name)]),by=machine]
present_bad_snacks_machine = merge(present_bad_snacks_machine,
                                    efficiency_table[,.(bad_snack_name = product_name,
                                                        efficiency_avg)],
                                    by='bad_snack_name',all.x=T)

# Restrict to the number of products to change in each machine
# Missing good
missing_good_snacks_machine = merge(missing_good_snacks_machine,
      replacements[,.(machine,snacks_to_change)],
      by='machine',all.x=T)
missing_good_snacks_machine = missing_good_snacks_machine[order(-efficiency_avg)]
missing_good_snacks_machine_limited = missing_good_snacks_machine[,.SD[1:snacks_to_change[1]],by=machine]
missing_good_snacks_machine_limited = missing_good_snacks_machine_limited[snacks_to_change>0]

# Present bad
present_bad_snacks_machine = merge(present_bad_snacks_machine,
                                    replacements[,.(machine,snacks_to_change)],
                                    by='machine',all.x=T)
present_bad_snacks_machine = present_bad_snacks_machine[order(efficiency_avg)]

present_bad_snacks_machine_limited = present_bad_snacks_machine[,.SD[1:snacks_to_change[1]],by=machine]
present_bad_snacks_machine_limited = present_bad_snacks_machine_limited[snacks_to_change>0]

# Order by machine
present_bad_snacks_machine_limited = present_bad_snacks_machine_limited[order(machine)]
missing_good_snacks_machine_limited = missing_good_snacks_machine_limited[order(machine)]

in_out_dt = cbind(present_bad_snacks_machine_limited[,.(machine_out = machine, bad_snack_name,
                                                        efficiency_avg_bad = efficiency_avg)],
                  missing_good_snacks_machine_limited[,.(machine_in = machine, good_snack_name,
                                                         efficiency_avg_good = efficiency_avg)])
# Check
in_out_dt[machine_in!=machine_out]
in_out_dt[, .N, by = .(good_snack_name, bad_snack_name)][order(-N)]

# Compute daily items increase --------------------------------------------
in_out_dt = merge(in_out_dt,
      daily_products[,.(machine_out = machine, bad_snack_name = product_name, daily_items)],
      by=c('machine_out','bad_snack_name'),
      all.x=T)

# How would you calculate the "daily sales to win" by the replacement?
in_out_dt[,sales_boost:=(efficiency_avg_good /efficiency_avg_bad) ]
in_out_dt[,new_daily_items:= sales_boost*daily_items]
in_out_dt = merge(in_out_dt, pd[,.(good_snack_name = product_name,
                       good_snack_margin = margin)], by='good_snack_name',all.x=T)
in_out_dt = merge(in_out_dt, pd[,.(bad_snack_name = product_name,
                                   bad_snack_margin = margin)], by='bad_snack_name',all.x=T)

in_out_dt[,prev_profit:=bad_snack_margin*daily_items]
in_out_dt[,new_profit:=good_snack_margin*new_daily_items]
in_out_dt[,profit_gain:=new_profit - prev_profit]
in_out_dt[,sum(profit_gain)*365*0.9]

# What about drinks?

