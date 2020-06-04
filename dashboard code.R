library(lubridate)

# Assumptions/ thresholds
billing_duration_cutoff <- 30 #in secs
trial_duration_cutoff <- 30 #in secs
billing_duration_cutoff <- 30 #in secs
dropoff_duration_cutoff <- 300 #in secs
new_visitor_period <- 30 #in days
store_area <- c('area1','area2','area3','area4','area5','area6','area_trial1','area_trial2','area_billing','area_out1','area_out2')

setwd('D:/Work/Simplify Analytics Consulting/IoT')
MacVisitSV <- read.csv('MacVisitSV.csv', stringsAsFactors = FALSE)
MacVisitSV$time_entry <- paste0(MacVisitSV$date,' ',MacVisitSV$time_entry)
MacVisitSV$time_exit <- paste0(MacVisitSV$date,' ',MacVisitSV$time_exit)
MacVisitSV$date <- as.Date(MacVisitSV$date, '%m/%d/%Y')

MacVisitSV$time_entry <- as.POSIXct(MacVisitSV$time_entry, format = "%m/%d/%Y %H:%M:%S")
MacVisitSV$time_exit <- as.POSIXct(MacVisitSV$time_exit, format = "%m/%d/%Y %H:%M:%S")

MacVisitSV$entrytimeofday <- ifelse(hour(MacVisitSV$time_entry)>=9 & hour(MacVisitSV$time_entry)<12,'Morning (9-12 AM)',ifelse(hour(MacVisitSV$time_entry)>=12 & hour(MacVisitSV$time_entry)<16,'Afternoon (12-4 PM)',ifelse(hour(MacVisitSV$time_entry)>=16 & hour(MacVisitSV$time_entry)<19,'Evening (4-7 PM)',ifelse(hour(MacVisitSV$time_entry)>=19 & hour(MacVisitSV$time_entry)<22,'Night (7-10 PM)',NA))))

for(k in store_area){
  MacVisitSV[,k][is.na(MacVisitSV[,k])] <- 0
}

# total visitors
tot_visitors_yday <- length(unique(MacVisitSV[MacVisitSV$date==today()-1,'mac']))
# week starts from sunday
tot_visitors_w2d <- length(unique(MacVisitSV[MacVisitSV$date>=today()-wday(today()) & MacVisitSV$date<=today()-1,'mac']))
tot_visitors_m2d <- length(unique(MacVisitSV[MacVisitSV$date>=today()-day(today())+1 & MacVisitSV$date<=today()-1,'mac']))

# billed visitors 
billed_visitors_yday <- length(unique(MacVisitSV[MacVisitSV$date==today()-1 & MacVisitSV$area_billing>=billing_duration_cutoff,'mac']))
billed_visitors_w2d <- length(unique(MacVisitSV[MacVisitSV$date>=today()-wday(today()) & MacVisitSV$date<=today()-1 & MacVisitSV$area_billing>=billing_duration_cutoff,'mac']))
billed_visitors_m2d <- length(unique(MacVisitSV[MacVisitSV$date>=today()-day(today())+1 & MacVisitSV$date<=today()-1 & MacVisitSV$area_billing>=billing_duration_cutoff,'mac']))

# trial visitors 
trial_visitors_yday <- length(unique(MacVisitSV[MacVisitSV$date==today()-1 & (MacVisitSV$area_trial1>=trial_duration_cutoff | MacVisitSV$area_trial2>=trial_duration_cutoff),'mac']))
trial_visitors_w2d <- length(unique(MacVisitSV[MacVisitSV$date>=today()-wday(today()) & MacVisitSV$date<=today()-1 & (MacVisitSV$area_trial1>=trial_duration_cutoff | MacVisitSV$area_trial2>=trial_duration_cutoff),'mac']))
trial_visitors_m2d <- length(unique(MacVisitSV[MacVisitSV$date>=today()-day(today())+1 & MacVisitSV$date<=today()-1 & (MacVisitSV$area_trial1>=trial_duration_cutoff | MacVisitSV$area_trial2>=trial_duration_cutoff),'mac']))

#new and repeat visitors







# dropoffs (shoppers who left in less than 5 min)
dropoffs_yday <- length(unique(MacVisitSV[MacVisitSV$date==today()-1 & MacVisitSV$duration<dropoff_duration_cutoff,'mac']))
dropoffs_w2d <- length(unique(MacVisitSV[MacVisitSV$date>=today()-wday(today()) & MacVisitSV$date<=today()-1 & MacVisitSV$duration<dropoff_duration_cutoff,'mac']))
dropoffs_m2d <- length(unique(MacVisitSV[MacVisitSV$date>=today()-day(today())+1 & MacVisitSV$date<=today()-1 & MacVisitSV$duration<dropoff_duration_cutoff,'mac']))

# browsers (shoppers who stayed for more than 5 min)
browsers_yday <- length(unique(MacVisitSV[MacVisitSV$date==today()-1 & MacVisitSV$duration>dropoff_duration_cutoff,'mac']))
browsers_w2d <- length(unique(MacVisitSV[MacVisitSV$date>=today()-wday(today()) & MacVisitSV$date<=today()-1 & MacVisitSV$duration>dropoff_duration_cutoff,'mac']))
browsers_m2d <- length(unique(MacVisitSV[MacVisitSV$date>=today()-day(today())+1 & MacVisitSV$date<=today()-1 & MacVisitSV$duration>dropoff_duration_cutoff,'mac']))

# area dwell times
area_duration_yday <- data.frame()
area_duration_w2d <- data.frame()
area_duration_m2d <- data.frame()
for(i in store_area){
area_duration_yday <- rbind(area_duration_yday, data.frame(Area=i, tot_dwell_time=sum(MacVisitSV[MacVisitSV$date==today()-1 & !is.na(MacVisitSV[,i]),i])))
area_duration_w2d <- rbind(area_duration_w2d, data.frame(Area=i, tot_dwell_time=sum(MacVisitSV[MacVisitSV$date>=today()-wday(today()) & MacVisitSV$date<=today()-1 & !is.na(MacVisitSV[,i]),i])))
area_duration_m2d <- rbind(area_duration_m2d, data.frame(Area=i, tot_dwell_time=sum(MacVisitSV[MacVisitSV$date>=today()-day(today())+1 & MacVisitSV$date<=today()-1 & !is.na(MacVisitSV[,i]),i])))
}


# browsers by area
area_browsers_yday <- data.frame()
area_browsers_w2d <- data.frame()
area_browsers_m2d <- data.frame()
for(i in store_area){
  area_browsers_yday <- rbind(area_browsers_yday, data.frame(Area=i, browsers=length(unique(MacVisitSV[MacVisitSV$date==today()-1 & MacVisitSV$duration>dropoff_duration_cutoff,'mac']))))
  area_browsers_w2d <- rbind(area_browsers_w2d, data.frame(Area=i, browsers=length(unique(MacVisitSV[MacVisitSV$date>=today()-wday(today()) & MacVisitSV$date<=today()-1 & MacVisitSV$duration>dropoff_duration_cutoff,'mac']))))
  area_browsers_m2d <- rbind(area_browsers_m2d, data.frame(Area=i, browsers=length(unique(MacVisitSV[MacVisitSV$date>=today()-day(today())+1 & MacVisitSV$date<=today()-1 & MacVisitSV$duration>dropoff_duration_cutoff,'mac']))))
}

# billers dwell time by area
area_billersduration_yday <- data.frame()
area_billersduration_w2d <- data.frame()
area_billersduration_m2d <- data.frame()
for(i in store_area){
  area_billersduration_yday <- rbind(area_billersduration_yday, data.frame(Area=i, billers_dwell_time=sum(MacVisitSV[MacVisitSV$date==today()-1 & MacVisitSV$area_billing>=billing_duration_cutoff & !is.na(MacVisitSV[,i]),i])))
  area_billersduration_w2d <- rbind(area_billersduration_w2d, data.frame(Area=i, billers_dwell_time=sum(MacVisitSV[MacVisitSV$date>=today()-wday(today()) & MacVisitSV$date<=today()-1 & MacVisitSV$area_billing>=billing_duration_cutoff & !is.na(MacVisitSV[,i]),i])))
  area_billersduration_m2d <- rbind(area_billersduration_m2d, data.frame(Area=i, billers_dwell_time=sum(MacVisitSV[MacVisitSV$date>=today()-day(today())+1 & MacVisitSV$date<=today()-1 & MacVisitSV$area_billing>=billing_duration_cutoff & !is.na(MacVisitSV[,i]),i])))
}

# trial room dwell time by area
area_trialduration_yday <- data.frame()
area_trialduration_w2d <- data.frame()
area_trialduration_m2d <- data.frame()
for(i in store_area){
  area_trialduration_yday <- rbind(area_trialduration_yday, data.frame(Area=i, billers_dwell_time=sum(MacVisitSV[MacVisitSV$date==today()-1 & (MacVisitSV$area_trial1>=trial_duration_cutoff | MacVisitSV$area_trial2>=trial_duration_cutoff) & !is.na(MacVisitSV[,i]),i])))
  area_trialduration_w2d <- rbind(area_trialduration_w2d, data.frame(Area=i, billers_dwell_time=sum(MacVisitSV[MacVisitSV$date>=today()-wday(today()) & MacVisitSV$date<=today()-1 & (MacVisitSV$area_trial1>=trial_duration_cutoff | MacVisitSV$area_trial2>=trial_duration_cutoff) & !is.na(MacVisitSV[,i]),i])))
  area_trialduration_m2d <- rbind(area_trialduration_m2d, data.frame(Area=i, billers_dwell_time=sum(MacVisitSV[MacVisitSV$date>=today()-day(today())+1 & MacVisitSV$date<=today()-1 & (MacVisitSV$area_trial1>=trial_duration_cutoff | MacVisitSV$area_trial2>=trial_duration_cutoff) & !is.na(MacVisitSV[,i]),i])))
}

# Browsing duration
browseduration_yday <- data.frame()
browseduration_w2d <- data.frame()
browseduration_m2d <- data.frame()

browseduration_yday <- rbind(browseduration_yday, data.frame(duration='< 5 mins', browsers=length(unique(MacVisitSV[MacVisitSV$date==today()-1 & MacVisitSV$duration<dropoff_duration_cutoff,'mac']))))
browseduration_yday <- rbind(browseduration_yday, data.frame(duration='< 5-10 mins', browsers=length(unique(MacVisitSV[MacVisitSV$date==today()-1 & MacVisitSV$duration>=300 & MacVisitSV$duration<600,'mac']))))
browseduration_yday <- rbind(browseduration_yday, data.frame(duration='< 10-20 mins', browsers=length(unique(MacVisitSV[MacVisitSV$date==today()-1 & MacVisitSV$duration>=600 & MacVisitSV$duration<1200,'mac']))))
browseduration_yday <- rbind(browseduration_yday, data.frame(duration='> 20 mins', browsers=length(unique(MacVisitSV[MacVisitSV$date==today()-1 & MacVisitSV$duration>=1200,'mac']))))

browseduration_w2d <- rbind(browseduration_w2d, data.frame(duration='< 5 mins', browsers=length(unique(MacVisitSV[MacVisitSV$date>=today()-wday(today()) & MacVisitSV$date<=today()-1 & MacVisitSV$duration<dropoff_duration_cutoff,'mac']))))
browseduration_w2d <- rbind(browseduration_w2d, data.frame(duration='< 5-10 mins', browsers=length(unique(MacVisitSV[MacVisitSV$date>=today()-wday(today()) & MacVisitSV$date<=today()-1 & MacVisitSV$duration>=300 & MacVisitSV$duration<600,'mac']))))
browseduration_w2d <- rbind(browseduration_w2d, data.frame(duration='< 10-20 mins', browsers=length(unique(MacVisitSV[MacVisitSV$date>=today()-wday(today()) & MacVisitSV$date<=today()-1 & MacVisitSV$duration>=600 & MacVisitSV$duration<1200,'mac']))))
browseduration_w2d <- rbind(browseduration_w2d, data.frame(duration='> 20 mins', browsers=length(unique(MacVisitSV[MacVisitSV$date>=today()-wday(today()) & MacVisitSV$date<=today()-1 & MacVisitSV$duration>=1200,'mac']))))

browseduration_m2d <- rbind(browseduration_m2d, data.frame(duration='< 5 mins', browsers=length(unique(MacVisitSV[MacVisitSV$date>=today()-day(today())+1 & MacVisitSV$date<=today()-1 & MacVisitSV$duration<dropoff_duration_cutoff,'mac']))))
browseduration_m2d <- rbind(browseduration_m2d, data.frame(duration='< 5-10 mins', browsers=length(unique(MacVisitSV[MacVisitSV$date>=today()-day(today())+1 & MacVisitSV$date<=today()-1 & MacVisitSV$duration>=300 & MacVisitSV$duration<600,'mac']))))
browseduration_m2d <- rbind(browseduration_m2d, data.frame(duration='< 10-20 mins', browsers=length(unique(MacVisitSV[MacVisitSV$date>=today()-day(today())+1 & MacVisitSV$date<=today()-1 & MacVisitSV$duration>=600 & MacVisitSV$duration<1200,'mac']))))
browseduration_m2d <- rbind(browseduration_m2d, data.frame(duration='> 20 mins', browsers=length(unique(MacVisitSV[MacVisitSV$date>=today()-day(today())+1 & MacVisitSV$date<=today()-1 & MacVisitSV$duration>=1200,'mac']))))

# drop offs by time of day
#yday
dropoff_yday <- aggregate(mac~entrytimeofday, data=MacVisitSV[MacVisitSV$date==today()-1 & MacVisitSV$duration<dropoff_duration_cutoff,c('mac','entrytimeofday')], FUN= function(x) length(unique(x)))
#w2d
dropoff_w2d <- aggregate(mac~entrytimeofday, data=MacVisitSV[MacVisitSV$date>=today()-wday(today()) & MacVisitSV$date<=today()-1 & MacVisitSV$duration<dropoff_duration_cutoff,c('mac','entrytimeofday')], FUN= function(x) length(unique(x)))
#m2d
dropoff_m2d <- aggregate(mac~entrytimeofday, data=MacVisitSV[MacVisitSV$date>=today()-day(today())+1 & MacVisitSV$date<=today()-1 & MacVisitSV$duration<dropoff_duration_cutoff,c('mac','entrytimeofday')], FUN= function(x) length(unique(x)))

# tot visitors by time of day
#yday
visitors_yday <- aggregate(mac~entrytimeofday, data=MacVisitSV[MacVisitSV$date==today()-1,c('mac','entrytimeofday')], FUN= function(x) length(unique(x)))
#w2d
visitors_w2d <- aggregate(mac~entrytimeofday, data=MacVisitSV[MacVisitSV$date>=today()-wday(today()) & MacVisitSV$date<=today()-1,c('mac','entrytimeofday')], FUN= function(x) length(unique(x)))
#m2d
visitors_m2d <- aggregate(mac~entrytimeofday, data=MacVisitSV[MacVisitSV$date>=today()-day(today())+1 & MacVisitSV$date<=today()-1,c('mac','entrytimeofday')], FUN= function(x) length(unique(x)))

# trial customers by time of day
#yday
trial_yday <- aggregate(mac~entrytimeofday, data=MacVisitSV[MacVisitSV$date==today()-1 & (MacVisitSV$area_trial1>=trial_duration_cutoff | MacVisitSV$area_trial2>=trial_duration_cutoff),c('mac','entrytimeofday')], FUN= function(x) length(unique(x)))
#w2d
trial_w2d <- aggregate(mac~entrytimeofday, data=MacVisitSV[MacVisitSV$date>=today()-wday(today()) & MacVisitSV$date<=today()-1 & (MacVisitSV$area_trial1>=trial_duration_cutoff | MacVisitSV$area_trial2>=trial_duration_cutoff),c('mac','entrytimeofday')], FUN= function(x) length(unique(x)))
#m2d
trial_m2d <- aggregate(mac~entrytimeofday, data=MacVisitSV[MacVisitSV$date>=today()-day(today())+1 & MacVisitSV$date<=today()-1 & (MacVisitSV$area_trial1>=trial_duration_cutoff | MacVisitSV$area_trial2>=trial_duration_cutoff),c('mac','entrytimeofday')], FUN= function(x) length(unique(x)))


# Final tables
visitors_yday2 <- merge(visitors_yday, dropoff_yday, by='entrytimeofday', all.x = TRUE)
colnames(visitors_yday2) <- c("entrytimeofday","total_visitors", "dropoffs")
visitors_yday3 <- merge(visitors_yday2, trial_yday, by='entrytimeofday', all.x = TRUE)
colnames(visitors_yday3)[4] <- 'trial_visitors'

visitors_w2d2 <- merge(visitors_w2d, dropoff_w2d, by='entrytimeofday', all.x = TRUE)
colnames(visitors_w2d2) <- c("entrytimeofday","total_visitors", "dropoffs")
visitors_w2d3 <- merge(visitors_w2d2, trial_w2d, by='entrytimeofday', all.x = TRUE)
colnames(visitors_w2d3)[4] <- 'trial_visitors'

visitors_m2d2 <- merge(visitors_m2d, dropoff_m2d, by='entrytimeofday', all.x = TRUE)
colnames(visitors_m2d2) <- c("entrytimeofday","total_visitors", "dropoffs")
visitors_m2d3 <- merge(visitors_m2d2, trial_m2d, by='entrytimeofday', all.x = TRUE)
colnames(visitors_m2d3)[4] <- 'trial_visitors'


################## Metrics for trend charts ##################################
# daily_trends
visitor_trend_daily <- aggregate(mac~date, data=MacVisitSV[MacVisitSV$date<=today()-1 & MacVisitSV$date>=today()-13,], FUN = function(x) length(unique(x)))
trialist_trend_daily <- aggregate(mac~date, data=MacVisitSV[MacVisitSV$date<=today()-1 & MacVisitSV$date>=today()-13 & (MacVisitSV$area_trial1>=trial_duration_cutoff | MacVisitSV$area_trial2>=trial_duration_cutoff),], FUN = function(x) length(unique(x)))
dropoff_trend_daily <- aggregate(mac~date, data=MacVisitSV[MacVisitSV$date<=today()-1 & MacVisitSV$date>=today()-13 & MacVisitSV$duration<dropoff_duration_cutoff,], FUN = function(x) length(unique(x)))
billed_trend_daily <- aggregate(mac~date, data=MacVisitSV[MacVisitSV$date<=today()-1 & MacVisitSV$date>=today()-13 & MacVisitSV$area_billing>=billing_duration_cutoff,], FUN = function(x) length(unique(x)))
browser_trend_daily <- aggregate(mac~date, data=MacVisitSV[MacVisitSV$date<=today()-1 & MacVisitSV$date>=today()-13 & MacVisitSV$duration>dropoff_duration_cutoff,], FUN = function(x) length(unique(x)))
area_duration_trend_daily <- aggregate(cbind(area1,area2,area3,area4,area5,area_trial1,area_trial2,area_billing,area_out1,area_out2)~date, data=MacVisitSV[MacVisitSV$date<=today()-1 & MacVisitSV$date>=today()-13,], FUN = sum)
area_duration_billed_trend_daily <- aggregate(cbind(area1,area2,area3,area4,area5,area_trial1,area_trial2,area_billing,area_out1,area_out2)~date, data=MacVisitSV[MacVisitSV$date<=today()-1 & MacVisitSV$date>=today()-13 & MacVisitSV$area_billing>=billing_duration_cutoff,], FUN = sum)
area_duration_trial_trend_daily <- aggregate(cbind(area1,area2,area3,area4,area5,area_trial1,area_trial2,area_billing,area_out1,area_out2)~date, data=MacVisitSV[MacVisitSV$date<=today()-1 & MacVisitSV$date>=today()-13 & (MacVisitSV$area_trial1>=trial_duration_cutoff | MacVisitSV$area_trial2>=trial_duration_cutoff),], FUN = sum)

