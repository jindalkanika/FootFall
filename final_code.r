library(data.table)
library(plyr)
library(dplyr)


basedata <- read.csv("locationdata3.csv", stringsAsFactors = FALSE)
basedata <- basedata[,c(2,3,13)]
colnames(basedata)[c(2,3)] <- c("time","location")

basedata$time <- as.POSIXct(basedata$time,format = "%Y-%m-%d %H:%M:%S")
basedata$date <- substr(basedata$time,1,10)
basedata$date <- as.Date(basedata$date)

basedata <- basedata[order(basedata$source_address, basedata$time),]
basedata2 <- basedata[1,]
j <- 1
for(i in 2:nrow(basedata)){
  if(basedata[i,1] != basedata2[j,1]){
    j <- j+1
    basedata2 <- rbind(basedata2,basedata[i,])
  }
  if((basedata[i,1] == basedata2[j,1]) & (basedata[i,3] != basedata2[j,3])){
    j <- j+1
    basedata2 <- rbind(basedata2,basedata[i,])
  }
}

mac_count <- as.data.frame(table(basedata2$source_address))
colnames(mac_count) <- c("mac","freq")
mac_count <- mac_count[mac_count$freq>3,]
basedata2 <- basedata2[basedata2$source_address %in% mac_count$mac,]

basedata2$movement_flag <- NA
basedata2$id_visit <- NA

fn_assign_first_movement_flag <- function(df,k){
  if(df$location[k] == "outside"){
    df[k,5] <- "entry_frm_out"
    df[k,6] <- 1
  }else{
    df[k,5] <- "entry_frm_in"
    df[k,6] <- 1
  }
  return(df)
}

fn_assign_movement_visit <- function(bd){
  bd <- bd[order(bd$time),]
  bd <- fn_assign_first_movement_flag(bd,1)
  for(i in 2:nrow(bd)){
    if(bd$location[i] == "outside" & bd$location[i-1] != "outside"){
      bd[i,5] <- "exit_to_out"
      bd[i,6] <- bd[i-1,6]
    }else if(bd$location[i-1] == "outside"){
      bd[i,5] <- "entry_frm_out"
      if(as.integer((bd$time[i]-bd$time[i-1])*60)>300){
        bd[i,6] <- bd[i-1,6] + 1
      }else{
        bd[i,6] <- bd[i-1,6]
      }
    }else{
      bd[i,5] <- "inside_store"
      bd[i,6] <- bd[i-1,6]
    }
  }
  return(bd)
}


basedata3 <- ddply(basedata2,c("source_address") , fn_assign_movement_visit)
basedata3 <- data.table(basedata3)
basedata3 <- basedata3[, duration := c( NA,`units<-`(diff(time), "secs")), by = c("source_address","id_visit")]

basedata3 <- basedata3 %>%
  mutate(duration=lead(duration))



f1 <- aggregate(basedata3$time, by = list(basedata3$source_address,basedata3$id_visit), min)
f1 <- data.table(f1)
colnames(f1) <- c("source_address","id_visit","start")
basedata3 <- merge(basedata3, f1, by = c("source_address","id_visit"), all = FALSE)

f2 <- aggregate(basedata3$time, by = list(basedata3$source_address,basedata3$id_visit), max)
f2 <- data.table(f2)
colnames(f2) <- c("source_address","id_visit","end")
basedata3  <- merge(basedata3, f2, by = c("source_address","id_visit"), all = FALSE)

basedata3 <- basedata3[order(basedata3$source_address,basedata3$time),]
 basedata3$index <- NA
 basedata3[1,10] <- 1

 for(i in 2:nrow(basedata3)){
   if((basedata3[i,1] == basedata3[i-1,1]) & (basedata3[i,2] == basedata3[i-1,2])){
     basedata3[i,10] <- basedata3[i-1,10]+1
   }else{
     basedata3[i,10] <- 1
   }
 }


#DURATION
basedata3$TotalDuration <- round((basedata3$end - basedata3$start))
basedata3a <- aggregate(duration~source_address+id_visit+location+start+end+date+TotalDuration, basedata3,sum)

singleview<- reshape(basedata3a,v.names = "duration",idvar = c("source_address","id_visit","start","end"),timevar = "location",direction = "wide")

basedata4 <- basedata3[basedata3$duration>=60,c(1,2,4,10)]

singleview_path <- reshape(basedata4,v.names = "location",idvar = c("source_address","id_visit"),timevar = "index",direction = "wide")
singleview$fl_footfall <- NA
for(i in 1:nrow(singleview)){
  if(singleview[i,c("TotalDuration")]<=300){
    singleview[i,c("fl_footfall")]<- "Drop-Offs" #drop offs
  }
  
  if(singleview[i,c("TotalDuration")]>300){
    singleview[i,c("fl_footfall")] <- "Browsers" #drop offs
  }
  
  if(!(is.na(singleview[i,c("duration.micro")]))){
    if(singleview[i,c("duration.micro")]>60){
    singleview[i,c("fl_footfall")] <- "Trialist"
    }
  }
  
  if(!(is.na(singleview[i,c("duration.chair")])))
    if(singleview[i,c("duration.chair")]>60)
    singleview[i,c("fl_footfall")] <- "Buyer"
  
  if(!(is.na(singleview[i,c("duration.chair")]))& 
     !(is.na(singleview[i,c("duration.micro")]))){
    if((singleview[i,c("duration.micro")]>60)&(singleview[i,c("duration.chair")]>60))
    {
      singleview[i,c("fl_footfall")] <- "Buyer and Trialist"
    } 
  }
}


# singleview$FootfallAnalysis <- ""

# View(basedata3)
# View(singleview)
# View(singleview_path)

library(lubridate)

# Assumptions/ thresholds
billing_duration_cutoff <- 30 #in secs
trial_duration_cutoff <- 30 #in secs
billing_duration_cutoff <- 30 #in secs
dropoff_duration_cutoff <- 300 #in secs
new_visitor_period <- 30 #in days
store_area <- c('area1','area2','area3','area_trial1','area_billing','area_out1')




# MacVisitSV <- read.csv('MacVisitSV.csv', stringsAsFactors = FALSE)
MacVisitSV <- singleview
colnames(MacVisitSV) <- c("mac", "id_visit", "time_entry", "time_exit", "date", "duration", "area_billing", "area1", "area2", "area3", "area_trial1", "area_out1", "fl_footfall")
# MacVisitSV$time_entry <- paste0(MacVisitSV$date,' ',MacVisitSV$time_entry)
# MacVisitSV$time_exit <- paste0(MacVisitSV$date,' ',MacVisitSV$time_exit)
# MacVisitSV$date <- as.Date(MacVisitSV$date, '%m/%d/%Y')

# MacVisitSV$time_entry <- as.POSIXct(MacVisitSV$time_entry, format = "%m/%d/%Y %H:%M:%S")
# MacVisitSV$time_exit <- as.POSIXct(MacVisitSV$time_exit, format = "%m/%d/%Y %H:%M:%S")

MacVisitSV$entrytimeofday <- ifelse(hour(MacVisitSV$time_entry)>=9 & hour(MacVisitSV$time_entry)<12,'Morning (9-12 AM)',ifelse(hour(MacVisitSV$time_entry)>=12 & hour(MacVisitSV$time_entry)<16,'Afternoon (12-4 PM)',ifelse(hour(MacVisitSV$time_entry)>=16 & hour(MacVisitSV$time_entry)<19,'Evening (4-7 PM)',ifelse(hour(MacVisitSV$time_entry)>=19 & hour(MacVisitSV$time_entry)<22,'Night (7-10 PM)',NA))))

for(k in store_area){
  MacVisitSV[,k][is.na(MacVisitSV[,k])] <- 0
}

# total visitors
tot_visitors_yday <- length(unique(MacVisitSV[MacVisitSV$date==today()-1,'mac']))
# week starts from sunday
tot_visitors_w2d <- length(unique(MacVisitSV[MacVisitSV$date>today()-wday(today()) & MacVisitSV$date<=today()-1,'mac']))
tot_visitors_m2d <- length(unique(MacVisitSV[MacVisitSV$date>=today()-day(today())+1 & MacVisitSV$date<=today()-1,'mac']))

# billed visitors 
billed_visitors_yday <- length(unique(MacVisitSV[MacVisitSV$date==today()-1 & MacVisitSV$area_billing>=billing_duration_cutoff,'mac']))
billed_visitors_w2d <- length(unique(MacVisitSV[MacVisitSV$date>today()-wday(today()) & MacVisitSV$date<=today()-1 & MacVisitSV$area_billing>=billing_duration_cutoff,'mac']))
billed_visitors_m2d <- length(unique(MacVisitSV[MacVisitSV$date>=today()-day(today())+1 & MacVisitSV$date<=today()-1 & MacVisitSV$area_billing>=billing_duration_cutoff,'mac']))

# trial visitors 
# trial_visitors_yday <- length(unique(MacVisitSV[MacVisitSV$date==today()-1 & (MacVisitSV$area_trial1>=trial_duration_cutoff | MacVisitSV$area_trial2>=trial_duration_cutoff),'mac']))
# trial_visitors_w2d <- length(unique(MacVisitSV[MacVisitSV$date>today()-wday(today()) & MacVisitSV$date<=today()-1 & (MacVisitSV$area_trial1>=trial_duration_cutoff | MacVisitSV$area_trial2>=trial_duration_cutoff),'mac']))
# trial_visitors_m2d <- length(unique(MacVisitSV[MacVisitSV$date>=today()-day(today())+1 & MacVisitSV$date<=today()-1 & (MacVisitSV$area_trial1>=trial_duration_cutoff | MacVisitSV$area_trial2>=trial_duration_cutoff),'mac']))

trial_visitors_yday <- length(unique(MacVisitSV[MacVisitSV$date==today()-1 & (MacVisitSV$area_trial1>=trial_duration_cutoff),'mac']))
trial_visitors_w2d <- length(unique(MacVisitSV[MacVisitSV$date>today()-wday(today()) & MacVisitSV$date<=today()-1 & (MacVisitSV$area_trial1>=trial_duration_cutoff),'mac']))
trial_visitors_m2d <- length(unique(MacVisitSV[MacVisitSV$date>=today()-day(today())+1 & MacVisitSV$date<=today()-1 & (MacVisitSV$area_trial1>=trial_duration_cutoff),'mac']))


#new and repeat visitors







# dropoffs (shoppers who left in less than 5 min)
dropoffs_yday <- length(unique(MacVisitSV[MacVisitSV$date==today()-1 & MacVisitSV$duration<dropoff_duration_cutoff,'mac']))
dropoffs_w2d <- length(unique(MacVisitSV[MacVisitSV$date>today()-wday(today()) & MacVisitSV$date<=today()-1 & MacVisitSV$duration<dropoff_duration_cutoff,'mac']))
dropoffs_m2d <- length(unique(MacVisitSV[MacVisitSV$date>=today()-day(today())+1 & MacVisitSV$date<=today()-1 & MacVisitSV$duration<dropoff_duration_cutoff,'mac']))

# browsers (shoppers who stayed for more than 5 min)
browsers_yday <- length(unique(MacVisitSV[MacVisitSV$date==today()-1 & MacVisitSV$duration>dropoff_duration_cutoff,'mac']))
browsers_w2d <- length(unique(MacVisitSV[MacVisitSV$date>today()-wday(today()) & MacVisitSV$date<=today()-1 & MacVisitSV$duration>dropoff_duration_cutoff,'mac']))
browsers_m2d <- length(unique(MacVisitSV[MacVisitSV$date>=today()-day(today())+1 & MacVisitSV$date<=today()-1 & MacVisitSV$duration>dropoff_duration_cutoff,'mac']))

# area dwell times
area_duration_yday <- data.frame()
area_duration_w2d <- data.frame()
area_duration_m2d <- data.frame()
for(i in store_area){
  area_duration_yday <- rbind(area_duration_yday, data.frame(Area=i, tot_dwell_time=sum(MacVisitSV[MacVisitSV$date==today()-1 & !is.na(MacVisitSV[,i]),i])))
  area_duration_w2d <- rbind(area_duration_w2d, data.frame(Area=i, tot_dwell_time=sum(MacVisitSV[MacVisitSV$date>today()-wday(today()) & MacVisitSV$date<=today()-1 & !is.na(MacVisitSV[,i]),i])))
  area_duration_m2d <- rbind(area_duration_m2d, data.frame(Area=i, tot_dwell_time=sum(MacVisitSV[MacVisitSV$date>=today()-day(today())+1 & MacVisitSV$date<=today()-1 & !is.na(MacVisitSV[,i]),i])))
}


# browsers by area
area_browsers_yday <- data.frame()
area_browsers_w2d <- data.frame()
area_browsers_m2d <- data.frame()
for(i in store_area){
  area_browsers_yday <- rbind(area_browsers_yday, data.frame(Area=i, browsers=length(unique(MacVisitSV[MacVisitSV$date==today()-1 & MacVisitSV$duration>dropoff_duration_cutoff,'mac']))))
  area_browsers_w2d <- rbind(area_browsers_w2d, data.frame(Area=i, browsers=length(unique(MacVisitSV[MacVisitSV$date>today()-wday(today()) & MacVisitSV$date<=today()-1 & MacVisitSV$duration>dropoff_duration_cutoff,'mac']))))
  area_browsers_m2d <- rbind(area_browsers_m2d, data.frame(Area=i, browsers=length(unique(MacVisitSV[MacVisitSV$date>=today()-day(today())+1 & MacVisitSV$date<=today()-1 & MacVisitSV$duration>dropoff_duration_cutoff,'mac']))))
}

# billers dwell time by area
area_billersduration_yday <- data.frame()
area_billersduration_w2d <- data.frame()
area_billersduration_m2d <- data.frame()
for(i in store_area){
  area_billersduration_yday <- rbind(area_billersduration_yday, data.frame(Area=i, billers_dwell_time=sum(MacVisitSV[MacVisitSV$date==today()-1 & MacVisitSV$area_billing>=billing_duration_cutoff & !is.na(MacVisitSV[,i]),i])))
  area_billersduration_w2d <- rbind(area_billersduration_w2d, data.frame(Area=i, billers_dwell_time=sum(MacVisitSV[MacVisitSV$date>today()-wday(today()) & MacVisitSV$date<=today()-1 & MacVisitSV$area_billing>=billing_duration_cutoff & !is.na(MacVisitSV[,i]),i])))
  area_billersduration_m2d <- rbind(area_billersduration_m2d, data.frame(Area=i, billers_dwell_time=sum(MacVisitSV[MacVisitSV$date>=today()-day(today())+1 & MacVisitSV$date<=today()-1 & MacVisitSV$area_billing>=billing_duration_cutoff & !is.na(MacVisitSV[,i]),i])))
}

# trial room dwell time by area
# area_trialduration_yday <- data.frame()
# area_trialduration_w2d <- data.frame()
# area_trialduration_m2d <- data.frame()
# for(i in store_area){
#   area_trialduration_yday <- rbind(area_trialduration_yday, data.frame(Area=i, billers_dwell_time=sum(MacVisitSV[MacVisitSV$date==today()-1 & (MacVisitSV$area_trial1>=trial_duration_cutoff | MacVisitSV$area_trial2>=trial_duration_cutoff) & !is.na(MacVisitSV[,i]),i])))
#   area_trialduration_w2d <- rbind(area_trialduration_w2d, data.frame(Area=i, billers_dwell_time=sum(MacVisitSV[MacVisitSV$date>today()-wday(today()) & MacVisitSV$date<=today()-1 & (MacVisitSV$area_trial1>=trial_duration_cutoff | MacVisitSV$area_trial2>=trial_duration_cutoff) & !is.na(MacVisitSV[,i]),i])))
#   area_trialduration_m2d <- rbind(area_trialduration_m2d, data.frame(Area=i, billers_dwell_time=sum(MacVisitSV[MacVisitSV$date>=today()-day(today())+1 & MacVisitSV$date<=today()-1 & (MacVisitSV$area_trial1>=trial_duration_cutoff | MacVisitSV$area_trial2>=trial_duration_cutoff) & !is.na(MacVisitSV[,i]),i])))
# }


area_trialduration_yday <- data.frame()
area_trialduration_w2d <- data.frame()
area_trialduration_m2d <- data.frame()
for(i in store_area){
  area_trialduration_yday <- rbind(area_trialduration_yday, data.frame(Area=i, billers_dwell_time=sum(MacVisitSV[MacVisitSV$date==today()-1 & (MacVisitSV$area_trial1>=trial_duration_cutoff) & !is.na(MacVisitSV[,i]),i])))
  area_trialduration_w2d <- rbind(area_trialduration_w2d, data.frame(Area=i, billers_dwell_time=sum(MacVisitSV[MacVisitSV$date>today()-wday(today()) & MacVisitSV$date<=today()-1 & (MacVisitSV$area_trial1>=trial_duration_cutoff) & !is.na(MacVisitSV[,i]),i])))
  area_trialduration_m2d <- rbind(area_trialduration_m2d, data.frame(Area=i, billers_dwell_time=sum(MacVisitSV[MacVisitSV$date>=today()-day(today())+1 & MacVisitSV$date<=today()-1 & (MacVisitSV$area_trial1>=trial_duration_cutoff) & !is.na(MacVisitSV[,i]),i])))
}





# Browsing duration
browseduration_yday <- data.frame()
browseduration_w2d <- data.frame()
browseduration_m2d <- data.frame()

browseduration_yday <- rbind(browseduration_yday, data.frame(duration='< 5 mins', browsers=length(unique(MacVisitSV[MacVisitSV$date==today()-1 & MacVisitSV$duration<dropoff_duration_cutoff,'mac']))))
browseduration_yday <- rbind(browseduration_yday, data.frame(duration='< 5-10 mins', browsers=length(unique(MacVisitSV[MacVisitSV$date==today()-1 & MacVisitSV$duration>=300 & MacVisitSV$duration<600,'mac']))))
browseduration_yday <- rbind(browseduration_yday, data.frame(duration='< 10-20 mins', browsers=length(unique(MacVisitSV[MacVisitSV$date==today()-1 & MacVisitSV$duration>=600 & MacVisitSV$duration<1200,'mac']))))
browseduration_yday <- rbind(browseduration_yday, data.frame(duration='> 20 mins', browsers=length(unique(MacVisitSV[MacVisitSV$date==today()-1 & MacVisitSV$duration>=1200,'mac']))))

browseduration_w2d <- rbind(browseduration_w2d, data.frame(duration='< 5 mins', browsers=length(unique(MacVisitSV[MacVisitSV$date>today()-wday(today()) & MacVisitSV$date<=today()-1 & MacVisitSV$duration<dropoff_duration_cutoff,'mac']))))
browseduration_w2d <- rbind(browseduration_w2d, data.frame(duration='< 5-10 mins', browsers=length(unique(MacVisitSV[MacVisitSV$date>today()-wday(today()) & MacVisitSV$date<=today()-1 & MacVisitSV$duration>=300 & MacVisitSV$duration<600,'mac']))))
browseduration_w2d <- rbind(browseduration_w2d, data.frame(duration='< 10-20 mins', browsers=length(unique(MacVisitSV[MacVisitSV$date>today()-wday(today()) & MacVisitSV$date<=today()-1 & MacVisitSV$duration>=600 & MacVisitSV$duration<1200,'mac']))))
browseduration_w2d <- rbind(browseduration_w2d, data.frame(duration='> 20 mins', browsers=length(unique(MacVisitSV[MacVisitSV$date>today()-wday(today()) & MacVisitSV$date<=today()-1 & MacVisitSV$duration>=1200,'mac']))))

browseduration_m2d <- rbind(browseduration_m2d, data.frame(duration='< 5 mins', browsers=length(unique(MacVisitSV[MacVisitSV$date>=today()-day(today())+1 & MacVisitSV$date<=today()-1 & MacVisitSV$duration<dropoff_duration_cutoff,'mac']))))
browseduration_m2d <- rbind(browseduration_m2d, data.frame(duration='< 5-10 mins', browsers=length(unique(MacVisitSV[MacVisitSV$date>=today()-day(today())+1 & MacVisitSV$date<=today()-1 & MacVisitSV$duration>=300 & MacVisitSV$duration<600,'mac']))))
browseduration_m2d <- rbind(browseduration_m2d, data.frame(duration='< 10-20 mins', browsers=length(unique(MacVisitSV[MacVisitSV$date>=today()-day(today())+1 & MacVisitSV$date<=today()-1 & MacVisitSV$duration>=600 & MacVisitSV$duration<1200,'mac']))))
browseduration_m2d <- rbind(browseduration_m2d, data.frame(duration='> 20 mins', browsers=length(unique(MacVisitSV[MacVisitSV$date>=today()-day(today())+1 & MacVisitSV$date<=today()-1 & MacVisitSV$duration>=1200,'mac']))))

# drop offs by time of day
#yday
dropoff_yday <- tryCatch(aggregate(mac~entrytimeofday, data=MacVisitSV[MacVisitSV$date==today()-1 & MacVisitSV$duration<dropoff_duration_cutoff,c('mac','entrytimeofday')], FUN= function(x) length(unique(x))),error=function(e) {})
#w2d
dropoff_w2d <- tryCatch(aggregate(mac~entrytimeofday, data=MacVisitSV[MacVisitSV$date>today()-wday(today()) & MacVisitSV$date<=today()-1 & MacVisitSV$duration<dropoff_duration_cutoff,c('mac','entrytimeofday')], FUN= function(x) length(unique(x))),error=function(e) {})
#m2d
dropoff_m2d <- tryCatch(aggregate(mac~entrytimeofday, data=MacVisitSV[MacVisitSV$date>=today()-day(today())+1 & MacVisitSV$date<=today()-1 & MacVisitSV$duration<dropoff_duration_cutoff,c('mac','entrytimeofday')], FUN= function(x) length(unique(x))),error=function(e) {})

# tot visitors by time of day
#yday
visitors_yday <- tryCatch(aggregate(mac~entrytimeofday, data=MacVisitSV[MacVisitSV$date==today()-1,c('mac','entrytimeofday')], FUN= function(x) length(unique(x))),error=function(e) {})
#w2d
visitors_w2d <- tryCatch(aggregate(mac~entrytimeofday, data=MacVisitSV[MacVisitSV$date>today()-wday(today()) & MacVisitSV$date<=today()-1,c('mac','entrytimeofday')], FUN= function(x) length(unique(x))),error=function(e) {})
#m2d
visitors_m2d <- tryCatch(aggregate(mac~entrytimeofday, data=MacVisitSV[MacVisitSV$date>=today()-day(today())+1 & MacVisitSV$date<=today()-1,c('mac','entrytimeofday')], FUN= function(x) length(unique(x))),error=function(e) {})

# trial customers by time of day
# #yday
# trial_yday <- tryCatch(aggregate(mac~entrytimeofday, data=MacVisitSV[MacVisitSV$date==today()-1 & (MacVisitSV$area_trial1>=trial_duration_cutoff | MacVisitSV$area_trial2>=trial_duration_cutoff),c('mac','entrytimeofday')], FUN= function(x) length(unique(x))),error=function(e) {})
# #w2d
# trial_w2d <- tryCatch(aggregate(mac~entrytimeofday, data=MacVisitSV[MacVisitSV$date>today()-wday(today()) & MacVisitSV$date<=today()-1 & (MacVisitSV$area_trial1>=trial_duration_cutoff | MacVisitSV$area_trial2>=trial_duration_cutoff),c('mac','entrytimeofday')], FUN= function(x) length(unique(x))),error=function(e) {})
# #m2d
# trial_m2d <- tryCatch(aggregate(mac~entrytimeofday, data=MacVisitSV[MacVisitSV$date>=today()-day(today())+1 & MacVisitSV$date<=today()-1 & (MacVisitSV$area_trial1>=trial_duration_cutoff | MacVisitSV$area_trial2>=trial_duration_cutoff),c('mac','entrytimeofday')], FUN= function(x) length(unique(x))),error=function(e) {})


#yday
trial_yday <- tryCatch(aggregate(mac~entrytimeofday, data=MacVisitSV[MacVisitSV$date==today()-1 & (MacVisitSV$area_trial1>=trial_duration_cutoff),c('mac','entrytimeofday')], FUN= function(x) length(unique(x))),error=function(e) {})
#w2d
trial_w2d <- tryCatch(aggregate(mac~entrytimeofday, data=MacVisitSV[MacVisitSV$date>today()-wday(today()) & MacVisitSV$date<=today()-1 & (MacVisitSV$area_trial1>=trial_duration_cutoff),c('mac','entrytimeofday')], FUN= function(x) length(unique(x))),error=function(e) {})
#m2d
trial_m2d <- tryCatch(aggregate(mac~entrytimeofday, data=MacVisitSV[MacVisitSV$date>=today()-day(today())+1 & MacVisitSV$date<=today()-1 & (MacVisitSV$area_trial1>=trial_duration_cutoff),c('mac','entrytimeofday')], FUN= function(x) length(unique(x))),error=function(e) {})






# Final tables
if(!is.null(visitors_yday) & !is.null(dropoff_yday) & !is.null(trial_yday)){
  visitors_yday2 <- merge(visitors_yday, dropoff_yday, by='entrytimeofday', all.x = TRUE)
  colnames(visitors_yday2) <- c("entrytimeofday","total_visitors", "dropoffs")
  visitors_yday3 <- merge(visitors_yday2, trial_yday, by='entrytimeofday', all.x = TRUE)
  colnames(visitors_yday3)[4] <- 'trial_visitors'
}

if(!is.null(visitors_w2d) & !is.null(dropoff_w2d) & !is.null(trial_w2d)){
  visitors_w2d2 <- merge(visitors_w2d, dropoff_w2d, by='entrytimeofday', all.x = TRUE)
  colnames(visitors_w2d2) <- c("entrytimeofday","total_visitors", "dropoffs")
  visitors_w2d3 <- merge(visitors_w2d2, trial_w2d, by='entrytimeofday', all.x = TRUE)
  colnames(visitors_w2d3)[4] <- 'trial_visitors'
}

if(!is.null(visitors_m2d) & !is.null(dropoff_m2d) & !is.null(trial_m2d)){
  visitors_m2d2 <- merge(visitors_m2d, dropoff_m2d, by='entrytimeofday', all.x = TRUE)
  colnames(visitors_m2d2) <- c("entrytimeofday","total_visitors", "dropoffs")
  visitors_m2d3 <- merge(visitors_m2d2, trial_m2d, by='entrytimeofday', all.x = TRUE)
  colnames(visitors_m2d3)[4] <- 'trial_visitors'
}

################## Metrics for trend charts ##################################
# daily_trends
visitor_trend_daily <- tryCatch(aggregate(mac~date, data=MacVisitSV[MacVisitSV$date<=today()-1 & MacVisitSV$date>=today()-13,], FUN = function(x) length(unique(x))),error=function(e) {})
#trialist_trend_daily <- tryCatch(aggregate(mac~date, data=MacVisitSV[MacVisitSV$date<=today()-1 & MacVisitSV$date>=today()-13 & (MacVisitSV$area_trial1>=trial_duration_cutoff | MacVisitSV$area_trial2>=trial_duration_cutoff),], FUN = function(x) length(unique(x))),error=function(e) {})
trialist_trend_daily <- tryCatch(aggregate(mac~date, data=MacVisitSV[MacVisitSV$date<=today()-1 & MacVisitSV$date>=today()-13 & (MacVisitSV$area_trial1>=trial_duration_cutoff),], FUN = function(x) length(unique(x))),error=function(e) {})

dropoff_trend_daily <- tryCatch(aggregate(mac~date, data=MacVisitSV[MacVisitSV$date<=today()-1 & MacVisitSV$date>=today()-13 & MacVisitSV$duration<dropoff_duration_cutoff,], FUN = function(x) length(unique(x))),error=function(e) {})
billed_trend_daily <- tryCatch(aggregate(mac~date, data=MacVisitSV[MacVisitSV$date<=today()-1 & MacVisitSV$date>=today()-13 & MacVisitSV$area_billing>=billing_duration_cutoff,], FUN = function(x) length(unique(x))),error=function(e) {})
browser_trend_daily <- tryCatch(aggregate(mac~date, data=MacVisitSV[MacVisitSV$date<=today()-1 & MacVisitSV$date>=today()-13 & MacVisitSV$duration>dropoff_duration_cutoff,], FUN = function(x) length(unique(x))),error=function(e) {})
area_duration_trend_daily <- tryCatch(aggregate(cbind(area1,area2,area3,area_trial1,area_billing,area_out1)~date, data=MacVisitSV[MacVisitSV$date<=today()-1 & MacVisitSV$date>=today()-13,], FUN = sum),error=function(e) {})
area_duration_billed_trend_daily <- tryCatch(aggregate(cbind(area1,area2,area3,area_trial1,area_billing,area_out1)~date, data=MacVisitSV[MacVisitSV$date<=today()-1 & MacVisitSV$date>=today()-13 & MacVisitSV$area_billing>=billing_duration_cutoff,], FUN = sum),error=function(e) {})
area_duration_trial_trend_daily <- tryCatch(aggregate(cbind(area1,area2,area3,area_trial1,area_billing,area_out1)~date, data=MacVisitSV[MacVisitSV$date<=today()-1 & MacVisitSV$date>=today()-13 & (MacVisitSV$area_trial1>=trial_duration_cutoff | MacVisitSV$area_trial2>=trial_duration_cutoff),], FUN = sum),error=function(e) {})

# weekly_trends
visitor_trend_weekly <- tryCatch(aggregate(mac~paste0(year(date),'-',week(date)), data=MacVisitSV[MacVisitSV$date<=today()-wday(today()) & MacVisitSV$date>=today()-wday(today())-84,], FUN = function(x) length(unique(x))),error=function(e) {})
colnames(visitor_trend_weekly)[1] <- 'week'
# trialist_trend_weekly <- tryCatch(aggregate(mac~paste0(year(date),'-',week(date)), data=MacVisitSV[MacVisitSV$date<=today()-wday(today()) & MacVisitSV$date>=today()-wday(today())-84 & (MacVisitSV$area_trial1>=trial_duration_cutoff | MacVisitSV$area_trial2>=trial_duration_cutoff),], FUN = function(x) length(unique(x))),error=function(e) {})
trialist_trend_weekly <- tryCatch(aggregate(mac~paste0(year(date),'-',week(date)), data=MacVisitSV[MacVisitSV$date<=today()-wday(today()) & MacVisitSV$date>=today()-wday(today())-84 & (MacVisitSV$area_trial1>=trial_duration_cutoff),], FUN = function(x) length(unique(x))),error=function(e) {})
colnames(trialist_trend_weekly)[1] <- 'week'
dropoff_trend_weekly <- tryCatch(aggregate(mac~paste0(year(date),'-',week(date)), data=MacVisitSV[MacVisitSV$date<=today()-wday(today()) & MacVisitSV$date>=today()-wday(today())-84 & MacVisitSV$duration<dropoff_duration_cutoff,], FUN = function(x) length(unique(x))),error=function(e) {})
if(!is.null(dropoff_trend_weekly)){colnames(dropoff_trend_weekly)[1] <- 'week'}
billed_trend_weekly <- tryCatch(aggregate(mac~paste0(year(date),'-',week(date)), data=MacVisitSV[MacVisitSV$date<=today()-wday(today()) & MacVisitSV$date>=today()-wday(today())-84 & MacVisitSV$area_billing>=billing_duration_cutoff,], FUN = function(x) length(unique(x))),error=function(e) {})
colnames(billed_trend_weekly)[1] <- 'week'

browser_trend_weekly <- tryCatch(aggregate(mac~paste0(year(date),'-',week(date)), data=MacVisitSV[MacVisitSV$date<=today()-wday(today()) & MacVisitSV$date>=today()-wday(today())-84 & MacVisitSV$duration>dropoff_duration_cutoff,], FUN = function(x) length(unique(x))),error=function(e) {})
if(!is.null(browser_trend_weekly)){colnames(browser_trend_weekly)[1] <- 'week'}

area_duration_trend_weekly <- tryCatch(aggregate(cbind(area1,area2,area3,area4,area5,area_trial1,area_trial2,area_billing,area_out1,area_out2)~paste0(year(date),'-',week(date)), data=MacVisitSV[MacVisitSV$date<=today()-wday(today()) & MacVisitSV$date>=today()-wday(today())-84,], FUN = sum),error=function(e) {})
if(!is.null(area_duration_trend_weekly)){colnames(area_duration_trend_weekly)[1] <- 'week'}

area_duration_billed_trend_weekly <- tryCatch(aggregate(cbind(area1,area2,area3,area4,area5,area_trial1,area_trial2,area_billing,area_out1,area_out2)~paste0(year(date),'-',week(date)), data=MacVisitSV[MacVisitSV$date<=today()-wday(today()) & MacVisitSV$date>=today()-wday(today())-84 & MacVisitSV$area_billing>=billing_duration_cutoff,], FUN = sum),error=function(e) {})
if(!is.null(area_duration_billed_trend_weekly)){colnames(area_duration_billed_trend_weekly)[1] <- 'week'}

area_duration_trial_trend_weekly <- tryCatch(aggregate(cbind(area1,area2,area3,area4,area5,area_trial1,area_trial2,area_billing,area_out1,area_out2)~paste0(year(date),'-',week(date)), data=MacVisitSV[MacVisitSV$date<=today()-wday(today()) & MacVisitSV$date>=today()-wday(today())-84 & (MacVisitSV$area_trial1>=trial_duration_cutoff | MacVisitSV$area_trial2>=trial_duration_cutoff),], FUN = sum),error=function(e) {})
if(!is.null(area_duration_trial_trend_weekly)){colnames(area_duration_trial_trend_weekly)[1] <- 'week'}
