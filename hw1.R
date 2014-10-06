load(url("http://eeyore.ucdavis.edu/stat141/Data/winterDelays.rda"))
names(winterDelays)
x=winterDelays[1:10000, ]
data=winterDelays
attach(data)
data$dep_time
data$DEP_TIME
mean(data$DEP_TIME)
length(data$FIRST_DEP_TIME)
names(data)
unique(data$YEAR)
count(data$DAY_OF_WEEK)
unique(data$DAY_OF_WEEK)
data$DAY_OF_WEEK = c(1:7)
data$DAY_OF_WEEK 
sum(data$FL_NUM)

##numbers of flights
dim(data)

##airline with most flights##
names(data)
data$UNIQUE_CARRIER
table(data$UNIQUE_CARRIER)

##two way table question 3##
names(data)
order(data$CARRIER, decreasing = T)
table1 = table(data$ORIGIN, data$CARRIER)
table1
x= table(data$ORIGIN)
x
sort(x)
y= table(data$CARRIER)
sort(y)
table

table2 = table[c('ATL', 'ORD', 'DFW','DEN','LAX','IAH','PHX','SFO','CLT','LAS'
        ,'DTW','EWR','MSP','MCO','SLC','JFK','BOS','BWI','LGA','SEA'),
      c('WN','EV','DL','OO','AA','UA','MQ','US','B6','FL')]
table2 =table1[c('ATL', 'ORD', 'DFW','DEN','LAX','IAH','PHX','SFO','CLT','LAS'
         ,'DTW','EWR','MSP','MCO','SLC','JFK','BOS','BWI','LGA','SEA'),
       c('WN','EV','DL','OO','AA','UA','MQ','US','B6','FL')]
table2

##mean delay question 4

names(data)
data$MONTH
  #dep_delay
dep_delay_nov = mean(data[data$MONTH == 11,'DEP_DELAY'], na.rm=TRUE)
dep_delay_dec = mean(data[data$MONTH == 12, 'DEP_DELAY'], na.rm=TRUE)
  #arr_delay
arr_delay_nov = mean(data[data$MONTH == 11,'ARR_DELAY'], na.rm=TRUE)
arr_delay_dec = mean(data[data$MONTH == 12,'ARR_DELAY'], na.rm=TRUE)
  #carrier_delay
carrier_delay_nov = mean(data[data$MONTH == 11,'CARRIER_DELAY'], na.rm=TRUE)
carrier_delay_dec = mean(data[data$MONTH == 12,'CARRIER_DELAY'], na.rm=TRUE)
  #weather_delay
weather_delay_nov = mean(data[data$MONTH == 11,'WEATHER_DELAY'], na.rm=TRUE)
weather_delay_dec = mean(data[data$MONTH == 12,'WEATHER_DELAY'], na.rm=TRUE)
  #nas_delay
nas_delay_nov = mean(data[data$MONTH == 11,'NAS_DELAY'], na.rm=TRUE)
nas_delay_dec = mean(data[data$MONTH == 12,'NAS_DELAY'], na.rm=TRUE)
  #security_delay
sec_delay_nov = mean(data[data$MONTH == 11,'SECURITY_DELAY'], na.rm=TRUE)
sec_delay_dec = mean(data[data$MONTH == 12,'SECURITY_DELAY'], na.rm=TRUE)
  #late aircraft delay
late_delay_nov = mean(data[data$MONTH == 11,'LATE_AIRCRAFT_DELAY'], na.rm=TRUE)
late_delay_dec = mean(data[data$MONTH == 12,'LATE_AIRCRAFT_DELAY'], na.rm=TRUE)

arr_delay_dec
arr_delay_nov

mean_delay_nov =mean(dep_delay_nov, arr_delay_nov,
     carrier_delay_nov, weather_delay_nov,
     nas_delay_nov, sec_delay_nov, late_delay_nov)
mean_delay_dec =mean(dep_delay_dec, arr_delay_dec,
     carrier_delay_dec, weather_delay_dec,
     nas_delay_dec, sec_delay_dec, late_delay_dec)
mean_delay_nov
mean_delay_dec

#question 5#
hist(data[data$MONTH == 11, 'LATE_AIRCRAFT_DELAY'], na.rm=TRUE)
