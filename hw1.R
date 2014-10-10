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

x_nov = list(arr_delay_nov, carrier_delay_nov, dep_delay_nov, weather_delay_nov, nas_delay_nov,sec_delay_nov,late_delay_nov)
sapply(x_nov, mean)

x_dec = list(arr_delay_dec, carrier_delay_dec, dep_delay_dec, weather_delay_dec, 
             nas_delay_dec,sec_delay_dec,late_delay_dec)

sapply(x_dec, mean)

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

##question 6
mean(data$ARR_DELAY[data$ORIGIN == 'SFO' & data$UNIQUE_CARRIER == 'UA'
                    & data$DAY_OF_WEEK == '6'|data$DAY_OF_WEEK=='7'], na.rm=T)
sd_delay = sd(data$ARR_DELAY[data$ORIGIN == "SFO" & data$UNIQUE_CARRIER == "UA" 
                    & data$DAY_OF_WEEK == '6'|data$DAY_OF_WEEK=='7'], na.rm=T)

sd_delay
unique(data$DAY_OF_WEEK)

mean_delay = mean(data$ARR_DELAY[data$ORIGIN == 'SFO' & data$UNIQUE_CARRIER == 'UA'
                    & data$DAY_OF_WEEK %in% c(6,7)], na.rm=T)
sd_delay = sd(data$ARR_DELAY[data$ORIGIN == "SFO" & data$UNIQUE_CARRIER == "UA" 
                  & data$DAY_OF_WEEK %in% c(6,7)], na.rm=T)

#question 7 plot distribution#

names(data)
axis.default(side=1, labels = (-1:3)
histogram(~data$ARR_DELAY|months, xlab = "Arrival Delay", main = "Arrival Delay By Month", col = "red", 
          type = c("percent"), ylim=c(1, 100))
months = factor(data$MONTH, levels = c(11,12,1,2), 
                labels = c("November", "December", "January", "February"))
unique(data$MONTH)
pls
pls = split(ARR_DELAY, MONTH)
plot(data$ARR_DELAY, data$MONTH)
library("lattice")

#question 8 single dot plot#

flights = table(data$ORIGIN)
flights
sorted_flights =sort(flights)
length(sorted_flights)
newflights = sorted_flights[277:307]
length(newflights)
dotchart(newflights, main = "Number of Flights Per Airport", xlab = 'Number of Flights',
         ylab = 'Airport', col = "blue", cex = .7)

#question 9 more weekend flights?

names(data)
unique(DAY_OF_WEEK)
data(data$FL_NUM, )
weekdays = table(DAY_OF_WEEK %in% c(1:5), data$FL_NUM)
length(weekdays)

weekdays = (data$FLIGHTS[data$DAY_OF_WEEK %in% c(1:5)])
length(weekdays)
weekends = (data$FLIGHTS[data$DAY_OF_WEEK %in% c(6,7)])
length(weekends)
unique(data$FLIGHTS)

#question 10 most number of delayed flights

data(data$ARR_DELAY > 0 & data$DAY_OF_WEEK == 1)

length(which(data$ARR_DELAY>0 & data$DAY_OF_WEEK >= 1))

  #ANSWER
week_delay = table(data$ARR_DELAY>0, data$DAY_OF_WEEK)
week_delay


sum(is.na(week_delay))
sum(is.na)
sum(is.na(data$ARR_DELAY))
sum(is.na(data$DAY_OF_WEEK))

sum(table(data$DAY_OF_WEEK[data$ARR_DELAY >0]))+
  sum(table(data$DAY_OF_WEEK[data$ARR_DELAY <=0]))+ 
  sum(is.na(data$ARR_DELAY))

#question 11 largest median overall delay

with(data,tapply(data$ARR_DELAY[ARR_DELAY>0],data$DAY_OF_WEEK[ARR_DELAY>0], quantile, probs = .9, na.rm=TRUE))

tapply(data$ARR_DELAY, data$DAY_OF_WEEK, quantile, probs = .9, na.rm= TRUE)
tapply(data$ARR_DELAY, data$DAY_OF_WEEK, median, na.rm= TRUE)

#question 12 

  #finding top 10 origin airports
most_flights1 = sort(table(data$ORIGIN))
most_flights = most_flights1[c(298:307)]
most_flights
names(data)


  #finding top 10 destination airports
dest_flights1 = sort(table(data$DEST))
dest_flights= dest_flights1[c(298:307)]
dest_flights1
dest_flights
most_flights

  #solving the problem
tapply(data$ARR_DELAY[data$ORIGIN %in% c(most_flights, dest_flights)], c(most_flights, dest_flights), median)
table(data$ARR_DELAY[data$ORIGIN %in% c(most_flights, dest_flights)])

tapply(data$ARR_DELAY[data$ARR_DELAY>0], data$DAY_OF_WEEK[data$ARR_DELAY>0], quantile, probs = .9, na.rm= TRUE)


tapply(data$ARR_DELAY)[data$ORIGIN == most_flight $ data$DEST == dest_flights], list(most_flights, dest_flights), median)
tapply(data$ARR_DELAY)[data$],  median)
tapply(data$ARR_DELAY[origin & destination], cbind(origin, destination), median)

origin = winterDelays$ORIGIN %in% most_flights
destination = winterDelays$ORIGIN %in% dest_flights

a= list(most_flights, dest_flights)
tapply(data$ARR_DELAY[], a, median)

aggregate(data$ARR_DELAY[data$ORIGIN %in% ], a, median)

b = data[data$ARR_DELAY, data$DEST_DELAY, drop = FALSE]

#top 10 airports in and outbound

names(data)

head(data$FLIGHTS)
unique(data$FLIGHTS)
order(table(data$ORIGIN, data$DEST))

with(TOP10AIRPORTS, aggregate(ARR_DELAY, list(ORIGIN, DEST), median, na.rm = TRUE))
(tapply(data$ARR_DELAY, list(data$ORIGIN, data$DEST), median, na.rm=TRUE))

merged_tables = most_flights1+ dest_flights1
most_flights1
dest_flights1
merged_tables

new_merged = merged_tables[c(298:307)]
new_merged
tapply(data$ARR_DELAY, list(merged_tables), median)

#question 13

head(data$DISTANCE)
tail(data$DISTANCE)
max(data$DISTANCE)
min(data$DISTANCE)
library("lattice")
plot = plot(data$DISTANCE, data$ARR_DELAY)
?cut
cut1 = cut(data$ARR_DELAY, breaks = seq(-, 1500, by=100))
cut1
table(cut1)

newplot = plot(data$DISTANCE, cut1)
newplot

  #ANSWER
scatterplot <- smoothScatter(data$DISTANCE, data$ARR_DELAY, bandwidth = c(1:3), nrpoint = 1000, pch= 20, cex = .7,
              xlab = 'Distance Traveled (In Miles)' , ylab='Delay (In Minutes)', main = 'Delay of Flights by Distance')

#question 14

tail(data$FIRST_DEP_TIME)
names(data)
head(data$ARR_TIME)
length(data$ARR_TIME)
split(data$ARR_DELAY, data$ARR_TIME)
table(data$ARR_DELAY, data$ARR_TIME)

smoothScatter(data$ARR_TIME, data$DEP_DELAY, nrpoints= 1000, pch=20, cex=.7,
              xlab='Departure Time (Military Time)', ylab='Arrival Delay (In Minutes)', main= 'Delay of Flights by Time')

head(data$ARR_TIME_BLK)
