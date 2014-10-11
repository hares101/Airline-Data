load(url("http://eeyore.ucdavis.edu/stat141/Data/winterDelays.rda"))
load("winterDelays.rda")
names(data)
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
?plot
##numbers of flights
dim(data)

##airline with most flights##
names(data)
data$UNIQUE_CARRIER
air = sort(table(data$UNIQUE_CARRIER),decreasing=TRUE)
air
airlines = rownames(air)
airline = airlines[1:10]

##two way table question 3##

x= sort(table(data$ORIGIN), decreasing=TRUE)
y =rownames(x[1:20])
y

airline
YES  = data[data$ORIGIN %in% y & data$UNIQUE_CARRIER %in% airlines,]
tablesubset = droplevels(subset(winterDelays, ORIGIN %in% y & UNIQUE_CARRIER %in% airline))

yes = subset(winterDelays, ORIGIN == 'SFO',]
yes = winterDelays[winterDelays$ORIGIN %in% x,]
table(tablesubset$ORIGIN, tablesubset$UNIQUE_CARRIER)


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
  ?plot
  install.packages("lattice")
  library("lattice")
  densityplot(~data$ARR_DELAY|months, xlab = "Arrival Delay", main = "Arrival Delay By Month", col = "red")
             months = factor(data$MONTH)
                             
                             , levels = c(11,12,1,2), 
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
             
      #finding top 10 airports
    origflights =sort(table(data$ORIGIN))
destflights = sort(table(data$DEST))  
destflights
topflights = destflights + origflights
topflights 
top10 = sort(topflights, decreasing=TRUE)[1:10]
topflights1 = rownames(top10)

subsetflights = data[data$ORIGIN %in% topflights1 |
                      data$DEST %in% topflights1, ]
medianflights =with(subsetflights, aggregate(ARR_DELAY, list(ORIGIN, DEST), median, na.rm=TRUE))
colnames(medianflights) = c('Origin','Destination','Median'))
orderedmedian = medianflights[order(-medianflights$median)]


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
warnings()

sort(table(data$DEP_TIME_BLK)[], decreasing=TRUE, na.rm=TRUE)

  
sort(tapply(data$ARR_DELAY[data$ARR_DELAY>0], data$DEP_TIME_BLK[data$ARR_DELAY>0], mean,na.rm=TRUE), decreasing=TRUE)[1]

sort(tapply(data$ARR_DELAY, data$DEP_TIME_BLK, mean,na.rm=TRUE), decreasing=TRUE)[1]


#question 15
  
sort(table(data$DEP_TIME_BLK))
names(data)

sample(data$FL_DATE, 15)

  #get the arrival delays and dates in a vector
delay_dates = tapply(data$ARR_DELAY, data$FL_DATE, mean, na.rm=TRUE)
sorted_delay_dates = sort(delay_dates)
  #find mean delays around xmas and thanksgiving time
sorted_delay_dates[c('2012-12-24','2012-12-25','2012-12-26')]
sorted_delay_dates[c('2012-11-22', '2012-11-21','2012-11-23')]
mean(data$ARR_DELAY, na.rm=TRUE)

#question 16

  #finding na's in each variable
countna1 = length(data$ARR_DELAY)[is.na(data$ARR_DELAY)]
countna = length(data)[is.na(data)]
length(countna)
  #this code from piazza
newNA = sapply(data, function(x) length(x[is.na(x)]))
newNA
sum(newNA)

#question 17
  
  #compare first two variables to see if they hold the same NA
firstNA = is.na(data$DEP_TIME) == is.na(data$DEP_DELAY)
unique(firstNA)
  #compare the second two variables to see if they also hold same NA
secondNA = is.na(data$DEP_DELAY) == is.na(data$DEP_DELAY_NEW)
unique(secondNA)
  #since both A and B hold same Na's, as do B and C, then we can assume A and C also do

#question 18
  
delayplot = sort(tapply(data$ARR_DELAY, data$ARR_TIME_BLK, mean, na.rm=TRUE), decreasing=TRUE)
delayplot
barplot(delayplot)

barchart(data$ARR_DELAY, data$ARR_TIME_BLK)
library("lattice")

#question 19

late_takeoffs = data[data$DEP_DELAY>0 ,]
dim(late_takeoffs)
  #748292/1961489 flights took off late, or 38.1 percent
early_takeoffs = data[data$DEP_DELAY<0, ]
dim(early_takeoffs)
  #1129185/1961489 took off late, or 57.56 percent
sum(is.na(data$DEP_DELAY))
 
#question 20
names(data)
  #find flights that had a delay
late_flights = data[data$ARR_DELAY>0, ]
late_flights
length(late_flights)
dim(late_flights)
dim(data)
739696/1961489
  #we find that 739696/1961489 flights took off late, or 37.7 percent

early_flights = data[data$ARR_DELAY<0, ]
dim(early_flights)
1212927/1961489
  #we find that 1212927/1961489 arrived early, or.61.8 percent
