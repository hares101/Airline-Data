load("winterDelays.rda")
names(data)
data=winterDelays
##numbers of flights
dim(data)
library("rmarkdown")
##airline with most flights##
data$UNIQUE_CARRIER
airline = sort(table(data$UNIQUE_CARRIER),decreasing=TRUE)
airline
airlines = rownames(airline)
most_airline = airlines[1]
most_airline
##two way table question 3##

most_flights = sort(table(data$ORIGIN), decreasing=TRUE)
most_flights_names =rownames(most_flights[1:20])
most_flights_names

airline
tablesubset = droplevels(subset(data, ORIGIN %in% most_flights_names
                                & UNIQUE_CARRIER %in% airlines))
table(tablesubset$ORIGIN, tablesubset$UNIQUE_CARRIER)


##mean delay question 4

names(data)
data$MONTH

#arr_delay
arr_delay_nov = mean(data[data$MONTH == 11,'ARR_DELAY'], na.rm=TRUE)
arr_delay_dec = mean(data[data$MONTH == 12,'ARR_DELAY'], na.rm=TRUE)

arr_delay_dec
arr_delay_nov


#question 5#
boxplot(data$ARR_DELAY, ylim = c(-20,50), xlab = 'Arrival Delay',
        ylab='Delay in Minutes', main='Arrival Delay of All Flights'
        ,col='light green')
  #skewed because outliers, use median

##question 6


mean_delay =mean(data$ARR_DELAY[data$ORIGIN == 'SFO' & data$UNIQUE_CARRIER == 'UA'
                                 & data$DAY_OF_WEEK %in% c(6,7)], na.rm=T)
sd_delay = sd(data$ARR_DELAY[data$ORIGIN == "SFO" & data$UNIQUE_CARRIER == "UA" 
                             & data$DAY_OF_WEEK %in% c(6,7)], na.rm=T)
mean_delay
sd_delay

#question 7 plot distribution#

  library("lattice")
  
January = data$ARR_DELAY[data$MONTH == 1]
February =data$ARR_DELAY[data$MONTH == 2]
November = data$ARR_DELAY[data$MONTH == 11]
December = data$ARR_DELAY[data$MONTH == 12]


boxplot(January, February, November, December,ylim=c(-20,30), horizontal = TRUE, names = c('January','February','November','December'), cex.lab=.7,cex.axis=.5,
        col=c("light blue","darkgreen","yellow", "red"), xlab='Arrival Delay', ylab= 'Flight Months', 
        main='Arrival Delay by Months')

#question 8 single dot plot#
             
flights = table(data$ORIGIN)
flights
sorted_flights =sort(flights)
length(sorted_flights)
newflights = sorted_flights[277:307]
length(newflights)
dotchart(newflights, main = "Number of Flights Per Airport", xlab = 'Number of Flights',
ylab = 'Airport', col = "blue", cex = .5)
             
#question 9 more weekend flights?
             
            
tapply(data$FL_NUM,data$DAY_OF_WEEK,length)

             
#question 10 most number of delayed flights
             
             
#ANSWER
week_delay = table(data$ARR_DELAY>0, data$DAY_OF_WEEK)
week_delay
             
             
#question 11 largest median overall delay
             
            
             
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
topflights1

subsetflights = data[data$ORIGIN %in% topflights1 |
                      data$DEST %in% topflights1, ]

medianflights = aggregate(subsetflights$ARR_DELAY, list(subsetflights
                                  $ORIGIN, subsetflights$DEST), median, na.rm=TRUE)
colnames(medianflights) = c('Origin','Destination','Median')
orderedmedian = medianflights[order(-medianflights$Median),]
head(orderedmedian)

#question 13

#ANSWER
scatterplot <- smoothScatter(data$DISTANCE, data$ARR_DELAY, bandwidth = c(1:3), nrpoint = 500, pch= 20, cex = .7,
                             xlab = 'Distance Traveled (In Miles)' , ylab='Delay (In Minutes)', main = 'Delay of Flights by Distance')

#question 14


smoothScatter(data$ARR_TIME, data$DEP_DELAY, nrpoints= 1000, pch=20, cex=.7,
              xlab='Departure Time (Military Time)', ylab='Arrival Delay (In Minutes)', main= 'Delay of Flights by Time')


  
sort(tapply(data$ARR_DELAY[data$ARR_DELAY>0], data$DEP_TIME_BLK[data$ARR_DELAY>0], mean,na.rm=TRUE), decreasing=TRUE)[1]

sort(tapply(data$ARR_DELAY, data$DEP_TIME_BLK, mean,na.rm=TRUE), decreasing=TRUE)[1]


#question 15
  

  #get the arrival delays and dates in a vector
delay_dates = tapply(data$ARR_DELAY, data$FL_DATE, mean, na.rm=TRUE)
sorted_delay_dates = sort(delay_dates)
  #find mean delays around xmas and thanksgiving time
sorted_delay_dates[c('2012-12-24','2012-12-25','2012-12-26')]
sorted_delay_dates[c('2012-11-22', '2012-11-21','2012-11-23')]
mean(data$ARR_DELAY, na.rm=TRUE)

#question 16

  #finding na's in each variable

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
barplot(delayplot, ylab = 'Mean Delay Time in Minutes',xlab='Time Block'
        ,main='Mean Arrival Delay Per Time Block',col='purple')

barchart(data$ARR_DELAY, data$ARR_TIME_BLK)
library("lattice")

#question 19

late_takeoffs = data[data$DEP_DELAY>0 ,]
dim(late_takeoffs)
  #748292/1961489 flights took off late, or 38.1 percent


#question 20
names(data)
  
  #find flights that had a delay

lateflights = data[data$ARR_DELAY>0,]
dim(lateflights)
739696/1961489
  #we find that 739696/1961489 flights arrived late, or 37.7 percent

early_flights = data[data$ARR_DELAY<0, ]
dim(early_flights)
1212927/1961489

  #we find that 1212927/1961489 arrived early, or.61.8 percent



#question 21

double_delay = data[data$DEP_DELAY>0 & data$ARR_DELAY>0, ]
dim(double_delay)
proportion_double_delay = 532192/748292
proportion_double_delay
  #532182 of 748292 flights that departed late also arrived late, or 71.1 percent


#question 22

names(data)
makeup_time = data[data$DEP_DELAY>0, ]

  #finding the mean/median delay of flights that took off late
mean_dep_delay = mean(data$DEP_DELAY[data$DEP_DELAY>0], na.rm=TRUE)
mean_dep_delay
median_dep_delay = median(data$DEP_DELAY[data$DEP_DELAY>0], na.rm=TRUE)
median_dep_delay

  #finding mean/median arrival delay of flights that took off late
mean_arr_delay = mean(data$ARR_DELAY[data$DEP_DELAY>0], na.rm=TRUE)
mean_arr_delay

median_arr_delay = median(data$ARR_DELAY[data$DEP_DELAY>0], na.rm=TRUE)
median_arr_delay

  #mean arrival delay is 5 minutes less than departure delay for flights that took off late, and 
  #median arrival delay is 3 minutes less han departure delay,
  #suggesting that flights that depart late on average do make up time

#question 23


  #finding average speed of flights that were on time or early
avg_speed = mean(data$DISTANCE[data$DEP_DELAY <=0]/
                   (data$AIR_TIME[data$DEP_DELAY<=0]/60), na.rm=TRUE)
avg_speed
  #finding average speed of flights that were late
avg_speed_delay = mean(data$DISTANCE[data$DEP_DELAY>0]/
                         (data$AIR_TIME[data$DEP_DELAY>0]/60), na.rm=TRUE)
avg_speed_delay

  #doing the same but for median speed
medianspeed = median(data$DISTANCE[data$DEP_DELAY <=0]/
       (data$AIR_TIME[data$DEP_DELAY<=0]/60), na.rm=TRUE)
medianspeed

medianspeed_delay = median(data$DISTANCE[data$DEP_DELAY>0]/
       (data$AIR_TIME[data$DEP_DELAY>0]/60), na.rm=TRUE)

medianspeed_delay


#question 24

sf_flights = data[data$ORIGIN == 'SFO', ]
top5_destinations = rownames(sort(table(sf_flights$DEST), decreasing = TRUE)[1:5])
top5_destinations

#question 25

top5_distance = tapply(data$DISTANCE[data$ORIGIN == 'SFO' & data$DEST %in% top5_destinations],droplevels
                     (data$DEST[data$ORIGIN == 'SFO' &data$DEST %in% top5_destinations]), unique, na.rm=TRUE)
top5_distance

#question 26

distance = data$DISTANCE[data$ORIGIN == 'SFO'& data$DEST %in% top5]
unique(distance)


#question 26

data$speed <- (data$DISTANCE/(data$AIR_TIME/60))
meanspeed1 = tapply(data$speed[data$ORIGIN=='SFO' & data$DEST %in% top5_destinations],
                    droplevels(data$DEST[data$ORIGIN == 'SFO' & data$DEST %in% top5_destinations]),
              mean, na.rm=TRUE)
meanspeed1
barplot(meanspeed1, main = 'Average Speed of Flights From SFO', xlab =
          'Top 5 Destination Flights From SFO', ylab = 'Average Speed in Miles'
        ,col= 'light blue')

#question 27

LAX_route = data$ARR_DELAY[data$ORIGIN %in% c('SFO','LAX') & data$DEST %in% c('LAX','SFO')]
JFK_route = data$ARR_DELAY[data$ORIGIN %in% c('SFO','JFK') & data$DEST %in% c('JFK','SFO')]
EWR_route = data$ARR_DELAY[data$ORIGIN %in% c('SFO','EWR') & data$DEST %in% c('EWR','SFO')]
boxplot(LAX_route, JFK_route, EWR_route ,ylim=c(-20,50), horizontal = TRUE, names = c('LAX','JFK','EWR'), cex.lab=.7,cex.axis=.7,
        col=c("light blue","darkgreen","yellow"), xlab='Arrival Delay', ylab= 'Airport Routes To/From SFO', 
        main='Arrival Delay of Routes Between SFO')

**28. What other questions could we address with this data?**
  There is a myriad of questions we could address. Here are a few:
  
  Which route has the worst delay?
Which airport experiences the worst delay?
Which airport has the worst delay from the time block of 1700-1800?
What are the most popular routes for flights that span at least 2000 miles?

**29. What other data/variables would allow us to address additional interesting questions?**
  Variables including: The amount of visitors an airport had during the season, the size of the airports, the amount of hangars an airport has.




