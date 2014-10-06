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
##mean delay
