
#start by installing this the NYC Flights package
#this contains your data that you will need
#for more information use ? or see https://github.com/hadley/nycflights13 
install.packages("nycflights13") # this only needs to be done once
library(nycflights13) # load the package --- everytime
library(tidyverse)

mtcars = as.tibble(mtcars)
head(cars)
head(mtcars)

data("flights")
head(flights)
colnames(flights)

#lets get rid of some columns we dont want using dplyr's select function
#only keep columns that have time in the name
flight_names = select(flights, contains("time")) #try: insert any character string of interest
colnames(flight_names)
#how about columns that end in something...like "ay"
flight_ay = select(flights, ends_with("ay"))
#or how about those that start with a specific string?
flight_de = select(flights, starts_with("de"))
#or specific columns by name only
flight_set = select(flights, one_of(c("year", "month", "day", "arr_time")))
#or we could get rid of just one column if we want with -
flight_minus = select(flights, -dep_delay)
#combine minus with other selctions
flight_de_minus = select(flights, -starts_with("de"))

#you can perform similar functions on rows
#lets only look at flights from united airlines (UA)
ua_flights = filter(flights, carrier == "UA")
#what if we want more than a single string for filtering? use group memebership
group = c("UA", "AA" , "DL")
ua_group_flights = filter(flights, carrier  %in% group)
#to remove we use the != logical operator on rows
ua_minus_flights = filter(flights, carrier != "UA")
#lets use some numerical filters now - lets just take febuary flights
feb_flight = filter(flights, month == 2)
#you can use other filters too - look at the documentation and play around
half_flight = filter (flights, month <= 6)


#code is for computers to read - piping makes it better for humans
#this is a pipe: %>%
#it reduces the need for nested functions that can be difficult to read
flight_set = flights %>% 
  select(-dep_delay) %>%
  filter(month == 2)
#now we just have everything except dep_delay in the month of febuary (2) as a new variable named flight_set

#here is where the power comes in -- lets get some actual info about the data with summarize
head(flight_set)

flight_set$carrier = as.factor(flight_set$carrier)

mean_dist = flight_set %>%
  group_by(carrier) %>%
  summarise(distance_avg = mean(distance), sd = sd(distance)) 


mean_dist$carrier = as.factor(mean_dist$carrier)
ggplot(mean_dist, aes(x= mean_dist$carrier, y =  mean_dist$distance_avg)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin=distance_avg-sd, ymax=distance_avg+sd),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) +
  ylab("Mean Distance Traveled") +
  xlab("Carrier") +
  labs(title = "Barplot of Data using ggplot2") +
  theme_bw()

