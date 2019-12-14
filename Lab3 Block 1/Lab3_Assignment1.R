RNGversion('3.5.1')
## Assignment 1:
## Implement a kernel method to predict the hourly temperatures for a date and place in Sweden.
## To do so, you are provided with the files stations.csv and temps50k.csv. These
## files contain information about weather stations and temperature measurements in the stations
## at different days and times. The data have been kindly provided by the Swedish Meteorological
## and Hydrological Institute (SMHI).
## You are asked to provide a temperature forecast for a date and place in Sweden. The
## forecast should consist of the predicted temperatures from 4 am to 24 pm in an interval of 2
## hours. Use a kernel that is the sum of three Gaussian kernels:
##  The first to account for the distance from a station to the point of interest.
##  The second to account for the distance between the day a temperature measurement
##    was made and the day of interest.
##  The third to account for the distance between the hour of the day a temperature measurement
##    was made and the hour of interest.
## Choose an appropriate smoothing coefficient or width for each of the three kernels above.
## Answer to the following questions:
##  Show that your choice for the kernels' width is sensible, i.e. that it gives more weight
##    to closer points. Discuss why your of definition of closeness is reasonable.
##  Instead of combining the three kernels into one by summing them up, multiply them.
##    Compare the results obtained in both cases and elaborate on why they may differ.
## Note that the file temps50k.csv may contain temperature measurements that are posterior
## to the day and hour of your forecast. You must filter such measurements out, i.e. they cannot
## be used to compute the forecast. Feel free to use the template below to solve the assignment.

set.seed(1234567890)
#install.packages("geosphere")
library(geosphere)
stations <- read.csv("stations.csv")
temps <- read.csv("temps50k.csv")
#A join operation on "station_number"
st <- merge(stations,temps,by="station_number")
n = dim(st)[1]
#Extract relevant vectors
lat = st$latitude
long = st$longitude
points = data.frame(lat,long)
dates = as.Date(st$date)
times = strptime(st$time, format="%H:%M:%S")
temperature = st$air_temperature
#Kernel weighting factors
h_distance <- 1
h_date <- 2
h_time <- 3
#Latitude of interest
a <- 58.4274
#Longitude of interest
b <- 14.826
#Create a vector of the point of interest
placeOI = c(a, b)
dateOI <- as.Date("2013-11-04") # The date to predict (up to the students)
timesOI = c("04:00:00", "06:00:00", "08:00:00", "10:00:00", "12:00:00", "14:00:00", "16:00:00", "18:00:00", "20:00:00",
          "22:00:00", "24:00:00")

#A gaussian function for the difference in distance
gaussian_dist = function(place, data, h) {
  u = distHaversine(place, data)/h
  return (exp(-u^2))
}

#A gaussian function for difference in days
gaussian_day = function(date, compare_date, h){
  if (abs(as.numeric(date-compare_date))>365){
    diff = as.numeric(date-compare_date) %% 365
    if(diff>182){
      diff=diff-182
    }
  } else {
    diff = as.numeric(date-compare_date)
  }
  u = diff/h
  return (exp(-u^2))
}

#A gaussian function for difference in hours
gaussian_hour = function(hour, compare_hour, h){
  hour = strptime(hour, format="%H:%M:%S")
  hour = as.numeric(format(hour, format="%H"))
  compare_hour = as.numeric(format(compare_hour, format="%H"))
  if(abs(hour-compare_hour)>12){
    diff = hour-compare_hour-12
  } else {
    diff = hour-compare_hour
  }
  u=diff/h
  return(exp(-u^2))
}

#Defining values that will be used in loop below
kernel_dist = c()
kernel_day = c()
kernel_time = c()
kernel_sum = c()
kernel_mult = c()
sum_kernel = 0
mult_kernel = 0
nominator_sum = 0
denominator_sum = 0
nominator_mult = 0
denominator_mult = 0
finished = FALSE
index = 1

#Looping through time array and data points in nested loop to calculate the 11 kernel values
for (time in timesOI) {
  for (i in 1:n) {
    if (!finished) {
      kernel_dist = c(kernel_dist, gaussian_dist(placeOI, points[i,], h_distance))
      kernel_day = c(kernel_day, gaussian_day(dateOI, dates[i], h_date))
    }
    kernel_time = c(kernel_time, gaussian_hour(time, times[i], h_time))
    sum_kernel = sum_kernel+kernel_dist[i]+kernel_day[i]+kernel_time[i]
    nominator_sum = nominator_sum+sum_kernel*temperature[i]
    denominator_sum = denominator_sum+sum_kernel
    mult_kernel = mult_kernel+kernel_dist[i]*kernel_day[i]*kernel_time[i]
    nominator_mult = nominator_mult+mult_kernel*temperature[i]
    denominator_mult = denominator_mult+mult_kernel
  }
  finished = TRUE;
  kernel_sum = c(kernel_sum, nominator_sum/denominator_sum)
  kernel_mult = c(kernel_mult, nominator_mult/denominator_mult)
  kernel_time = c()
  sum_kernel = 0
  mult_kernel = 0
  nominator_sum = 0
  denominator_sum = 0
  nominator_mult= 0
  denominator_mult = 0
  index = index + 1
}


plot(kernel_sum, type="o", main = "Temperature estimate through sum of Kernels")