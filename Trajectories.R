##### Trajectory and Home Range
## Nathan Hahn

library(moveVis)
library(lubridate)
library(sf)
library(tidyverse)
library(adehabitatLT)


##### 1. Prep Data

# load raw data and check the contents - what are the data fields?
data(whitestork_data)
stork <- df[!df$name %in% c('Muffine', 'Redrunner'),] # remove two individuals

#head(stork) # check col names 
#str(stork) # what is the structure?

# create a time object for the tracking times using lubridate package
stork$timestamp <- lubridate::as_datetime(stork$timestamp) 

# If we want to look at distance components in the trajectory, we need to convert to UTM
library(sf)
t <- st_as_sf(stork, coords = c('location-long', 'location-lat'), crs = "+proj=longlat +datum=WGS84")
t <- st_transform(t, crs = "+proj=utm +zone=32n")

library(magrittr)
stork <- t %>%
  mutate(x = unlist(map(t$geometry,1)),
         y = unlist(map(t$geometry,2)))
stork$geometry <- NULL
#stork


#write.csv(stork, 'stork.csv')


##### 2. Create a trajectory - adeHabitatLT

# What do the fixtimes look like?
library(ggplot2)
library(lubridate)
ggplot(stork, aes(x = timestamp, y = lubridate::minute(timestamp))) + geom_point() +
  facet_wrap(.~name)

# We just want to look at long-distance movement, so lets downsample to 30 minute fixes. This can also be useful in cases where you have lots of extraneous fixes... 
stork.ds <- stork %>%
  filter(minute(timestamp) > 55 | minute(timestamp) < 5 | between(minute(timestamp), 27, 33)) %>% droplevels()

# check
ggplot(stork.ds, aes(x = timestamp, y = lubridate::minute(timestamp))) + geom_point() +
  facet_wrap(.~name)

## Make the trajectory - supply the coordinates, date, and id
library(adehabitatLT)
stork.traj <- as.ltraj(xy = stork.ds[,c('x', 'y')], date = stork.ds[,'timestamp'], id = stork.ds[,'name'],
                       typeII = TRUE) # indicates time-based trajectory

# explore the trajectory object -
stork.traj

## How do we identify missing data and deal with large periods of missing data? - bursts

# set burst function -- no relocs within x hours creates a separate burst, dt must be specified in seconds
foo <- function(dt) { 
  return(dt > (24*3600)) # hours * 3600
}

stork.burst <- cutltraj(stork.traj, 'foo(dt)', nextr = TRUE)
#stork.burst

# fill NAs
refda <- strptime('2018-06-30 00:00:00', "%Y-%m-%d %H:%M:%S", tz="UTC")   #add ref date
stork.burst <- setNA(stork.burst, refda, dt = 30, units = 'min')


# round the timestamps to make a 'regular' trajectory
stork.burst <- sett0(stork.burst, refda, 30, units = 'min')
#stork.burst

# create a dataframe to play with
stork.clean <- ld(stork.burst)
head(stork.clean)

hist.ltraj(stork.burst)

##### 3. Simple Movement Metrics

## Speed 

# step length distribution
hist(stork.clean$dist)

# speed over time
ggplot(stork.clean, aes(x = date, y = dist/1000)) + geom_point() + geom_line() + facet_wrap(.~id) + ylab('km per 30 min') # or dist/1000*2 for km/hr

# !!Are these values biologically reasonable? May want to create an additional filter to remove relocations with very high speeds!!

## Daily distance moved

# create a day variable
stork.clean$ymd <- lubridate::as_date(stork.clean$date)

# aggregate by day
stat <- function(x) c(sum = sum(x))
mov.daily <- as.data.frame(aggregate(dist ~ ymd + id, stork.clean, stat)) # aggregated stats by day

ggplot(mov.daily, aes(x = ymd, y = dist)) + geom_point() +geom_line() + facet_wrap(.~id)

## Turning angles

# what is the distribution of turning angles?
rose.diag(stork.clean[!is.na(stork.clean$rel.angle),]$rel.angle, bins=24, prop=1.8, main = 'rose diagram of turning angles')

# how do turning angles relate to step length? 
plot(stork.clean$dist, stork.clean$rel.angle, pch = 19)

##### 4. Net squared displacement

# Where is net squared displacement stored in the dataset?
head(stork.clean)

# What does it look like over time? How might we be able to use this to identify migration patterns?
ggplot(stork.clean, aes(x = date, y = R2n/1000)) + geom_point() + facet_wrap(.~id)

# See R package 'migrateR' for additional methods related to net squared displacement 

##### 5. Visualize the movements
library(mapview)

stork.sf <- st_as_sf(stork.ds, coords = c('x', 'y'), crs = "+proj=utm +zone=32n")

mapview(stork.sf, zcol = 'name', cex = 0.5) # cover zcol, cex

##### 6. Movement states - Probably won't cover this. Very simplified method
library(mclust)
t <- stork.clean[!is.na(stork.clean$R2n),]
t$nsd_diff <- lag(t$R2n) - t$R2n
t <- t[!is.na(t$nsd_diff),]
clust <- Mclust(t$R2n, G = 2)
plot(clust)

t$state <- clust$classification
stork.sf <- st_as_sf(t, coords = c('x', 'y'), crs = "+proj=utm +zone=32n")
mapview(stork.sf, zcol = 'state') 







