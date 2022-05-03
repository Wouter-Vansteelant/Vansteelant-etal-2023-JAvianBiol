### Set Working directory
#setwd("C:/Users/wvanste1/Documents/2019 - Eleonoras Falcon/analyses/")

### Read packages 
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage
packages <- c("RODBC","ggplot2","lubridate","maptools","circular","lattice","fossil","RColorBrewer","rgdal","sp","raster","tidyverse")
ipak(packages)

##################################################
### Read tracking data  from uva-bits server  ####   LAST UPDATED 29 NOV 2019
##################################################
#db.file <- "GPS"
#db <- odbcConnect(db.file) 

#ids <- sqlQuery(db,query = "SELECT t.device_info_serial, max(t.longitude) 
#                FROM gps.ee_track_session_limited s
#                 JOIN gps.ee_individual_limited i
#                ON s.individual_id = i.individual_id
#                JOIN gps.ee_tracking_speed_limited t
#                ON t.device_info_serial = s.device_info_serial
#                WHERE i.species_latin_name = 'Falco eleonorae'  
#                GROUP BY t.device_info_serial
#                ORDER BY t.device_info_serial ASC")
#colnames(ids) <- c("dev","maxlong")

## We only want data for birds that completed at least one migration
## to Madagascar and back, as determined based on maximum recorded longitude.
## The names in this list should be inserted in the query below
# last updated 2019-02-25
#(selection <- c(ids[which(ids$maxlong > 43),]$dev))

#data <- sqlQuery(db, query = "SELECT t.device_info_serial as dev, 
#                 date_part('year'::text, t.date_time) as yr,
#                 date_part('month'::text, t.date_time) as mth,
#                 date_part('day'::text, t.date_time) as day,
#                 date_part('hour'::text, t.date_time) as hr,
#                 date_part('minute'::text, t.date_time) as min,
#                 date_part('second'::text, t.date_time) as sec,
#                 t.latitude as lat, t.longitude as long, t.altitude as alt
#                 FROM gps.ee_track_session_limited s
#                 JOIN gps.ee_individual_limited i
#                  ON s.individual_id = i.individual_id
#                 JOIN gps.ee_tracking_speed_limited t
#                 ON t.device_info_serial = s.device_info_serial
#                 WHERE i.species_latin_name = 'Falco eleonorae'
#                 AND t.device_info_serial IN (1011,1012,1013,1014,2048,2051,2337,2341,2368,2378,2380,2391,2392,2394,2400,2423,2424,2447,2450,2453)
#                 AND t.latitude is not null AND t.longitude is not null AND t.userflag = 0
#                 AND t.date_time > s.start_date
#                 ORDER by t.device_info_serial, t.date_time")

#write.csv(data,'EF_fulldata-v20190225.csv')

data <- read.csv('./Data/EF_fulldata_migrants-v20201207.csv') #LAST VERSION INCLUDES ALL DEVS. 
length(unique(data$dev))

# formate datetimes
data$mth <- sprintf("%02d", data$mth)
data$day <- sprintf("%02d", data$day)
data$hr <- sprintf("%02d", data$hr)
data$min <- sprintf("%02d", data$min)
data$sec <- sprintf("%02d", data$sec)

data$date <- as.Date(paste(data$yr,data$mth,data$day,sep='-'))
data$time <- paste(data$hr,data$min,data$sec,sep=':')

data$dt <- paste(data$date,data$time,sep=' ')
data$dt <- as.POSIXct(strptime(data$dt, format="%Y-%m-%d %H:%M:%S"), tz='UTC')

# subset columns
data <- data[,c("dev","dt","lat","long","alt")]

## extract year and month columns for tracking data and nests
library(lubridate)
data$date <- as.Date(data$dt)
data$mth <- month(data$dt)
data$yr <- year(data$dt)

## create new bird ID as character
data$dev <- paste('B',data$dev,sep='')

# extract julian day since 1st january 2000 for tracking data 
data$julian <- as.numeric(julian(trunc(data$dt, "days"), origin = as.POSIXct(paste(min(data$yr),"-01-01 00:00:00",sep=""), tz="UTC")))
data$julian <- factor(sprintf("%04d",data$julian))

# extract day of year
data$yday <- yday(data$dt)

# add column dev/julian
data$indday <- as.factor(paste(data$dev, data$julian, sep ='_', collapse = NULL))

############################################################
#####     Classify trips based on distance to island    ####
############################################################
# order dataframe
data<-data[order(data$dev,data$dt),]

# center point Alegranza
data$colony <- rep("Canaries")
meta <- read.csv('./Data/Metadata_full-v20201207.csv')

data <- merge(data,unique(meta[,c("colony","colony.lat","colony.long")]),all.x=TRUE)

# Calculate distance of bird to center point of Alegranza
source('sidescript_pt2pt_fxns.R')
xx <- pt2pt.range(data$lat,data$long,data$colony.lat,data$colony.long,threshold=5*1000)
data$dist.to.colony <- xx$range.distance
data$near.colony <- xx$is.in.range

## Create column for tripID (i.e. divides track in half year periods)
data$trip <- ifelse(data$mth >=8,'out','return')
data$tripID <- paste(data$dev,data$yr,data$trip,sep="_")

## Find final return data in spring
## label days based on proximity to colony: if at least one fix in colony then colony-day
label.colony.days <- function(x) ifelse(sum(x) > 1,1,0)
xx <- aggregate(data$near.colony,by=list(data$indday),FUN="label.colony.days")
colnames(xx) <- c("indday","colony.day")
data <- merge(data,xx,all.x=TRUE)

data <- data[order(data$dev,data$dt),]
data$segment.col.days <- as.numeric(c(0,cumsum(as.logical(diff(as.numeric(as.factor(paste(data$dev,data$colony.day))))))))
data$segment.col.days <- sprintf("%04d",data$segment.col.days)
data$segment.col.days <- paste(data$dev,data$segment.col.days,sep="_")

cul <- function(x) length(unique(x))
xx <- aggregate(data$indday,by=list(data$segment.col.days),FUN="cul")
colnames(xx) <- c("segment.col.days","col.days.dur")
data <- merge(data,xx,all.x=TRUE)

ss <- subset(data,data$trip =="return" & data$col.days.dur >=7 & data$colony.day == 1)
final.return.dates <- aggregate(ss$date,by=list(ss$tripID),FUN="min")
colnames(final.return.dates) <- c("tripID","final.return")

## Segment track into segments (based on distance to colony, using 5km as threshold)
data <- data[order(data$dev,data$dt),]
data$segment <- as.numeric(c(0,cumsum(as.logical(diff(as.numeric(as.factor(paste(data$dev,data$near.colony))))))))
data$segment <- sprintf("%04d",data$segment)
data$segment.f <- paste(data$dev,data$segment,sep="_")

## Calculate max dist from colony in each segment to determine what segments include migratory journeys
## we flag segments containing migration data based on 8000km threshold
xx <- aggregate(data$dist.to.colony,by=list(data$segment.f),FUN="max")
colnames(xx) <- c("segment.f","maxdist")
xx$flag <- ifelse(xx$maxdist > 8000*1000,1,0)
data <- merge(data,xx[,c("segment.f","flag")],all.x=TRUE)
rm(xx)

# Before subsetting data to segmetns including migratory journeys, save short trips in other df
data_trips_short <- subset(data,data$flag == 0)

prebreed <- subset(data_trips_short,data_trips_short$trip == "return")
prebreed <- merge(prebreed,final.return.dates,all.x=TRUE)
prebreed <- subset(prebreed,prebreed$date < prebreed$final.return)
#write.csv(data_trips_short,'./Data/EF_SummerShortTrips-v20210322.csv')
rm(data_trips_short)

# Subset data to those segments including migratory journeys (i.e. where birds travel further than 8000km)
ss <- subset(data,data$flag == 1)

# remove faulty point for 2453
ss <- subset(ss,as.character(ss$dt) != "2019-08-14 14:11:50")

# overwrite df data with subset ocntaining only those trips continaing migratory movements
rm(data)
data <- ss
rm(ss)

#####     Determine geographical position               ####     
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
countries <- shapefile("~/Documents/2019 - Eleonoras Falcon/EF01 - analyses/Maps/ne_50m_admin_0_countries/ne_50m_admin_0_countries.shp")
countries <- countries[countries$continent %in% c("Europe","Africa","Asia"),]

np <- data[,c("dev","dt","long","lat")]
coordinates(np) <- ~ long + lat
proj4string(np) <- crs(countries)

## Extract country per point
require(spatialEco)
out <- point.in.poly(np, countries["admin"])
out <- as.data.frame(out)
colnames(out)[3] <- "country"
data <- merge(data,out[,c("dev","dt","country")],all.x=T)
rm(np,out)

## Extract continent per point
#out <- point.in.poly(np, countries["continent"])
#out <- as.data.frame(out)

#data <- merge(data,out[,c("dev","dt","continent")],all.x=T)
#data$continent <- ifelse(is.na(data$continent) == T,'sea',as.character(data$continent))
#data$continent <- factor(as.factor(data$continent),levels=c("Europe","Africa","sea"))

###   DETERMINE START AND END DATES OF MIGRATORY TRIPS   ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
## Calculate Daily statistics to identify travel days (> 100km between roosts)
source('sidescript_CalcDailyStats.R') #we use support script to get daily distance, duration, direction
data$travel <- ifelse(data$daily.dist < 100*1000,0,1) # Use 50km as threshold for travel

data <- data[order(data$dev,data$dt),]
data$segment <- as.numeric(c(0,cumsum(as.logical(diff(as.numeric(as.factor(paste(data$segment.f,data$travel))))))))
data$segment <- sprintf("%04d",data$segment)
data$segment.f <- paste(data$tripID,data$segment,sep="_")

seglengths <- data %>%
  group_by(segment.f) %>%
  summarize(segment.length = length(unique(yday))) 

data <- merge(data,seglengths,all.x=TRUE)

### End of outbound and start of return identified as first point on Madagascar during autumn,
# and last point on Mad on return  
out.ends.fin <- data %>%
  filter(as.character(trip) == "out",
         as.character(country) == "Madagascar",
         travel == 0,
         segment.length >= 3) %>%
  group_by(tripID) %>%
  summarize(trip.end.fin = min(dt, na.rm = TRUE)) 
out.ends.fin <- as.data.frame(out.ends.fin)

out.ends <- data %>%
  filter(as.character(trip) == "out",
         as.character(country) == "Madagascar") %>%
  group_by(tripID) %>%
  summarize(trip.end = min(dt, na.rm = TRUE)) 
out.ends <- as.data.frame(out.ends)

return.starts <- data %>%
  filter(as.character(trip) == "return",
         as.character(country) == "Madagascar") %>%
  group_by(tripID) %>%
  summarize(trip.start = max(dt, na.rm = TRUE)) 
return.starts <- as.data.frame(return.starts)

return.starts.fin <- data %>%
  filter(as.character(trip) == "return",
         as.character(country) == "Madagascar",
         travel == 0,
         segment.length >= 3) %>%
  group_by(tripID) %>%
  summarize(trip.start.fin = max(dt, na.rm = TRUE)) 
return.starts.fin <- as.data.frame(return.starts.fin)

### Start of outbound and end of return identified based on mobility in NW AFrica (excluding pre and post migratory stops north of sahara)
return.ends <- data %>%
  filter((as.character(trip) == "return" & travel == 0 & as.character(country) %in% c("Morocco","Western Sahara","Spain"))|
           (as.character(trip) == "return" & is.na(country)==TRUE & long < 5)|
           (as.character(trip) == "return" & dist.to.colony < 50)) %>%
  group_by(tripID) %>%
  summarize(trip.end = min(dt, na.rm = TRUE)) 
return.ends <- as.data.frame(return.ends)

return.ends.fin <- data %>%
  filter(as.character(trip) == "return") %>%
  group_by(tripID) %>%
  summarize(trip.end.fin= max(dt, na.rm = TRUE)) 
return.ends.fin <- as.data.frame(return.ends.fin)

out.starts <- data %>%
  filter(as.character(trip) == "out",
         travel == 1,
         as.character(country) %in% c("Morocco","Western Sahara")) %>%
  group_by(tripID) %>%
  summarize(trip.start = max(dt, na.rm = TRUE)) 
out.starts <- as.data.frame(out.starts)

out.starts.fin <- data %>%
  filter(as.character(trip) == "out") %>%
  group_by(tripID) %>%
  summarize(trip.start.fin = min(dt, na.rm = TRUE)) 
out.starts.fin <- as.data.frame(out.starts.fin)

### We can now clip data between starts and ends of journeys
trip.ends <- rbind(out.ends,return.ends)
trip.starts <- rbind(out.starts,return.starts)
trip.starts.fin <- rbind(out.starts.fin,return.starts.fin)
trip.ends.fin <- rbind(out.ends.fin,return.ends.fin)
trip.lims <- merge(trip.ends,trip.starts)
trip.lims.fin <- merge(trip.starts.fin,trip.ends.fin)
trip.lims <- merge(trip.lims,trip.lims.fin)

data <- merge(data,trip.lims,all.x=TRUE)
rm(out.ends,return.ends,trip.ends,out.starts,return.starts,trip.starts,
   return.starts.fin,out.starts.fin,return.ends.fin,out.ends.fin,
   trip.ends.fin,trip.starts.fin,
   trip.lims,trip.lims.fin,seglengths)

data$phase <- ifelse(data$trip == "out" & data$date >= as.Date(data$trip.start.fin) & data$dt < data$trip.end.fin,"out",
                    ifelse(data$trip == "return" & data$dt >= data$trip.start.fin & data$dt < data$trip.end,"return",
                           ifelse(data$lat >= 0,"summer","winter")))

# extract remaining data for pre-breeding season (i.e. between end of spring to first return within 5km from Island)
prebreed2 <- subset(data,data$phase == "summer" & data$trip == "return")

prebreed.fin <- rbind(prebreed[,c("dev","indday","date","dt","lat","long","yr","trip")],prebreed2[,c("dev","indday","date","dt","lat","long","yr","trip")])
rm(prebreed,prebreed2)
write.csv(prebreed.fin,"./Data/prebreeding-data-full_v20210520.csv")

# flag migration data
data$flag <- ifelse(data$phase %in% c("out","return"),1,0)

# NOTE: for two cylces no phase annotated due to gaps in data
# remove half year periods during which only partial data were recorded uring migration
data <- subset(data,!(data$tripID %in% c("B1014_2016_return","B2337_2018_return")))

# Annotate cycle of migration
data$cycle <- ifelse(data$date < as.Date("2013-07-01"),"2012-13",
                     ifelse(data$date < as.Date("2014-07-01"),"2013-14",
                            ifelse(data$date < as.Date("2015-07-01"),"2014-15",
                                   ifelse(data$date < as.Date("2016-07-01"),"2015-16",
                                          ifelse(data$date < as.Date("2017-07-01"),"2016-17",
                                                 ifelse(data$date < as.Date("2018-07-01"),"2017-18",
                                                        ifelse(data$date < as.Date("2019-07-01"),"2018-19",
                                                               ifelse(data$date < as.Date("2020-07-01"),"2019-20","2020-2021"))))))))

# remove cycle for which only summer data were recorded
data <- subset(data,data$cycle != "2020-2021")
    
cycle.starts <- data %>%
  group_by(cycle) %>%
  summarize(cycle.start = as.Date(paste(min(yr),"-07-01 00:00:01",sep=''))) 
cycle.starts <- as.data.frame(cycle.starts)

data <- merge(data,cycle.starts,all.x=TRUE)
data$cycle.start <- as.POSIXct(strptime(paste(data$cycle.start,"00:00:01",sep=" "), format="%Y-%m-%d %H:%M:%S"), tz='UTC')
data$cycle.time <- as.numeric(difftime(data$dt,data$cycle.start,units="days"))
rm(cycle.starts)

write.csv(data,'./Data/EF_fulldata_migrants-AnnotatedCycle-v20210322.csv') 
length(unique(data$dev))

### Segment track in travel vs stationary parts
################################################################
data <- data[order(data$dev,data$dt),]
data$segment <- as.numeric(c(0,cumsum(as.logical(diff(as.numeric(as.factor(paste(data$tripID,data$travel))))))))
data$segment <- sprintf("%04d",data$segment)
data$segment <- paste(data$dev,data$segment,sep="_")

seglengths <- data %>%
  group_by(segment.f) %>%
  summarize(segment.length = length(unique(yday))) 

data <- merge(data,seglengths,all.x=TRUE)

###################################################
#####     STORE MIGRATIoN DATA FOR ANLAYSES       ####     
###################################################
ss <- subset(data,data$flag == 1)
ss2 <- subset(data,data$flag == 0)
rm(data)
data <- ss
data <- data[order(data$dev,data$dt),]

write.csv(data,'./Data/EF_migration-v20210322.csv')
write.csv(ss2,'./Data/EF_SummerWinter-v20210322.csv')
rm(ss,ss2)

###################################################
#####     CREATE DF FOR STOP-OVERS      ####     
###################################################
### Calculate duration for each stopover event
source('sidescript_CalcStopovers.R')

ref <- unique(meta[which(meta$dev %in% unique(data$dev)),c("colony","colony.lat","colony.long")])

segs <- merge(segs,unique(meta[,c("dev","colony")]),all.x=TRUE)
segs$dist.to.colony <- deg.dist(long1=ref[[3]],lat1=ref[[2]],long2=segs$st.long,lat2=segs$st.lat)*1000
#segs <- subset(segs,segs$dist.to.colony < 8000000 & segs$dist.to.colony >= 1700000)

segs <- merge(segs,unique(data[,c("tripID","segment","cycle","cycle.start","phase")]))
segs$cycle.start <- as.POSIXct(strptime(segs$cycle.start, format="%Y-%m-%d %H:%M:%S"), tz='UTC')

seg.starts <- data %>%
  group_by(segment) %>%
  summarize(seg.start = min(dt, na.rm = TRUE)) 
seg.starts <- as.data.frame(seg.starts)
segs <- merge(segs,seg.starts,all.x=TRUE)
segs$cycle.time <- as.numeric(difftime(segs$seg.start,segs$cycle.start,units='days'))
rm(seg.starts,obsperday)

write.csv(segs,'./Data/EF_migration_stop-segments-v20210322.csv')
