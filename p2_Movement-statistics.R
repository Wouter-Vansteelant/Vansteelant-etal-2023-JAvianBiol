### Read packages 
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage
packages <- c("ggplot2","lubridate","maptools","circular","lattice","fossil","RColorBrewer","rgdal","sp","raster","tidyverse")
ipak(packages)

## WHEN LAST RUN THIS SCRIPT WE RAN RESAMPLING PROCEDURE.
## TO FOLLOW UP REACH CSV FILE AND ENSURE ALL DATA ARE IN CORRECT FORMAT
#meta <- read.csv('./Data/Metadata_full-v20201014.csv')
#data <- read.csv('./Data/EF-resampled-v2020-10-14.csv')

meta <- read.csv('./Data/Metadata_full-v20201207b.csv')
data <- read.csv('./Data/EF-resampled-v20210322.csv')

# Format data
data <- data[,!(colnames(data) %in% c("X","julian","near.colony","phase","segment","segment.length","colony.lat","colony.long"))]
data$dt <- as.POSIXct(strptime(data$dt, format="%Y-%m-%d %H:%M:%S"), tz='UTC')
data$date <- as.Date(data$dt,tz='UTC')
data$alt <- as.numeric(data$alt)

data <- data[order(data$dev,data$dt),]

#source('pooling_mellone.R')
#data <- rbind(data,mellone)
#data <- merge(data,unique(meta[,c("dev","colony")]),all.x=T)

############################################################
#####     Classify Day vs Night observations            ####     
############################################################
crds <- cbind(data$long,data$lat)
crds <- SpatialPoints(crds,proj4string=CRS("+proj=longlat +datum=WGS84"))

dates <- as.POSIXct(strptime(data$dt,format="%Y-%m-%d"),tz="UTC")

# calculate sunrise times
srise <- sunriset(crds, dates, direction=c("sunrise"),POSIXct.out=TRUE)
colnames(srise)[1:2] <- c("srise.dayfrac","srise.time")

# calculate sunset times
sset <- sunriset(crds, dates, direction=c("sunset"),POSIXct.out=TRUE)
colnames(sset)[1:2] <- c("sset.dayfrac","sset.time")

# append sunrise and sunset in exact times back to dataframe
data$srise_R <- srise[,2]
data$sset_R <- sset[,2]

# calculate solar noon
snoon <- solarnoon(crds,dates,POSIXct.out=TRUE)
colnames(snoon)[1:2] <- c("snoon.dayfrac","snoon.time")

# append solar noon and calculate dt relative to noon
data$snoon_R <- snoon[,2]
data$dt_to_noon <- as.numeric(difftime(data$dt,data$snoon_R,units='hours'))

# calculate srise and sset relative to solar noon
data$srise_to_noon <- as.numeric(difftime(data$srise_R,data$snoon_R,units='hours'))
data$sset_to_noon <- as.numeric(difftime(data$sset_R,data$snoon_R,units='hours'))

# calculate start of astronomical dawn
crep1 <- crepuscule(crds, dates, solarDep=18, direction=c("dawn"),POSIXct.out=TRUE)
colnames(crep1)[1:2] <- c("srise.dayfrac","srise.time")

# calculate end of astronumical dusk
crep2 <- crepuscule(crds, dates, solarDep=18, direction=c("dusk"),POSIXct.out=TRUE)
colnames(crep2)[1:2] <- c("sset.dayfrac","sset.time")

# append sunrise and sunset in exact times back to dataframe
data$crep1_R <- crep1[,2]
data$crep2_R <- crep2[,2]

# calculate astronomical dawn and dusk relative to noon
data$crep1_to_noon <- as.numeric(difftime(data$crep1_R,data$snoon_R,units='hours'))
data$crep2_to_noon <- as.numeric(difftime(data$crep2_R,data$snoon_R,units='hours'))


# classify day and night locations
data$daynight <- ifelse(data$dt > data$crep1_R & data$dt < data$srise_R,'dawn',
                        ifelse(data$dt >= data$srise_R & data$dt < data$sset_R,'day',
                               ifelse(data$dt >= data$sset_R & data$dt < data$crep2_R,'dusk','night')))


# get limits for sunrise and sunset in both seasons
srise <- cbind(srise,snoon$snoon.time)
colnames(srise)[3] <- "snoon.time"
sset <- cbind(sset,snoon$snoon.time)
colnames(sset)[3] <- "snoon.time"
srise$srise.to.noon <- as.numeric(difftime(srise$srise.time,srise$snoon.time,units='hours'))
sset$sset.to.noon <- as.numeric(difftime(sset$sset.time,sset$snoon.time,units='hours'))

srise$trip <- ifelse(month(srise$srise.time) > 7,'out','return')
srise2 <- srise %>%
  group_by(trip) %>%
  summarize(srise.min = min(srise.dayfrac),
            srise.max = max(srise.dayfrac),
            srise.to.noon.min = min(srise.to.noon),
            srise.to.noon.max = max(srise.to.noon)) %>%
  ungroup()

sset$trip <- ifelse(month(sset$sset.time) > 7,'out','return')
sset2 <- sset %>%
  group_by(trip) %>%
  summarize(sset.min = min(sset.dayfrac),
            sset.max = max(sset.dayfrac),
            sset.to.noon.min = min(sset.to.noon),
            sset.to.noon.max = max(sset.to.noon)) %>%
  ungroup()

suntimes <- merge(srise2,sset2)

# Remove obsolete
rm(crds,dates,srise,sset,srise2,sset2,crep1,crep2,snoon)

#########################################
#####     MOVEMENT STATISTICS        ####     
#########################################
# order dataframe
data<-data[order(data$tripID,data$dt),]

source('sidescript_pt2pt_fxns.R')
## define functions to calculate forward distances and duration
calcdist <- function(x) pt2pt.distance(longitude=x$long,latitude=x$lat)
calcdur <- function(x) pt2pt.duration(datetime=x$dt)
## define functions to calculate backward distances and duration
calcdist.b <- function(x) pt2pt.back.distance(longitude=x$long,latitude=x$lat)
calcdur.b <- function(x) pt2pt.back.duration(datetime=x$dt)

## We must order the dataframe in order to ensure the correct application of our coming functions
data <- data[order(data$tripID,data$dt),]
v1 <- lapply(split(data,data$tripID),"calcdist")
v2 <- lapply(split(data,data$tripID),"calcdur")
v1b <- lapply(split(data,data$tripID),"calcdist.b")
v2b <- lapply(split(data,data$tripID),"calcdur.b")

data$dist.f <- as.numeric(unlist(v1))
data$dur.f <- as.numeric(unlist(v2))
data$spd.f <- data$dist.f/data$dur.f
data$dist.b <- as.numeric(unlist(v1b))
data$dur.b <- as.numeric(unlist(v2b))
data$spd.b <- data$dist.b/data$dur.b

data$spd <- (data$spd.f + data$spd.b) / 2

data <- data[which(data$spd < 30),]

## We must order the dataframe in order to ensure the correct application of our coming functions
data <- data[order(data$tripID,data$dt),]
v1 <- lapply(split(data,data$tripID),"calcdist")
v2 <- lapply(split(data,data$tripID),"calcdur")
v1b <- lapply(split(data,data$tripID),"calcdist.b")
v2b <- lapply(split(data,data$tripID),"calcdur.b")

data$dist.f <- as.numeric(unlist(v1))
data$dur.f <- as.numeric(unlist(v2))
data$spd.f <- data$dist.f/data$dur.f
data$dist.b <- as.numeric(unlist(v1b))
data$dur.b <- as.numeric(unlist(v2b))
data$spd.b <- data$dist.b/data$dur.b

data$spd <- (data$spd.f + data$spd.b) / 2

calcdir <- function(x) pt2pt.direction(longitude=x$long,latitude=x$lat)
v3 <- lapply(split(data,data$tripID),"calcdir")
data$dir <- as.numeric(unlist(v3))

rm(v1,v2,v3,v1b,v2b)

source('sidescript_CalcDailyStats-inclCumDist.R')
data$daily.straight <- data$daily.dist/data$daily.cum.dist

create.obs.nr <- function(x) seq(1:length(x$dt))
v1 <- lapply(split(data,data$tripID),"create.obs.nr")

data$trip.obs.nr <- as.numeric(unlist(v1))
### Distinguish travel from rest day sand segment track     ####
################################################################
## Use 100km as threshold for travel
data$travel <- ifelse(data$daily.dist < 100*1000,0,1)

## Segment track in travel vs stop
data <- data[order(data$dev,data$dt),]
data$segment <- as.numeric(c(0,cumsum(as.logical(diff(as.numeric(as.factor(paste(data$tripID,data$travel))))))))
data$segment <- sprintf("%04d",data$segment)
data$segment <- paste(data$dev,data$segment,sep="_")

seglengths <- data %>%
  group_by(segment) %>%
  summarize(segment.length = length(unique(yday))) 

data <- merge(data,seglengths,all.x=TRUE)

data$travel2 <- ifelse(data$daily.dist > 750000,2,data$travel)

### Calculate duration for each stopover event
source('sidescript_CalcStopovers.R')
segs <- merge(segs,unique(meta[,c("dev","sex","colony")]),all.x=TRUE)

ref <- unique(meta[which(meta$dev %in% unique(data$dev)),c("colony","colony.lat","colony.long")])
segs$dist.to.colony <- deg.dist(long1=ref[[3]],lat1=ref[[2]],long2=segs$st.long,lat2=segs$st.lat)*1000
#segs <- subset(segs,segs$dist.to.colony < 8000000 & segs$dist.to.colony >= 1700000)

rm(segs,seglengths,v1)


write.csv(segs,'./Data/EF_resampled_stop_segments-v20210322.csv')

