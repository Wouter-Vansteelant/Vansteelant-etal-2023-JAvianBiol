### Read packages 
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage
packages <- c("geosphere")
ipak(packages)

#######################################################
#####     Resample migration tracks                ####
#####     and calculate final movement stats       ####     
########################################################

### READ MIGRATION DATA (TO BE RESAMPLED)
##########################################
data <- read.csv('./Data/EF_migration-v20210322.csv')
data <- data[,!(colnames(data) %in% c("X","julian","segment.f","segment","segment.length","flag","daily.range","daily.obs","daily.dist","daily.dur","daily.dir"))]
data$dt <- as.POSIXct(strptime(data$dt, format="%Y-%m-%d %H:%M:%S"), tz='UTC')
data$date <- as.Date(data$dt)
data$alt <- as.numeric(data$alt)
## order dataframe chronologically per device
data<-data[order(data$tripID,data$dt),]


### RESAMPLE MIGRATION DATA
##########################################
## read resample function
source('Rfunction_resampling_movement.R')
data_resample2<-moveTimeSample(data,data$dt,data$tripID,60,10,subset=TRUE)

# Check dresampling by counting nr of observations per day
ul <- function(x) length(unique(x))
obsperday <- aggregate(as.numeric(data_resample2$dt), by=list(data_resample2$indday), FUN=ul)
colnames(obsperday)[1:2] <- c("indday","daily.obs")
hist(obsperday$daily.obs)

data <- data_resample2
rm(data_resample2)

# remove faulty points 
data <- subset(data,!(data$lat <1 & data$long < 0))

# create same grouping variable for drawing paths as made for nonmig dataset
data$group <- paste(data$dev,data$cycle,data$phase,data$mth)
data <- data[order(data$group,data$dt),]

write.csv(data,paste('./Data/EF-resampled-v20210322.csv',sep=''))

### READ STOP-OVER DATA 
##########################################
segs <- read.csv('./Data/EF_migration_stop-segments-v20210105.csv')
segs$st.start <- as.POSIXct(strptime(segs$st.start, format="%Y-%m-%d %H:%M:%S"), tz='UTC')
segs$st.end <- as.POSIXct(strptime(segs$st.end, format="%Y-%m-%d %H:%M:%S"), tz='UTC')


### READ NON-MIGRATION DATA (MAPPING ONLY)
###########################################
nonmig <- read.csv('./Data/EF_SummerWinter-v20210105.csv')
nonmig <- nonmig[,!(colnames(nonmig) %in% c("X","julian","segment","trip.end","trip.start","segment.f","segment.length","flag","daily.range","daily.obs","daily.dist","daily.dur","daily.dir"))]
nonmig$dt <- as.POSIXct(strptime(nonmig$dt, format="%Y-%m-%d %H:%M:%S"), tz='UTC')
nonmig$date <- as.Date(nonmig$dt)
nonmig$alt <- as.numeric(nonmig$alt)
## order dataframe chronologically per device
nonmig<-nonmig[order(nonmig$tripID,nonmig$dt),]

# remove faulty fixes over Indian Ocean in winter
nonmig <- subset(nonmig,!(nonmig$long > 50.1 & is.na(nonmig$country == TRUE)))

# create grouping variable for drawing movement paths
nonmig$group <- paste(nonmig$dev,nonmig$cycle,nonmig$phase,nonmig$mth)
nonmig <- nonmig[order(nonmig$group,nonmig$dt),]

