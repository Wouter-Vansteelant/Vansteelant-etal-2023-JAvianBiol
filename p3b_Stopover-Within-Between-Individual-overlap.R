# usage
packages <- c("adehabitatHR")
ipak(packages)

# specify sex for analyses (note that code needs to be ran twice or three times
# to produce results for each sex and all birds combined)
selected <- c("male","female")

### Prep stop-over data
### ### ### ### ### 
stops <- read.csv('./Data/EF_resampled_stop_segments-v20210322.csv')

# change season labels
stops$trip <- ifelse(stops$trip == "out","Autumn","Spring")

# proces stops to individuals with multiple trips, ...
stops <- merge(stops,ind.summ[,c("dev","trip","n.trips")],all.x=TRUE)
stops <- subset(stops,stops$n.trips > 1)

stops$cycle <- ifelse(stops$trip == "Autumn",paste(stops$yr,stops$yr+1,sep="-"),paste(stops$yr-1,stops$yr,sep="-"))
stops$cycle.start <- ifelse(stops$trip == "Autumn",paste(stops$yr,"-07-01 00:00:01",sep=""),paste(stops$yr-1,"-07-01 00:00:01",sep=""))
stops$cycle.start <- as.POSIXct(strptime(stops$cycle.start, format="%Y-%m-%d %H:%M:%S"), tz='UTC')
stops$st.start <- as.POSIXct(strptime(stops$st.start, format="%Y-%m-%d %H:%M:%S"), tz='UTC')
stops$st.end <- as.POSIXct(strptime(stops$st.end, format="%Y-%m-%d %H:%M:%S"), tz='UTC')
stops$cycle.time <- as.numeric(difftime(stops$st.start,stops$cycle.start,units="days"))

# remove stop-overs <100 km from colony
stops <- subset(stops,stops$dist.to.colony > 100000)

# remove stop-overs north of 20N in spring: these stop-overs are part of
# the pre-breeding cycle
stops <- subset(stops,!(stops$trip == "Spring" & stops$st.lat >= 20))

## add pre-breeding stops to migratory stop-over df
stops <- rbind(stops[,c("dev","sex","trip","st.lat","st.long","st.dur","st.start","st.end","tripID","yr","dist.to.colony","cycle","cycle.time")],
       prebreed.stops[,c("dev","sex","trip","st.lat","st.long","st.dur","st.start","st.end","tripID","yr","dist.to.colony","cycle","cycle.time")])
#rm(prebreed.stops)

### classify stops per region
stops$region <- ifelse(stops$trip == 'Autumn' & stops$st.lat > 20,'north-autumn',
                       ifelse(stops$trip == 'Autumn' & stops$st.long < 25,'sahel-autumn',
                              ifelse(stops$trip == 'Autumn' & stops$st.long > 25 & stops$st.long <= 43,'east-autumn',
                                     ifelse(stops$trip == 'Spring' & stops$st.lat > -5 & stops$st.long > 20,'east-spring',
                                            ifelse(stops$trip == 'Spring' & stops$st.long < 20 & stops$st.lat < 15,'sahel-spring',
                                                   ifelse(stops$trip == 'Spring' & stops$st.lat > 18,'north-spring','other'))))))


stops2 <- subset(stops,stops$sex %in% selected)

# quick visual check of classification: 
#ggplot()+borders('world')+geom_point(data=stops,aes(x=st.long,y=st.lat,col=region))

# determine first and last stop-over timestamp for each ID per region/season/yr (we will use this to extract relevant tracking data from full data)
trip.stops <- stops %>%
  group_by(sex,dev,trip,tripID,yr,cycle,region) %>%
  summarize(region.stop.start = min(st.start, na.rm = TRUE),
            region.stop.end = max(st.end, na.rm = TRUE),
            region.stop.dur = as.numeric(round(difftime(region.stop.end,region.stop.start,units="days")))) %>%
  ungroup()

# use previously calculated date ranges to subset tracking data for each of the stop-over periods
stop.tracks <- data[,c("dev","sex","trip","dt","lat","long","tripID","yr","cycle","daily.dist")]
prebreed.tracks <- prebreed.fin[,c("dev","sex","trip","dt","lat","long","tripID","yr","cycle","daily.dist")]

stop.tracks <- rbind(stop.tracks,prebreed.tracks)
rm(prebreed.tracks)

stop.tracks$region <- ifelse(stop.tracks$trip == 'Autumn' & stop.tracks$lat > 20,'north-autumn',
                             ifelse(stop.tracks$trip == 'Autumn' & stop.tracks$lat < 15 & stop.tracks$long < 25,'sahel-autumn',
                                    ifelse(stop.tracks$trip == 'Autumn' & stop.tracks$long > 25 & stop.tracks$long <= 43,'east-autumn',
                                           ifelse(stop.tracks$trip == 'Spring' & stop.tracks$lat > -5 & stop.tracks$long > 20,'east-spring',
                                                  ifelse(stop.tracks$trip == 'Spring' & stop.tracks$lat < 15 & stop.tracks$long < 20,'sahel-spring',
                                                         ifelse(stop.tracks$trip == 'Spring' & stop.tracks$lat > 18,'north-spring','other'))))))
stop.tracks$region2 <- as.data.frame(do.call("rbind",strsplit(stop.tracks$region,"-",fixed=TRUE)))[,1]

stop.tracks <- subset(stop.tracks,stop.tracks$sex %in% selected)
stop.tracks <- stop.tracks[order(stop.tracks$dev,stop.tracks$dt),]
#stop.tracks2 <- stop.tracks2[order(stop.tracks2$dev,stop.tracks2$dt),]

## quick visual check of classification: 
#ggplot()+borders('world')+
#  geom_path(data=stop.tracks,aes(x=long,y=lat,col=region,group=tripID))+
#  facet_grid(.~trip)

stop.tracks <- merge(stop.tracks,trip.stops[which(trip.stops$region != "other"),c("dev","trip","yr","tripID","region","region.stop.end","region.stop.start")],all.x=TRUE)
stop.tracks$flag <- ifelse(stop.tracks$dt >= stop.tracks$region.stop.start & stop.tracks$dt < stop.tracks$region.stop.end + 1,1,0)
stop.tracks.full <- subset(stop.tracks,stop.tracks$flag == 1)
stop.tracks <- subset(stop.tracks,stop.tracks$flag == 1 & stop.tracks$daily.dist <= 100000)
rm(trip.stops)

# create stopID
stop.tracks$stopID <- paste(stop.tracks$tripID,stop.tracks$region,sep="-")

# summary stats for each stop-over event per id/region/season/yr
# For each stop-over event we also create a list of DOY values ('daylist') covering
# the entire stop-over event (i.e. from first to last stop-over day within
# each region). We will use this later to calculate overlap in stop-over periods.
days.per.region.per.trip <- stop.tracks %>%
  group_by(sex,dev,tripID,trip,region2) %>%
  summarize(regional.days = yday(max(dt))-yday(min(dt))+1,
            regional.stop.days = length(unique(as.Date(dt))),
            min.yday = min(yday(dt)),
            max.yday = max(yday(dt)),
            median.yday = median(unique(yday(dt))),
            daylist = list(seq(min(yday(dt)),max(yday(dt)),1))) %>%
  ungroup()

# count prop of trips with regional stopovers and mean stop-over stats per ID/region/season
# we here calculate a series of variables to look at variability in stop-over timing (yday)
# or duration (incl and excl travel days during regional stop-over periods). However, note that
# for final analyses we explicitly look at temporal overlap in stop-over periods, for conceptual
# analogy with overlap/similarity in stop-over home ranges.
summary.stops.per.region.perID <- days.per.region.per.trip %>%
  group_by(sex,dev,trip,region2) %>%
  summarize(n.trips.with.stops = length(unique(tripID)),
            within.mean.regional.days = mean(regional.days),
            within.range.regional.days = max(regional.days)-min(regional.days),
            within.dev.regional.days = mean(abs(regional.days-within.mean.regional.days)),
            within.mean.regional.stop.days = mean(regional.stop.days),
            within.range.regional.stop.days = max(regional.stop.days)-min(regional.stop.days),
            within.mean.yday = mean(median.yday),
            within.dev.yday = mean(abs(median.yday-within.mean.yday))) %>%
  ungroup()

# summary stats per region/season
summary.stops.per.region <- summary.stops.per.region.perID %>%
  group_by(trip,region2) %>%
  summarize(between.mean.yday = mean(within.mean.yday),
            between.mean.regional.days = mean(within.mean.regional.days)) %>%
  ungroup()

days.per.region.per.trip <- merge(days.per.region.per.trip,summary.stops.per.region,all.x=TRUE)
days.per.region.per.trip$dev.yday <- abs(days.per.region.per.trip$median.yday-days.per.region.per.trip$between.mean.yday)

summary.stops.per.region.perID <- merge(summary.stops.per.region.perID,summary.stops.per.region,all.x=TRUE)
summary.stops.per.region.perID$between.dev.yday <- abs(summary.stops.per.region.perID$within.mean.yday-summary.stops.per.region.perID$between.mean.yday)
summary.stops.per.region.perID$between.dev.regional.days <- abs(summary.stops.per.region.perID$within.mean.regional.days-summary.stops.per.region.perID$between.mean.regional.days)

n.trips <- ind.summ[which(ind.summ$dev %in% unique(trip.i.coords$dev) & ind.summ$sex %in% selected),c("sex","dev","trip","n.trips")]
n.trips <- merge(n.trips,unique(stop.tracks[,c("trip","region2")]),all.x=TRUE)
n.trips <- merge(n.trips,summary.stops.per.region.perID,all.x=TRUE)
n.trips$n.trips.with.stops <- ifelse(is.na(n.trips$n.trips.with.stops)==TRUE,0,n.trips$n.trips.with.stops)
n.trips$region2 <- factor(n.trips$region2,labels=c("East","North","Sahel"))
n.trips$region2 <- factor(n.trips$region2,levels=c("North","Sahel","East"))

rm(summary.stops.per.region.perID,summary.stops.per.region)

### OVERLAP STOP-OVER PERIODS: 
## pairwise-comparison of all events in each region/season
############################################################
# create a dataframe (ss) that contains all possible pairwise comparisons between stop-over even in each region
ss1 <- days.per.region.per.trip[,c("tripID","trip","region2","daylist")]
ss2 <- days.per.region.per.trip[,c("tripID","trip","region2","daylist")]
colnames(ss1) <- c("tripIDa","trip","region2","daylista")
colnames(ss2) <- c("tripIDb","trip","region2","daylistb")
ss <- merge(ss1,ss2,all=TRUE)
ss$region2 <- factor(ss$region2,labels=c("East","North","Sahel"))
ss$region <- paste(ss$region2,ss$trip,sep="-")
rm(ss1,ss2)

# create empty vectors for outputs from overlap calculations
overlap.days <- rep(NA,length(ss[,1]))
overlap.dur<- rep(NA,length(ss[,1]))    
overlap.prop<- rep(NA,length(ss[,1]))    

rm(days.per.region.per.trip)

# execute a loop to calculate the number of matching DOY-values for each pairwise comparison of stop-over events,
# the full set of possible DOY-values from the first to last stop-over day across both individuals, and finally
# the proportion of this 'overall' DOY-range during which the two stop-over periods overlapped
for (i in 1:length(ss[,1])){ 
  overlap.ydays <- intersect(unlist(ss$daylista[i]),unlist(ss$daylistb[i]))
  min.ydays <- min(rbind(unlist(ss$daylista[i]),unlist(ss$daylistb[i])))
  max.ydays <- max(rbind(unlist(ss$daylista[i]),unlist(ss$daylistb[i])))
  full.daylist <- list(seq(min.ydays,max.ydays,1))
  overlap.days[i] <- ifelse(length(overlap.ydays)==0,NA,list(overlap.ydays))
  overlap.dur[i] <- ifelse(length(overlap.ydays)==0,0,length(unlist(overlap.days[i])))
  overlap.prop[i] <- ifelse(length(overlap.ydays)==0,0,overlap.dur[i]/length(unlist(full.daylist)))
}

ss$overlap.days <- overlap.days
ss$overlap.dur <- overlap.dur
ss$overlap.prop <- overlap.prop

rm(overlap.days,overlap.dur,overlap.prop)

ss <- subset(ss,ss$tripIDa != ss$tripIDb)
overlaps.timing <- ss[,c("tripIDa","tripIDb","region","overlap.prop")]
rm(ss,ss1,ss2,overlap.ydays,min.ydays,max.ydays,full.daylist,i)

### OVERLAP OF STOP-OVER HOME RANGES: 
## pairwise-comparison of all events in each region/season
############################################################
require(sf)
# create function to determine UTM zone per case
lonlat2UTM = function(lonlat) {
  utm = (floor((lonlat[1] + 180) / 6) %% 60) + 1
  if(lonlat[2] > 0) {
    utm + 32600
  } else{
    utm + 32700
  }
}

# calculate overlaps in HR per region/season and store results in csv
list_df <- split(stop.tracks, stop.tracks$region) #split example dataset by group factor

for (i in 1:length(list_df)){ #run a loop over the dataframes in the list
  ss2 <- list_df[[i]]
  
  loc <- ss2[, c("long","lat")]
  id <- ss2[,"tripID"]
  id <- as.data.frame(id)
  loc <- SpatialPointsDataFrame(loc,id)
  utmzone <- lonlat2UTM(c(median(ss2$long),median(ss2$lat)))
  
  proj4string(loc) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
  loc <- spTransform(loc, CRS(st_crs(utmzone)$proj4string))
  
  UD <- kernelUD(loc, id, h="href", same4all=TRUE, grid= 500, kern=c("bivnorm"), extent= 1)
  kern_over <- kerneloverlaphr(UD, meth="BA", percent= 90)
  overlaps <- as.data.frame(as.table(kern_over))
  colnames(overlaps) <- c("tripIDa","tripIDb","overlap")
  write.csv(overlaps, paste("./output - tables/temp/Overlaps_",unique(ss2$region),".csv",sep=""))
  rm(loc,id,utmzone,UD,kern_over,overlaps)
}

# read overlap estimates from stored csv files
overlaps.east.spring <- read.csv('./output - tables/temp/Overlaps_east-spring.csv')
overlaps.east.spring$region <- rep('East-Spring')

overlaps.east.autumn <- read.csv('./output - tables/temp/Overlaps_east-autumn.csv')
overlaps.east.autumn$region <- rep('East-Autumn')

overlaps.sahel.spring <- read.csv('./output - tables/temp/Overlaps_sahel-spring.csv')
overlaps.sahel.spring$region <- rep('Sahel-Spring')

overlaps.sahel.autumn <- read.csv('./output - tables/temp/Overlaps_sahel-autumn.csv')
overlaps.sahel.autumn$region <- rep('Sahel-Autumn')

overlaps.north.autumn <- read.csv('./output - tables/temp/Overlaps_north-autumn.csv')
overlaps.north.autumn$region <- rep('North-Autumn')

overlaps.north.spring <- read.csv('./output - tables/temp/Overlaps_north-spring.csv')
overlaps.north.spring$region <- rep('North-Spring')

overlap <- rbind(overlaps.east.spring,overlaps.sahel.spring,overlaps.sahel.autumn,overlaps.east.autumn,overlaps.north.autumn,overlaps.north.spring)
rm(overlaps.east.spring,overlaps.sahel.spring,overlaps.sahel.autumn,overlaps.east.autumn,overlaps.north.autumn,overlaps.north.spring)
# remove auto-comparisons of same stop-over events 
overlap <- overlap[which(overlap$tripIDa != overlap$tripIDb),c("tripIDa","tripIDb","region","overlap")]

### MERGE TEMPORAL AND SPATIAL OVERLAP METRICS 
############################################################
overlap <- merge(overlap,overlaps.timing,all.x=TRUE)
rm(overlaps.timing)

# create columns needed to compare overlaps within and between IDs
require(stringr)
overlap$deva <- as.data.frame(do.call("rbind",strsplit(overlap$tripIDa,"_",fixed=TRUE)))[,1]
overlap$devb <- as.data.frame(do.call("rbind",strsplit(overlap$tripIDb,"_",fixed=TRUE)))[,1]

overlap$region2 <- as.data.frame(do.call("rbind",strsplit(overlap$region,"-",fixed=TRUE)))[,1]
overlap$region2 <- as.factor(overlap$region2)
overlap$region2 <- factor(overlap$region2,levels=c("North","Sahel","East"))
overlap$trip <- as.data.frame(do.call("rbind",strsplit(overlap$region,"-",fixed=TRUE)))[,2]


### WITHIN-INDIVIDUAL SPATIAL AND TEMPORAL OVERLAP IN STOP-OVERS
############################################################
within.overlap <- overlap[which(overlap$deva == overlap$devb),]

within.overlap.perID <- within.overlap %>%
  group_by(deva,region2,trip) %>%
  summarize(n.comps = ifelse(length(unique(tripIDa))==3,3,ifelse(length(unique(tripIDa))==4,6,1)),
            overlap.mean = sum(unique(overlap))/n.comps,
            overlap.temp.mean = sum(unique(overlap.prop))/n.comps) %>%
  ungroup()

colnames(within.overlap.perID)[1] <- "dev"
within.overlap.perID <- merge(within.overlap.perID,n.trips[,c("dev","trip","region2","n.trips")],all.x=TRUE)
rm(within.overlap)

### BETWEEN-INDIVIDUAL SPATIAL AND TEMPORAL OVERLAP IN STOP-OVERS
############################################################
between.overlap <- overlap[which(overlap$deva != overlap$devb),]

between.overlap.perIDpair <- between.overlap %>%
  group_by(deva,devb,region2,trip) %>%
  summarize(overlap.mean = mean(overlap),
            overlap.temp.mean = mean(overlap.prop)) %>%
  ungroup()

between.overlap.perID <- between.overlap.perIDpair %>%
  group_by(deva,region2,trip) %>%
  summarize(overlap.mean = mean(overlap.mean),
            overlap.temp.mean = mean(overlap.temp.mean)) %>%
  ungroup()

colnames(between.overlap.perID)[1] <- "dev"
between.overlap.perID <- merge(between.overlap.perID,n.trips[,c("dev","trip","region2","n.trips")],all.x=TRUE)
rm(between.overlap.perIDpair,between.overlap)
rm(overlap)

### CREATE DF WITH SAMPLE SIZES FOR LABELLING GRAPHS
############################################################
# calculate number of birds that stopped-over on all trips, on at least 2 trips, or at least 1 trip 
# for each stop-over region
summary_tripnr <- n.trips %>% 
  group_by(region2,trip) %>% 
  summarise(count.alltrips = length(unique(dev[which(n.trips.with.stops == n.trips)])),
            count.2trips = length(unique(dev[which(n.trips.with.stops > 1)])),
            count.1trip = length(unique(dev[which(n.trips.with.stops > 0)])))

### PLOT SPATIAL OVERLAP STOP-OVERS 
### ### ### ### ### ### ### ### ### 
# Plot proportion of repeated trips with stop-overs per region/season
# extract ANOVA labels for group comparisons
n.trips$region <- as.factor(paste(n.trips$region2,n.trips$trip,sep="-"))
n.trips$prop.trips <- n.trips$n.trips.with.stops/n.trips$n.trips
m <- aov(prop.trips ~ region,data=n.trips)

# get Tukey HSD results
tukey <- as.data.frame(agricolae::HSD.test(m,"region",alpha=0.5, group=TRUE)$groups)
tukey$region <- rownames(tukey)
tukey$region2 <- as.data.frame(do.call("rbind",strsplit(tukey$region,"-",fixed=TRUE)))[,1]
tukey$trip <- as.data.frame(do.call("rbind",strsplit(tukey$region,"-",fixed=TRUE)))[,2]

# plot
p1 <- ggplot(data=n.trips,aes(x=region2,y=100*(n.trips.with.stops/n.trips),fill=trip,group=paste(region2,trip)))+
  #  geom_violin(position = position_dodge(width = 0.9),trim=TRUE,alpha=0.5,stat = "ydensity",scale="width",width=.9) +
  geom_boxplot(position = position_dodge(width = 0.9,preserve = "single"),width=.5,outlier.colour = "transparent",alpha=.5)+
  geom_point(aes(shape=as.factor(n.trips)),col='black',position = position_jitterdodge(seed = 1, dodge.width = 0.9, jitter.height=0, jitter.width=0.3),size=1.8,alpha=.6)+
  geom_text(data=summary_tripnr, aes(y=100*1.17, x=region2, col = trip, label=paste(count.1trip,"(",paste(count.2trips,count.alltrips,sep="/"),")",sep="")), position = position_dodge(width = 0.9)) +
  geom_text(data=tukey, aes(y=100*1.07, x=region2, label=groups), position = position_dodge(width = 0.9)) +
  scale_y_continuous(limits = c(0,100*1.22),breaks=seq(0,100*1,100*0.25)) +
  scale_fill_manual(values = c("Autumn"="orangered","Spring"="cornflowerblue")) +
  scale_colour_manual(values = c("Autumn"="orangered","Spring"="cornflowerblue")) +
  scale_shape_manual(values=c("2"=21,"3"=22,"4"=23))+
  ylab("Prop. of trips\nwith stop-overs [%]")+
  theme_classic()+
  theme(panel.border	    = element_blank(),
        strip.background	= element_rect(fill='white',colour='NA'),
        legend.position   = 'none',
        panel.grid.minor.y   = element_blank(),
        panel.grid.major.y   = element_blank(),
        panel.grid.minor.x   = element_blank(),
        panel.grid.major.x   = element_blank(),
        legend.title 	    = element_text(size=11),
        legend.text 	    = element_text(size=10),
        axis.text.y	      = element_text(size=10),
        axis.text.x	      = element_blank(),
        axis.title.y		    = element_text(size=11),
        axis.title.x		    = element_blank(),
        plot.margin       = unit(c(0.02,0.02,0.02,0.02),"cm"))


# plot between individual 'affinity' of stop-overs
# extract ANOVA labels for group comparisons
within.overlap.perID$region <- as.factor(paste(within.overlap.perID$region2,within.overlap.perID$trip,sep="-"))
m <- aov(overlap.mean ~ region,data=within.overlap.perID)

# get Tukey HSD results
tukey <- as.data.frame(agricolae::HSD.test(m,"region",alpha=0.5, group=TRUE)$groups)
tukey$region <- rownames(tukey)
tukey$region2 <- as.data.frame(do.call("rbind",strsplit(tukey$region,"-",fixed=TRUE)))[,1]
tukey$trip <- as.data.frame(do.call("rbind",strsplit(tukey$region,"-",fixed=TRUE)))[,2]

# plot
p2 <- ggplot(data=within.overlap.perID,aes(x=region2,y=overlap.mean,fill=trip,group=paste(region2,trip)))+
#  geom_violin(position = position_dodge(width = 0.9),trim=TRUE,alpha=0.5,stat = "ydensity",scale="width",width=.9) +
  geom_boxplot(position = position_dodge(width = 0.9,preserve = "single"),width=.5,outlier.colour = "transparent",alpha=.5)+
  geom_point(aes(shape=as.factor(n.trips)),position = position_jitterdodge(seed = 1, dodge.width = 0.9, jitter.height=0, jitter.width=0.3),col='black',size=1.8,alpha=.6)+
  geom_text(data=summary_tripnr, aes(y=1.17, x=region2, col = trip, label=count.2trips), position = position_dodge(width = 0.45)) +
  geom_text(data=tukey, aes(y=1.07, x=region2, label=groups), position = position_dodge(width = 0.9)) +
  scale_y_continuous(limits = c(0,1.22),breaks=seq(0,1,0.25)) +
  scale_fill_manual(values = c("Autumn"="orangered","Spring"="cornflowerblue")) +
  scale_colour_manual(values = c("Autumn"="orangered","Spring"="cornflowerblue")) +
  scale_shape_manual(values=c("2"=21,"3"=22,"4"=23))+
  ylab("Within-individual\nspatial similarity [BA]")+
  theme_classic()+
  theme(panel.border	    = element_blank(),
        strip.background	= element_rect(fill='white',colour='NA'),
        legend.position   = 'none',
        panel.grid.minor.y   = element_blank(),
        panel.grid.major.y   = element_blank(),
        panel.grid.minor.x   = element_blank(),
        panel.grid.major.x   = element_blank(),
        legend.title 	    = element_text(size=11),
        legend.text 	    = element_text(size=10),
        axis.text.y	      = element_text(size=10),
        axis.text.x		      = element_text(size=11,face='bold'),
        axis.title.y		    = element_text(size=11),
        axis.title.x		    = element_blank(),
        plot.margin       = unit(c(0.02,0.02,0.02,0.02),"cm"))

# plot between individual 'affinity' of stop-overs
# extract ANOVA labels for group comparisons
between.overlap.perID$region <- as.factor(paste(between.overlap.perID$region2,between.overlap.perID$trip,sep="-"))
m <- aov(overlap.mean ~ region,data=between.overlap.perID)

# get Tukey HSD results
tukey <- as.data.frame(agricolae::HSD.test(m,"region",alpha=0.5, group=TRUE)$groups)
tukey$region <- rownames(tukey)
tukey$region2 <- as.data.frame(do.call("rbind",strsplit(tukey$region,"-",fixed=TRUE)))[,1]
tukey$trip <- as.data.frame(do.call("rbind",strsplit(tukey$region,"-",fixed=TRUE)))[,2]

#plot
p3 <- ggplot(data=between.overlap.perID,aes(x=region2,y=overlap.mean,fill=trip,group=paste(region2,trip)))+ 
  #  geom_violin(position = position_dodge(width = 0.9),trim=TRUE,alpha=0.5,stat = "ydensity",scale="width",width=.9) +
  geom_boxplot(position = position_dodge(width = 0.9,preserve = "single"),width=.5,outlier.colour = "transparent",alpha=.5)+
  geom_point(aes(shape=as.factor(n.trips)),position = position_jitterdodge(seed = 1, dodge.width = 0.9, jitter.height=0, jitter.width=0.3),col='black',size=1.8,alpha=.6)+
  geom_text(data=summary_tripnr, aes(y=1.17, x=region2, col = trip, label=count.1trip), position = position_dodge(width = 0.9)) +
  geom_text(data=tukey, aes(y=1.07, x=region2, label=groups), position = position_dodge(width = 0.9)) +
  scale_y_continuous(limits = c(0,1.22),breaks=seq(0,1,0.25)) +
  scale_fill_manual(values = c("Autumn"="orangered","Spring"="cornflowerblue")) +
  scale_colour_manual(values = c("Autumn"="orangered","Spring"="cornflowerblue")) +
  scale_shape_manual(values=c("2"=21,"3"=22,"4"=23))+
  ylab("Between-individual\nspatial similarity [BA]")+
  theme_classic()+
  theme(panel.border	    = element_blank(),
        strip.background	= element_rect(fill='white',colour='NA'),
        legend.position   = 'none',
        panel.grid.minor.y   = element_blank(),
        panel.grid.major.y   = element_blank(),
        panel.grid.minor.x   = element_blank(),
        panel.grid.major.x   = element_blank(),
        legend.title 	    = element_text(size=11),
        legend.text 	    = element_text(size=10),
        axis.text.y		      = element_text(size=10),
        axis.text.x		      = element_blank(),
        axis.title.y		    = element_text(size=11),
        axis.title.x		    = element_blank(),
        plot.margin       = unit(c(0.02,0.02,0.02,0.02),"cm"))

(pcomp<- cowplot::plot_grid(p1,p3,p2,ncol=1,align="v",axis="l",labels=c("a","b","c")))

# extract the legend from one of the plots
legend <- get_legend(
  # create some space to the left of the legend
  p1 + theme(legend.position = "bottom",
             legend.direction = "horizontal",
             legend.title 	    = element_text(size=10,face='bold'),
             legend.text 	    = element_text(size=9))+
    guides(fill = guide_legend("Season",title.position = "top"),
           shape = guide_legend("Number of trips per bird",title.position = "top",override.aes = c(size=2)),
           col = FALSE)
)

# add the legend to the row we made earlier. 
(pfin <- cowplot::plot_grid(pcomp, legend, ncol=1,rel_heights = c(1, .1)))

ggsave(plot=pfin,filename=paste('./output - graphs/FigSX_StopOverConsistency-Spatial',ifelse(length(selected)>1,'both',unique(selected)),'.pdf',sep=''),width=7,height=8.5)


### PLOT TEMPORAL VARIANCE STOP-OVERS 
### ### ### ### ### ### ### ### ### 
# Plot  mean stop-over duration
# extract ANOVA labels for group comparisons
m <- aov(within.mean.regional.days ~ region,data=n.trips)

# get Tukey HSD results
tukey <- as.data.frame(agricolae::HSD.test(m,"region",alpha=0.5, group=TRUE)$groups)
tukey$region <- rownames(tukey)
tukey$region2 <- as.data.frame(do.call("rbind",strsplit(tukey$region,"-",fixed=TRUE)))[,1]
tukey$trip <- as.data.frame(do.call("rbind",strsplit(tukey$region,"-",fixed=TRUE)))[,2]

#plot
p1 <- ggplot(data=n.trips,aes(x=region2,y=within.mean.regional.days,fill=trip,group=paste(region2,trip)))+
  #  geom_violin(position = position_dodge(width = 0.9),trim=TRUE,alpha=0.5,stat = "ydensity",scale="width",width=.9) +
  geom_boxplot(position = position_dodge(width = 0.9,preserve = "single"),width=.5,outlier.colour = "transparent",alpha=.5)+
  geom_point(aes(shape=as.factor(n.trips)),col='black',position = position_jitterdodge(seed = 1, dodge.width = 0.9, jitter.height=0, jitter.width=0.3),size=1.8,alpha=.6)+
  geom_text(data=summary_tripnr, aes(y=70, x=region2, col = trip, label=paste(count.1trip,"(",paste(count.2trips,count.alltrips,sep="/"),")",sep="")), position = position_dodge(width = 0.9)) +
  geom_text(data=tukey, aes(y=65, x=region2, label=groups), position = position_dodge(width = 0.9)) +
  theme_bw() +
  scale_fill_manual(values = c("Autumn"="orangered","Spring"="cornflowerblue")) +
  scale_colour_manual(values = c("Autumn"="orangered","Spring"="cornflowerblue")) +
  scale_shape_manual(values=c("2"=21,"3"=22,"4"=23))+
  ylab("Individual mean\nstop-over period [days]")+
  theme_classic()+
  theme(panel.border	    = element_blank(),
        strip.background	= element_rect(fill='white',colour='NA'),
        legend.position   = 'none',
        panel.grid.minor.y   = element_blank(),
        panel.grid.major.y   = element_blank(),
        panel.grid.minor.x   = element_blank(),
        panel.grid.major.x   = element_blank(),
        legend.title 	    = element_text(size=11),
        legend.text 	    = element_text(size=10),
        axis.text.y	      = element_text(size=10),
        axis.text.x	      = element_blank(),
        axis.title.y		    = element_text(size=11),
        axis.title.x		    = element_blank(),
        plot.margin       = unit(c(0.02,0.02,0.02,0.02),"cm"))

# Plot  within-individual overlap in stop-over periods
# extract ANOVA labels for group comparisons
m <- aov(overlap.temp.mean ~ region,data=within.overlap.perID)

# get Tukey HSD results
tukey <- as.data.frame(agricolae::HSD.test(m,"region",alpha=0.5, group=TRUE)$groups)
tukey$region <- rownames(tukey)
tukey$region2 <- as.data.frame(do.call("rbind",strsplit(tukey$region,"-",fixed=TRUE)))[,1]
tukey$trip <- as.data.frame(do.call("rbind",strsplit(tukey$region,"-",fixed=TRUE)))[,2]

#plot
p2 <- ggplot(data=within.overlap.perID,aes(x=region2,y=100*overlap.temp.mean,fill=trip,group=paste(region2,trip)))+
  #  geom_violin(position = position_dodge(width = 0.9),trim=TRUE,alpha=0.5,stat = "ydensity",scale="width",width=.9) +
  geom_boxplot(position = position_dodge(width = 0.9,preserve = "single"),width=.5,outlier.colour = "transparent",alpha=.5)+
  #geom_violindot()+
  geom_point(aes(shape=as.factor(n.trips)),col='black',position = position_jitterdodge(seed = 1, dodge.width = 0.9, jitter.height=0, jitter.width=0.3),size=1.8,alpha=.6)+
  geom_text(data=summary_tripnr, aes(y=100*1.17, x=region2, col = trip, label=count.2trips), position = position_dodge(width = 0.9)) +
  geom_text(data=tukey, aes(y=100*1.07, x=region2, label=groups), position = position_dodge(width = 0.9)) +
  theme_bw() +
  scale_y_continuous(limits = c(0,100*1.22),breaks=seq(0,100*1,100*0.25)) +
  scale_fill_manual(values = c("Autumn"="orangered","Spring"="cornflowerblue")) +
  scale_colour_manual(values = c("Autumn"="orangered","Spring"="cornflowerblue")) +
  scale_shape_manual(values=c("2"=21,"3"=22,"4"=23))+
  ylab("Within-individual\ntemporal overlap [%]")+
  theme_classic()+
  theme(panel.border	    = element_blank(),
        strip.background	= element_rect(fill='white',colour='NA'),
        legend.position   = 'none',
        panel.grid.minor.y   = element_blank(),
        panel.grid.major.y   = element_blank(),
        panel.grid.minor.x   = element_blank(),
        panel.grid.major.x   = element_blank(),
        legend.title 	    = element_text(size=11),
        legend.text 	    = element_text(size=10),
        axis.text.y	      = element_text(size=10),
        axis.text.x	      = element_text(size=11,face='bold'),
        axis.title.y		    = element_text(size=11),
        axis.title.x		    = element_blank(),
        plot.margin       = unit(c(0.02,0.02,0.02,0.02),"cm"))

# Plot  between-individual overlap in stop-over periods
# extract ANOVA labels for group comparisons
m <- aov(overlap.temp.mean ~ region,data=between.overlap.perID)

# get Tukey HSD results
tukey <- as.data.frame(agricolae::HSD.test(m,"region",alpha=0.5, group=TRUE)$groups)
tukey$region <- rownames(tukey)
tukey$region2 <- as.data.frame(do.call("rbind",strsplit(tukey$region,"-",fixed=TRUE)))[,1]
tukey$trip <- as.data.frame(do.call("rbind",strsplit(tukey$region,"-",fixed=TRUE)))[,2]

#plot
p3 <- ggplot(data=between.overlap.perID,aes(x=region2,y=100*overlap.temp.mean,fill=trip,group=paste(region2,trip)))+
  #  geom_violin(position = position_dodge(width = 0.9),trim=TRUE,alpha=0.5,stat = "ydensity",scale="width",width=.9) +
  geom_boxplot(position = position_dodge(width = 0.9,preserve = "single"),width=.5,outlier.colour = "transparent",alpha=.5)+
  geom_point(aes(shape=as.factor(n.trips)),col='black',position = position_jitterdodge(seed = 1, dodge.width = 0.9, jitter.height=0, jitter.width=0.3),size=1.8,alpha=.6)+
  geom_text(data=summary_tripnr, aes(y=100*1.17, x=region2, col = trip, label=count.1trip), position = position_dodge(width = 0.9)) +
  geom_text(data=tukey, aes(y=100*1.07, x=region2, label=groups), position = position_dodge(width = 0.9)) +
  theme_bw() +
  scale_y_continuous(limits = c(0,100*1.22),breaks=seq(0,100*1,100*0.25)) +
  scale_fill_manual(values = c("Autumn"="orangered","Spring"="cornflowerblue")) +
  scale_colour_manual(values = c("Autumn"="orangered","Spring"="cornflowerblue")) +
  scale_shape_manual(values=c("2"=21,"3"=22,"4"=23))+
  ylab("Between-individual\ntemporal overlap [%]")+
  theme_classic()+
  theme(panel.border	    = element_blank(),
        strip.background	= element_rect(fill='white',colour='NA'),
        legend.position   = 'none',
        panel.grid.minor.y   = element_blank(),
        panel.grid.major.y   = element_blank(),
        panel.grid.minor.x   = element_blank(),
        panel.grid.major.x   = element_blank(),
        legend.title 	    = element_text(size=11),
        legend.text 	    = element_text(size=10),
        axis.text.y      = element_text(size=10),
        axis.text.x      = element_blank(),
        axis.title.y		    = element_text(size=11),
        axis.title.x		    = element_blank(),
        plot.margin       = unit(c(0.02,0.02,0.02,0.02),"cm"))

(pcomp<- cowplot::plot_grid(p1,p3,p2,ncol=1,align = "v", axis="lb",labels=c("a","b","c")))

# extract the legend from one of the plots
legend <- get_legend(
  # create some space to the left of the legend
  p2 + theme(legend.position = "bottom",
             legend.direction = "horizontal",
             legend.title 	    = element_text(size=10,face='bold'),
             legend.text 	    = element_text(size=9))+
    guides(fill = guide_legend("Season",title.position = "top"),
           shape = guide_legend("Number of trips per bird",title.position = "top",override.aes = c(size=2)),
           col = FALSE)
)

# add the legend to the row we made earlier. 
pfin <- cowplot::plot_grid(pcomp, legend, ncol=1,rel_heights = c(1, .1))

ggsave(plot=pfin,filename=paste('./output - graphs/FigSX_StopOverConsistency-Temporal',ifelse(length(selected)>1,'both',unique(selected)),'.pdf',sep=''),width=7,height=8.5)

write.csv(between.overlap.perID,paste('./output - tables/stopovers-between-',unique(selected),'.csv',sep=""))
write.csv(within.overlap.perID,paste('./output - tables/stopovers-within-',unique(selected),'.csv',sep=""))

### SUMMARY STATS PER REGION PER SEX 
### ### ### ### ### ### ### ### ### 
between.overlap.males <- read.csv('./output - tables/stopovers-between-male.csv') 
between.overlap.males$sex <- rep('male')
between.overlap.females <- read.csv('./output - tables/stopovers-between-female.csv') 
between.overlap.females$sex <- rep('female')
within.overlap.males <- read.csv('./output - tables/stopovers-within-male.csv') 
within.overlap.males$sex <- rep('male')
within.overlap.females <- read.csv('./output - tables/stopovers-within-female.csv') 
within.overlap.females$sex <- rep('female')

between.overlap <- rbind(between.overlap.males,between.overlap.females)
within.overlap <- rbind(within.overlap.males,within.overlap.females)

similarity.between.per.sex <- between.overlap %>%
  group_by(trip,region2) %>%
  t_test(overlap.mean ~ sex,paired=FALSE) %>%
  adjust_pvalue() %>%
  add_significance("p.adj")
similarity.between.per.sex <- similarity.between.per.sex[,c("region2","trip","n1","n2","statistic","df","p")]
similarity.between.per.sex$resp <- rep("similarity-between")

overlap.between.per.sex <- between.overlap %>%
  group_by(trip,region2) %>%
  t_test(overlap.temp.mean ~ sex, paired = FALSE) %>%
  adjust_pvalue() %>%
  add_significance("p.adj")
overlap.between.per.sex <- overlap.between.per.sex[,c("region2","trip","n1","n2","statistic","df","p")]
overlap.between.per.sex$resp <- rep("overlap-between")

# within overlap can only be tested if there 
similarity.within.per.sex <- within.overlap %>%
  filter(region != 'North-Autumn') %>%
  group_by(trip,region2) %>%
  t_test(overlap.mean ~ sex, paired = FALSE) %>%
  adjust_pvalue() %>%
  add_significance("p.adj")
similarity.within.per.sex <- similarity.within.per.sex[,c("region2","trip","n1","n2","statistic","df","p")]
similarity.within.per.sex$resp <- rep("similarity-within")

overlap.within.per.sex <- within.overlap %>%
  filter(region != 'North-Autumn') %>%
  group_by(trip,region2) %>%
  t_test(overlap.temp.mean ~ sex, paired=FALSE) %>%
  adjust_pvalue() %>%
  add_significance("p.adj")
overlap.within.per.sex <- overlap.within.per.sex[,c("region2","trip","n1","n2","statistic","df","p")]
overlap.within.per.sex$resp <- rep("overlap-within")

overlaps <- rbind(similarity.between.per.sex,overlap.between.per.sex,similarity.within.per.sex,overlap.within.per.sex)
write.csv(overlaps,'./output - tables/Stopovers-between-sexes-overlap-similarity.csv')
##
  ##summarize(n.birds = length(unique(dev)),
  #          mean.n.trips = mean(n.trips),
  #          mean.BA.between = mean(overlap.mean),
  #          mean.overlap.between = mean(overlap.temp.mean)) %>%
  #ungroup()

summary.within.per.region <- within.overlap.perID %>%
  group_by(trip,region2) %>%
  summarize(n.birds = length(unique(dev)),
            mean.n.trips = mean(n.trips),
            mean.BA.between = mean(overlap.mean),
            mean.overlap.between = mean(overlap.temp.mean)) %>%
  ungroup()

### remove
rm(p1,p2,p3,pfin,pcomp,legend)
rm(summary_tripnr,data_summary,within.overlap.perID,between.overlap.perID,within.overlap,between.overlap)
rm(basemap.north.aut,basemap.sahel.aut,basemap.east.aut,basemap.north.spr,basemap.sahel.spr,basemap.horn.spr)


