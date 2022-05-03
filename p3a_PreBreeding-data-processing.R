prebreed.fin <- read.csv("./Data/prebreeding-data-full_v20210520.csv")

prebreed.fin$trip <- ifelse(prebreed.fin$trip == "out","Autumn","Spring")

prebreed.fin$cycle <- paste(prebreed.fin$yr-1,prebreed.fin$yr,sep="-")
prebreed.fin$cycle.start <- paste(prebreed.fin$yr-1,"-07-01 00:00:01",sep="")
prebreed.fin$cycle.start <- as.POSIXct(strptime(prebreed.fin$cycle.start, format="%Y-%m-%d %H:%M:%S"), tz='UTC')
prebreed.fin$cycle.time <- as.numeric(difftime(prebreed.fin$dt,prebreed.fin$cycle.start,units="days"))

prebreed.fin <- subset(prebreed.fin,prebreed.fin$cycle != "2012-2013")

prebreed.fin <- merge(prebreed.fin,ind.summ[,c("dev","trip","n.trips")],all.x=TRUE)
prebreed.fin <- merge(prebreed.fin,meta[,c("dev","sex")],all.x=TRUE)

prebreed.fin <- subset(prebreed.fin,prebreed.fin$n.trips > 1)
prebreed.fin <- prebreed.fin[order(prebreed.fin$dev,prebreed.fin$dt),]

prebreed.fin$tripID <- paste(prebreed.fin$dev,prebreed.fin$yr,"return",sep="_")

### RESAMPLE PREBREEDING DATA
##########################################
## read resample function
source('Rfunction_resampling_movement.R')
prebreed_resample <- moveTimeSample(prebreed.fin,prebreed.fin$dt,prebreed.fin$tripID,60,10,subset=TRUE)

# Check dresampling by counting nr of observations per day
ul <- function(x) length(unique(x))
obsperday <- aggregate(prebreed_resample$dt, by=list(prebreed_resample$indday), FUN=ul)
colnames(obsperday)[1:2] <- c("indday","daily.obs")
hist(obsperday$daily.obs)

prebreed.fin <- prebreed_resample
rm(prebreed_resample)

### CALCULATE DAILY DISTANCES, DEFINE STOP-DAYS
##########################################
prebreed.dists <- prebreed.fin %>%
  group_by(indday) %>%
  summarize(indday.start = head(dt, 1),
            indday.end = tail(dt, 1),
            indday.start.lat = head(lat,1),
            indday.end.lat = tail(lat,1),
            indday.start.long = head(long,1),
            indday.end.long = tail(long,1),
            daily.dist = deg.dist(long1=indday.start.long,lat1=indday.start.lat,long2=indday.end.long,lat2=indday.end.lat)*1000)  %>%
  ungroup()

prebreed.fin <- merge(prebreed.fin,prebreed.dists[,c("indday","daily.dist")],all.x=TRUE)
prebreed.fin$travel <- ifelse(prebreed.fin$daily.dist > 100000,1,0)

### Segment track in travel vs stationary parts
################################################################
prebreed.fin <- prebreed.fin[order(prebreed.fin$dev,prebreed.fin$dt),]
prebreed.fin$segment <- as.numeric(c(0,cumsum(as.logical(diff(as.numeric(as.factor(paste(prebreed.fin$tripID,prebreed.fin$travel))))))))
prebreed.fin$segment <- sprintf("%04d",prebreed.fin$segment)
prebreed.fin$segment <- paste(prebreed.fin$dev,prebreed.fin$segment,sep="_")

seglengths <- prebreed.fin %>%
  group_by(segment) %>%
  summarize(segment.length = length(unique(indday))) 

prebreed.fin <- merge(prebreed.fin,seglengths,all.x=TRUE)

### Determine stop-over events
################################################################
### Generate table with duration and location of stop-overs (incl premigr and winter and breeding stages)
stat <- subset(prebreed.fin,prebreed.fin$travel == 0)

# calculate mean lat per stage
mean.lats <- aggregate(stat$lat,by=list(stat$segment),FUN="median")
colnames(mean.lats) <- c("segment","st.lat")

# calculate mean long per stage
mean.longs <- aggregate(stat$long,by=list(stat$segment),FUN="median")
colnames(mean.longs) <- c("segment","st.long")

# calculate duration stage
calcdur <- function(x) difftime(max(x),min(x),units="days")
st.durs <- aggregate(stat$dt,by=list(stat$segment),FUN="calcdur")
colnames(st.durs) <- c("segment","st.dur")
st.durs$st.dur <- as.numeric(st.durs$st.dur)

# get first date of each stop
st.first <- stat %>%
  group_by(segment) %>%
  summarize(st.start = min(dt, na.rm = TRUE),
            st.end = max(dt, na.rm = TRUE)) 
st.first <- as.data.frame(st.first)


prebreed.stops <- merge(mean.lats,mean.longs,all.x=T)
prebreed.stops <- merge(prebreed.stops,st.durs,all.x=T)
prebreed.stops <- merge(prebreed.stops,st.first,all.x=TRUE)

prebreed.stops <- merge(prebreed.stops,unique(stat[,c("segment","dev","tripID","trip","yr")]),all.x=T)

ref <- unique(meta[which(meta$dev %in% unique(data$dev)),c("colony","colony.lat","colony.long")])
prebreed.stops <- merge(prebreed.stops,unique(meta[,c("dev","colony","sex")]),all.x=TRUE)
prebreed.stops$dist.to.colony <- deg.dist(long1=ref[[3]],lat1=ref[[2]],long2=prebreed.stops$st.long,lat2=prebreed.stops$st.lat)*1000
#segs <- subset(segs,segs$dist.to.colony < 8000000 & segs$dist.to.colony >= 1700000)

prebreed.stops <- merge(prebreed.stops,unique(prebreed.fin[,c("tripID","segment","cycle","cycle.start")]))
prebreed.stops$cycle.start <- as.POSIXct(strptime(prebreed.stops$cycle.start, format="%Y-%m-%d %H:%M:%S"), tz='UTC')

prebreed.stops$cycle.time <- as.numeric(difftime(prebreed.stops$st.start,prebreed.stops$cycle.start,units='days'))

rm(seglengths,prebreed.dists)
rm(mean.lats,mean.longs,st.durs,stat,st.first)

### EXPLORATORY MAPS PREBREEDING  
#######################################
longlimits <- c(min(prebreed.fin$long)-0.5,max(prebreed.fin$long)+0.5)
latlimits <- c(min(prebreed.fin$lat)-0.5,max(prebreed.fin$lat)+0.5)

# create basemap
basemap <- ggplot()+
  ## Add biomes (fill) and elevation (alpha)
  geom_raster(data=hdf3[which(is.na(hdf3$biome2)==FALSE & hdf3$trip == "Spring"),],mapping=aes(long,lat,alpha=alt,fill=biome2))+
  scale_alpha_continuous(name="Elevation",range=c(0.35,1))+
  scale_fill_manual(name="Biome",values=c("desert"="wheat4","forest"="olivedrab4","other"="grey30"))+
  ## Add borders and lakes
  #geom_polygon(data=political,aes(long,lat,group=paste(country,piece)),col='transparent',fill='grey90',alpha=.1)+	
  geom_polygon(data=lakes, aes(x = long, y = lat, group = group), fill = 'white') +
  ## Add colony location
  geom_point(data=ref,aes(x=ale.long,y=ale.lat),size=2.1,col='red',shape=3)+
 ## Layout map
  coord_quickmap(xlim=longlimits,ylim=latlimits,expand=FALSE) + 
  scale_x_continuous(breaks = ewbrks, labels = ewlbls, expand = c(0, 0)) +
  scale_y_continuous(breaks = nsbrks, labels = nslbls, expand = c(0, 0)) +
  theme_bw() + 
  # Layout text items
  theme(panel.border	    = element_blank(),
        strip.background	= element_rect(fill='white',colour='NA'),
        strip.text        = element_text(size=12,face='bold'),
        legend.position   = "bottom",
        legend.direction  = 'horizontal',
        legend.spacing.y = unit (0.1, "cm"),
        legend.box.margin = margin(t=0.02,r=0,b=0,l=0,unit="cm"),
        legend.title 	    = element_text(size=9,face='bold'),
        legend.text 	    = element_text(size=9),
        panel.grid        = element_blank(),
        axis.text		      = element_text(size=10),
        #     axis.title		    = element_text(size=11,face='bold').
        axis.title        = element_blank())+
  # Add white box bottom right and lay-out legend
  #  annotate(geom="rect",xmin=longlimits[1],xmax=7.2,ymin=latlimits[1],ymax=7.1,col='black',fill='white')+
  guides(colour= guide_legend(order =1,nrow =3,title.position='top'),
         size = guide_legend(order =2,nrow =2,title.position='top'),
         fill=guide_legend(order=3,nrow =3,title.position='top',override.aes= list(alpha = .4)),
         alpha=guide_legend(order = 4,title.position='top',nrow=3,override.aes=list(fill='grey30')))

# HIGHLIGHT SELECTION OF BIRDS
selection <- c("B1011","B1014","B2453","B2400","B2051")  
prebreed.fin <- prebreed.fin[order(prebreed.fin$dev,prebreed.fin$dt),]

p1 <- basemap + 
  geom_path(data=prebreed.fin[which(!(prebreed.fin$dev %in% selection)),],aes(x=long,y=lat,group=paste(dev,yr,trip)),col='grey20',size=.2,alpha=.75)+
  geom_path(data=prebreed.fin[which(prebreed.fin$dev %in% selection),],aes(x=long,y=lat,col=dev,group=paste(dev,yr,trip)),size=.5,alpha=.8)+
  scale_colour_viridis_d(name="Bird ID",option="magma")
#ggsave(p1,filename='./output - maps/FigSx_Prebreeding-per-ID.tiff',dpi=300,width=6,height=5)

p2 <- basemap + 
  geom_path(data=prebreed.fin,aes(x=long,y=lat,col=sex,group=paste(dev,yr,trip)),size=.5,alpha=.8)+
  scale_colour_manual(name="Sex",values=c("male"="darkorange","female"="brown"))
#ggsave(p2,filename='./output - maps/FigSx_Prebreeding-per-sex.tiff',dpi=300,width=6,height=5)

p3 <- basemap + 
  geom_path(data=prebreed.fin[which(prebreed.fin$dev %in% selection),],aes(x=long,y=lat,col=factor(yr),group=paste(dev,yr,trip)),size=.5,alpha=.8)+
  scale_colour_viridis_d(name="Year",option="magma")+
  facet_wrap(~sex+dev,ncol=3) 
#ggsave(p3,filename='./output - maps/FigSx_Prebreeding-per-ID-per-yr.tiff',dpi=300,width=10,height=7.5)

p4 <- basemap + 
  geom_path(data=prebreed.fin,aes(x=long,y=lat,col=dev,group=paste(dev,yr,trip)),size=.5,alpha=.8)+
  scale_colour_viridis_d(name="Year",option="magma")+
  facet_wrap(~sex,ncol=2) 
#ggsave(p4,filename='./output - maps/FigSx_Prebreeding-per-ID-per-sex.tiff',dpi=300,width=10,height=7.5)


ss <- subset(prebreed.fin,prebreed.fin$sex == 'male')

p5 <- basemap + 
  geom_path(data=ss,aes(x=long,y=lat,col=factor(yr),group=paste(dev,yr,trip)),size=.5,alpha=.8)+
  scale_colour_manual(name="Year",values=c(c("2013"="black","2014"="brown","2015"="purple","2016"="yellow","2017"="red","2018"="cornflowerblue","2019"="orange","2020"="olivedrab1")))+
  facet_wrap(~dev,ncol=3) 
#ggsave(p5,filename='./output - maps/FigSx_Prebreeding-per-yr-per-ID.tiff',dpi=300,width=10,height=10)


rm(ss,p1,p2,p3,p4,p5,ss,)
