### Read packages 
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage
packages <- c("rptR","ggthemes","gridExtra","geosphere","rgeos","ggtext","tgp","arrayhelpers","cowplot","tmap")
ipak(packages)

### Prep route data: 
### ### ### ### ### 
# segments per 100km distance to colony
data$i <- round(data$dist.to.colony/100000)
data$i <- sprintf("%04d",data$i)
data <- data[order(data$dev,data$dt),]

# calculate nr of trips per ID per season
data <- merge(data,ind.summ[,c("dev","trip","n.trips")],all.x=TRUE)

# change season labels
data$trip <- ifelse(data$trip == "out","Autumn","Spring")
ind.summ$trip <- ifelse(ind.summ$trip == "out","Autumn","Spring")
trip.summary$trip <- ifelse(trip.summary$trip == "out","Autumn","Spring")

# remove tracks <100 km from colony
data <- subset(data,data$dist.to.colony > 100000)

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

### Extract env layers for mapping
#####################################
## Set limits for geographical domain
longlimits <- c(min(data$long)-0.5,max(data$long)+0.5)
latlimits <- c(min(data$lat)-0.5,max(data$lat)+0.5)

# center point colonies
ref <- unique(meta[which(meta$dev %in% unique(data$dev)),c("colony","colony.lat","colony.long")])

# ref colony
ale.long <- ref$colony.long[1]
ale.lat <- ref$colony.lat[1]

# create buffers at 1000km intervals from colony for mapping
pts <- data.frame(lon = ref$colony.long,lat=ref$colony.lat)

make_GeodesicBuffer <- function(pts, width) {
  
  # A) Construct buffers as points at given distance and bearing ---------------
  
  dg <- seq(from = 0, to = 360, by = 5)
  
  # Construct equidistant points defining circle shapes (the "buffer points")
  buff.XY <- geosphere::destPoint(p = pts, 
                                  b = rep(dg, each = length(pts)), 
                                  d = width)
  
  # B) Make SpatialPolygons -------------------------------------------------
  
  # Group (split) "buffer points" by id
  buff.XY <- as.data.frame(buff.XY)
  id  <- rep(1:dim(pts)[1], times = length(dg))
  lst <- split(buff.XY, id)
  
  # Make SpatialPolygons out of the list of coordinates
  poly   <- lapply(lst, sp::Polygon, hole = FALSE)
  polys  <- lapply(list(poly), sp::Polygons, ID = NA)
  spolys <- sp::SpatialPolygons(Srl = polys, 
                                proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
  # Disaggregate (split in unique polygons)
  spolys <- sp::disaggregate(spolys)
  return(spolys)
}

buf_1000km <- make_GeodesicBuffer(as.matrix(pts), width = 1000*10^3)
buf_2000km <- make_GeodesicBuffer(as.matrix(pts), width = 2000*10^3)
buf_3000km <- make_GeodesicBuffer(as.matrix(pts), width = 3000*10^3)
buf_4000km <- make_GeodesicBuffer(as.matrix(pts), width = 4000*10^3)
buf_5000km <- make_GeodesicBuffer(as.matrix(pts), width = 5000*10^3)
buf_6000km <- make_GeodesicBuffer(as.matrix(pts), width = 6000*10^3)
buf_7000km <- make_GeodesicBuffer(as.matrix(pts), width = 7000*10^3)
buf_8000km <- make_GeodesicBuffer(as.matrix(pts), width = 8000*10^3)

## Read lakes
lakes <- shapefile("~/Documents/1. Environmental data/ne_50m_lakes/ne_50m_lakes")

## Read biomes
biomes <- readOGR("~/Documents/1. Environmental data/BIOMES/wwf_terr_ecos.shp")

### Read NDVI maps
ndvi_autumn <- raster("~/Documents/1. Environmental data/ndvi/Greenest_pixel_composite_autumn-fullb.tif")
ndvi_spring <- raster("~/Documents/1. Environmental data/ndvi/Greenest_pixel_composite_spring-fullb.tif")

ndvi_aut = projectRaster(ndvi_autumn, crs = proj4string(lakes), method = "bilinear")
rm(ndvi_autumn)
ndvi_spr = projectRaster(ndvi_spring, crs = proj4string(lakes), method = "bilinear")
rm(ndvi_spring)

### Read DEM
dem <- raster('~/Documents/1. Environmental data/alt_30s_bil/alt.bil')
dem2 <- crop(dem, extent(longlimits[1],longlimits[2],latlimits[1],latlimits[2]))
dem3 <- aggregate(dem2,5,FUN='mean')
hdf2 <- rasterToPoints(dem3)
hdf2 <- data.frame(hdf2)
colnames(hdf2) <- c("long","lat","alt")
rm(dem,dem2,dem3)

# extract biome value for each raster cell in DEM 
pts <- SpatialPoints(cbind(hdf2$long, hdf2$lat), 
                     proj4string = CRS(proj4string(biomes)))
hdf2$biome <- sp::over(pts, biomes[,c("BIOME")])
hdf2$biome <- ifelse(is.na(hdf2$biome)==TRUE,NA,
                     ifelse(hdf2$biome == 13,'desert',
                            ifelse(hdf2$biome == 1,'forest',
                                  # ifelse(hdf2$biome == 2,'forest',
                                          ifelse(hdf2$biome == 14,'forest','other'))))#)

hdf2$ndvi.aut <- raster::extract(ndvi_aut,pts)
hdf2$ndvi.spr <- raster::extract(ndvi_spr,pts)

hdf2$biome2 <- ifelse(hdf2$biome %in% c("other") & hdf2$ndvi.aut < 0.25 & hdf2$ndvi.spr < 0.25 & hdf2$lat > 8,"desert",
                      ifelse(hdf2$biome %in% c("other") & (hdf2$ndvi.aut < 0.25 | hdf2$ndvi.spr < 0.25) & hdf2$lat > 8,"desert - seasonal",hdf2$biome))

# create another DEM split per season
hdf.aut <- hdf2[,c("long","lat","alt","biome",'ndvi.aut')]
hdf.spr <- hdf2[,c("long","lat","alt","biome",'ndvi.spr')]
#write.csv(hdf.aut,'DEM_biome-annotated-autumn_v20210523.csv')
#write.csv(hdf.spr,'DEM_biome-annotated-spring_v20210523.csv')

colnames(hdf.aut)[5] <- "ndvi"
colnames(hdf.spr)[5] <- "ndvi"
hdf.aut$trip <- rep("Autumn")
hdf.spr$trip <- rep("Spring")

hdf3 <- rbind(hdf.aut,hdf.spr)
hdf3$biome2 <- ifelse(hdf3$biome %in% c("other") & hdf3$ndvi < 0.25 & hdf3$lat > 8,"desert",hdf3$biome)
rm(hdf.aut,hdf.spr)

# country boundaries
countries <- readOGR("~/Documents/1. Environmental data/ne_50m_admin_0_countries/ne_50m_admin_0_countries.shp")
countries <- countries[countries$continent %in% c("Africa") | 
                         countries$admin %in% c("Spain","Italy","Greece","Portugal","Malta","Iran",
                                                "Northern Cyprus","Turkey","Syria","Lebanon","Jordan","Iraq",
                                                "Israel","Palestine","Yemen","Saudi Arabia","Kuwait","United Arab Emirates"),]

political <- fortify(countries, region = "admin")
political$country <- political$id
political$group <- ifelse(political$country %in% unique(data$country) & political$piece == 1,1,0)
rm(countries)

## get falcon picture to include in paper
#get_png <- function(filename) {
#  grid::rasterGrob(png::readPNG(filename), interpolate = TRUE)
#}

#img <- get_png("falcon.png")


### Annotate data with biome and ndvi data
############################################################################
## Annotate data with biome column
pts <- SpatialPoints(cbind(data$long, data$lat), 
                     proj4string = CRS(proj4string(biomes)))
biomevalues <- sp::over(pts, biomes[,c("BIOME")])
data$biome <- biomevalues$BIOME
data$biome <- ifelse(is.na(data$biome)==TRUE,'sea',
                     ifelse(data$biome == 13,'desert',
                            ifelse(data$biome == 1,'forest',
                                   #                ifelse(data$biome == 2,'humid forest',
                                   ifelse(data$biome == 14,'forest','other'))))#)


rm(pts,biomevalues)

## Annotate data with NDVI column
aut <- subset(data,data$trip == "Autumn")[,c("dev","dt","lat","long")]
spr <- subset(data,data$trip == "Spring")[,c("dev","dt","lat","long")]

aut.pts <- SpatialPoints(cbind(aut$long, aut$lat), 
                         proj4string = CRS(proj4string(lakes)))
spr.pts <- SpatialPoints(cbind(spr$long, spr$lat), 
                         proj4string = CRS(proj4string(lakes)))

ndvi.aut.values <- raster::extract(ndvi_aut,aut.pts)
ndvi.spr.values <- raster::extract(ndvi_spr,spr.pts)

aut$ndvi <- ndvi.aut.values
spr$ndvi <- ndvi.spr.values

ndvi.pts <- rbind(aut,spr)

data <- merge(data,ndvi.pts,all.x=TRUE)
rm(aut,spr,aut.pts,spr.pts,ndvi.aut.values,ndvi.spr.values,ndvi.pts)
rm(ndvi_aut,ndvi_spr)

#write.csv(data,'data_resampled_biome-annotated_v20210523.csv')

# we only want to account for Sahara and congo basin crossing and therefore correct 
# landscape annotations for small 'forest' and 'desert' patches in East Africa
data$biome2 <- ifelse(data$biome %in% c("other") & data$ndvi < 0.25 & data$lat > 8,"desert",data$biome)
data$biome2 <- ifelse(is.na(data$biome2)==TRUE,'other',data$biome2)
data$biome2 <- ifelse(data$biome2 == 'forest' & data$long > 35,'other',data$biome2)
data$biome2 <- ifelse(data$biome2 == 'forest' & data$trip == "Spring",'other',data$biome2)

# segment trips based on biome crossings, extract longest desert crossing
data <- data[order(data$dev,data$dt),]
data$biome.seg <- as.numeric(c(0,cumsum(as.logical(diff(as.numeric(as.factor(paste(data$tripID,data$biome2))))))))
data$biome.seg <- sprintf("%04d",data$biome.seg)
data$biome.seg <- paste(data$dev,data$biome.seg,sep="_")

seglengths <- data %>%
  filter(biome2 == "desert")  %>%
  group_by(biome.seg,tripID,biome2) %>%
  summarize(biome.seg.length = sum(dur.f)/(3600*24)) 

longest.desert.crossing.per.trip <- seglengths %>%
  group_by(tripID) %>%
  summarize(longest.crossing.time = max(biome.seg.length),
            longest.crossing = biome.seg[which(biome.seg.length == longest.crossing.time)])

longest.crossings <- as.data.frame(longest.desert.crossing.per.trip)[,c("tripID","longest.crossing")]
data <- merge(data,longest.crossings,all.x=TRUE)
data$biome3 <- ifelse(data$biome.seg == data$longest.crossing,'desert',
                      ifelse(data$biome2 == 'desert','other',data$biome2))

rm(longest.crossings,longest.desert.crossing.per.trip,seglengths)

### CALCULATE DISTANCE THRESHOLDS TO DELINEATE BIOME 'AREAS' ON ALL PLOTS
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# calculate distance thresholds for desert per season
desert.trip <- data %>%
  filter(biome3 == "desert") %>%
  group_by(trip,dev,tripID) %>%
  summarize(start = min(dt),
            start.dist = dist.to.colony[dt == start],
            end = max(dt),
            end.dist = dist.to.colony[dt == end]) 

desert.trip.id <- desert.trip  %>%
  group_by(trip,dev) %>%
  summarize(mean.start.dist = mean(start.dist),
            mean.end.dist = mean(end.dist)) 

desert.trip.mean <- desert.trip.id  %>%
  group_by(trip) %>%
  summarize(desert.start.dist = mean(mean.start.dist),
            desert.end.dist = mean(mean.end.dist)) 
desert.trip.mean <- as.data.frame(desert.trip.mean)
rm(desert.trip,desert.trip.id)

# calculate distance thresholds for forest per season
forest.trip <- data %>%
  filter(biome2 == "forest") %>%
  group_by(trip,dev,tripID) %>%
  summarize(start = min(dt),
            start.dist = dist.to.colony[dt == start],
            end = max(dt),
            end.dist = dist.to.colony[dt == end]) 

forest.trip.id <- forest.trip  %>%
  group_by(trip,dev) %>%
  summarize(mean.start.dist = mean(start.dist),
            mean.end.dist = mean(end.dist)) 

forest.trip.mean <- forest.trip.id  %>%
  group_by(trip) %>%
  summarize(forest.start.dist = mean(mean.start.dist),
            forest.end.dist = mean(mean.end.dist)) 
forest.trip.mean <- as.data.frame(forest.trip.mean)
rm(forest.trip,forest.trip.id)


# calculate distance thresholds for sea per season
sea.trip <- data %>%
  filter(biome == "sea" & data$long > 35) %>%
  group_by(trip,dev,tripID) %>%
  summarize(start = min(dt),
            start.dist = dist.to.colony[dt == start],
            end = max(dt),
            end.dist = dist.to.colony[dt == end]) 

sea.trip.id <- sea.trip  %>%
  group_by(trip,dev) %>%
  summarize(mean.start.dist = mean(start.dist),
            mean.end.dist = mean(end.dist)) 

sea.trip.mean <- sea.trip.id  %>%
  group_by(trip) %>%
  summarize(sea.start.dist = mean(mean.start.dist),
            sea.end.dist = mean(mean.end.dist)) 
sea.trip.mean <- as.data.frame(sea.trip.mean)
rm(sea.trip,sea.trip.id)

# merge all distance thresholds in one df
trip.dists <- merge(desert.trip.mean,forest.trip.mean,all.x=TRUE)
trip.dists <- merge(trip.dists,sea.trip.mean,all.x=TRUE)
rm(forest.trip.mean,desert.trip.mean,sea.trip.mean)

#####################################
### ADD SEX TO PRINCIPAL DF'S    ###  
#####################################
data <- merge(data,meta[,c("dev","sex")],all.x=TRUE)
stops <- merge(stops,meta[,c("dev","sex")],all.x=TRUE)

data <- data[order(data$dev,data$dt),]

#####################################
### CREATE BASEMAP FOR MAPPING    ###  
#####################################
# create the breaks- and label vectors for coordinate axes
ewbrks <- seq(-10,40,10)
nsbrks <- seq(-20,30,10)
ewlbls <- unlist(lapply(ewbrks, function(x) ifelse(x < 0, paste(-x,"°W",sep=""), ifelse(x > 0, paste(x,"°E",sep=""),x))))
nslbls <- unlist(lapply(nsbrks, function(x) ifelse(x < 0, paste(-x,"°S",sep=""), ifelse(x > 0, paste(x,"°N",sep=""),x))))

# create basemap
basemap <- ggplot()+
  ## Add biomes (fill) and elevation (alpha)
  geom_raster(data=hdf3[which(is.na(hdf3$biome2)==FALSE),],mapping=aes(long,lat,alpha=alt,fill=biome2))+
  scale_alpha_continuous(name="Elevation",range=c(0.35,1))+
                         #breaks=c(quantile(hdf2$alt,0.05),quantile(hdf2$alt,0.15),
                        #          quantile(hdf2$alt,0.30),quantile(hdf2$alt,0.50),
                        #          quantile(hdf2$alt,0.75)))+	
#  scale_fill_manual(name="Biome",values=c("desert"="cornsilk4","forest"="darkolivegreen4","other"="burlywood4"))+
  scale_fill_manual(name="Biome",values=c("desert"="wheat3","forest"="olivedrab4","other"="grey30"))+
  #scale_fill_manual(values=c("desert"="grey70","forest"="grey30","NA"="white","other"="grey50"))+
  ## Add borders and lakes
  #geom_polygon(data=political,aes(long,lat,group=paste(country,piece)),col='transparent',fill='grey90',alpha=.1)+	
  geom_polygon(data=lakes, aes(x = long, y = lat, group = group), fill = 'white') +
  ## Add colony location
  geom_point(data=ref,aes(x=ale.long,y=ale.lat),size=2.1,col='red',shape=3)+
  ## Add distance buffers
  geom_polygon(data=buf_1000km,aes(x=long,y=lat,group=group),size=.4,col='grey50',fill='transparent',linetype='dotted')+
  geom_polygon(data=buf_2000km,aes(x=long,y=lat,group=group),size=.4,col='grey50',fill='transparent',linetype='dotted')+
  geom_polygon(data=buf_3000km,aes(x=long,y=lat,group=group),size=.4,col='grey50',fill='transparent',linetype='dotted')+
  geom_polygon(data=buf_4000km,aes(x=long,y=lat,group=group),size=.4,col='grey50',fill='transparent',linetype='dotted')+
  geom_polygon(data=buf_5000km,aes(x=long,y=lat,group=group),size=.4,col='grey50',fill='transparent',linetype='dotted')+
  geom_polygon(data=buf_6000km,aes(x=long,y=lat,group=group),size=.4,col='grey50',fill='transparent',linetype='dotted')+
  geom_polygon(data=buf_7000km,aes(x=long,y=lat,group=group),size=.4,col='grey50',fill='transparent',linetype='dotted')+
  geom_polygon(data=buf_8000km,aes(x=long,y=lat,group=group),size=.4,col='grey50',fill='transparent',linetype='dotted')+
  ## Layout map
  coord_quickmap(xlim=longlimits,ylim=latlimits,expand=FALSE) + 
  scale_x_continuous(breaks = ewbrks, labels = ewlbls, expand = c(0, 0)) +
  scale_y_continuous(breaks = nsbrks, labels = nslbls, expand = c(0, 0)) +
  theme_bw() + 
  facet_grid(.~trip) +
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


#####################################
### MAP TRACKS PER ID             ###  
#####################################
#> unique(mean.i.coords$dev)
#[1] "B1011" "B1012" "B1013" "B1014" "B2048" "B2051" "B2368" "B2391" "B2392" "B2400" "B2423"
#[12] "B2424" "B2453"
selection <- c("B1011","B1014","B2453","B2400","B2051")  
data <- data[order(data$dev,data$dt),]

p1 <- basemap + 
  geom_path(data=data[which(data$n.trips > 1 & !(data$dev %in% selection)),],aes(x=long,y=lat,group=paste(dev,yr,trip)),col='grey20',size=.2,alpha=.75)+
  geom_path(data=data[which(data$n.trips > 1 & data$dev %in% selection),],aes(x=long,y=lat,col=dev,group=paste(dev,yr,trip)),size=.5,alpha=.8)+
  scale_colour_viridis_d(name="Bird ID",option="magma")+
  # scale_colour_viridis_d(name="Biome",option="magma")+
  # Add stop-overs
  geom_point(data=stops[which(!(stops$dev %in% selection)),],aes(x=st.long,y=st.lat,size=round(as.numeric(st.dur))),col='grey20',fill='white',alpha=.4,shape=21)+
  geom_point(data=stops[which(!(stops$dev %in% selection)),],aes(x=st.long,y=st.lat),col='grey60',size=.3)+
  geom_point(data=stops[which(stops$dev %in% selection),],aes(x=st.long,y=st.lat,size=round(as.numeric(st.dur)),col=dev),fill='white',alpha=.4,shape=21)+
  geom_point(data=stops[which(stops$dev %in% selection),],aes(x=st.long,y=st.lat,col=dev),size=.3)+
  scale_size_binned(name="Stop duration [days]",range=c(1,6),breaks=c(1,3,7,14),labels=c("1","2-3","4-7","8-14"))
  
## Add falcon image
#gg <- p1 + annotation_custom(img, xmin = 25, xmax = 41.2, ymin = 15, ymax = latlimits[2])

ggsave(plot=p1,path='./output - maps/',filename='FigS1A_IndividualMigrations-v20220131.tiff',width=8.5,height=6,dpi=300)

# plot timing in autumn
p3 <- ggplot()+
  ## Add approximation of biomes
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(ymin=0,ymax=desert.start.dist/1000,xmin=-Inf,xmax=Inf),fill='grey30',alpha=.3)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(ymin=desert.start.dist/1000,ymax=desert.end.dist/1000,xmin=-Inf,xmax=Inf),fill='wheat3',alpha=.6)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(ymin=desert.end.dist/1000,ymax=forest.start.dist/1000,xmin=-Inf,xmax=Inf),fill='grey30',alpha=.3)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(ymin=forest.start.dist/1000,ymax=forest.end.dist/1000,xmin=-Inf,xmax=Inf),fill='olivedrab4',alpha=.5)+
#  geom_hline(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(yintercept=forest.start.dist/1000),col='chartreuse4',linetype='solid',size=.4)+
#  geom_hline(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(yintercept=forest.end.dist/1000),col='chartreuse4',linetype='solid',size=.4)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(ymin=forest.end.dist/1000,ymax=sea.start.dist/1000,xmin=-Inf,xmax=Inf),fill='grey30',alpha=.3)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(ymin=sea.start.dist/1000,ymax=sea.end.dist/1000,xmin=-Inf,xmax=Inf),fill='white',alpha=.3)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(ymin=sea.end.dist/1000,ymax=Inf,xmin=-Inf,xmax=Inf),fill='grey30',alpha=.3)+
  ## Add tracking data
  geom_path(data=data[which(!(data$dev %in% selection) & data$n.trips > 1),],aes(x=cycle.time,y=dist.to.colony/1000,group=group),col='grey20',size=.3,alpha=.6)+
  geom_point(data=stops[which(!(stops$dev %in% selection & stops$n.trips > 1)),],aes(x=cycle.time+(st.dur/2),y=dist.to.colony/1000),fill='white',col='grey60',size=4,alpha=.4,shape=21)+
  geom_point(data=stops[which(!(stops$dev %in% selection & stops$n.trips > 1)),],aes(x=cycle.time+(st.dur/2),y=dist.to.colony/1000),col='grey60',size=.6)+
  geom_path(data=data[which(data$dev %in% selection & data$n.trips > 1),],aes(x=cycle.time,y=dist.to.colony/1000,col=dev,group=group),size=.7,alpha=1)+
  geom_point(data=stops[which(stops$dev %in% selection & stops$n.trips > 1),],aes(x=cycle.time+(st.dur/2),y=dist.to.colony/1000,col=dev),fill='white',size=4,alpha=.4,shape=21)+
  geom_point(data=stops[which(stops$dev %in% selection & stops$n.trips > 1),],aes(x=cycle.time+(st.dur/2),y=dist.to.colony/1000,col=dev),size=.6)+
  scale_colour_viridis_d(option="magma")+
  #scale_colour_viridis_d(name="Biome",option="magma")+
  ##Add stop-overs
#  geom_point(data=segs,aes(y=dist.to.colony/1000,x=cycle.time),col='black',size=.3)+
  ## Layout map
  coord_cartesian(expand=FALSE) + 
  scale_y_reverse() +
  scale_x_continuous(breaks=c(92,123,153,184),labels=c("Oct","Nov","Dec","Jan"),limits=c(80,180),position='top')+
  theme_bw() + xlab("Date") + ylab ("Distance to colony [km]") + 
  ## Layout text items
  theme(panel.border	    = element_blank(),
        strip.background	= element_rect(fill='white',colour='NA'),
        legend.position   = 'none',
        panel.grid.minor.x   = element_blank(),
        panel.grid.major.x   = element_blank(),
        panel.grid.minor.y   = element_line(size=.2,linetype='dashed',colour='grey40'),
        panel.grid.major.y   = element_line(size=.2,linetype='dashed',colour='grey40'),
        axis.line         = element_line(size=.4),
        axis.text		      = element_text(size=10),
        axis.title		    = element_text(size=11))

# plot timign in spring
p4 <- ggplot()+
  ## Add tracking data
  geom_rect(data=trip.dists[which(trip.dists$trip == "Spring"),],aes(ymin=0,ymax=desert.end.dist/1000,xmin=-Inf,xmax=Inf),fill='grey30',alpha=.3)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Spring"),],aes(ymin=desert.end.dist/1000,ymax=desert.start.dist/1000,xmin=-Inf,xmax=Inf),fill='wheat3',alpha=.6)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Spring"),],aes(ymin=sea.end.dist/1000,ymax=desert.start.dist/1000,xmin=-Inf,xmax=Inf),fill='grey30',alpha=.3)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Spring"),],aes(ymin=sea.start.dist/1000,ymax=sea.end.dist/1000,xmin=-Inf,xmax=Inf),fill='white',alpha=.3)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Spring"),],aes(ymin=sea.start.dist/1000,ymax=Inf,xmin=-Inf,xmax=Inf),fill='grey30',alpha=.3)+
  #  geom_rect(data=forest.trip.mean[which(forest.trip.mean$trip == "Spring"),],aes(ymin=mean.start.dist/1000,ymax=mean.end.dist/1000,xmin=-Inf,xmax=Inf),fill='olivedrab4',alpha=.5)+
  # geom_path(data=nonmig,aes(x=cycle.time,y=dist.to.colony/1000,col=dev,group=group),size=.6,alpha=.7)+
  geom_path(data=data[which(!(data$dev %in% selection) & data$n.trips > 1),],aes(x=cycle.time,y=dist.to.colony/1000,group=group),col='grey20',size=.3,alpha=.6)+
  geom_point(data=stops[which(!(stops$dev %in% selection & stops$n.trips > 1)),],aes(x=cycle.time+(st.dur/2),y=dist.to.colony/1000),fill='white',col='grey60',size=4,alpha=.4,shape=21)+
  geom_point(data=stops[which(!(stops$dev %in% selection & stops$n.trips > 1)),],aes(x=cycle.time+(st.dur/2),y=dist.to.colony/1000),col='grey60',size=.6)+
  geom_path(data=data[which(data$dev %in% selection & data$n.trips > 1),],aes(x=cycle.time,y=dist.to.colony/1000,col=dev,group=group),size=.7,alpha=1)+
  geom_point(data=stops[which(stops$dev %in% selection & stops$n.trips > 1),],aes(x=cycle.time+(st.dur/2),y=dist.to.colony/1000,col=dev),fill='white',size=4,alpha=.4,shape=21)+
  geom_point(data=stops[which(stops$dev %in% selection & stops$n.trips > 1),],aes(x=cycle.time+(st.dur/2),y=dist.to.colony/1000,col=dev),size=.6)+
  scale_colour_viridis_d(option="magma")+
  #scale_colour_viridis_d(name="Biome",option="magma")+
##Add stop-overs
 # geom_point(data=segs,aes(y=dist.to.colony/1000,x=cycle.time),col='black',size=.3)+
  ## Layout map
  coord_cartesian(expand=FALSE) +  scale_y_reverse() +
  scale_x_continuous(breaks=c(274,304,335),labels=c("Apr","May","Jun"),limits=c(262,362),position='top')+
  theme_bw() + 
  xlab("Date") + ylab ("Distance to colony [km]") + 
  ## Layout text items
  theme(
    #panel.border	    = element_blank(),
        strip.background	= element_rect(fill='white',colour='NA'),
        legend.position   = 'none',
        panel.grid.minor.x   = element_blank(),
        panel.grid.major.x   = element_blank(),
        panel.grid.minor.y   = element_line(size=.2,linetype='dashed',colour='grey40'),
        panel.grid.major.y   = element_line(size=.2,linetype='dashed',colour='grey40'),
        axis.line         = element_line(size=.4),
        axis.text.x		      = element_text(size=10),
        axis.text.y		      = element_blank(),
        axis.title.y		      = element_blank(),
        axis.title		    = element_text(size=11))

# combine and save timing plots as panel B-C for Fig.1
(pfin <- cowplot::plot_grid(p3,p4, ncol = 2))
ggsave(plot=pfin,path='./output - graphs/',filename='FigS1B_IndividualMigrations-v20220131.tiff',width=8.5,height=3.5,dpi=300)
rm(pfin,p1,p3,p4)


#####################################
### MAP TRACKS PER SEX             ###  
#####################################

p1 <- basemap + 
  geom_path(data=data[which(data$n.trips > 1),],aes(x=long,y=lat,col=sex,group=paste(dev,yr,trip)),size=.5,alpha=.8)+
  scale_colour_manual(name="Sex",values=c("male"="darkorange","female"="brown"))+
  # scale_colour_viridis_d(name="Biome",option="magma")+
  # Add stop-overs
  geom_point(data=stops[which(stops$n.trips > 1),],aes(x=st.long,y=st.lat,size=round(as.numeric(st.dur)),col=sex),fill='white',alpha=.4,shape=21)+
  geom_point(data=stops[which(stops$n.trips > 1),],aes(x=st.long,y=st.lat,col=sex),size=.3)+
  scale_size_binned(name="Stop duration [days]",range=c(1,6),breaks=c(1,3,7,14),labels=c("1","2-3","4-7","8-14"))

## Add falcon image
#gg <- p1 + annotation_custom(img, xmin = 25, xmax = 41.2, ymin = 15, ymax = latlimits[2])

ggsave(plot=p1,path='./output - maps/',filename='Fig1A_Migrations-per-sex_v20210707.tiff',width=8.5,height=6,dpi=300)

# plot timing in autumn per sex
p3 <- ggplot()+
  ## Add approximation of biomes
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(ymin=0,ymax=desert.start.dist/1000,xmin=-Inf,xmax=Inf),fill='grey30',alpha=.3)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(ymin=desert.start.dist/1000,ymax=desert.end.dist/1000,xmin=-Inf,xmax=Inf),fill='wheat3',alpha=.6)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(ymin=desert.end.dist/1000,ymax=forest.start.dist/1000,xmin=-Inf,xmax=Inf),fill='grey30',alpha=.3)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(ymin=forest.start.dist/1000,ymax=forest.end.dist/1000,xmin=-Inf,xmax=Inf),fill='olivedrab4',alpha=.5)+
  #  geom_hline(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(yintercept=forest.start.dist/1000),col='chartreuse4',linetype='solid',size=.4)+
  #  geom_hline(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(yintercept=forest.end.dist/1000),col='chartreuse4',linetype='solid',size=.4)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(ymin=forest.end.dist/1000,ymax=sea.start.dist/1000,xmin=-Inf,xmax=Inf),fill='grey30',alpha=.3)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(ymin=sea.start.dist/1000,ymax=sea.end.dist/1000,xmin=-Inf,xmax=Inf),fill='white',alpha=.3)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(ymin=sea.end.dist/1000,ymax=Inf,xmin=-Inf,xmax=Inf),fill='grey30',alpha=.3)+
  ## Add tracking data
  geom_path(data=data[which(data$n.trips > 1),],aes(x=cycle.time,y=dist.to.colony/1000,col=sex,group=group),size=.7,alpha=1)+
  geom_point(data=stops[which(stops$n.trips > 1),],aes(x=cycle.time+(st.dur/2),y=dist.to.colony/1000,col=sex),fill='white',size=4,alpha=.4,shape=21)+
  geom_point(data=stops[which(stops$n.trips > 1),],aes(x=cycle.time+(st.dur/2),y=dist.to.colony/1000,col=sex),size=.6)+
  scale_colour_manual(name="Sex",values=c("male"="darkorange","female"="brown"))+
  ## Layout graph
  coord_cartesian(expand=FALSE) + 
  scale_y_reverse() +
  scale_x_continuous(breaks=c(92,123,153,184),labels=c("Oct","Nov","Dec","Jan"),limits=c(80,180),position='top')+
  theme_bw() + xlab("Date") + ylab ("Distance to colony [km]") + 
  ## Layout text items
  theme(panel.border	    = element_blank(),
        strip.background	= element_rect(fill='white',colour='NA'),
        legend.position   = 'none',
        panel.grid.minor.x   = element_blank(),
        panel.grid.major.x   = element_blank(),
        panel.grid.minor.y   = element_line(size=.2,linetype='dashed',colour='grey40'),
        panel.grid.major.y   = element_line(size=.2,linetype='dashed',colour='grey40'),
        axis.line         = element_line(size=.4),
        axis.text		      = element_text(size=10),
        axis.title		    = element_text(size=11))

# plot timing in spring per sex
p4 <- ggplot()+
  ## Add approximation of biomes
  geom_rect(data=trip.dists[which(trip.dists$trip == "Spring"),],aes(ymin=0,ymax=desert.end.dist/1000,xmin=-Inf,xmax=Inf),fill='grey30',alpha=.3)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Spring"),],aes(ymin=desert.end.dist/1000,ymax=desert.start.dist/1000,xmin=-Inf,xmax=Inf),fill='wheat3',alpha=.6)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Spring"),],aes(ymin=sea.end.dist/1000,ymax=desert.start.dist/1000,xmin=-Inf,xmax=Inf),fill='grey30',alpha=.3)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Spring"),],aes(ymin=sea.start.dist/1000,ymax=sea.end.dist/1000,xmin=-Inf,xmax=Inf),fill='white',alpha=.3)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Spring"),],aes(ymin=sea.start.dist/1000,ymax=Inf,xmin=-Inf,xmax=Inf),fill='grey30',alpha=.3)+
  ## add tracking data
  geom_path(data=data[which(data$n.trips > 1),],aes(x=cycle.time,y=dist.to.colony/1000,col=sex,group=group),size=.7,alpha=1)+
  geom_point(data=stops[which(stops$n.trips > 1),],aes(x=cycle.time+(st.dur/2),y=dist.to.colony/1000,col=sex),fill='white',size=4,alpha=.4,shape=21)+
  geom_point(data=stops[which(stops$n.trips > 1),],aes(x=cycle.time+(st.dur/2),y=dist.to.colony/1000,col=sex),size=.6)+
  scale_colour_manual(name="Sex",values=c("male"="darkorange","female"="brown"))+
  ## Layout graph
  coord_cartesian(expand=FALSE) +  scale_y_reverse() +
  scale_x_continuous(breaks=c(274,304,335),labels=c("Apr","May","Jun"),limits=c(262,362),position='top')+
  theme_bw() + 
  xlab("Date") + ylab ("Distance to colony [km]") + 
  ## Layout text items
  theme(
    #panel.border	    = element_blank(),
    strip.background	= element_rect(fill='white',colour='NA'),
    legend.position   = 'none',
    panel.grid.minor.x   = element_blank(),
    panel.grid.major.x   = element_blank(),
    panel.grid.minor.y   = element_line(size=.2,linetype='dashed',colour='grey40'),
    panel.grid.major.y   = element_line(size=.2,linetype='dashed',colour='grey40'),
    axis.line         = element_line(size=.4),
    axis.text.x		      = element_text(size=10),
    axis.text.y		      = element_blank(),
    axis.title.y		      = element_blank(),
    axis.title		    = element_text(size=11))

# combine and save timing plots as panel B-C for Fig.1
(pfin <- cowplot::plot_grid(p3,p4, ncol = 2))
ggsave(plot=pfin,path='./output - graphs/',filename='Fig1B_Migration-Timing-per-Sex_v20210707.tiff',width=8.5,height=3.5,dpi=300)
rm(pfin,p1,p3,p4)

#####################################
### Summarize performance per trip###  
#####################################
### Calculate summary statistics per trip, which we can use to look at individual consistency
# Variables of interest are
# departure, arrival, restdays, traveldays, total duration, 
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# calculate coords and timing at each distance threshold per trip
trip.i.coords <- data %>%
  group_by(tripID,sex,dev,yr,trip,i) %>%
  filter(n.trips > 1) %>%
  summarize(i = unique(i),
            i.lat = head(lat,1),
            i.long = head(long,1),
            i.yday = head(yday,1),
            i.cycle.time = head(cycle.time,1),
            i.biome = Mode(biome3)) %>%
  ungroup()
trip.i.coords <- as.data.frame(trip.i.coords)

# calculate mean seasonal routes and timing at ID level
mean.i.coords <- trip.i.coords %>%
  group_by(sex,dev,trip,i) %>%
  summarize(i = unique(i),
            i.n.trips = length(unique(tripID)),
            i.mean.lat = mean(i.lat),
            i.mean.long = mean(i.long),
            i.mean.yday = mean(i.yday),
            i.mean.cycle.time = mean(i.cycle.time)) %>%
  ungroup()
mean.i.coords <- as.data.frame(mean.i.coords)

mean.i.coords <- merge(mean.i.coords,ind.summ[,c("dev","trip","n.trips")],all.x=TRUE)
#mean.i.coords <- subset(mean.i.coords,mean.i.coords$i.n.trips == mean.i.coords$n.trips)

trip.i.coords <- merge(trip.i.coords,mean.i.coords,all.x=TRUE)

trip.i.coords$i.intra.dist <- deg.dist(long1=trip.i.coords$i.long,lat1=trip.i.coords$i.lat,
                               long2=trip.i.coords$i.mean.long,lat2=trip.i.coords$i.mean.lat)
trip.i.coords$i.intra.yday <- abs(trip.i.coords$i.yday - trip.i.coords$i.mean.yday)

# recalculate mean seasonal routes and timing AND mean variance in km at ID level
mean.i.coords <- trip.i.coords %>%
  group_by(sex,dev,trip,i) %>%
  filter(length(unique(tripID)) == i.n.trips) %>%
  summarize(i = unique(i),
            i.mean.lat = mean(i.lat),
            i.mean.long = mean(i.long),
            i.mean.yday = mean(i.yday),
            i.mean.cycle.time = mean(i.cycle.time),
            i.var.temp = mean(i.intra.yday),
            i.var.spat = mean(i.intra.dist),
            i.mean.biome = Mode(i.biome)) %>%
  ungroup()
mean.i.coords <- as.data.frame(mean.i.coords)

#trip.i.coords2 <- merge(trip.i.coords,mean.i.coords,all.x=TRUE)
#data <- merge(data,trip.i.coords,all.x=TRUE)

# calculate mean seasonal routes and timing at sex level
sexmean.i.coords <- mean.i.coords %>%
  group_by(sex,trip,i) %>%
  summarize(i = unique(i),
            i.sex = unique(sex),
            i.sex.dev = length(unique(dev)),
            i.sexmean.lat = mean(i.mean.lat),
            i.sexmean.long = mean(i.mean.long),
            i.sexmean.yday = mean(i.mean.yday),
            i.sexmean.cycle.time = mean(i.mean.cycle.time)) %>%
  ungroup()
sexmean.i.coords <- as.data.frame(sexmean.i.coords)
sexmean.i.coords <- subset(sexmean.i.coords,(sexmean.i.coords$i.sex == "male" & sexmean.i.coords$i.sex.dev == 8)|(sexmean.i.coords$i.sex == "female" & sexmean.i.coords$i.sex.dev == 5))
        

# calculate mean seasonal routes and timing at pop level
popmean.i.coords <- mean.i.coords %>%
  group_by(trip,i) %>%
 # filter(length(unique(dev))>=10) %>%
  summarize(i = unique(i),
            i.dev = length(unique(dev)),
            i.popmean.lat = mean(i.mean.lat),
            i.popmean.long = mean(i.mean.long),
            i.popmean.yday = mean(i.mean.yday),
            i.popmean.cycle.time = mean(i.mean.cycle.time)) %>%
  ungroup()
popmean.i.coords <- as.data.frame(popmean.i.coords)
popmean.i.coords <- subset(popmean.i.coords,popmean.i.coords$i.dev == 13)

# calculate between-individual var as average deviation from mean individual routes and mean pop route
mean.i.coords <- merge(mean.i.coords,popmean.i.coords,all.x=TRUE)
mean.i.coords$i.inter.dist <- deg.dist(long1=mean.i.coords$i.mean.long,lat1=mean.i.coords$i.mean.lat,
                                       long2=mean.i.coords$i.popmean.long,lat2=mean.i.coords$i.popmean.lat)
mean.i.coords$i.inter.yday <- abs(mean.i.coords$i.mean.yday-mean.i.coords$i.popmean.yday)

popmean.i.coords <- mean.i.coords %>%
  group_by(trip,i) %>%
  summarize(i = unique(i),
            i.dev = length(unique(dev)),
            i.popmean.lat = mean(i.mean.lat),
            i.popmean.long = mean(i.mean.long),
            i.popmean.yday = mean(i.mean.yday),
            i.popmean.cycle.time = mean(i.mean.cycle.time),
            i.popvar.temp = mean(i.inter.yday),
            i.popvar.spat = mean(i.inter.dist),
            i.popmean.biome = Mode(i.mean.biome)) %>%
  ungroup()
popmean.i.coords <- as.data.frame(popmean.i.coords)
popmean.i.coords <- subset(popmean.i.coords,popmean.i.coords$i.dev == 13)

mean.i.coords <- merge(mean.i.coords,popmean.i.coords,all.x=TRUE)
#mean.i.coords$i.prop.popvar.spat <- mean.i.coords$i.var.spat/mean.i.coords$i.popvar.spat
#mean.i.coords$i.prop.popvar.temp <- mean.i.coords$i.var.temp/mean.i.coords$i.popvar.temp

# calculate within-sex var as average deviation from mean individual routes to mean sex route
mean.i.coords <- merge(mean.i.coords,sexmean.i.coords,all.x=TRUE)
mean.i.coords$i.intrasex.dist <- deg.dist(long1=mean.i.coords$i.mean.long,lat1=mean.i.coords$i.mean.lat,
                                       long2=mean.i.coords$i.sexmean.long,lat2=mean.i.coords$i.sexmean.lat)
mean.i.coords$i.intrasex.yday <- abs(mean.i.coords$i.mean.yday-mean.i.coords$i.sexmean.yday)

sexmean.i.coords <- mean.i.coords %>%
  group_by(sex,trip,i) %>%
  summarize(i = unique(i),
            i.sex = unique(sex),
            i.sex.dev = length(unique(dev)),
            i.sexmean.lat = mean(i.mean.lat),
            i.sexmean.long = mean(i.mean.long),
            i.sexmean.yday = mean(i.mean.yday),
            i.sexmean.cycle.time = mean(i.mean.cycle.time),
            i.intrasex.temp = mean(i.intrasex.yday),
            i.intrasex.spat = mean(i.intrasex.dist),
            i.sexmean.biome = Mode(i.mean.biome)) %>%
  ungroup()
sexmean.i.coords <- as.data.frame(sexmean.i.coords)
sexmean.i.coords <- subset(sexmean.i.coords,(sexmean.i.coords$i.sex == "male" & sexmean.i.coords$i.sex.dev == 8)|(sexmean.i.coords$i.sex == "female" & sexmean.i.coords$i.sex.dev == 5))

# calculate between-sex var as average deviation from mean sex routes and mean pop route
sexmean.i.coords <- merge(sexmean.i.coords,popmean.i.coords,all.x=TRUE)
sexmean.i.coords$i.intersex.dist <- deg.dist(long1=sexmean.i.coords$i.sexmean.long,lat1=sexmean.i.coords$i.sexmean.lat,
                                             long2=sexmean.i.coords$i.popmean.long,lat2=sexmean.i.coords$i.popmean.lat)
sexmean.i.coords$i.intersex.yday <- abs(sexmean.i.coords$i.sexmean.yday-sexmean.i.coords$i.popmean.yday)

popmean.i.coordsb <- sexmean.i.coords %>%
  group_by(trip,i) %>%
  summarize(i = unique(i),
            i.intersex.temp = mean(i.intersex.yday),
            i.intersex.spat = mean(i.intersex.dist)) %>%
  ungroup()
popmean.i.coordsb <- as.data.frame(popmean.i.coordsb)

popmean.i.coords <- merge(popmean.i.coords,popmean.i.coordsb,all.x=TRUE)
rm(popmean.i.coordsb)
# calculate population-level variation as deviation from individual tracks to population mean
trip.i.coords <- merge(trip.i.coords,mean.i.coords,all.x=TRUE)
data <- merge(data,mean.i.coords,all.x=TRUE)

trip.i.coords <- trip.i.coords[order(trip.i.coords$dev,trip.i.coords$trip,trip.i.coords$i),]
mean.i.coords <- mean.i.coords[order(mean.i.coords$dev,mean.i.coords$trip,mean.i.coords$i),]
sexmean.i.coords <- sexmean.i.coords[order(sexmean.i.coords$i.sex,sexmean.i.coords$trip,sexmean.i.coords$i),]
popmean.i.coords <- popmean.i.coords[order(popmean.i.coords$trip,popmean.i.coords$i),]
data <- data[order(data$dev,data$dt),]

## we have absolute deviations for plotting patterns in variation, and signed deviaitons (*.rel) to test for repeatability
trip.i.coords$i.dist.to.pop <- deg.dist(long1=trip.i.coords$i.long,lat1=trip.i.coords$i.lat,
                                       long2=trip.i.coords$i.popmean.long,lat2=trip.i.coords$i.popmean.lat)
trip.i.coords$i.dist.to.pop.rel <- ifelse(trip.i.coords$i.lat < trip.i.coords$i.popmean.lat , trip.i.coords$i.dist.to.pop*-1, 
                                      ifelse(trip.i.coords$i.long < trip.i.coords$i.popmean.long, trip.i.coords$i.dist.to.pop*-1, trip.i.coords$i.dist.to.pop))
trip.i.coords$i.days.to.pop.rel <- trip.i.coords$i.yday-trip.i.coords$i.popmean.yday
trip.i.coords$i.days.to.pop <- abs(trip.i.coords$i.days.to.pop.rel)

trip.i.coords$i.dist.to.sex <- deg.dist(long1=trip.i.coords$i.long,lat1=trip.i.coords$i.lat,
                                        long2=trip.i.coords$i.sexmean.long,lat2=trip.i.coords$i.sexmean.lat)
trip.i.coords$i.dist.to.sex.rel <- ifelse(trip.i.coords$i.lat < trip.i.coords$i.sexmean.lat , trip.i.coords$i.dist.to.sex*-1, 
                                          ifelse(trip.i.coords$i.long < trip.i.coords$i.sexmean.long, trip.i.coords$i.dist.to.sex*-1, trip.i.coords$i.dist.to.sex))
trip.i.coords$i.days.to.sex.rel <- trip.i.coords$i.yday-trip.i.coords$i.sexmean.yday

### GAMS POPULATION-LEVEL VARIATION
### ### ### ### ### ### ### ### ### ### ### 
pa <- ggplot()+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=-Inf,xmax=desert.start.dist/1000,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=desert.start.dist/1000,xmax=desert.end.dist/1000,ymin=-Inf,ymax=Inf),fill='wheat3',alpha=.6)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=desert.end.dist/1000,xmax=forest.start.dist/1000,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=forest.start.dist/1000,xmax=forest.end.dist/1000,ymin=-Inf,ymax=Inf),fill='olivedrab4',alpha=.5)+
  #  geom_vline(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xintercept=forest.start.dist/1000),col='chartreuse4',size=.8)+
#  geom_vline(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xintercept=forest.end.dist/1000),col='chartreuse4',size=.8)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=forest.end.dist/1000,xmax=sea.start.dist/1000,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+ 
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=sea.start.dist/1000,xmax=sea.end.dist/1000,ymin=-Inf,ymax=Inf),fill='white',alpha=.2)+ 
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=sea.end.dist/1000,xmax=Inf,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+ 
  #  geom_rect(data=trip.dists[which(trip.dists$trip == "Spring"),],aes(ymin=0,ymax=desert.end.dist/1000,xmin=-Inf,xmax=Inf),fill='grey30',alpha=.3)+
#  geom_rect(data=trip.dists[which(trip.dists$trip == "Spring"),],aes(ymin=desert.start.dist/1000,ymax=Inf,xmin=-Inf,xmax=Inf),fill='grey30',alpha=.3)+
  geom_smooth(data=trip.i.coords,aes(x=as.numeric(i)*100,y=i.dist.to.pop,fill=trip),size=.1,col='black')+
  #  geom_point(data=mean.i.coords,aes(x=as.numeric(i)*100,y=i.var.spat,fill=trip),size=.1,col='black',shape=21)+
  scale_fill_manual(values=c("Autumn"="orangered","Spring"="cornflowerblue"))+
  ggtitle("Variation in route choice [km]")+
  ## Add star for Alegranza
  ## Layout map
  theme_classic() + xlab("\nDist. to Colony [km]") + ylab ("\nPopulation-level") +
  coord_cartesian(ylim=c(-50,800))+
  ## Layout text items
  theme(panel.border	    = element_blank(),
        strip.background	= element_rect(fill='white',colour='NA'),
        legend.position   = 'none',
        panel.grid.minor.y   = element_blank(),
        panel.grid.major.y   = element_blank(),
        panel.grid.minor.x   = element_line(size=.2,linetype='dashed',colour='grey40'),
        panel.grid.major.x   = element_line(size=.2,linetype='dashed',colour='grey40'),
        legend.title 	    = element_text(size=11),
        legend.text 	    = element_text(size=10),
        axis.text		      = element_text(size=10,),
        axis.title.y		    = element_text(size=11),
        axis.title.x		    = element_blank(),
        plot.margin       = unit(c(0.1,0.02,0.02,0.1),"cm"),
        plot.title = element_text(size=14,face='bold'))

pb <- ggplot()+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=-Inf,xmax=desert.start.dist/1000,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=desert.start.dist/1000,xmax=desert.end.dist/1000,ymin=-Inf,ymax=Inf),fill='wheat3',alpha=.6)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=desert.end.dist/1000,xmax=forest.start.dist/1000,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=forest.start.dist/1000,xmax=forest.end.dist/1000,ymin=-Inf,ymax=Inf),fill='olivedrab4',alpha=.5)+
  #  geom_vline(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xintercept=forest.start.dist/1000),col='chartreuse4',size=.8)+
  #  geom_vline(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xintercept=forest.end.dist/1000),col='chartreuse4',size=.8)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=forest.end.dist/1000,xmax=sea.start.dist/1000,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+ 
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=sea.start.dist/1000,xmax=sea.end.dist/1000,ymin=-Inf,ymax=Inf),fill='white',alpha=.2)+ 
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=sea.end.dist/1000,xmax=Inf,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+ 
  geom_smooth(data=trip.i.coords,aes(x=as.numeric(i)*100,y=i.days.to.pop,fill=trip),size=.1,col='black')+
  #  geom_point(data=popmean.i.coords,aes(x=as.numeric(i)*100,y=i.popvar.spat,fill=trip),size=.1,col='black',shape=21)+
  scale_fill_manual(values=c("Autumn"="orangered","Spring"="cornflowerblue"))+
  ## Add star for Alegranza
  ## Layout map
  theme_classic() + xlab("\nDist. to Colony [km]") + 
  ylab ("\n ") +
  #ylab ("\nPopulation-level") +
  coord_cartesian(ylim=c(-1,10))+
  ggtitle("Variation in timing [days]")+
    ## Layout text items
  theme(panel.border	    = element_blank(),
        strip.background	= element_rect(fill='white',colour='NA'),
        legend.position   = 'none',
        panel.grid.minor.y   = element_blank(),
        panel.grid.major.y   = element_blank(),
        panel.grid.minor.x   = element_line(size=.2,linetype='dashed',colour='grey40'),
        panel.grid.major.x   = element_line(size=.2,linetype='dashed',colour='grey40'),
        legend.title 	    = element_text(size=11),
        legend.text 	    = element_text(size=10),
        axis.text		    = element_text(size=10),
        axis.title.x		    = element_blank(),
        axis.title.y		    = element_text(size=11),
        plot.margin       = unit(c(0.1,0.02,0.02,0.1),"cm"),
        plot.title = element_text(size=14,face='bold'))

### GAMS WITHIN-INDIVIDUAL VARIATION
### ### ### ### ### ### ### ### ### ### ### 
p1 <- ggplot()+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=-Inf,xmax=desert.start.dist/1000,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=desert.start.dist/1000,xmax=desert.end.dist/1000,ymin=-Inf,ymax=Inf),fill='wheat3',alpha=.6)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=desert.end.dist/1000,xmax=forest.start.dist/1000,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=forest.start.dist/1000,xmax=forest.end.dist/1000,ymin=-Inf,ymax=Inf),fill='olivedrab4',alpha=.5)+
  #  geom_vline(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xintercept=forest.start.dist/1000),col='chartreuse4',size=.8)+
  #  geom_vline(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xintercept=forest.end.dist/1000),col='chartreuse4',size=.8)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=forest.end.dist/1000,xmax=sea.start.dist/1000,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+ 
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=sea.start.dist/1000,xmax=sea.end.dist/1000,ymin=-Inf,ymax=Inf),fill='white',alpha=.2)+ 
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=sea.end.dist/1000,xmax=Inf,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+ 
  geom_smooth(data=mean.i.coords,aes(x=as.numeric(i)*100,y=i.var.spat,fill=trip,linetype=sex),size=.1,col='black')+
#  geom_point(data=mean.i.coords,aes(x=as.numeric(i)*100,y=i.var.spat,fill=trip),size=.1,col='black',shape=21)+
  scale_fill_manual(values=c("Autumn"="orangered","Spring"="cornflowerblue"))+
  ## Add star for Alegranza
  ## Layout map
  theme_classic() + xlab("Distance to Colony [km]") + 
  ylab ("Per sex:\nwithin-individual") +
  coord_cartesian(ylim=c(-50,800))+
  ## Layout text items
  theme(panel.border	    = element_blank(),
        strip.background	= element_rect(fill='white',colour='NA'),
        legend.position   = 'none',
        panel.grid.minor.y   = element_blank(),
        panel.grid.major.y   = element_blank(),
        panel.grid.minor.x   = element_line(size=.2,linetype='dashed',colour='grey40'),
        panel.grid.major.x   = element_line(size=.2,linetype='dashed',colour='grey40'),
        legend.title 	    = element_text(size=11),
        legend.text 	    = element_text(size=10),
        axis.text		      = element_text(size=10,),
        axis.title.y		    = element_text(size=11),
        axis.title.x		    = element_text(size=11),
        plot.margin       = unit(c(0.1,0.02,0.02,0.1),"cm"))

p2 <- ggplot()+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=-Inf,xmax=desert.start.dist/1000,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=desert.start.dist/1000,xmax=desert.end.dist/1000,ymin=-Inf,ymax=Inf),fill='wheat3',alpha=.6)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=desert.end.dist/1000,xmax=forest.start.dist/1000,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=forest.start.dist/1000,xmax=forest.end.dist/1000,ymin=-Inf,ymax=Inf),fill='olivedrab4',alpha=.5)+
  #  geom_vline(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xintercept=forest.start.dist/1000),col='chartreuse4',size=.8)+
  #  geom_vline(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xintercept=forest.end.dist/1000),col='chartreuse4',size=.8)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=forest.end.dist/1000,xmax=sea.start.dist/1000,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+ 
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=sea.start.dist/1000,xmax=sea.end.dist/1000,ymin=-Inf,ymax=Inf),fill='white',alpha=.2)+ 
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=sea.end.dist/1000,xmax=Inf,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+ 
  geom_smooth(data=mean.i.coords,aes(x=as.numeric(i)*100,y=i.var.temp,fill=trip,linetype=sex),size=.1,col='black')+
#  geom_point(data=mean.i.coords,aes(x=as.numeric(i)*100,y=i.var.temp,fill=trip),size=.1,col='black',shape=21)+
  scale_fill_manual(values=c("Autumn"="orangered","Spring"="cornflowerblue"))+
  ## Add star for Alegranza
  ## Layout map
  theme_classic() + xlab("Distance to Colony [km]") + 
  ylab ("\n ") +
  #ylab ("Per sex:\nwithin-individual") +
  coord_cartesian(ylim=c(-1,10))+
  ## Layout text items
  theme(panel.border	    = element_blank(),
        strip.background	= element_rect(fill='white',colour='NA'),
        legend.position   = 'none',
        panel.grid.minor.y   = element_blank(),
        panel.grid.major.y   = element_blank(),
        panel.grid.minor.x   = element_line(size=.2,linetype='dashed',colour='grey40'),
        panel.grid.major.x   = element_line(size=.2,linetype='dashed',colour='grey40'),
        legend.title 	    = element_text(size=11),
        legend.text 	    = element_text(size=10),
        strip.text		    = element_text(size=12,face='bold'),
        axis.text		      = element_text(size=10,),
        axis.title.y		    = element_text(size=11),
        axis.title.x		    = element_text(size=11),
        plot.margin       = unit(c(0.1,0.02,0.02,0.1),"cm"))

### GAMS WITHIN-SEX VARIATION
### ### ### ### ### ### ### ### ### ### ### 
p1b <- ggplot()+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=-Inf,xmax=desert.start.dist/1000,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=desert.start.dist/1000,xmax=desert.end.dist/1000,ymin=-Inf,ymax=Inf),fill='wheat3',alpha=.6)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=desert.end.dist/1000,xmax=forest.start.dist/1000,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=forest.start.dist/1000,xmax=forest.end.dist/1000,ymin=-Inf,ymax=Inf),fill='olivedrab4',alpha=.5)+
  #  geom_vline(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xintercept=forest.start.dist/1000),col='chartreuse4',size=.8)+
  #  geom_vline(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xintercept=forest.end.dist/1000),col='chartreuse4',size=.8)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=forest.end.dist/1000,xmax=sea.start.dist/1000,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+ 
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=sea.start.dist/1000,xmax=sea.end.dist/1000,ymin=-Inf,ymax=Inf),fill='white',alpha=.2)+ 
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=sea.end.dist/1000,xmax=Inf,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+ 
  geom_smooth(data=sexmean.i.coords,aes(x=as.numeric(i)*100,y=i.intrasex.spat,fill=trip,linetype=sex),size=.1,col='black')+
  #  geom_point(data=mean.i.coords,aes(x=as.numeric(i)*100,y=i.var.spat,fill=trip),size=.1,col='black',shape=21)+
  scale_fill_manual(values=c("Autumn"="orangered","Spring"="cornflowerblue"))+
  ## Add star for Alegranza
  ## Layout map
  theme_classic() + xlab("\nDist. to Colony [km]") + 
  ylab ("Per sex:\nbetween-individual") +
  coord_cartesian(ylim=c(-50,800))+
  ## Layout text items
  theme(panel.border	    = element_blank(),
        strip.background	= element_rect(fill='white',colour='NA'),
        legend.position   = 'none',
        panel.grid.minor.y   = element_blank(),
        panel.grid.major.y   = element_blank(),
        panel.grid.minor.x   = element_line(size=.2,linetype='dashed',colour='grey40'),
        panel.grid.major.x   = element_line(size=.2,linetype='dashed',colour='grey40'),
        legend.title 	    = element_text(size=11),
        legend.text 	    = element_text(size=10),
        axis.text		      = element_text(size=10,),
        axis.title.y		    = element_text(size=11),
        axis.title.x		    = element_blank(),
        plot.margin       = unit(c(0.1,0.02,0.02,0.1),"cm"))

p2b <- ggplot()+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=-Inf,xmax=desert.start.dist/1000,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=desert.start.dist/1000,xmax=desert.end.dist/1000,ymin=-Inf,ymax=Inf),fill='wheat3',alpha=.6)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=desert.end.dist/1000,xmax=forest.start.dist/1000,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=forest.start.dist/1000,xmax=forest.end.dist/1000,ymin=-Inf,ymax=Inf),fill='olivedrab4',alpha=.5)+
  #  geom_vline(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xintercept=forest.start.dist/1000),col='chartreuse4',size=.8)+
  #  geom_vline(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xintercept=forest.end.dist/1000),col='chartreuse4',size=.8)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=forest.end.dist/1000,xmax=sea.start.dist/1000,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+ 
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=sea.start.dist/1000,xmax=sea.end.dist/1000,ymin=-Inf,ymax=Inf),fill='white',alpha=.2)+ 
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=sea.end.dist/1000,xmax=Inf,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+ 
  geom_smooth(data=sexmean.i.coords,aes(x=as.numeric(i)*100,y=i.intrasex.temp,fill=trip,linetype=sex),size=.1,col='black')+
  #  geom_point(data=mean.i.coords,aes(x=as.numeric(i)*100,y=i.var.temp,fill=trip),size=.1,col='black',shape=21)+
  scale_fill_manual(values=c("Autumn"="orangered","Spring"="cornflowerblue"))+
  ## Add star for Alegranza
  ## Layout map
  theme_classic() + xlab("\nDist. to Colony [km]") + 
  ylab ("\n ") +
  #ylab ("Per sex:\nbetween-individual") +
  coord_cartesian(ylim=c(-1,10))+
  ## Layout text items
  theme(panel.border	    = element_blank(),
        strip.background	= element_rect(fill='white',colour='NA'),
        legend.position   = 'none',
        panel.grid.minor.y   = element_blank(),
        panel.grid.major.y   = element_blank(),
        panel.grid.minor.x   = element_line(size=.2,linetype='dashed',colour='grey40'),
        panel.grid.major.x   = element_line(size=.2,linetype='dashed',colour='grey40'),
        legend.title 	    = element_text(size=11),
        legend.text 	    = element_text(size=10),
        strip.text		    = element_text(size=12,face='bold'),
        axis.text		      = element_text(size=10,),
        axis.title.y		    = element_text(size=11),
        axis.title.x		    = element_blank(),
        plot.margin       = unit(c(0.1,0.02,0.02,0.1),"cm"))

### GAMS BETWEEN-INDIVIDUAL VARIATION
### ### ### ### ### ### ### ### ### ### ### 
p3 <- ggplot()+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=-Inf,xmax=desert.start.dist/1000,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=desert.start.dist/1000,xmax=desert.end.dist/1000,ymin=-Inf,ymax=Inf),fill='wheat3',alpha=.6)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=desert.end.dist/1000,xmax=forest.start.dist/1000,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=forest.start.dist/1000,xmax=forest.end.dist/1000,ymin=-Inf,ymax=Inf),fill='olivedrab4',alpha=.5)+
  #  geom_vline(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xintercept=forest.start.dist/1000),col='chartreuse4',size=.8)+
  #  geom_vline(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xintercept=forest.end.dist/1000),col='chartreuse4',size=.8)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=forest.end.dist/1000,xmax=sea.start.dist/1000,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+ 
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=sea.start.dist/1000,xmax=sea.end.dist/1000,ymin=-Inf,ymax=Inf),fill='white',alpha=.2)+ 
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=sea.end.dist/1000,xmax=Inf,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+ 
  geom_smooth(data=popmean.i.coords,aes(x=as.numeric(i)*100,y=i.popvar.spat,fill=trip),size=.1,col='black')+
  #  geom_point(data=popmean.i.coords,aes(x=as.numeric(i)*100,y=i.popvar.spat,fill=trip),size=.1,col='black',shape=21)+
  scale_fill_manual(values=c("Autumn"="orangered","Spring"="cornflowerblue"))+
  ## Add star for Alegranza
  ## Layout map
  theme_classic() + xlab("\nDist. to Colony [km]") + 
  ylab ("All birds:\nbetween-individual") +
  coord_cartesian(ylim=c(-50,800))+
  ## Layout text items
  theme(panel.border	    = element_blank(),
        strip.background	= element_rect(fill='white',colour='NA'),
        legend.position   = 'none',
        panel.grid.minor.y   = element_blank(),
        panel.grid.major.y   = element_blank(),
        panel.grid.minor.x   = element_line(size=.2,linetype='dashed',colour='grey40'),
        panel.grid.major.x   = element_line(size=.2,linetype='dashed',colour='grey40'),
        legend.title 	    = element_text(size=11),
        legend.text 	    = element_text(size=10),
        axis.text		    = element_text(size=10),
        axis.title.x		    = element_blank(),
        axis.title.y		    = element_text(size=11),
        plot.margin       = unit(c(0.1,0.02,0.02,0.1),"cm"))

p4 <- ggplot()+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=-Inf,xmax=desert.start.dist/1000,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=desert.start.dist/1000,xmax=desert.end.dist/1000,ymin=-Inf,ymax=Inf),fill='wheat3',alpha=.6)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=desert.end.dist/1000,xmax=forest.start.dist/1000,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=forest.start.dist/1000,xmax=forest.end.dist/1000,ymin=-Inf,ymax=Inf),fill='olivedrab4',alpha=.5)+
  #  geom_vline(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xintercept=forest.start.dist/1000),col='chartreuse4',size=.8)+
  #  geom_vline(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xintercept=forest.end.dist/1000),col='chartreuse4',size=.8)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=forest.end.dist/1000,xmax=sea.start.dist/1000,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+ 
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=sea.start.dist/1000,xmax=sea.end.dist/1000,ymin=-Inf,ymax=Inf),fill='white',alpha=.2)+ 
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=sea.end.dist/1000,xmax=Inf,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+ 
  geom_smooth(data=popmean.i.coords,aes(x=as.numeric(i)*100,y=i.popvar.temp,fill=trip),size=.1,col='black')+
#  geom_point(data=popmean.i.coords,aes(x=as.numeric(i)*100,y=i.popvar.temp,fill=trip),size=.1,col='black',shape=21)+
  scale_fill_manual(values=c("Autumn"="orangered","Spring"="cornflowerblue"))+
  ## Add star for Alegranza
  ## Layout map
  theme_classic() + xlab("\nDist. to Colony [km]") + 
  ylab ("\n ") +
  #ylab ("All birds:\nbetween-individual") +
  coord_cartesian(ylim=c(-1,10))+
  ## Layout text items
  theme(panel.border	    = element_blank(),
        strip.background	= element_rect(fill='white',colour='NA'),
        legend.position   = 'none',
        panel.grid.minor.y   = element_blank(),
        panel.grid.major.y   = element_blank(),
        panel.grid.minor.x   = element_line(size=.2,linetype='dashed',colour='grey40'),
        panel.grid.major.x   = element_line(size=.2,linetype='dashed',colour='grey40'),
        legend.title 	    = element_text(size=11),
        legend.text 	    = element_text(size=10),
        strip.text		    = element_text(size=12,face='bold'),
        axis.text		      = element_text(size=10,),
        axis.title.y		    = element_text(size=11),
        axis.title.x		    = element_blank(),
        plot.margin       = unit(c(0.1,0.02,0.02,0.1),"cm"))


### GAMS BETWEEN-SEX VARIATION
### ### ### ### ### ### ### ### ### ### ### 
p3b <- ggplot()+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=-Inf,xmax=desert.start.dist/1000,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=desert.start.dist/1000,xmax=desert.end.dist/1000,ymin=-Inf,ymax=Inf),fill='wheat3',alpha=.6)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=desert.end.dist/1000,xmax=forest.start.dist/1000,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=forest.start.dist/1000,xmax=forest.end.dist/1000,ymin=-Inf,ymax=Inf),fill='olivedrab4',alpha=.5)+
  #  geom_vline(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xintercept=forest.start.dist/1000),col='chartreuse4',size=.8)+
  #  geom_vline(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xintercept=forest.end.dist/1000),col='chartreuse4',size=.8)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=forest.end.dist/1000,xmax=sea.start.dist/1000,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+ 
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=sea.start.dist/1000,xmax=sea.end.dist/1000,ymin=-Inf,ymax=Inf),fill='white',alpha=.2)+ 
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=sea.end.dist/1000,xmax=Inf,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+ 
  geom_smooth(data=popmean.i.coords,aes(x=as.numeric(i)*100,y=i.intersex.spat,fill=trip),size=.1,col='black')+
  #  geom_point(data=popmean.i.coords,aes(x=as.numeric(i)*100,y=i.popvar.spat,fill=trip),size=.1,col='black',shape=21)+
  scale_fill_manual(values=c("Autumn"="orangered","Spring"="cornflowerblue"))+
  ## Add star for Alegranza
  ## Layout map
  theme_classic() + xlab("\nDist. to Colony [km]") + 
  ylab ("\n ") +
  ylab ("All birds:\nbetween-sex") +
  coord_cartesian(ylim=c(-50,800))+
  ## Layout text items
  theme(panel.border	    = element_blank(),
        strip.background	= element_rect(fill='white',colour='NA'),
        legend.position   = 'none',
        panel.grid.minor.y   = element_blank(),
        panel.grid.major.y   = element_blank(),
        panel.grid.minor.x   = element_line(size=.2,linetype='dashed',colour='grey40'),
        panel.grid.major.x   = element_line(size=.2,linetype='dashed',colour='grey40'),
        legend.title 	    = element_text(size=11),
        legend.text 	    = element_text(size=10),
        axis.text		    = element_text(size=10),
        axis.title.x		    = element_blank(),
        axis.title.y		    = element_text(size=11),
        plot.margin       = unit(c(0.1,0.02,0.02,0.1),"cm"))

p4b <- ggplot()+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=-Inf,xmax=desert.start.dist/1000,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=desert.start.dist/1000,xmax=desert.end.dist/1000,ymin=-Inf,ymax=Inf),fill='wheat3',alpha=.6)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=desert.end.dist/1000,xmax=forest.start.dist/1000,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=forest.start.dist/1000,xmax=forest.end.dist/1000,ymin=-Inf,ymax=Inf),fill='olivedrab4',alpha=.5)+
  #  geom_vline(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xintercept=forest.start.dist/1000),col='chartreuse4',size=.8)+
  #  geom_vline(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xintercept=forest.end.dist/1000),col='chartreuse4',size=.8)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=forest.end.dist/1000,xmax=sea.start.dist/1000,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+ 
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=sea.start.dist/1000,xmax=sea.end.dist/1000,ymin=-Inf,ymax=Inf),fill='white',alpha=.2)+ 
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=sea.end.dist/1000,xmax=Inf,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+ 
  geom_smooth(data=popmean.i.coords,aes(x=as.numeric(i)*100,y=i.intersex.temp,fill=trip),size=.1,col='black')+
  #  geom_point(data=popmean.i.coords,aes(x=as.numeric(i)*100,y=i.popvar.temp,fill=trip),size=.1,col='black',shape=21)+
  scale_fill_manual(values=c("Autumn"="orangered","Spring"="cornflowerblue"))+
  ## Add star for Alegranza
  ## Layout map
  theme_classic() + xlab("\nDist. to Colony [km]") + 
  ylab ("\n ") +
  #ylab ("All birds:\nbetween-sex") +
  coord_cartesian(ylim=c(-1,10))+
  ## Layout text items
  theme(panel.border	    = element_blank(),
        strip.background	= element_rect(fill='white',colour='NA'),
        legend.position   = 'none',
        panel.grid.minor.y   = element_blank(),
        panel.grid.major.y   = element_blank(),
        panel.grid.minor.x   = element_line(size=.2,linetype='dashed',colour='grey40'),
        panel.grid.major.x   = element_line(size=.2,linetype='dashed',colour='grey40'),
        legend.title 	    = element_text(size=11),
        legend.text 	    = element_text(size=10),
        strip.text		    = element_text(size=12,face='bold'),
        axis.text		      = element_text(size=10,),
        axis.title.y		    = element_text(size=11),
        axis.title.x		    = element_blank(),
        plot.margin       = unit(c(0.1,0.02,0.02,0.1),"cm"))

#pcomp <- cowplot::plot_grid(pa,pb,p3,p4,p3b,p4b,p1b,p2b,p1,p2,ncol=2,align="v",rel_heights = c(5, .35))
pspat <- cowplot::plot_grid(pa,p3,p3b,p1b,p1,ncol=1,align="v",rel_heights = c(1.2,1,1,1,1.2),labels=c("a","b","c","d","e"))
ptemp<- cowplot::plot_grid(pb,p4,p4b,p2b,p2,ncol=1,align="v",rel_heights = c(1.2,1,1,1,1.2))

pcomp <- cowplot::plot_grid(pspat,ptemp,ncol=2,align="v",axis="l")

# extract the legend from one of the plots
legend <- get_legend(
  # create some space to the left of the legend
  p1 + theme(legend.position = "bottom",
             legend.direction = "horizontal",
             legend.title 	    = element_text(size=10,face='bold'),
             legend.text 	    = element_text(size=9))+
    guides(fill = guide_legend("Season",title.position = "top"),
           linetype = guide_legend("Sex",title.position = "top"))
)

# add the legend to the row we made earlier. 
pfin <- cowplot::plot_grid(pcomp, legend, ncol=1,rel_heights = c(1, .1))

# save plot as Fig2
ggsave(plot=pfin,filename='./output - graphs/Fig2_GAMS-v20220425.tiff',width=8.5,height=10,dpi=300)
rm(pcomp,legend,pa,pb,p3,p4,p3b,p4b,p1b,p2b,p1,p2,pfin)


