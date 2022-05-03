
#####################################################
### ADD PREBREEDING DATA TO MIGRATION DATA        ###  
#####################################################
aut <- subset(stop.tracks.full,stop.tracks.full$trip == 'Autumn' & !(stop.tracks.full$dt %in% c(stop.tracks.full$region.stop.start,stop.tracks.full$region.stop.end)))
aut <- aut[order(aut$dev,aut$dt),]
spr <- subset(stop.tracks.full,stop.tracks.full$trip == 'Spring' & !(stop.tracks.full$dt %in% c(stop.tracks.full$region.stop.start,stop.tracks.full$region.stop.end)))
spr <- spr[order(spr$dev,spr$dt),]

# create the breaks- and label vectors for coordinate axes
ewbrks <- seq(-20,45,5)
nsbrks <- seq(-20,35,5)
ewlbls <- unlist(lapply(ewbrks, function(x) ifelse(x < 0, paste(-x,"°W",sep=""), ifelse(x > 0, paste(x,"°E",sep=""),x))))
nslbls <- unlist(lapply(nsbrks, function(x) ifelse(x < 0, paste(-x,"°S",sep=""), ifelse(x > 0, paste(x,"°N",sep=""),x))))

# create basemap
basemap.spring <- ggplot()+
  ## Add biomes (fill) and elevation (alpha)
  geom_raster(data=hdf3[which(is.na(hdf3$biome2)==FALSE & hdf3$trip == 'Spring'),],mapping=aes(long,lat,alpha=alt,fill=biome2))+
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
  ## Layout map
  scale_x_continuous(breaks = ewbrks, labels = ewlbls, expand = c(0, 0)) +
  scale_y_continuous(breaks = nsbrks, labels = nslbls, expand = c(0, 0)) +
  theme_bw() + 
  # Layout text items
  theme(panel.border	    = element_blank(),
        strip.background	= element_rect(fill='white',colour='NA'),
        strip.text        = element_text(size=12,face='bold'),
        legend.position   = "none",
        panel.grid        = element_blank(),
        axis.text		      = element_text(size=10),
        #     axis.title		    = element_text(size=11,face='bold').
        axis.title        = element_blank())


# create basemap autumn
basemap.autumn <- ggplot()+
  ## Add biomes (fill) and elevation (alpha)
  geom_raster(data=hdf3[which(is.na(hdf3$biome2)==FALSE & hdf3$trip == 'Autumn'),],mapping=aes(long,lat,alpha=alt,fill=biome2))+
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
  ## Layout map
  scale_x_continuous(breaks = ewbrks, labels = ewlbls, expand = c(0, 0)) +
  scale_y_continuous(breaks = nsbrks, labels = nslbls, expand = c(0, 0)) +
  theme_bw() + 
  # Layout text items
  theme(panel.border	    = element_blank(),
        strip.background	= element_rect(fill='white',colour='NA'),
        strip.text        = element_text(size=12,face='bold'),
        legend.position   = "none",
        panel.grid        = element_blank(),
        axis.text		      = element_text(size=10),
        axis.title        = element_blank())


basemap.north.aut <- basemap.autumn + coord_quickmap(xlim=c(-16,-3),ylim=c(22,36),expand=FALSE) 
basemap.sahel.aut <- basemap.autumn + coord_quickmap(xlim=c(-15,20),ylim=c(7,15),expand=FALSE) 
basemap.east.aut <- basemap.autumn + coord_quickmap(xlim=c(25,45),ylim=c(-18,-1),expand=FALSE) 

basemap.north.spr <- basemap.spring + coord_quickmap(xlim=c(-16,-3),ylim=c(22,36),expand=FALSE) 
basemap.sahel.spr <- basemap.spring + coord_quickmap(xlim=c(-15,20),ylim=c(7,15),expand=FALSE) 
basemap.horn.spr <- basemap.spring+ coord_quickmap(xlim=c(25,45),ylim=c(-4,13),expand=FALSE) 


#> unique(mean.i.coords$dev)
#[1] "B1011" "B1012" "B1013" "B1014" "B2048" "B2051" "B2368" "B2391" "B2392" "B2400" "B2423"
#[12] "B2424" "B2453"
selection <- c("B1011","B1014","B2453","B2400","B2051")  

#####################################
### MAP STOP-OVERS PER ID         ###  
#####################################
# NW Africa - spring 
p1 <- basemap.north.aut + 
  geom_path(data=aut[which(!(aut$dev %in% selection)),],aes(x=long,y=lat,group=paste(dev,yr,region)),col='grey20',size=.2,alpha=.75)+
  geom_path(data=aut[which(aut$dev %in% selection),],aes(x=long,y=lat,col=dev,group=paste(dev,yr,region)),size=.5,alpha=.8)+
  scale_colour_viridis_d(name="Bird ID",option="magma")+
  # Add stop-overs
  geom_point(data=stops[which(!(stops$dev %in% selection) & stops$trip == 'Autumn'),],aes(x=st.long,y=st.lat,size=round(as.numeric(st.dur))),col='grey20',fill='white',alpha=.4,shape=21)+
  geom_point(data=stops[which(!(stops$dev %in% selection) & stops$trip == 'Autumn'),],aes(x=st.long,y=st.lat),col='grey60',size=.3)+
  geom_point(data=stops[which(stops$dev %in% selection & stops$trip == 'Autumn'),],aes(x=st.long,y=st.lat,size=round(as.numeric(st.dur)),col=dev),fill='white',alpha=.4,shape=21)+
  geom_point(data=stops[which(stops$dev %in% selection & stops$trip == 'Autumn'),],aes(x=st.long,y=st.lat,col=dev),size=.3)+
  scale_size_binned(name="Stop duration [days]",range=c(3,8),breaks=c(1,3,7,14),labels=c("1","2-3","4-7","8-14"))#+

# NW Africa - autumn 
p2 <- basemap.north.spr + 
  geom_path(data=spr[which(!(spr$dev %in% selection)),],aes(x=long,y=lat,group=paste(dev,yr,region)),col='grey20',size=.2,alpha=.75)+
  geom_path(data=spr[which(spr$dev %in% selection),],aes(x=long,y=lat,col=dev,group=paste(dev,yr,region)),size=.5,alpha=.8)+
  scale_colour_viridis_d(name="Bird ID",option="magma")+
  # Add stop-overs
  geom_point(data=stops[which(!(stops$dev %in% selection) & stops$trip == 'Spring'),],aes(x=st.long,y=st.lat,size=round(as.numeric(st.dur))),col='grey20',fill='white',alpha=.4,shape=21)+
  geom_point(data=stops[which(!(stops$dev %in% selection) & stops$trip == 'Spring'),],aes(x=st.long,y=st.lat),col='grey60',size=.3)+
  geom_point(data=stops[which(stops$dev %in% selection & stops$trip == 'Spring'),],aes(x=st.long,y=st.lat,size=round(as.numeric(st.dur)),col=dev),fill='white',alpha=.4,shape=21)+
  geom_point(data=stops[which(stops$dev %in% selection & stops$trip == 'Spring'),],aes(x=st.long,y=st.lat,col=dev),size=.3)+
  scale_size_binned(name="Stop duration [days]",range=c(3,8),breaks=c(1,3,7,14),labels=c("1","2-3","4-7","8-14"))#+

# Sahel- autumn 
p3 <- basemap.sahel.aut + 
  geom_path(data=aut[which(!(aut$dev %in% selection)),],aes(x=long,y=lat,group=paste(dev,yr,region)),col='grey20',size=.2,alpha=.75)+
  geom_path(data=aut[which(aut$dev %in% selection),],aes(x=long,y=lat,col=dev,group=paste(dev,yr,region)),size=.5,alpha=.8)+
  scale_colour_viridis_d(name="Bird ID",option="magma")+
  # Add stop-overs
  geom_point(data=stops[which(!(stops$dev %in% selection) & stops$trip == 'Autumn'),],aes(x=st.long,y=st.lat,size=round(as.numeric(st.dur))),col='grey20',fill='white',alpha=.4,shape=21)+
  geom_point(data=stops[which(!(stops$dev %in% selection) & stops$trip == 'Autumn'),],aes(x=st.long,y=st.lat),col='grey60',size=.3)+
  geom_point(data=stops[which(stops$dev %in% selection & stops$trip == 'Autumn'),],aes(x=st.long,y=st.lat,size=round(as.numeric(st.dur)),col=dev),fill='white',alpha=.4,shape=21)+
  geom_point(data=stops[which(stops$dev %in% selection & stops$trip == 'Autumn'),],aes(x=st.long,y=st.lat,col=dev),size=.3)+
  scale_size_binned(name="Stop duration [days]",range=c(3,8),breaks=c(1,3,7,14),labels=c("1","2-3","4-7","8-14"))#+


# Sahel - autumn 
p4 <- basemap.sahel.spr + 
  geom_path(data=spr[which(!(spr$dev %in% selection)),],aes(x=long,y=lat,group=paste(dev,yr,region)),col='grey20',size=.2,alpha=.75)+
  geom_path(data=spr[which(spr$dev %in% selection),],aes(x=long,y=lat,col=dev,group=paste(dev,yr,region)),size=.5,alpha=.8)+
  scale_colour_viridis_d(name="Bird ID",option="magma")+
  # Add stop-overs
  geom_point(data=stops[which(!(stops$dev %in% selection) & stops$trip == 'Spring'),],aes(x=st.long,y=st.lat,size=round(as.numeric(st.dur))),col='grey20',fill='white',alpha=.4,shape=21)+
  geom_point(data=stops[which(!(stops$dev %in% selection) & stops$trip == 'Spring'),],aes(x=st.long,y=st.lat),col='grey60',size=.3)+
  geom_point(data=stops[which(stops$dev %in% selection & stops$trip == 'Spring'),],aes(x=st.long,y=st.lat,size=round(as.numeric(st.dur)),col=dev),fill='white',alpha=.4,shape=21)+
  geom_point(data=stops[which(stops$dev %in% selection & stops$trip == 'Spring'),],aes(x=st.long,y=st.lat,col=dev),size=.3)+
  scale_size_binned(name="Stop duration [days]",range=c(3,8),breaks=c(1,3,7,14),labels=c("1","2-3","4-7","8-14"))#+

# East Africa - autumn 
p5 <- basemap.east.aut + 
  geom_path(data=aut[which(!(aut$dev %in% selection)),],aes(x=long,y=lat,group=paste(dev,yr,region)),col='grey20',size=.2,alpha=.75)+
  geom_path(data=aut[which(aut$dev %in% selection),],aes(x=long,y=lat,col=dev,group=paste(dev,yr,region)),size=.5,alpha=.8)+
  scale_colour_viridis_d(name="Bird ID",option="magma")+
  # Add stop-overs
  geom_point(data=stops[which(!(stops$dev %in% selection) & stops$trip == 'Autumn'),],aes(x=st.long,y=st.lat,size=round(as.numeric(st.dur))),col='grey20',fill='white',alpha=.4,shape=21)+
  geom_point(data=stops[which(!(stops$dev %in% selection) & stops$trip == 'Autumn'),],aes(x=st.long,y=st.lat),col='grey60',size=.3)+
  geom_point(data=stops[which(stops$dev %in% selection & stops$trip == 'Autumn'),],aes(x=st.long,y=st.lat,size=round(as.numeric(st.dur)),col=dev),fill='white',alpha=.4,shape=21)+
  geom_point(data=stops[which(stops$dev %in% selection & stops$trip == 'Autumn'),],aes(x=st.long,y=st.lat,col=dev),size=.3)+
  scale_size_binned(name="Stop duration [days]",range=c(3,8),breaks=c(1,3,7,14),labels=c("1","2-3","4-7","8-14"))#+

# East Africa - spring 
p6 <- basemap.horn.spr + 
  geom_path(data=spr[which(!(spr$dev %in% selection)),],aes(x=long,y=lat,group=paste(dev,yr,region)),col='grey20',size=.2,alpha=.75)+
  geom_path(data=spr[which(spr$dev %in% selection),],aes(x=long,y=lat,col=dev,group=paste(dev,yr,region)),size=.5,alpha=.8)+
  scale_colour_viridis_d(name="Bird ID",option="magma")+
  # Add stop-overs
  geom_point(data=stops[which(!(stops$dev %in% selection) & stops$trip == 'Spring'),],aes(x=st.long,y=st.lat,size=round(as.numeric(st.dur))),col='grey20',fill='white',alpha=.4,shape=21)+
  geom_point(data=stops[which(!(stops$dev %in% selection) & stops$trip == 'Spring'),],aes(x=st.long,y=st.lat),col='grey60',size=.3)+
  geom_point(data=stops[which(stops$dev %in% selection & stops$trip == 'Spring'),],aes(x=st.long,y=st.lat,size=round(as.numeric(st.dur)),col=dev),fill='white',alpha=.4,shape=21)+
  geom_point(data=stops[which(stops$dev %in% selection & stops$trip == 'Spring'),],aes(x=st.long,y=st.lat,col=dev),size=.3)+
  scale_size_binned(name="Stop duration [days]",range=c(3,8),breaks=c(1,3,7,14),labels=c("1","2-3","4-7","8-14"))#+

# extract the legend from one of the plots
legend <- get_legend(
  # create some space to the left of the legend
  p6 + theme(legend.position = "bottom",
             legend.direction = "horizontal",
             legend.title 	    = element_text(size=10,face='bold'),
             legend.text 	    = element_text(size=9))+
    guides(colour= guide_legend(order =1,nrow =3,title.position='top'),
           size = guide_legend(order =2,nrow =2,title.position='top'),
           fill=guide_legend(order=3,nrow =3,title.position='top',override.aes= list(alpha = .4)),
           alpha=guide_legend(order = 4,title.position='top',nrow=3,override.aes=list(fill='grey30')))
)

north <- cowplot::plot_grid(p1,p2,ncol=2,align="v",axis="lr",labels = c("Autumn","Spring"))
north.fin <- cowplot::plot_grid(north, legend, ncol=1,rel_heights = c(1, .2))
ggsave(plot=north.fin,filename='./output - maps/FigSx_StopOvers_NWAfrica.tiff',dpi=300,width=10,height=7)

sahel <- cowplot::plot_grid(p3,p4,ncol=1,align="v",axis="lr",labels = c("Autumn","Spring"))
sahel.fin <- cowplot::plot_grid(sahel, legend, ncol=1,rel_heights = c(1, .17))
ggsave(plot=sahel.fin,filename='./output - maps/FigSx_StopOvers_Sahel.tiff',dpi=300,width=10,height=7)

east <- cowplot::plot_grid(p5,p6,ncol=2,align="v",axis="lr",labels = c("Autumn","Spring"))
east.fin <- cowplot::plot_grid(east, legend, ncol=1,rel_heights = c(1, .2))
ggsave(plot=east.fin,filename='./output - maps/FigSx_StopOvers_EAfrica.tiff',dpi=300,width=10,height=5.5)


rm(p1,p2,p3,p4,p5,p6,legend)
rm(basemap.north.aut,basemap.north.spr,basemap.east.aut,basemap.horn.spr,basemap.sahel.aut,basemap.sahel.spr,basemap.autumn,basemap.spring)
rm(north,sahel,east,north.fin,sahel.fin,east.fin)
rm(aut,spr)
rm(ewbrks,ewlbls,nsbrks,nslbls)
