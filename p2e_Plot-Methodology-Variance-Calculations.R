#### MAP INTRA AND INTER INDIVIDUAL/SEXUAL SPATIAL VARIABILITY 
###############################################################
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
        legend.position   = "right",
        legend.direction  = 'vertical',
        legend.title 	    = element_text(size=9,face='bold'),
        legend.text 	    = element_text(size=9),
        panel.grid        = element_blank(),
        axis.text		      = element_text(size=10),
        axis.title        = element_blank(),
        plot.margin       = unit(c(0,0,0,0),"cm"))+
  # Add white box bottom right and lay-out legend
  #  annotate(geom="rect",xmin=longlimits[1],xmax=7.2,ymin=latlimits[1],ymax=7.1,col='black',fill='white')+
  guides(colour= guide_colorbar(order =1,title.position='top'),
         size = guide_legend(order =2,nrow =2,title.position='top'),
         fill=guide_legend(order=3,nrow =3,title.position='top',override.aes= list(alpha = .4)),
         alpha=guide_legend(order = 4,title.position='top',nrow=3,override.aes=list(fill='grey30')))

# WITHIN-INDIVIDUAL SPATIAL VARIANCE
p1b <- basemap+
  # add tracking data
  geom_path(data=data[which(data$n.trips > 1),],aes(x=long,y=lat,group=tripID),size=.1,col='grey30')+
  geom_path(data=mean.i.coords,aes(x=i.mean.long,y=i.mean.lat,col=i.var.spat,group=paste(dev,trip)),size=0.3)+
  scale_colour_viridis_c(option="C", limits=c(0,1100))+
  theme(strip.text = element_text(size=12,face='bold'),
        strip.background	= element_rect(fill='white',colour='NA'))+
  guides(alpha = FALSE,
         fill = FALSE,
         colour = guide_colorbar(title ="Within-individual\nRoute Variation [km]",ncol = 1, title.position='top'))

# BETWEEN-INDIVIDUAL SPATIAL VARIANCE
p2b <- basemap +
  # add tracking data
  geom_path(data=mean.i.coords,aes(x=i.mean.long,y=i.mean.lat,group=dev),size=.2,col='grey30')+
  geom_path(data=popmean.i.coords,aes(x=i.popmean.long,y=i.popmean.lat,col=i.popvar.spat,group=paste(trip)),size=0.8)+
  scale_colour_viridis_c(option="C", limits=c(0,1100))+
  theme(strip.text = element_blank(),
        strip.background = element_blank())+
  guides(alpha = FALSE,
         fill = FALSE,
         colour = guide_colorbar(title ="Between-individual\nRoute Variation [km]",ncol = 1, title.position='top'))

# BETWEEN-CONGENERIC SPATIAL VARIANCE
p3b <- basemap +
  # add tracking data
  geom_path(data=mean.i.coords,aes(x=i.mean.long,y=i.mean.lat,group=dev),size=.2,col='grey30')+
  geom_path(data=sexmean.i.coords,aes(x=i.sexmean.long,y=i.popmean.lat,col=i.intrasex.spat,group=paste(trip,sex)),size=0.8)+
  scale_colour_viridis_c(option="C", limits=c(0,1100))+
  theme(strip.text = element_blank(),
        strip.background = element_blank())+
  guides(alpha = FALSE,
         fill = FALSE,
         colour = guide_colorbar(title ="Between-individual\nRoute Variation\nPer Sex[km]",ncol = 1, title.position='top'))

# BETWEEN-SEX SPATIAL VARIANCE
p4b <- basemap +
  # add tracking data
  geom_path(data=sexmean.i.coords,aes(x=i.sexmean.long,y=i.sexmean.lat,group=paste(trip,sex)),size=.2,col='grey30')+
  geom_path(data=popmean.i.coords,aes(x=i.popmean.long,y=i.popmean.lat,col=i.intersex.spat,group=paste(trip)),size=0.8)+
  scale_colour_viridis_c(option="C", limits=c(0,1100))+
  theme(strip.text = element_blank(),
        strip.background = element_blank())+
  guides(alpha = FALSE,
         fill = FALSE,
         colour = guide_colorbar(title ="Between-sex\nRoute Variation [km]",ncol = 1, title.position='top'))



pfin <- cowplot::plot_grid(p1b,p2b,p3b,p4b,ncol=1,nrow=4,align="h",axis="l",labels=c("a","b","c","d"))
ggsave(plot=pfin,filename='./output - maps/FigS5_Spatial-Variances-v20220426.pdf',width=8,height=12)

rm(p1a,p1b,p2a,p2b,p3a,p3b,pfin)



#### MAP INTRA AND INTER TEMP VARIABILITY FOR ALL IDS
#################################################
base.autumn <- ggplot()+
  ## Add approximation of biomes
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(ymin=0,ymax=desert.start.dist/1000,xmin=-Inf,xmax=Inf),fill='grey30',alpha=.3)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(ymin=desert.start.dist/1000,ymax=desert.end.dist/1000,xmin=-Inf,xmax=Inf),fill='wheat3',alpha=.6)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(ymin=desert.end.dist/1000,ymax=forest.start.dist/1000,xmin=-Inf,xmax=Inf),fill='grey30',alpha=.2)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(ymin=forest.start.dist/1000,ymax=forest.end.dist/1000,xmin=-Inf,xmax=Inf),fill='olivedrab4',alpha=.5)+
#  geom_hline(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(yintercept=forest.start.dist/1000),col='chartreuse4',linetype='solid',size=.4)+
#  geom_hline(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(yintercept=forest.end.dist/1000),col='chartreuse4',linetype='solid',size=.4)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(ymin=forest.end.dist/1000,ymax=sea.start.dist/1000,xmin=-Inf,xmax=Inf),fill='grey30',alpha=.2)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(ymin=sea.start.dist/1000,ymax=sea.end.dist/1000,xmin=-Inf,xmax=Inf),fill='white',alpha=.2)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(ymin=sea.end.dist/1000,ymax=Inf,xmin=-Inf,xmax=Inf),fill='grey30',alpha=.2)+
  ## Layout graph
  coord_cartesian(expand=FALSE) + 
  scale_y_reverse() +
  scale_x_continuous(breaks=c(92,123,153,184),labels=c("Oct","Nov","Dec","Jan"),limits=c(80,180),position='top')+
  theme_bw() + xlab("Autumn") + ylab ("Distance to colony [km]") + 
  ## Layout text items
  theme(panel.border	    = element_blank(),
        legend.position   = 'none',
        panel.grid.minor.x   = element_blank(),
        panel.grid.major.x   = element_blank(),
        panel.grid.minor.y   = element_line(size=.2,linetype='dashed',colour='grey40'),
        panel.grid.major.y   = element_line(size=.2,linetype='dashed',colour='grey40'),
        axis.line         = element_line(size=.4),
        axis.text		      = element_text(size=10),
        axis.title.y		    = element_text(size=11),
        plot.margin     = unit(c(0.05,0.05,0.05,0.05),"cm"))

base.spring <- ggplot()+
  ## Add approximation of biomes
  geom_rect(data=trip.dists[which(trip.dists$trip == "Spring"),],aes(ymin=0,ymax=desert.end.dist/1000,xmin=-Inf,xmax=Inf),fill='grey30',alpha=.3)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Spring"),],aes(ymin=desert.end.dist/1000,ymax=desert.start.dist/1000,xmin=-Inf,xmax=Inf),fill='wheat3',alpha=.6)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Spring"),],aes(ymin=sea.end.dist/1000,ymax=desert.start.dist/1000,xmin=-Inf,xmax=Inf),fill='grey30',alpha=.3)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Spring"),],aes(ymin=sea.start.dist/1000,ymax=sea.end.dist/1000,xmin=-Inf,xmax=Inf),fill='white',alpha=.3)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Spring"),],aes(ymin=sea.start.dist/1000,ymax=Inf,xmin=-Inf,xmax=Inf),fill='grey30',alpha=.3)+
  ## Layout graph
  coord_cartesian(expand=FALSE) +  scale_y_reverse() +
  scale_x_continuous(breaks=c(274,304,335),labels=c("Apr","May","Jun"),limits=c(262,362),position='top')+
  theme_bw() + 
  xlab("Spring") + ylab ("Distance to colony [km]") + 
  ## Layout text items
  theme(panel.border	    = element_blank(),
        legend.position   = 'none',
        panel.grid.minor.x   = element_blank(),
        panel.grid.major.x   = element_blank(),
        panel.grid.minor.y   = element_line(size=.2,linetype='dashed',colour='grey40'),
        panel.grid.major.y   = element_line(size=.2,linetype='dashed',colour='grey40'),
        axis.line         = element_line(size=.4),
        axis.text		      = element_text(size=10),
        axis.title.y       = element_blank(),
        plot.margin     = unit(c(0.05,0.05,0.05,0.05),"cm"))
  
## Within-individual temporal variation
p1a <- base.autumn +
  # add tracking data
  geom_path(data=data[which(data$n.trips > 1),],aes(y=dist.to.colony/1000,x=cycle.time,group=tripID),size=.1,col='grey30')+
  geom_path(data=mean.i.coords,aes(x=i.mean.cycle.time,y=as.numeric(i)*100,col=i.var.temp,group=paste(dev,trip)),size=0.3)+
  scale_colour_viridis_c(option="C", limits=c(0,13))+
  theme(axis.title.x = element_text(size=12,face='bold'))

p1b <- base.spring +
  # add tracking data
  geom_path(data=data[which(data$n.trips > 1),],aes(y=dist.to.colony/1000,x=cycle.time,group=tripID),size=.1,col='grey30')+
  geom_path(data=mean.i.coords,aes(x=i.mean.cycle.time,y=as.numeric(i)*100,col=i.var.temp,group=paste(dev,trip)),size=0.3)+
  scale_colour_viridis_c(option="C", limits=c(0,13))+
  theme(axis.title.x = element_text(size=12,face='bold'))

p1 <- cowplot::plot_grid(p1a,p1b,ncol=2,align="v",labels=c("a",""))

# extract the legend from one of the plots
legend.p1 <- get_legend(
  # create some space to the left of the legend
  p1b + theme(legend.position = "right",
             legend.direction = "vertical",
             legend.title 	    = element_text(size=10,face='bold'),
             legend.text 	    = element_text(size=9))+
    guides(colour = guide_colorbar(title ="Within-individual\nTiming Variation [days]",ncol = 1, title.position='top')))

p1fin <- cowplot::plot_grid(p1,legend.p1,rel_widths = c(1,0.35))


## Between-individual temporal variation
p2a <- base.autumn +
  # add tracking data
  geom_path(data=mean.i.coords,aes(x=i.mean.cycle.time,y=as.numeric(i)*100,group=paste(dev,trip)),col="grey30",size=0.2)+
  geom_path(data=popmean.i.coords,aes(y=as.numeric(i)*100,x=i.popmean.cycle.time,group=trip,col=i.popvar.temp),size=.4)+
  scale_colour_viridis_c(option="C", limits=c(0,13))+
  theme(axis.title.x = element_blank())

p2b <- base.spring +
  # add tracking data
  geom_path(data=mean.i.coords,aes(x=i.mean.cycle.time,y=as.numeric(i)*100,group=paste(dev,trip)),col="grey30",size=0.2)+
  geom_path(data=popmean.i.coords,aes(y=as.numeric(i)*100,x=i.popmean.cycle.time,group=trip,col=i.popvar.temp),size=.4)+
  scale_colour_viridis_c(option="C", limits=c(0,13))+
  theme(axis.title.x = element_blank())

p2 <- cowplot::plot_grid(p2a,p2b,ncol=2,align="v",labels=c("b",""))

# extract the legend from one of the plots
legend.p2 <- get_legend(
  # create some space to the left of the legend
  p2b + theme(legend.position = "right",
              legend.direction = "vertical",
              legend.title 	    = element_text(size=10,face='bold'),
              legend.text 	    = element_text(size=9))+
    guides(colour = guide_colorbar(title ="Between-individual\nTiming Variation [days]",ncol = 1, title.position='top')))

p2fin <- cowplot::plot_grid(p2,legend.p2,rel_widths = c(1,0.35))


## Between-congeneric temporal variation
p3a <- base.autumn +
  # add tracking data
  geom_path(data=mean.i.coords,aes(x=i.mean.cycle.time,y=as.numeric(i)*100,group=paste(dev,trip)),col="grey30",size=0.2)+
  geom_path(data=sexmean.i.coords,aes(y=as.numeric(i)*100,x=i.sexmean.cycle.time,group=paste(trip,i.sex),col=i.intrasex.temp),size=.5)+
  scale_colour_viridis_c(option="C", limits=c(0,13))+
  theme(axis.title.x = element_blank())

p3b <- base.spring +
  # add tracking data
  geom_path(data=mean.i.coords,aes(x=i.mean.cycle.time,y=as.numeric(i)*100,group=paste(dev,trip)),col="grey30",size=0.2)+
  geom_path(data=sexmean.i.coords,aes(y=as.numeric(i)*100,x=i.sexmean.cycle.time,group=paste(trip,i.sex),col=i.intrasex.temp),size=.5)+
  scale_colour_viridis_c(option="C", limits=c(0,13))+
  theme(axis.title.x = element_blank())

p3 <- cowplot::plot_grid(p3a,p3b,ncol=2,align="v",labels=c("c",""))

# extract the legend from one of the plots
legend.p3 <- get_legend(
  # create some space to the left of the legend
  p3b + theme(legend.position = "right",
              legend.direction = "vertical",
              legend.title 	    = element_text(size=10,face='bold'),
              legend.text 	    = element_text(size=9))+
    guides(colour = guide_colorbar(title ="Between-individual\nTiming Variation\nPer Sex [days]",ncol = 1, title.position='top')))

p3fin <- cowplot::plot_grid(p3,legend.p3,rel_widths = c(1,0.35))


## Between-sex temporal variation
p4a <- base.autumn +
  # add tracking data
  geom_path(data=sexmean.i.coords,aes(x=i.sexmean.cycle.time,y=as.numeric(i)*100,group=paste(trip,i.sex)),col="grey30",size=0.4)+
  geom_path(data=popmean.i.coords,aes(y=as.numeric(i)*100,x=i.popmean.cycle.time,group=trip,col=i.intersex.temp),size=.5)+
  scale_colour_viridis_c(option="C", limits=c(0,13))+
  theme(axis.title.x = element_blank())

p4b <- base.spring +
  # add tracking data
  geom_path(data=sexmean.i.coords,aes(x=i.sexmean.cycle.time,y=as.numeric(i)*100,group=paste(trip,i.sex)),col="grey30",size=0.4)+
  geom_path(data=popmean.i.coords,aes(y=as.numeric(i)*100,x=i.popmean.cycle.time,group=trip,col=i.intersex.temp),size=.5)+
  scale_colour_viridis_c(option="C", limits=c(0,13))+
  theme(axis.title.x = element_blank())

p4 <- cowplot::plot_grid(p4a,p4b,ncol=2,align="v",labels=c("d",""))

# extract the legend from one of the plots
legend.p4 <- get_legend(
  # create some space to the left of the legend
  p4b + theme(legend.position = "right",
              legend.direction = "vertical",
              legend.title 	    = element_text(size=10,face='bold'),
              legend.text 	    = element_text(size=9))+
    guides(colour = guide_colorbar(title ="Between-sex\nTiming Variation [days]",ncol = 1, title.position='top')))

p4fin <- cowplot::plot_grid(p4,legend.p4,rel_widths = c(1,0.35))


# combine all figues
pfin <- cowplot::plot_grid(p1fin,p2fin,p3fin,p4fin,ncol=1)
ggsave(plot=pfin,filename='./output - graphs/FigS6_Timing-Variation-v20220426.pdf',width=8,height=12)

rm(p1a,p1b,p1,legend.p1,p1fin,
   p2a,p2b,p2,legend.p2,p2fin,
   p3a,p3b,p3,legend.p3,p3fin,
   p4a,p4b,p4,legend.p4,p4fin,
   pfin)

#####################################
### PLOT METHODOLOGY VARIATION    ###  
#####################################
# Create subset of coordinates at 1000km intervals for mapping/graphing
ss.coords <- as.character(sprintf("%04d",c(10,20,30,40,50,60,70,80)))
# select individual for which to show within-individual variance calcualtion
ss <- c("B2048")
# select individuals for between-individual variance calculations
selection <- c("B1011","B2453")

## MAP CALCULATIONS WITHIN AND BETWEEN-IND SPATIAL VARIANCE

# within-indiviudal SPATIAL variation 
p1a <- basemap +
  ## Add tracking data
  #geom_path(data=trip.i.coords[which(!(trip.i.coords$dev %in% ss)),],aes(x=i.long,y=i.lat,group=tripID),size=.2,col='grey30')+
  # add mean seasonal routes
  #  geom_path(data=popmean.i.coords[which(popmean.i.coords$trip == "Autumn"),],aes(x=i.popmean.long,y=i.popmean.lat),col='red',size=.8)+
  #  geom_path(data=popmean.i.coords[which(popmean.i.coords$trip == "Spring"),],aes(x=i.popmean.long,y=i.popmean.lat),col='blue',size=.8)+
  geom_path(data=trip.i.coords[which(trip.i.coords$dev %in% ss),],aes(x=i.long,y=i.lat,col=i.var.spat,group=tripID),size=.5)+
  geom_path(data=mean.i.coords[which(mean.i.coords$dev %in% ss),],aes(x=i.mean.long,y=i.mean.lat,col=i.var.spat,group=paste(dev,trip)),size=1.1)+
  # draw segments from mean to real trips at each 1000km interval
  geom_segment(data=trip.i.coords[which(trip.i.coords$dev %in% ss & (trip.i.coords$i %in% c(ss.coords))),],aes(x=i.mean.long,y=i.mean.lat,xend=i.long,yend=i.lat,group=paste(dev,trip,i),col=i.var.spat))+
  # add all points small size
  geom_point(data=trip.i.coords[which(trip.i.coords$dev %in% ss & !(trip.i.coords$i %in% c(ss.coords))),],aes(x=i.long,y=i.lat),col='black',size=.2,alpha=.7)+
  geom_point(data=trip.i.coords[which(trip.i.coords$dev %in% ss & (trip.i.coords$i %in% c(ss.coords))),],aes(x=i.long,y=i.lat,col=i.var.spat),fill='black',size=1.8,alpha=.7,shape=21)+
  geom_point(data=mean.i.coords[which(mean.i.coords$dev %in% ss & (mean.i.coords$i %in% c(ss.coords))),],aes(x=i.mean.long,y=i.mean.lat,col=i.var.spat),fill='black',size=1.8,alpha=.7,shape=21)+
# set colour scale for dist to colony
  scale_colour_viridis_c(option="C",limits=c(0,1110))+
  theme(strip.text = element_text(size=12,face='bold'),
        strip.background	= element_rect(fill='white',colour='NA'))+
  guides(colour=guide_colorbar("Within-individual\nRoute Variaton [km]",title.position="top"),
         fill=FALSE,
         alpha=FALSE)

# between-indiviudal SPATIAL variation 
p1b <- basemap +
  ## Add tracking data
 # geom_path(data=mean.i.coords[which(!(mean.i.coords$dev %in% selection)),],aes(x=i.mean.long,y=i.mean.lat,group=paste(trip,dev)),size=.2,col='grey30')+
  # add mean seasonal routes
  geom_path(data=popmean.i.coords,aes(x=i.popmean.long,y=i.popmean.lat,col=i.popvar.spat),size=.8)+
  geom_path(data=mean.i.coords[which(mean.i.coords$dev %in% selection),],aes(x=i.mean.long,y=i.mean.lat,col=i.popvar.spat,group=paste(trip,dev)),size=.5)+
  geom_segment(data=mean.i.coords[which(mean.i.coords$dev %in% selection & (mean.i.coords$i %in% c(ss.coords))),],aes(x=i.popmean.long,y=i.popmean.lat,xend=i.mean.long,yend=i.mean.lat,group=paste(dev,trip,i),col=i.popvar.spat))+
  # add all points small size
  geom_point(data=mean.i.coords[which(mean.i.coords$dev %in% selection & !(mean.i.coords$i %in% c(ss.coords))),],aes(x=i.mean.long,y=i.mean.lat),col='black',size=.2,alpha=.7)+
  # highlight coords at 1000km intervals for popmean route
  geom_point(data=mean.i.coords[which(mean.i.coords$dev %in% selection & (mean.i.coords$i %in% c(ss.coords))),],aes(x=i.mean.long,y=i.mean.lat,col=i.popvar.spat),fill='black',size=1.8,alpha=.7,shape=21)+
  geom_point(data=popmean.i.coords[which(popmean.i.coords$i %in% c(ss.coords)),],aes(x=i.popmean.long,y=i.popmean.lat,col=i.popvar.spat),fill='black',size=1.8,alpha=.7,shape=21)+
  # set colour scale for dist to colony
  scale_colour_viridis_c(option="C",limits=c(0,1110))+
  theme(strip.text = element_blank(),
        strip.background	= element_blank())+
  guides(colour=guide_colorbar("Between-individual\nRoute Variation [km]",title.position="top"),
         fill=FALSE,
         alpha=FALSE)

# combine within and between spatial variance calculations in one plot
p1 <- cowplot::plot_grid(p1a,p1b,ncol=1,align="h",axis="l",labels=c("a","b"))
ggsave(plot = p1, filename = './output - maps/FigS3_Calculation-Spatial-Variability_v20220426.pdf',width=8,height=7)

rm(p1a,p1b,p1)

## MAP CALCULATIONS WITHIN AND BETWEEN-IND SPATIAL VARIANCE
#######################################################
## WITHIN-individual TEMPORAL variation
p2a <- base.autumn +
  # add tracking data
 geom_path(data=trip.i.coords[which(!(trip.i.coords$dev %in% ss)),],aes(y=as.numeric(i)*100,x=i.cycle.time,group=tripID),size=.1,col='grey30')+
  geom_path(data=trip.i.coords[which((trip.i.coords$dev %in% ss)),],aes(y=as.numeric(i)*100,x=i.cycle.time,group=tripID,col=i.var.temp),size=.3)+
  geom_path(data=mean.i.coords[which((mean.i.coords$dev %in% ss)),],aes(x=i.mean.cycle.time,y=as.numeric(i)*100,col=i.var.temp,group=paste(dev,trip)),size=0.8)+
  #  geom_path(data=popmean.i.coords,aes(y=as.numeric(i)*100,x=i.popmean.cycle.time,group=trip),col="red",size=.6)+
  # highlight coords at 1000km intervals for popmean route
  #  geom_point(data=popmean.i.coords[which(popmean.i.coords$i %in% c(ss.coords) & popmean.i.coords$trip == "Autumn"),],aes(x=i.popmean.cycle.time,y=as.numeric(i)*100),col='black',fill='red',size=1.8,alpha=.7,shape=21)+
  #  geom_segment(data=mean.i.coords[which(mean.i.coords$dev %in% ss & (mean.i.coords$i %in% c(ss.coords)) & mean.i.coords$trip == "Autumn"),],aes(x=i.popmean.cycle.time,y=as.numeric(i)*100,xend=i.mean.cycle.time,yend=as.numeric(i)*100,group=paste(dev,trip,i)),col='red')+
  # draw segments from mean to real trips at each 1000km interval
  geom_segment(data=trip.i.coords[which(trip.i.coords$dev %in% ss & (trip.i.coords$i %in% c(ss.coords))),],aes(x=i.mean.cycle.time,y=as.numeric(i)*100,xend=i.cycle.time,yend=as.numeric(i)*100,group=paste(dev,trip,i),col=i.var.temp),linetype='solid')+
  # add all points small size
  geom_point(data=trip.i.coords[which(trip.i.coords$dev %in% ss & !(trip.i.coords$i %in% c(ss.coords))),],aes(x=i.cycle.time,y=as.numeric(i)*100),col='black',size=.3,alpha=.7)+
  # highlight coords at 1000km intervals for ID mean route
  geom_point(data=trip.i.coords[which(trip.i.coords$dev %in% ss & (trip.i.coords$i %in% c(ss.coords))),],aes(x=i.cycle.time,y=as.numeric(i)*100,col=i.var.temp),fill='black',size=1.8,alpha=.7,shape=21)+
  geom_point(data=mean.i.coords[which(mean.i.coords$dev %in% ss & (mean.i.coords$i %in% c(ss.coords))),],aes(x=i.mean.cycle.time,y=as.numeric(i)*100,col=i.var.temp),fill='black',size=1.8,alpha=.7,shape=21)+
 scale_colour_viridis_c(option="C",limits=c(0,13))+
  theme(axis.title.x = element_text(size=12,face='bold'))

p2b <- base.spring +
  # add tracking data
  geom_path(data=trip.i.coords[which(!(trip.i.coords$dev %in% ss)),],aes(y=as.numeric(i)*100,x=i.cycle.time,group=tripID),size=.1,col='grey30')+
  geom_path(data=trip.i.coords[which((trip.i.coords$dev %in% ss)),],aes(y=as.numeric(i)*100,x=i.cycle.time,group=tripID,col=i.var.temp),size=.3)+
  geom_path(data=mean.i.coords[which((mean.i.coords$dev %in% ss)),],aes(x=i.mean.cycle.time,y=as.numeric(i)*100,col=i.var.temp,group=paste(dev,trip)),size=0.8)+
  #  geom_path(data=popmean.i.coords,aes(y=as.numeric(i)*100,x=i.popmean.cycle.time,group=trip),col="blue",size=.6)+
  # highlight coords at 1000km intervals for popmean route
  #  geom_point(data=popmean.i.coords[which(popmean.i.coords$i %in% c(ss.coords) & popmean.i.coords$trip == "Spring"),],aes(x=i.popmean.cycle.time,y=as.numeric(i)*100),col='black',fill='blue',size=1.8,alpha=.7,shape=21)+
  #  geom_segment(data=mean.i.coords[which(mean.i.coords$dev %in% ss & (mean.i.coords$i %in% c(ss.coords)) & mean.i.coords$trip == "Spring"),],aes(x=i.popmean.cycle.time,y=as.numeric(i)*100,xend=i.mean.cycle.time,yend=as.numeric(i)*100,group=paste(dev,trip,i)),col='blue')+
  # draw segments from mean to real trips at each 1000km interval
  geom_segment(data=trip.i.coords[which(trip.i.coords$dev %in% ss & (trip.i.coords$i %in% c(ss.coords))),],aes(x=i.mean.cycle.time,y=as.numeric(i)*100,xend=i.cycle.time,yend=as.numeric(i)*100,group=paste(dev,trip,i),col=i.var.temp),linetype='solid')+
  # add all points small size
  geom_point(data=trip.i.coords[which(trip.i.coords$dev %in% ss & !(trip.i.coords$i %in% c(ss.coords))),],aes(x=i.cycle.time,y=as.numeric(i)*100),col='black',size=.3,alpha=.7)+
  # highlight coords at 1000km intervals for ID mean route
  geom_point(data=trip.i.coords[which(trip.i.coords$dev %in% ss & (trip.i.coords$i %in% c(ss.coords))),],aes(x=i.cycle.time,y=as.numeric(i)*100,col=i.var.temp,group=tripID),fill='black',size=1.8,alpha=.7,shape=21)+
  geom_point(data=mean.i.coords[which(mean.i.coords$dev %in% ss & (mean.i.coords$i %in% c(ss.coords))),],aes(x=i.mean.cycle.time,y=as.numeric(i)*100,col=i.var.temp),fill='black',size=1.8,alpha=.7,shape=21)+
  scale_colour_viridis_c(option="C",limits=c(0,13))+
  theme(axis.title.x = element_text(size=12,face='bold'))

# combine autumn and spring for within individual variance in one plot
p2.within <- cowplot::plot_grid(p2a,p2b,ncol=2,align="v")

# extract the legend from one of the plots
legend.within <- get_legend(
  # create some space to the left of the legend
  p2b + theme(legend.position = "right",
              legend.direction = "vertical",
              legend.title 	    = element_text(size=10,face='bold'),
              legend.text 	    = element_text(size=9))+
    guides(colour=guide_colorbar("Within-individual\nTiming Variation\n[days]",title.position="top"))
)

# add legend to within plot
p2.within.legend <- cowplot::plot_grid(p2.within, legend.within, nrow=1,rel_widths = c(1, .25))

## BETWEEN-individual TEMPORAL variation
p2c <- base.autumn +
  # add tracking data
  geom_path(data=mean.i.coords[which(!(mean.i.coords$dev %in% selection)),],aes(y=as.numeric(i)*100,x=i.mean.cycle.time,group=paste(dev,trip)),size=.2,col='grey30')+
  geom_path(data=mean.i.coords[which((mean.i.coords$dev %in% selection)),],aes(y=as.numeric(i)*100,x=i.mean.cycle.time,group=paste(dev,trip),col=i.popvar.temp),size=.3)+
  geom_path(data=popmean.i.coords,aes(x=i.popmean.cycle.time,y=as.numeric(i)*100,col=i.popvar.temp,group=trip),size=0.8)+
  # highlight coords at 1000km intervals for popmean route
  #  geom_point(data=popmean.i.coords[which(popmean.i.coords$i %in% c(ss.coords) & popmean.i.coords$trip == "Spring"),],aes(x=i.popmean.cycle.time,y=as.numeric(i)*100),col='black',fill='blue',size=1.8,alpha=.7,shape=21)+
  #  geom_segment(data=mean.i.coords[which(mean.i.coords$dev %in% ss & (mean.i.coords$i %in% c(ss.coords)) & mean.i.coords$trip == "Spring"),],aes(x=i.popmean.cycle.time,y=as.numeric(i)*100,xend=i.mean.cycle.time,yend=as.numeric(i)*100,group=paste(dev,trip,i)),col='blue')+
  # draw segments from mean to real trips at each 1000km interval
  geom_segment(data=mean.i.coords[which(mean.i.coords$dev %in% selection & (mean.i.coords$i %in% c(ss.coords))),],aes(x=i.popmean.cycle.time,y=as.numeric(i)*100,xend=i.mean.cycle.time,yend=as.numeric(i)*100,group=paste(dev,trip,i),col=i.popvar.temp),linetype='solid')+
  # add all points small size
  geom_point(data=mean.i.coords[which(mean.i.coords$dev %in% selection & !(mean.i.coords$i %in% c(ss.coords))),],aes(x=i.mean.cycle.time,y=as.numeric(i)*100),col='black',size=.3,alpha=.7)+
  # highlight coords at 1000km intervals for ID mean route
  geom_point(data=mean.i.coords[which(mean.i.coords$dev %in% selection & (mean.i.coords$i %in% c(ss.coords))),],aes(x=i.mean.cycle.time,y=as.numeric(i)*100,col=i.popvar.temp,group=paste(dev,trip)),fill='black',size=1.8,alpha=.7,shape=21)+
  geom_point(data=popmean.i.coords[which((popmean.i.coords$i %in% c(ss.coords))),],aes(x=i.popmean.cycle.time,y=as.numeric(i)*100,col=i.popvar.temp),fill='black',size=1.8,alpha=.7,shape=21)+
 scale_colour_viridis_c(option="C",limits=c(0,13))+
  theme(axis.title.x = element_blank())


p2d <- base.spring +
  # add tracking data
  geom_path(data=mean.i.coords[which(!(mean.i.coords$dev %in% selection)),],aes(y=as.numeric(i)*100,x=i.mean.cycle.time,group=paste(dev,trip)),size=.2,col='grey30')+
  geom_path(data=mean.i.coords[which((mean.i.coords$dev %in% selection)),],aes(y=as.numeric(i)*100,x=i.mean.cycle.time,group=paste(dev,trip),col=i.popvar.temp),size=.3)+
  geom_path(data=popmean.i.coords,aes(x=i.popmean.cycle.time,y=as.numeric(i)*100,col=i.popvar.temp,group=trip),size=0.8)+
  # highlight coords at 1000km intervals for popmean route
  #  geom_point(data=popmean.i.coords[which(popmean.i.coords$i %in% c(ss.coords) & popmean.i.coords$trip == "Spring"),],aes(x=i.popmean.cycle.time,y=as.numeric(i)*100),col='black',fill='blue',size=1.8,alpha=.7,shape=21)+
  #  geom_segment(data=mean.i.coords[which(mean.i.coords$dev %in% ss & (mean.i.coords$i %in% c(ss.coords)) & mean.i.coords$trip == "Spring"),],aes(x=i.popmean.cycle.time,y=as.numeric(i)*100,xend=i.mean.cycle.time,yend=as.numeric(i)*100,group=paste(dev,trip,i)),col='blue')+
  # draw segments from mean to real trips at each 1000km interval
  geom_segment(data=mean.i.coords[which(mean.i.coords$dev %in% selection & (mean.i.coords$i %in% c(ss.coords))),],aes(x=i.popmean.cycle.time,y=as.numeric(i)*100,xend=i.mean.cycle.time,yend=as.numeric(i)*100,group=paste(dev,trip,i),col=i.popvar.temp),linetype='solid')+
  # add all points small size
  geom_point(data=mean.i.coords[which(mean.i.coords$dev %in% selection & !(mean.i.coords$i %in% c(ss.coords))),],aes(x=i.mean.cycle.time,y=as.numeric(i)*100),col='black',size=.3,alpha=.7)+
  # highlight coords at 1000km intervals for ID mean route
  geom_point(data=mean.i.coords[which(mean.i.coords$dev %in% selection & (mean.i.coords$i %in% c(ss.coords))),],aes(x=i.mean.cycle.time,y=as.numeric(i)*100,col=i.popvar.temp,group=paste(dev,trip)),fill='black',size=1.8,alpha=.7,shape=21)+
  geom_point(data=popmean.i.coords[which((popmean.i.coords$i %in% c(ss.coords))),],aes(x=i.popmean.cycle.time,y=as.numeric(i)*100,col=i.popvar.temp),fill='black',size=1.8,alpha=.7,shape=21)+
  scale_colour_viridis_c(option="C",limits=c(0,13))+
  theme(axis.title.x = element_blank())

# combine autumn and spring for between individual variance in one plot
p2.between <- cowplot::plot_grid(p2c,p2d,ncol=2,align="v")

# extract the legend from one of the plots
legend.between <- get_legend(
  # create some space to the left of the legend
  p2d + theme(legend.position = "right",
              legend.direction = "vertical",
              legend.title 	    = element_text(size=10,face='bold'),
              legend.text 	    = element_text(size=9))+
    guides(colour=guide_colorbar("Between-individual\nTiming Variation\n[days]",title.position="top"))
)

# add legend to within plot
p2.between.legend <- cowplot::plot_grid(p2.between, legend.between, nrow=1,rel_widths = c(1, .25))


## COMBINE WITHIN AND BETWEEN-ID TEMPORAL IN ONE PLOT and save
p2 <- cowplot::plot_grid(p2.within.legend,p2.between.legend,ncol=1,nrow=2,axis="lt",align="h",labels=c("a","b"))
ggsave(plot = p2, filename = './output - graphs/FigS4_Calculation-Temporal-Variability_v20210526.pdf',width=8,height=7)

rm(p2a,p2b,p2c,p2d,p2.within,p2.between,p2.within.legend,p2.between.legend,legend.within,legend.between,p2)

rm(ss,ss.coords,selection)