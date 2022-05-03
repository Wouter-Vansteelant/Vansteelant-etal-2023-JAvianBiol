
### Read packages 
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage
packages <- c("rptR","ggthemes","gridExtra","rsq","lmerTest","MuMIn")
ipak(packages)

drops <- c("trip.start","trip.end")
data <- data[,!(colnames(data) %in% drops)]

#####################################
### Summarize performance per trip###  
#####################################
### Calculate summary statistics per trip, which we can use to look at individual consistency
# Variables of interest are
# departure, arrival, restdays, traveldays, total duration, 

trip.summary <- data %>%
  group_by(tripID) %>%
  summarize(trip.start = min(dt, na.rm = TRUE),
            trip.end = max(dt, na.rm = TRUE),
            trip.dur = round(as.numeric(difftime(trip.end,trip.start,unit="days")))) 
trip.summary <- as.data.frame(trip.summary)

lun <- function(x) length(unique(x))
trip.restdays <- data %>%
  filter(travel == 0,
         !(as.character(country) %in% c("Morocco","Western Sahara","Madagascar") & trip == "return")) %>%
  group_by(tripID) %>%
  summarize(trip.restdays = lun(indday)) 
trip.restdays <- as.data.frame(trip.restdays)

trip.restdays.west <- data %>%
  filter(travel == 0,
         !(as.character(country) %in% c("Morocco","Western Sahara","Madagascar") & trip == "return"),
         dist.to.colony < 4000*1000) %>%
  group_by(tripID) %>%
  summarize(trip.restdays.west = lun(indday)) 
trip.restdays.west <- as.data.frame(trip.restdays.west)

trip.restdays.east <- data %>%
  filter(travel == 0,
         dist.to.colony > 4000*1000) %>%
  group_by(tripID) %>%
  summarize(trip.restdays.east = lun(indday)) 
trip.restdays.east <- as.data.frame(trip.restdays.east)

trip.traveldays <- data %>%
  filter(travel >= 1) %>%
  group_by(tripID) %>%
  summarize(trip.traveldays = lun(indday),
            trip.cumdist = sum(dist.f,na.rm=TRUE)/1000) 
trip.traveldays <- as.data.frame(trip.traveldays)

trip.summary <- merge(trip.summary,trip.traveldays,all.x=TRUE)
trip.summary <- merge(trip.summary,trip.restdays,all.x=TRUE)
trip.summary <- merge(trip.summary,trip.restdays.west,all.x=TRUE)
trip.summary <- merge(trip.summary,trip.restdays.east,all.x=TRUE)

trip.summary$trip.restdays <- ifelse(is.na(trip.summary$trip.restdays)==TRUE,0,trip.summary$trip.restdays)
trip.summary$trip.restdays.west <- ifelse(is.na(trip.summary$trip.restdays.west)==TRUE,0,trip.summary$trip.restdays.west)
trip.summary$trip.restdays.east <- ifelse(is.na(trip.summary$trip.restdays.east)==TRUE,0,trip.summary$trip.restdays.east)

trip.summary <- merge(trip.summary,unique(data[,c("tripID","dev","trip","yr")]),all.x=TRUE)

trip.summary$trip.start.yday <- yday(trip.summary$trip.start)
trip.summary$trip.end.yday <- yday(trip.summary$trip.end)

rm(trip.traveldays,trip.restdays,trip.restdays.west,trip.restdays.east)

trip.dayhrs <- data %>%
  filter(travel > 0 & spd.f*3.6 >= 5 & daynight == 'day') %>%
  group_by(tripID) %>%
  summarize(trip.dayhrs = sum((dur.f)/(3600),na.rm=TRUE))
trip.dayhrs <- as.data.frame(trip.dayhrs)

trip.nighthrs <- data %>%
  filter(travel > 0 & spd.f*3.6 >= 5 & daynight != 'day') %>%
  group_by(tripID) %>%
  summarize(trip.nighthrs = sum((dur.f)/(3600),na.rm=TRUE))
trip.nighthrs <- as.data.frame(trip.nighthrs)

trip.summary <- merge(trip.summary,trip.dayhrs,all.x=TRUE)
trip.summary <- merge(trip.summary,trip.nighthrs,all.x=TRUE)
trip.summary$trip.prop.night <- trip.summary$trip.nighthrs/(trip.summary$trip.nighthrs+trip.summary$trip.dayhrs)

rm(trip.dayhrs,trip.nighthrs)

## Standardize arrival and departure dates for visualisation purposes
mean.dep.aut <- mean(trip.summary[which(trip.summary$trip == "out"),]$trip.start.yday)
mean.dep.spr <- mean(trip.summary[which(trip.summary$trip == "return"),]$trip.start.yday)
trip.summary$trip.start.rel <- ifelse(trip.summary$trip == 'out',trip.summary$trip.start.yday-mean.dep.aut,trip.summary$trip.start.yday-mean.dep.spr)

mean.arr.aut <- mean(trip.summary[which(trip.summary$trip == "out"),]$trip.end.yday)
mean.arr.spr <- mean(trip.summary[which(trip.summary$trip == "return"),]$trip.end.yday)
trip.summary$trip.end.rel <- ifelse(trip.summary$trip == 'out',trip.summary$trip.end.yday-mean.arr.aut,trip.summary$trip.end.yday-mean.arr.spr)

mean.dur.aut <- mean(trip.summary[which(trip.summary$trip == "out"),]$trip.dur)
mean.dur.spr <- mean(trip.summary[which(trip.summary$trip == "return"),]$trip.dur)
trip.summary$trip.dur.rel <- ifelse(trip.summary$trip == 'out',trip.summary$trip.dur-mean.dur.aut,trip.summary$trip.dur-mean.dur.spr)

# Annotate cycle of migration
trip.summary$cycle <- ifelse((trip.summary$trip == "out" & trip.summary$yr == 2012) | (trip.summary$trip == "return" & trip.summary$yr == 2013),"2012-2013",
                             ifelse((trip.summary$trip == "out" & trip.summary$yr == 2013) | (trip.summary$trip == "return" & trip.summary$yr == 2014),"2013-2014",
                                    ifelse((trip.summary$trip == "out" & trip.summary$yr == 2014) | (trip.summary$trip == "return" & trip.summary$yr == 2015),"2014-2015",
                                           ifelse((trip.summary$trip == "out" & trip.summary$yr == 2015) | (trip.summary$trip == "return" & trip.summary$yr == 2016),"2015-2016",
                                                  ifelse((trip.summary$trip == "out" & trip.summary$yr == 2016) | (trip.summary$trip == "return" & trip.summary$yr == 2017),"2016-2017",
                                                         ifelse((trip.summary$trip == "out" & trip.summary$yr == 2017) | (trip.summary$trip == "return" & trip.summary$yr == 2018),"2017-2018","2018-2019"))))))


 
cul <- function(x)length(unique(x))

trip.summary <- merge(trip.summary,unique(data[,c("tripID","dev","trip")]),all.x=TRUE)

ind.summ <- trip.summary %>%
  group_by(dev,trip) %>%
  summarize(n.trips = cul(tripID),
            mean.trip.end.rel = mean(trip.end.rel),
            sd.trip.end.rel = sd(trip.end.rel),
            mean.trip.start.rel = mean(trip.start.rel),
            sd.trip.start.rel = sd(trip.start.rel),
            mean.trip.end = mean(trip.end.yday),
            sd.trip.end = sd(trip.end.yday),
            mean.trip.start = mean(trip.start.yday),
            sd.trip.start = sd(trip.start.yday),
            mean.trip.dur = mean(trip.dur),
            sd.trip.dur = sd(trip.dur),
            mean.trip.cumdist = mean(trip.cumdist),
            sd.trip.cumdist = sd(trip.cumdist),
            mean.trip.restday = mean(trip.restdays),
            sd.trip.restday = sd(trip.restdays),
            mean.trip.restday.east = mean(trip.restdays.east),
            sd.trip.restday.east = sd(trip.restdays.east),
            mean.trip.restday.west = mean(trip.restdays.west),
            sd.trip.restday.west = sd(trip.restdays.west),
            mean.trip.traveldays = mean(trip.traveldays),
            sd.trip.traveldays = sd(trip.traveldays),
            mean.trip.dayhrs = mean(trip.dayhrs),
            sd.trip.dayhrs = sd(trip.dayhrs),
            mean.trip.nighthrs = mean(trip.nighthrs),
            sd.trip.nighthrs = sd(trip.nighthrs)) 
ind.summ <- as.data.frame(ind.summ)

ind.summ <- merge(ind.summ,unique(meta[,c("dev","sex","age")]))

data$md <- as.character(format(data$dt,"%m-%d"))
ydays <- unique(data[which(data$yr == 2018),c("yday","md")])

data <- merge(data,trip.summary[,c("tripID","trip.start","trip.end")],all.x=TRUE)
p1s <- data[which(data$dt == data$trip.start),c("tripID","dev","lat","long")]
p2s <- data[which(data$dt == data$trip.end),c("tripID","dev","lat","long")]
colnames(p1s)[3:4]<-c("start.lat",'start.long')
colnames(p2s)[3:4]<-c("end.lat",'end.long')
detours <- merge(p1s,p2s,by=c("tripID","dev"))
detours$shortest <- deg.dist(long1=detours$start.long,long2=detours$end.long,lat1=detours$start.lat,lat2=detours$end.lat)
detours <- merge(detours,unique(data[,c("tripID","trip")]),all.x=TRUE)

detours <- merge(detours,trip.summary[,c("tripID","trip.cumdist")])
detours$detour <- detours$trip.cumdist/detours$shortest

ind.detours<- detours %>%
  group_by(dev,trip) %>%
  summarize(mean.detour = mean(detour),
            sd.detour=sd(detour))
ind.detours <- as.data.frame(ind.detours)

#write.csv(ind.detours,'./Figures 2021/detours-v20210105.csv')

trip.summary <- merge(trip.summary,detours[,c("tripID","detour")],all.x=TRUE)
ind.summ <- merge(ind.summ,ind.detours[,c("dev","trip","mean.detour","sd.detour")],all.x=TRUE)
rm(ind.detours,detours,p1s,p2s,ydays)

rm(mean.arr.aut,mean.dep.aut,mean.arr.spr,mean.dep.spr,mean.dur.spr,mean.dur.aut)
write.csv(trip.summary,'./Data/Summary_Stats_per_Trip.csv')

ind.summ <- ind.summ[order(ind.summ$trip,ind.summ$sex,ind.summ$n.trips,ind.summ$dev),]
write.csv(as.data.frame(ind.summ),'./output - tables/EF02_Table1.csv')

sex.summ <- ind.summ %>%
  filter(n.trips > 1) %>%
  group_by(trip,sex) %>%
  summarize(n.devs = cul(dev),
            n.trips = sum(n.trips),
            sexmean.trip.end = mean(mean.trip.end),
            sexsd.trip.end = sd(mean.trip.end),
            sexmean.trip.start = mean(mean.trip.start),
            sexsd.trip.start = sd(mean.trip.start),
            sexmean.trip.cumdist = mean(mean.trip.cumdist),
            sexsd.trip.cumdist = sd(mean.trip.cumdist),
            sexmean.detour = mean(mean.detour),
            sexsd.detour = sd(mean.detour),
            sexmean.trip.dur = mean(mean.trip.dur),
            sexsd.trip.dur = sd(mean.trip.dur),
            sexmean.trip.restday = mean(mean.trip.restday),
            sexsd.trip.restday = sd(mean.trip.restday),
            sexmean.trip.traveldays = mean(mean.trip.traveldays),
            sexsd.trip.traveldays = sd(mean.trip.traveldays)) 

write.csv(as.data.frame(sex.summ),'./output - tables/EF02_TableS2.csv')

#seasondates <- ind.summ %>%
#  group_by(trip) %>%
#  summarize(start = mean(mean.trip.start),
#            end = mean(mean.trip.end))


#####################################
#### PRODUCE BOXPLOTS FOR FIG S4
#####################################
library(ggpubr)

xx <- merge(trip.summary,ind.summ[,c("dev","trip","n.trips","sex")],all.x=TRUE)
xx <- subset(xx,xx$n.trips > 1)
xfin <- do.call(rbind, lapply(split(xx, paste(xx$dev,xx$trip)), tail, 1))
xfin$tripsex <- paste(xfin$trip,xfin$sex,sep="-")

## PLOT DETOUR
# get Tukey HSD results from ANOVA for trip and season
m <- aov(detour ~ tripsex,data=xfin)
tukey <- as.data.frame(agricolae::HSD.test(m,"tripsex",alpha=0.5, group=TRUE)$groups)
tukey$tripsex <- rownames(tukey)
tukey$trip <- as.data.frame(do.call("rbind",strsplit(tukey$tripsex,"-",fixed=TRUE)))[,1]
tukey$sex <- as.data.frame(do.call("rbind",strsplit(tukey$tripsex,"-",fixed=TRUE)))[,2]

# plots for detour
p1 <- ggplot(data=xfin,
             aes(x=trip,y=detour,fill=sex,group=paste(trip,sex))) + 
  geom_violin(position = position_dodge(width = 0.9),trim=TRUE,alpha=0.5,stat = "ydensity",scale="width") +
  geom_boxplot(position = position_dodge(width = 0.9,preserve = "single"),width=.05,outlier.colour = "transparent")+
  geom_point(col='black',position = position_jitterdodge(seed = 1, dodge.width = 0.9, jitter.height=0.01, jitter.width=0.1),size=1.8,alpha=.6,shape=21)+
  geom_text(data=tukey, aes(y=1.8, x=trip, group= paste(trip,sex),label=groups), position = position_dodge(width = 0.9)) +
  scale_colour_manual(name="Sex",values=c("male"="darkorange","female"="brown"))+
  scale_fill_manual(name="Sex",values=c("male"="darkorange","female"="brown"))+
  ylab("Detour\nIndex")+ theme_bw()+
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1))+
  #facet_wrap(~trip,ncol=2)+
  theme(legend.position   = 'none',
        legend.direction  = 'vertical',
        panel.border	    = element_rect(colour='white'),
        panel.grid.minor  = element_blank(),
        panel.grid.major = element_blank(),
        strip.text        = element_blank(),
        strip.background	= element_blank(),
        axis.line         = element_line(size=.4),
        legend.title 	    = element_text(size=11,face='bold'),
        legend.text 	    = element_text(size=10,face='bold'),
        axis.text.y		     = element_text(size=11),
        axis.title.y		  = element_text(size=14),
        axis.title.x		  = element_blank(),
        axis.text.x        = element_blank())


## PLOT DETOUR
# get Tukey HSD results from ANOVA for trip and season
m <- aov(trip.dur ~ tripsex,data=xfin)
tukey <- as.data.frame(agricolae::HSD.test(m,"tripsex",alpha=0.5, group=TRUE)$groups)
tukey$tripsex <- rownames(tukey)
tukey$trip <- as.data.frame(do.call("rbind",strsplit(tukey$tripsex,"-",fixed=TRUE)))[,1]
tukey$sex <- as.data.frame(do.call("rbind",strsplit(tukey$tripsex,"-",fixed=TRUE)))[,2]

# plots for duration
p2 <- ggplot(data=xfin,
             aes(x=trip,y=trip.dur,fill=sex,group=paste(trip,sex))) + 
  geom_violin(position = position_dodge(width = 0.9),trim=TRUE,alpha=0.5,stat = "ydensity",scale="width") +
  geom_boxplot(position = position_dodge(width = 0.9,preserve = "single"),width=.05,outlier.colour = "transparent")+
  geom_point(col='black',position = position_jitterdodge(seed = 1, dodge.width = 0.9, jitter.height=0.01, jitter.width=0.1),size=1.8,alpha=.6,shape=21)+
  geom_text(data=tukey, aes(y=74, x=trip, group= paste(trip,sex),label=groups), position = position_dodge(width = 0.9)) +
  scale_colour_manual(name="Sex",values=c("male"="darkorange","female"="brown"))+
  scale_fill_manual(name="Sex",values=c("male"="darkorange","female"="brown"))+
  ylab("Trip Duration\n[days]")+ theme_bw()+
#  ylim(18,65)+
  #facet_wrap(~trip,ncol=2)+
  theme(legend.position   = 'none',
        legend.direction  = 'vertical',
        panel.border	    = element_rect(colour='white'),
        panel.grid.minor  = element_blank(),
        panel.grid.major = element_blank(),
        strip.text        = element_blank(),
        strip.background	= element_blank(),
        axis.line         = element_line(size=.4),
        axis.text.y		     = element_text(size=11),
        axis.title.y		  = element_text(size=14),
        axis.title.x		  = element_blank(),
        axis.text.x        = element_blank())


## PLOT rest days
# get Tukey HSD results from ANOVA for trip and season
m <- aov(trip.restdays ~ tripsex,data=xfin)
tukey <- as.data.frame(agricolae::HSD.test(m,"tripsex",alpha=0.5, group=TRUE)$groups)
tukey$tripsex <- rownames(tukey)
tukey$trip <- as.data.frame(do.call("rbind",strsplit(tukey$tripsex,"-",fixed=TRUE)))[,1]
tukey$sex <- as.data.frame(do.call("rbind",strsplit(tukey$tripsex,"-",fixed=TRUE)))[,2]

# plots for detour
p3 <- ggplot(data=xfin,
             aes(x=trip,y=trip.restdays,fill=sex,group=paste(trip,sex))) + 
  geom_violin(position = position_dodge(width = 0.9),trim=TRUE,alpha=0.5,stat = "ydensity",scale="width") +
  geom_boxplot(position = position_dodge(width = 0.9,preserve = "single"),width=.05,outlier.colour = "transparent")+
  geom_point(col='black',position = position_jitterdodge(seed = 1, dodge.width = 0.9, jitter.height=0.01, jitter.width=0.1),size=1.8,alpha=.6,shape=21)+
  geom_text(data=tukey, aes(y=34, x=trip, group= paste(trip,sex),label=groups), position = position_dodge(width = 0.9)) +
  scale_colour_manual(name="Sex",values=c("male"="darkorange","female"="brown"))+
  scale_fill_manual(name="Sex",values=c("male"="darkorange","female"="brown"))+
  ylab("Stop-over\ndays")+ theme_bw()+
  #facet_wrap(~trip,ncol=2)+
  theme(legend.position   = 'none',
        legend.direction  = 'vertical',
        panel.border	    = element_rect(colour='white'),
        panel.grid.minor  = element_blank(),
        panel.grid.major = element_blank(),
        strip.text        = element_blank(),
        strip.background	= element_blank(),
        axis.line         = element_line(size=.4),
        axis.text.y		     = element_text(size=11),
        axis.title.y		  = element_text(size=14),
        axis.title.x		  = element_blank(),
        axis.text.x        = element_blank())



## PLOT travel days
# get Tukey HSD results from ANOVA for trip and season
m <- aov(trip.traveldays ~ tripsex,data=xfin)
tukey <- as.data.frame(agricolae::HSD.test(m,"tripsex",alpha=0.5, group=TRUE)$groups)
tukey$tripsex <- rownames(tukey)
tukey$trip <- as.data.frame(do.call("rbind",strsplit(tukey$tripsex,"-",fixed=TRUE)))[,1]
tukey$sex <- as.data.frame(do.call("rbind",strsplit(tukey$tripsex,"-",fixed=TRUE)))[,2]

# plots for detour
p4 <- ggplot(data=xfin,
             aes(x=trip,y=trip.traveldays,fill=sex,group=paste(trip,sex))) + 
  geom_violin(position = position_dodge(width = 0.9),trim=TRUE,alpha=0.5,stat = "ydensity",scale="width") +
  geom_boxplot(position = position_dodge(width = 0.9,preserve = "single"),width=.05,outlier.colour = "transparent")+
  geom_point(col='black',position = position_jitterdodge(seed = 1, dodge.width = 0.9, jitter.height=0.01, jitter.width=0.1),size=1.8,alpha=.6,shape=21)+
  geom_text(data=tukey, aes(y=43, x=trip, group= paste(trip,sex),label=groups), position = position_dodge(width = 0.9)) +
  scale_colour_manual(name="Sex",values=c("male"="darkorange","female"="brown"))+
  scale_fill_manual(name="Sex",values=c("male"="darkorange","female"="brown"))+
  ylab("Travel\ndays")+ theme_bw()+
  scale_x_discrete(labels=c("Autumn","Spring"))+
  #facet_wrap(~trip,ncol=2)+
  theme(legend.position   = 'none',
        legend.direction  = 'vertical',
        panel.border	    = element_rect(colour='white'),
        panel.grid.minor  = element_blank(),
        panel.grid.major = element_blank(),
        strip.text        = element_blank(),
        strip.background	= element_blank(),
        axis.line         = element_line(size=.4),
        axis.text.y		     = element_text(size=11),
        axis.title.y		  = element_text(size=14),
        axis.title.x		  = element_blank(),
        axis.text.x        = element_text(size=11,face='bold'))


## calculate relative timing based on reduced dataset
mean.dep.aut <- mean(xfin[which(xfin$trip == "out"),]$trip.start.yday)
mean.dep.spr <- mean(xfin[which(xfin$trip == "return"),]$trip.start.yday)
xfin$trip.start.rel <- ifelse(xfin$trip == 'out',xfin$trip.start.yday-mean.dep.aut,xfin$trip.start.yday-mean.dep.spr)

mean.arr.aut <- mean(xfin[which(xfin$trip == "out"),]$trip.end.yday)
mean.arr.spr <- mean(xfin[which(xfin$trip == "return"),]$trip.end.yday)
xfin$trip.end.rel <- ifelse(xfin$trip == 'out',xfin$trip.end.yday-mean.arr.aut,xfin$trip.end.yday-mean.arr.spr)

## PLOT departure
# get Tukey HSD results from ANOVA for trip and season
m <- aov(trip.start.rel ~ tripsex,data=xfin)
tukey <- as.data.frame(agricolae::HSD.test(m,"tripsex",alpha=0.5, group=TRUE)$groups)
tukey$tripsex <- rownames(tukey)
tukey$trip <- as.data.frame(do.call("rbind",strsplit(tukey$tripsex,"-",fixed=TRUE)))[,1]
tukey$sex <- as.data.frame(do.call("rbind",strsplit(tukey$tripsex,"-",fixed=TRUE)))[,2]

# plots for departure
pa <- ggplot(data=xfin,
             aes(x=trip,y=trip.start.rel,fill=sex,group=paste(trip,sex))) + 
  geom_violin(position = position_dodge(width = 0.9),trim=TRUE,alpha=0.5,stat = "ydensity",scale="width") +
  geom_boxplot(position = position_dodge(width = 0.9,preserve = "single"),width=.05,outlier.colour = "transparent")+
  geom_point(col='black',position = position_jitterdodge(seed = 1, dodge.width = 0.9, jitter.height=0.01, jitter.width=0.1),size=1.8,alpha=.6,shape=21)+
  geom_text(data=tukey, aes(y=22, x=trip, group= paste(trip,sex),label=groups), position = position_dodge(width = 0.9)) +
  scale_colour_manual(name="Sex",values=c("male"="darkorange","female"="brown"))+
  scale_fill_manual(name="Sex",values=c("male"="darkorange","female"="brown"))+
  ylab("Relative\nDeparture")+ theme_bw()+
  #facet_wrap(~trip,ncol=2)+
  theme(legend.position   = 'none',
        legend.direction  = 'vertical',
        panel.border	    = element_rect(colour='white'),
        panel.grid.minor  = element_blank(),
        panel.grid.major = element_blank(),
        strip.text        = element_blank(),
        strip.background	= element_blank(),
        axis.line         = element_line(size=.4),
        axis.text.y		     = element_text(size=11),
        axis.title.y		  = element_text(size=14),
        axis.title.x		  = element_blank(),
        axis.text.x        = element_blank())


## PLOT arrival
# get Tukey HSD results from ANOVA for trip and season
m <- aov(trip.end.rel ~ tripsex,data=xfin)
tukey <- as.data.frame(agricolae::HSD.test(m,"tripsex",alpha=0.5, group=TRUE)$groups)
tukey$tripsex <- rownames(tukey)
tukey$trip <- as.data.frame(do.call("rbind",strsplit(tukey$tripsex,"-",fixed=TRUE)))[,1]
tukey$sex <- as.data.frame(do.call("rbind",strsplit(tukey$tripsex,"-",fixed=TRUE)))[,2]

# plots for arrival
pb <- ggplot(data=xfin,
             aes(x=trip,y=trip.end.rel,fill=sex,group=paste(trip,sex))) + 
  geom_violin(position = position_dodge(width = 0.9),trim=TRUE,alpha=0.5,stat = "ydensity",scale="width") +
  geom_boxplot(position = position_dodge(width = 0.9,preserve = "single"),width=.05,outlier.colour = "transparent")+
  geom_point(col='black',position = position_jitterdodge(seed = 1, dodge.width = 0.9, jitter.height=0.01, jitter.width=0.1),size=1.8,alpha=.6,shape=21)+
  geom_text(data=tukey, aes(y=30, x=trip, group= paste(trip,sex),label=groups), position = position_dodge(width = 0.9)) +
  scale_colour_manual(name="Sex",values=c("male"="darkorange","female"="brown"))+
  scale_fill_manual(name="Sex",values=c("male"="darkorange","female"="brown"))+
  ylab("Relative\nArrival")+ theme_bw()+
  #facet_wrap(~trip,ncol=2)+
  theme(legend.position   = 'none',
        legend.direction  = 'vertical',
        panel.border	    = element_rect(colour='white'),
        panel.grid.minor  = element_blank(),
        panel.grid.major = element_blank(),
        strip.text        = element_blank(),
        strip.background	= element_blank(),
        axis.line         = element_line(size=.4),
        axis.text.y		     = element_text(size=11),
        axis.title.y		  = element_text(size=14),
        axis.title.x		  = element_blank(),
        axis.text.x        = element_blank())


pcomp <- cowplot::plot_grid(pa,pb,p1,p2,p3,p4,ncol=1,align="v",axis="l",
                            labels=c("a","b","c","d","e","f"),
                            rel_heights = c(1,1,1,1,1,1.15))


# extract the legend from one of the plots
library(cowplot)
legend <- get_legend(
  # create some space to the left of the legend
  p1 + theme(legend.position = "right",
             legend.direction = "vertical",
             legend.title 	    = element_text(size=12,face='bold'),
             legend.text 	    = element_text(size=10))+
    guides(fill = guide_legend("Sex",title.position = "top"))
)

pfin <- cowplot::plot_grid(pcomp, legend, ncol=2,rel_widths = c(1, .15))

ggsave(plot=pfin,filename='./output - graphs/FigS7_ViolinPlots-Season-Sex-allbirds-v20220425.tiff',width=7.5,height=10,dpi=300)
rm(pcomp,pfin,p1,p2,p3,p4,pa,pb,xfin,tukey,legend,m,xx)

# TEST FOR SEX DIFF AND INDIVIDUALITY IN SEASONAL TIMING AND PEROFRMANCE
##############################################################################
# RUN MODELS FOR SPRING
mod1 <- rpt(trip.start.yday~sex+(1|dev),data=xx[which(xx$trip == "out"),],grname=c("dev","Fixed"),datatype = "Gaussian", nboot = 500, npermut = 0,adjusted=TRUE) #mixed model
results_temp_df1<-cbind(rep("Autumn",2),rep("Departure",2),c("dev","fixed"),t(mod1$R),mod1$CI_emp,mod1$P[,1]) #extract coefficients to dataframe
colnames(results_temp_df1) <- c("Season","Response","Effect","R2","CI_low","CI_high","P")

mod2 <- rpt(trip.end.yday~sex+(1|dev),data=xx[which(xx$trip == "out"),],grname=c("dev","Fixed"),datatype = "Gaussian", nboot = 500, npermut = 0,adjusted=TRUE) #mixed model
results_temp_df2<-cbind(rep("Autumn",2),rep("Arrival",2),c("dev","fixed"),t(mod2$R),mod2$CI_emp,mod2$P[,1]) #extract coefficients to dataframe
colnames(results_temp_df2) <- c("Season","Response","Effect","R2","CI_low","CI_high","P")

mod3 <- rpt(detour~sex+(1|dev),data=xx[which(xx$trip == "out"),],grname=c("dev","Fixed"),datatype = "Gaussian", nboot = 500, npermut = 0,adjusted=TRUE) #mixed model
results_temp_df3<-cbind(rep("Autumn",2),rep("Detour",2),c("dev","fixed"),t(mod3$R),mod3$CI_emp,mod3$P[,1]) #extract coefficients to dataframe
colnames(results_temp_df3) <- c("Season","Response","Effect","R2","CI_low","CI_high","P")

mod4 <- rpt(trip.dur~sex+(1|dev),data=xx[which(xx$trip == "out"),],grname=c("dev","Fixed"),datatype = "Gaussian", nboot = 500, npermut = 0,adjusted=TRUE) #mixed model
results_temp_df4<-cbind(rep("Autumn",2),rep("Duration",2),c("dev","fixed"),t(mod4$R),mod4$CI_emp,mod4$P[,1]) #extract coefficients to dataframe
colnames(results_temp_df4) <- c("Season","Response","Effect","R2","CI_low","CI_high","P")

mod5 <- rpt(log(trip.restdays+1)~sex+(1|dev),data=xx[which(xx$trip == "out"),],grname=c("dev","Fixed"),datatype = "Gaussian", nboot = 500, npermut = 0,adjusted=TRUE) #mixed model
results_temp_df5<-cbind(rep("Autumn",2),rep("Stop-over days",2),c("dev","fixed"),t(mod5$R),mod5$CI_emp,mod5$P[,1]) #extract coefficients to dataframe
colnames(results_temp_df5) <- c("Season","Response","Effect","R2","CI_low","CI_high","P")

mod6 <- rpt(trip.traveldays~sex+(1|dev),data=xx[which(xx$trip == "out"),],grname=c("dev","Fixed"),datatype = "Gaussian", nboot = 500, npermut = 0,adjusted=TRUE) #mixed model
results_temp_df6<-cbind(rep("Autumn",2),rep("Travel days",2),c("dev","fixed"),t(mod6$R),mod6$CI_emp,mod6$P[,1]) #extract coefficients to dataframe
colnames(results_temp_df6) <- c("Season","Response","Effect","R2","CI_low","CI_high","P")

results_autumn <- rbind(results_temp_df1,results_temp_df2,results_temp_df3,
                        results_temp_df4,results_temp_df5,results_temp_df6)

# RUN MODELS FOR SPRING
mod1 <- rpt(trip.start.yday~sex+(1|dev),data=xx[which(xx$trip == "return"),],grname=c("dev","Fixed"),datatype = "Gaussian", nboot = 500, npermut = 0,adjusted=TRUE) #mixed model
results_temp_df1<-cbind(rep("Spring",2),rep("Departure",2),c("dev","fixed"),t(mod1$R),mod1$CI_emp,mod1$P[,1]) #extract coefficients to dataframe
colnames(results_temp_df1) <- c("Season","Response","Effect","R2","CI_low","CI_high","P")

mod2 <- rpt(trip.end.yday~sex+(1|dev),data=xx[which(xx$trip == "return"),],grname=c("dev","Fixed"),datatype = "Gaussian", nboot = 500, npermut = 0,adjusted=TRUE) #mixed model
results_temp_df2<-cbind(rep("Spring",2),rep("Arrival",2),c("dev","fixed"),t(mod2$R),mod2$CI_emp,mod2$P[,1]) #extract coefficients to dataframe
colnames(results_temp_df2) <- c("Season","Response","Effect","R2","CI_low","CI_high","P")

mod3 <- rpt(detour~sex+(1|dev),data=xx[which(xx$trip == "return"),],grname=c("dev","Fixed"),datatype = "Gaussian", nboot = 500, npermut = 0,adjusted=TRUE) #mixed model
results_temp_df3<-cbind(rep("Spring",2),rep("Detour",2),c("dev","fixed"),t(mod3$R),mod3$CI_emp,mod3$P[,1]) #extract coefficients to dataframe
colnames(results_temp_df3) <- c("Season","Response","Effect","R2","CI_low","CI_high","P")

mod4 <- rpt(trip.dur~sex+(1|dev),data=xx[which(xx$trip == "return"),],grname=c("dev","Fixed"),datatype = "Gaussian", nboot = 500, npermut = 0,adjusted=TRUE) #mixed model
results_temp_df4<-cbind(rep("Spring",2),rep("Duration",2),c("dev","fixed"),t(mod4$R),mod4$CI_emp,mod4$P[,1]) #extract coefficients to dataframe
colnames(results_temp_df4) <- c("Season","Response","Effect","R2","CI_low","CI_high","P")

mod5 <- rpt(log(trip.restdays+1)~sex+(1|dev),data=xx[which(xx$trip == "return"),],grname=c("dev","Fixed"),datatype = "Gaussian", nboot = 500, npermut = 0,adjusted=TRUE) #mixed model
results_temp_df5<-cbind(rep("Spring",2),rep("Stop-over days",2),c("dev","fixed"),t(mod5$R),mod5$CI_emp,mod5$P[,1]) #extract coefficients to dataframe
colnames(results_temp_df5) <- c("Season","Response","Effect","R2","CI_low","CI_high","P")

mod6 <- rpt(trip.traveldays~sex+(1|dev),data=xx[which(xx$trip == "return"),],grname=c("dev","Fixed"),datatype = "Gaussian", nboot = 500, npermut = 0,adjusted=TRUE) #mixed model
results_temp_df6<-cbind(rep("Spring",2),rep("Travel days",2),c("dev","fixed"),t(mod6$R),mod6$CI_emp,mod6$P[,1]) #extract coefficients to dataframe
colnames(results_temp_df6) <- c("Season","Response","Effect","R2","CI_low","CI_high","P")

results_spring <- rbind(results_temp_df1,results_temp_df2,results_temp_df3,
                        results_temp_df4,results_temp_df5,results_temp_df6)

results <- rbind(results_autumn,results_spring)
write.csv(results,'./output - tables/EF02_Table3_Sex&IDeffects_Seasonal-timing-performance.csv')

rm(results_temp_df1,results_temp_df2,results_temp_df3,
   results_temp_df4,results_temp_df5,results_temp_df6,
   results_autumn,results_spring,
   mod1,mod2,mod3,mod4,mod5,mod6,xx)

