
### CALCULATE REPEATABILITY FOR MALES ONLY
### ### ### ### ### ### ### ### ### ### ### 
ss <- subset(trip.i.coords,is.na(trip.i.coords$i.dist.to.sex.rel) == FALSE & trip.i.coords$sex == 'male')
list_df <- split(ss, paste(ss$trip,ss$i)) #split example dataset by group factor

### calculate spatial repeatability at intervals of 100km to nest 
results_spat_df <- as.data.frame(matrix(ncol=7,nrow=1*length(list_df))) # make an empty dataframe
colnames(results_spat_df)<-c("trip","i","pred","R","CI_low","CI_high","P") #give the dataframe column names

for (i in 1:length(list_df)){ #run a loop over the dataframes in the list
  ss2 <- list_df[[i]]
  if(ss2$i.n.trips != ss2$n.trips)
  #if((length(ss2$i.dist.to.sex.rel) < (length(unique(ss2$dev)) + 1))|(length(unique(ss2$dev)) < 6))
  {results_spat_df[i,]<-cbind(unique(ss2$trip),unique(ss2$i),c("dev"),NA,NA,NA,NA)
  }
  else{mod <- rpt(i.dist.to.sex.rel~(1|dev),data=list_df[[i]],grname=c("dev"),datatype = "Gaussian", nboot = 500, npermut = 0,adjusted=TRUE) #mixed model
  results_spat_df[i,]<-cbind(unique(ss2$trip),unique(ss2$i),c("dev"),t(mod$R),mod$CI_emp,mod$P[,1]) #extract coefficients to dataframe
  }
  rownames(results_spat_df)[i]<- paste(names(list_df)[i],"a",sep="") #assign rowname to results from data used
}

results_spat_df$resp <- rep("spat")
results_spat_df$R <- as.numeric(results_spat_df$R)
results_spat_df$CI_low <- as.numeric(results_spat_df$CI_low)
results_spat_df$CI_high <- as.numeric(results_spat_df$CI_high)
results_spat_df$P <- as.numeric(results_spat_df$P)

### calculate temporal repeatability at intervals of 100km to nest 
results_temp_df <- as.data.frame(matrix(ncol=7,nrow=1*length(list_df))) # make an empty dataframe
colnames(results_temp_df)<-c("trip","i","pred","R","CI_low","CI_high","P") #give the dataframe column names

for (i in 1:length(list_df)){ #run a loop over the dataframes in the list
  ss2 <- list_df[[i]]
  if(ss2$i.n.trips != ss2$n.trips)
  #if((length(ss2$i.days.to.pop.rel) < (length(unique(ss2$dev)) + 1))|(length(unique(ss2$dev)) < 6))
  {results_temp_df[i,]<-cbind(unique(ss2$trip),unique(ss2$i),c("dev"),NA,NA,NA,NA)
  }
  else{mod <- rpt(i.days.to.sex.rel~(1|dev),data=list_df[[i]],grname=c("dev"),datatype = "Gaussian", nboot = 500, npermut = 0,adjusted=TRUE) #mixed model
  results_temp_df[i,]<-cbind(unique(ss2$trip),unique(ss2$i),c("dev"),t(mod$R),mod$CI_emp,mod$P[,1]) #extract coefficients to dataframe
  }
  rownames(results_temp_df)[i]<- paste(names(list_df)[i],"a",sep="") #assign rowname to results from data used
}

#results_temp_df <- cbind(results_temp_df,unique(ss[order(ss$trip,ss$i),c("trip","i")]))
results_temp_df$resp <- rep("temp")
results_temp_df$R <- as.numeric(results_temp_df$R)
results_temp_df$CI_low <- as.numeric(results_temp_df$CI_low)
results_temp_df$CI_high <- as.numeric(results_temp_df$CI_high)
results_temp_df$P <- as.numeric(results_temp_df$P)

# annotate biome per i per season
results_temp_males <- merge(results_temp_df,sexmean.i.coords[which(sexmean.i.coords$sex == "male"),c("trip","i","i.sexmean.biome")],all.x=TRUE)
results_spat_males <- merge(results_spat_df,sexmean.i.coords[which(sexmean.i.coords$sex == "male"),c("trip","i","i.sexmean.biome")],all.x=TRUE)

rm(ss,ss2,list_df,results_temp_df,results_spat_df)

### CALCULATE REPEATABILITY FOR FEMALES ONLY
### ### ### ### ### ### ### ### ### ### ### 
ss <- subset(trip.i.coords,is.na(trip.i.coords$i.dist.to.sex.rel) == FALSE & trip.i.coords$sex == 'female')
list_df <- split(ss, paste(ss$trip,ss$i)) #split example dataset by group factor

### calculate spatial repeatability at intervals of 100km to nest 
results_spat_df <- as.data.frame(matrix(ncol=7,nrow=1*length(list_df))) # make an empty dataframe
colnames(results_spat_df)<-c("trip","i","pred","R","CI_low","CI_high","P") #give the dataframe column names

for (i in 1:length(list_df)){ #run a loop over the dataframes in the list
  ss2 <- list_df[[i]]
  if(ss2$i.n.trips != ss2$n.trips)
    #if((length(ss2$i.dist.to.pop.rel) < (length(unique(ss2$dev)) + 1))|(length(unique(ss2$dev)) < 4))
  {results_spat_df[i,]<-cbind(unique(ss2$trip),unique(ss2$i),c("dev"),NA,NA,NA,NA)
  }
  else{mod <- rpt(i.dist.to.sex.rel~(1|dev),data=list_df[[i]],grname=c("dev"),datatype = "Gaussian", nboot = 500, npermut = 0,adjusted=TRUE) #mixed model
  results_spat_df[i,]<-cbind(unique(ss2$trip),unique(ss2$i),c("dev"),t(mod$R),mod$CI_emp,mod$P[,1]) #extract coefficients to dataframe
  }
  rownames(results_spat_df)[i]<- paste(names(list_df)[i],"a",sep="") #assign rowname to results from data used
}

results_spat_df$resp <- rep("spat")
results_spat_df$R <- as.numeric(results_spat_df$R)
results_spat_df$CI_low <- as.numeric(results_spat_df$CI_low)
results_spat_df$CI_high <- as.numeric(results_spat_df$CI_high)
results_spat_df$P <- as.numeric(results_spat_df$P)

### calculate temporal repeatability at intervals of 100km to nest 
results_temp_df <- as.data.frame(matrix(ncol=7,nrow=1*length(list_df))) # make an empty dataframe
colnames(results_temp_df)<-c("trip","i","pred","R","CI_low","CI_high","P") #give the dataframe column names

for (i in 1:length(list_df)){ #run a loop over the dataframes in the list
  ss2 <- list_df[[i]]
  if(ss2$i.n.trips != ss2$n.trips)
    #if((length(ss2$i.dist.to.pop.rel) < (length(unique(ss2$dev)) + 1))|(length(unique(ss2$dev)) < 4))
  {results_temp_df[i,]<-cbind(unique(ss2$trip),unique(ss2$i),c("dev"),NA,NA,NA,NA)
  }
  else{mod <- rpt(i.days.to.sex.rel~(1|dev),data=list_df[[i]],grname=c("dev"),datatype = "Gaussian", nboot = 500, npermut = 0,adjusted=TRUE) #mixed model
  results_temp_df[i,]<-cbind(unique(ss2$trip),unique(ss2$i),c("dev"),t(mod$R),mod$CI_emp,mod$P[,1]) #extract coefficients to dataframe
  }
  rownames(results_temp_df)[i]<- paste(names(list_df)[i],"a",sep="") #assign rowname to results from data used
}

#results_temp_df <- cbind(results_temp_df,unique(ss[order(ss$trip,ss$i),c("trip","i")]))
results_temp_df$resp <- rep("temp")
results_temp_df$R <- as.numeric(results_temp_df$R)
results_temp_df$CI_low <- as.numeric(results_temp_df$CI_low)
results_temp_df$CI_high <- as.numeric(results_temp_df$CI_high)
results_temp_df$P <- as.numeric(results_temp_df$P)

# annotate biome per i per season
results_temp_females <- merge(results_temp_df,sexmean.i.coords[which(sexmean.i.coords$sex == "female"),c("trip","i","i.sexmean.biome")],all.x=TRUE)
results_spat_females <- merge(results_spat_df,sexmean.i.coords[which(sexmean.i.coords$sex == "female"),c("trip","i","i.sexmean.biome")],all.x=TRUE)

rm(ss,ss2,list_df,results_temp_df,results_spat_df)


### CALCULATE SEX AND INDIVIDUAL REPEATABILITY (SPATIAL AND TEMPORAL) FOR ALL BIRDS
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
ss <- subset(trip.i.coords,as.numeric(trip.i.coords$i) > 0 & is.na(trip.i.coords$i.dist.to.pop.rel) == FALSE)
#ss <- merge(ss,unique(data[,c("tripID","yr","dev")]),all.x=TRUE)
ss$yr <- as.factor(ss$yr)

#ss <- merge(ss,meta[,c("dev","sex")],all.x=TRUE)
list_df <- split(ss, paste(ss$trip,ss$i)) #split example dataset by group factor

### calculate spatial repeatability at intervals of 100km to nest 
results_spat_df <- as.data.frame(matrix(ncol=7,nrow=2*length(list_df))) # make an empty dataframe
colnames(results_spat_df)<-c("trip","i","pred","R","CI_low","CI_high","P") #give the dataframe column names

for (i in 1:length(list_df)){ #run a loop over the dataframes in the list
  ss2 <- list_df[[i]]
  if((length(ss2$i.dist.to.pop.rel) < (length(unique(ss2$yr)) + length(unique(ss2$dev)) + 1))|(length(unique(ss2$dev)) < 10))
  {results_spat_df[(2*i-1):(2*i),]<-cbind(rep(unique(ss2$trip),2),rep(unique(ss2$i),2),c("dev","fixed"),rep(NA,2),rep(NA,2),rep(NA,2),rep(NA,2))
  }
  else{mod <- rpt(i.dist.to.pop.rel~sex+(1|dev),data=list_df[[i]],grname=c("dev","Fixed"),datatype = "Gaussian", nboot = 500, npermut = 0,adjusted=TRUE) #mixed model
  results_spat_df[(2*i-1):(2*i),]<-cbind(rep(unique(ss2$trip),2),rep(unique(ss2$i),2),c("dev","fixed"),t(mod$R),mod$CI_emp,mod$P[,1]) #extract coefficients to dataframe
  }
  rownames(results_spat_df)[(2*i-1)]<- paste(names(list_df)[i],"a",sep="") #assign rowname to results from data used
  rownames(results_spat_df)[(2*i)]<- paste(names(list_df)[i],"b",sep="") #assign rowname to results from data used
}

results_spat_df$resp <- rep("spat")
results_spat_df$R <- as.numeric(results_spat_df$R)
results_spat_df$CI_low <- as.numeric(results_spat_df$CI_low)
results_spat_df$CI_high <- as.numeric(results_spat_df$CI_high)
results_spat_df$P <- as.numeric(results_spat_df$P)

### calculate temporal repeatability at intervals of 100km to nest 
results_temp_df <- as.data.frame(matrix(ncol=7,nrow=2*length(list_df))) # make an empty dataframe
colnames(results_temp_df)<-c("trip","i","pred","R","CI_low","CI_high","P") #give the dataframe column names

for (i in 1:length(list_df)){ #run a loop over the dataframes in the list
  ss2 <- list_df[[i]]
  if((length(ss2$i.dist.to.pop.rel) < (length(unique(ss2$yr)) + length(unique(ss2$dev)) + 1))|(length(unique(ss2$dev)) < 10))
  {results_temp_df[(2*i-1):(2*i),]<-cbind(rep(unique(ss2$trip),2),rep(unique(ss2$i),2),c("dev","fixed"),rep(NA,2),rep(NA,2),rep(NA,2),rep(NA,2))
  }
  else{mod <- rpt(i.days.to.pop.rel~sex+(1|dev),data=list_df[[i]],grname=c("dev","Fixed"),datatype = "Gaussian", nboot = 500, npermut = 0,adjusted=TRUE) #mixed model
  results_temp_df[(2*i-1):(2*i),]<-cbind(rep(unique(ss2$trip),2),rep(unique(ss2$i),2),c("dev","fixed"),t(mod$R),mod$CI_emp,mod$P[,1]) #extract coefficients to dataframe
  }
  rownames(results_temp_df)[(2*i-1)]<- paste(names(list_df)[i],"a",sep="") #assign rowname to results from data used
  rownames(results_temp_df)[(2*i)]<- paste(names(list_df)[i],"b",sep="") #assign rowname to results from data used
}

#results_temp_df <- cbind(results_temp_df,unique(ss[order(ss$trip,ss$i),c("trip","i")]))
results_temp_df$resp <- rep("temp")
results_temp_df$R <- as.numeric(results_temp_df$R)
results_temp_df$CI_low <- as.numeric(results_temp_df$CI_low)
results_temp_df$CI_high <- as.numeric(results_temp_df$CI_high)
results_temp_df$P <- as.numeric(results_temp_df$P)

# annotate biome per i per season
results_temp_df <- merge(results_temp_df,popmean.i.coords[,c("trip","i","i.popmean.biome")],all.x=TRUE)
results_spat_df <- merge(results_spat_df,popmean.i.coords[,c("trip","i","i.popmean.biome")],all.x=TRUE)

rm(ss,ss2,list_df)


## plot repeatability route per sex
p5a <- ggplot()+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=-Inf,xmax=desert.start.dist/1000,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=desert.start.dist/1000,xmax=desert.end.dist/1000,ymin=-Inf,ymax=Inf),fill='wheat3',alpha=.6)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=desert.end.dist/1000,xmax=forest.start.dist/1000,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=forest.start.dist/1000,xmax=forest.end.dist/1000,ymin=-Inf,ymax=Inf),fill='olivedrab4',alpha=.5)+
  #  geom_vline(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xintercept=forest.start.dist/1000),col='chartreuse4',size=.8)+
  #  geom_vline(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xintercept=forest.end.dist/1000),col='chartreuse4',size=.8)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=forest.end.dist/1000,xmax=sea.start.dist/1000,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+ 
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=sea.start.dist/1000,xmax=sea.end.dist/1000,ymin=-Inf,ymax=Inf),fill='white',alpha=.2)+ 
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=sea.end.dist/1000,xmax=Inf,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+ 
  # geom_point(data=results_spat_df,aes(x=ifelse(trip=="Autumn",as.numeric(i)*100-20,as.numeric(i)*100+20),y=R,col=trip))+
  #  geom_pointrange(data=results_spat_df,aes(x=ifelse(trip=="Autumn",as.numeric(i)*100-20,as.numeric(i)*100+20),y=R,col=trip,ymin = CI_low, ymax = CI_high,linetype=factor(ifelse(P<0.05,"P<0.05","P>0.05")),alpha=factor(ifelse(P<0.05,"P<0.05","P>0.05"))),position=position_dodge(width=0.7),size=.2)+
  geom_path(data=results_spat_df[which(results_spat_df$pred == "fixed"),],aes(x=as.numeric(i)*100,y=R,col=trip),size=.2)+
  geom_pointrange(data=results_spat_df[which(results_spat_df$pred == "fixed"),],aes(x=as.numeric(i)*100,y=R,col=trip,ymin = CI_low, ymax = CI_high,shape=factor(ifelse(CI_low>0.1,"CI_low > 0.1","CI_low < 0.1"))),position=position_dodge(width=80),size=.2)+
  scale_shape_manual("Significance",values=c("CI_low > 0.1"=19,"CI_low < 0.1"=21))+
  #scale_alpha_manual("Significance",values=c(1,0.7))+
  scale_colour_manual("Season",values=c("orangered","cornflowerblue"))+
  xlab("Distance to colony [km]")+
  ylab("Sex-specific\nRepeatability")+
  scale_x_continuous(breaks=c(0,2000,4000,6000,8000),limits=c(0,8500))+
  scale_y_continuous(limits=c(0,1))+
  ggtitle("Repeatability of route choice")+
  theme_classic()+
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
        plot.margin       = unit(c(0.1,0.02,0.02,0.1),"cm"),
        plot.title = element_text(size=14,face='bold'))

## plot repeatability route per ID
p5b <- ggplot()+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=-Inf,xmax=desert.start.dist/1000,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=desert.start.dist/1000,xmax=desert.end.dist/1000,ymin=-Inf,ymax=Inf),fill='wheat3',alpha=.6)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=desert.end.dist/1000,xmax=forest.start.dist/1000,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=forest.start.dist/1000,xmax=forest.end.dist/1000,ymin=-Inf,ymax=Inf),fill='olivedrab4',alpha=.5)+
  #  geom_vline(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xintercept=forest.start.dist/1000),col='chartreuse4',size=.8)+
  #  geom_vline(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xintercept=forest.end.dist/1000),col='chartreuse4',size=.8)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=forest.end.dist/1000,xmax=sea.start.dist/1000,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+ 
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=sea.start.dist/1000,xmax=sea.end.dist/1000,ymin=-Inf,ymax=Inf),fill='white',alpha=.2)+ 
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=sea.end.dist/1000,xmax=Inf,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+ 
  # geom_point(data=results_spat_df,aes(x=ifelse(trip=="Autumn",as.numeric(i)*100-20,as.numeric(i)*100+20),y=R,col=trip))+
  #  geom_pointrange(data=results_spat_df,aes(x=ifelse(trip=="Autumn",as.numeric(i)*100-20,as.numeric(i)*100+20),y=R,col=trip,ymin = CI_low, ymax = CI_high,linetype=factor(ifelse(P<0.05,"P<0.05","P>0.05")),alpha=factor(ifelse(P<0.05,"P<0.05","P>0.05"))),position=position_dodge(width=0.7),size=.2)+
  geom_pointrange(data=results_spat_df[which(results_spat_df$pred == "dev"),],aes(x=as.numeric(i)*100,y=R,col=trip,ymin = CI_low, ymax = CI_high,shape=factor(ifelse(CI_low>0.1,"CI_low > 0.1","CI_low < 0.1"))),position=position_dodge(width=80),size=.2)+
  scale_shape_manual("Significance",values=c("CI_low > 0.1"=19,"CI_low < 0.1"=21))+
  scale_colour_manual("Season",values=c("orangered","cornflowerblue"))+
  xlab("Distance to colony [km]")+
  ylab("Individual\nRepeatability")+
  scale_x_continuous(breaks=c(0,2000,4000,6000,8000),limits=c(0,8500))+
  scale_y_continuous(limits=c(0,1))+
  theme_classic()+
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


## plot repeatability timing per sex
p6a <- ggplot()+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=-Inf,xmax=desert.start.dist/1000,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=desert.start.dist/1000,xmax=desert.end.dist/1000,ymin=-Inf,ymax=Inf),fill='wheat3',alpha=.6)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=desert.end.dist/1000,xmax=forest.start.dist/1000,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=forest.start.dist/1000,xmax=forest.end.dist/1000,ymin=-Inf,ymax=Inf),fill='olivedrab4',alpha=.5)+
  #  geom_vline(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xintercept=forest.start.dist/1000),col='chartreuse4',size=.8)+
  #  geom_vline(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xintercept=forest.end.dist/1000),col='chartreuse4',size=.8)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=forest.end.dist/1000,xmax=sea.start.dist/1000,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+ 
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=sea.start.dist/1000,xmax=sea.end.dist/1000,ymin=-Inf,ymax=Inf),fill='white',alpha=.2)+ 
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=sea.end.dist/1000,xmax=Inf,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+ 
  # geom_point(data=results_spat_df,aes(x=ifelse(trip=="Autumn",as.numeric(i)*100-20,as.numeric(i)*100+20),y=R,col=trip))+
  geom_path(data=results_temp_df[which(results_temp_df$pred == "fixed"),],aes(x=as.numeric(i)*100,y=R,col=trip),size=.2)+
  geom_pointrange(data=results_temp_df[which(results_temp_df$pred == "fixed"),],aes(x=as.numeric(i)*100,y=R,col=trip,ymin = CI_low, ymax = CI_high,shape=factor(ifelse(CI_low>0.1,"CI_low > 0.1","CI_low < 0.1"))),position=position_dodge(width=80),size=.2)+
  scale_shape_manual("Significance",values=c("CI_low > 0.1"=19,"CI_low < 0.1"=21))+
  scale_colour_manual("Season",values=c("orangered","cornflowerblue"))+
  xlab("Distance to colony [km]")+
  ylab("\n ")+
  #ylab("Sex-specific\nRepeatability")+
  scale_x_continuous(breaks=c(0,2000,4000,6000,8000),limits=c(0,8500))+
  scale_y_continuous(limits=c(0,1))+
  ggtitle("Repeatability of timing")+
  theme_classic()+
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
        plot.margin       = unit(c(0.1,0.02,0.02,0.1),"cm"),
        plot.title = element_text(size=14,face='bold'))

## plot repeatability timing per ID
p6b <- ggplot()+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=-Inf,xmax=desert.start.dist/1000,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=desert.start.dist/1000,xmax=desert.end.dist/1000,ymin=-Inf,ymax=Inf),fill='wheat3',alpha=.6)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=desert.end.dist/1000,xmax=forest.start.dist/1000,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=forest.start.dist/1000,xmax=forest.end.dist/1000,ymin=-Inf,ymax=Inf),fill='olivedrab4',alpha=.5)+
  #  geom_vline(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xintercept=forest.start.dist/1000),col='chartreuse4',size=.8)+
  #  geom_vline(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xintercept=forest.end.dist/1000),col='chartreuse4',size=.8)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=forest.end.dist/1000,xmax=sea.start.dist/1000,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+ 
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=sea.start.dist/1000,xmax=sea.end.dist/1000,ymin=-Inf,ymax=Inf),fill='white',alpha=.2)+ 
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=sea.end.dist/1000,xmax=Inf,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+ 
  # geom_point(data=results_spat_df,aes(x=ifelse(trip=="Autumn",as.numeric(i)*100-20,as.numeric(i)*100+20),y=R,col=trip))+
  geom_pointrange(data=results_temp_df[which(results_temp_df$pred == "dev"),],aes(x=as.numeric(i)*100,y=R,col=trip,ymin = CI_low, ymax = CI_high,shape=factor(ifelse(CI_low>0.1,"CI_low > 0.1","CI_low < 0.1"))),position=position_dodge(width=80),size=.2)+
  scale_shape_manual("Significance",values=c("CI_low > 0.1"=19,"CI_low < 0.1"=21))+
  scale_colour_manual("Season",values=c("orangered","cornflowerblue"))+
  xlab("Distance to colony [km]")+
  ylab("\n ")+
  #ylab("Individual\nRepeatability")+
  scale_x_continuous(breaks=c(0,2000,4000,6000,8000),limits=c(0,8500))+
  scale_y_continuous(limits=c(0,1))+
  theme_classic()+
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


#plot route repeatability females
pf1 <- ggplot()+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=-Inf,xmax=desert.start.dist/1000,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=desert.start.dist/1000,xmax=desert.end.dist/1000,ymin=-Inf,ymax=Inf),fill='wheat3',alpha=.6)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=desert.end.dist/1000,xmax=forest.start.dist/1000,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=forest.start.dist/1000,xmax=forest.end.dist/1000,ymin=-Inf,ymax=Inf),fill='olivedrab4',alpha=.5)+
  #  geom_vline(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xintercept=forest.start.dist/1000),col='chartreuse4',size=.8)+
  #  geom_vline(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xintercept=forest.end.dist/1000),col='chartreuse4',size=.8)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=forest.end.dist/1000,xmax=sea.start.dist/1000,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+ 
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=sea.start.dist/1000,xmax=sea.end.dist/1000,ymin=-Inf,ymax=Inf),fill='white',alpha=.2)+ 
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=sea.end.dist/1000,xmax=Inf,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+ 
  # geom_point(data=results_spat_df,aes(x=ifelse(trip=="Autumn",as.numeric(i)*100-20,as.numeric(i)*100+20),y=R,col=trip))+
  #  geom_pointrange(data=results_spat_df,aes(x=ifelse(trip=="Autumn",as.numeric(i)*100-20,as.numeric(i)*100+20),y=R,col=trip,ymin = CI_low, ymax = CI_high,linetype=factor(ifelse(P<0.05,"P<0.05","P>0.05")),alpha=factor(ifelse(P<0.05,"P<0.05","P>0.05"))),position=position_dodge(width=0.7),size=.2)+
  geom_pointrange(data=results_spat_females[which(results_spat_females$pred == "dev"),],aes(x=as.numeric(i)*100,y=R,col=trip,ymin = CI_low, ymax = CI_high,shape=factor(ifelse(CI_low>0.1,"CI_low > 0.1","CI_low < 0.1"))),position=position_dodge(width=80),size=.2)+
  scale_shape_manual("Significance",values=c("CI_low > 0.1"=19,"CI_low < 0.1"=21))+
  scale_colour_manual("Season",values=c("orangered","cornflowerblue"))+
  xlab("Distance to colony [km]")+
  ylab("Female Individual\nRepeatability")+
  scale_x_continuous(breaks=c(0,2000,4000,6000,8000),limits=c(0,8500))+
  scale_y_continuous(limits=c(0,1))+
  theme_classic()+
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


## plot repeatability route for males
pm1 <- ggplot()+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=-Inf,xmax=desert.start.dist/1000,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=desert.start.dist/1000,xmax=desert.end.dist/1000,ymin=-Inf,ymax=Inf),fill='wheat3',alpha=.6)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=desert.end.dist/1000,xmax=forest.start.dist/1000,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=forest.start.dist/1000,xmax=forest.end.dist/1000,ymin=-Inf,ymax=Inf),fill='olivedrab4',alpha=.5)+
  #  geom_vline(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xintercept=forest.start.dist/1000),col='chartreuse4',size=.8)+
  #  geom_vline(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xintercept=forest.end.dist/1000),col='chartreuse4',size=.8)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=forest.end.dist/1000,xmax=sea.start.dist/1000,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+ 
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=sea.start.dist/1000,xmax=sea.end.dist/1000,ymin=-Inf,ymax=Inf),fill='white',alpha=.2)+ 
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=sea.end.dist/1000,xmax=Inf,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+ 
  # geom_point(data=results_spat_df,aes(x=ifelse(trip=="Autumn",as.numeric(i)*100-20,as.numeric(i)*100+20),y=R,col=trip))+
  #  geom_pointrange(data=results_spat_df,aes(x=ifelse(trip=="Autumn",as.numeric(i)*100-20,as.numeric(i)*100+20),y=R,col=trip,ymin = CI_low, ymax = CI_high,linetype=factor(ifelse(P<0.05,"P<0.05","P>0.05")),alpha=factor(ifelse(P<0.05,"P<0.05","P>0.05"))),position=position_dodge(width=0.7),size=.2)+
  geom_pointrange(data=results_spat_males[which(results_spat_males$pred == "dev"),],aes(x=as.numeric(i)*100,y=R,col=trip,ymin = CI_low, ymax = CI_high,shape=factor(ifelse(CI_low>0.1,"CI_low > 0.1","CI_low < 0.1"))),position=position_dodge(width=80),size=.2)+
  scale_shape_manual("Significance",values=c("CI_low > 0.1"=19,"CI_low < 0.1"=21))+
  scale_colour_manual("Season",values=c("orangered","cornflowerblue"))+
  xlab("Distance to colony [km]")+
  ylab("Male Individual\nRepeatability")+
  scale_x_continuous(breaks=c(0,2000,4000,6000,8000),limits=c(0,8500))+
  scale_y_continuous(limits=c(0,1))+
  theme_classic()+
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


#plot timing repeatability females
pf2 <- ggplot()+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=-Inf,xmax=desert.start.dist/1000,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=desert.start.dist/1000,xmax=desert.end.dist/1000,ymin=-Inf,ymax=Inf),fill='wheat3',alpha=.6)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=desert.end.dist/1000,xmax=forest.start.dist/1000,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=forest.start.dist/1000,xmax=forest.end.dist/1000,ymin=-Inf,ymax=Inf),fill='olivedrab4',alpha=.5)+
  #  geom_vline(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xintercept=forest.start.dist/1000),col='chartreuse4',size=.8)+
  #  geom_vline(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xintercept=forest.end.dist/1000),col='chartreuse4',size=.8)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=forest.end.dist/1000,xmax=sea.start.dist/1000,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+ 
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=sea.start.dist/1000,xmax=sea.end.dist/1000,ymin=-Inf,ymax=Inf),fill='white',alpha=.2)+ 
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=sea.end.dist/1000,xmax=Inf,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+ 
  # geom_point(data=results_spat_df,aes(x=ifelse(trip=="Autumn",as.numeric(i)*100-20,as.numeric(i)*100+20),y=R,col=trip))+
  #  geom_pointrange(data=results_spat_df,aes(x=ifelse(trip=="Autumn",as.numeric(i)*100-20,as.numeric(i)*100+20),y=R,col=trip,ymin = CI_low, ymax = CI_high,linetype=factor(ifelse(P<0.05,"P<0.05","P>0.05")),alpha=factor(ifelse(P<0.05,"P<0.05","P>0.05"))),position=position_dodge(width=0.7),size=.2)+
  geom_pointrange(data=results_temp_females[which(results_temp_females$pred == "dev"),],aes(x=as.numeric(i)*100,y=R,col=trip,ymin = CI_low, ymax = CI_high,shape=factor(ifelse(CI_low>0.1,"CI_low > 0.1","CI_low < 0.1"))),position=position_dodge(width=80),size=.2)+
  scale_shape_manual("Significance",values=c("CI_low > 0.1"=19,"CI_low < 0.1"=21))+
  scale_colour_manual("Season",values=c("orangered","cornflowerblue"))+
  xlab("Distance to colony [km]")+
  ylab("\n ")+
  #ylab("Female Individual\nRepeatability")+
  scale_x_continuous(breaks=c(0,2000,4000,6000,8000),limits=c(0,8500))+
  scale_y_continuous(limits=c(0,1))+
  theme_classic()+
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


## plot repeatability timing for males
pm2 <- ggplot()+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=-Inf,xmax=desert.start.dist/1000,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=desert.start.dist/1000,xmax=desert.end.dist/1000,ymin=-Inf,ymax=Inf),fill='wheat3',alpha=.6)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=desert.end.dist/1000,xmax=forest.start.dist/1000,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=forest.start.dist/1000,xmax=forest.end.dist/1000,ymin=-Inf,ymax=Inf),fill='olivedrab4',alpha=.5)+
  #  geom_vline(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xintercept=forest.start.dist/1000),col='chartreuse4',size=.8)+
  #  geom_vline(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xintercept=forest.end.dist/1000),col='chartreuse4',size=.8)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=forest.end.dist/1000,xmax=sea.start.dist/1000,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+ 
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=sea.start.dist/1000,xmax=sea.end.dist/1000,ymin=-Inf,ymax=Inf),fill='white',alpha=.2)+ 
  geom_rect(data=trip.dists[which(trip.dists$trip == "Autumn"),],aes(xmin=sea.end.dist/1000,xmax=Inf,ymin=-Inf,ymax=Inf),fill='grey30',alpha=.2)+ 
  # geom_point(data=results_spat_df,aes(x=ifelse(trip=="Autumn",as.numeric(i)*100-20,as.numeric(i)*100+20),y=R,col=trip))+
  #  geom_pointrange(data=results_spat_df,aes(x=ifelse(trip=="Autumn",as.numeric(i)*100-20,as.numeric(i)*100+20),y=R,col=trip,ymin = CI_low, ymax = CI_high,linetype=factor(ifelse(P<0.05,"P<0.05","P>0.05")),alpha=factor(ifelse(P<0.05,"P<0.05","P>0.05"))),position=position_dodge(width=0.7),size=.2)+
  geom_pointrange(data=results_temp_males[which(results_temp_males$pred == "dev"),],aes(x=as.numeric(i)*100,y=R,col=trip,ymin = CI_low, ymax = CI_high,shape=factor(ifelse(CI_low>0.1,"CI_low > 0.1","CI_low < 0.1"))),position=position_dodge(width=80),size=.2)+
  scale_shape_manual("Significance",values=c("CI_low > 0.1"=19,"CI_low < 0.1"=21))+
  scale_colour_manual("Season",values=c("orangered","cornflowerblue"))+
  xlab("Distance to colony [km]")+
  ylab("\n ")+
  #ylab("Male Individual\nRepeatability")+
  scale_x_continuous(breaks=c(0,2000,4000,6000,8000),limits=c(0,8500))+
  scale_y_continuous(limits=c(0,1))+
  theme_classic()+
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

cowplot::plot_grid(pf1,pf2,pm1,pm2,ncol=2,axis="bl",align="vh")
#ggsave('./output - graphs/Repeatability-per-sex.tiff',dpi=300,width=10,height=7)

pspat <- cowplot::plot_grid(p5a,p5b,pf1,pm1,nrow=4,ncol=1,align="v",axis="b",rel_heights=c(1.15,1,1,1.15),labels=c("a","b","c","d"))
ptemp <- cowplot::plot_grid(p6a,p6b,pf2,pm2,nrow=4,ncol=1,align="v",axis="b",rel_heights=c(1.15,1,1,1.15))
(pcomp <- cowplot::plot_grid(pspat,ptemp,ncol=2,nrow=1,align="v",axis="l"))

# extract the legend from one of the plots
legend <- get_legend(
  # create some space to the left of the legend
  p5a + theme(legend.position = "bottom",
             legend.direction = "horizontal",
             legend.title 	    = element_text(size=10,face='bold'),
             legend.text 	    = element_text(size=9))+
    guides(colour = guide_legend("Season",title.position = "top"),
           shape = guide_legend("Confidence",title.position =  "top",override.aes=c(size=.8)))
)

# add the legend to the row we made earlier. 
pfin <- cowplot::plot_grid(pcomp, legend, ncol=1,rel_heights = c(1, .1))

ggsave(plot=pfin,filename='./output - graphs/Fig3_Repeatabilities-vs-DistToColonyb-v20220426.tiff',width=8.5,height=9,dpi=300)

rm(p1a,p1b,p2a,p2b,p3a,p3b,p4a,p4b,p5a,p5b,p5c,p6a,p6b,p6c,pspat,ptemp,pcomp,pm1,pm2,pf1,pf2,legend)
