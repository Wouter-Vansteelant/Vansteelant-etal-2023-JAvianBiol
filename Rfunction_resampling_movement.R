########################################################################
# Function to resample movement-tracking data at a given time resolution
# Author: Duarte S. Viana
# E-mail: viana.sptd@gmail.com
# February 2016
########################################################################

moveTimeSample<-function(dataFrame,timeVar,animalID,threshold,tolerance,subset){
  burst.list<-list()
  for(j in unique(animalID)) {
    time <-timeVar[animalID==j]
    burst<-1
    for (i in 2:(length(time)-1)) {
      tb1<-time[which(burst==burst[i-1])][1]
      t1<-time[i]
      t2<-time[i+1]
      tl.timei<-difftime(t1,tb1,units="mins")
      tl12<-difftime(t2,t1,units="mins")
      if(tl.timei<=threshold-tolerance) burst[i]<-burst[i-1]
      if(tl.timei>=threshold-tolerance & tl.timei<threshold & tl12<threshold) burst[i]<-burst[i-1]
      if(tl.timei>threshold-tolerance & tl.timei<threshold & tl12>=threshold) burst[i]<-burst[i-1]+1
      if(tl.timei>=threshold) burst[i]<-burst[i-1]+1
    }
    last.burst<-which(burst==max(burst))[1]
    tl.last<-difftime(time[length(time)],time[last.burst],units="mins")
    if(tl.last<threshold) burst[length(time)]<-burst[length(burst)]
    if(tl.last>=threshold) burst[length(time)]<-burst[length(burst)]+1
    burst.list[[j]]<-burst
  }
  if(subset==TRUE){
    resample<-unlist(burst.list)
    dataResample <- dataFrame[!unlist(tapply(resample,animalID,duplicated)),]
    return(dataResample)
  }
  if(subset==FALSE) return(burst.list)
}

# ARGUMENTS

# dataFrame: an object of class "data.frame" containing tracking data.

# timeVar: the date-time vector of class "POSIXct", ordered by animal (or any other category) and increasing date-time.
# NAs in the time variable must be omitted

# animalID: vector of categories (e.g. animal, season, period, etc).

# threshold: required resolution (mins).

# tolerance: tolerance to apply when next fix is beyond the resolution threshold (mins).
# Example: if the tolerance is 5 min and the next fix is beyond the threshold+tolerance (e.g. 20+5 min), then a fix within the time 15-20 min is sampled

# subset: logical argument indicating whether the resampled dataFrame or classificatio vector should be provided.
# If subset=TRUE, the output is the resampled dataFrame
# If subset=FALSE, the output is a list of vectors (one for each animalID) containing the burst classification (each burst corresponds to the fixes within time intervals of length "threshold") 





