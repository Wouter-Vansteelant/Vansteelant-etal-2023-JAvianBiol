

### Generate table with duration and location of stop-overs (incl premigr and winter and breeding stages)
stat <- subset(data,data$travel == 0)

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


segs <- merge(mean.lats,mean.longs,all.x=T)
segs <- merge(segs,st.durs,all.x=T)
segs <- merge(segs,st.first,all.x=TRUE)

segs <- merge(segs,unique(stat[,c("segment","dev","tripID","trip","yr")]),all.x=T)

rm(mean.lats,mean.longs,st.durs,stat,st.first)
