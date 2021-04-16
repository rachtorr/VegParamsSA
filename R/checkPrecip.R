### double checking rain files because fuck it 
### the rhessys precip output wasn't matching up with the clim data i had been looking at, so i went back and checked the input, and that was not matching up with the clim either!!! so i was like wtffff 
# and now am going back and looking at the data from both stations: downtown SB, and botanical gardens

# 234 is downtown SB
sb <- read.csv("../../../Google Drive/My Drive/clim_files/234dailyrain2020.csv")
sb$meters <- sb$mm/1000
sb$date = as.Date(paste(sb$year, sb$month, sb$day, sep="-"))

sbcent <- sb %>% dplyr::filter(year>2000)
# checking to see which dates are missing
sbdate = as.data.frame(seq.Date(from=as.Date('2001-01-08'), to=as.Date("2020-05-01"), by='day'))
colnames(sbdate) <- "date"
check <- left_join(sbdate, sbcent, by='date')

bg <- read.csv("../../GitHub/climfiles/sb_all_clim_UPDATED.csv") %>% mutate(date=cal)
bgcent <- undate(bg) %>% mkwy() %>% 
  dplyr::filter(year>2000) %>%
  dplyr::select(-X, -cal)

ggplot(bgcent) + geom_line(aes(x=date, y=p, group=1)) + geom_point(data=check, aes(x=date, y=precip,col='dt'))

bgcent %>% group_by(wy) %>% summarize(precip=sum(p)) %>% dplyr::filter(wy>2009) %>% ggplot() + geom_col(aes(x=wy, y=precip))

bgcent$meters <- bgcent$p/1000
write.csv(bgcent$meters, "../clim/sb_mc_daily.rain", row.names = F, quote = F)



bgdate = as.data.frame(seq.Date(from=as.Date('1920-10-01'), to=as.Date("2020-09-30"), by='day'))
colnames(bgdate) <- 'date'
bg$date <- as.Date(bg$date)
check2 <- left_join(bgdate, bg, by='date')
check2 <- undate(check2) %>% dplyr::filter(year>1939)
summary(check2)

check2$nas <- is.na(check2$p)
ggplot(check2) + geom_line(aes(x=date, y=p, col=nas)) 
for(i in 1:nrow(check2)){
   if(is.na(check2$tmin[i])){
     check2$tmin[i] = mean(check2$tmin[i+1], 
                        check2$tmin[i-1],
                        check2$tmin[i+2],
                        check2$tmin[i-2],
                        na.rm=TRUE)
   }
}
  
summary(bgdate)

clim <- check2
for(i in 2:nrow(clim)){
  if(is.na(clim$tmin[i])==T){
    clim$tmin[i] <- (clim$tmin[i-1] + clim$tmin[i+1])/2
  }
  if(is.na(clim$tmin[i])==T & is.na(clim$tmin[i-1])==F){
    clim$tmin[i] <- clim$tmin[i-1]
  }
  if(is.na(clim$tmax[i])==T){
    clim$tmax[i] <- (clim$tmax[i-1] + clim$tmax[i+1])/2
  }
  if(is.na(clim$tmax[i])==T & is.na(clim$tmax[i-1])==F){
    clim$tmax[i] <- clim$tmax[i-1]
  }
  if(clim$tmax[i] < clim$tmin[i]){
    clim$tmax[i] <- clim$tmin[i] + 1
  }
}

clim$p[is.na(clim$p)] <- 0
summary(clim)

ggplot(clim) + geom_line(aes(x=date, y=p))
mkwy(clim) %>% group_by(wy) %>% summarize(precip=sum(p)) %>% 
  ggplot() + geom_col(aes(x=wy, y=precip))

write.csv(clim$p/1000, "sb_mc_daily.rain", row.names = F, quote = F)

undate <- function(x){
  split = str_split(x$date, pattern="-")
  x$year = as.numeric(lapply(split, "[[",1))
  x$month = as.numeric(lapply(split, "[[",2))
  x$day = as.numeric(lapply(split, "[[",3))
  return(x)
}
