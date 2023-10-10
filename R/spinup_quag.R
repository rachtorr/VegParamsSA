### looking at how different levels of spin up affects the subsequent years that include drought 
### species is Quercus agrifolia 
### years of spinup include: 50, 65, 70, 80, 100
### beginning date: 1947 10 1 1 
### dates of model output: 2010 9 30 1 - 2018 10 1 1 

### actual climate data is 1947 - 2018, and the spinups past those dates were ran with climrepeat

library(RHESSysIOinR)
library(tidyverse)
source("../R/readin_selected_vars.R")
setwd("~/Google Drive File Stream/My Drive/patches/out/")

### look at rainfall data being used real quick
clim <- read.csv("../clim/sb_mc_daily.rain")
dates <- seq.Date(from=as.Date("1947-10-1"), by="day", length.out = 25895)
clim$date <- dates
split_date <- str_split(dates, pattern="-")
clim$year <- as.numeric(lapply(split_date, "[[",1))
colnames(clim) <- c("precip", "date","year")
ann_p <- clim %>% group_by(year) %>% summarize_at(vars(precip), list(sum)) 
plot(ann_p$year, ann_p$precip*1000)
abline(b=0, a=mean(ann_p$precip)*1000, col='red')

### create loop to 100 years 
climrepeat <- as.data.frame(seq.Date(from=as.Date("1947-10-1"), by="day", to=as.Date("2047-10-1")))
climrepeat$precip <- rep(clim$precip, length.out=nrow(climrepeat))
colnames(climrepeat) <- c("date","precip")
split_date <- str_split(climrepeat$date, pattern="-")
climrepeat$year <- as.numeric(lapply(split_date, "[[",1))
climplot <- climrepeat %>% group_by(year) %>% 
  summarize_at(vars(precip), list(sum)) %>% 
  ggplot() + geom_col(aes(x=year, y=precip*1000))

### viz when spinup stopped 

climplot + geom_vline(aes(xintercept=1997, col='50')) +
  geom_vline(aes(xintercept=2011, col='65')) + 
  geom_vline(aes(xintercept=2017, col='70')) +
  geom_vline(aes(xintercept=2027, col='80')) +
  geom_vline(aes(xintercept=2047, col='100')) + 
  ggtitle("precip (repeated 1947-2018) and model spinup output years")

### load in data from the different outputs 
v = c("height", "plantc", "lai")

fifty <- readin_selected_vars("spinup50/allsim", vars = v, start_date = "2010/9/30", end_date="2018/9/30") 

sixtyfive <- readin_selected_vars("spinup65/allsim", vars = v, start_date = "2010/9/30", end_date="2018/9/30")

seventy <- readin_selected_vars("spinup70/allsim", vars = v, start_date = "2010/9/30", end_date="2018/9/30")

eighty <- readin_selected_vars("spinup80/allsim", vars = v, start_date = "2010/9/30", end_date="2018/9/30")

hundo <- readin_selected_vars("spinup100/allsim", vars = v, start_date = "2010/9/30", end_date="2018/9/30")

### group by year 
splt_dte <- str_split(fifty$date, pattern="-")

fifty$year = as.numeric(lapply(splt_dte, "[[",1))
datefifty <- fifty %>% group_by(date) %>% 
  summarize_all(list(mean))

datesixty <- sixtyfive %>% group_by(date) %>% 
  summarize_all(list(mean))

dateseven <- seventy %>% group_by(date) %>% 
  summarize_all(list(mean))

dateeighty <- eighty %>% group_by(date) %>% 
  summarize_all(list(mean))

datehun <- hundo %>% group_by(date) %>% 
  summarize_all(list(mean))


### make some plots 
mean_lai <- ggplot() + geom_line(data=datefifty, aes(x=date, y=lai, col='50')) + 
  geom_line(data=datesixty, aes(x=date, y=lai, col='65')) + 
  geom_line(data=dateseven, aes(x=date, y=lai, col='70')) +
  geom_line(data=dateeighty, aes(x=date, y=lai, col='80')) +
  geom_line(data=datehun, aes(x=date, y=lai, col='100')) + ggtitle("Avg daily LAI")

mean_plantc <- ggplot() + geom_line(data=datefifty, aes(x=date, y=plantc, col='50')) + 
  geom_line(data=datesixty, aes(x=date, y=plantc, col='65')) + 
  geom_line(data=dateseven, aes(x=date, y=plantc, col='70')) +
  geom_line(data=dateeighty, aes(x=date, y=plantc, col='80')) +
  geom_line(data=datehun, aes(x=date, y=plantc, col='100')) + ggtitle("Avg daily plantC")


### use average from june / july 

fifty_jj <- fifty %>%
mutate(year=as.numeric(lapply(splt_dte, "[[",1))) %>%
mutate(month=as.numeric(lapply(splt_dte, "[[",2))) %>% 
  dplyr::filter(month==6 | month==7) %>% 
  group_by(year) %>% 
  summarize_all(list(mean))

sixtyfive_jj <- sixtyfive %>%
  mutate(year=as.numeric(lapply(splt_dte, "[[",1))) %>%
  mutate(month=as.numeric(lapply(splt_dte, "[[",2))) %>% 
  dplyr::filter(month==6 | month==7) %>% 
  group_by(year) %>% 
  summarize_all(list(mean))

seventy_jj <- seventy %>%
  mutate(year=as.numeric(lapply(splt_dte, "[[",1))) %>%
  mutate(month=as.numeric(lapply(splt_dte, "[[",2))) %>% 
  dplyr::filter(month==6 | month==7) %>% 
  group_by(year) %>% 
  summarize_all(list(mean))

eighty_jj <- eighty %>%
  mutate(year=as.numeric(lapply(splt_dte, "[[",1))) %>%
  mutate(month=as.numeric(lapply(splt_dte, "[[",2))) %>% 
  dplyr::filter(month==6 | month==7) %>% 
  group_by(year) %>% 
  summarize_all(list(mean))

hun_jj <- hundo %>%
  mutate(year=as.numeric(lapply(splt_dte, "[[",1))) %>%
  mutate(month=as.numeric(lapply(splt_dte, "[[",2))) %>% 
  dplyr::filter(month==6 | month==7) %>% 
  group_by(year) %>% 
  summarize_all(list(mean))

jj_lai <- ggplot() +
  geom_line(data=fifty_jj, aes(x=year, y=lai, col='50')) +
  geom_line(data=sixtyfive_jj, aes(x=year, y=lai, col='65')) +
  geom_line(data=seventy_jj, aes(x=year, y=lai, col='70')) +
  geom_line(data=eighty_jj, aes(x=year, y=lai, col='80')) +
  geom_line(data=hun_jj, aes(x=year, y=lai, col='100')) +
  ggtitle("avg june/july LAI")

grid.arrange(mean_lai, jj_lai)

jj_plantc <- ggplot() +
  geom_line(data=fifty_jj, aes(x=year, y=plantc, col='50')) +
  geom_line(data=sixtyfive_jj, aes(x=year, y=plantc, col='65')) +
  geom_line(data=seventy_jj, aes(x=year, y=plantc, col='70')) +
  geom_line(data=eighty_jj, aes(x=year, y=plantc, col='80')) +
  geom_line(data=hun_jj, aes(x=year, y=plantc, col='100')) +
  ggtitle("avg june/july plant C")

grid.arrange(mean_plantc, jj_plantc)


### whyyyyy in the find_species_params_weighted_quag file we get decent lai and plantc values, and in the spinups we get low values?? what's up w that timing. 
### in filtered... spinup was 80 years and then ran +10 years 2002-2012

combined_lai <- fifty %>%
  dplyr::select(day, date, run) %>% 
  mutate(year=as.numeric(lapply(splt_dte, "[[",1))) %>%
  mutate(month=as.numeric(lapply(splt_dte, "[[",2))) %>% 
  mutate(fifty = fifty$lai, 
         sixtyfive = sixtyfive$lai,
         seventy = seventy$lai,
         eighty = eighty$lai, 
         hun = hundo$lai)

lai_year11 <- combined_lai %>% 
  dplyr::filter(year == 2011) %>% 
  dplyr::filter(month==6 | month == 7) %>% 
  group_by(year, run) %>% 
  summarize_at(vars(fifty, sixtyfive, seventy, eighty, hun), funs(max))


ggplot(lai_year11) + geom_density(aes(x=fifty, col='50')) + 
  geom_density(aes(x=sixtyfive, col='65')) +
  geom_density(aes(x=seventy, col='70')) +
  geom_density(aes(x=eighty, col='80')) + 
  geom_density(aes(x=hun, col='100'))



## include the actual output you were using before 
out <- maxx[maxx$year==2011,]
ggplot(lai_year11) + geom_density(aes(x=fifty, col='50')) + 
  geom_density(aes(x=sixtyfive, col='65')) +
  geom_density(aes(x=seventy, col='70')) +
  geom_density(aes(x=eighty, col='80')) + 
  geom_density(aes(x=hun, col='100')) + 
  geom_density(data=out, aes(x=lai, col='out')) + 
  ggtitle("LAI 2011 - model output from different amounts of spinup")

### compare medians 


head(lai_year11)

lai_year14 <- combined_lai %>% 
  dplyr::filter(year == 2014) %>% 
  dplyr::filter(month==6 | month == 7) %>% 
  group_by(year, run) %>% 
  summarize_at(vars(fifty, sixtyfive, seventy, eighty, hun), funs(max))

lai_year17 <- combined_lai %>% 
  dplyr::filter(year == 2017) %>% 
  dplyr::filter(month==6 | month == 7) %>% 
  group_by(year, run) %>% 
  summarize_at(vars(fifty, sixtyfive, seventy, eighty, hun), funs(max))

## percent change from 2011-2014 across spinups 
decline = (lai_year11 - lai_year14)/lai_year11
decline <- sample_n(decline, 50)

# plot to see outliers 
decline %>% melt(id.vars=c('year','run')) %>% dplyr::filter(value > -2) %>% 
  ggplot() + geom_boxplot(aes(x=variable, y=value)) + 
  geom_boxplot(data=ndvi, aes(x='data', y=(elevn-fourtn)/elevn)) +
                 ggtitle("LAI percent change '11-'14")

decline %>% melt(id.vars=c('year','run')) %>% dplyr::filter(value > -2) %>%
  ggplot() + geom_density(aes(x=value, col=variable)) +
  geom_density(data=ndvi, aes(col='data', x=(elevn-fourtn)/elevn))

### percent change from 2011-2017
recover = (lai_year11 - lai_year17)/lai_year11
recover <- sample_n(recover, 50)

recover %>% melt(id.vars=c('year','run')) %>% dplyr::filter(value > -2) %>%
  ggplot() + geom_boxplot(aes(x=variable, y=value)) + 
  #geom_boxplot(data=ndvi, aes(x='data', y=(elevn-fourtn)/elevn)) +
  ggtitle("LAI percent change '11-'14")
          
recover %>% melt(id.vars=c('year','run')) %>% dplyr::filter(value > -2) %>%
  ggplot() + geom_density(aes(x=value, col=variable)) +
  geom_density(data=ndvi, aes(col='data', x=(elevn-seventn)/elevn))     

### compare medians 
## ndvi data: 2011-14 median % change was 15%
### 2011-17 median change was 9%

head(decline)

decline %>% dplyr::select(-year, -run) %>%
  summarize_all(list(median), na.rm=T)

recover %>% dplyr::select(-year, -run) %>%
  summarize_all(list(median), na.rm=T)

## 70 years of spinup leads to closest % diff from 2011 to 2014, but then it continues to decline, unlike the NDVI that has a bounce back.
## all spinup scenarios leads to a continued decline over the entire time 2011-2018
