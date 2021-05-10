setwd("../out/")
setwd("~/Documents/patches/out")
library(RHESSysIOinR)
library(tidyverse)
library(reshape2)
library(gridExtra)
library(zoo)
source("../R/readin_selected_vars.R")

### load in David's data 
vis <- read.csv("/Volumes/GoogleDrive/My Drive/plant_params/DM_data/tree_and_turfgrass_monroe2_selected_weightedmeanVIs.csv")

### select species 
quag_data <- vis %>% dplyr::filter(sp_code==
                                     "PLRA")
### load in RHESSys output 
vars = c("lai",
         "height",
         "plantc")
 
### this won't work 
quag <- readin_selected_vars(vars=vars, dir="~/Documents/patches/out/RHESSysIOinR_output/allsim/")
quag <- read.csv("../R/tree_outputs/plra_outputs.csv")
length(unique(quag$run))

p <- read.csv("RHESSysIOinR_output/allsim/evergreen.sob_all_options.csv")
head(quag)

### check it out 
boxplot(quag$lai)

quag %>% 
  dplyr::filter(year == 2011 | year == 2014 | year ==2017) %>% 
  dplyr::filter(month==6 | month == 7) %>% 
  group_by(year, run) %>% 
  summarize_at(vars(lai), funs(mean)) %>%
  ggplot() + geom_density(aes(x=lai, col=as.factor(year)))
  

### to plot timeseries
mean <- quag %>% dplyr::filter(year>2004) %>% 
  group_by(date) %>% 
  summarize_at(vars(lai), funs(mean))

stats <- quag %>%  dplyr::filter(year>2004) %>% 
  dplyr::group_by(date) %>% 
  dplyr::summarize(quant_first = quantile(lai, probs = c(0.25)), quant_third = quantile(lai, probs = c(0.75)))

## plot
timeser <- quag %>% 
  dplyr::filter(year>1990) %>% 
  ggplot() + geom_line(aes(x=date, y=lai), col='grey') + 
  geom_line(data=mean, aes(x=date, y=lai), col='red') + 
  geom_line(data=stats, aes(x=date, y=quant_first, group=1), col='blue', linetype='dashed') +
  geom_line(data=stats, aes(x=date, y=quant_third, group=1), col='blue', linetype='dashed')
timeser

## check out time series for all runs for 20 years 
annlai1 <- quag %>% 
  group_by(year, run) %>% 
  summarize_at(vars(lai), funs(max)) %>%
  dplyr::filter(year==2011) %>%
  dplyr::filter(lai > low_lai & lai < high_lai) 

annlai2 <- quag[quag$run %in% annlai1$run,] %>% 
  dplyr::filter(year == 2011 | year == 2014 | year ==2017) %>% 
  group_by(year, run) %>% 
  summarize_at(vars(lai), funs(mean)) %>%
  dcast(run ~ year) %>%
  dplyr::filter(`2014`<`2011`)

annlai <- quag[quag$run %in% annlai2$run,]

filt_lai <- annlai %>% 
  group_by(year, run) %>% 
  summarize_at(vars(lai), funs(mean)) %>% 
  ggplot() + geom_line(aes(x=year, y=lai, col=as.factor(run)), show.legend = F) + 
  geom_vline(aes(xintercept=2011)) +
  geom_vline(aes(xintercept=2014)) + 
  geom_vline(aes(xintercept=2017))
filt_lai

### summarize annual lai for 2011, 2014, 2017 
meann <- quag %>% 
  dplyr::filter(year == 2011 | year == 2014 | year ==2017) %>% 
  dplyr::filter(month==6 | month == 7) %>% 
  group_by(year, run) %>% 
  summarize_at(vars(lai), funs(mean))

maxx <- annlai %>%  dplyr::filter(year == 2011 | year == 2014 | year ==2017) %>% 
  dplyr::filter(month==6 | month == 7) %>% 
  group_by(year, run) %>% 
  summarize_at(vars(lai), funs(max))
  
## plot
ggplot(meann) + geom_density(aes(x=lai, col=as.factor(year)))
ggplot(maxx) + geom_density(aes(x=lai, col=as.factor(year)))



######################################################
######################################################
### creates ndvi dataset
nd11 <- quag_data %>% dplyr::filter(year==2011) %>% dplyr::select(NDVI)
nd14 <- quag_data %>% dplyr::filter(year==2014) %>% dplyr::select(NDVI)
nd17 <- quag_data %>% dplyr::filter(year==2017) %>% dplyr::select(NDVI)

ndvi <- cbind(eleven=nd11, fourtn=nd14, seventn = nd17)
colnames(ndvi) = c('elevn', 'fourtn', 'seventn')
ndvi$decline <- (ndvi$elevn - ndvi$fourtn)/ndvi$elevn
ndvi$recover <- (ndvi$elevn - ndvi$seventn)/ndvi$elevn

nd <- ggplot(ndvi) + geom_density(aes(x=elevn, col='11')) +
  geom_density(aes(x=fourtn, col='14')) + 
  geom_density(aes(x=seventn, col='17')) + 
  ggtitle("change in NDVI for QUAG (n=50)")
nd

######################################################
######################################################
# look at extent of changes for entire parameter set 
# filter by the change in ndvi 

mean_yrs <- dcast(meann, run ~ year, value.var = 'lai')
colnames(mean_yrs) <- c("run","pre","dur","post")
ggplot(mean_yrs) + geom_jitter(aes(x=pre, y=post, col=dur)) + geom_abline(aes(intercept=0, slope=1))

## try to filter param sets to those that match changes 

mean_yrs$decline <- (mean_yrs$pre-mean_yrs$dur)/mean_yrs$pre
mean_yrs$recover <- (mean_yrs$pre-mean_yrs$post)/mean_yrs$pre
head(mean_yrs)
boxplot(mean_yrs$decline)

int <- list(2*sd(ndvi$decline), 2*sd(ndvi$recover))

### filter by difference in LAI between 2011-2014 (decline) and 2011-2017 (recover)
rh_filter <- as.data.frame(mean_yrs) %>% 
  dplyr::filter(mean_yrs$decline < mean(ndvi$decline)+int[[1]] &
                mean_yrs$decline > mean(ndvi$decline)-int[[1]] &    mean_yrs$recover < mean(ndvi$recover)+int[[2]] &    mean_yrs$decline > mean(ndvi$recover)-int[[2]])
nrow(rh_filter)

ggplot(rh_filter) + geom_density(aes(x=decline, col='decline')) + geom_density(aes(x=recover, col='recover'))

rh <- ggplot(rh_filter) + geom_density(aes(x=pre, col='11')) +
  geom_density(aes(x=dur, col='14')) +
  geom_density(aes(x=post, col='17')) +
  ggtitle(paste("change in LAI (n=",nrow(rh_filter),")"))
rh

grid.arrange(nd, rh)
################################33

rh_melt <- rh_filter %>% dplyr::select(-decline, -recover) %>% melt( id.var='run')

time_rh <-ggplot(rh_melt) + geom_line(aes(x=as.factor(variable), y=value, col=as.factor(run), group=as.factor(run)), show.legend=F)

time_rs <- ndvi %>% dplyr::select(-decline, -recover) %>% mutate(rown = rownames(ndvi)) %>% melt(id.var='rown') %>% ggplot() + geom_line(aes(x=variable, y=value, col=rown, group=rown), show.legend = F)

grid.arrange(time_rh, time_rs)

######################################################
######################################################

# compare each year individually (doesn't make sense because data points don't line up)

### separate for each year 
### ndvi and lai
x = nrow(quag_data)/3
rh11 <- rh_filter %>% dplyr::select(pre) %>% sample_n(x) 
quag11 <- cbind(rh11, nd11)
## plot
ggplot(quag11) + geom_point(aes(x=pre, y=NDVI))

rh14 <- rh_filter %>% dplyr::select(dur) %>% sample_n(x) 
quag14 <- cbind(rh14, nd14)
## plot 
ggplot(quag14) + geom_point(aes(x=dur, y=NDVI))

rh17 <- rh_filter %>% dplyr::select(post) %>% sample_n(x) 
quag17 <- cbind(rh17, nd17)
## plot 
ggplot(quag17) + geom_point(aes(x=post, y=NDVI))

## qqplot 
qqplot(quag17$post, quag17$NDVI)
qqnorm(rh17$post)


## plot sample of parameter sets
ggplot() + geom_density(data=rh11, aes(x=pre, col='11')) +
  geom_density(data=rh14, aes(x=dur, col='14')) + 
  geom_density(data=rh17, aes(x=post, col='17'))

### check % changes 
pre <- sample_n(rh_filter, x) %>% dplyr::select(decline)
post <- sample_n(rh_filter, x) %>% dplyr::select(recover)
pre <- as.matrix(pre)
post <- as.matrix(post)
## plots
qqnorm(pre)
qqnorm(post)

### % changes of dlm 
pre_rs <- (nd11$NDVI - nd14$NDVI)/nd11$NDVI
post_rs <- (nd11$NDVI - nd17$NDVI)/nd11$NDVI

qqnorm(pre_rs)
qqnorm(post_rs)

## more plots 
decline <- as.data.frame(cbind(model=pre, rs=pre_rs))
recover <- as.data.frame(cbind(model=post, rs=post_rs))

## plot density of both model, data, pre and post
ggplot() + geom_density(data=decline, aes(x=decline,col='model')) +
  geom_density(data=decline, aes(x=rs,col='rs')) +
  geom_density(data=recover, aes(x=recover,col='model'), linetype='dashed') +
  geom_density(data=recover, aes(x=rs,col='rs'), linetype='dashed') 

## look at density of model only 
ggplot() + geom_density(data=decline, aes(x=decline,col='model')) +
  #geom_density(data=decline, aes(x=rs,col='rs')) +
  geom_density(data=recover, aes(x=recover,col='model'), linetype='dashed') 
  

## plot boxplots of both model and data (pre)
ggplot() + 
  geom_boxplot(aes(x="model", y=pre$decline, fill='model'), col='grey', alpha=0.75) + 
  geom_boxplot(aes(x="rs data", y=pre_rs, fill='rs'), col='grey', alpha=0.75) +
  geom_jitter(aes(x="model", y=pre$decline)) +
  geom_jitter(aes(x="rs data", y=pre_rs)) +
  geom_boxplot(aes(x="model-post", y=post$recover)) +
  geom_boxplot(aes(x="data-post", y=post_rs))

### pre is change from 2011-2014 
### post is change from 2011-2017
median(pre)
median(pre_rs)
median(post)
median(post_rs)
  
### t.test for whenever that seems like the next step 
#tt  <- t.test(x=decline$model, y=decline$rs, mu=0)



### test: Wilcoxon rank-sum test / Mann-Whitney U for comparison of median between two independent samples
wilcox.test(pre, pre_rs, alternative="two.sided", paired=FALSE, conf.int = T)

wilcox.test(post, post_rs, alternative="two.sided", paired=FALSE, conf.int = T, conf.level = 0.95)

### Wilcoxon test null hypothesis is equal medians.
### If null is rejected, there is evidence that one of the distributions is shifted to the left or right, and the medians of the two populations differ, also called a 'location shift' 
### Wilcoxon statistic W is number of times the values from one distribution are less than the second distribution in a matrix (in this case a 50x50 matrix)
### when including conf.int=T, computes measure of how distributions differ with 95% confidence interval. The difference in location is estimated as the median of the difference between a sample from x and a sample from y.

### bootstrapping is another non-parameteric approach for non-normal data 


### write draft of this section for methods 





