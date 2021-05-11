setwd("VegParamsSA/R/tree_outputs/")
library(RHESSysIOinR)
library(tidyverse)
library(reshape2)
library(gridExtra)
library(zoo)

code="QUAG"
spn = 81

### load in David's data 
vis <- read.csv("../../data/tree_and_turfgrass_monroe2_selected_weightedmeanVIs.csv")
lai_ind <- read.csv("../../data/alonzo_data_df.csv")

### select species 
quag_data <- vis %>% dplyr::filter(sp_code==code)
lai_data <- lai_ind %>% dplyr::filter(class==spn & 
                                        !is.na(lai) &
                                        fractional_cov>0.7)
lai_data <- lai_data[!lai_data %in% boxplot.stats(lai_data$carbobperarea)$out,]
  
### load in RHESSys output 
quag <- readRDS(paste(tolower(code), "_outputs.rds", sep=""))
length(unique(quag$run))

p <- read.csv("evg_broadleaf_params.csv")

quag_mean <- quag %>% dplyr::filter(year == 2011 | year == 2014 | year ==2017) %>% 
  group_by(year, run) %>% 
  summarize_at(vars(lai), funs(mean))

density_plot <- ggplot(quag_mean) + geom_density(aes(x=lai, col=as.factor(year)))
  
##############################################################
### to plot timeseries
mean_df <- quag %>% 
  group_by(wy) %>% 
  summarize_at(vars(lai), funs(mean))

stats <- quag %>%  
  dplyr::group_by(wy) %>% 
  dplyr::summarize(quant_first = quantile(lai, probs = c(0.25)), quant_third = quantile(lai, probs = c(0.75)))

## plot
timeser <- quag %>% 
  ggplot() + geom_line(aes(x=wy, y=lai, group=run), col='grey') + 
  geom_line(data=mean_df, aes(x=wy, y=lai, group=1), col='red') + 
  geom_line(data=stats, aes(x=wy, y=quant_first, group=1), col='blue', linetype='dashed') +
  geom_line(data=stats, aes(x=wy, y=quant_third, group=1), col='blue', linetype='dashed')
timeser

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

######################################################
######################################################
# look at extent of changes for entire parameter set 
# filter by the change in ndvi 

mean_yrs <- dcast(quag_mean, run ~ year, value.var = 'lai')
colnames(mean_yrs) <- c("run","pre","dur","post")
ggplot(mean_yrs) + geom_jitter(aes(x=pre, y=post, col=dur)) + geom_abline(aes(intercept=0, slope=1))

mean_yrs$decline <- (mean_yrs$pre-mean_yrs$dur)/mean_yrs$pre
mean_yrs$recover <- (mean_yrs$pre-mean_yrs$post)/mean_yrs$pre
head(mean_yrs)

int <- list(sd(ndvi$decline), sd(ndvi$recover))

### filter by difference in LAI between 2011-2014 (decline) and 2011-2017 (recover)
rh_filter <- as.data.frame(mean_yrs) %>% 
  dplyr::filter(mean_yrs$decline < mean(ndvi$decline)+int[[1]] &
                mean_yrs$decline > mean(ndvi$decline)-int[[1]] &    mean_yrs$recover < mean(ndvi$recover)+int[[2]] &    mean_yrs$decline > mean(ndvi$recover)-int[[2]])
nrow(rh_filter)

rh <- ggplot(rh_filter) + geom_density(aes(x=pre, col='11')) +
  geom_density(aes(x=dur, col='14')) +
  geom_density(aes(x=post, col='17')) +
  ggtitle(paste("change in LAI (n=",nrow(rh_filter),")"))

grid.arrange(nd, rh)
################################
tosave <- quag[quag$run %in% rh_filter$run,]
write_rds(tosave, "quag_outputs.rds")

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
# ggplot(quag11) + geom_point(aes(x=pre, y=NDVI))

rh14 <- rh_filter %>% dplyr::select(dur) %>% sample_n(x) 
quag14 <- cbind(rh14, nd14)
## plot 
# ggplot(quag14) + geom_point(aes(x=dur, y=NDVI))

rh17 <- rh_filter %>% dplyr::select(post) %>% sample_n(x) 
quag17 <- cbind(rh17, nd17)
## plot 
# ggplot(quag17) + geom_point(aes(x=post, y=NDVI))

## qqplot 
qqplot(quag17$post, quag17$NDVI)
qqnorm(rh17$post)

## plot sample of parameter sets
ggplot() + geom_density(data=rh11, aes(x=pre, col='11')) +
  geom_density(data=rh14, aes(x=dur, col='14')) + 
  geom_density(data=rh17, aes(x=post, col='17'))

### check % changes 
tmp <- sample_n(rh_filter, x) 
tmp <- rh_filter
pre <- tmp %>% dplyr::select(decline)
post <- tmp %>% dplyr::select(recover)
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
ggplot() + geom_density(data=decline, aes(x=decline,col='model',linetype='pre')) +
  geom_density(data=decline, aes(x=rs,col='rs',linetype='pre')) +
  geom_density(data=recover, aes(x=recover,col='model', linetype='post')) +
  geom_density(data=recover, aes(x=rs,col='rs', linetype='post')) 

## look at density of model only 
ggplot() + geom_density(data=as.data.frame(pre), aes(x=decline,col='model')) +
  geom_density(data=decline, aes(x=rs,col='rs')) 

## plot boxplots of both model and data (pre)
ggplot() + 
  geom_boxplot(data=decline, aes(x="model-decline", y=decline, fill='model'), col='grey') + 
  geom_boxplot(data=decline, aes(x="data-decline", y=rs, fill='rs'), col='grey') +
  geom_boxplot(data=recover, aes(x="model-post", y=recover, fill='model')) +
  geom_boxplot(data=recover, aes(x="data-post", y=rs, fill='rs')) 

### pre is change from 2011-2014 
### post is change from 2011-2017
median(pre)
#median(pre$decline)
median(pre_rs)
median(post)
#median(post$recover)
median(post_rs)
  
### t.test for whenever that seems like the next step 
#tt  <- t.test(x=decline$model, y=decline$rs, mu=0)



### test: Wilcoxon rank-sum test / Mann-Whitney U for comparison of median between two independent samples
wilcox.test(pre, pre_rs, alternative="two.sided", paired=FALSE, conf.int = T)

wilcox.test(post, post_rs, alternative="two.sided", paired=FALSE, conf.int = T, conf.level = 0.95)

## LAI 
## compare mean LAI in 2011 for model (pre drought conditions)

ggplot(lai_data) + geom_boxplot(aes(x="data", y=lai)) +
  geom_boxplot(data=rh_filter, aes(x="model", y=pre))

ggplot(lai_data) + geom_density(aes(x=lai, col='data')) + 
  geom_density(data=rh_filter, aes(x=pre, col='model'))

wilcox.test(rh_filter$pre, lai_data$lai, alternative="two.sided", paired=FALSE, conf.int = T, conf.level = 0.95)

n=100
modlai = sample_n(as.data.frame(rh_filter$pre), n)
modlai = sort(modlai$`rh_filter$pre`)
datlai = sample_n(as.data.frame(lai_data$lai), n)
datlai = sort(datlai$`lai_data$lai`)
cortest = as.data.frame(cbind(model = modlai,
                        data = datlai))
## correlation with Kendall's tau 
cor.test(cortest$model, cortest$data, method="kendall", alternative="two.sided")
## this is similar to a qqplot 
ggplot(cortest) + 
  geom_point(aes(x=data,y=model)) +
  geom_abline(aes(slope=1, intercept=0))

## using Kendall's rank test for correlation 
## non-parametric 
## Kendall’s Tau: usually smaller values than Spearman’s rho correlation. Calculations based on concordant and discordant pairs. Insensitive to error. P values are more accurate with smaller sample sizes 
# https://www.statisticssolutions.com/kendalls-tau-and-spearmans-rank-correlation-coefficient/
# also Spearman method gives a warning message 'Cannot compute exact p-value with ties'




### Wilcoxon test null hypothesis is equal medians.
### If null is rejected, there is evidence that one of the distributions is shifted to the left or right, and the medians of the two populations differ, also called a 'location shift' 
### Wilcoxon statistic W is number of times the values from one distribution are less than the second distribution in a matrix (in this case a 50x50 matrix)
### when including conf.int=T, computes measure of how distributions differ with 95% confidence interval. The difference in location is estimated as the median of the difference between a sample from x and a sample from y.

### bootstrapping is another non-parameteric approach for non-normal data 



### write draft of this section for methods 





