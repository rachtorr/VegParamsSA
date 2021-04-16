
setwd("~/Documents/patches/out/")

library(tidyverse)
library(reshape2)
library(RHESSysIOinR)
library(zoo)
source("../R/readin_selected_vars.R")
source("../R/readin_selected_vars_old.R")
source("../R/adjust_params.R")

data = read.csv("../R/data/weighted_tree_summary.csv")
quag <- data[data$class == 81,]
low_lai <- quag$mean_wt_lai - 2*quag$sd_wt_lai
high_lai <- quag$mean_wt_lai + 2*quag$sd_wt_lai
low_carb <- quag$mean_wt_carb - 2*quag$sd_wt_carb
high_carb <- quag$mean_wt_carb + 2*quag$sd_wt_carb

outvars <- c("lai",
             "height",
             "plantc",
             "rootdepth", "cpool")
             # "leafc",
             # "dead_leafc",
             # "leafc_store",
             # "live_stemc",
             # "live_crootc",
             # "dead_stemc",
             #  "dead_crootc",
             #  "cpool")
             # "psn_to_cpool")
#setwd("../out/gsi/")
system.time(
  read <- readin_selected_vars("RHESSysIOinR_output/allsim/",
                               vars=outvars)
)

#read <- check 

# take average by year and run
grouped <- read %>% group_by(year, run) %>%
  summarize_at(vars(plantc, lai, height, cpool), funs(mean))

# only run this if the carbon stores were outputs of model 
grouped2 <- read %>% group_by(date) %>%
  summarize_at(vars(plantc, leafc,
                    dead_leafc,
                    leafc_store,
                    live_stemc,
                    live_crootc,
                    dead_stemc,
                    dead_crootc), funs(mean))

# ggplot(grouped2) + geom_line(aes(x=date, y=plantc*1000)) +
#   geom_line(aes(x=date, y=leafc, col='leafc')) +
#   geom_line(aes(x=date, y=live_stemc, col='live_stemc')) +
#   geom_line(aes(x=date, y=dead_leafc, col='deadleaf')) +
#   geom_line(aes(x=date, y=dead_stemc, col='deadstem')) +
#   geom_line(aes(x=date, y=live_crootc, col='livecroot')) +
#   geom_line(aes(x=date, y=dead_crootc, col='deadcroot')) +
#   geom_line(aes(x=date, y=leafc_store, col='storedleafc'))

grouped2 %>% dplyr::select(-plantc) %>%
  reshape2::melt(id.vars='date') %>%
  ggplot() + geom_area(aes(x=date, y=value, fill=variable)) +
  geom_vline(aes(xintercept=as.Date('2011-07-01'))) +
  geom_vline(aes(xintercept=as.Date('2014-07-01'))) +
  geom_vline(aes(xintercept=as.Date('2017-07-01')))

# read %>% dplyr::select(plantc, leafc,
#                        dead_leafc,
#                        leafc_store,
#                        live_stemc,
#                        live_crootc,
#                        dead_stemc,
#                        dead_crootc, run) %>%
#   reshape2::melt(id.vars='run') %>%
#   ggplot(aes(y=value, x=variable)) + geom_boxplot()

# plot it
grouped %>% ggplot(aes(x=year,y=cpool, col=as.factor(run))) + 
  geom_line(show.legend = F)

grouped %>% ggplot(aes(x=year,y=lai, col=as.factor(run))) + 
  geom_line(show.legend = F)

act_cpool <- grouped %>% 
  dplyr::filter(year>2000 & year<2020) %>%
  ggplot() + geom_line(aes(x=year, y=cpool, col=as.factor(run)), show.legend = F)

# get rolling mean for 5-10 years
test2 <- arrange(grouped, run, year) %>%
  mutate(ma2=rollapply(lai, 5, mean, fill=NA)) %>%
  mutate(pc2=rollapply(plantc, 5, mean,fill=NA)) %>%
  mutate(pool2=rollapply(cpool, 5, mean,fill=NA))

running_lai <- test2 %>% dplyr::filter(year > 2000 &
                                         year < 2020) %>%
  ggplot() +
  geom_line(aes(x=year, y=ma2, col=as.factor(run)), show.legend=F) +
  geom_hline(aes(yintercept=low_lai)) +
  geom_hline(aes(yintercept=high_lai))

actual_lai <- test2 %>% dplyr::filter(year > 2000 &
                                        year < 2020) %>%
  ggplot() +
  geom_line(aes(x=year, y=lai, col=as.factor(run)), show.legend=F) +
  geom_hline(aes(yintercept=low_lai)) +
  geom_hline(aes(yintercept=high_lai))

running_plantc <- test2 %>% dplyr::filter(year > 2000 &
                                            year < 2020) %>%
  ggplot(aes(x=year, y=pc2, col=as.factor(run))) +
  geom_line(show.legend=F) +
  geom_hline(aes(yintercept=low_carb)) +
  geom_hline(aes(yintercept=high_carb))

running_cpool <- test2 %>% dplyr::filter(year > 2000 &
                                           year < 2020) %>%
  ggplot(aes(x=year, y=pool2, col=as.factor(run))) +
  geom_line(show.legend=F)

grid.arrange(running_cpool, act_cpool)
grid.arrange(running_lai, actual_lai)
grid.arrange(running_lai, running_plantc)

## filter by running mean AND current value
# for lai and plantc
mean_5 <- test2 %>% dplyr::filter(year == 2011) %>%
  dplyr::filter(ma2 > low_lai & ma2 < high_lai &
                  lai > low_lai & lai < high_lai)

fiveyravg <- grouped[grouped$run %in% mean_5$run,]

## plot what the average of the drought years look like
fiveyr_plt <- fiveyravg %>% dplyr::filter(year > 2010 & year < 2020) %>%
  ggplot(aes(x=year, y=lai, col=as.factor(run))) +
  geom_line(show.legend=F) + 
  geom_vline(aes(xintercept=2011)) +
  geom_vline(aes(xintercept=2014)) +
  geom_vline(aes(xintercept=2017))
fiveyr_plt


tenyr_plt <- tenyravg %>% dplyr::filter(year > 2010 & year < 2020) %>%
  ggplot(aes(x=year, y=lai, col=as.factor(run)), group=1) +
  geom_line(show.legend=F) + 
  geom_vline(aes(xintercept=2011)) +
  geom_vline(aes(xintercept=2014)) +
  geom_vline(aes(xintercept=2017))

grid.arrange(fiveyr_plt, tenyr_plt)

read_filt <- read[read$run %in% mean_5$run,]

### comparing growth / decline within a year of LAI rather than a single value of summertime LAI?
## so in the model, there is growing season and senescence, and the amount of growth depends on the year before? which means that we should be looking at the change from beginning to summer of 2014 LAI, which is not as steep as the other years - and there's more decline
read_filt %>% dplyr::filter(year > 2010 & year < 2020) %>% group_by(run, month, year) %>% summarize_at(vars(lai, plantc), funs(mean))  %>% ggplot() + geom_line(aes(x=month, y=lai, col=as.factor(year))) + facet_wrap(vars(run))
plotly::ggplotly()

read_filt %>% dplyr::filter(year > 2010 & year < 2020) %>% group_by(run, year, month) %>% summarize_at(vars(lai, plantc), funs(mean))  %>% ggplot() + geom_line(aes(x=month, y=lai, col=as.factor(run)), show.legend=F) + facet_wrap(vars(year))
plotly::ggplotly()

summ <- read_filt %>% 
  dplyr::filter(year > 2010 & year < 2020) %>%
  group_by(run, year) %>% summarize_at(vars(lai, plantc), funs(mean))  %>% ggplot() + geom_line(aes(x=year, y=plantc, col=as.factor(run)), show.legend=F) + ggtitle("filtered plantc")
plotly::ggplotly()

read_filt %>% dplyr::filter(year > 2010 & year < 2020) %>%
  group_by(year, month) %>% 
  summarize_at(vars(lai), funs(mean)) %>% 
  ggplot() + geom_point(aes(x=as.factor(month), y=lai, col=as.factor(year)))
plotly::ggplotly()

### tried to filter based on monthly values - how much growth occurrs in a single month? This is irrelevent now that using drought model with growing season lasting many months in some cases 
april <- read_filt

april2 <- april %>% dplyr::filter(year==2017 & month==4) %>%
  dplyr::filter(day==1 | day==30) %>%
  reshape2::dcast(run ~ day, value.var='lai',mean) %>%
  mutate(slope = (`30`-`1`)/30)

april3 <- april2[order(april2$slope, decreasing = T),]
################################


#### Join with params

params <- read.csv("../out/RHESSysIOinR_output/allsim/evergreen.sob_all_options.csv")
p <- params[,14:52]

p_names <- c(
             "pore_size_index",
             "psi_air_entry",
             "run",
             "epc.leaf_cn",
             "epc.branch_turnover",
             "epc.gl_smax",
             "epc.flnr_age_mult",
             "epc.litter_moist_coef",
             "epc.psi_close",
             "mrc.q10",
             "epc.vpd_close",
             "epc.leaf_turnover",
             "epc.froot_turnover",
             "epc.storage_transfer_prop",
             "epc.height_to_stem_coef",
             "epc.waring_pa",
             "epc.root_growth_direction",
             "epc.root_distrib_parm",
             "epc.proj_sla",
             "epc.alloc_stemc_leafc",
             "epc.ext_coef",
             "specific_rain_capacity",
             "epc.flnr_sunlit",
             "epc.flnr_shade",
             "epc.netpabs_sunlit",
             "epc.netpabs_shade",
             "epc.netpabs_age_mult",
             "epc.cpool_mort_fract",
             "epc.min_percent_leafg",
             "epc.livewood_turnover",
             "epc.waring_pb",
             "epc.max_storage_percent",
             "epc.min_leaf_carbon",
             "epc.resprout_leaf_carbon",
             "epc.alloc_frootc_leafc",
             "epc.frootc_crootc",
             "epc.alloc_prop_day_growth",
             "epc.gs_vpd_min",
             "epc.gs_vpd_max"
)

colnames(p) <- c(p_names)
joined <- left_join(read_ann, p, by='run')

adjust_params(read, p, param="epc.leaf_cn")
adjust_params(read, p, param="epc.waring_pa")
adjust_params(read, p, param="epc.root_distrib_parm")
adjust_params(read, p, param="epc.leaf_turnover")
adjust_params(read, p, param="epc.gl_smax")
adjust_params(read, p, param="epc.alloc_frootc_leafc")
adjust_params(read, p, param="epc.min_percent_leafg")
adjust_params(read, p, param="epc.storage_transfer_prop")
adjust_params(read, p, param="gs_ravg_days")
adjust_params(read, p, param="epc.alloc_prop_day_growth")

##########################################
#### testing sensitivity of the nitrif_parm_smax parameter - used to shift the curve that it related to drought 
ggplot(joined) + geom_jitter(aes(x=nitrif_parm_smax, y=lai)) + geom_smooth(aes(x=nitrif_parm_smax, y=lai), method = 'lm') + ggtitle("2011 LAI")

ggplot(joined) + geom_jitter(aes(x=nitrif_parm_smax, y=ma2)) + geom_smooth(aes(x=nitrif_parm_smax, y=ma2), method = 'lm') + ggtitle("5 yr running average 2011 LAI")

ggplot(joined) + geom_density(aes(x=nitrif_parm_smax)) +
  geom_density(data=p, aes(x=nitrif_parm_smax), col='red')

summary(joined$nitrif_parm_smax)


##### filter out specific runs

filtered <- params[params$...defs.shallow_NT.def.group_id %in% quag_params2$run,]

get_full <- params %>% dplyr::filter(
  `...defs.shallow_NT.def.group_id`== 363 |
  `...defs.shallow_NT.def.group_id`== 714 |
  `...defs.shallow_NT.def.group_id`== 1092)


ggplot(grouped_cpool) + geom_line(aes(x=year, y=lai, col=as.factor(run))) + 
  geom_line(aes(x=year, y=cpool/1000, col=as.factor(run)), linetype='dashed')

#### if want to filter by changes across years
## 0.192 is mean+1sd of ndvi change
filr <- sann %>%
  dplyr::filter(change1 < 0.192)

## we want the difference between 2011-14 to be greater than the difference between 2011-17, but not because it died
filr2 <- sann %>% dplyr::filter(change1 > change2)


## whats up with this cpool

carbs <- read %>% group_by(year, run) %>% 
  summarize_at(vars(cpool), list(mean))
head(carbs)
ggplot(carbs) + geom_line(aes(x=year, y=cpool, col=as.factor(run)), show.legend = F)

carbs %>% dplyr::filter(cpool < 5) %>% ggplot() + geom_line(aes(x=year, y=cpool, col=as.factor(run)), show.legend=F)
carbs2 <- carbs %>% group_by(run) %>% 
  summarize_at(vars(cpool), list(mean)) %>%
                 dplyr::filter(cpool < 5)

get_full <- params[params$`...defs.shallow_NT.def.group_id` %in% carbs2$run,]

##############################################################################

# filter by seasonal changes to LAI 
filt <- read %>% dplyr::filter(year > 2010 & year < 2020) %>%
  group_by(month, run) %>% summarize_at(vars(lai), funs(max))

ggplot(filt[filt$year==2014,]) + geom_line(aes(x=as.factor(month), y=lai, col=as.factor(run), group=run), show.legend = F) + facet_wrap('year') +
  geom_vline(aes(xintercept=10))

## filter by (1) does it change seasonally? is LAI higher in the summer than in the fall? so we know some senescence occurs 
filtd <- filt %>% 
  dcast(run ~ month, value.var='lai') %>%
  dplyr::filter(`8` > `11`) 
nrow(filtd)

## filter by (2) is the 2017 LAI greater than the 2014 LAI? 
filt_yr <- grouped %>% dplyr::filter(year<2018) %>%
  dcast(run ~ year, value.var='lai') %>%
  dplyr::filter(`2014` < `2017`) %>%
  mutate(change1 = (`2011`-`2014`)/`2011`) %>% 
  mutate(change2 = (`2011`-`2017`)/`2011`)

## combine these two filtered sets 
filtd_melt <- melt(filtd, variable.name = 'month', value.name = 'lai', id.vars='run')
filt_yr_melt <- melt(filt_yr, variable.name = 'year', value.name='lai', id.vars='run')


read_filt <- read[read$run %in% filtd$run,]
read_filt <- read_filt[read_filt$run %in% filt_yr$run,]

read_filt %>% group_by(year, run) %>% summarize_at(vars(lai), funs(mean)) %>%
  ggplot() + geom_line(aes(x=year, y=lai, col=as.factor(run)), show.legend=T)


## combine with the parameters to look at values for vpd thresholds during growing season 
params <- read.csv("RHESSysIOinR_output/allsim/evergreen.sob_all_options.csv")
params2 <- params[,14:18]
colnames(params2) <- c("epc.gs_vpd_min", 
                       "epc.gs_vpd_max",
                       "epc.gs_psi_max",
                       "epc.gs_psi_min",
                       "run")

read_filt2 <- read_filt %>% dplyr::filter(month==7 | month==6) %>% 
  group_by(run) %>% summarize_at(vars(lai), funs(mean))

## down to 499 parameter sets (about half of all of them)

## combine with the parameter values to plot 
joined <- inner_join(read_filt2, params2, by='run')

ggplot(joined) + geom_jitter(aes(x=epc.gs_vpd_min, y=lai, col=epc.gs_vpd_max)) + geom_smooth(aes(x=epc.gs_vpd_min, y=lai), method='lm')
ggplot(joined) + geom_jitter(aes(x=epc.gs_vpd_max, y=lai, col=epc.gs_vpd_min)) + geom_smooth(aes(x=epc.gs_vpd_max, y=lai), method='lm')
ggplot(joined) + 
  geom_density2d(aes(x=epc.gs_vpd_min, y=epc.gs_vpd_max, col=lai)) + 
  geom_point(aes(x=epc.gs_vpd_min, y=epc.gs_vpd_max, col=lai)) +
  geom_vline(aes(xintercept=mean(epc.gs_vpd_min))) + 
  geom_hline(yintercept = mean(joined$epc.gs_vpd_max))

joined %>% mutate(vpd_range = epc.gs_vpd_max - epc.gs_vpd_min) %>%
  ggplot() + geom_jitter(aes(x=vpd_range, y=lai)) +
  geom_smooth(aes(x=vpd_range, y=lai), method='lm')

#ggplot(joined) + geom_density(aes(x=epc.gs_ravg_days))
ggplot(joined) + geom_density(aes(x=epc.gs_vpd_min)) +
  geom_vline(aes(xintercept=mean(epc.gs_vpd_min))) 
ggplot(joined) + geom_density(aes(x=epc.gs_vpd_max)) + 
  geom_vline(aes(xintercept=mean(epc.gs_vpd_max))) 
ggplot(joined) + geom_density(aes(x=epc.gs_psi_min))
ggplot(joined) + geom_density(aes(x=epc.gs_psi_max))

## from looking at the density plot for vpd_max, there are two humps, the avg is in the middle, gonna play it safe and use the larger hump
read_filt3 <- dplyr::filter(joined, epc.gs_vpd_max < 625)
nrow(read_filt3)
ggplot(read_filt3) + 
       geom_density2d(aes(x=epc.gs_vpd_min, y=epc.gs_vpd_max, col=lai)) + 
       geom_hex(aes(x=epc.gs_vpd_min, y=epc.gs_vpd_max, col=lai)) +
       geom_vline(aes(xintercept=median(epc.gs_vpd_min))) + 
       geom_hline(yintercept = median(read_filt3$epc.gs_vpd_max))


read_filt3 <- read[read$run %in% read_filt3$run,]
ggplot(read_filt3) + geom_line(aes(x=date, y=lai, col=as.factor(run)), show.legend=T)

### leaving gs_ravg_days at 15 - lai wasn't too sensitive to this 
### narrowing VPD thresholds

