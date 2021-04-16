setwd("~/Documents/patches/out")
source("../R/readin_selected_vars.R")
source("../../GitHub/climfiles/mkwy.R")
library(tidyverse)
library(data.table)
library(zoo)
library(gridExtra)


## load in parameters 
params_dec <- read.csv("decid/allsim/veg.decid_all_options.csv")
params_dec$run <- params_dec$...defs.shallow_NT.def.group_id

## start with entire decid output
vars = c("lai",
         "plantc")
read <- readin_selected_vars(vars,  dir="decid/allsim/")
read <- mkdate(read)
max(read$run)

# lai_yrmo <- lai %>% dplyr::filter(month==6 | month==7) %>% group_by(year) %>% summarize_all(list(mean)) %>%
# dplyr::select(-day,-month, -basinID) %>% reshape2::melt(id.vars=c('year'))
# colnames(lai_yrmo) <- c("year","run","lai")
# 
# lai <- mkwy(lai)
# lai_wy <- lai %>% group_by(wy) %>% summarize_all(funs(mean))
# grouped <- lai_wy %>% dplyr::select(-day, -month, -year, -wymo, -basinID) %>% reshape2::melt(id.vars='wy')
# colnames(grouped) <- c("wy","run","lai")


grouped <- read %>% group_by(wy, run) %>%
  summarize_at(vars(lai, plantc), funs(mean))

roll_mean <- arrange(grouped, run, wy) %>%
  mutate(ma2=rollapply(lai, 5, mean, fill=NA)) %>%
  mutate(pc2=rollapply(plantc, 5, mean,fill=NA))


# load in David's data 
vis <- read.csv("/Volumes/GoogleDrive/My Drive/plant_params/DM_data/tree_and_turfgrass_monroe2_selected_weightedmeanVIs.csv")

#####################################################
### filter out EUGL first 

data <- read.csv("~/Google Drive/My Drive/patches/R/data/weighted_tree_summary.csv")
quag <- data[data$class == 34,]
low_lai <- quag$mean_wt_lai - 2*quag$sd_wt_lai
high_lai <- quag$mean_wt_lai + 2*quag$sd_wt_lai
low_carb <- quag$mean_wt_carb - 2*quag$sd_wt_carb
high_carb <- quag$mean_wt_carb + 2*quag$sd_wt_carb

read_ann <- roll_mean %>% dplyr::filter(wy == 2011) %>%
  dplyr::filter(lai > low_lai & lai < high_lai) %>%
  dplyr::filter(ma2 > low_lai & ma2 < high_lai)
  #dplyr::filter(pc2 > low_carb & pc2 < high_carb)  %>% 
  dplyr::filter(plantc > low_carb & plantc < high_carb)
nrow(read_ann)
# spltrun <- str_split(read_ann$run, pattern="_")
# read_ann$run <- as.numeric(lapply(spltrun, "[[", 2)) 
# eugl_params <- inner_join(params_dec, read_ann, by="run")

read_ann2 <- read[read$run %in% read_ann$run,]

read_chng <- read_ann2 %>% 
  dplyr::filter(year == 2011 | year== 2014 | year == 2017)  %>% 
  #dplyr::filter(month==6 | month==7) %>% 
  reshape2::dcast(run ~ year, value.var='lai',mean) %>% 
  mutate(change1 = (`2011`-`2014`)/`2011`) %>% 
  mutate(change2 = (`2011`-`2017`)/`2011`) %>%
  dplyr::filter(change1 > change2)
nrow(read_chng)

read_ann3 <- read_ann2[read_ann2$run %in% read_chng$run,]

density <- ggplot(read_chng) + 
  geom_density(aes(x=`2011`, col='11')) +
  geom_density(aes(x=`2014`, col='14')) + 
  geom_density(aes(x=`2017`, col='17'))

#########################
### select species from NDVI data 
eugl_data <- vis %>% dplyr::filter(sp_code==
                                     "EUGL")
### creates ndvi dataset
nd11 <- eugl_data %>% dplyr::filter(year==2011) %>% dplyr::select(NDVI)
nd14 <- eugl_data %>% dplyr::filter(year==2014) %>% dplyr::select(NDVI)
nd17 <-  eugl_data %>% dplyr::filter(year==2017) %>% dplyr::select(NDVI)

eugl_ndvi <- cbind(eleven=nd11, fourtn=nd14, seventn = nd17)
colnames(eugl_ndvi) = c('elevn', 'fourtn', 'seventn')
eugl_ndvi$decline <- (eugl_ndvi$elevn - eugl_ndvi$fourtn)/eugl_ndvi$elevn
eugl_ndvi$recover <- (eugl_ndvi$elevn - eugl_ndvi$seventn)/eugl_ndvi$elevn

nd_eu <- ggplot(eugl_ndvi) + geom_density(aes(x=elevn, col='11')) +
  geom_density(aes(x=fourtn, col='14')) + 
  geom_density(aes(x=seventn, col='17')) + 
  ggtitle("change in NDVI for EUGL (n=50)")

grid.arrange(nd_eu, density)

#### filter by month where LAI is highest - should be Dec, Jan, Feb for eugl ; april or may for plra 
## used grouped_wyd
maxl <- read_ann3 %>% group_by(month, run) %>%
  summarize_at(vars(lai), funs(mean)) %>% 
  group_by(run) %>% dplyr::filter(lai == max(lai))
hist(maxl$month)

#### for eucalyptus 
eucc <- maxl %>% dplyr::filter(month==12 | month==4)
nrow(eucc)
eucc2 <- read[read$run %in% eucc$run,]

# plots 
ggplot(eucc2) + geom_line(aes(x=date, y=lai, col=as.factor(run)), show.legend=F)
eucc2 %>% group_by(wyd, run) %>% summarize_at(vars(lai), list(mean)) %>% 
  ggplot() + geom_line(aes(x=wyd, y=lai, col=as.factor(run)), show.legend=F)

euc_lai <- eucc2 %>% 
  dplyr::filter(year == 2011 | year== 2014 | year == 2017)  %>% 
  dplyr::filter(month==6 | month==7) %>% 
  pivot_wider(id_cols='run', names_from='year', values_from='lai', values_fn=mean) %>% 
  mutate(change1 = (`2011`-`2014`)/`2011`) %>% 
  mutate(change2 = (`2011`-`2017`)/`2011`) %>%
  dplyr::filter(change2 < 0.12970) %>% 
  dplyr::filter(change1 < sum(mean(eugl_ndvi$decline), sd(eugl_ndvi$decline)))
nrow(euc_lai)

density2 <- ggplot(euc_lai) + geom_density(aes(x=`2011`, col='11')) + 
  geom_density(aes(x=`2014`, col='14')) + 
  geom_density(aes(x=`2017`, col='17')) + 
  ggtitle(paste("changes in EUGL LAI (n=",nrow(euc_lai),")"))

grid.arrange(nd_eu, density2)

### plot the change from 2011-2014 
## look at differences with lai and ndvi 
ggplot(read_chng) + geom_density(aes(x=change1, col='rhess')) +
  geom_density(data=eugl_ndvi, aes(x=decline, col='ndvi')) + 
  geom_density(data=euc_lai, aes(x=change1, col='filt')) + 
  ggtitle("change in LAI and NDVI 2011-2014")

ggplot(read_chng) + geom_density(aes(x=change2, col='rhess')) +
  geom_density(data=eugl_ndvi, aes(x=recover, col='ndvi')) + 
  geom_density(data=euc_lai, aes(x=change2, col='filt')) + 
  ggtitle("change in LAI and NDVI 2011-2017")

towrite <- read[read$run %in% euc_lai$run,]
write.csv(towrite, "../R/tree_outputs/eugl_outputs.csv")

p_eugl <- params_dec[params_dec$...defs.veg_decid.def.group_id %in% euc_lai$run,] %>% 
  mutate(code="eugl")

###########################################################
###########################################################
## for the sycamore trees 

### filter out sycamore from LAI and biomass estimates
quag <- data[data$class == 74,]
low_lai <- quag$mean_wt_lai - 2*quag$sd_wt_lai
high_lai <- quag$mean_wt_lai + 2*quag$sd_wt_lai
low_carb <- quag$mean_wt_carb - 2*quag$sd_wt_carb
high_carb <- quag$mean_wt_carb + 2*quag$sd_wt_carb

#########################
### filter from NDVI data
plra_data <- vis %>% dplyr::filter(sp_code==
                                     "PLRA")
nd11 <- plra_data %>% dplyr::filter(year==2011) %>% dplyr::select(NDVI)
nd14 <- plra_data %>% dplyr::filter(year==2014) %>% dplyr::select(NDVI)
nd17 <-  plra_data %>% dplyr::filter(year==2017) %>% dplyr::select(NDVI)

plra_ndvi <- cbind(eleven=nd11, fourtn=nd14, seventn = nd17)
colnames(plra_ndvi) = c('elevn', 'fourtn', 'seventn')
plra_ndvi$decline <- (plra_ndvi$elevn - plra_ndvi$fourtn)/plra_ndvi$elevn
plra_ndvi$recover <- (plra_ndvi$elevn - plra_ndvi$seventn)/plra_ndvi$elevn

nd_pl <- ggplot(plra_ndvi) + geom_density(aes(x=elevn, col='11')) +
  geom_density(aes(x=fourtn, col='14')) + 
  geom_density(aes(x=seventn, col='17')) + 
  ggtitle("change in NDVI for PLRA (n=50)")

#########################

## filter by the LAI and running avg for 2011
read_ann <- roll_mean %>% dplyr::filter(wy == 2011) %>%
  dplyr::filter(ma2 > low_lai & ma2 < high_lai &
                  lai > low_lai & lai < high_lai) %>% 
  dplyr::filter(plantc > low_carb & plantc < high_carb & 
                 pc2 > low_carb & pc2 < high_carb )
nrow(read_ann)
# spltrun <- str_split(read_ann$run, pattern="_")
# read_ann$run <- as.numeric(lapply(spltrun, "[[", 2)) 
# plra_params <- inner_join(params_dec, read_ann, by="run")
# filtered_dec <- rbind(eugl_params, plra_params)

read_ann2 <- read[read$run %in% read_ann$run,]

## filter by changes between years 
read_chng <- read_ann2 %>% 
  dplyr::filter(year == 2011 | year== 2014 | year == 2017)  %>% 
  dplyr::filter(month==6 | month==7) %>% 
  reshape2::dcast(run ~ year, value.var='lai',mean) %>% 
  mutate(change1 = (`2011`-`2014`)/`2011`) %>% 
  mutate(change2 = (`2011`-`2017`)/`2011`) %>%
  dplyr::filter(change1 > change2)
nrow(read_chng)

read_ann3 <- read_ann2[read_ann2$run %in% read_chng$run,]

## filter by 'greenest' month 
maxl <- read_ann3 %>% group_by(month, run) %>%
  summarize_at(vars(lai), funs(mean)) %>% 
  group_by(run) %>% dplyr::filter(lai == max(lai))
hist(maxl$month)

syc <- maxl %>% dplyr::filter(month==4 | month==3)
nrow(syc)
syc2 <- read[read$run %in% syc$run,]

# plot 
ggplot(syc2) + geom_line(aes(x=date, y=lai, col=as.factor(run)), show.legend=F)
syc2 %>% group_by(month, run) %>% summarize_at(vars(lai), list(mean)) %>% 
  ggplot() + geom_line(aes(x=month, y=lai, col=as.factor(run)), show.legend=F)

syc_lai <- syc2 %>% 
  dplyr::filter(year == 2011 | year== 2014 | year == 2017)  %>% 
  dplyr::filter(month==6 | month==7) %>% 
  pivot_wider(id_cols='run', 
              names_from='year', 
              values_from='lai', 
              values_fn=mean) %>% 
  mutate(change1 = (`2011`-`2014`)/`2011`) %>% 
  mutate(change2 = (`2011`-`2017`)/`2011`) %>%
  dplyr::filter(change2 < 0.087616) %>% 
  dplyr::filter(change1 < sum(mean(plra_ndvi$decline), sd(plra_ndvi$decline)))
nrow(syc_lai)

density3 <- ggplot(syc_lai) + geom_density(aes(x=`2011`, col='11')) + 
  geom_density(aes(x=`2014`, col='14')) + 
  geom_density(aes(x=`2017`, col='17')) + 
  ggtitle(paste("changes in PLRA LAI (n=",nrow(syc_lai),")"))

grid.arrange(nd_pl, density3)

ggplot() + geom_density(data=syc_lai, aes(x=change1, col='rhess')) + 
  geom_density(data=plra_ndvi, aes(x=decline, col='ndvi')) + ggtitle("% changes 2011-2014")

ggplot() + geom_density(data=syc_lai, aes(x=change2, col='rhess')) + 
  geom_density(data=plra_ndvi, aes(x=recover, col='ndvi')) + ggtitle("% changes 2011-2017")

towrite <- read[read$run %in% syc_lai$run,]
write.csv(towrite, "../R/tree_outputs/plra_outputs.csv")

p_plra <- params_dec[params_dec$...defs.veg_decid.def.group_id %in% syc_lai$run,] %>% 
  mutate(code="plra")

all_params_d <- rbind(p_eugl, p_plra)
write.csv(all_params_d, "../R/tree_outputs/decid_params.csv")

grid.arrange(nd_eu, nd_pl, density2, density3, ncol=2)



