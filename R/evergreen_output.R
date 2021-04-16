## start with entire evergreen output
## conifer - pinus canariensis - code is 66
## nonnative broadleaf - 73 | 86 | 60
## native broadleaf - liveoak - 81

install.packages("parallel")
library(parallel)

setwd("~/Documents/patches/out")
library(RHESSysIOinR)
library(tidyverse)
library(reshape2)
library(gridExtra)
library(zoo)
library(data.table)
source("../R/readin_selected_vars.R")
source("~/Documents/GitHub/climfiles/mkwy.R")

### load in RHESSys output 
vars = c("lai",
         "plantc")

### this loads in output 
# setwd("RHESSysIOinR_output/allsim/")
# readin_large <- function(var){
# 	read <- fread(paste(var))
# 	read <- mkwy(read)
# 	read_wy <- read %>% group_by(wy) %>% summarize_all(funs(mean))
# 	read_melt <- read_wy %>% dplyr::select(-day, -month, -year, -wymo, -basinID) %>% reshape2::melt(id.vars='wy')
# 	colnames(read_melt) <- c("wy","run",var)
# 	return(read_melt)
# }
# lai <- fread("RHESSysIOinR_output/allsim/lai")
# lai_yrmo <- lai %>% dplyr::filter(month==6 | month==7) %>% group_by(year) %>% summarize_all(list(mean)) %>%
# dplyr::select(-day,-month, -basinID) %>% reshape2::melt(id.vars=c('year'))
# colnames(lai_yrmo) <- c("year","run","lai")
# lai_wy <- mkwy(lai) %>% group_by(wy) %>% summarize_all(funs(mean))
# grouped <- lai_wy %>% dplyr::select(-day, -month, -year, -wymo, -basinID) %>% reshape2::melt(id.vars='wy')
# colnames(grouped) <- c("wy","run","lai")

# lai <- readin_large("lai")
# plantcarb <- readin_large("plantc")
# grouped <- inner_join(lai_melt, plantcarb, by=c('wy','run'))


read <- readin_selected_vars(vars=vars, dir="RHESSysIOinR_output/allsim")
read <- mkwy(read)

grouped <- read %>% group_by(wy, run) %>%
  summarize_at(vars(lai, plantc), funs(mean))

roll_mean <- arrange(grouped, run, wy) %>%
  mutate(ma2=rollapply(lai, 3, mean, fill=NA, align='right')) %>% 
  mutate(forwrd = rollapply(lai, 3, mean, fill=NA, align='left')) %>%
  mutate(pc2=rollapply(plantc, 5, mean,fill=NA))

ggplot(roll_mean) + geom_line(aes(x=year, y=lai, col=as.factor(run)), show.legend=F)

# load in David's data 
vis <- read.csv("/Volumes/GoogleDrive/My Drive/plant_params/DM_data/tree_and_turfgrass_monroe2_selected_weightedmeanVIs.csv")
data <- read.csv("~/Google Drive/My Drive/patches/R/data/weighted_tree_summary.csv")

# load in parameters
params_ev <- read.csv("RHESSysIOinR_output/allsim/evergreen_all_options.csv")
params_ev$run <- params_ev$...defs.shallow_NT.def.group_id

#### have to recompile the parameters with quag runs that passed the test 
readin <- read.csv("../R/tree_outputs/quag_outputs.csv")
read_ann <- readin %>% group_by(run, year) %>%
  summarize_at(vars(lai), funs(mean)) %>% 
  dplyr::filter(year==2011)

params_ev <- read.csv("../R/tree_outputs/evg_params.csv")
params_q <- params_ev[params_ev$code == "quag",]
params_pi <- params_ev[params_ev$code != "quag",]
new_params <- params_q[params_q$...defs.shallow_NT.def.group_id %in% read_ann$run,]
to_write <- rbind(params_pi, new_params)
write.csv(to_write, "../R/tree_outputs/evg_params.csv")

params <- read.csv("RHESSysIOinR_output/allsim/conifer_all_options.csv")
params$run <- params$...defs.shallow_NT.def.group_id
join <- left_join(read_ann, params, by='run')
### params changing for pica
# epc.proj_sla 
# epc.leaf_turnover
# epc.leaf_cn
# epc.branch_turnover 
# epc.waring_pa
# epc.psi_close
## read in newer PICA 

read_picatest <- readin_selected_vars(vars=vars, dir="~/Documents/patches/out/RHESSysIOinR_output/allsim/")
read_picatest <- mkdate(read_picatest)
max(read_picatest$run)

#####################################################
### filter out PICA first 
                
quag <- data[data$class == 66,]
low_lai <- quag$mean_wt_lai - 2*quag$sd_wt_lai
high_lai <- quag$mean_wt_lai + 2*quag$sd_wt_lai
low_carb <- quag$mean_wt_carb - 2*quag$sd_wt_carb
high_carb <- quag$mean_wt_carb + 2*quag$sd_wt_carb

read_ann <- roll_mean %>% dplyr::filter(wy == 2011) %>%
  dplyr::filter(lai > low_lai & lai < high_lai) %>%
  dplyr::filter(ma2 > low_lai & ma2 < high_lai) %>% 
  #dplyr::filter(pc2 > low_carb & pc2 < high_carb) %>% 
  dplyr::filter(plantc > low_carb & plantc < high_carb)
nrow(read_ann)

pica_params <- inner_join(params_ev, read_ann, by="run")

read_ann2 <- read[read$run %in% read_ann$run,]

read_chng <- read_ann2 %>% 
  dplyr::filter(year == 2011 | year== 2014 | year == 2017)  %>% 
  reshape2::dcast(run ~ year, value.var='lai',mean) %>% 
  mutate(change1 = (`2011`-`2014`)/`2011`) %>% 
  mutate(change2 = (`2011`-`2017`)/`2011`) %>%
  dplyr::filter(change1 > change2)
nrow(read_chng)

read_ann3 <- read_ann2[read_ann2$run %in% read_chng$run,]

density_pi <- ggplot(read_chng) + 
  geom_density(aes(x=`2011`, col='11')) +
  geom_density(aes(x=`2014`, col='14')) + 
  geom_density(aes(x=`2017`, col='17')) + 
  ggtitle(paste("change in LAI for PICA (n=",nrow(read_chng), ")"))

#########################
### select species from NDVI data 
pica_data <- vis %>% dplyr::filter(sp_code==
                                     "PICA")
### creates ndvi dataset
nd11 <- pica_data %>% dplyr::filter(year==2011) %>% dplyr::select(NDVI)
nd14 <- pica_data %>% dplyr::filter(year==2014) %>% dplyr::select(NDVI)
nd17 <-  pica_data %>% dplyr::filter(year==2017) %>% dplyr::select(NDVI)

pica_ndvi <- cbind(eleven=nd11, fourtn=nd14, seventn = nd17)
colnames(pica_ndvi) = c('elevn', 'fourtn', 'seventn')
pica_ndvi$decline <- (pica_ndvi$elevn - pica_ndvi$fourtn)/pica_ndvi$elevn
pica_ndvi$recover <- (pica_ndvi$elevn - pica_ndvi$seventn)/pica_ndvi$elevn

nd_pi <- ggplot(pica_ndvi) + geom_density(aes(x=elevn, col='11')) +
  geom_density(aes(x=fourtn, col='14')) + 
  geom_density(aes(x=seventn, col='17')) + 
  ggtitle("change in NDVI for PICA (n=50)")

grid.arrange(nd_pi, density_pi)


#########################

#### filter by month where LAI is highest - should be Dec, Jan, Feb for eugl ; april or may for plra 
## used grouped_wyd
maxl <- read_ann3 %>% group_by(month, run) %>%
  summarize_at(vars(lai), funs(mean)) %>% 
  group_by(run) %>% dplyr::filter(lai == max(lai))
hist(maxl$month)

#### for pica (needleleaf) - 12 or 1 
picaa <- maxl %>% dplyr::filter(month<=2 | month==12)
nrow(picaa)
pica2 <- read[read$run %in% picaa$run,]

## plots # by date
ggplot(pica2) + geom_line(aes(x=date, y=lai, col=as.factor(run)), show.legend=F)
# seasonal - by wyd
pica2 %>% group_by(wyd, run) %>% summarize_at(vars(lai), list(mean)) %>% 
  ggplot() + geom_line(aes(x=wyd, y=lai, col=as.factor(run)), show.legend=F)

pic_lai <- read_ann3 %>% 
  dplyr::filter(year == 2011 | year== 2014 | year == 2017)  %>% 
  dplyr::filter(month==6 | month==7) %>% 
  dcast(run ~ year, value.var='lai',mean) %>% 
  mutate(change1 = (`2011`-`2014`)/`2011`) %>% 
  mutate(change2 = (`2011`-`2017`)/`2011`) %>% 
  dplyr::filter(change2 < sum(mean(pica_ndvi$recover),sd(pica_ndvi$recover))) %>%
  dplyr::filter(change1 < max(pica_ndvi$decline)) # make sure the decline is not too large 
nrow(pic_lai)

density_pica <- ggplot(pic_lai) + geom_density(aes(x=`2011`, col='11')) + 
  geom_density(aes(x=`2014`, col='14')) + 
  geom_density(aes(x=`2017`, col='17')) + 
  ggtitle(paste("change in LAI for PICA (n=",nrow(pic_lai), ") filtered by month with highest LAI"))

grid.arrange(nd_pi, density_pica, density_pi)

### plot the change from 2011-2014 
## look at differences with lai and ndvi 
ggplot(read_chng) + geom_density(aes(x=change1, col='rhessys')) +
  geom_density(data=pica_ndvi, aes(x=decline, col='ndvi')) + 
  geom_density(data=pic_lai, aes(x=change1, col='pic_lai')) + 
  ggtitle("change in LAI and NDVI 2011-2014")

ggplot(read_chng) + geom_density(aes(x=change2, col='rhessys')) +
  geom_density(data=pica_ndvi, aes(x=recover, col='ndvi')) + 
  geom_density(data=pic_lai, aes(x=change2, col='pic_lai')) + 
  ggtitle("change in LAI and NDVI 2011-2017")
##

towrite <- read[read$run %in% pic_lai$run,]
write.csv(towrite, "~/Documents/patches/R/tree_outputs/pica_outputs.csv")

p_pica <- params[params$...defs.veg_liveoak.def.group_id %in% pic_lai$run,] %>% 
  mutate(code = "pica")
write.csv(p_pica, "~/Documents/patches/R/tree_outputs/pica_conifer_params.csv")
###########################################################
###########################################################
## for the coast live oak 

### LAI and biomass estimates
quag <- data[data$class == 81,]
low_lai <- quag$mean_wt_lai - 2*quag$sd_wt_lai
high_lai <- quag$mean_wt_lai + 2*quag$sd_wt_lai
low_carb <- quag$mean_wt_carb - 2*quag$sd_wt_carb
high_carb <- quag$mean_wt_carb + 2*quag$sd_wt_carb

#########################
### filter from NDVI data
quag_data <- vis %>% dplyr::filter(sp_code==
                                     "QUAG")
nd11 <- quag_data %>% dplyr::filter(year==2011) %>% dplyr::select(NDVI)
nd14 <- quag_data %>% dplyr::filter(year==2014) %>% dplyr::select(NDVI)
nd17 <-  quag_data %>% dplyr::filter(year==2017) %>% dplyr::select(NDVI)

quag_ndvi <- cbind(eleven=nd11, fourtn=nd14, seventn = nd17)
colnames(quag_ndvi) = c('elevn', 'fourtn', 'seventn')
quag_ndvi$decline <- (quag_ndvi$elevn - quag_ndvi$fourtn)/quag_ndvi$elevn
quag_ndvi$recover <- (quag_ndvi$elevn - quag_ndvi$seventn)/quag_ndvi$elevn

nd_qu <- ggplot(quag_ndvi) + geom_density(aes(x=elevn, col='11')) +
  geom_density(aes(x=fourtn, col='14')) + 
  geom_density(aes(x=seventn, col='17')) + 
  ggtitle("change in NDVI for QUAG (n=50)")
grid.arrange(nd_pi, nd_qu)
#########################

## filter by the LAI and running avg for 2011
read_ann <- roll_mean %>% dplyr::filter(wy == 2011) %>%
  dplyr::filter(ma2 > low_lai & ma2 < high_lai &
                  lai > low_lai & lai < high_lai) 
#%>%
 # dplyr::filter(plantc > low_carb & plantc < high_carb)
nrow(read_ann)
ggplot(read_ann) + geom_boxplot(aes(y=plantc)) +
  geom_hline(aes(yintercept=low_carb, col='bound')) +
  geom_hline(aes(yintercept=high_carb, col='bound'))

spltrun <- str_split(read_ann$run, pattern="_")
read_ann$run <- as.numeric(lapply(spltrun, "[[", 2)) 
quag_params <- inner_join(params_ev, read_ann, by="run")
filtered <- rbind(pica_params, quag_params)

read_ann2 <- roll_mean[roll_mean$run %in% read_ann$run,]
read_ann2 <- read[read$run %in% read_ann$run,]

read_ann2 %>% dplyr::filter(year>2009) %>% ggplot() + geom_line(aes(x=year, y=lai, col=run), show.legend=F) + geom_vline(aes(xintercept=2011)) + geom_vline(aes(xintercept=2014)) + geom_vline(aes(xintercept=2017))


## filter by changes between years 
read_chng <- read_ann2 %>% 
  dplyr::filter(year == 2011 | year== 2014 | year == 2017)  %>% 
  #dplyr::filter(month==6 | month==7) %>% 
  reshape2::dcast(run ~ year, value.var='lai', mean) %>% 
  mutate(change1 = (`2011`-`2014`)/`2011`) %>% 
  mutate(change2 = (`2011`-`2017`)/`2011`) %>%
  dplyr::filter(change1 > change2)
nrow(read_chng)


read_chng2 <- inner_join(read_chng, params_ev, by='run')
ggplot(read_chng2) + geom_density(aes(x=`2011`, col="2011")) + geom_density(aes(x=`2014`, col="2014")) +
geom_density(aes(x=`2017`, col="2017"))

ggplot(read_chng2) + geom_point(aes(x=`2011`, y=`2017`, col=`...defs.veg_liveoak.def.epc.leaf_turnover`)) +
  geom_abline(aes(slope=1, intercept=0))


read_ann3 <- read_ann2[read_ann2$run %in% read_chng$run,]
ggplot(read_ann3) + geom_line(aes(x=year, y=lai))


## filter by 'greenest' month 
maxl <- read_ann3 %>% group_by(month, run) %>%
  summarize_at(vars(lai), funs(mean)) %>% 
  group_by(run) %>% dplyr::filter(lai == max(lai))
hist(maxl$month)

## for coast live oak its anywhere from 12-4
quagg <- maxl %>% dplyr::filter(month<=4)
nrow(quagg)
quag2 <- read[read$run %in% quagg$run,]

## plot it 
ggplot(quag2) + geom_line(aes(x=date, y=lai, col=as.factor(run)), show.legend=F)
quag2 %>% group_by(month, run) %>% summarize_at(vars(lai), list(mean)) %>% 
  ggplot() + geom_line(aes(x=month, y=lai, col=as.factor(run)), show.legend=F)

quag_lai <- quag2 %>% 
  dplyr::filter(year == 2011 | year== 2014 | year == 2017)  %>% 
  dplyr::filter(month==6 | month==7) %>% 
  dcast(run ~ year, value.var='lai',mean) %>% 
  mutate(change1 = (`2011`-`2014`)/`2011`) %>% 
  mutate(change2 = (`2011`-`2017`)/`2011`) %>%
  dplyr::filter(`2011`>`2017`) %>% # make sure the 2017 LAI still isn't all the way back to 2011 value
  dplyr::filter(change1 < max(quag_ndvi$decline)) # make sure the decline in 2014 is not too large ^ max values of ndvi change 
nrow(quag_lai)

density_quag <- ggplot(quag_lai) + 
  geom_density(aes(x=`2011`, col='11')) + 
  geom_density(aes(x=`2014`, col='14')) + 
  geom_density(aes(x=`2017`, col='17')) + 
  ggtitle(paste("change in LAI for QUAG (n=",nrow(quag_lai), ")"))

grid.arrange(nd_qu, density_quag)

ggplot() + geom_density(data=quag_lai, aes(x=change1, col='rhessys')) + 
  geom_density(data=quag_ndvi, aes(x=decline, col='ndvi')) + ggtitle("% changes from 2011-2014")

ggplot() + geom_density(data=quag_lai, aes(x=change2, col='rhessys')) + 
  geom_density(data=quag_ndvi, aes(x=recover, col='ndvi')) + ggtitle("% changes from 2011-2017")

quag_lai_towrite <- read[read$run %in% quag_lai$run,]
write.csv(quag_lai_towrite, "../R/tree_outputs/quag_outputs.csv")

p_quag <- params_ev[params_ev$...defs.veg_liveoak.def.group_id %in% quag_lai$run,] %>% 
  mutate(code="quag")

#########################################
## 
## broadleaf evergreens - nonnative - not oak
## represented by pittosporum undulatum

### LAI and biomass estimates
quag <- data[data$class == 73,]
low_lai <- quag$mean_wt_lai - 2*quag$sd_wt_lai
high_lai <- quag$mean_wt_lai + 2*quag$sd_wt_lai
low_carb <- quag$mean_wt_carb - 2*quag$sd_wt_carb
high_carb <- quag$mean_wt_carb + 2*quag$sd_wt_carb

#########################
### filter from NDVI data
piun_data <- vis %>% dplyr::filter(sp_code==
                                     "PIUN")
nd11 <- piun_data %>% dplyr::filter(year==2011) %>% dplyr::select(NDVI)
nd14 <- piun_data %>% dplyr::filter(year==2014) %>% dplyr::select(NDVI)
nd17 <- piun_data %>% dplyr::filter(year==2017) %>% dplyr::select(NDVI)

piun_ndvi <- cbind(eleven=nd11, fourtn=nd14, seventn = nd17)
colnames(piun_ndvi) = c('elevn', 'fourtn', 'seventn')
piun_ndvi$decline <- (piun_ndvi$elevn - piun_ndvi$fourtn)/piun_ndvi$elevn
piun_ndvi$recover <- (piun_ndvi$elevn - piun_ndvi$seventn)/piun_ndvi$elevn

nd_pu <- ggplot(piun_ndvi) + geom_density(aes(x=elevn, col='11')) +
  geom_density(aes(x=fourtn, col='14')) + 
  geom_density(aes(x=seventn, col='17')) + 
  ggtitle("change in NDVI for PIUN (n=50)")
grid.arrange(nd_pi, nd_qu, nd_pu)
#########################

## filter by the LAI and running avg for 2011
read_ann <- roll_mean %>% dplyr::filter(wy == 2011) %>%
  dplyr::filter(ma2 > low_lai & ma2 < high_lai &
                  lai > low_lai & lai < high_lai) %>% 
  dplyr::filter(plantc > low_carb & plantc < high_carb)
nrow(read_ann)

read_ann2 <- read[read$run %in% read_ann$run,]

## filter by changes between years 
read_chng <- read_ann2 %>% 
  dplyr::filter(year == 2011 | year== 2014 | year == 2017)  %>% 
  reshape2::dcast(run ~ year, value.var='lai',mean) %>% 
  mutate(change1 = (`2011`-`2014`)/`2011`) %>% 
  mutate(change2 = (`2011`-`2017`)/`2011`) %>%
  dplyr::filter(change1 > change2)
nrow(read_chng)

read_ann3 <- read_ann2[read_ann2$run %in% read_chng$run,]
read_ann3 %>% dplyr::filter(year>2009) %>%
ggplot() + geom_line(aes(x=year, y=lai, col=run), show.legend=F) +
geom_vline(aes(xintercept=2011)) + geom_vline(aes(xintercept=2017))

read_ann3 %>% dplyr::filter(year>2010) %>% ggplot() + geom_boxplot(aes(x=as.factor(year), y=lai))

## filter by 'greenest' month 
maxl <- read_ann3 %>% group_by(month, run) %>%
  summarize_at(vars(lai), funs(mean)) %>% 
  group_by(run) %>% dplyr::filter(lai == max(lai))
hist(maxl$month)

## for coast live oak its anywhere from 3-7
piunn <- maxl %>% dplyr::filter(month>3 & month<8)
nrow(piunn)
piun2 <- read[read$run %in% piunn$run,]

##plots 
ggplot(piun2) + geom_line(aes(x=date, y=lai, col=as.factor(run)), show.legend=F)

piun2 %>% group_by(month, run) %>% summarize_at(vars(lai), list(mean)) %>% 
  ggplot() + geom_line(aes(x=month, y=lai, col=as.factor(run)), show.legend=F)

piun_lai <- piun2 %>% 
  dplyr::filter(year == 2011 | year== 2014 | year == 2017)  %>% 
  dplyr::filter(month==6 | month==7) %>% 
  dcast(run ~ year, value.var='lai',mean) %>% 
  mutate(change1 = (`2011`-`2014`)/`2011`) %>% 
  mutate(change2 = (`2011`-`2017`)/`2011`) %>%
  dplyr::filter(change1 < 0.3043443) # mean % diff NDVI + 1 sd 
nrow(piun_lai)

density_piun <- ggplot(piun_lai) + geom_density(aes(x=`2011`, col='11')) + 
  geom_density(aes(x=`2014`, col='14')) + 
  geom_density(aes(x=`2017`, col='17')) + 
  ggtitle(paste("change in LAI for PIUN (n=",nrow(read_chng), ")"))

grid.arrange(nd_pu, density_piun)

ggplot() +
  geom_density(data=piun_lai, aes(x=change1, col='rhessys')) + 
  geom_density(data=piun_ndvi, aes(x=decline, col='ndvi')) + ggtitle("% changes 2011-2014")

ggplot() +
  geom_boxplot(data=piun_lai, aes(x='model', y=change1, col='rhessys')) + 
  geom_boxplot(data=piun_ndvi, aes(x='ndvi', y=decline, col='ndvi')) + ggtitle("% changes 2011-2014")

ggplot() +
  geom_density(data=piun_lai, aes(x=change2, col='rhessys')) + 
  geom_density(data=piun_ndvi, aes(x=recover, col='ndvi')) + ggtitle("% changes 2011-2017")

ggplot() +
  geom_boxplot(data=piun_lai, aes(x='model', y=change2, col='rhessys')) + 
  geom_boxplot(data=piun_ndvi, aes(x='ndvi', y=recover, col='ndvi')) + ggtitle("% changes 2011-2017")

towrite <- read[read$run %in% piun_lai$run,]
towrite <- piun_lai %>% dplyr::select(year, month, day, read_chng$run)
write.csv(towrite, "../R/tree_outputs/piun_outputs.csv")

spltrun <- str_split(piun_lai$run, pattern="_")
piun_lai$run <- as.numeric(lapply(spltrun, "[[", 2))

p_piun <- params_ev[params_ev$...defs.veg_liveoak.def.group_id %in% piun_lai$run,] %>% 
  mutate(code="piun")
write.csv(p_piun, "../R/tree_outputs/piun_params.csv")

all_params_e <- rbind(p_quag, p_piun)
write.csv(all_params_e, "../R/tree_outputs/evg_broadleaf_params.csv")

## compare all evergreen 
grid.arrange(nd_pi, nd_qu, nd_pu,
             density_pica, density_quag, density_piun, ncol=3)




#### save the runs from the decid/eugl/read
#write.csv(eucc2, "../out/decid/euc_filtered_outputs.csv")
#write.csv(syc2, "../out/decid/syc_filtered_outputs.csv")

##### can we filter by the %changes? or the LAI in 2011? 

