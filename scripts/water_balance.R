library(tidyverse)
library(patchwork)
library(lubridate)
library(RHESSysIOinR)
setwd("~/VegParamsSA/out")
source("/Volumes/GoogleDrive/My Drive/help_with_rhessys/watbal.R")

# output from regular old way 
# out = readin_rhessys_output("testing")
# 
# water = watbal(out$bd)
# 
# water %>% dplyr::filter(year<1960) %>% ggplot() + geom_point(aes(x=date, y=watbal))

# output from output filters
wbal = read.csv("~/VegParamsSA/out/wb_bas.csv") %>% mkdate() %>%
  mutate(canopy_snow_stored = snow_stored,
         canopy_rain_stored = rain_stored) %>% 
  watbal_basin_of()

ggplot(wbal, aes(x=date, y=watbal)) + geom_point()
       
ggplot(wbal, aes(x=date, y=watbal)) + geom_point() + 
  geom_line(aes(x=date, y=rain, col='rain'))

wbal %>% dplyr::filter(year<2000) %>% ggplot() + geom_point(aes(x=date, y=watbal)) +
  geom_line(aes(x=date, y=rz_storage, col="rz_stor")) 

tmp=diff(wbal$rz_storage)
tmp=c(0,tmp)
wbal$rzdiff = tmp

wbal %>% dplyr::filter(year<2000) %>% ggplot() + geom_point(aes(x=date, y=watbal)) +
  geom_line(aes(x=date, y=rz_storage, col='rz')) + 
  geom_line(aes(x=date, y=sat_deficit, col="sd")) 

wbal %>% dplyr::filter(year==1996 & month<4) %>% ggplot() + geom_point(aes(x=date, y=watbal)) +
  geom_line(aes(x=date, y=trans, col="transp")) + 
  geom_line(aes(x=date, y=streamflow, col="R")) + 
  geom_line(aes(x=date, y=recharge, col="inf"))

wbal %>% dplyr::filter(year==1996 & month<4) %>% 
  mutate(inf_diff = watbal - recharge) %>% 
  ggplot() + geom_line(aes(x=date, y=watbal, col="bal")) + 
  geom_line(aes(x=date, y=inf_diff, col="diff")) +
  geom_line(aes(x=date, y=detention_store, col="ds"))

## for checking surface transfers
wb_p <- read.csv("~/VegParamsSA/out/wb_p.csv") %>% mkdate()

ggplot(wb_p) + geom_line(aes(x=date, y=surface_transfer, col=as.factor(patchID)))

ggplot(wb_p) + geom_line(aes(x=date, y=sum(surface_transfer)))

ggplot(wb_p) + geom_line(aes(x=date, y=rz_transfer, col=as.factor(patchID)))
ggplot(wb_p) + geom_line(aes(x=date, y=sum(rz_transfer)))
## does not add to zero - try weighting by area and adding also 

ggplot(wb_p) + geom_line(aes(x=date, y=unsat_transfer, col=as.factor(patchID)))

ggplot(wb_p) + geom_point(aes(x=date, y=recharge, col=as.factor(patchID)))

ggplot(wb_p) + geom_point(aes(x=date, y=streamflow, col=as.factor(patchID)))

ggplot(wb_p) + geom_point(aes(x=date, y=rain_throughfall)) + 
  geom_line(aes(x=date, y=recharge, col='recharge')) + 
  geom_line(aes(x=date, y=streamflow, col='streamdflow')) + 
  facet_grid('patchID')


# transfers = wb_p %>% 
#   #dplyr::filter(yd>300) %>% 
#   pivot_wider(id_cols = c(yd, date), values_from=c(surface_transfer), names_from="patchID", names_prefix = "p") %>%
#   mutate(sum_transfers = p60853701 + p60853702 + p60853703)

transfers = wb_p %>% 
  #dplyr::filter(yd>300) %>% 
  pivot_wider(id_cols = c(yd, date), values_from=c(surface_transfer), names_from="patchID", names_prefix = "p") %>%
  mutate(area_pav = p60853701*66.84,
         area_oak = p60853702*33.42,
         area_tg = p60853703*33.42) %>% 
  pivot_longer(cols=c('area_oak',"area_pav", "area_tg"), names_to="area_wtd")

ggplot(transfers) + geom_line(aes(x=date, y=value, col=area_wtd)) 
ggplot(transfers) + geom_line(aes(x=date, y=sum(value), col=area_wtd)) 

###--- read in stratum output ----
strat = read.csv("wb_strat.csv") %>% mkdate()

# plot evapotranspiration 
ggplot(strat) + geom_line(aes(x=date, y=et, col=as.factor(patchID)))

ggplot(strat) + geom_line(aes(x=date, y=trans, col=as.factor(patchID)))

# make an msr test patch with 1 tree and 1 grass (i.e. edit the 3fam worldfile and takeout impervious)
wb_p = strat
# check on vegetation growth
ggplot(wb_p) + geom_line(aes(x=date, y=epv.height, col=as.factor(patchID)))
ggplot(wb_p) + geom_line(aes(x=date, y=cs.live_stemc, col=as.factor(patchID)))
ggplot(wb_p) + geom_line(aes(x=date, y=cs.totalc, col=as.factor(patchID)))
ggplot(wb_p) + geom_line(aes(x=date, y=epv.proj_lai, col=as.factor(patchID)))
ggplot(wb_p) + geom_line(aes(x=date, y=rootzone.depth, col=as.factor(patchID)))
ggplot(wb_p) + geom_line(aes(x=date, y=trans, col=as.factor(patchID))) 
ggplot(wb_p) + geom_line(aes(x=date, y=cdf.psn_to_cpool, col=as.factor(patchID)))
ggplot(wb_p) + geom_line(aes(x=date, y=npp, col=as.factor(patchID)))


## zoom in time 
april = wb_p %>% dplyr::filter(year==1995 & month==4 & day>10 & day<20)

april %>% dplyr::filter(patchID == 60853701) %>% 
  ggplot(aes(x=day, y=recharge)) + 
  geom_line(aes(x=day, y=rain_throughfall, col="rain")) + 
  geom_point() + 
  geom_point(aes(x=day, y=surface_transfer, col="trans")) +
  geom_line(aes(x=day, y=streamflow, col="str")) + 
  geom_line(data= wbal[wbal$date %in% april$date,], aes(x=day, y=streamflow))

rzs = wb_p %>% dplyr::filter(year==1996 & patchID == 60853701) %>% 
  ggplot(aes(x=date, y=rz_storage)) +
  geom_line() 

re = wb_p %>% dplyr::filter(year==1996 & patchID == 60853701) %>% 
  ggplot(aes(x=date, y=recharge)) +
  geom_line()

re/rzs

## compare with and without msr 
ymsr = read.csv("wb_p.csv") %>% mutate(msr="on") %>% mkdate()
ymsr_bas = read.csv("wb_bas.csv") %>% mutate(msr="on") %>% mkdate()
# after runnning without and loading in as wb_p
nomsr = read.csv("wb_p.csv") %>% mutate(msr="off") %>% mkdate()
nomsr_bas = read.csv("wb_bas.csv") %>% mutate(msr="off") %>% mkdate()

c_bas = rbind(ymsr_bas, nomsr_bas) %>% 
  mutate(canopy_snow_stored = snow_stored,
         canopy_rain_stored = rain_stored) %>% 
  watbal_basin_of()

ggplot(c_bas) + geom_point(aes(x=date, y=watbal, col=msr))

compare_res = rbind(nomsr, ymsr) %>% 
  #dplyr::filter(patchID==60853701) %>% 
  mutate(sd_bal = sat_deficit-rz_storage-unsat_storage)

# plot journey 
ggplot(compare_res) + geom_point(aes(x=date, y=total_water_in, col=msr))
ggplot(compare_res) + geom_point(aes(x=date, y=streamflow, col=msr))
ggplot(compare_res) + geom_point(aes(x=date, y=recharge, col=msr))
ggplot(compare_res) + geom_point(aes(x=date, y=rz_storage, col=msr)) + facet_grid('patchID')
ggplot(compare_res) + geom_point(aes(x=date, y=trans, col=msr)) + facet_grid('patchID')
ggplot(compare_res) + geom_point(aes(x=date, y=unsat_storage, col=msr)) + facet_grid('patchID')
ggplot(compare_res) + geom_point(aes(x=date, y=exfiltration_unsat_zone, col=msr))
ggplot(compare_res) + geom_point(aes(x=date, y=rootzone.S, col=msr)) + facet_grid('patchID')

start = compare_res %>% dplyr::filter(year<1996 & month>=4)
ggplot(start) + geom_point(aes(x=date, y=recharge, col=msr))
ggplot(start) + geom_point(aes(x=date, y=sat_deficit, col=msr))
ggplot(start) + geom_point(aes(x=date, y=unsat_transfer, col=msr))
ggplot(start) + geom_point(aes(x=date, y=rz_storage, col=msr))
ggplot(start) + geom_point(aes(x=date, y=rootzone.field_capacity, col=msr))

# option 1 
ggplot(start) + 
  geom_point(aes(x=date, y=sd_bal, col=msr)) + 
  geom_line(aes(x=date, y=recharge, col=msr), linetype="dashed") + facet_grid("msr") + ggtitle("recharge >")

# option 4 
ggplot(start) + 
  geom_hline(aes(yintercept=0)) + 
  geom_line(aes(x=date, y=sd_bal, col=msr)) + 
  geom_point(aes(x=date, y=sat_deficit-rootzone.potential_sat, col=msr)) + 
  geom_line(aes(x=date, y=recharge, col=msr), linetype="dashed") + facet_grid("msr") + ggtitle("recharge < sat_def - rz_stor-unsat_stor")


no_surf = readr::read_csv("subsurface_msr_test.csv") %>% 
  mutate(surface="no", surface_transfer=0)

ysurf = readr::read_csv("surface_msr_test.csv") %>% mutate(surface="yes")

tmp = rbind(no_surf, ysurf)

insp = tmp %>% dplyr::filter(year==1995 & month<=4 &
                               patchID == 60853701) 

ggplot(insp) + geom_point(aes(x=date, y=rz_drainage, col=surface)) +
  facet_grid(patchID~msr)
