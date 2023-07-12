library(tidyverse)
library(RHESSysIOinR)
setwd("~/VegParamsSA/out")
source("/Volumes/GoogleDrive/My Drive/help_with_rhessys/watbal.R")

# output from regular old way 
out = readin_rhessys_output("testing")

water = watbal(out$bd)

water %>% dplyr::filter(year<1960) %>% ggplot() + geom_point(aes(x=date, y=watbal))

# output from output filters
wbal = read.csv("wb_bas.csv") %>% mkdate()

wbal %>% dplyr::filter(year<1960) %>% ggplot() + geom_point(aes(x=date, y=water_balance)) +
  geom_line(aes(x=date, y=rz_storage)) + 
  geom_line(aes(x=date, y=water_balance + rz_storage, col="both"))

tmp=diff(wbal$rz_storage)
tmp=c(0,tmp)
wbal$rzdiff = tmp

wbal %>% dplyr::filter(year<1960) %>% ggplot() + geom_point(aes(x=date, y=water_balance)) +
  geom_line(aes(x=date, y=rz_storage)) + 
  geom_line(aes(x=date, y=rzdiff, col="diff")) + 
  geom_line(aes(x=date, y=detention_store, col="sd")) + 
  geom_line(aes(x=date, y=water_balance - rzdiff + rz_storage, col="sum"))

wbal$sd=with(wbal,sat_deficit-rz_storage-unsat_storage)
tmp=diff(wbal$sd)
tmp=c(0,tmp)
wbal$sddiff=tmp  

wbal %>% dplyr::filter(year<1960) %>% ggplot() + geom_point(aes(x=date, y=water_balance)) +
  geom_line(aes(x=date, y=rz_storage)) + 
  geom_line(aes(x=date, y=sddiff, col="diff")) + 
  geom_line(aes(x=date, y=water_balance + sddiff + rz_storage, col="sum"))
               