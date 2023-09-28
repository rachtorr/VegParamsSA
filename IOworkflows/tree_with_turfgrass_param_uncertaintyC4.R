
# automatically change worldfile areas (% area and actual amount)
# vary tree params and turfgrass params 

setwd("~/VegParamsSA/scripts/")
library(RHESSysIOinR)
library(tidyverse)
library(sensitivity)
library(randtoolbox)
library(zoo)
source("../R/surface_msr_helper_funcs.R")

n = 500

# load in tree params 
treepu = read.csv("../defs/quag_params.csv") 


# set up tree params 
option_sets_all = sample_n(treepu, n)
p2 <- option_sets_all[,15:48]
p_names <- c(
  "pore_size_index",
  "psi_air_entry",
  "group_id",
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
  "epc.root_distrib_parm",
  "epc.proj_sla",
  "epc.alloc_stemc_leafc",
  "epc.ext_coef",
  "specific_rain_capacity",
  "epc.netpabs_age_mult",
  "epc.cpool_mort_fract",
  "epc.livewood_turnover",
  "epc.waring_pb",
  "epc.min_leaf_carbon",
  "epc.resprout_leaf_carbon",
  "epc.alloc_frootc_leafc",
  "epc.alloc_frootc_crootc",
  "epc.alloc_prop_day_growth",
  "epc.day_leafon",
  "epc.day_leafoff",
  "epc.ndays_expand",
  "epc.ndays_litfall")

colnames(p2) <- p_names 
soils <- p2[,1:3]
veg <- p2[,3:34]

# load in turfgrass params 
# set up new SA with parameter constraints 
parms0 <- list(
  epc.height_to_stem_coef = c(0.18, 1), 
  epc.proj_sla = c(23, 34), # bijoor et al 2014 31.3 for C4
  epc.max_root_depth = c(0.8, 1.51),
  epc.waring_pa = c(0.2, 1),
  epc.gl_smax = c(0.004, 0.012), # reyes et al. 2017
  epc.day_leafoff = c(290, 320),
  epc.day_leafon = c(80, 120),
  epc.ndays_expand =  c(15, 40),
  epc.ndays_litfall =  c(15, 40),
  epc.alloc_prop_day_growth = c(0.2, 0.65),
  epc.storage_transfer_prop = c(0.25, 0.7),
  epc.leaf_turnover = c(6, 12),
  epc.froot_turnover = c(0.3, 1.1))

get_parms <- sensitivity::parameterSets(parms0, samples=n, method="sobol")
parm_df <- cbind(data.frame(get_parms), group_id = 1:n)
colnames(parm_df) <- c(names(parms0), "group_id")
saveRDS(parm_df, "../out/tgrass50/c4/turfgrass_parmsC4.rds")

#option_sets_def_par = list(soils, veg, parm_df)
#names(option_sets_def_par) <- c(def_soil = "../defs/shallow.def", 
#                                def_evr = "../defs/veg_liveoak.def", 
#                                def_tg = "../defs/turfgrass.def")
option_sets_def_par = list(parm_df)
names(option_sets_def_par) <- c(def_tg = "../defs/turfgrass.def")

# inputs 
rhv = "/Users/rtorres/RHESSys-rt/rhessys/rhessys7.4"
world = "../worldfiles/test_msr_no_irr_2stratum.world" 
tec= "../tecfiles/tec.spinup"
str = "1989 10 1 1"
end = "1995 10 1 1"
cmd = "-g -vmort_off -of watbal_of.yml -climrepeat -msr"

# saving here
outdir = "../out/tgrass50/c4/"
msr_flag="on"

# area changes - testing with two patch, two stratum 
pids = c(60853701, 60853702)
pct_areas = list(c(0.5, 0.5),
                 c(0.25, 0.75),
                 c(0.75, 0.25))
pct_areas = list(c(0.5, 0.5))

# start here to run loop - loop 1 is worldfile, then sequence through parameter sets 

system.time(
  for (aa in seq_along(pct_areas)){
  
  change_wf_area(world, 
                 unlist(pct_areas[aa]), 
                 pids)

  for (bb in seq_along(1:n)){
    
    for (cc in seq_along(option_sets_def_par)){
      
        change_def_file(def_file = names(option_sets_def_par)[cc],
                      par_sets = as_tibble(dplyr::select(option_sets_def_par[[cc]], -group_id))[bb,],
                      file_name_ext = "")
      
        print(paste("New def file written for file", names(option_sets_def_par)[cc]))
      
    }
      
      print(paste("----------------- Run", bb ,"of", n, "-----------------"))
  
      rhessys_command(rhessys_version = rhv,
                      world_file = world,
                      world_hdr_file = "../worldfiles/test_msr.hdr",
                      tec_file = tec,
                      flow_file = "../flowtables/test_msr.flow",
                      start_date = str,
                      end_date = end,
                      output_file = "../out/msr_tests",
                      input_parameters = "-s 0.0363 359.486 -sv 0.0363 359.486 -gw 0.346 0.416",
                      command_options = cmd)
      
      # save basin output 
      # output <- read.csv("../out/wb_bas.csv")
      # tmp = mkdate(output) %>% 
      #   mutate(canopy_snow_stored = snow_stored,                                           canopy_rain_stored = rain_stored, 
      #          run = bb,
      #          msr = msr_flag,
      #          veg_area = unlist(pct_areas[aa])[1]) %>%
      #   watbal_basin_of()
      # 
      # if(bb==1 & aa==1){
      #   write.table(as.data.frame(tmp), file=paste(outdir, "param_u_bas.csv", sep=""), append=F, row.names = FALSE)
      # }else{
      #   write.table(as.data.frame(tmp), file=paste(outdir, "param_u_bas.csv", sep=""), append=T, col.names = FALSE, row.names = FALSE)
      # }
      
      # aggregate and save stratum output 
      output2 <- read.csv("../out/wb_strat.csv")
      tmp = mkdate(output2) %>% 
        mutate(run = bb,
               msr = msr_flag,
               type = "c4",
               veg_area = unlist(pct_areas[aa])[1]) %>% 
        dplyr::filter(year > 1992) 
        
      # group_by(month, year, run, msr, type, veg_area, veg_parm_ID) %>% 
      #   summarise(lai = mean(epv.proj_lai),
      #             height = mean(epv.height),
      #             rz_depth = mean(rootzone.depth),
      #             psn = mean(cdf.psn_to_cpool),
      #             trans = mean(trans))
      
      if(bb==1 & aa==1){
        write.table(as.data.frame(tmp), file=paste(outdir, "param_u_strat.csv", sep=""), append=F, row.names = FALSE)
      }else{
        write.table(as.data.frame(tmp), file=paste(outdir, "param_u_strat.csv", sep=""), append=T, col.names = FALSE, row.names = FALSE)}
      
      # save patch output 
      # output3 <- read.csv("../out/wb_p.csv")
      # tmp = mkdate(output3) %>% 
      #   mutate(run = bb,
      #          msr = msr_flag,
      #          veg_area = unlist(pct_areas[aa])[1])
      # 
      # if(bb==1 & aa==1){
      #   write.table(as.data.frame(tmp), file=paste(outdir, "param_u_p.csv", sep=""), append=F, row.names = FALSE)
      # }else{
      #   write.table(as.data.frame(tmp), file=paste(outdir, "param_u_p.csv", sep=""), append=T, col.names = FALSE, row.names = FALSE)}
  
    }
  }
)


# make sure to change the days of litfall and growth 

#####################################
#----check parameter sensitivity ####
#####################################

parm_strat = readr::read_table("../mod_data/c4/param_u_strat.csv") 

# plot time series 
parm_strat %>% dplyr::filter(parm_strat$`"veg_parm_ID"`==3) %>% 
  mutate(yrmo = as.yearmon(paste(`"year"`,`"month"`, sep="-"))) %>% 
  group_by(yrmo) %>% 
  summarise(lai = mean(`"epv.proj_lai"`)) %>% 
  ggplot() + geom_line(aes(x=yrmo, y=lai))

parm_strat %>% dplyr::filter(parm_strat$`"veg_parm_ID"`==3) %>% 
  group_by(`"month"`) %>% 
  summarise(lai = mean(`"epv.proj_lai"`)) %>% 
  ggplot() + geom_line(aes(x=`"month"`, y=lai))

parm_strat %>% dplyr::filter(parm_strat$`"veg_parm_ID"`==3) %>% 
  group_by(`"yd"`) %>% 
  summarise(lai = mean(`"epv.proj_lai"`)) %>% 
  ggplot() + geom_line(aes(x=`"yd"`, y=lai))

parm_strat %>% dplyr::filter(parm_strat$`"veg_parm_ID"`==3) %>% 
  group_by(`"yd"`) %>% 
  summarise(lai = mean(`"cdf.psn_to_cpool"`)) %>% 
  ggplot() + geom_line(aes(x=`"yd"`, y=lai))

# get month when LAI is the highest and the lowest 
parm_veg_state = parm_strat %>% 
  dplyr::filter(`"veg_parm_ID"`==3) %>%
  mutate(yrmo = as.yearmon(paste(`"year"`,`"month"`, sep="-"))) %>% 
  rename(lai = `"epv.proj_lai"`, month = `"month"`, run=`"run"`, year=`"year"`) %>% 
  group_by(run, year) %>% 
  reframe(lai_mo = month[lai==max(lai)]) %>%
  group_by(run) %>% 
  summarise(month_lai = mean(lai_mo)) %>% 
  rename(group_id = run) %>%
  inner_join(parm_df, by="group_id") %>% 
  pivot_longer(cols=names(parms0)) %>% 
  mutate(month_psn = round(month_lai))

ggplot(parm_veg_state) + geom_boxplot(aes(x=as.factor(month_psn), y=value, group=month_psn)) + 
  facet_wrap('name', scales='free') + 
  coord_flip()

# get month when PSN is the highest and the lowest 
parm_veg_state = parm_strat %>% 
  dplyr::filter(`"veg_parm_ID"`==3) %>%
  mutate(yrmo = as.yearmon(paste(`"year"`,`"month"`, sep="-"))) %>% 
  rename(lai = `"epv.proj_lai"`, month = `"month"`, run=`"run"`, year=`"year"`) %>% 
  group_by(run, year) %>% 
  reframe(lai_mo = month[lai==max(lai)]) %>%
  group_by(run) %>% 
  summarise(month_lai = mean(lai_mo)) %>% 
  rename(group_id = run) %>%
  inner_join(parm_df, by="group_id") %>% 
  pivot_longer(cols=names(parms0)) %>% 
  mutate(month_psn = round(month_lai))

ggplot(parm_veg_state) + geom_boxplot(aes(x=as.factor(month_psn), y=value, group=month_psn)) + 
  facet_wrap('name', scales='free') + 
  coord_flip()

# filter by month 
parm.mo_filt_c4 = parm_veg_state %>% 
  dplyr::filter(month_lai >= 6 & month_lai <=8)
length(unique(parm.mo_filt_c4$group_id))


# to plot all parameters v. output                              
parm_veg_out = parm_strat %>% 
  dplyr::filter(`"veg_parm_ID"`==3) %>%
  group_by(`"run"`) %>% 
  summarise(height = mean(`"epv.height"`),
            rz_depth = mean(`"rootzone.depth"`),
            psn = mean(`"cdf.psn_to_cpool"`),
            trans = mean(`"trans"`), 
            lai_mn = mean(`"epv.proj_lai"`),
            lai_mx = max(`"epv.proj_lai"`)) %>% 
  rename(group_id = `"run"`) %>%
  inner_join(parm_df, by="group_id") %>% 
  pivot_longer(cols=names(parms0))

parm_veg_out.filt = parm_veg_out[parm_veg_out$group_id %in%
                                   parm.mo_filt_c4$group_id,]
# get summary of parm_veg_out.filt params and rerun 

# lai and all parameters
ggplot(parm_veg_out.filt) + geom_point(aes(x=value, y=lai_mn)) + facet_wrap('name', scales="free") +
  geom_smooth(aes(x=value, y=lai_mn))

parm_veg_out.filt %>% 
  ggplot() + geom_point(aes(x=value, y=rz_depth)) + facet_wrap('name', scales="free") +
  geom_smooth(aes(x=value, y=rz_depth))

parm_veg_out.filt %>% 
  ggplot() + geom_point(aes(x=value, y=height)) + facet_wrap('name', scales="free") +
  geom_smooth(aes(x=value, y=height))


parm_veg_out.filt_ht = parm_veg_out.filt %>% 
  dplyr::filter(height >= 0.025 & height< 0.11) 

length(unique(parm_veg_out.filt_ht$group_id))

saveRDS(parm_veg_out.filt_ht, paste(outdir, "c4_output_parm.rds", sep=""))

ps = parm_strat[parm_strat$`"run"` %in% parm_veg_out.filt_ht$group_id,] %>%
  dplyr::filter(`"veg_parm_ID"`==3)
saveRDS(ps, paste(outdir, "filtered_output.rds", sep=""))

##### below is old 

# lai and epc.storage_transfer_prop 
ggplot(parm_veg_state[parm_veg_state$name=="epc.storage_transfer_prop",]) + 
  geom_point(aes(x=value, y=lai, col=rz_depth)) + 
  scale_color_viridis_c()

ggplot(parm_veg_state[parm_veg_state$name=="epc.storage_transfer_prop",]) + 
  geom_point(aes(x=value, y=lai, col=height)) + 
  scale_color_viridis_c()

# range for epc.storage_transfer_prop = [0.25, 0.7]
library(segmented)
seg = parm_veg_state[parm_veg_state$name=="epc.storage_transfer_prop",]
seg.lm = lm(data=seg, lai~value)
getbp = segmented(seg.lm, seg.Z=~value, psi=0.6)
getbp$psi

ggplot(parm_veg_state[parm_veg_state$name=="epc.storage_transfer_prop",]) + 
  geom_point(aes(x=value, y=lai, col=height)) + 
  scale_color_viridis_c() +
  geom_vline(aes(xintercept=getbp$psi[,2]),linetype='dashed')

# height and all params
ggplot(parm_veg_state) + geom_point(aes(x=value, y=height)) + facet_wrap('name', scales="free")

ggplot(parm_veg_state[parm_veg_state$name=="epc.height_to_stem_coef",]) + 
  geom_point(aes(x=value, y=height, col=height)) + 
  scale_color_viridis_c() +
  geom_smooth(aes(x=value, y=height), method='lm')

getrange = parm_veg_state[parm_veg_state$name=="epc.height_to_stem_coef",]
getrange.lm = lm(data=getrange, height~value)
predict.lm(object=getrange.lm, newdata=data.frame(value=0.18))
# height range [0.18, 1]

ggplot(parm_veg_state[parm_veg_state$name=="epc.proj_sla",]) + 
  geom_point(aes(x=value, y=height, col=height)) + 
  scale_color_viridis_c() +
  geom_smooth(aes(x=value, y=height), method='lm')

ht_filt = parm_veg_state %>% 
  dplyr::filter(height > 2)
# proj sla = [11.6, 34]


# rootzone depth and all params
ggplot(parm_veg_state) + geom_point(aes(x=value, y=rz_depth)) + facet_wrap('name', scales="free")

rz_filt = parm_veg_state %>% dplyr::filter(name=="epc.max_root_depth") %>% dplyr::filter(rz_depth >=20.3 & rz_depth <= 152.4)

ggplot(rz_filt) + geom_point(aes(x=value, y=rz_depth, col=height))
summary(rz_filt$value)
# epc.max_root_depth [0.21, 1.51]



#### check fluxs - transpiration and growth rates 
parm_veg_flux = parm_strat %>% 
  dplyr::filter(`"veg_parm_ID"`==3 & 
                  `"year"` >= 2000 & `"year"` < 2012) %>%
  group_by(`"run"`, `"year"`) %>% 
  summarise(trans_yr = sum(`"trans"`)*1000,
            trans_day = max(`"trans"`)*1000,
            et = sum(`"et"`)*1000,
            npp_day = mean(`"npp"`)*1000,
            npp_yr = sum(`"npp"`)*1000,
            psn_day = mean(`"cdf.psn_to_cpool"`)*1000) %>% 
  group_by(`"run"`) %>% 
  summarise(trans_yr = mean(trans_yr),
            trans_day = mean(trans_day),
            et = mean(et),
            npp_day = mean(npp_day),
            npp_yr = mean(npp_yr),
            psn_day = mean(psn_day)) %>% 
  rename(group_id = `"run"`) %>%
  inner_join(parm_df, by="group_id") %>% 
  pivot_longer(cols=names(parms0))

ggplot(parm_veg_flux) + geom_point(aes(x=value, y=trans_day)) +
  facet_wrap('name', scales='free') + 
  geom_smooth(aes(x=value, y=trans_day), method='lm')

ggplot(parm_veg_flux) + geom_point(aes(x=value, y=trans_yr)) +
  facet_wrap('name', scales='free') + 
  geom_smooth(aes(x=value, y=trans_yr), method='lm')

ggplot(parm_veg_flux) + geom_point(aes(x=value, y=et)) +
  facet_wrap('name', scales='free') + 
  geom_smooth(aes(x=value, y=et), method='lm')

ggplot(parm_veg_flux) + geom_point(aes(x=value, y=npp_yr)) +
  facet_wrap('name', scales='free') + 
  geom_smooth(aes(x=value, y=npp_yr), method='lm')

parm_veg_flux %>% dplyr::filter(npp_day>0) %>% 
  ggplot() + geom_point(aes(x=value, y=npp_day)) +
  facet_wrap('name', scales='free') + 
  geom_smooth(aes(x=value, y=npp_day), method='lm')

parm_veg_flux %>% dplyr::filter(npp_day>0) %>% 
  ggplot() + geom_point(aes(x=value, y=psn_day)) +
  facet_wrap('name', scales='free') + 
  geom_smooth(aes(x=value, y=psn_day), method='lm')

## seasonal patterns 

parm_veg_szn = parm_strat %>% 
  dplyr::filter(`"veg_parm_ID"`==3 & 
                  `"year"` >= 2000 & `"year"` < 2012) %>%
  group_by(`"run"`, `"yd"`) %>%
  summarise(trans = mean(`"trans"`)*1000,
            psn = mean(`"cdf.psn_to_cpool"`)*1000,
            lai = mean(`"epv.proj_lai"`)) %>% 
  rename(group_id = `"run"`,
         yd = `"yd"`) %>%
  inner_join(parm_df, by="group_id") %>% 
  pivot_longer(cols=names(parms0))

ggplot(parm_veg_szn) + geom_point(aes(x=yd, y=lai, col=value)) +
  facet_wrap('name', scales="free")

parm_veg_szn %>% group_by(yd) %>% 
  summarise(lai = mean(lai)) %>% 
  ggplot() + geom_point(aes(x=yd, y=lai))
 