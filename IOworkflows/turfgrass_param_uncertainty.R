
# vegetation turfgrass C3 parameters 
# scenarios testing 
# C3 and C4 - need different header files 
# -- test_msr_tgC4.hdr
# -- test_msr_tgC3.hdr
# 2 patch, 2 stratum in 1 patch (50%)
# -- test_msr_no_irr_2stratum.VegOnly.world
# -- clim file: mission_base / patch_base for irrigation 

setwd("~/Desktop/VegParamsSA/scripts/")
library(RHESSysIOinR)
library(tidyverse)
library(sensitivity)
library(randtoolbox)
library(zoo)
library(patchwork)

# load in turfgrass params 
# tp = read.csv("../defs/tgrass_filtered_parms.csv")

# load in turfgrass params 
# set up new SA with parameter constraints 
parms0 <- list(
  epc.height_to_stem_coef = c(0.2, 1.1), 
  epc.proj_sla = c(18, 27), # bijoor et al 2014 
  epc.max_root_depth = c(0.21, 0.5),
  epc.waring_pa = c(0.4, 1),
  epc.gl_smax = c(0.004, 0.012), # reyes et al. 2017
  epc.day_leafon = c(270, 300),
  epc.ndays_expand =  c(16, 30),
  epc.ndays_litfall =  c(16, 40),
  epc.alloc_prop_day_growth = c(0.5, 0.75),
  epc.froot_cn = c(37, 68), # reyes / calibrated
  epc.leaf_cn = c(40, 54), # reyes / calibrated
  epc.leaf_turnover = c(6, 12)) # ratio not percent 

# accounting for not just leaves but seed 
# higher leaf turnover is all seed 

n=1000
get_parms <- sensitivity::parameterSets(parms0, samples=n, method="sobol")
parm_df <- cbind(data.frame(get_parms), group_id = 1:n)
colnames(parm_df) <- c(names(parms0), "group_id")
saveRDS(parm_df, "../out/test_tgrass50/c3/turfgrass_parmsC3.rds")

# inputs 
rhv = "/Users/rtorres/RHESSys-rt/rhessys/rhessys7.4"
tec= "../tecfiles/tec.spinup"
str = "1989 10 1 1"
end = "1999 10 1 1"
cmd = "-g -vmort_off -of watbal_of.yml -climrepeat -msr"

# what's being changed 
world_tgrass = "../worldfiles/test_msr_irr_2stratum.VegOnly.world"
flowt = "../flowtables/test_msr.2stratum.50.flow"
#hdr = c("test_msr_tgC4.hdr", "test_msr_tgC3.hdr")
hdr = "../worldfiles/test_msr_tgC3.hdr"

# saving here
outdir = "../out/test_tgrass50/c3/"
msr_flag="on"

option_sets_def_par = list(parm_df)
#option_sets_def_par = readRDS("../out/test_tgrass50/c3/filtered_sets.rds")
#option_sets_def_par = list(option_sets_def_par)
names(option_sets_def_par) <- c(def_tg = "../defs/turfgrass-C3.def")

#n = length(unique(option_sets_def_par$`../defs/turfgrass-C3.def`))

# start loop here 
system.time(
  for (bb in seq_along(1:n)){
    
    change_def_file(def_file = names(option_sets_def_par)[1],
                    par_sets = parm_df[bb,],
                    file_name_ext = "")
    
    print(paste("New def file written for file", names(option_sets_def_par)[1]))
    
    print(paste("----------------- Run", bb ,"of", n, "-----------------"))
    
    # spin up the grass 
    rhessys_command(rhessys_version = rhv,
                    world_file = world_tgrass,
                    world_hdr_file = hdr,
                    tec_file = tec,
                    flow_file = flowt,
                    start_date = str,
                    end_date = end,
                    output_file = outdir,
                    input_parameters = "-s 0.0363 359.486 -sv 0.0363 359.486 -gw 0.346 0.416",
                    command_options = cmd) 
    
    
    # save basin output 
    # output <- read.csv("../out/wb_bas.csv")
    # tmp = mkdate(output) %>% 
    #   mutate(canopy_snow_stored = snow_stored,                                           canopy_rain_stored = rain_stored, 
    #          run = bb,
    #          msr = msr_flag) %>%
    #   watbal_basin_of()
    # 
    # if(bb==1){
    #   write.table(as.data.frame(tmp), file=paste(outdir, "param_u_bas.csv", sep=""), append=F, row.names = FALSE)
    # }else{
    #   write.table(as.data.frame(tmp), file=paste(outdir, "param_u_bas.csv", sep=""), append=T, col.names = FALSE, row.names = FALSE)
    # }
    
    # save stratum output 
    output2 <- read.csv("../out/wb_strat.csv")
    
    # fix df 
    tmp = mkdate(output2) %>% 
      dplyr::filter(veg_parm_ID==3) %>% 
      mutate(id = factor(patchID,levels=c(60853701, 60853702), labels = c("shade","sun"))) %>% 
      dplyr::filter(year==1998) # this was a wet year towards end of run
    
    vars = tmp %>% group_by(id) %>% 
      summarise(lai = mean(epv.proj_lai),
                height = mean(epv.height),
                rootdepth = mean(rootzone.depth),
                gpp = sum(cdf.psn_to_cpool),
                npp = sum(npp),
                trans_ann = sum(trans),
                trans_day_max = max(trans),
                psi_ravg = mean(epv.psi_ravg))
    
    t_vars = tmp %>% 
      dplyr::filter(id=="sun") %>% 
      select(gpp = cdf.psn_to_cpool, month, id) %>% 
      dplyr::filter(gpp == max(gpp)) 
    
    # remove duplicates 
    t_vars = t_vars[!duplicated(t_vars$gpp),]
    
    t_vars_df = t_vars %>% 
      inner_join(vars, by="id") %>% 
      mutate(run = bb,
             msr = msr_flag)
    
    # save to output 
    
    if(bb==1){
      write.table(as.data.frame(t_vars_df), file=paste(outdir, "param_u_strat.csv", sep=""), append=F, row.names = FALSE)
    }else{
      write.table(as.data.frame(t_vars_df), file=paste(outdir, "param_u_strat.csv", sep=""), append=T, col.names = FALSE, row.names = FALSE)
    }
    
    if(bb==1){
      write.table(as.data.frame(tmp), file=paste(outdir, "param_u_strat_year.csv", sep=""), append=F, row.names = FALSE)
    }else{
      write.table(as.data.frame(tmp), file=paste(outdir, "param_u_strat_year.csv", sep=""), append=T, col.names = FALSE, row.names = FALSE)
    }
    
  }
)

## had an issue at the end so it stopped at 907
check = read.csv("../out/test_tgrass50/c3/param_u_strat.csv", sep="")
summary(check)
check$group_id = check$run

  
# tmp = read.csv("../out/test_tgrass50/c3/param_u_strat_year.csv", sep="")
# ggplot(tmp) + geom_point(aes(x=yd, y=epv.proj_lai,
#                              col=epv.psi_ravg)) + facet_wrap("id")
# 
# ggplot(tmp) + geom_point(aes(x=yd, y=trans,
#                              col=epv.psi_ravg)) + facet_wrap("id")
# 
# ggplot(tmp) + geom_point(aes(col=month, y=cdf.psn_to_cpool,
#                              x=epv.psi_ravg)) + facet_wrap("id") + 
#   scale_color_viridis_c()

pat = read.csv("../out/wb_p.csv")
rz = ggplot(pat, aes(x=year, y=rz_transfer, col=as.factor(patchID))) + geom_point()
st = ggplot(pat, aes(x=year, y=surface_transfer, col=as.factor(patchID))) + geom_point()
st/rz

## negative gpp 
gpp = check %>% dplyr::filter(gpp.y < 0)



### parameter output 
#####################################################################

# need lower npp 
npp_res = pcc(dplyr::select(parm_df[1:978,],-group_id), check$npp, rank=T)
npp_res$PRCC$names = rownames(npp_res$PRCC)
npp_df = npp_res$PRCC[order(npp_res$PRCC$original),]
ggplot(npp_df) + geom_col(aes(x=original, y=names))

# same for transpiration 
trans_res = pcc(dplyr::select(parm_df[1:978,],-group_id), check$trans_ann, rank=T)
trans_res$PRCC$names = rownames(trans_res$PRCC)
trans_df = trans_res$PRCC[order(trans_res$PRCC$original),]
ggplot(trans_df) + geom_col(aes(x=original, y=names))

# join output with parameters 
joined = inner_join(check, parm_df) %>% 
  dplyr::filter(gpp.y > 0 & npp < 1)

joined %>% pivot_longer(cols=names(parms0)) %>% ggplot(aes(x=value, y=npp)) + geom_point() + facet_wrap("name", scales="free") + geom_smooth(method="lm")

joined %>% pivot_longer(cols=names(parms0)) %>% ggplot(aes(x=value, y=trans_ann)) + geom_point() + facet_wrap("name", scales="free") + geom_smooth(method="lm")

filtered = joined %>% 
  dplyr::filter(month >=4 & month <=6 &
                  lai <2.7 &
                  npp > 0)

nrow(filtered)
# change froot turnover, storage transfer prop, leaf CN, froot CN
summary(filtered)
filt.npp = filtered %>% dplyr::filter(npp < 0.11 & npp>0)
nrow(filt.npp)


filt.npp %>% dplyr::select(-id, -msr) %>% pivot_longer(cols=names(dplyr::select(check,-id, -msr))) %>% ggplot(aes(x=value, y=epc.leaf_dayoff)) + geom_point() + facet_wrap("name", scales="free") + geom_smooth(method="lm")

## save these sets
saveRDS(filt.npp, file=paste(outdir,"filtered_sets.rds"))
