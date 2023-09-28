
# automatically change worldfile areas (% area and actual amount)
# automatically change worldfile veg def file in second patch 
# or use different worldfile for each i.e. make a loop that goes through all of them as a list - within that loop change areas
# read in the output, add IDs for area and veg, then save to separate file - check irrigation ch2 code for how you did this with adding all to same file

setwd("~/VegParamsSA/scripts/")
library(RHESSysIOinR)
library(tidyverse)
library(sensitivity)
library(randtoolbox)


# load in turfgrass params 
tp = read.csv("../defs/tgrass_filtered_parms.csv")
tp_adj = tp[,3:13]

parm_df = tp_adj %>% dplyr::select(-run)
n = nrow(tp_adj)

# set up new SA with parameter constraints 
n = 200
parms0 <- list(
  epc.height_to_stem_coef = c(0.1, 0.3), 
  epc.proj_sla = c(11.5, 36), # bijoor et al 2014
  epc.max_root_depth = c(0.07, 1.52),
  epc.storage_transfer_prop = c(0.25, 0.4),
  epc.waring_pa = c(0.3, 0.5),
  epc.gl_smax = c(0.004, 0.012), # reyes et al. 2017
  epc.day_leafoff = c(90, 151),
  epc.day_leafon = c(244, 305),
  epc.ndays_expand =  c(14, 60),
  epc.ndays_litfall =  c(14, 60),
  epc.alloc_prop_day_growth = c(0.5, 0.7))

get_parms <- sensitivity::parameterSets(parms0, samples=200, method="sobol")

parm_df <- data.frame(get_parms)
colnames(parm_df) <- names(parms0)


# inputs 
rhv = "/Users/rtorres/RHESSys-rt/rhessys/rhessys7.4"
world = "../worldfiles/test_msr_no_irr_tgrass-only.world" 
tec= "../tecfiles/tec.spinup"
str = "1989 10 1 1"
end = "1999 10 1 1"
cmd = "-g -vmort_off -of watbal_of.yml -v -6 -climrepeat"

# saving here
outdir = "../out/tgrass50/"
msr_flag="off"

for (bb in seq_along(1:n)){
  
    change_def_file(def_file = "../defs/turfgrass.def",
                  par_sets = parm_df[bb,],
                  file_name_ext = "")
  
    print(paste("New def file written for file ../defs/turfgrass/turfgrass_.def"))
  
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
    output <- read.csv("../out/wb_bas.csv")
    tmp = mkdate(output) %>% 
      mutate(canopy_snow_stored = snow_stored,                                           canopy_rain_stored = rain_stored, 
             run = bb,
             msr = msr_flag) %>%
      watbal_basin_of()
    
    if(bb==1){
      write.table(as.data.frame(tmp), file=paste(outdir, "param_u_bas.csv", sep=""), append=F, row.names = FALSE)
    }else{
      write.table(as.data.frame(tmp), file=paste(outdir, "param_u_bas.csv", sep=""), append=T, col.names = FALSE, row.names = FALSE)
    }
    
    # save stratum output 
    output2 <- read.csv("../out/wb_strat.csv")
    tmp = mkdate(output2) %>% 
      mutate(run = bb,
             msr = msr_flag)
    
    if(bb==1){
      write.table(as.data.frame(tmp), file=paste(outdir, "param_u_strat.csv", sep=""), append=F, row.names = FALSE)
    }else{
      write.table(as.data.frame(tmp), file=paste(outdir, "param_u_strat.csv", sep=""), append=T, col.names = FALSE, row.names = FALSE)}
    
    # save patch output 
    output3 <- read.csv("../out/wb_p.csv")
    tmp = mkdate(output3) %>% 
      mutate(run = bb,
             msr = msr_flag)
    
    if(bb==1){
      write.table(as.data.frame(tmp), file=paste(outdir, "param_u_p.csv", sep=""), append=F, row.names = FALSE)
    }else{
      write.table(as.data.frame(tmp), file=paste(outdir, "param_u_p.csv", sep=""), append=T, col.names = FALSE, row.names = FALSE)}
  

}


