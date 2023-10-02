
# scenarios testing 
# C3 and C4 - need different header files 
# -- test_msr_tgC4.hdr
# -- test_msr_tgC3.hdr
# 2 patch / 2 stratum / 50% pavement 
# -- test_msr_no_irr_2stratum.world
# 2 patch / 1 stratum / 50% pavement 
# -- test_msr_no_irr_tgrass-only.world
# 2 patch / 2 stratum / 50% pavement / irrigation - clim file needs to change, also basestation worldfiles - this is not set up here - need irrigation in rhessys set up
# -- test_msr_2stratum.irr.world
# -- clim file: patch_base 
# 2 patch / 1 stratum / 50% pavement / irrigation - clim file 
# 3 patch / 2 stratum / 25% pavement 
# -- test_msr_no_irr_3fam.world (need to change areas)

setwd("~/VegParamsSA/scripts/")
library(RHESSysIOinR)
library(tidyverse)
library(sensitivity)
library(randtoolbox)
library(zoo)

# load in turfgrass params 
tp = read.csv("../defs/tgrass_filtered_parms.csv")

# inputs 
rhv = "/Users/rtorres/RHESSys-rt/rhessys/rhessys7.4"
tec= "../tecfiles/tec.spinup"
str = "1989 10 1 1"
end = "1999 10 1 1"
cmd = "-g -vmort_off -of watbal_of.yml -v -6 -climrepeat"

# what's being changed 
world = c("../worldfiles/test_msr_no_irr_2stratum.world",
          "test_msr_no_irr_tgrass-only.world")
hdr = c("test_msr_tgC4.hdr", "test_msr_tgC3.hdr")


# saving here
outdir = "../out/tgrass50/"
msr_flag="off"

for (bb in seq_along(1:n)){
  
    change_def_file(def_file = "../defs/turfgrass.def",
                  par_sets = parm_df[bb,],
                  file_name_ext = "")
  
    print(paste("New def file written for file", def_file[bb]))
  
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


