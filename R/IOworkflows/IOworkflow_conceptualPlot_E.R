# load packages and set directory
library(RHESSysIOinR)
library(tidyverse)
library(sensitivity)
source("~/RHESSysIOinR-master/R/select_output_variables_R.R")
setwd("~/Documents/patches/R/tree_outputs/")

# read in the parameters 
evr1 <- read.csv("evg_broadleaf_params.csv") %>% dplyr::select(-X)
evr2 <- read.csv("pica_conifer_params.csv") %>% dplyr::select(-X)
evr <- rbind(evr1, evr2)

# select which variables to look at from output
output_variables <- data.frame(out_file=character(), variable=character(), stringsAsFactors=FALSE)
output_variables[1,] <- data.frame("bd", "lai", stringsAsFactors=FALSE)
output_variables[2,] <- data.frame("bd", "height", stringsAsFactors = FALSE)
output_variables[3,] <- data.frame("bd", "plantc", stringsAsFactors = FALSE)
output_variables[4,] <- data.frame("bd", "precip", stringsAsFactors = FALSE)
output_variables[5,] <- data.frame("bd", "gpsn", stringsAsFactors = FALSE)
output_variables[6,] <- data.frame("bdg", "plant_resp", stringsAsFactors = FALSE)
output_variables[7,] <- data.frame("cdg", "leafc", stringsAsFactors = FALSE)
output_variables[8,] <- data.frame("bd", "evap_can", stringsAsFactors = FALSE)
output_variables[9,] <- data.frame("bd", "evap", stringsAsFactors = FALSE)
output_variables[10,] <- data.frame("bd", "trans", stringsAsFactors = FALSE)
output_variables[11,] <- data.frame("bd", "psn", stringsAsFactors = FALSE)
output_variables[12,] <- data.frame("bd", "resp", stringsAsFactors = FALSE)


#########################################################################
# make tec file - neeed to add part from generate_input_files
#########################################################################
## ***Stop - only do this if needed***
#input_tec_data <- NULL
input_tec_data <- data.frame(year=integer(),month=integer(),day=integer(),hour=integer(),name=character(),stringsAsFactors=FALSE)
input_tec_data[1,] <- data.frame(1957, 10, 1, 1, "print_daily_on", stringsAsFactors=FALSE)
input_tec_data[2,] <- data.frame(1957, 10, 1, 2, "print_daily_growth_on", stringsAsFactors=FALSE)

if (is.null(input_tec_data) == FALSE){
  make_tec_file(tec_file = input_rhessys$tec_file, tec_data = input_tec_data)
  print(paste("Tec file has been written"))
}



setwd("../../scripts/")

# for changing output location and option sets 
spec = c("quag","piun","pica")
# for changing worldfiles and corresponding def files
type = c("Oak","Oak","Oak")
type = c("Con","Con","Con")

spec = "quag"
type="Oak"



for(i in seq_along(spec)){

#########################################################################
  ### set up option_sets_def_par 
  option_sets_all = sample_n(evr[evr$code==spec[i],], 50)
  option_sets_all$output_folder = "../out/rcps"
  option_sets_all$future_hdr_prefix = type[i]
  option_sets_all$world_hdr_prefix = paste("../worldfiles/", type[i],"Patch.hdr", sep="")

  ### which def file is being changed
  def_evr = tolower(paste("../defs/veg_live", type[i],".def", sep=""))
  def_soil = "../defs/shallow_NT.def"
  
  ### convert back to option_sets_def_par so that def file param names are listed as column headers 
  p2 <- option_sets_all[,14:53]
  
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
    "epc.day_leafon",
    "epc.day_leafoff",
    "epc.ndays_expand",
    "epc.ndays_litfall")
  
  colnames(p2) <- p_names 
  soils <- p2[,1:3]
  veg <- p2[,3:40]
  option_sets_def_par = list(soils, veg)
  names(option_sets_def_par) <- c(def_soil, def_evr)
  #########################################################################
  
  
# this first generates a new def file based on parameters from list
# then runs rhessys for however much time is set
# then takes output and puts into new file in directory 'allsim'
# each time params change, the def file is written over, not saved. Each run will have an output that should correspond with ID params in row of csv
  
# start here to run
if(nrow(option_sets_def_par[[1]])==nrow(option_sets_all)){

  option_sets_rhessys_rows <- nrow(option_sets_all)
  
  system.time(
# Generate new def files for each run
# aa = def file being changed, in this case it's only 1
# bb is ID, so also applies to what run #
   # for (aa in seq_along(option_sets_def_par)){

      # Step through each unique parameter set and make def file
     
    for (bb in seq_along(option_sets_def_par[[1]]$group_id)){
      
        for(aa in seq_along(option_sets_def_par)){
          change_def_file(def_file = names(option_sets_def_par)[aa],
                          par_sets = as_tibble(dplyr::select(option_sets_def_par[[aa]], -group_id))[bb,],
                          file_name_ext = "")
          
          print(paste("New def file written for file", names(option_sets_def_par)[aa]))
        }

        print(paste("----------------- Run", bb ,"of", option_sets_rhessys_rows, "-----------------"))

        # Call RHESSys for spinup
        rhessys_command(rhessys_version = "~/RHESSys-develop/bin/rhessys7.2",
                        world_file = option_sets_all$world_file[bb],
                        world_hdr_file = "../worldfiles/OakPatch.spin.hdr",
                        tec_file = "../tecfiles/tec.spinup",
                        flow_file = option_sets_all$flow_file[bb],
                        start_date = "1940 10 1 1",
                        end_date = "2010 10 2 2",
                        output_file = paste(option_sets_all$output_folder[bb],"/", option_sets_all$code[bb], sep=""),
                        input_parameters = "-s 0.036294 359.485800 -sv 0.036294 359.485800 -gw 0.346205 0.416299",
                        command_options = "-b -g -c -p -climrepeat -vmort_off")
        
        if (is.null(output_variables[1]) == F){
          select_output_variables_R(output_variables = output_variables,
                                    output_folder =  paste("../out/rcps"),
                                    output_filename = option_sets_all$code[bb],
                                    run = bb,
                                    max_run = 50
          )
        }

    #}
#     # }
#   )
# }
# }


#############################################################
        ## historical drought first 
#############################################################
        
        cmd = sprintf("mv ../worldfiles/OakPatch.world.0.Y2010M9D30H1.state ../worldfiles/OakWorld.spun70")
      system(cmd)

      print(paste("Spin up complete"))

      # for run with output
      rhessys_command(rhessys_version = option_sets_all$rhessys_version[bb],
                      world_file = "../worldfiles/OakWorld.spun70",
                      world_hdr_file = option_sets_all$world_hdr_prefix[bb],
                      tec_file = "../tecfiles/tec.coast",
                      flow_file = option_sets_all$flow_file[bb],
                      start_date = "2010 10 1 1",
                      end_date = "2020 10 1 1",
                      output_file = paste("../out/plot/hist",option_sets_all$code[bb],option_sets_all$code[bb], sep="/"),
                      input_parameters = "-s 0.036294 359.485800 -sv 0.036294 359.485800 -gw 0.346205 0.416299",
                      command_options = "-b -g -c -climrepeat -vmort_off")


        # Process RHESSys output
          if (is.null(output_variables[1]) == F){
            select_output_variables_R(output_variables = output_variables,
                                      output_folder =  paste("../out/plot/hist", option_sets_all$code[bb], sep="/"),
                                      output_filename = option_sets_all$code[bb],
                                      run = bb,
                                      max_run = 50
            )
}


      #############################################################
      ## 2 deg hotter  
      #############################################################
      
      rhessys_command(rhessys_version = option_sets_all$rhessys_version[bb],
                      world_file = "../worldfiles/OakWorld.spun70",
                      world_hdr_file = "../worldfiles/OakPatch.hot",
                      tec_file = "../tecfiles/tec.coast",
                      flow_file = option_sets_all$flow_file[bb],
                      start_date = "2010 10 1 1",
                      end_date = "2020 10 1 1",
                      output_file = paste("../out/plot/hot",option_sets_all$code[bb], option_sets_all$code[bb], sep="/"),
                      input_parameters = "-s 0.036294 359.485800 -sv 0.036294 359.485800 -gw 0.346205 0.416299",
                      command_options = "-b -g -c -climrepeat -vmort_off")
      
      
      # Process RHESSys output
      if (is.null(output_variables[1]) == F){
        select_output_variables_R(output_variables = output_variables,
                                  output_folder = paste("../out/plot/hot", option_sets_all$code[bb], sep="/"),
                                  output_filename = option_sets_all$code[bb],
                                  run = bb,
                                  max_run = 50
        )
      }
      #############################################################
      ## above 4 deg warmer 
      #############################################################
      
      rhessys_command(rhessys_version = option_sets_all$rhessys_version[bb],
                      world_file = "../worldfiles/OakWorld.spun70",
                      world_hdr_file = "../worldfiles/OakPatch.hotter",
                      tec_file = "../tecfiles/tec.coast",
                      flow_file = option_sets_all$flow_file[bb],
                      start_date = "2010 10 1 1",
                      end_date = "2020 10 1 1",
                      output_file = paste("../out/plot/hotter",option_sets_all$code[bb], option_sets_all$code[bb], sep="/"),
                      input_parameters = "-s 0.036294 359.485800 -sv 0.036294 359.485800 -gw 0.346205 0.416299",
                      command_options = "-b -g -c -climrepeat -vmort_off")
      
      
      # Process RHESSys output
      if (is.null(output_variables[1]) == F){
        select_output_variables_R(output_variables = output_variables,
                                  output_folder =  paste("../out/plot/hotter",option_sets_all$code[bb], sep="/"),
                                  output_filename = option_sets_all$code[bb],
                                  run = bb,
                                  max_run = 50
        )
      }
      
      #############################################################
      ## slightly cooler 
      #############################################################
      
      rhessys_command(rhessys_version = option_sets_all$rhessys_version[bb],
                      world_file = "../worldfiles/OakWorld.spun70",
                      world_hdr_file = "../worldfiles/OakPatch.lower",
                      tec_file = "../tecfiles/tec.coast",
                      flow_file = option_sets_all$flow_file[bb],
                      start_date = "2010 10 1 1",
                      end_date = "2020 10 1 1",
                      output_file = paste("../out/plot/lower",option_sets_all$code[bb], option_sets_all$code[bb], sep="/"),
                      input_parameters = "-s 0.036294 359.485800 -sv 0.036294 359.485800 -gw 0.346205 0.416299",
                      command_options = "-b -g -c -climrepeat -vmort_off")
      
      
      # Process RHESSys output
      if (is.null(output_variables[1]) == F){
        select_output_variables_R(output_variables = output_variables,
                                  output_folder =  paste("../out/plot/lower", option_sets_all$code[bb], sep="/"),
                                  output_filename = option_sets_all$code[bb],
                                  run = bb,
                                  max_run = 50
        )
      }
      
    
      }
    )
  }
}




setwd("../out/plot")
vars = c("lai","plantc","height", "precip","gpsn","plant_resp","leafc", "evap_can","evap","trans","psn", "resp")
hist <- readin_selected_vars(dir="hist/allsim",vars=vars)
hist$scenario = "historic drought"
hot <- readin_selected_vars(dir = "hot/allsim", vars=vars)
hot$scenario = "2 deg. warmer"  
irr <- readin_selected_vars(dir = "irr/allsim", vars=vars)
irr$scenario = "historic with irrigation"
allscen <- rbind(hist, hot, irr)
allscen <- mkwy(allscen)
allscen$et <- allscen$evap_can + allscen$evap + allscen$trans
allscen$npp = allscen$gpsn - allscen$resp

allscen %>% group_by(wy, scenario) %>% 
  summarize_at(vars(et, npp), funs(sum)) %>% 
  ggplot() + geom_line(aes(x=as.factor(wy), y=npp, group=scenario, col=scenario))

plantc <- allscen %>% dplyr::filter(year<2019) %>% 
  group_by(wy, scenario) %>% 
  summarize_at(vars(lai, plantc), funs(mean)) %>% 
 ggplot() +
  geom_line(aes(x=as.factor(wy), y=plantc, group=scenario, col=scenario)) + 
  ggtitle("Carbon [kgC/m^2]") + xlab("") + ylab("plant carbon") + theme(text = element_text(size=10), plot.title = element_text(size = 11))

rain <- ggplot() + 
  geom_col(data=precip_wy, aes(x=as.factor(wy), y=precip)) + geom_hline(aes(yintercept=450, col='avg. annual rain')) + theme(legend.position="none", text = element_text(size=10), plot.title = element_text(size = 11)) +
  # geom_line(data=allscenet, aes(x=as.factor(wy), y=et, col=scenario, group=scenario)) 
  theme_bw() + xlab("water year") + ylab("precipitation [mm]")  + labs(col="")

allscenet <- allscen %>% group_by(wy, scenario, run) %>% 
  summarize_at(vars(et), funs(sum)) %>% 
  group_by(wy, scenario) %>% 
  summarize_at(vars(et), funs(mean))

allscen %>% group_by(wy, scenario) %>% 
  summarize_at(vars(lai, plantc, plant_resp), funs(mean)) %>% 
  ggplot() + geom_line(aes(x=as.factor(wy), y=plant_resp, group=scenario, col=scenario))


allscen %>% group_by(wy, scenario) %>% 
  summarize_at(vars(lai, plant_resp, gpsn), funs(mean)) %>% 
  ggplot() + geom_line(aes(x=as.factor(wy), y=gpsn-plant_resp, group=scenario, col=scenario))

allscen %>% group_by(wy, scenario) %>% 
  summarize_at(vars(lai, leafc), funs(mean)) %>% 
  ggplot() + geom_line(aes(x=as.factor(wy), y=leafc, group=scenario, col=scenario))


allscen %>% dplyr::filter(wy<2018) %>% 
  group_by(wy, scenario) %>% 
  summarize_at(vars(precip, et, trans), funs(sum)) %>% 
  ggplot() + 
  geom_col(aes(x=as.factor(wy), y=precip/3000)) + 
  geom_line(aes(x=as.factor(wy), y=trans/1000, group=scenario, col=scenario)) 

precip = mkwy(hist) %>% dplyr::filter(run==1) %>% dplyr::select(precip, date, year, month, day, wy, wymo)
precip_wy <- precip %>% group_by(wy) %>% summarize(precip=sum(precip)) %>% dplyr::filter(wy < 2019)


allscen %>% dplyr::filter(wy<2018) %>% 
  ggplot() + 
  geom_col(data=precip, aes(x=date, y=precip)) + 
  geom_line(aes(x=date, y=trans, group=scenario, col=scenario)) 

allscen %>% dplyr::filter(wy<2019) %>% 
  group_by(wy, scenario) %>% 
  summarize_at(vars(lai, plantc, gpsn, resp), funs(mean)) %>% 
  ggplot() + 
  #geom_col(data=precip_wy, aes(x=as.factor(wy), y=precip/100)) +
  geom_line(aes(x=as.factor(wy), y=gpsn-resp, group=scenario, col=scenario)) 
  
  


ggplot() + geom_col(data=precip_wy, aes(x=wy, y=precip))

### resilience 
ex  = allscen
res <- ex %>% group_by(wy, run, scenario) %>% 
  summarize_at(vars(lai, gpsn), funs(mean)) 
res$resist = res$lai/res$lai[res$wy==2011] 

ggplot(res) + geom_line(aes(x=wy, y=resist, col=as.factor(run)), show.legend=F) + facet_wrap('scenario')
  ggtitle("resistance = (avg. LAI in yr)/(avg 2011 LAI)")

ex1 = ex[ex$scenario=="irr",]
lai_ind <- relativeresilience(df=ex1, 
                              var='lai',
                              preyr=2011)
drtyr = lai_ind[[2]]
lai_ind = as.data.frame(lai_ind[1]) %>% 
  mutate(scenario="irr")
one <- rbind(lai_ind, one)
lai_ind = one
# plots 
rrplot <- lai_ind %>% dplyr::filter(wy > drtyr-1) %>% 
  ggplot() + geom_line(aes(x=as.factor(wy), y=rr, col=as.factor(run), group=run), show.legend=F) + ggtitle("relative resilience") + facet_wrap('scenario')

recplot <- lai_ind %>% 
  #dplyr::filter(wy > drtyr-1) %>% 
  ggplot() + geom_line(aes(x=as.factor(wy), y=recover, col=as.factor(run), group=run), show.legend=F) +
  facet_wrap('scenario')

resilplot <- res %>% 
  #dplyr::filter(wy > drtyr-1) %>% 
  ggplot() + geom_line(aes(x=as.factor(wy), y=resist, col=as.factor(run), group=run), show.legend=F) + ggtitle("resilience = post/pre")
resilplot + facet_wrap('scenario')

res %>% dplyr::filter(wy == 2017) %>% 
  ggplot() + geom_boxplot(aes(x=scenario, y=resist))

grid.arrange(resilplot, recplot)
