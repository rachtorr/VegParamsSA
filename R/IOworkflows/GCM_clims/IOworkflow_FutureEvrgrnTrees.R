# load packages and set directory
library(RHESSysIOinR)
library(tidyverse)
library(sensitivity)
source("~/RHESSysIOinR-master/R/select_output_variables_R.R")
setwd("~/Documents/patches/R/tree_outputs/")

# read in the parameters 
evr <- read.csv("evg_params.csv") %>% dplyr::select(-X)


# select which variables to look at from output
output_variables <- data.frame(out_file=character(), variable=character(), stringsAsFactors=FALSE)
output_variables[1,] <- data.frame("bd", "lai", stringsAsFactors=FALSE)
output_variables[2,] <- data.frame("bd", "height", stringsAsFactors = FALSE)
output_variables[3,] <- data.frame("bd", "plantc", stringsAsFactors = FALSE)
# output_variables[4,] <- data.frame("bd", "vpd", stringsAsFactors = FALSE)
# output_variables[5,] <- data.frame("cd", "psi", stringsAsFactors = FALSE)
# output_variables[6,] <- data.frame("cdg", "gsi", stringsAsFactors = FALSE)
output_variables[4,] <- data.frame("bdg", "plant_resp", stringsAsFactors = FALSE)
output_variables[5,] <- data.frame("bd", "gpsn", stringsAsFactors = FALSE)
output_variables[6,] <- data.frame("bd", "evap_can", stringsAsFactors = FALSE)
output_variables[7,] <- data.frame("bd", "trans", stringsAsFactors = FALSE)
output_variables[7,] <- data.frame("bd", "precip", stringsAsFactors = FALSE)
output_variables[8,] <- data.frame("bd", "evap", stringsAsFactors = FALSE)


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
type = c("Oak","Oak","Con")


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
  p2 <- option_sets_all[,14:54]
  
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
    "epc.day_leafon",
    "epc.day_leafoff",
    "epc.ndays_expand",
    "epc.ndays_litfall")
  
  colnames(p2) <- p_names 
  soils <- p2[,1:3]
  veg <- p2[,3:41]
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
                        world_hdr_file = option_sets_all$world_hdr_prefix[bb],
                        tec_file = "../tecfiles/tec.spinup",
                        flow_file = option_sets_all$flow_file[bb],
                        start_date = "1940 5 1 1",
                        end_date = "2010 10 2 2",
                        output_file = paste(option_sets_all$output_folder[bb],"/", option_sets_all$code[bb], sep=""),
                        input_parameters = "-s 0.036294 359.485800 -sv 0.036294 359.485800 -gw 0.346205 0.416299",
                        command_options = "-b -g -c -p -climrepeat -vmort_off")
        
#         if (is.null(output_variables[1]) == F){
#           select_output_variables_R(output_variables = output_variables,
#                                     output_folder =  paste("../out/rcps"),
#                                     output_filename = option_sets_all$code[bb],
#                                     run = bb,
#                                     max_run = 50
#           )
#         }
# 
#     }
#     # }
#   )
# }
# }


#############################################################
        ## historical drought first 
#############################################################
        
        cmd = sprintf("mv ../worldfiles/OakPatch.world.0.Y2010M8D1H1.state ../worldfiles/OakWorld.spun70")
      system(cmd)

      print(paste("Spin up complete"))

      # for run with output
      rhessys_command(rhessys_version = option_sets_all$rhessys_version[bb],
                      world_file = "../worldfiles/OakWorld.spun70",
                      world_hdr_file = option_sets_all$world_hdr_prefix[bb],
                      tec_file = "../tecfiles/tec.coast",
                      flow_file = option_sets_all$flow_file[bb],
                      start_date = "2010 5 30 1",
                      end_date = "2018 10 1 1",
                      output_file = paste(option_sets_all$output_folder[bb], "hist", option_sets_all$code[bb],option_sets_all$code[bb], sep="/"),
                      input_parameters = "-s 0.036294 359.485800 -sv 0.036294 359.485800 -gw 0.346205 0.416299",
                      command_options = "-b -g -c -climrepeat -vmort_off")


        # Process RHESSys output
          if (is.null(output_variables[1]) == F){
            select_output_variables_R(output_variables = output_variables,
                                      output_folder =  paste("../out/rcps/hist",option_sets_all$code[bb], sep="/"),
                                      output_filename = option_sets_all$code[bb],
                                      run = bb,
                                      max_run = 50
            )
          }
      
      
      #############################################################
      ## canesm 
      #############################################################
      
      rhessys_command(rhessys_version = option_sets_all$rhessys_version[bb],
                      world_file = "../worldfiles/OakWorld.spun70",
                      world_hdr_file = paste("../worldfiles/rcp45/", option_sets_all$future_hdr_prefix[bb], "_canesm.hdr", sep=""),
                      tec_file = "../tecfiles/tec.coast",
                      flow_file = option_sets_all$flow_file[bb],
                      start_date = "2094 5 30 1",
                      end_date = "2099 10 1 1",
                      output_file = paste(option_sets_all$output_folder[bb], "canesm", option_sets_all$code[bb],option_sets_all$code[bb], sep="/"),
                      input_parameters = "-s 0.036294 359.485800 -sv 0.036294 359.485800 -gw 0.346205 0.416299",
                      command_options = "-b -g -c -climrepeat -vmort_off")
      
      
      # Process RHESSys output
      if (is.null(output_variables[1]) == F){
        select_output_variables_R(output_variables = output_variables,
                                  output_folder =  paste("../out/rcps/canesm",option_sets_all$code[bb], sep="/"),
                                  output_filename = option_sets_all$code[bb],
                                  run = bb,
                                  max_run = 50
        )
      }
      #############################################################
      ## cnrm
      #############################################################
      
      rhessys_command(rhessys_version = option_sets_all$rhessys_version[bb],
                      world_file = "../worldfiles/OakWorld.spun70",
                      world_hdr_file = paste("../worldfiles/rcp45/", option_sets_all$future_hdr_prefix[bb], "_cnrm.hdr", sep=""),
                      tec_file = "../tecfiles/tec.coast",
                      flow_file = option_sets_all$flow_file[bb],
                      start_date = "2092 5 30 1",
                      end_date = "2096 10 1 1",
                      output_file = paste(option_sets_all$output_folder[bb], "cnrm", option_sets_all$code[bb],option_sets_all$code[bb], sep="/"),
                      input_parameters = "-s 0.036294 359.485800 -sv 0.036294 359.485800 -gw 0.346205 0.416299",
                      command_options = "-b -g -c -climrepeat -vmort_off")
      
      
      # Process RHESSys output
      if (is.null(output_variables[1]) == F){
        select_output_variables_R(output_variables = output_variables,
                                  output_folder =  paste("../out/rcps/cnrm",option_sets_all$code[bb], sep="/"),
                                  output_filename = option_sets_all$code[bb],
                                  run = bb,
                                  max_run = 50
        )
      }
      
      #############################################################
      ## hadgem
      #############################################################
      
      rhessys_command(rhessys_version = option_sets_all$rhessys_version[bb],
                      world_file = "../worldfiles/OakWorld.spun70",
                      world_hdr_file = paste("../worldfiles/rcp45/", option_sets_all$future_hdr_prefix[bb], "_hadgem.hdr", sep=""),
                      tec_file = "../tecfiles/tec.coast",
                      flow_file = option_sets_all$flow_file[bb],
                      start_date = "2041 5 30 1",
                      end_date = "2046 10 1 1",
                      output_file = paste(option_sets_all$output_folder[bb], "hadgem", option_sets_all$code[bb],option_sets_all$code[bb], sep="/"),
                      input_parameters = "-s 0.036294 359.485800 -sv 0.036294 359.485800 -gw 0.346205 0.416299",
                      command_options = "-b -g -c -climrepeat -vmort_off")
      
      
      # Process RHESSys output
      if (is.null(output_variables[1]) == F){
        select_output_variables_R(output_variables = output_variables,
                                  output_folder =  paste("../out/rcps/hadgem",option_sets_all$code[bb], sep="/"),
                                  output_filename = option_sets_all$code[bb],
                                  run = bb,
                                  max_run = 50
        )
      }
      
      #############################################################
      ## miroc
      #############################################################
      
      rhessys_command(rhessys_version = option_sets_all$rhessys_version[bb],
                      world_file = "../worldfiles/OakWorld.spun70",
                      world_hdr_file = paste("../worldfiles/rcp45/", option_sets_all$future_hdr_prefix[bb], "_miroc.hdr", sep=""),
                      tec_file = "../tecfiles/tec.coast",
                      flow_file = option_sets_all$flow_file[bb],
                      start_date = "2080 5 30 1",
                      end_date = "2085 10 1 1",
                      output_file = paste(option_sets_all$output_folder[bb], "miroc", option_sets_all$code[bb],option_sets_all$code[bb], sep="/"),
                      input_parameters = "-s 0.036294 359.485800 -sv 0.036294 359.485800 -gw 0.346205 0.416299",
                      command_options = "-b -g -c -climrepeat -vmort_off")
      
      
      # Process RHESSys output
      if (is.null(output_variables[1]) == F){
        select_output_variables_R(output_variables = output_variables,
                                  output_folder =  paste("../out/rcps/miroc",option_sets_all$code[bb], sep="/"),
                                  output_filename = option_sets_all$code[bb],
                                  run = bb,
                                  out = option_sets_all$code[bb],
                                  max_run = 50
        )
      }
    
      
    
      }
    )
  }
}

  
  










