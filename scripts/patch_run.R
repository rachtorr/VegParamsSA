# this script is for spinning up the species at patch level and then using output to run from 2010 - 2018 over the drought period 
# Index data is from 2011, 2014, 2017 so get years before and after, could even get a few years before, check clim data to see what water was doing 

setwd("~/Google Drive File Stream/My Drive/patches/scripts/")

# load in option sets from IOworkflow run through 
option_sets_all <- read.csv("../out/RHESSysIOinR_output/evergreen.6_all_options.csv")
# RHESSys Inputs 
input_rhessys <- list()
input_rhessys$rhessys_version <- "~/RHESSys-develop/bin/rhessys7.1.1"
input_rhessys$tec_file <- "../tecfiles/tec.spinup"
input_rhessys$world_file <- "../worldfiles/OakPatch.world.0"
input_rhessys$world_hdr_prefix <- "../worldfiles/piun.hdr"
input_rhessys$flow_file <- "../flowtables/OakPatch.flow"
input_rhessys$start_date <- "1947 10 1 1"
input_rhessys$end_date <- "2048 10 1 2"
input_rhessys$output_folder <- "../out/RHESSysIOinR_output"
input_rhessys$output_filename <- "piun_spin"
input_rhessys$command_options <- c("-b -g -climrepeat")

# HDR (header) file
input_hdr_list <- list()
input_hdr_list$basin_def <- c("../defs/stdbasin.def")
input_hdr_list$hillslope_def <- c("../defs/stdhillslope.def")
input_hdr_list$zone_def <- c("../defs/stdzone.def")
input_hdr_list$soil_def <- c("../defs/shallow.def")
input_hdr_list$landuse_def <- c("../defs/lu_undev.def")
# make the stratum def a folder where files will be pulled from
input_hdr_list$stratum_def <- c("../defs/veg_liveoak/")
input_hdr_list$base_stations <- c("../clim/mission_base")

defs <- list("../defs/veg_liveoak/veg_liveoak_PIUNeg_1.def",
             "../defs/veg_liveoak/veg_liveoak_SCTEeg_1.def")
hdrs <- list("../worldfiles/piun.hdr","../worldfiles/pepper.hdr")
out <- list("piun_spin","pepp_spin")
out.coast <- list("PIUNeg", "SCTEeg")


system.time(
  # Generate new hdr file for each def file in folder 
  # world_path <- dirname(input_rhessys$world_file)[1]
  # world_hdr_path <- file.path(world_path, input_rhessys$world_hdr_prefix)
  # if(dir.exists(world_hdr_path) == FALSE){dir.create(world_hdr_path)}
  # 
  # for (aa in seq_along(option_sets_def_par)){
  #   
  #   # Step through each unique parameter set and make def file
  #   for (bb in seq_along(option_sets_def_par[[aa]]$group_id)){
  #     
  #     change_def_file(def_file = names(option_sets_def_par)[aa],
  #                     par_sets = as_tibble(dplyr::select(option_sets_def_par[[aa]], -group_id))[bb,],
  #                     file_name_ext = "")
  #     
  #     print(paste("New def file written for file", names(option_sets_def_par)[aa]))
  #     
  #     print(paste("----------------- Run", bb ,"of", option_sets_rhessys_rows, "-----------------"))

  for(bb in 1:length(defs)){
      # Call RHESSys for spinup
      rhessys_command(rhessys_version = option_sets_all$rhessys_version[bb],
                      world_file = option_sets_all$world_file[bb],
                      world_hdr_file = hdrs[bb],
                      tec_file = "../tecfiles/tec.spinup",
                      flow_file = option_sets_all$flow_file[bb],
                      start_date = option_sets_all$start_date[bb],
                      end_date = option_sets_all$end_date[bb],
                      output_file = paste(option_sets_all$output_folder[bb],"/",out[bb], sep=""),
                      input_parameters = "-s 0.036294 359.485800 -sv 0.036294 359.485800 -svalt 1.518305 1.785178 -gw 0.346205 0.416299",
                      command_options = option_sets_all$command_options[bb])
      
      cmd = sprintf("mv ../worldfiles/OakPatch.world.0.Y2047M10D1H1.state ../worldfiles/OakWorld.spun100")
      system(cmd)
      
      print(paste("Spin up complete"))
      
      # for run with output (set tec file to output all years)
      rhessys_command(rhessys_version = option_sets_all$rhessys_version[bb],
                      world_file = "../worldfiles/OakWorld.spun100",
                      world_hdr_file = hdrs[bb],
                      tec_file = "../tecfiles/tec.coast",
                      flow_file = option_sets_all$flow_file[bb],
                      start_date = "2005 10 1 1",
                      end_date = "2018 10 1 1",
                      output_file = (paste(option_sets_all$output_folder[bb],"/",out.coast[bb], sep="")),
                      input_parameters = "-s 0.036294 359.485800 -sv 0.036294 359.485800 -svalt 1.518305 1.785178 -gw 0.346205 0.416299",
                      command_options = "-b -g -c -climrepeat")
      
  }
      