# load packages and set directory
library(RHESSysIOinR)
library(tidyverse)
library(sensitivity)
source("~/RHESSysIOinR/R/select_output_variables_R.R")
setwd("~/VegParamsSA/scripts")

# new output folder: 
#"~/../../scratch/ratorres/" 

## put nice in front of command - gives it less priority 

# 1: set up input files and command line options
# RHESSys Inputs
input_rhessys <- list()
input_rhessys$rhessys_version <- "~/RHESSys/bin/rhessys7.2"
input_rhessys$tec_file <- "../tecfiles/tec.coast"
input_rhessys$world_file <- "../worldfiles/OakPatch.world.0"
input_rhessys$world_hdr_prefix <- "../worldfiles/OakPatch.hdr"
input_rhessys$flow_file <- "../flowtables/OakPatch.flow"
input_rhessys$start_date <- "1947 10 1 1"
input_rhessys$end_date <- "2007 10 2 2"
input_rhessys$output_folder <- "~/../../scratch/ratorres/out"
input_rhessys$output_filename <- "evergreen.sob"
input_rhessys$command_options <- c("-b -g -climrepeat")

# HDR (header) file
input_hdr_list <- list()
input_hdr_list$basin_def <- c("../defs/stdbasin.def")
input_hdr_list$hillslope_def <- c("../defs/stdhillslope.def")
input_hdr_list$zone_def <- c("../defs/stdzone.def")
input_hdr_list$soil_def <- c("../defs/shallow.def")
input_hdr_list$landuse_def <- c("../defs/lu_undev.def")
input_hdr_list$stratum_def <- c("../defs/veg_liveoak.def")
input_hdr_list$base_stations <- c("../clim/mission_base")

# sample input def list - can change parameters
# [1] on stratum_def corresponds to veg file listed above
# parameter method LHC - c(low, high, n=number of runs)
n = 300
input_def_list <- list()
input_def_list <- list(
  list(input_hdr_list$soil_def[1], "pore_size_index", c(0.55,0.7, n)),
  list(input_hdr_list$soil_def[1], "psi_air_entry", c(0.05, 3, n)),
  list(input_hdr_list$stratum_def[1], "epc.leaf_cn", c(30.0125, 65, n)),
  list(input_hdr_list$stratum_def[1], "epc.branch_turnover", c(0.01, 0.04, n)),
  list(input_hdr_list$stratum_def[1], "epc.gl_smax", c(0.0025, 0.004, n)),
  list(input_hdr_list$stratum_def[1], "epc.flnr_age_mult", c(1.00025, 1.99975, n)),
  list(input_hdr_list$stratum_def[1], "epc.litter_moist_coef", c(5.00125e-05, 0.00069985, n)),
  list(input_hdr_list$stratum_def[1], "epc.psi_close", c(-9, -3, n)),
  list(input_hdr_list$stratum_def[1], "mrc.q10", c(1.8, 2.2, n)),
  list(input_hdr_list$stratum_def[1], "epc.vpd_close", c(2500.875, 5993.125, n)),
  list(input_hdr_list$stratum_def[1], "epc.leaf_turnover", c(0.12, 0.425, n)),
  list(input_hdr_list$stratum_def[1], "epc.froot_turnover", c(0.2, 0.7, n)),
  list(input_hdr_list$stratum_def[1], "epc.storage_transfer_prop", c(0.475, 0.575, n)),
  list(input_hdr_list$stratum_def[1], "epc.height_to_stem_coef", c(0.1501875, 0.9492, n)),
  list(input_hdr_list$stratum_def[1], "epc.waring_pa", c(0.05, 0.8, n)),
  list(input_hdr_list$stratum_def[1], "epc.root_growth_direction", c(0.5, 1.5, n)),
  list(input_hdr_list$stratum_def[1], "epc.root_distrib_parm", c(2.5, 4.25, n)),
  list(input_hdr_list$stratum_def[1], "epc.proj_sla", c(6, 13, n)),
  list(input_hdr_list$stratum_def[1], "epc.alloc_stemc_leafc", c(1.3, 1.8, n)),
  list(input_hdr_list$stratum_def[1], "epc.ext_coef", c(0.650025, 0.749975, n)),
  list(input_hdr_list$stratum_def[1], "specific_rain_capacity", c(0.000100025, 0.000199975, n)),
  list(input_hdr_list$stratum_def[1], "epc.flnr_sunlit", c(0.1, 0.638134193125, n)),
  list(input_hdr_list$stratum_def[1], "epc.flnr_shade", c(0.090801719375, 0.397329453125, n)),
  list(input_hdr_list$stratum_def[1], "epc.netpabs_sunlit", c(0.1, 0.439022910625, n)),
  list(input_hdr_list$stratum_def[1], "epc.netpabs_shade", c(0.1, 0.4, n)),
  list(input_hdr_list$stratum_def[1], "epc.netpabs_age_mult", c(1.00025, 1.99975, n)),
  list(input_hdr_list$stratum_def[1], "epc.cpool_mort_fract", c(0.001, 0.05, n)),
  list(input_hdr_list$stratum_def[1], "epc.min_percent_leafg", c(0.001, 0.01, n)),
  list(input_hdr_list$stratum_def[1], "epc.livewood_turnover", c(0.02, 0.1, n)),
  list(input_hdr_list$stratum_def[1], "epc.waring_pb", c(0.05, 1.0, n)),
  list(input_hdr_list$stratum_def[1], "epc.max_storage_percent", c(0.2, 0.4, n)),
  list(input_hdr_list$stratum_def[1], "epc.min_leaf_carbon", c(0.00102, 0.02, n)),
  list(input_hdr_list$stratum_def[1], "epc.resprout_leaf_carbon", c(0.005, 0.01, n)),
  list(input_hdr_list$stratum_def[1], "epc.alloc_frootc_leafc", c(0.3, 1.2, n)),
  list(input_hdr_list$stratum_def[1], "epc.frootc_crootc", c(0.7, 1.25, n))
)


# select which variables to look at from output
output_variables <- data.frame(out_file=character(), variable=character(), stringsAsFactors=FALSE)
output_variables[1,] <- data.frame("bd", "lai", stringsAsFactors=FALSE)
output_variables[2,] <- data.frame("bd", "height", stringsAsFactors = FALSE)
output_variables[3,] <- data.frame("bd", "plantc", stringsAsFactors = FALSE)
output_variables[11,] <- data.frame("bd", "rootdepth", stringsAsFactors=FALSE)

output_variables[4,] <- data.frame("cdg", "leafc", stringsAsFactors=FALSE)
output_variables[5,] <- data.frame("cdg", "live_stemc", stringsAsFactors=FALSE)
output_variables[6,] <- data.frame("cdg", "live_crootc", stringsAsFactors=FALSE)
output_variables[7,] <- data.frame("cdg", "dead_stemc", stringsAsFactors = FALSE)
output_variables[8,] <- data.frame("cdg", "dead_crootc", stringsAsFactors=FALSE)
output_variables[9,] <- data.frame("cdg", "dead_leafc", stringsAsFactors = FALSE)
output_variables[10,] <- data.frame("cdg", "leafc_store", stringsAsFactors=FALSE)

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


#########################################################################
# Generate table for def file param changes
#########################################################################

if (is.null(input_def_list[1]) == FALSE){

  method = "monte_carlo"
  parameter_method = match.arg(c("all_combinations", "lhc", "monte_carlo", "exact_values"), arg=method)

  input_def_file <- unlist(lapply(input_def_list, function(x) x[1]))
  input_def_file_unique <- unique(input_def_file)

  input_def_list_by_unique_file <- input_def_file_unique %>%
    # Generates matrix indicating which components of input_def_list are assigned to each def file
    sapply(function(y) lapply(input_def_list, function(x) x[1] == y)) %>%
    # Generates three nested lists
    lapply(seq_along(input_def_file_unique), function(x,y) input_def_list[y[,x] == TRUE], .)

  option_sets_def_par1=list()
  option_sets_def_par2=list()
  # Subset by each def file
  for (bb in seq_along(input_def_file_unique)){
    names(input_def_list_by_unique_file[[bb]]) <- lapply(input_def_list_by_unique_file[[bb]], function(x) x[[2]])
    input_def_change_par <- sapply(input_def_list_by_unique_file[[bb]], function(x) x[[3]], simplify = FALSE, USE.NAMES = TRUE) # Isolate parameter values
    
    option_sets_def_par1[[bb]] <- make_option_set_combinations(input_list=input_def_change_par, parameter_method=parameter_method)
    # Attach group ID to option_sets_def_par
    tmp <- seq_along(option_sets_def_par1[[bb]][[1]])
    option_sets_def_par1[[bb]] <- bind_cols(option_sets_def_par1[[bb]], group_id = tmp)
    
    option_sets_def_par2[[bb]] <- make_option_set_combinations(input_list=input_def_change_par, parameter_method=parameter_method)
    # Attach group ID to option_sets_def_par
    tmp <- seq_along(option_sets_def_par2[[bb]][[1]])
    option_sets_def_par2[[bb]] <- bind_cols(option_sets_def_par2[[bb]], group_id = tmp)
  }
  
# turn into data frame with all parameters to input to sobol function
    X1 <-inner_join(option_sets_def_par1[[1]], option_sets_def_par1[[2]], by="group_id") %>% dplyr::select(-group_id)
    X2 <- inner_join(option_sets_def_par2[[1]], option_sets_def_par2[[2]], by="group_id") %>% dplyr::select(-group_id)
    
    run_sob <- soboljansen(model=NULL, X1, X2, nboot=100)
    
    # Attach group ID to option_sets_def_par
    tmp <- seq_along(1:nrow(run_sob$X))
    sobol_tmp <- cbind(run_sob$X, group_id=tmp)
    option_sets_def_par <- list(sobol_tmp[,c(1:2,29)], 
                                sobol_tmp[,3:29])

    names(option_sets_def_par) <- input_def_file_unique
}
#########################################################################
# create option_sets_all to be output as csv

# if dated seq is NULL (which it should be because it has to do with fire?)
option_sets_dated_seq <- data.frame(dated_id = 0)

# option sets all combines veg params with rhessys command line input
option_sets_all <- make_all_option_table(parameter_method,
                                         input_rhessys,
                                         input_hdr_list,
                                         option_sets_def_par,
                                         option_sets_standard_par=NULL,
                                         option_sets_dated_seq)

nrow(option_sets_all)
# command line parameters
# values from script, calibrated by Janet or Erin or someone for Rattlesnake / Mission creek
n = nrow(option_sets_def_par[[1]])
  input_standard_par_list <- list(
    m =  rep(0.036294,n),
    k = rep(359.4858, n),
    m_v = rep(0.036294,n),
    k_v = rep(359.4858, n),
    pa = rep(1.518305, n),
    po = rep(1.785178, n),
    gw1 = rep(0.346205, n),
    gw2 = rep(0.416299, n)
  )

#########################################################################

# this first generates a new def file based on parameters from list
# then runs rhessys for however much time is set
# then takes output and puts into new file in directory 'allsim'
# each time params change, the def file is written over, not saved. Each run will have an output that should correspond with ID params in row of csv

# start here to run
if(nrow(option_sets_def_par[[1]])==nrow(option_sets_all)){

  saveRDS(run_sob, "../out/soboljansenoutput.rds")
  write.csv(option_sets_all, file.path(input_rhessys$output_folder, paste(input_rhessys$output_filename, "_all_options.csv", sep="")), row.names = FALSE, quote=FALSE)

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
        rhessys_command(rhessys_version = option_sets_all$rhessys_version[bb],
                        world_file = option_sets_all$world_file[bb],
                        world_hdr_file = "../worldfiles/OakPatch.hdr",
                        tec_file = "../tecfiles/tec.spinup",
                        flow_file = option_sets_all$flow_file[bb],
                        start_date = option_sets_all$start_date[bb],
                        end_date = "2012 10 1 1",
                        output_file = paste(option_sets_all$output_folder[bb],"/", option_sets_all$output_filename[bb], sep=""),
                        input_parameters = "-s 0.036294 359.485800 -sv 0.036294 359.485800 -gw 0.346205 0.416299",
                        command_options = "-b -g -c -climrepeat")

      #   cmd = sprintf("mv ../worldfiles/OakPatch.world.0.Y2007M10D1H1.state ../worldfiles/OakWorld.spun60")
      # system(cmd)
      # 
      # print(paste("Spin up complete"))
      # 
      # # for run with output
      # rhessys_command(rhessys_version = option_sets_all$rhessys_version[bb],
      #                 world_file = "../worldfiles/OakWorld.spun60",
      #                 world_hdr_file = "../worldfiles/OakPatch.hdr",
      #                 tec_file = "../tecfiles/tec.coast",
      #                 flow_file = option_sets_all$flow_file[bb],
      #                 start_date = "2002 9 30 1",
      #                 end_date = "2012 10 1 1",
      #                 output_file = paste(option_sets_all$output_folder[bb],"/", option_sets_all$output_filename[bb], sep=""),
      #                 input_parameters = "-s 0.036294 359.485800 -sv 0.036294 359.485800 -gw 0.346205 0.416299",
      #                 command_options = "-b -g -c -climrepeat")


        # Process RHESSys output
          if (is.null(output_variables[1]) == F){
            select_output_variables_R(output_variables = output_variables,
                                      output_folder =  input_rhessys$output_folder,
                                      output_filename = input_rhessys$output_filename,
                                      run = bb,
                                      max_run = option_sets_rhessys_rows
            )
          }
      }
   # }
  )
}

  
  










