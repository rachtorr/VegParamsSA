# This eventually needs to morph into a vignette example of some sort.
# Currently this is a testing input file for Ryan Bart

library(RHESSysIOinR)

# Note: Need to change directory!! This example uses fire_effects_model directory.
setwd("~/Google Drive File Stream/My Drive/patches")
# ---------------------------------------------------------------------
# Model inputs

# Processing options
parameter_method <- "lhc"

# RHESSys Inputs
input_rhessys <- list()
#input_rhessys$rhessys_version <- "/Users/christina/Desktop/LocalModel/rhessys_git/rhessys/rhessys5.20.1.thin"
input_rhessys$rhessys_version <- "~/RHESSys-develop-edit/bin/rhessys7.0"
input_rhessys$tec_file <- "tecfiles/tec.target"
input_rhessys$world_file <- "worldfiles/OakPatch.test"
input_rhessys$world_hdr_prefix <- "RH_lhc_test"
input_rhessys$flow_file <- "flowtables/OakPatch.flow"
input_rhessys$start_date <- "1947 10 1 1"
input_rhessys$end_date <- "1997 10 1 1"
input_rhessys$output_folder <- "out/RHESSysIOinR_output"
input_rhessys$output_filename <- "2020test"
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


# Define path to a pre-selected df containing parameter sets
input_preexisting_table <- NULL
#input_preexisting_table <- file.path(input_rhessys$output_folder, paste(input_rhessys$output_filename, "_parameter_sets.txt", sep=""))


# Def file parameter changes
# List of lists containing def_file, parameter and parameters values
input_def_list <- NULL
input_def_list <- list(
  list(input_hdr_list$stratum_def[2], "epc.leaf_turnover", c(0.4, 0.3)),
  list(input_hdr_list$stratum_def[2], "epc.livewood_turnover", c(0.1)),
  list(input_hdr_list$stratum_def[2], "epc.alloc_frootc_leafc", c(1.4)),
  list(input_hdr_list$stratum_def[2], "epc.alloc_crootc_stemc", c(0.4)),
  list(input_hdr_list$stratum_def[2], "epc.alloc_stemc_leafc", c(0.2)),
  list(input_hdr_list$stratum_def[2], "epc.alloc_livewoodc_woodc", c(0.9)),
  list(input_hdr_list$stratum_def[2], "epc.branch_turnover", c(0.02)),
  list(input_hdr_list$stratum_def[2], "epc.height_to_stem_exp", c(0.57)),
  list(input_hdr_list$stratum_def[2], "epc.height_to_stem_coef", c(4.0)),
  list(input_hdr_list$stratum_def[1], "epc.height_to_stem_exp", c(0.57)),
  list(input_hdr_list$stratum_def[1], "epc.height_to_stem_coef", c(11.39, 5))
)

# Def file parameter changes
# List of lists containing def_file, parameter and parameters values
#input_def_list <- NULL
input_def_list <- list(
  list(input_hdr_list$stratum_def[1], "epc.leaf_turnover", c(0.1, 1, 4)),
  list(input_hdr_list$stratum_def[1], "epc.branch_turnover", c(0.02, 2, 4)),
  list(input_hdr_list$stratum_def[1], "epc.height_to_stem_exp", c(0.57, 40, 4)),
  list(input_hdr_list$stratum_def[1], "epc.height_to_stem_coef", c(11.39, 20, 4))
)

# Make climate base station file

#input_clim_base_list <- NULL
input_clim_base_list <- list(
  list(core = data.frame(c1=character(),c2=character(),stringsAsFactors=FALSE),
       annual = data.frame(c1=character(),c2=character(),stringsAsFactors=FALSE),
       monthly = data.frame(c1=character(),c2=character(),stringsAsFactors=FALSE),
       daily = data.frame(c1=character(),c2=character(),stringsAsFactors=FALSE),
       hourly = data.frame(c1=character(),c2=character(),stringsAsFactors=FALSE)
  )
)
input_clim_base_list[[1]][[1]][1,] <- data.frame(c1=101, c2="base_station_id",stringsAsFactors=FALSE)
input_clim_base_list[[1]][[1]][2,] <- data.frame(c1=100.0, c2="x_coordinate",stringsAsFactors=FALSE)
input_clim_base_list[[1]][[1]][3,] <- data.frame(c1=100.0, c2="y_coordinate",stringsAsFactors=FALSE)
input_clim_base_list[[1]][[1]][4,] <- data.frame(c1=640, c2="z_coordinate",stringsAsFactors=FALSE)
input_clim_base_list[[1]][[1]][5,] <- data.frame(c1=3.5, c2="effective_lai",stringsAsFactors=FALSE)
input_clim_base_list[[1]][[1]][6,] <- data.frame(c1=10, c2="screen_height",stringsAsFactors=FALSE)

input_clim_base_list[[1]][[2]][1,] <- data.frame(c1="annual", c2="annual_climate_prefix",stringsAsFactors=FALSE)
input_clim_base_list[[1]][[2]][2,] <- data.frame(c1=0, c2="number_non_critical_annual_sequences",stringsAsFactors=FALSE)

input_clim_base_list[[1]][[3]][1,] <- data.frame(c1="monthly", c2="monthly_climate_prefix",stringsAsFactors=FALSE)
input_clim_base_list[[1]][[3]][2,] <- data.frame(c1=0, c2="number_non_critical_monthly_sequences",stringsAsFactors=FALSE)

input_clim_base_list[[1]][[4]][1,] <- data.frame(c1="../clim/sb_mc_daily", c2="daily_climate_prefix",stringsAsFactors=FALSE)
input_clim_base_list[[1]][[4]][2,] <- data.frame(c1=1, c2="number_non_critical_daily_sequences",stringsAsFactors=FALSE)

input_clim_base_list[[1]][[5]][1,] <- data.frame(c1="hourly", c2="hourly_climate_prefix",stringsAsFactors=FALSE)
input_clim_base_list[[1]][[5]][2,] <- data.frame(c1=0, c2="number_non_critical_hourly_sequences",stringsAsFactors=FALSE)



# Make a list of dated sequence data.frames (file name, year, month, day, hour, value)
# input_dated_seq_list <- NULL
 input_dated_seq_list <- list()
 # input_dated_seq_list[[1]] <- data.frame(name="lowProv",type="pspread",year=1941,month=10,day=1,hour=1,value=0.1,stringsAsFactors=FALSE)
 # input_dated_seq_list[[2]] <- data.frame(name="lowProv",type="pspread",year=1941,month=10,day=1,hour=1,value=0.2,stringsAsFactors=FALSE)
# input_dated_seq_list[[3]] <- data.frame(name="lowProv",type="pspread",year=1941,month=10,day=1,hour=1,value=0.3,stringsAsFactors=FALSE)
# input_dated_seq_list[[4]] <- data.frame(name="lowProv",type="pspread",year=1941,month=10,day=1,hour=1,value=0.4,stringsAsFactors=FALSE)
# input_dated_seq_list[[5]] <- data.frame(name="lowProv",type="pspread",year=1941,month=10,day=1,hour=1,value=0.5,stringsAsFactors=FALSE)
# input_dated_seq_list[[6]] <- data.frame(name="lowProv",type="pspread",year=1941,month=10,day=1,hour=1,value=0.6,stringsAsFactors=FALSE)
# input_dated_seq_list[[7]] <- data.frame(name="lowProv",type="pspread",year=1941,month=10,day=1,hour=1,value=0.7,stringsAsFactors=FALSE)
# input_dated_seq_list[[8]] <- data.frame(name="lowProv",type="pspread",year=1941,month=10,day=1,hour=1,value=0.8,stringsAsFactors=FALSE)
# input_dated_seq_list[[9]] <- data.frame(name="lowProv",type="pspread",year=1941,month=10,day=1,hour=1,value=0.9,stringsAsFactors=FALSE)
# input_dated_seq_list[[10]] <- data.frame(name="lowProv",type="pspread",year=1941,month=10,day=1,hour=1,value=1.0,stringsAsFactors=FALSE)

# Standard sub-surface parameters
# input_standard_par_list <- NULL
# input_standard_par_list <- list(
#   m = c(1.792761),
#   k = c(1.566492),
#   m_v = c(1.792761),
#   k_v = c(1.566492, 4),
#   pa = c(7.896941),
#   po = c(1.179359),
#   gw1 = c(0.1668035),
#   gw2 = c(0.178753)
# )

#input_standard_par_list <- NULL
input_standard_par_list <- list(
  m = c(1,1000, 4),
  k = c(1, 10, 4),
  m_v = c(1,1000, 4),
  k_v = c(1,10, 4),
  pa = c(2, 10, 4),
  po = c(1, 10, 4),
  gw1 = c(0.4, 0.4, 4),
  gw2 = c(1, 1, 4)
)

input_standard_par_list <- list(
  m =  0.036294,
  k = 359.4858,
  m_v = 0.036294,
  k_v = 359.4858,
  pa = 1.518305,
  po = 1.785178,
  gw1 = 0.346205,
  gw2 = 0.416299
)



# Make tec-file
#input_tec_data <- NULL
input_tec_data <- data.frame(year=integer(),month=integer(),day=integer(),hour=integer(),name=character(),stringsAsFactors=FALSE)
input_tec_data[1,] <- data.frame(1941, 10, 1, 1, "print_daily_on", stringsAsFactors=FALSE)
input_tec_data[2,] <- data.frame(1941, 10, 1, 2, "print_daily_growth_on", stringsAsFactors=FALSE)


# Data frame containing variable of interest, location/name of awk file (relative to output
# file location), and the location/name of rhessys output file with variable of interest.
# output_variables <- NULL
output_variables <- data.frame(variable=character(),awk_path=character(),out_file=character(),stringsAsFactors=FALSE)
output_variables[1,] <- data.frame("lai", "awks/output_var_bd_lai.awk","patch_sim_basin.daily",stringsAsFactors=FALSE)
output_variables[2,] <- data.frame("leafc", "awks/output_var_cdg_leafc.awk","patch_sim_grow_stratum.daily",stringsAsFactors=FALSE)
output_variables[3,] <- data.frame("stemc", "awks/output_var_cdg_stemc.awk","patch_sim_grow_stratum.daily",stringsAsFactors=FALSE)
output_variables[4,] <- data.frame("live_stemc", "awks/output_var_cdg_live_stemc.awk","patch_sim_grow_stratum.daily",stringsAsFactors=FALSE)
output_variables[5,] <- data.frame("dead_stemc", "awks/output_var_cdg_dead_stemc.awk","patch_sim_grow_stratum.daily",stringsAsFactors=FALSE)
output_variables[6,] <- data.frame("rootc", "awks/output_var_cdg_rootc.awk","patch_sim_grow_stratum.daily",stringsAsFactors=FALSE)

output_variables[7,] <- data.frame("litrc", "awks/output_var_bd_litrc.awk","patch_sim_basin.daily",stringsAsFactors=FALSE)
output_variables[8,] <- data.frame("cwdc", "awks/output_var_cdg_cwdc.awk","patch_sim_grow_stratum.daily",stringsAsFactors=FALSE)
output_variables[9,] <- data.frame("soil1c", "awks/output_var_pdg_soil1c.awk","patch_sim_grow_patch.daily",stringsAsFactors=FALSE)

output_variables[10,] <- data.frame("height", "awks/output_var_cd_height.awk","patch_sim_stratum.daily",stringsAsFactors=FALSE)


# Output variables for R version
# output_variables <- NULL
output_variables <- data.frame(out_file=character(), variable=character(), stringsAsFactors=FALSE)
output_variables[1,] <- data.frame("bd", "lai", stringsAsFactors=FALSE)
# output_variables[2,] <- data.frame("cdg", "leafc", stringsAsFactors=FALSE)
# output_variables[3,] <- data.frame("cdg", "live_stemc", stringsAsFactors=FALSE)
# output_variables[4,] <- data.frame("cdg", "dead_stemc", stringsAsFactors=FALSE)

# ---------------------------------------------------------------------


system.time(
  run_rhessys(parameter_method = parameter_method,
            input_rhessys = input_rhessys,
            input_hdr_list = input_hdr_list,
            input_preexisting_table = input_preexisting_table,
            input_def_list = input_def_list,
            input_standard_par_list = input_standard_par_list,
            input_clim_base_list = input_clim_base_list,
            input_dated_seq_list = input_dated_seq_list,
            input_tec_data = input_tec_data,
            output_variables = output_variables)
)

beep(1)

# have to generate option sets first
rhessys_command(rhessys_version = input_rhessys$rhessys_version,
                world_file = input_rhessys$world_file,
                world_hdr_file = "../pre_output/surface_roads.world.hdr",
                tec_file = input_rhessys$tec_file,
                flow_file = input_rhessys$flow_file,
                start_date = input_rhessys$start_date,
                end_date = input_rhessys$end_date,
                output_file = input_rhessys$output_filename,
                input_parameters = option_sets_rhessys$input_parameters[1],
                command_options = option_sets_rhessys$command_options[1])
