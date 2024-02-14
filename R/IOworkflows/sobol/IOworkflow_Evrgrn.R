# load packages and set directory
library(RHESSysIOinR)
library(tidyverse)
library(sensitivity)
#source("~/RHESSysIOinR-master/R/select_output_variables_R.R")
#setwd("~/Google Drive File Stream/My Drive/patches/scripts")
setwd("~/VegParamsSA/scripts/")

## parameter ranges and any settings as of March 14th 2021  

## Updates for AGU revisions Jan. 28, 2024
## sensitivity analysis with irrigation 
## changing: clim file with irrigation (patch_base), using output filter (watbal_patch.yml to check water balance and of_sensitivity.yml for sobol)



# 1: set up input files and command line options
# RHESSys Inputs
input_rhessys <- list()
input_rhessys$rhessys_version <- "~/RHESSys-rt/rhessys/rhessys7.3"
input_rhessys$tec_file <- "../tecfiles/tec.spinup"
input_rhessys$world_file <- "../worldfiles/Veg.world.0"
input_rhessys$world_hdr_prefix <- "../worldfiles/OakPatch.hdr" # updated to patch_base
input_rhessys$flow_file <- "../flowtables/OakPatch.flow"
input_rhessys$start_date <- "1980 10 1 1"
input_rhessys$end_date <- "2010 10 2 2"
input_rhessys$output_folder <- "../out"
input_rhessys$output_filename <- "oak"
input_rhessys$command_options <- c("-g -climrepeat -vmort_off -of watbal_of_noSurfaceMSR.yml")

input_hdr_list = list()
input_hdr_list$soil_def <- c("../defs/shallow.def")
input_hdr_list$landuse_def <- c("../defs/lu_undev.def")
input_hdr_list$stratum_def <- c("../defs/veg_liveoak.def")


# sample input def list - can change parameters
# [1] on stratum_def corresponds to veg file listed above
# parameter method LHC - c(low, high, n=number of runs)
n = 15
#nsp = 12
input_def_list <- list()
input_def_list <- list(
  list(input_hdr_list$soil_def[1], "pore_size_index", c(0.1,0.2, n)),
  list(input_hdr_list$soil_def[1], "psi_air_entry", c(0.17, 0.26, n)),
  list(input_hdr_list$stratum_def[1], "epc.leaf_cn", c(33, 67, n)),
  list(input_hdr_list$stratum_def[1], "epc.branch_turnover", c(0.01, 0.034, n)),
  list(input_hdr_list$stratum_def[1], "epc.gl_smax", c(0.0028, 0.00312, n)),
  list(input_hdr_list$stratum_def[1], "epc.flnr_age_mult", c(0.035, 0.053, n)),
  list(input_hdr_list$stratum_def[1], "epc.litter_moist_coef", c(0.00015, 0.0005995, n)),
  list(input_hdr_list$stratum_def[1], "epc.psi_close", c(-9, -3, n)),
  list(input_hdr_list$stratum_def[1], "mrc.q10", c(1.8, 2.4, n)),
  list(input_hdr_list$stratum_def[1], "epc.vpd_close", c(2600, 6400, n)),
  list(input_hdr_list$stratum_def[1], "epc.leaf_turnover", c(0.2, 1.2, n)),
  list(input_hdr_list$stratum_def[1], "epc.froot_turnover", c(0.3, 0.55, n)),
  list(input_hdr_list$stratum_def[1], "epc.storage_transfer_prop", c(0.39, 0.6, n)),
  list(input_hdr_list$stratum_def[1], "epc.height_to_stem_coef", c(0.25, 0.75, n)),
  list(input_hdr_list$stratum_def[1], "epc.waring_pa", c(0.24, 0.38, n)),
  list(input_hdr_list$stratum_def[1], "epc.root_distrib_parm", c(2, 8, n)),
  list(input_hdr_list$stratum_def[1], "epc.proj_sla", c(14, 25, n)),
  list(input_hdr_list$stratum_def[1], "epc.alloc_stemc_leafc", c(1.3, 1.8, n)),
  list(input_hdr_list$stratum_def[1], "epc.ext_coef", c(0.65, 0.75, n)),
  list(input_hdr_list$stratum_def[1], "specific_rain_capacity", c(0.000100025, 0.000199975, n)),
  list(input_hdr_list$stratum_def[1], "epc.netpabs_age_mult", c(0.232, 0.348, n)),
  list(input_hdr_list$stratum_def[1], "epc.cpool_mort_fract", c(0.0001, 0.1, n)),
  list(input_hdr_list$stratum_def[1], "epc.min_percent_leafg", c(0.0005, 0.01, n)),
  list(input_hdr_list$stratum_def[1], "epc.livewood_turnover", c(0.56, 0.84, n)),
  list(input_hdr_list$stratum_def[1], "epc.waring_pb", c(0.05, 1.0, n)),
  list(input_hdr_list$stratum_def[1], "epc.max_storage_percent", c(0.2, 0.4, n)),
  list(input_hdr_list$stratum_def[1], "epc.min_leaf_carbon", c(0.001, 0.02, n)),
  list(input_hdr_list$stratum_def[1], "epc.resprout_leaf_carbon", c(0.0005, 0.01, n)),
  list(input_hdr_list$stratum_def[1], "epc.alloc_frootc_leafc", c(0.2, 1.5, n)),
  list(input_hdr_list$stratum_def[1], "epc.frootc_crootc", c(0.53, 0.78, n)),
  list(input_hdr_list$stratum_def[1], "epc.alloc_prop_day_growth", c(0.4, 0.6, n)),
  list(input_hdr_list$stratum_def[1], "epc.day_leafon", c(274, 305, n)),
  list(input_hdr_list$stratum_def[1], "epc.day_leafoff", c(60, 90, n)),
  list(input_hdr_list$stratum_def[1], "epc.ndays_expand", c(45, 57, n)),
  list(input_hdr_list$stratum_def[1], "epc.ndays_litfall", c(43, 60, n))
)


#########################################################################
# Generate table for def file param changes
#########################################################################

if (is.null(input_def_list[1]) == FALSE){

  # select method of parameter randomisation 
  method = "monte_carlo"
  
  # set up def parameters as list 
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
    
    option_sets_def_par1[[bb]] <- make_option_set_combinations(input_list=input_def_change_par, parameter_method=method)
    # Attach group ID to option_sets_def_par
    tmp <- seq_along(option_sets_def_par1[[bb]][[1]])
    option_sets_def_par1[[bb]] <- bind_cols(option_sets_def_par1[[bb]], group_id = tmp)
    
    option_sets_def_par2[[bb]] <- make_option_set_combinations(input_list=input_def_change_par, parameter_method=method)
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
    option_sets_def_par <- list(sobol_tmp[,c(1:2,36)], 
                                sobol_tmp[,3:36])

    names(option_sets_def_par) <- input_def_file_unique
}
#########################################################################
# create option_sets_all to be output as csv

# if dated seq is NULL (which it should be because it has to do with fire?)
option_sets_dated_seq <- data.frame(dated_id = 0)

# option sets all combines veg params with rhessys command line input
option_sets_all <- make_all_option_table(method,
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

  # Save the sobol object as an R file 
  saveRDS(run_sob, "../out/2024jansenoutput.rds")
  
  # save the inputs used in the runs 
  write.csv(option_sets_all, "../out/rhessys_all_options.csv", sep="", row.names = FALSE, quote=FALSE)

  # get number of runs 
  option_sets_rhessys_rows <- nrow(option_sets_all)

  system.time(
# Generate new def files for each run
# aa = def file being changed, in this case it's only 1
# bb is ID, so also applies to what run #
   # for (aa in seq_along(option_sets_def_par)){

      # Step through each unique parameter set and make def file
      for (bb in seq_along(option_sets_def_par[[1]]$group_id)){

        # for each run change the parameters in the def file 
        for(aa in seq_along(option_sets_def_par)){
          change_def_file(def_file = names(option_sets_def_par)[aa],
                          par_sets = as_tibble(dplyr::select(option_sets_def_par[[aa]], -group_id))[bb,],
                          file_name_ext = "")
          
          print(paste("New def file written for file", names(option_sets_def_par)[aa]))
        }

        print(paste("----------------- Run", bb ,"of", option_sets_rhessys_rows, "-----------------"))

        # Call RHESSys for spinup
        rhessys_command(rhessys_version = input_rhessys$rhessys_version,
                        world_file = input_rhessys$world_file,
                        world_hdr_file = input_rhessys$world_hdr_prefix,
                        tec_file = "../tecfiles/tec.spinup",
                        flow_file = input_rhessys$flow_file,
                        start_date = "1980 10 1 1",
                        end_date = "2010 10 1 1",
                        output_file = "test",
                        input_parameters = "-s 0.036294 359.485800 -sv 0.036294 359.485800 -gw 0.346205 0.416299",
                        command_options = input_rhessys$command_options)

         # move spinup file 
        cmd = sprintf("mv ../worldfiles/Veg.world.0.Y2010M9D30H1.state ../worldfiles/OakWorld.spun70")
       system(cmd)
      # 
       print(paste("Spin up complete"))

      # for run with output for study period 
       rhessys_command(rhessys_version = input_rhessys$rhessys_version,
                       world_file = "../worldfiles/OakWorld.spun70",
                       world_hdr_file = input_rhessys$world_hdr_prefix,
                       tec_file = "../tecfiles/tec.coast",
                       flow_file = input_rhessys$flow_file,
                       start_date = "2010 10 1 1",
                       end_date = "2020 10 1 1",
                       output_file = "tmp",
                       input_parameters = "-s 0.036294 359.485800 -sv 0.036294 359.485800 -gw 0.346205 0.416299",
                       command_options = "-g -climrepeat -vmort_off -of of_sensitivity.yml")


        # Process RHESSys output
          read = read.csv("../out/SA_strat.csv") 
          
          # summarise by year to get 2011 value 
          out_yr = read %>% group_by(year) %>%
            summarize_at(vars(epv.proj_lai, cs.totalc, epv.height), 
                         list(mean)) %>% 
            mutate(run = bb) %>% 
            filter(year==2011 | year ==2014 | year==2017)
          
          # summarise by the month with the highest LAI, on average 
          out_mo = read %>% 
            group_by(year, month) %>% 
            summarize(lai=max(epv.proj_lai)) %>% 
            group_by(month) %>% 
            summarize(lai = mean(lai)) %>% 
            dplyr::filter(lai==max(lai)) %>% 
            mutate(run=bb)
          
          # save to data frame with runs as a column 
          if(bb==1){
            write.table(as.data.frame(out_yr), 
                        file=paste(input_rhessys$output_folder, "SA_ann.csv", sep="/"), 
                        append=F, row.names = FALSE, quote=FALSE, col.names=TRUE)
            write.table(as.data.frame(out_mo), 
                        file=paste(input_rhessys$output_folder, "SA_month.csv", sep="/"), 
                        append=F, row.names = FALSE, quote=FALSE, col.names=TRUE)
          }else{
            write.table(as.data.frame(out_yr), 
                        file=paste(input_rhessys$output_folder, "SA_ann.csv", sep="/"), append=T, col.names = FALSE, row.names = FALSE)
            write.table(as.data.frame(out_mo), 
                        file=paste(input_rhessys$output_folder, "SA_month.csv", sep="/"), append=T, col.names = FALSE, row.names = FALSE)
            
            
            }
         }
        )
      }
  

     
  
  
  






