library(data.table)
library(tidyverse)
library(gridExtra)
source("../R/ch1/readin_selected_vars.R")
source("~/Documents/GitHub/climfiles/mkwy.R")
source("../R/ch1/get_filtered_params.R")

### load in RHESSys output 
vars = c("lai",
         "plantc",
         "mresp",
         "gresp",
         "psn_to_cpool",
         "height") 


# load in rs data
vis <- read.csv("../data/tree_and_turfgrass_monroe2_selected_weightedmeanVIs.csv")

lai_data <- read.csv("../data/weighted_tree_summary.csv")



############## - start - ############################ 
# do it for eugl- change where it is reading from 
params_ev <- read.csv("../out/ch1/decid/allsim/veg.eugl_all_options.csv")
params_ev$run <- params_ev$...defs.shallow.def.group_id
dir= "../out/ch1/decid/allsim"
tree=list(34, "EUGL")
test <- get_filtered_params(dir=dir, vars=vars, params=params_ev, 
                                ndvi=vis, lai_data=data, spcode=tree, 
                                ht=60,
                                #month_range = c(4,11),
                                plot=T,
                                write=F,
                                carbon_filt=T)
do.call("grid.arrange", c(test$plots, ncol=2))
test$count

# for plra
params_ev <- read.csv("../out/ch1/decid/allsim/veg.plra.pt2_all_options.csv")
params_ev$run <- params_ev$...defs.shallow.def.group_id
dir= "../out/ch1/decid/allsim/"
tree=list(74, "PLRA")
plra <- get_filtered_params(dir=dir, vars=vars, params=params_ev, 
                            ndvi=vis, lai_data=data, spcode=tree, 
                            ht=30,
                            plot=T,
                            write=F,
                            carbon_filt=F)
do.call("grid.arrange", c(plra$plots, ncol=2))
plra$count

############## - quag - ############################
params_ev <- read.csv("../out/rhessys_all_options.csv")
params_ev$run <- params_ev$...defs.shallow.def.group_id
dir= "../out/"
tree=list(81, "QUAG")
quag <- get_filtered_params(dir=dir, dfann="SA_ann.csv", dfmonth = "SA_month.csv", 
                            ndvi=vis, lai_data=lai_data, spcode=tree, 
                            ht=25,
                            month_range=c(4, 12),
                            plot=T,
                            write=F,
                            carbon_filt=F)
do.call("grid.arrange", c(quag$plots, ncol=2))
quag$count



# for piun
params_ev <- read.csv("../out/ch1/evergreen/allsim/quag_all_options.csv")
params_ev$run <- params_ev$...defs.shallow.def.group_id
dir= "../out/ch1/evergreen/allsim/"
tree=list(73, "PIUN")
piun <- get_filtered_params(dir=dir, vars=vars, params=params_ev, 
                            ndvi=vis, lai_data=data, spcode=tree, 
                            ht=13,
                            plot=T,
                            write=T,
                            month_range = c(3,8),
                            carbon_filt=F)
do.call("grid.arrange", c(piun$plots, ncol=2))
piun$count

ev <- rbind(piun$out_full, quag$out_full)
ev_p <- inner_join(ev, params_ev, by="run")

