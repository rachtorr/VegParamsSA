# decid def files for Janet 
# creating def file from the median of the parameter sets for PLRA (cal sycamore) - however these were used with evergreen flag, because in SB not all leaves are dropped 
library(RHESSysIOinR)
setwd("Desktop/VegParamsSA/")

params <- read_csv("R/tree_outputs/plra_params.csv")
params <- params %>% dplyr::select(-X, -X1)

### which def file is being changed
def_soil = "defs/shallow_NT.def"
def_dec  = "defs/veg_eucalypt.def"

### convert back to option_sets_def_par so that def file param names are listed as column headers 
p2 <- params[,14:53]

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
names(option_sets_def_par) <- c(def_soil, def_dec)

# get median of parametrs 
veg_med = summarize_all(veg, median) %>% dplyr::select(-group_id)

# write to def file
change_def_file(def_file = def_dec,
                par_sets = veg_med,
                file_name_ext = "med")
