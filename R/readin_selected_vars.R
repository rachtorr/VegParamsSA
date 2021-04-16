# script for reading in output from changing def file and outputing from
# select_output_variables_R.R

# first make list of variables that were output in senstivity 
# vars <- c("lai","height","plantc","leafc","live_crootc","live_stemc")
# n = number of simulations ran 
# days = number of days of output (need to put in automatic way to do this if it's unknown)
# dir = directory where the output is in relation to working directory 

# set up empty df with days in year and each run #
readin_selected_vars <- function(vars, dir){
  
  readout <- function(var){
    p = fread(paste(dir,var, sep = "/"))
    
    if(names(p)[5]=="hillID"){
      tomelt <- as.data.frame(p) %>% 
        dplyr::select(-basinID,-hillID,-zoneID,-patchID,-stratumID) 
    }else{
      tomelt <- as.data.frame(p) %>% 
        dplyr::select(-basinID)
   }
    
    toprint <- reshape2::melt(as.data.frame(tomelt), id.var=c('day','month','year'))
    
  }
  
  todf <- lapply(vars, FUN=readout)
  lap <- lapply(todf, "[[", 5)
  df <- as.data.frame(do.call(cbind, lap))
  colnames(df) <- vars
  
  dates <- as.data.frame(todf[[1]][,1:4])
  toret <- cbind(dates, df)
  
  spltrun <- str_split(toret$variable, pattern="_")
  toret$run = as.numeric(lapply(spltrun, "[[", 2))
  
  toret$date = as.Date(paste(toret$year, toret$month, toret$day, sep="-"))
  
  return(as_tibble(toret))
}


