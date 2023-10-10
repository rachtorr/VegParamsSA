# functions to help run scenarios with surface msr 

### --- change_wf_area-----
# takes input of area percent and changes that worldfiles 
# must know order of patches within patch family 

change_wf_area <- function(worldfile, pct_area_list, patchids){
  area_amt = 133.6961*pct_area_list
  world_rd = trimws(readLines(worldfile))
  world_df = data.frame(world_rd)
  tcols = separate(data=world_df, col=world_rd, into=c("left","right"), sep="\\s+")
  tcols$left[tcols$right == "pct_family_area"] <- pct_area_list
  tcols$left[tcols$right == "area"][2:3] <- area_amt
  write.table(tcols, worldfile, 
              row.names = FALSE, 
              col.names = FALSE, 
              quote = FALSE, 
              sep="     ")
  
  print(paste("changed", patchids, "area in worldfile to", pct_area_list))
  
}



### --- function for reading in basin output and plotting --- ### 
watbal_wrapper = function(file){
  tmp = read.csv(file)
  bas = tmp %>% mutate(canopy_snow_stored = snow_stored,
                 canopy_rain_stored = rain_stored) %>% 
    watbal_basin_of() %>% 
    mkdate() 
  
  plot = ggplot(bas) + geom_point(aes(x=date, y=watbal))
  
  return(list(df = bas, plot=plot))
}



### --- function for running water balance for single patch --- ###
patch_balance_of = function (pd, cd){
  
  pd$trans_sat = pd$transpiration_sat_zone
  pd$trans_unsat = pd$transpiration_unsat_zone
  qouta = ifelse(pd$streamflow > 0, pd$streamflow, pd$Qout)
  pd$watbal.tmp = with(pd, rain_throughfall + Qin - 
                         qouta -  
                         trans_sat - 
                         trans_unsat - 
                         evaporation - evaporation_surf)
  pd$sd = with(pd, sat_deficit - 
                 rz_storage - 
                 unsat_storage)
  cd$weighted_snow_stored = cd$snow_stored*cd$cover_fraction
  cd$weighted_rain_stored = cd$rain_stored*cd$cover_fraction 
  tmp = cd %>% group_by(zoneID, hillID, basinID, patchID, day, 
                        month, year) %>% summarize(can_snow_stored = sum(weighted_snow_stored), 
                                                   can_rain_stored = sum(weighted_rain_stored))
  pd = left_join(pd, tmp[, c("basinID", "hillID", "zoneID", 
                             "patchID", "day", "month", "year", "can_snow_stored", 
                             "can_rain_stored")])
  pd$can_water_stored = pd$can_rain_stored + pd$can_snow_stored
  tmp = diff(pd$sd)
  tmp = c(0, tmp)
  pd$sddiff = tmp
  tmp = diff(pd$snow_throughfall)
  tmp = c(0, tmp)
  pd$snodiff = tmp
  tmp = diff(pd$detention_store)
  tmp = c(0, tmp)
  pd$detdiff = tmp
  tmp = diff(pd$litter.rain_stored)
  tmp = c(0, tmp)
  pd$litdiff = tmp
  tmp = diff(pd$can_water_stored)
  tmp = c(0, tmp)
  pd$candiff = tmp
  pd$watbal = with(pd, watbal.tmp + sddiff - snodiff - detdiff - 
                     litdiff - candiff)
  pd$watbal[1] = 0
  return(pd)
}
