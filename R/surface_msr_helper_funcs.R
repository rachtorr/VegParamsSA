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
