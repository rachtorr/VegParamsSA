# creating relative resilience index based on Lloret et al. 2011 
# df is the data frame, must include column wy
# var is the column in the data frame you want to base the indices on 
# preyr is the baseline, or pre drought year
# function is how you want to group the variable. set to mean (for lai), but could also be sum for npp
# drtyr is incase you want to set your own drt yr

relativeresilience <- function(df, var, preyr, fun=mean, drtyr=NULL)
  {
  ann <- df %>% 
  group_by(wy, run, code) %>% 
    summarize_at(vars(var), funs(mean)) 
  
  ann$V1 <- ann[[var]]
  
  if(is.null(drtyr)){
    ann$resist = ann$V1/ann$V1[ann$wy==preyr]
    drtyr = ann$wy[ann$resist==min(ann$resist)]
  }
  
  ann$recover = ann$V1/ann$V1[ann$wy==drtyr]
  ann$resil = ann$V1/ann$V1[ann$wy==preyr]
  ann$rr = (ann$V1 - ann$V1[ann$wy==drtyr])/ann$V1[ann$wy==preyr]
  
  togo <- ann %>% group_by(run, code) %>% mutate(resist = V1[wy==drtyr]/V1[wy==preyr]) %>% 
    dplyr::select(-V1)
  
  return(list(togo,drtyr))
}


