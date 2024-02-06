
get_filtered_params <- function(dir, vars, params, 
                                ndvi, lai_data, spcode, 
                                ht=NULL,
                                month_range=NULL,
                                plot=F,
                                write=F,
                                carbon_filt=F){
  
  # read in output 
  read <- readin_selected_vars(vars=vars, dir=dir)
  read <- mkwy(read)
  
  # group by year
  grouped <- read %>% group_by(year, run) %>%
    summarize_at(vars(lai, plantc, height), funs(mean))
  
  # select species from NDVI data 
  ndvi_data <- vis %>% dplyr::filter(sp_code==
                                       spcode[2]) %>%
    dplyr::select(year, NDVI, ID) %>% 
    pivot_wider(names_from='year', names_prefix="y", values_from = NDVI) 
  
  ndvi_data$decline <- (ndvi_data$y2011 - ndvi_data$y2014)/ndvi_data$y2011
  ndvi_data$recover <- (ndvi_data$y2011 - ndvi_data$y2017)/ndvi_data$y2011
  
  # select species from LAI lidar data
  quag <- lai_data[lai_data$class == spcode[1],]
  # set bounds
  low_lai <- quag$mean_wt_lai - 2*quag$sd_wt_lai
  high_lai <- quag$mean_wt_lai + 2*quag$sd_wt_lai
  low_carb <- quag$mean_wt_carb - 2*quag$sd_wt_carb
  high_carb <- quag$mean_wt_carb + 2*quag$sd_wt_carb
  
  # filter by values in 2011 
  read_ann <- grouped %>% dplyr::filter(year == 2011) %>%
    dplyr::filter(lai > low_lai & lai < high_lai)
  
  if(!is.null(ht)){
   read_ann <- read_ann %>% dplyr::filter(height < ht)  
  }

  if(carbon_filt==T){
    read_ann <- read_ann %>% dplyr::filter(plantc > low_carb & plantc < high_carb)
  }
    
  first<-nrow(read_ann)
  
  read_ann2 <- read[read$run %in% read_ann$run,]
  
  # filter by differences compared to 2011
  read_chng <- read_ann2 %>% 
    dplyr::filter(year == 2011 | year== 2014 | year == 2017)  %>% 
    pivot_wider(id_cols='run', names_from='year', values_from='lai', values_fn=mean) %>% 
    mutate(change1 = (`2011`-`2014`)/`2011`) %>% 
    mutate(change2 = (`2011`-`2017`)/`2011`) %>%
    dplyr::filter(change1 > change2)
  second<-nrow(read_chng)
  
  read_ann3 <- read[read$run %in% read_chng$run,]
  
  # filter by month where LAI is highest based on NDVI (David ch 2)
  if(!is.null(month_range)){
    maxl <- read_ann3 %>% group_by(month, year, run) %>%
      summarize(lai=max(lai)) %>% 
      group_by(month, run) %>% 
      summarize(lai = mean(lai)) %>% 
      group_by(run) %>% dplyr::filter(lai==max(lai))
    hist(maxl$month)
    
    veg_mo <- maxl %>% dplyr::filter(month<=month_range[1] | month>=month_range[2])
    third<-nrow(veg_mo)
    read_veg <- read[read$run %in% veg_mo$run,]
  }else{
    read_veg=read_ann3
    third<-NA
  }
  
  if(nrow(read_veg)>0){
    
  # filter to be within range of % NDVI changes
  veg_lai <- read_veg %>% 
    dplyr::filter(wy == 2011 | wy== 2014 | wy == 2017)  %>% 
    pivot_wider(id_cols='run', names_from='year', values_from='lai', values_fn=mean) %>% 
    mutate(change1 = (`2011`-`2014`)/`2011`) %>% 
    mutate(change2 = (`2011`-`2017`)/`2011`) %>% 
    dplyr::filter(change2 < (mean(ndvi_data$recover)+sd(ndvi_data$recover))) %>%
    dplyr::filter(change2 > (mean(ndvi_data$recover)-sd(ndvi_data$recover))) %>%
    dplyr::filter(change1 < (mean(ndvi_data$decline)+sd(ndvi_data$decline))) %>% 
    dplyr::filter(change1 > (mean(ndvi_data$decline)-sd(ndvi_data$decline))) 
  fourth<-nrow(veg_lai)
  count <- list(first, second, third, fourth)
  }else{
    veg_lai = read_ann3
    count <- list(first, second, third)
  }
  
  if(nrow(veg_lai)==0){
    veg_lai=read_chng
  }
  
  if(plot==T){
    
    nd_pi <- ggplot(ndvi_data) + geom_density(aes(x=y2011, col='11')) +
      geom_density(aes(x=y2014, col='14')) + 
      geom_density(aes(x=y2017, col='17')) + 
      ggtitle(paste("change in NDVI for",spcode[2],"(n=50)"))
    
    density_pica <- ggplot(veg_lai) + geom_density(aes(x=`2011`, col='11')) + 
      geom_density(aes(x=`2014`, col='14')) + 
      geom_density(aes(x=`2017`, col='17')) + 
      ggtitle(paste("change in LAI for", spcode[2],"n=", nrow(read_chng)))
  
    ### plotting the change from 2011-2014 
    rd1 <- ggplot(read_chng) + geom_density(aes(x=change1, col='rhessys')) +
      geom_density(data=ndvi_data, aes(x=decline, col='ndvi')) + 
      geom_density(data=veg_lai, aes(x=change1, col='veg_lai')) + 
      ggtitle("change in LAI and NDVI 2011-2014")
  
    rd2 <- ggplot(read_chng) + geom_density(aes(x=change2, col='rhessys')) +
      geom_density(data=ndvi_data, aes(x=recover, col='ndvi')) + 
      geom_density(data=veg_lai, aes(x=change2, col='veg_lai')) + 
      ggtitle("change in LAI and NDVI 2011-2017")
  
    bx1 <- ggplot() + geom_boxplot(data=ndvi_data, aes(x='ndvi', y=recover, col='ndvi')) + 
      geom_boxplot(data=veg_lai, aes(x='rhessys', y=change2, col='veg_lai')) + ggtitle("resilience")
    
    bx2 <- ggplot() + geom_boxplot(data=ndvi_data, aes(x='ndvi', y=decline, col='ndvi')) + 
      geom_boxplot(data=veg_lai, aes(x='rhessys', y=change1, col='veg_lai')) + ggtitle("resistence")
    
    plots <- list(nd_pi, density_pica, rd1, rd2, bx2, bx1)
    
  }else{
    plots=NA
  }
  
  # save parameters 
  p_filt <- data.frame(params_ev[params_ev$run %in% veg_lai$run,])
  
  if(nrow(veg_lai) > 0) {
    towrite <- read[read$run %in% veg_lai$run,]
  }else{
    towrite <- read_veg
  }
  
  if(write==T){
    # save output
    write.csv(towrite, paste(dir, spcode[2], "_outputs.csv", sep=""))
    # save parameters 
    write.csv(p_filt, paste(dir, spcode[2], "_params.csv", sep=""))
  }
  
  return(list(per_chng=read_chng, out_lai=veg_lai,out_full=towrite, parms=p_filt, count=count, plots=plots))

}

