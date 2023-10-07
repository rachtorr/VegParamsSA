setwd("../")

parm_stratc3 = readRDS("../out/tgrass50/c3/filtered_output.rds") %>% 
  mutate(phen="drt-c3")

parm_stratc4 = readRDS("../out/tgrass50/c4/filtered_output.rds") %>% 
  mutate(phen="drt-c4")

# combine 
allout = rbind(parm_stratc3, parm_stratc4) %>% 
  dplyr::filter(`"veg_parm_ID"`==3) 

allout %>% 
  group_by(`"date"`, phen) %>% 
  summarise(psn = mean(`"cdf.psn_to_cpool"`)) %>% 
  ggplot() + geom_line(aes(x=`"date"`, y=psn, col=phen))

allout %>% 
  group_by(`"yd"`, phen) %>% 
  summarise(psn = mean(`"cdf.psn_to_cpool"`)) %>% 
  ggplot() + geom_line(aes(x=`"yd"`, y=psn, col=phen))

allout %>% 
  group_by(`"yd"`, phen) %>% 
  summarise(trans = mean(`"trans"`)) %>% 
  ggplot() + geom_line(aes(x=`"yd"`, y=trans, col=phen))

allout %>% 
  group_by(`"yd"`, phen) %>% 
  summarise(lai = mean(`"epv.proj_lai"`)) %>% 
  ggplot() + geom_line(aes(x=`"yd"`, y=lai, col=phen))

allout %>% 
  group_by(`"month"`, phen, `"year"`) %>% 
  summarise(psn = sum(`"cdf.psn_to_cpool"`)) %>% 
  group_by(`"month"`, phen) %>% 
  summarise(psn = mean(psn)) %>% 
  ggplot() + geom_line(aes(x=`"month"`, y=psn, col=phen))

allout %>% 
  group_by(`"month"`, phen) %>% 
  summarise(lai = mean(`"epv.proj_lai"`)) %>% 
  ggplot() + geom_line(aes(x=`"month"`, y=lai, col=phen))


allout %>% 
  ggplot() + geom_boxplot(aes(x=phen, y=`"epv.proj_lai"`, col=`"type"`))

allout %>% 
  group_by(phen) %>% 
  ggplot() + geom_boxplot(aes(x=phen, y=`"epv.height"`))

allout %>% 
  group_by(phen) %>% 
  ggplot() + geom_boxplot(aes(x=phen, y=`"rootzone.depth"`))

allout %>% group_by(`"year"`, `"run"`, phen) %>% 
  summarise(trans_ann = sum(`"trans"`)) %>% 
  ggplot() + geom_boxplot(aes(x=phen, y=trans_ann*1000))



c3_parm = readRDS("../mod_data/c3/twostratum/c3_output_parm.rds") %>% 
  pivot_wider(id_cols=c(group_id, height, rz_depth, psn, trans, lai_mn, lai_mx), values_from=value, names_from=name)

c4_parm = readRDS("../out/tgrass50/c4/c4_output_parm.rds") %>% 
  pivot_wider(id_cols=c(group_id, height, rz_depth, psn, trans, lai_mn, lai_mx), values_from=value, names_from=name)

c3_parm_to_bind = c3_parm %>% dplyr::select(-height, -psn, -rz_depth, -lai_mn, -lai_mx, -trans) %>% 
  mutate(type="c3")

c4_parm_to_bind = c4_parm %>% dplyr::select(-height, -psn, -rz_depth, -lai_mn, -lai_mx, -trans) %>% 
  mutate(type="c4")

params = saveRDS(rbind(c3_parm_to_bind, c4_parm_to_bind), "../mod_data/turfgrass_parameters.rds")
