setwd("~/VegParamsSA/R/sobol/")

lai1 <- readRDS("sens_lai_results_evg.rds") %>%
  mutate(tree = "evg")
lai2 <- readRDS("sens_lai_results.rds") %>% 
  mutate(tree="dec")
res1 <- readRDS("sens_res_results_evg.rds") %>% 
  mutate(tree="evg")
res2 <- readRDS("sens_res_results.rds") %>% 
  mutate(tree="dec")

# order by highest Total; keep top 5 

lai1_ord = lai1 %>% dplyr::filter(rank=="T") 
lai1_ord = lai1_ord[order(lai1_ord$original),][35:39, 'param']

lai2_ord = lai2 %>% dplyr::filter(rank=="T") 
lai2_ord = lai2_ord[order(lai2_ord$original),][35:39, 'param']

res1_ord = res1 %>% dplyr::filter(rank=='T')
res1_ord = res1_ord[order(res1_ord$original),][35:39, 'param']

res2_ord = res2 %>% dplyr::filter(rank=='T')
res2_ord = res2_ord[order(res2_ord$original),][35:39, 'param']

# combine LAI and get the parameters 
lai_p <- unique(c(lai1_ord, lai2_ord))
lai_table <- rbind(
  lai1[lai1$param %in% lai_p,],
  lai2[lai2$param %in% lai_p,]
)
lai_table2 <- lai_table[order(lai_table$tree, lai_table$param),]

# combine resilience and get parameters 
res_p <- unique(c(res1_ord, res2_ord))
res_table <- rbind(
  res1[res1$param %in% res_p,],
  res2[res2$param %in% res_p,]
)
res_table2 <- res_table[order(res_table$tree, res_table$param),]

# combine for outputting to csv

final_table = rbind(lai_table2 %>% mutate(table="LAI"),
                    res_table2 %>% mutate(table="resil"))

write.csv(final_table, "topSA.table.csv")
