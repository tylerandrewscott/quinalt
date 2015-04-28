
library(RcppRoll)

obs.data = all.params.spdf@data

tempAll = huc8_data %>% dplyr::group_by(HUC8) %>% dplyr::arrange(HUC8,Abs.Month) %>%
  dplyr::mutate_each(funs(cumsum),contains('Total'))
names(tempAll)[grep('Total',names(tempAll))] = paste(names(tempAll)[grep('Total',names(tempAll))],'All',sep='_')

temp12 = huc8_data %>% dplyr::group_by(HUC8) %>% dplyr::arrange(HUC8,Abs.Month) %>%
  dplyr::mutate_each(funs(roll_sumr(.,n=12,fill=0)),contains('Total'))
names(temp12)[grep('Total',names(temp12))] = paste(names(temp12)[grep('Total',names(temp12))],'12',sep='_')

temp36 = huc8_data %>% dplyr::group_by(HUC8) %>% dplyr::arrange(HUC8,Abs.Month) %>%
  dplyr::mutate_each(funs(roll_sumr(.,n=36,fill=0)),contains('Total'))
names(temp36)[grep('Total',names(temp36))] = paste(names(temp36)[grep('Total',names(temp36))],'36',sep='_')

temp60 = huc8_data %>% dplyr::group_by(HUC8) %>% dplyr::arrange(HUC8,Abs.Month) %>%
  dplyr::mutate_each(funs(roll_sumr(.,n=60,fill=0)),contains('Total'))
names(temp60)[grep('Total',names(temp60))] = paste(names(temp60)[grep('Total',names(temp60))],'60',sep='_')

temp = join_all(list(tempAll,temp12,temp36,temp60))

obs.data = join(obs.data,temp)

