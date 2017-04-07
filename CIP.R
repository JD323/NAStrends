test2016 <- group_by(alltaxa2016, HUC8, sciName) %>% mutate(minyear = min(year))
test2016 <- mutate(test2016, time = 2017-minyear)
test2016 <- test2016 %>% group_by(HUC8, sciName) %>% distinct(sciName, .keep_all = TRUE)
test2016 <- test2016 %>% group_by(HUC8) %>% mutate(cip = log10(sum(time)))
cip2016 <- test2016 %>% select(HUC8, cip2016 = cip) %>% distinct(HUC8, .keep_all = TRUE)

cip <- left_join(huc8s, cip1900)
cip <- left_join(cip, cip1930)
cip <- left_join(cip, cip1950)
cip <- left_join(cip, cip1970)
cip <- left_join(cip, cip1990)
cip <- left_join(cip, cip2016)
cip[is.na(cip)] <- 0

cip$HUC2 <- substr(cip$HUC8,1,nchar(cip$HUC8)-6)

write.csv(cip, "temporalCIP.csv", row.names = FALSE)