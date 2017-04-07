CIPb <- function (filename, huc8file, periods) {
    DB <- tbl_df(read.csv(filename, header = TRUE))
    temporalCIPs <- tbl_df(read.csv(huc8file, header = TRUE))
    for (i in 1:length(periods)) {
        data <- filter(DB, year <= periods[i])
        colname <- paste("cip", periods[i], sep = "")
        CIP <- group_by(data, HUC8, sciName) %>% mutate(minyear = min(year)) %>% 
        mutate(time = 2017-minyear) %>% 
        group_by(HUC8, sciName) %>% distinct(sciName, .keep_all = TRUE) %>% 
        group_by(HUC8) %>% mutate(cip = sum(time)) %>% 
        select(HUC8, cip) %>% distinct(HUC8, .keep_all = TRUE)
        colnames(CIP)[2] <- colname
        temporalCIPs <- left_join(temporalCIPs, CIP, by = "HUC8")
    }
    
    temporalCIPs[is.na(temporalCIPs)] <- 0
    temporalCIPs$HUC2 <- substr(temporalCIPs$HUC8, 1, nchar(temporalCIPs$HUC8)-6)
    write.csv(temporalCIPs, "temporalCIPb.csv", row.names = FALSE)
}
