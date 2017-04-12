###Function to calculate the CUmulative richness per huc8
###dependency on dplyr; requires appropriate .csv data files (plant and animal counts
###and huc8 IDs) to be loaded in working directory

calculateCR <- function (filename, huc8file, periods) {
    DB <- tbl_df(read.csv(filename, header = TRUE))
    temporalCRs <- tbl_df(read.csv(huc8file, header = TRUE))
    for (i in 1:length(periods)) {
        data <- filter(DB, year <= periods[i])
        colname <- paste("CR", periods[i], sep = "")
        richness <- group_by(data, HUC8) %>% 
        distinct(sciName, .keep_all = TRUE) %>% 
        mutate(CR = n()) %>% 
        distinct(HUC8, .keep_all = TRUE) %>%
        select(HUC8, CR)
        colnames(richness)[2] <- colname
        temporalCRs <- left_join(temporalCRs, richness, by = "HUC8")
    }
    
    temporalCRs[is.na(temporalCRs)] <- 0
    temporalCRs$HUC2 <- substr(temporalCRs$HUC8, 1, nchar(temporalCRs$HUC8)-6)
    write.csv(temporalCRs, "temporalCR.csv", row.names = FALSE)
}
