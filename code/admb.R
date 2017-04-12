# make a file for the admb code
source("conleyte.R")
source("datefish.R")

# get list of fish
leyte <- conleyte()
fish <- leyte %>% tbl("clownfish") %>% filter(!is.na(capid)) %>% select(capid, size, sample_id, anem_table_id) %>% collect()

# get date for fish because need time at large
date <- datefish(fish$anem_table_id)
date$date <- as.Date(date$date)
fish <- left_join(fish, date, by = "anem_table_id")

# remove extra columns
fish$anem_table_id <- NULL
fish$dive_table_id <- NULL
# 
# # eliminate fish for which there are incomplete data
# fish$capid[is.na(fish$size)] # should be 1, 9
# fish$capid[fish$capid == 1] <- NA
# fish$capid[fish$capid == 9] <- NA
# fish <- fish[!is.na(fish$capid), ]
# 
# # remove the elementary school fish from the analysis because it was caught twice in the same day
# fish <- fish[fish$name != "Elementary School", ]

# populate an L1 and L2, growth increment and TAL column for recaptured fish
recapture <- data.frame()
for(i in 1:nrow(fish)){
  X <- fish[which(fish$capid == fish$capid[i]), ]
  X$L1 <- min(X$size)
  X$L2 <- max(X$size)
  X$T1 <- min(X$date)
  X$T2 <- max(X$date)
  X$tal <- max(X$date) - min(X$date)
  X$growinc <- X$L2[1] - X$L1[1]
  recapture <- rbind(recapture, X[1,])
}

# remove duplicate rows
recapture <- dplyr::distinct(recapture)

# convert tal from days to portions of year
recapture$tal <- recapture$tal/365


# create an L1L2 file for BET-grow in admb
lens <- recapture[ , c("L1", "L2", "growinc", "tal")]

#### contains NAs ########

write.table(lens, file = paste("admb/",Sys.Date(), "L1L2.tsv", sep = ""), row.names=FALSE, quote=FALSE, col.names=FALSE) # won't let me use header=FALSE - use col.names instead of header
