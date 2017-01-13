# a script to find samples, map them, and make note of seq results

source("conleyte.R")
source("../../myRcode/Laboratory/R/conlabor.R")
labor <- conlabor()
leyte <- conleyte()

# find all samples collected in 2012
suppressWarnings(dive <- leyte %>% tbl("diveinfo")  %>% filter(date > "2012-01-01" & date < "2013-01-01")  %>% select(id, date) %>% collect())
anem <- leyte %>% tbl("anemones") %>% filter(dive_table_id %in% dive$id) %>% select(anem_table_id, ObsTime, dive_table_id) %>% collect()
samples <- left_join(anem, dive, by = c("dive_table_id" = "id"))
fish <- leyte %>% tbl("clownfish") %>% filter(anem_table_id %in% anem$anem_table_id & !is.na(sample_id)) %>% select(anem_table_id, sample_id) %>% collect()
samples <- left_join(samples, fish, by = "anem_table_id")
latlong <- leyte %>% tbl("GPX") %>% filter(year == 2012) %>% collect()
latlong$date <- paste(latlong$year, formatC(latlong$month, digits = 1, flag = 0), formatC(latlong$day, digits = 1, flag = 0), sep = "-")

latlong$test <- latlong$hour + 8

latlong$time <- paste(latlong$date, " ",latlong$test, ":", latlong$min, ":00", sep = "")
samples$time <- paste(samples$date, " ", samples$ObsTime, sep = "")

samples <- left_join(samples, latlong, by = "time")
samples <- samples[ , c("sample_id", "lat", "long")]
samples <- samples[!is.na(samples$sample_id), ]
samples <- distinct(samples, sample_id)
samples <- arrange(samples, sample_id)
twelve <- samples[!is.na(samples$lat), ]
write.csv(twelve, file = "map_2012.csv") # import into QGIS


# find all samples collected in 2013
suppressWarnings(dive <- leyte %>% tbl("diveinfo")  %>% filter(date > "2013-01-01" & date < "2014-01-01")  %>% select(id, date) %>% collect())
anem <- leyte %>% tbl("anemones") %>% filter(dive_table_id %in% dive$id) %>% select(anem_table_id, ObsTime, dive_table_id) %>% collect()
samples <- left_join(anem, dive, by = c("dive_table_id" = "id"))
fish <- leyte %>% tbl("clownfish") %>% filter(anem_table_id %in% anem$anem_table_id & !is.na(sample_id)) %>% select(anem_table_id, sample_id) %>% collect()
samples <- left_join(samples, fish, by = "anem_table_id")
latlong <- leyte %>% tbl("GPX") %>% filter(year == 2013) %>% collect()
latlong$date <- paste(latlong$year, formatC(latlong$month, digits = 1, flag = 0), formatC(latlong$day, digits = 1, flag = 0), sep = "-")

latlong$test <- latlong$hour + 8

latlong$time <- paste(latlong$date, " ",latlong$test, ":", latlong$min, ":00", sep = "")
samples$time <- paste(samples$date, " ", samples$ObsTime, sep = "")

samples <- left_join(samples, latlong, by = "time")
samples <- samples[ , c("sample_id", "lat", "long")]
samples <- samples[!is.na(samples$sample_id), ]
samples <- distinct(samples, sample_id)
samples <- arrange(samples, sample_id)
thirteen <- samples[!is.na(samples$lat), ]
write.csv(thirteen, file = "map_2013.csv") # import into QGIS

# find all samples collected in 2014
suppressWarnings(dive <- leyte %>% tbl("diveinfo")  %>% filter(date > "2014-01-01" & date < "2015-01-01")  %>% select(id, date) %>% collect())
anem <- leyte %>% tbl("anemones") %>% filter(dive_table_id %in% dive$id) %>% select(anem_table_id, ObsTime, dive_table_id) %>% collect()
samples <- left_join(anem, dive, by = c("dive_table_id" = "id"))
fish <- leyte %>% tbl("clownfish") %>% filter(anem_table_id %in% anem$anem_table_id & !is.na(sample_id)) %>% select(anem_table_id, sample_id) %>% collect()
samples <- left_join(samples, fish, by = "anem_table_id")
latlong <- leyte %>% tbl("GPX") %>% filter(year == 2014) %>% collect()
latlong$date <- paste(latlong$year, formatC(latlong$month, digits = 1, flag = 0), formatC(latlong$day, digits = 1, flag = 0), sep = "-")

latlong$test <- latlong$hour + 8

latlong$time <- paste(latlong$date, " ",latlong$test, ":", latlong$min, ":00", sep = "")
samples$time <- paste(samples$date, " ", samples$ObsTime, sep = "")

samples <- left_join(samples, latlong, by = "time")
samples <- samples[ , c("sample_id", "lat", "long")]
samples <- samples[!is.na(samples$sample_id), ]
samples <- distinct(samples, sample_id)
samples <- arrange(samples, sample_id)
fourteen <- samples[!is.na(samples$lat), ]
write.csv(fourteen, file = "map_2014.csv") # import into QGIS

# find all samples collected in 2015
suppressWarnings(dive <- leyte %>% tbl("diveinfo")  %>% filter(date > "2015-01-01" & date < "2016-01-01")  %>% select(id, date) %>% collect())
anem <- leyte %>% tbl("anemones") %>% filter(dive_table_id %in% dive$id) %>% select(anem_table_id, ObsTime, dive_table_id) %>% collect()
samples <- left_join(anem, dive, by = c("dive_table_id" = "id"))
fish <- leyte %>% tbl("clownfish") %>% filter(anem_table_id %in% anem$anem_table_id & !is.na(sample_id)) %>% select(anem_table_id, sample_id) %>% collect()
samples <- left_join(samples, fish, by = "anem_table_id")
latlong <- leyte %>% tbl("GPX") %>% filter(year == 2015) %>% collect()
latlong$date <- paste(latlong$year, formatC(latlong$month, digits = 1, flag = 0), formatC(latlong$day, digits = 1, flag = 0), sep = "-")

latlong$test <- latlong$hour + 8

latlong$time <- paste(latlong$date, " ",latlong$test, ":", latlong$min, ":00", sep = "")
samples$time <- paste(samples$date, " ", samples$ObsTime, sep = "")

samples <- left_join(samples, latlong, by = "time")
samples <- samples[ , c("sample_id", "lat", "long")]
samples <- samples[!is.na(samples$sample_id), ]
samples <- distinct(samples, sample_id)
samples <- arrange(samples, sample_id)
fifteen <- samples[!is.na(samples$lat), ]
write.csv(fifteen, file = "map_2015.csv") # import into QGIS

# find all samples collected in 2016
suppressWarnings(dive <- leyte %>% tbl("diveinfo")  %>% filter(date > "2016-01-01" & date < "2017-01-01")  %>% select(id, date) %>% collect())
anem <- leyte %>% tbl("anemones") %>% filter(dive_table_id %in% dive$id) %>% select(anem_table_id, ObsTime, dive_table_id) %>% collect()
samples <- left_join(anem, dive, by = c("dive_table_id" = "id"))
fish <- leyte %>% tbl("clownfish") %>% filter(anem_table_id %in% anem$anem_table_id & !is.na(sample_id)) %>% select(anem_table_id, sample_id) %>% collect()
samples <- left_join(samples, fish, by = "anem_table_id")
latlong <- leyte %>% tbl("GPX") %>% filter(year == 2016) %>% collect()
latlong$date <- paste(latlong$year, formatC(latlong$month, digits = 1, flag = 0), formatC(latlong$day, digits = 1, flag = 0), sep = "-")

latlong$test <- latlong$hour + 8

latlong$time <- paste(latlong$date, " ",latlong$test, ":", latlong$min, ":00", sep = "")
samples$time <- paste(samples$date, " ", samples$ObsTime, sep = "")

samples <- left_join(samples, latlong, by = "time")
samples <- samples[ , c("sample_id", "lat", "long")]
samples <- samples[!is.na(samples$sample_id), ]
samples <- distinct(samples, sample_id)
samples <- arrange(samples, sample_id)
sixteen <- samples[!is.na(samples$lat), ]
write.csv(sixteen, file = "map_2015.csv") # import into QGIS
