# a script to put samples in order of collection - this is for the 2015_05 field
# season where samples were not identified by order of collection but by tag
# number.

source("conleyte.R")

leyte <- conleyte()

# for all of the dives from 2015_05 season
c1 <- leyte %>% tbl("diveinfo")  %>% filter(date > "2015-04-01" & date < "2016-01-01")  %>% select(id, date)

# for all anem obs from those dives
c2 <- leyte %>% tbl("anemones") 
c3 <- left_join(c1, c2, by = c("id" = "dive_table_id"))  %>% select(anem_table_id, ObsTime, date) %>% collect()

# find all fish from that season in order of time collected
c4 <- leyte %>% tbl("clownfish")
fish <- left_join(c3, c4, by = "anem_table_id", copy = T)  %>% select(ObsTime, sample_id, date)
