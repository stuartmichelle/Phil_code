# a script to find repeat tag numbers in the data base
source("conleyte.R")

leyte <- conleyte()

# find multiple occurances of tagids
library(dplyr)
dups <- leyte  %>% tbl("clownfish") %>% collect()
dups <- dups %>% group_by(tagid)  %>% filter(tagid, n()>1)  %>% select(anem_table_id, size, sample_id, col, tagid, capid)
dups$tagid <- as.character(dups$tagid)

# add metadata
c1 <- leyte  %>% tbl("diveinfo") %>% select(id, name)
c2 <- leyte %>% tbl("anemones") %>% select(anem_table_id, dive_table_id)
anem <- left_join(c2, c1, by = c("dive_table_id" = "id"), copy = T) %>% select(anem_table_id, name)

dups <- left_join(dups, anem, by = "anem_table_id", copy = T)  %>% arrange(tagid)
