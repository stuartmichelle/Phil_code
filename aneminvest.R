# invesitage a group of anemones

source("conleyte.R")
leyte <- conleyte()

# 2016 anem data ----------------------------------------------------------

anem <- leyte %>% tbl("anemones") %>% filter(anem_id == 286 | anem_id == 2313) %>% collect()

# clownfish that were recaptured, do their anems match up?
suppressWarnings(dive <- leyte %>% tbl("diveinfo") %>% filter(name == "Visca") %>% collect())
anems <- leyte %>% tbl("anemones") %>% filter (dive_table_id %in% dive$id) %>% select(anem_table_id, anemobs, anem_id, old_anem_id) %>% collect()
fish <- leyte %>% tbl("clownfish") %>% filter(anem_table_id %in% anems$anem_table_id & !is.na(capid)) %>%  select(sample_id, capid, anem_table_id, recap, tagid) %>% collect()

fish <- left_join(fish, anems, by = "anem_table_id")
