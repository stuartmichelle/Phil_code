# invesitage a group of anemones

source("conleyte.R")
leyte <- conleyte()

# 2016 anem data ----------------------------------------------------------

anems <- leyte %>% tbl("anemones") %>% filter(anem_id == 2128 | anem_id == 2299) %>% select(dive_table_id, anem_id, old_anem_id, anemobs, AnemSpp, AnemDia, NumFish, Notes, ObsTime) %>% collect()
suppressWarnings(dive <- leyte %>% tbl("diveinfo") %>% filter(id %in% anems$dive_table_id) %>% select(id, date, name)  %>% collect())
anems <- left_join(anems, dive, by = c("dive_table_id" = "id"))

# clownfish that were recaptured, do their anems match up?
suppressWarnings(dive <- leyte %>% tbl("diveinfo") %>% filter(name == "Poroc Rose") %>% collect())
anems <- leyte %>% tbl("anemones") %>% filter (dive_table_id %in% dive$id) %>% select(anem_table_id, anemobs, anem_id, old_anem_id) %>% collect()
fish <- leyte %>% tbl("clownfish") %>% filter(anem_table_id %in% anems$anem_table_id & !is.na(capid)) %>%  select(sample_id, capid, anem_table_id, recap, tagid) %>% collect()

fish <- left_join(fish, anems, by = "anem_table_id")
