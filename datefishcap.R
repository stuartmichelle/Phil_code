# which date was a fish captured?

datefishcap <- function(x){
  source("conleyte.R")
  leyte <- conleyte()
  fish <- leyte %>% tbl("clownfish") %>% filter(sample_id == x) %>% select(anem_table_id) %>% collect()
  anem <- leyte %>% tbl("anemones") %>% filter(anem_table_id == fish$anem_table_id) %>% select(dive_table_id) %>% collect()
  day <- leyte %>% tbl("diveinfo") %>% filter(id == anem$dive_table_id) %>% select(date) %>% collect()
  return(day)
}
