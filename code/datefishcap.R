# which date was a sample_id captured?

datefishcap <- function(x){
  fish <- leyte %>% tbl("clownfish") %>% filter(sample_id %in% x) %>% select(anem_table_id, sample_id) %>% collect()
  anem <- leyte %>% tbl("anemones") %>% filter(anem_table_id %in% fish$anem_table_id) %>% select(dive_table_id, anem_table_id) %>% collect()
  day <- leyte %>% tbl("diveinfo") %>% filter(id %in% anem$dive_table_id) %>% select(date, id) %>% collect()
  day <- left_join(day, anem, by = c("id" = "dive_table_id"))
  day <- left_join(day, fish, by = "anem_table_id") %>% 
    select(-anem_table_id, -id)
  return(day)
}
