# find date of fish capture

datefish <- function(x){
  source("~/Documents/Philippines/Phil_code/conleyte.R")
  leyte <- conleyte()
  # connect anem ids to dive ids
  anem <- leyte %>% tbl("anemones") %>% filter(anem_table_id %in% x) %>% select(dive_table_id, anem_table_id) %>% collect()
  # get date and site
  dive <- leyte %>% tbl("diveinfo") %>% filter(id %in% anem$dive_table_id) %>% select(id, date) %>% collect()
  date <- left_join(anem, dive, by = c("dive_table_id" = "id"))
  return(date)
}
