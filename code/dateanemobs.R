# which date was an anemone observed based on clownfish table?

#' This function allows you to find the date based on the anem_table_id
#' @param table$anem_table_id - Where are the anem_table_id's located?
#' @keywords 
#' @export
#' @examples
#' dateanem(table$anem_table_id)

dateanem <- function(x){
  anem <- leyte %>% tbl("anemones") %>% filter(anem_table_id %in% x) %>% select(dive_table_id, anem_table_id) %>% collect()
  day <- leyte %>% tbl("diveinfo") %>% filter(id %in% anem$dive_table_id) %>% select(date, id) %>% collect()
  day <- left_join(day, anem, by = c("id" = "dive_table_id"))
  return(day)
}
