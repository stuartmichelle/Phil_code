# which site was an anemone observed based on clownfish table?

#' This function allows you to find the date based on the anem_table_id
#' @param table$anem_table_id - Where are the anem_table_id's located?
#' @keywords 
#' @export
#' @examples
#' siteanem(table$anem_table_id)


siteanem <- function(x){
  source("~/Documents/Philippines/Phil_code/conleyte.R")
  leyte <- conleyte()
  # connect anem ids to dive ids
  anem <- leyte %>% tbl("anemones") %>% filter(anem_table_id %in% x) %>% select(dive_table_id, anem_table_id) %>% collect()
  # get site
  suppressWarnings(dive <- leyte %>% tbl("diveinfo") %>% filter(id %in% anem$dive_table_id) %>% select(id, name) %>% collect())
  site <- left_join(anem, dive, by = c("dive_table_id" = "id"))
  return(site)
}
