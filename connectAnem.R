# this code is to match up existing anemones with their old anem IDs

# connect to the database
# open the laboratory database to retrieve sample info
suppressMessages(library(dplyr))
leyte <- src_mysql(dbname = "Leyte", default.file = path.expand("~/myconfig.cnf"), port = 3306, create = F, host = NULL, user = NULL, password = NULL)

# pull in all of the anemone data
anem <- leyte %>% tbl("anemones") %>% collect()

# find all of the anemones that had old tags that were replaced
replace <- anem %>% filter(!is.na(oldAnemID))

# note the future anem ID in the old anem ID row
# test i <- 1
anem$futureAnemId <- NA
for(i in 1:nrow(replace)){
  anem$futureAnemId[which(anem$AnemID == replace$oldAnemID[i])] <- replace$AnemID[i]
}

# add this data to the database
library(RMySQL)
leyte <- dbConnect(MySQL(), dbname="Leyte", default.file = path.expand("~/myconfig.cnf"), port = 3306, create = F, host = NULL, user = NULL, password = NULL)

# Send data to database
dbWriteTable(leyte,"anemones",data.frame(anem), row.names = FALSE, overwrite = TRUE)

dbDisconnect(leyte)
rm(leyte)

