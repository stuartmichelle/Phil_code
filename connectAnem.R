# this code is to match up existing anemones with their old anem IDs

source("conleyte.R")
source("writeleyte.R")

# connect to the database
leyte <- conleyte()

# pull in all of the anemone data
anem <- leyte %>% tbl("anemones") %>% collect()

# make a backup of the table in case anything goes wrong
write.csv(anem, file = paste(Sys.time(), "_anembackup.csv", sep = ""))


# find all of the anemones that have a value in the oldAnemID column
replace <- anem %>% filter(!is.na(old_anem_id))

# note the future anem ID in the old anem ID row
# test i <- 1
anem$futureAnemId <- NA
for(i in 1:nrow(replace)){
  anem$futureAnemId[which(anem$AnemID == replace$old_anem_id[i])] <- replace$AnemID[i]
}

# add this data to the database
leytes <- writeleyte()

# Send data to database
dbWriteTable(leyte,"anemones",data.frame(anem), row.names = FALSE, overwrite = TRUE)

dbDisconnect(leyte)
rm(leyte)

