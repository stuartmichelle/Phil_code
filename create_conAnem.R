# this code is to match up existing anemones with their old anem IDs

source("conleyte.R")
source("writeleyte.R")

# connect to the database
leyte <- conleyte()

# pull in all of the anemone data
anem <- leyte %>% tbl("anemones") %>% collect()

# make a backup of the table in case anything goes wrong
write.csv(anem, file = paste(Sys.time(), "_anembackup.csv", sep = ""))

# find all of the anemones that have a value in the oldAnemID column and remove duplicates
multi <- anem %>% filter(!is.na(old_anem_id)) %>% select(old_anem_id, anem_id, anemobs) %>% distinct()

# assign an observation to new pairs that have not been observed before
multi$anemobs <- 1:nrow(multi)

# connect repeat anems 
for (i in 1:nrow(multi)) {
  multi$anemobs[which(multi$old_anem_id == multi$anem_id[i])] <- multi$anemobs[i]
}

# incorporate this into the anem table
# test i <- 1
for(i in 1:nrow(multi)){
  anem$anemobs[which(anem$anem_id == multi$anem_id[i])] <- multi$anemobs[i]
  anem$anemobs[which(anem$anem_id == multi$old_anem_id[i])] <- multi$anemobs[i]
  
}

rm(leyte)
# add this data to the database
leytes <- writeleyte()

# Send data to database
RMySQL::dbWriteTable(leytes, name = "anemones", value = data.frame(anem), row.names = FALSE, overwrite = TRUE)



dbDisconnect(leyte)
rm(leytes)

