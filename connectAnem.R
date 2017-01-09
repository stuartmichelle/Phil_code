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
multi <- anem %>% filter(!is.na(old_anem_id) & is.na(anemobs)) %>% select(old_anem_id, anem_id, anemobs) %>% distinct()

# add all of the repeats for the same anem_id number
dups <- anem[!is.na(anem$anem_id), ]
dups <- dups[duplicated(dups$anem_id), ]
dups <- dups[, c("old_anem_id", "anemobs", "anem_id")]

for (i in 1:nrow(dups)){
  X <- anem[which(anem$anem_id == dups$anem_id[i]), c("anemobs", "anem_id", "old_anem_id")]
  multi <- rbind(multi, X)
}

# remove duplicates
multi <- distinct(multi)

# remove samples with observations
multi <- multi[is.na(multi$anemobs), ]

# find the next obs number
n <- max(anem$anemobs, na.rm = T)
multi$anemobs <- (n+1):(n+nrow(multi))

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

# add this data to the database
leytes <- writeleyte()

# Send data to database
library(RMySQL)
leytes <- dbConnect(MySQL(), dbname="Leyte", default.file = path.expand("~/myconfig.cnf"), port = 3306, create = F, host = NULL, user = NULL, password = NULL)

dbWriteTable(leytes, "anemones", data.frame(anem), row.names = F, overwrite = T)


dbDisconnect(leytes)
rm(leytes)

