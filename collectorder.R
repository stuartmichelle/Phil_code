# a script to put samples in order of collection - this is for the 2015_05 field
# season where samples were not identified by order of collection but by tag
# number.

source("conleyte.R")
source("../../myRcode/Laboratory/R/conlabor.R")

leyte <- conleyte()

# for all of the dives from 2015_05 season
dive <- leyte %>% tbl("diveinfo")  %>% filter(date > "2015-04-01" & date < "2016-01-01")  %>% select(id, date) %>% collect()
anem <- leyte %>% tbl("anemones") %>% filter(dive_table_id %in% dive$id) %>% select(anem_table_id, ObsTime, dive_table_id) %>% collect()
anem <- left_join(anem, dive, by = c("dive_table_id" = "id"))
rm(dive)
fish <- leyte %>% tbl("clownfish") %>% filter(anem_table_id %in% anem$anem_table_id & !is.na(sample_id)) %>% select(anem_table_id, sample_id) %>% collect()
fish <- left_join(fish, anem, by = "anem_table_id")

# put in order of collection
fish <- fish[ , c("sample_id", "date", "ObsTime")]
fish <- fish %>% arrange(date, ObsTime)


# give them a collection number
fish$collection_number <- 1:nrow(fish)

# create a plan number column
fish$plan_number <- NA

# fish$plan_number[26] <- "26a"
#  fish$plan_number[25] <- "27a"
#  fish$plan_number[24] <- "28a"
#  fish$plan_number[23] <- "29a"
# 
#  fish$plan_number[434] <- "29b"
#  > fish$plan_number[435] <- "30b"
#  > fish$plan_number[436] <- "31b"
#  > fish$plan_number[437] <- "32b"
#  > fish$plan_number[440] <- "33b"
#  > fish$plan_number[441] <- 34
#  > fish$plan_number[441:443] <- 34:36
#  > fish$plan_number[446] <- 37
#  > fish$plan_number[448:453] <- 38:43
#  > fish$plan_number[455] <- 44
#  > fish$plan_number[458:460] <- 45:47
#  > fish$plan_number[462:470] <- 48:56
#  > fish$plan_number[472:473] <- 57:58
#  > fish$plan_number[475:476] <- 59:60

# add scanned pit order
pit <- leyte %>% tbl("pitscan") %>% filter(date > "2015-04-01" & date < "2016-01-01" & city == 985) %>% collect()

# create a column of 6 digit tags
pit$id <- substr(pit$tag, 7, 12)

pit <- pit %>% select(id, date, time) %>% arrange(date, time)

# add a scan number to the pit table
pit$scan <- 1:nrow(pit)

# create a 6 digit tag column in fish
fish$id <- NA
for (i in 1:nrow(fish)){
  if (nchar(fish$sample_id[i]) == 13){
    fish$id[i] <- substr(fish$sample_id[i], 8, 13)
  }
}

# get rid of any non-tagged fish
for (i in 1:nrow(fish)){
  if (nchar(fish$sample_id[i]) < 13){
    fish$sample_id[i] <- NA
  }
}
fish <- fish[!is.na(fish$sample_id), ]

# join the fish and pit scan orders
join <- left_join(fish, pit, by = "id")

# make id a number
join$id <- as.numeric(join$id)

# find numbers scanned twice
two <- join %>% filter(duplicated(join$id))

# dive one
d1 <- join %>% filter(date.x == "2015-05-28" & ObsTime < "12:00:00")
d2 <- join %>% filter(date.x == "2015-05-28" & ObsTime > "12:00:00")
d3 <- join %>% filter(date.x == "2015-05-29" & ObsTime < "12:30:00")
d4 <- join %>% filter(date.x == "2015-05-29" & ObsTime > "12:30:00")

# create an extraction order column
labor <- conlabor()
extract <- labor %>% tbl("extraction") %>% filter(sample_id %in% fish$sample_id) %>% select(sample_id, date, plate, well) %>% collect() 
extract$Row <- substr(extract$well, 1, 1)
extract$Col <- substr(extract$well, 2, 3)
extract$Col <- as.integer(extract$Col)
extract <- arrange(extract, date, plate, Col, Row)
extract$extract <- 1:nrow(extract)

# simplify table
extract <- extract[ , c("sample_id", "extract")]

fish <- full_join(fish, extract, by = "sample_id")

# find current extract box order
fish <- arrange(fish, extract)
fish[630:648, ] <- NA
fish$ebox <- NA
for (i in 1:8){
  fish$ebox[((81*(i-1))+1):(81*i)] <- i
}

# find coll box number
fish <- arrange(fish, collection_number)
fish$cbox <- NA
for (i in 1:8){
  fish$cbox[(81*(i-1)+1):(81*i)] <- i
}

# create box well number
fish$boxwell <- NA
fish$boxwell[which(fish$cbox == 1)] <- 1:81
# box4_2015 <- subset(fish, fish$cbox == 4)
# write.csv(box4_2015, file = "../Surveys_2015_05/box4_2015.csv")

fish$boxwell[which(fish$cbox == 2)] <- 1:81
# box4_2015 <- subset(fish, fish$cbox == 4)
# write.csv(box4_2015, file = "../Surveys_2015_05/box4_2015.csv")


fish$boxwell[which(fish$cbox == 3)] <- 1:81
# box4_2015 <- subset(fish, fish$cbox == 4)
# write.csv(box4_2015, file = "../Surveys_2015_05/box4_2015.csv")

fish$boxwell[which(fish$cbox == 4)] <- 1:81
box4_2015 <- subset(fish, fish$cbox == 4)
write.csv(box4_2015, file = "../Surveys_2015_05/box4_2015.csv")

fish$boxwell[which(fish$cbox == 5)] <- 1:81
box5_2015 <- subset(fish, fish$cbox == 5)
write.csv(box5_2015, file = "../Surveys_2015_05/box5_2015.csv")

fish$boxwell[which(fish$cbox == 6)] <- 1:81
box6_2015 <- subset(fish, fish$cbox == 6)
write.csv(box6_2015, file = "../Surveys_2015_05/box6_2015.csv")


fish$boxwell[which(fish$cbox == 7)] <- 1:81
box7_2015 <- subset(fish, fish$cbox == 7)
write.csv(box7_2015, file = "../Surveys_2015_05/box7_2015.csv")


fish$boxwell[which(fish$cbox == 8)] <- 1:81
box8_2015 <- subset(fish, fish$cbox == 8)
write.csv(box8_2015, file = "../Surveys_2015_05/box8_2015.csv")

mystery <- subset(pit, date == "2015-06-13")




