# Make a list of tags scanned on day 1, make a list of tags scanned on day 2, make sure no fish from day 1 have tags from day 2. - tagscan.R
source("datefishcap.R")
source("conleyte.R")
leyte <- conleyte()


# 5/28 Day 1 -------------------------------------------------------------------
# tags scanned into pool
pool <- leyte %>% tbl("pitscan")  %>% filter(date == "2015-05-28") %>% collect()
# create a column for comparison with db
pool$tagid <- paste(pool$city, pool$tag, sep = "")

# tags used on day 1
suppressWarnings(dive <- leyte %>% tbl("diveinfo") %>% filter(date == "2015-05-28") %>% collect())
anem <- leyte %>% tbl("anemones") %>% filter(dive_table_id %in% dive$id) %>% collect() 
fish <- leyte  %>% tbl("clownfish") %>% filter(anem_table_id %in% anem$anem_table_id) %>% collect()
fish$tagid <- as.character(fish$tagid)  
fish <- fish[!is.na(fish$tagid), ]
fish1 <- fish

pool <- anti_join(pool, fish, by = "tagid") # 12 tags still in the pool
pool$tagid <- NULL # get rid of different columns

# were any tags used that weren't in the pool? # this doesn't seem to be working
# fish1 <- anti_join(fish1, pool, by = "tagid")
pool1 <- pool


# 5/29 Day 2 -------------------------------------------------------------------

# more tags scanned into pool
X <- leyte %>% tbl("pitscan")  %>% filter(date == "2015-05-29") %>% collect()
pool <- full_join(pool, X, by = "tag")
pool <- pool[, c("tag", "city.y", "date.y", "time.y")]
rm(X)
names(pool) <- c("tag", "city", "date", "time")
pool$tagid <- paste(pool$city, pool$tag, sep = "")

# tags used on day 2
suppressWarnings(dive <- leyte %>% tbl("diveinfo") %>% filter(date == "2015-05-29") %>% collect())
anem <- leyte %>% tbl("anemones") %>% filter(dive_table_id %in% dive$id) %>% collect() 
fish <- leyte  %>% tbl("clownfish") %>% filter(anem_table_id %in% anem$anem_table_id) %>% collect()
fish$tagid <- as.character(fish$tagid)  

# were any scanned tags from day 2 used on day 1
fish2 <- fish
used <- full_join(fish1, pool, by = "tagid")
used <- used[!is.na(used$fish_table_id), ] # remove any scanned tags that weren't used
used <- used[!is.na(used$tag), ] # remove any tags that weren't scanned
# should be 0

pool <- anti_join(pool, fish, by = "tagid") # 13 tags still in the pool
pool2 <- pool
pool$tagid <- NULL



# 5/30 Day 3 -------------------------------------------------------------------

# more tags scanned into pool
X <- leyte %>% tbl("pitscan")  %>% filter(date == "2015-05-30") %>% collect()
pool <- full_join(pool, X, by = "tag") # verified that full join was not necessary
blanks <- pool[is.na(pool$city.x), ]
pool <- pool[!is.na(pool$city.x), ]
blanks$city.x <- blanks$city.y
blanks$date.x <- blanks$date.y
blanks$time.x <- blanks$time.y
pool <- rbind(pool, blanks)
rm(X, blanks)
pool$tagid <- paste(pool$city.x, pool$tag, sep = "")

# tags used on day 3
suppressWarnings(dive <- leyte %>% tbl("diveinfo") %>% filter(date == "2015-05-30") %>% collect())
anem <- leyte %>% tbl("anemones") %>% filter(dive_table_id %in% dive$id) %>% collect() 
fish <- leyte  %>% tbl("clownfish") %>% filter(anem_table_id %in% anem$anem_table_id) %>% collect()
fish$tagid <- as.character(fish$tagid)  
fish <- fish[!is.na(fish$tagid), ]

# were any scanned tags used on the wrong day
fish3 <- fish
used <- full_join(fish2, pool, by = "tagid")
used <- used[!is.na(used$fish_table_id), ] # remove any scanned tags that weren't used
used <- used[!is.na(used$tag), ] # remove any tags that weren't scanned
# should be 0

pool <- anti_join(pool, fish, by = "tagid") # 19 tags still in the pool
pool3 <- pool
pool$tagid <- NULL
pool$city.y <- NULL
pool$date.y <- NULL
pool$time.y <- NULL


# 5/31 Day 4 -------------------------------------------------------------------
# more tags scanned into pool
X <- leyte %>% tbl("pitscan")  %>% filter(date == "2015-05-31") %>% collect()
pool <- full_join(pool, X, by = "tag") # verified that full join was not necessary
blanks <- pool[is.na(pool$city), ]
pool <- pool[!is.na(pool$city), ]
blanks$city <- blanks$city.x
blanks$date <- blanks$date.x
blanks$time <- blanks$time.x
pool <- rbind(pool, blanks)
rm(X, blanks)
pool$tagid <- paste(pool$city, pool$tag, sep = "")
pool$city.x <- NULL
pool$date.x <- NULL
pool$time.x <- NULL

# tags used on day 4
suppressWarnings(dive <- leyte %>% tbl("diveinfo") %>% filter(date == "2015-05-31") %>% collect())
anem <- leyte %>% tbl("anemones") %>% filter(dive_table_id %in% dive$id) %>% collect() 
fish <- leyte  %>% tbl("clownfish") %>% filter(anem_table_id %in% anem$anem_table_id) %>% collect()
fish$tagid <- as.character(fish$tagid)  
fish <- fish[!is.na(fish$tagid), ]

# were any scanned tags used on the wrong day
fish4 <- fish
used <- full_join(fish3, pool, by = "tagid")
used <- used[!is.na(used$fish_table_id), ] # remove any scanned tags that weren't used
used <- used[!is.na(used$tag), ] # remove any tags that weren't scanned
# should be 0

pool <- anti_join(pool, fish, by = "tagid") # 22 tags still in the pool
pool$tagid <- NULL
pool4 <- pool



# 6/1 Day 5 -------------------------------------------------------------------
# more tags scanned into pool
X <- leyte %>% tbl("pitscan")  %>% filter(date == "2015-06-01") %>% collect()
pool <- full_join(pool, X, by = "tag") # verified that full join was not necessary
blanks <- pool[is.na(pool$city.x), ]
pool <- pool[!is.na(pool$city.x), ]
blanks$city.x <- blanks$city.y
blanks$date.x <- blanks$date.y
blanks$time.x <- blanks$time.y
pool <- rbind(pool, blanks)
rm(X, blanks)
pool$tagid <- paste(pool$city.x, pool$tag, sep = "")
pool$city.x <- NULL
pool$date.x <- NULL
pool$time.x <- NULL

# tags used on day 5
suppressWarnings(dive <- leyte %>% tbl("diveinfo") %>% filter(date == "2015-06-01") %>% collect())
anem <- leyte %>% tbl("anemones") %>% filter(dive_table_id %in% dive$id) %>% collect() 
fish <- leyte  %>% tbl("clownfish") %>% filter(anem_table_id %in% anem$anem_table_id) %>% collect()
fish$tagid <- as.character(fish$tagid)
fish <- fish[!is.na(fish$tagid), ]

# were any scanned tags used on the wrong day
fish5 <- fish
used <- full_join(fish4, pool, by = "tagid")
used <- used[!is.na(used$fish_table_id), ] # remove any scanned tags that weren't used
used <- used[!is.na(used$tag), ] # remove any tags that weren't scanned
# should be 0

pool <- anti_join(pool, fish, by = "tagid") # 34 tags still in the pool
pool$tagid <- NULL
pool5 <- pool


# 6/2 Day 6 -------------------------------------------------------------------
X <- leyte %>% tbl("pitscan")  %>% filter(date == "2015-06-02") %>% collect()
pool <- full_join(pool, X, by = "tag") # verified that full join was not necessary
blanks <- pool[is.na(pool$city), ]
pool <- pool[!is.na(pool$city), ]
blanks$city <- blanks$city.y
blanks$date <- blanks$date.y
blanks$time <- blanks$time.y
pool <- rbind(pool, blanks)
rm(X, blanks)
pool$tagid <- paste(pool$city, pool$tag, sep = "")
pool$city.y <- NULL
pool$date.y <- NULL
pool$time.y <- NULL

# tags used on day 6
suppressWarnings(dive <- leyte %>% tbl("diveinfo") %>% filter(date == "2015-06-02") %>% collect())
anem <- leyte %>% tbl("anemones") %>% filter(dive_table_id %in% dive$id) %>% collect() 
fish <- leyte  %>% tbl("clownfish") %>% filter(anem_table_id %in% anem$anem_table_id) %>% collect()
fish$tagid <- as.character(fish$tagid)  
fish <- fish[!is.na(fish$tagid), ]

# were any scanned tags used on the wrong day
fish6 <- fish
used <- full_join(fish5, pool, by = "tagid")
used <- used[!is.na(used$fish_table_id), ] # remove any scanned tags that weren't used
used <- used[!is.na(used$tag), ] # remove any tags that weren't scanned
# should be 0

pool <- anti_join(pool, fish, by = "tagid") # 15 tags still in the pool
pool$tagid <- NULL
pool6 <- pool


# 6/4 Day 7 -------------------------------------------------------------------
# more tags scanned into pool
X <- leyte %>% tbl("pitscan")  %>% filter(date == "2015-06-04") %>% collect()
pool <- full_join(pool, X, by = "tag") # verified that full join was not necessary
blanks <- pool[is.na(pool$city.x), ]
pool <- pool[!is.na(pool$city.x), ]
blanks$city.x <- blanks$city.y
blanks$date.x <- blanks$date.y
blanks$time.x <- blanks$time.y
pool <- rbind(pool, blanks)
rm(X, blanks)
pool$tagid <- paste(pool$city.x, pool$tag, sep = "")
pool$city.y <- NULL
pool$date.y <- NULL
pool$time.y <- NULL

# tags used on day 7
suppressWarnings(dive <- leyte %>% tbl("diveinfo") %>% filter(date == "2015-06-04") %>% collect())
anem <- leyte %>% tbl("anemones") %>% filter(dive_table_id %in% dive$id) %>% collect() 
fish <- leyte  %>% tbl("clownfish") %>% filter(anem_table_id %in% anem$anem_table_id) %>% collect()
fish$tagid <- as.character(fish$tagid)
fish <- fish[!is.na(fish$tagid), ]

pool <- anti_join(pool, fish, by = "tagid") # 17 tags still in the pool
pool$tagid <- NULL
pool7 <- pool


# Day 8 -------------------------------------------------------------------
X <- leyte %>% tbl("pitscan")  %>% filter(date == "2015-06-05") %>% collect()
pool <- full_join(pool, X, by = "tag") # verified that full join was not necessary
blanks <- pool[is.na(pool$city), ]
pool <- pool[!is.na(pool$city), ]
blanks$city <- blanks$city.x
blanks$date <- blanks$date.x
blanks$time <- blanks$time.x
pool <- rbind(pool, blanks)
rm(X, blanks)
pool$tagid <- paste(pool$city, pool$tag, sep = "")
pool$city.x <- NULL
pool$date.x <- NULL
pool$time.x <- NULL

# tags used on day 6
suppressWarnings(dive <- leyte %>% tbl("diveinfo") %>% filter(date == "2015-06-05") %>% collect())
anem <- leyte %>% tbl("anemones") %>% filter(dive_table_id %in% dive$id) %>% collect() 
fish <- leyte  %>% tbl("clownfish") %>% filter(anem_table_id %in% anem$anem_table_id) %>% collect()
fish$tagid <- as.character(fish$tagid)  
fish <- fish[!is.na(fish$tagid), ]

pool <- anti_join(pool, fish, by = "tagid") # 34 tags still in the pool
pool$tagid <- NULL
pool8 <- pool



# which date was a fish captured
suppressWarnings(day <- datefishcap("APCL15_403055"))
print(day$date)
