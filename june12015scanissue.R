# special script for the 6/1/2015 6/2/2015 tag scan situtation (where a tag was put into a fish on 6/1 but scanned in the lab on 6/2)

source("datefishcap.R")
source("conleyte.R")
leyte <- conleyte()


# June1 -------------------------------------------------------------------

# tags scanned on 6/1/2015
X <- leyte %>% tbl("pitscan")  %>% filter(date == "2015-06-01") %>% collect()

# reduce tagid to 6 digit number
X$tagid <- substr(X$tag, 7, 12)

# simplify table
X$city <- NULL
X$tag <- NULL
june1scan <- X
june1scan <- arrange(june1scan, time)

# put into matrix
rows <- data.frame( Row = rep(LETTERS[1:10], 10), Col = unlist(lapply(1:10, rep, 10)))
rows <- arrange(rows, Row)
rows <- rows[1:nrow(X), ]
simple <- cbind(june1scan, rows)
simple <- simple[, c("Row", "Col", "tagid")]
platemap1 <- as.matrix(reshape2::acast(simple,simple[,1] ~ simple[,2]))


# fish tagged on 6/1/2015
suppressWarnings(dive <- leyte %>% tbl("diveinfo") %>% filter(date == "2015-06-01") %>% collect())
anem <- leyte %>% tbl("anemones") %>% filter(dive_table_id %in% dive$id) %>% collect() 
fish <- leyte  %>% tbl("clownfish") %>% filter(anem_table_id %in% anem$anem_table_id) %>% collect()
fish$tagid <- as.character(fish$tagid)
fish <- fish[!is.na(fish$tagid), ]

# simplify fish tagid
fish$tagid <- substr(fish$tagid, 10, 15)

# simplify table 
fish <- fish[ , c("sample_id", "tagid")]

# join fish and tag tables
tag <- full_join(X, fish, by = "tagid")

june1 <- tag
june1 <- dplyr::arrange(june1, time)

# it is possible that all of the tags after 405807 are off by 1 (that 405807 is actually 403740 and so on)
june1$altid <- NA
for (i in 1:nrow(june1)){
  june1$altid[i] <- june1$tagid[i+1]
}
june1$altid[1:10] <- NA

unused <- june1[is.na(june1$sample_id), ]
june1 <- june1[!is.na(june1$sample_id), ]

# remove used tags from plate
for (i in 1:nrow(simple)){
  if(simple$tagid[i] %in% june1$tagid){
    simple$tagid[i] <- NA
  }
}
platemap2 <- as.matrix(reshape2::acast(simple,simple[,1] ~ simple[,2]))


# June2 -------------------------------------------------------------------

# tags scanned on 6/2/2015
X <- leyte %>% tbl("pitscan")  %>% filter(date == "2015-06-02") %>% collect()

# reduce tagid to 6 digit number
X$tagid <- substr(X$tag, 7, 12)

# simplify table
X$city <- NULL
X$tag <- NULL
june2scan <- X
june2scan <- arrange(june2scan, time)

# put into matrix
rows <- data.frame( Row = rep(LETTERS[1:10], 10), Col = unlist(lapply(1:10, rep, 10)))
rows <- arrange(rows, Row)
rows <- rows[1:nrow(X), ]
Y <- cbind(june2scan, rows)
Y <- Y[, c("Row", "Col", "tagid")]
simple <- rbind(Y, simple)
simple <- simple[!is.na(simple$tagid), ]
platemap3 <- as.matrix(reshape2::acast(simple,simple[,1] ~ simple[,2], value.var = "tagid"))


# fish tagged on 6/2/2015
suppressWarnings(dive <- leyte %>% tbl("diveinfo") %>% filter(date == "2015-06-02") %>% collect())
anem <- leyte %>% tbl("anemones") %>% filter(dive_table_id %in% dive$id) %>% collect() 
fish <- leyte  %>% tbl("clownfish") %>% filter(anem_table_id %in% anem$anem_table_id) %>% collect()
fish$tagid <- as.character(fish$tagid)
fish <- fish[!is.na(fish$tagid), ]

# simplify fish tagid
fish$tagid <- substr(fish$tagid, 10, 15)

# simplify table 
fish <- fish[ , c("sample_id", "tagid")]

# join fish and tag tables
tag <- full_join(X, fish, by = "tagid")

june2 <- tag
june2 <- dplyr::arrange(june2, time)


unused <- june2[is.na(june2$sample_id), ]
june2 <- june2[!is.na(june2$sample_id), ]

# remove used tags from plate
for (i in 1:nrow(simple)){
  if(simple$tagid[i] %in% june2$tagid){
    simple$tagid[i] <- NA
  }
}
platemap4 <- as.matrix(reshape2::acast(simple,simple[,1] ~ simple[,2], value.var = "tagid"))


# June4 -------------------------------------------------------------------

# tags scanned on 6/4/2015
X <- leyte %>% tbl("pitscan")  %>% filter(date == "2015-06-04") %>% collect()

# reduce tagid to 6 digit number
X$tagid <- substr(X$tag, 7, 12)

# simplify table
X$city <- NULL
X$tag <- NULL
june4scan <- X
june4scan <- arrange(june4scan, time)

# put into matrix
rows <- data.frame( Row = rep(LETTERS[1:10], 10), Col = unlist(lapply(1:10, rep, 10)))
rows <- arrange(rows, Row)
rows <- rows[1:nrow(X), ]
Y <- cbind(june4scan, rows)
Y <- Y[, c("Row", "Col", "tagid")]
simple <- rbind(Y, simple)
simple <- simple[!is.na(simple$tagid), ]
platemap5 <- as.matrix(reshape2::acast(simple,simple[,1] ~ simple[,2], value.var = "tagid"))


# fish tagged on 6/4/2015
suppressWarnings(dive <- leyte %>% tbl("diveinfo") %>% filter(date == "2015-06-04") %>% collect())
anem <- leyte %>% tbl("anemones") %>% filter(dive_table_id %in% dive$id) %>% collect() 
fish <- leyte  %>% tbl("clownfish") %>% filter(anem_table_id %in% anem$anem_table_id) %>% collect()
fish$tagid <- as.character(fish$tagid)
fish <- fish[!is.na(fish$tagid), ]

# simplify fish tagid
fish$tagid <- substr(fish$tagid, 10, 15)

# simplify table 
fish <- fish[ , c("sample_id", "tagid")]

# join fish and tag tables
tag <- full_join(X, fish, by = "tagid")

june4 <- tag
june4 <- dplyr::arrange(june4, time)


unused <- june4[is.na(june4$sample_id), ]
june4 <- june4[!is.na(june4$sample_id), ]

# remove used tags from plate
for (i in 1:nrow(simple)){
  if(simple$tagid[i] %in% june4$tagid){
    simple$tagid[i] <- NA
  }
}
platemap6 <- as.matrix(reshape2::acast(simple,simple[,1] ~ simple[,2], value.var = "tagid"))


# June5 -------------------------------------------------------------------

# tags scanned on 6/5/2015
X <- leyte %>% tbl("pitscan")  %>% filter(date == "2015-06-05") %>% collect()

# reduce tagid to 6 digit number
X$tagid <- substr(X$tag, 7, 12)

# simplify table
X$city <- NULL
X$tag <- NULL
june5scan <- X
june5scan <- arrange(june5scan, time)

# put into matrix
rows <- data.frame( Row = rep(LETTERS[1:10], 10), Col = unlist(lapply(1:10, rep, 10)))
rows <- arrange(rows, Row)
rows <- rows[1:nrow(X), ]
Y <- cbind(june5scan, rows)
Y <- Y[, c("Row", "Col", "tagid")]
simple <- rbind(Y, simple)
simple <- simple[!is.na(simple$tagid), ]
platemap7 <- as.matrix(reshape2::acast(simple,simple[,1] ~ simple[,2], value.var = "tagid"))


# fish tagged on 6/5/2015
suppressWarnings(dive <- leyte %>% tbl("diveinfo") %>% filter(date == "2015-06-05") %>% collect())
anem <- leyte %>% tbl("anemones") %>% filter(dive_table_id %in% dive$id) %>% collect() 
fish <- leyte  %>% tbl("clownfish") %>% filter(anem_table_id %in% anem$anem_table_id) %>% collect()
fish$tagid <- as.character(fish$tagid)
fish <- fish[!is.na(fish$tagid), ]

# simplify fish tagid
fish$tagid <- substr(fish$tagid, 10, 15)

# simplify table 
fish <- fish[ , c("sample_id", "tagid")]

# join fish and tag tables
tag <- full_join(X, fish, by = "tagid")

june5 <- tag
june5 <- dplyr::arrange(june5, time)


unused <- june5[is.na(june5$sample_id), ]
june5 <- june5[!is.na(june5$sample_id), ]

# remove used tags from plate
for (i in 1:nrow(simple)){
  if(simple$tagid[i] %in% june5$tagid){
    simple$tagid[i] <- NA
  }
}
platemap8 <- as.matrix(reshape2::acast(simple,simple[,1] ~ simple[,2], value.var = "tagid"))


# June7 -------------------------------------------------------------------

# tags scanned on 6/7/2015
X <- leyte %>% tbl("pitscan")  %>% filter(date == "2015-06-07") %>% collect()

# reduce tagid to 6 digit number
X$tagid <- substr(X$tag, 7, 12)

# simplify table
X$city <- NULL
X$tag <- NULL
june7scan <- X
june7scan <- arrange(june7scan, desc(time))

# # were any tags from simple scanned again in june7scan? only 355666
# simple$Repeat <- NA
# for (i in 1:nrow(simple)){
#   if(simple$tagid[i] %in% june7scan$tagid){
#     simple$Repeat[i] <- "YES"
#   }
# }

# put into matrix
rows <- data.frame( Row = rep(LETTERS[1:10], 10), Col = unlist(lapply(1:10, rep, 10)))
rows <- arrange(rows, Row)
rows <- rows[1:nrow(june7scan), ]
Y <- cbind(june7scan, rows)
Y <- Y[, c("Row", "Col", "tagid")]
# # not sure what order the old tags were in at this point so plate map 9 does not hold them
simple_old <- simple
simple <- Y
platemap9 <- as.matrix(reshape2::acast(simple,simple[,1] ~ simple[,2], value.var = "tagid"))


# fish tagged on 6/7/2015
suppressWarnings(dive <- leyte %>% tbl("diveinfo") %>% filter(date == "2015-06-07") %>% collect())
anem <- leyte %>% tbl("anemones") %>% filter(dive_table_id %in% dive$id) %>% collect() 
fish <- leyte  %>% tbl("clownfish") %>% filter(anem_table_id %in% anem$anem_table_id) %>% collect()
fish$tagid <- as.character(fish$tagid)
fish <- fish[!is.na(fish$tagid), ]

# simplify fish tagid
fish$tagid <- substr(fish$tagid, 10, 15)

# simplify table 
fish <- fish[ , c("sample_id", "tagid")]

# join fish and tag tables
tag <- full_join(X, fish, by = "tagid")

june7 <- tag
june7 <- dplyr::arrange(june7, time)


unused <- june7[is.na(june7$sample_id), ]
june7 <- june7[!is.na(june7$sample_id), ]

# remove used tags from plate
for (i in 1:nrow(simple)){
  if(simple$tagid[i] %in% june7$tagid){
    simple$tagid[i] <- NA
  }
}
platemap10 <- as.matrix(reshape2::acast(simple,simple[,1] ~ simple[,2], value.var = "tagid"))


# June9 -------------------------------------------------------------------

# no tags were scanned on June 8 and none were used on June 8
# no tags were scanned on June 9 and the tags were used as follows

# tags left over from June 7 in order
june9gp1 <- simple[!is.na(simple$tagid), ]
june9gp1$Row <- NULL
june9gp1$Col <- NULL
rows <- data.frame( Row = rep(LETTERS[1:10], 10), Col = unlist(lapply(1:10, rep, 10)))
rows <- arrange(rows, Row)
rows <- rows[1:nrow(june9gp1), ]
june9gp1 <- cbind(rows, june9gp1)

# tags left over from june 5 in reverse order
june9gp2 <- full_join(june5scan, simple_old, by = "tagid")
june9gp2 <- june9gp2[!is.na(june9gp2$date), ]
june9gp2 <- june9gp2[!is.na(june9gp2$Row), ]
june9gp2$Row <- NULL
june9gp2$Col <- NULL
june9gp2$date <- NULL
june9gp2 <- arrange(june9gp2, desc(time))
june9gp2$time <- NULL
rows <- data.frame( Row = rep(LETTERS[1:10], 10), Col = unlist(lapply(1:10, rep, 10)))
rows <- arrange(rows, Row)
rows <- rows[6:22, ]
june9gp2 <- cbind(rows, june9gp2)

# tags left over from june 4 in order
june9gp3 <- full_join(june4scan, simple_old, by = "tagid")
june9gp3 <- june9gp3[!is.na(june9gp3$tagid), ]
june9gp3 <- june9gp3[!is.na(june9gp3$Row), ]
june9gp3 <- arrange(june9gp3, Row)
june9gp3 <- june9gp3[18:nrow(june9gp3), ]
june9gp3$Row <- NULL
june9gp3$Col <- NULL
june9gp3$date <- NULL
june9gp3$time <- NULL
rows <- data.frame( Row = rep(LETTERS[1:10], 10), Col = unlist(lapply(1:10, rep, 10)))
rows <- arrange(rows, Row)
rows <- rows[23:38, ]
june9gp3 <- cbind(rows, june9gp3)

# combine the groups
june9scan <- rbind(june9gp1, june9gp2, june9gp3)
simple <- distinct(june9scan) # was getting an error "Aggregation function missing: defaulting to length" because I had a duplicate row
simple$Row <- as.character(simple$Row)
platemap11 <- as.matrix(reshape2::acast(simple,simple[,1] ~ simple[,2]))


# fish tagged on 6/9/2015
suppressWarnings(dive <- leyte %>% tbl("diveinfo") %>% filter(date == "2015-06-09") %>% collect())
anem <- leyte %>% tbl("anemones") %>% filter(dive_table_id %in% dive$id) %>% collect() 
fish <- leyte  %>% tbl("clownfish") %>% filter(anem_table_id %in% anem$anem_table_id) %>% collect()
fish$tagid <- as.character(fish$tagid)
fish <- fish[!is.na(fish$tagid), ]

# simplify fish tagid
fish$tagid <- substr(fish$tagid, 10, 15)

# simplify table 
fish <- fish[ , c("sample_id", "tagid")]

# join fish and tag tables
tag <- full_join(june9scan, fish, by = "tagid")

june9 <- tag

unused <- june9[is.na(june9$sample_id), ]
june9 <- june9[!is.na(june9$sample_id), ]

# remove used tags from plate
for (i in 1:nrow(simple)){
  if(simple$tagid[i] %in% june9$tagid){
    simple$tagid[i] <- NA
  }
}
platemap12 <- as.matrix(reshape2::acast(simple,simple[,1] ~ simple[,2], value.var = "tagid"))


# June11 ------------------------------------------------------------------

# tags scanned on 6/11/2015
X <- leyte %>% tbl("pitscan")  %>% filter(date == "2015-06-11") %>% collect()

# reduce tagid to 6 digit number
X$tagid <- substr(X$tag, 7, 12)

# simplify table
X$city <- NULL
X$tag <- NULL
june11scan <- X
june11scan <- arrange(june11scan, time)

# used tags from previous days before using June 11 scanned tags
simple <- simple[!is.na(simple$tagid), ]
rows <- data.frame( Row = rep(LETTERS[1:10], 10), Col = unlist(lapply(1:10, rep, 10)))
rows <- arrange(rows, Row)
rows <- rows[1:nrow(simple), ]
simple$Row <- rows$Row
simple$Col <- rows$Col

# put into matrix
rows <- data.frame( Row = rep(LETTERS[1:10], 10), Col = unlist(lapply(1:10, rep, 10)))
rows <- arrange(rows, Row)
rows <- rows[14:42, ]
Y <- cbind(june11scan, rows)
Y <- Y[, c("Row", "Col", "tagid")]
simple <- rbind(Y, simple)
simple <- simple[!is.na(simple$tagid), ]
simple <- arrange(simple, Row)
platemap13 <- as.matrix(reshape2::acast(simple,simple[,1] ~ simple[,2], value.var = "tagid"))


# fish tagged on 6/11/2015
suppressWarnings(dive <- leyte %>% tbl("diveinfo") %>% filter(date == "2015-06-11") %>% collect())
anem <- leyte %>% tbl("anemones") %>% filter(dive_table_id %in% dive$id) %>% collect() 
fish <- leyte  %>% tbl("clownfish") %>% filter(anem_table_id %in% anem$anem_table_id) %>% collect()
fish$tagid <- as.character(fish$tagid)
fish <- fish[!is.na(fish$tagid), ]

# simplify fish tagid
fish$tagid <- substr(fish$tagid, 10, 15)

# simplify table 
fish <- fish[ , c("sample_id", "tagid")]

# join fish and tag tables
tag <- full_join(X, fish, by = "tagid")

june11 <- tag
june11 <- dplyr::arrange(june11, time)


unused <- june11[is.na(june11$sample_id), ]
june11 <- june11[!is.na(june11$sample_id), ]

# remove used tags from plate
for (i in 1:nrow(simple)){
  if(simple$tagid[i] %in% june11$tagid){
    simple$tagid[i] <- NA
  }
}
platemap14 <- as.matrix(reshape2::acast(simple,simple[,1] ~ simple[,2], value.var = "tagid"))


# June13 ------------------------------------------------------------------

# tags scanned on 6/13/2015 - rescanned tags from previous days - also scanned fish underwater
X <- leyte %>% tbl("pitscan")  %>% filter(date == "2015-06-13") %>% collect()

# reduce tagid to 6 digit number
X$tagid <- substr(X$tag, 7, 12)

# simplify table
X$city <- NULL
X$tag <- NULL
june13scan <- X
june13scan <- arrange(june13scan, time)
predive13 <- june13scan[1:30,]

# put into matrix
rows <- data.frame( Row = rep(LETTERS[1:10], 10), Col = unlist(lapply(1:10, rep, 10)))
rows <- arrange(rows, Row)
rows <- rows[1:nrow(predive13), ]
Y <- cbind(predive13, rows)
Y <- Y[, c("Row", "Col", "tagid")]
simple <- Y
simple <- simple[!is.na(simple$tagid), ]
simple <- arrange(simple, Row)
platemap15 <- as.matrix(reshape2::acast(simple,simple[,1] ~ simple[,2], value.var = "tagid"))


# fish tagged on 6/13/2015
suppressWarnings(dive <- leyte %>% tbl("diveinfo") %>% filter(date == "2015-06-13") %>% collect())
anem <- leyte %>% tbl("anemones") %>% filter(dive_table_id %in% dive$id) %>% collect() 
fish <- leyte  %>% tbl("clownfish") %>% filter(anem_table_id %in% anem$anem_table_id) %>% collect()
fish$tagid <- as.character(fish$tagid)
fish <- fish[!is.na(fish$tagid), ]

# simplify fish tagid
fish$tagid <- substr(fish$tagid, 10, 15)

# simplify table 
fish <- fish[ , c("sample_id", "tagid")]

# join fish and tag tables
tag <- full_join(X, fish, by = "tagid")

june13 <- tag
june13 <- dplyr::arrange(june13, time)


unused <- june13[is.na(june13$sample_id), ]
june13 <- june13[!is.na(june13$sample_id), ]

# remove used tags from plate
for (i in 1:nrow(simple)){
  if(simple$tagid[i] %in% june13$tagid){
    simple$tagid[i] <- NA
  }
}
platemap16 <- as.matrix(reshape2::acast(simple,simple[,1] ~ simple[,2], value.var = "tagid"))

# the tables aren't making sense as far as scanned order and fish collection order
fish13 <- fish_col_order[which(fish_col_order$date == "2015-06-13"), ]
fish13 <- fish13[which(nchar(fish13$sample_id) == 13), ]
fish13$tagid <- substr(fish13$sample_id, 8, 13)
fish13 <- arrange(fish13, ObsTime)
fish13$collection_number <- 1:nrow(fish13)

june13scan <- arrange(june13scan, time)
june13scan$scan_order <- 1:nrow(june13scan)

test <- full_join(fish13, june13scan, by = "tagid")


# June14 ------------------------------------------------------------------

X <- leyte %>% tbl("pitscan")  %>% filter(date == "2015-06-14") %>% collect()
# reduce tagid to 6 digit number
X$tagid <- substr(X$tag, 7, 12)

# simplify table
X$city <- NULL
# X$tag <- NULL
june14scan <- X
june14scan <- arrange(june14scan, time)

# fish tagged on 6/14/2015
suppressWarnings(dive <- leyte %>% tbl("diveinfo") %>% filter(date == "2015-06-14") %>% collect())
anem <- leyte %>% tbl("anemones") %>% filter(dive_table_id %in% dive$id) %>% collect() 
fish <- leyte  %>% tbl("clownfish") %>% filter(anem_table_id %in% anem$anem_table_id) %>% collect()
fish$tagid <- as.character(fish$tagid)
fish <- fish[!is.na(fish$tagid), ]

# simplify fish tagid
fish$tagid <- substr(fish$tagid, 10, 15)

# simplify table 
fish <- fish[ , c("sample_id", "tagid")]

# join fish and tag tables
tag <- full_join(X, fish, by = "tagid")

june14 <- tag
june14 <- dplyr::arrange(june14, time)

unused <- june14[is.na(june14$sample_id), ]
june14 <- june14[!is.na(june14$sample_id), ]

# remove used tags from plate
for (i in 1:nrow(june14scan)){
  if(june14scan$tagid[i] %in% june14$tagid){
    june14scan$tagid[i] <- NA
  }
}

# everything was scanned was given a sample Id


suppressWarnings(datefishcap("APCL15_402726"))

##################### tag vs. identity clarification #####################

# Scenario A - tag was not used and all 




