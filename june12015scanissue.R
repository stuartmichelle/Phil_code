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



