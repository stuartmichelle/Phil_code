# test the growth features of the fishmethods package

library(fishmethods)
source("conleyte.R")
source("datefish.R")
source("sitefish.R")

# get a list of recaptured fish
leyte <- conleyte()
fish <- leyte %>% tbl("clownfish") %>% filter(!is.na(capid) | recap == "Y") %>% select(capid, recap, tagid, size, sample_id, anem_table_id) %>% collect()

# get the first capture of tag recaptured fish
tags <- leyte %>% tbl("clownfish") %>% filter(tagid %in% fish$tagid) %>% select(capid, recap, tagid, size, sample_id, anem_table_id) %>% collect()
tags$tagid <- as.character(tags$tagid)
fish <- rbind(fish, tags)

# attach dates
date <- datefish(fish$anem_table_id)
date$date <- as.Date(date$date)
fish <- left_join(fish, date, by = "anem_table_id")

# attach sites
site <- sitefish(fish$anem_table_id)
site$dive_table_id <- NULL # remove column so it is not duplicated in the join 
fish <- left_join(fish, site, by = "anem_table_id")

rm(date, site, tags)
fish$anem_table_id <- NULL
fish$dive_table_id <- NULL

# deal with empty value cells
# is.na(fish) <- "0" # this doesn't work, NAs stay NA, also using 0 doesn't work.
# fish$capid[1] <- "0" # that worked
fish$capid[is.na(fish$capid)] <- "0" # that worked


# remove fish that were caught on the same date
for(i in 1:nrow(fish)){
    X <- fish[which(fish$date == fish$date[i])]
    Y <- X[duplicated(X$capid), ]
    
    
  }  
  }
  


# 
# # eliminate fish for which there are incomplete data
# fish$capid[is.na(fish$size)] # should be 1, 9
# fish$capid[fish$capid == 1] <- NA
# fish$capid[fish$capid == 9] <- NA
# fish <- fish[!is.na(fish$capid), ]
# 
# # remove the elementary school fish from the analysis because it was caught twice in the same day
# fish <- fish[fish$name != "Elementary School", ]

# populate an L1 and L2, and TAL column for recaptured fish
recapture <- data.frame()
for(i in 1:nrow(fish)){
  X <- fish[which(fish$capid == fish$capid[i]), ]
  X$L1 <- min(X$size)
  X$L2 <- max(X$size)
  X$T1 <- min(X$date)
  X$T2 <- max(X$date)
  X$tal <- max(X$date) - min(X$date)
  recapture <- rbind(recapture, X[1,])
}

# remove duplicate rows
recapture <- dplyr::distinct(recapture)

# convert tal from days to fraction of year
recapture$tal <- recapture$tal/365

# K originally 0.27 (from fishbase), using admb output of 0.0379 - doesn't help 
grow <- growhamp(L1 = recapture$L1, L2 = recapture$L2, TAL = recapture$tal, Linf = list(startLinf = 15.9, lowerLinf = 13.4, upperLinf = 18.9), K = list(startK = 0.27, lowerK = 0.01, upperK = 1),sigma2_error=list(startsigma2=100,lowersigma2=0.1,uppersigma2=10000),
  sigma2_Linf=list(startsigma2=100,lowersigma2=0.1,uppersigma2=100000),	
  sigma2_K=list(startsigma2=0.5,lowersigma2=1e-8,uppersigma2=10))

write.csv(grow$results, file = paste(Sys.Date(), "grow_results.csv", sep = ""))

# now plot it 

# rename columns for growthTraject function
names(recapture) <- c("capid", "size", "sample_id", "date", "lentag", "lenrec", "T1", "T2", "timelib")

# growthTraject(0.19,97.5,lentag=temp$L1, lenrec=temp$L2,timelib=c(temp$T2-temp$T1)) # had to remove the elementary school fish in order to get this to work without the timelib is numeric and not >0

# growthTraject(grow$results[1,3], grow$results[1,2], lentag = recapture$L1, lenrec = recapture$L2, timelib = recapture$tal, , main = "Growth Trajectories and Fitted Curve", cex.lab=1.5, cex.axis=1.5, cex.main=1,xlab="Relative age, yr", ylab="Length, cm",xlim=NULL, ylim=NULL,ltytraject=1, lwdtraject=1,coltraject=1, ltyvonB=1, lwdvonB=2, colvonB="red",  returnvec=FALSE, returnlimits=FALSE, warn=TRUE)

# Error in seq.int(from, to, length.out = n) : 'to' must be finite
# In addition: Warning message:
#   In log(1 - lentag/Linf) : NaNs produced


growthTraject(grow$results[1,3], grow$results[1,2], lentag = recapture$L1, lenrec = recapture$L2, timelib = recapture$tal, main = "Faber Growth trajectories and fitted curve, K=1.13, Linf = 10.48")
# Error in plot.window(...) : need finite 'xlim' values
# In addition: Warning message:
#   In log(1 - lentag/Linf) : NaNs produced

# ### Because Faber has the smallest AIC, change Linf to 11.1 and it will plot...
# growthTraject(grow$results[1,3], 11.1, lentag = recapture$L1, lenrec = recapture$L2, timelib = recapture$tal, main = "Faber Growth trajectories and fitted curve, K=1.13, Linf = 11.1")


growthTraject(grow$results[2,3], grow$results[2,2], lentag = recapture$L1, lenrec = recapture$L2, timelib = recapture$tal, main = "Kirkwood & Somers Growth trajectories and fitted curve, K=46.74, Linf=9.2")
# Error in plot.window(...) : need finite 'xlim' values
# In addition: Warning message:
#   In log(1 - lentag/Linf) : NaNs produced

growthTraject(grow$results[3,3], grow$results[3,2], lentag = recapture$L1, lenrec = recapture$L2, timelib = recapture$tal, main = "Kirkwood & Somers Growth with ME trajectories and fitted curve, K=0.759, Linf=11.097")

growthTraject(grow$results[4,3], grow$results[4,2], lentag = recapture$L1, lenrec = recapture$L2, timelib = recapture$tal, main = "Kirkwood & Somers Growth with ME & RLE trajectories and fitted curve, K=0.759, Linf=11.097")

growthTraject(grow$results[5,3], grow$results[5,2], lentag = recapture$L1, lenrec = recapture$L2, timelib = recapture$tal, main = "Sainsbury trajectories and fitted curve, K=24.95, Linf=9.81")
# Error in plot.window(...) : need finite 'xlim' values
# In addition: Warning message:
#   In log(1 - lentag/Linf) : NaNs produced

growthTraject(grow$results[6,3], grow$results[6,2], lentag = recapture$L1, lenrec = recapture$L2, timelib = recapture$tal, main = "Sainsbury with ME trajectories and fitted curve, K=1.38, Linf=18.21")

growthTraject(grow$results[7,3], grow$results[7,2], lentag = recapture$L1, lenrec = recapture$L2, timelib = recapture$tal, main = "Sainsbury with ME & RLE trajectories and fitted curve, K=1.38, Linf=18.21")


# create an L1L2 file for BET-grow in admb
lens <- recapture[ , c("L1", "L2")]

msg1 <- c("#init_int numLenShObs")
msg2 <- nrow(lens)
msg3 <- c("# init_matrix LenShDat(1,numLenShObs,1,2)")

# have to convert the data to a tab separated vector or else every line will have msg1, msg2, etc.
L1 <- vector()
for (i in 1:nrow(lens)){
  L1[i] <- paste(lens$L1[i], lens$L2[i], sep = "  ")
}

out <- c(msg1, msg2, msg3, L1)

write.table(out, file = paste("admb/",Sys.Date(), "L1L2.tsv", sep = ""), row.names=FALSE, quote=FALSE, col.names=FALSE) # won't let me use header=FALSE - use col.names instead of header






# By site -----------------------------------------------------------------


# examine growth rate by site
cab <- subset(fish, name == "Cabatuan")
elem <- subset(fish, name == "Elementary School")
haina <- subset(fish, name == "Haina")
mag <- subset(fish, name == "Magbangon")
pal <- subset(fish, name == "Palanas")
rose <- subset(fish, name == "Poroc Rose")
flower <- subset(fish, name == "Poroc San Flower")
sitio <- subset(fish, name == "Sitio Baybayon")
tamakin <- subset(fish, name == "Tamakin Dacot")
visca <- subset(fish, name == "Visca")
wang <- subset(fish, name == "Wangag")

# make a list of the site subsets just made
sites <- list(cab, flower, haina, mag, pal, rose, sitio, tamakin, visca, wang)
for (j in 1:length(sites)){
  recapture <- data.frame()
  Y <- as.data.frame(sites[j])
  for(i in 1:nrow(Y)){
    X <- fish[which(fish$capid == Y$capid[i]), ]
    X$L1 <- min(X$size)
    X$L2 <- max(X$size)
    X$tal <- max(X$date) - min(X$date)
    recapture <- rbind(recapture, X[1,])
  }
  # remove duplicate rows
  recapture <- dplyr::distinct(recapture)
  
  # convert tal from days to portions of year
  recapture$tal <- recapture$tal/365
  
  growhamp(L1 = recapture$L1, L2 = recapture$L2, TAL = recapture$tal, Linf = list(startLinf = 15.9, lowerLinf = 13.4, upperLinf = 18.9), K = list(startK = 0.27, lowerK = 0.01, upperK = 1),sigma2_error=list(startsigma2=100,lowersigma2=0.1,uppersigma2=10000),
    sigma2_Linf=list(startsigma2=100,lowersigma2=0.1,uppersigma2=100000),	
    sigma2_K=list(startsigma2=0.5,lowersigma2=1e-8,uppersigma2=10)) 
}


#### CAN'T use the growth because don't have ages and can't use year caught as a proxy for age ###
# # try the regular growth with TAL as age...
# growth(size=fish$size, age = fish$age, Sinf=15.9,K=0.26,t0=-1)
# 
# # create a smaller number for age than year (12=1, 13=2, 14=3, 15=4, 16=5)
# fish$tal <- NA
# fish$tal[fish$age == 12] <- 1
# fish$tal[fish$age == 13] <- 2
# fish$tal[fish$age == 14] <- 3
# fish$tal[fish$age == 15] <- 4
# fish$tal[fish$age == 16] <- 5
# 
# # try growth again
# growth(size=fish$size, age = fish$tal, Sinf=15.9,K=0.26,t0=-0.73)





# # # try it out with their data
# ## Not run: 
# ## Models 1,2 and 3 below are models 1,2, and 4 in Table 4.17 of ##Quinn and Deriso 
# data(trout)
# growhamp(L1=trout$L1,L2=trout$L2,TAL=trout$dt,models=c(1,2,3),
#   method=c("Nelder-Mead","Nelder-Mead","L-BFGS-B"),
#   varcov=c(TRUE,TRUE,TRUE),
#   Linf=list(startLinf=650,lowerLinf=400,upperLinf=800),       
#   K=list(startK=0.30,lowerK=0.01,upperK=1),
#   sigma2_error=list(startsigma2=100,lowersigma2=0.1,uppersigma2=10000),
#   sigma2_Linf=list(startsigma2=100,lowersigma2=0.1,uppersigma2=100000),	
#   sigma2_K=list(startsigma2=0.5,lowersigma2=1e-8,uppersigma2=10))
# 
# ## End(Not run)
# 
# data(pinfish)
# growth(intype=1,unit=1,size=pinfish$sl,age=pinfish$age,
#   calctype=1,wgtby=1,error=1,Sinf=200,K=0.3,t0=-1)
