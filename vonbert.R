# test the growth features of the fishmethods package

library(fishmethods)

# try it with our data
source("conleyte.R")
leyte <- conleyte()
dive <- leyte %>% tbl("diveinfo") %>% select(id, date) %>% collect()
anem <- leyte %>% tbl("anemones") %>% select(dive_table_id, anem_table_id) %>% collect()
anem <- left_join(anem, dive, by = c("dive_table_id" = "id"))
rm(dive)
fish <- leyte %>% tbl("clownfish") %>% filter(!is.na(capid)) %>% select(capid, size, sample_id, anem_table_id) %>% collect()
fish <- left_join(fish, anem, by = "anem_table_id")
rm(anem)
fish$anem_table_id <- NULL
fish$dive_table_id <- NULL
fish$date <- as.Date(fish$date)



fish$capid[is.na(fish$size)] # should be 1, 9
fish$capid[fish$capid == 1] <- NA
fish$capid[fish$capid == 9] <- NA
fish <- fish[!is.na(fish$capid), ]

# populate an L1 and L2, and TAL column for recaptured fish
recapture <- data.frame()
for(i in 1:nrow(fish)){
  X <- fish[which(fish$capid == fish$capid[i]), ]
  X$L1 <- min(X$size)
  X$L2 <- max(X$size)
  X$tal <- max(X$age) - min(X$age)
  recapture <- rbind(recapture, X[1,])
}

# remove duplicate rows
recapture <- dplyr::distinct(recapture)

# select only the pertinent columns
calc <- recapture[ , c("L1", "L2", "tal")]

# which(calc$L2 > calc$L1) # verify that all second recaptures are larger than initial captures

growhamp(L1 = calc$L1, L2 = calc$L2, TAL = calc$tal, Linf = list(startLinf = 15.9, lowerLinf = 13.4, upperLinf = 18.9), K = list(startK = 0.27, lowerK = 0.01, upperK = 1), models=c(1,2,3),
  method=c("Nelder-Mead","Nelder-Mead","L-BFGS-B"),
  varcov=c(TRUE,TRUE,TRUE),sigma2_error=list(startsigma2=100,lowersigma2=0.1,uppersigma2=10000),
  sigma2_Linf=list(startsigma2=100,lowersigma2=0.1,uppersigma2=100000),	
  sigma2_K=list(startsigma2=0.5,lowersigma2=1e-8,uppersigma2=10))

growhamp(L1 = recapture$L1, L2 = recapture$L2, TAL = recapture$tal, Linf = list(startLinf = 15.9, lowerLinf = 13.4, upperLinf = 18.9), K = list(startK = 0.27, lowerK = 0.01, upperK = 1),sigma2_error=list(startsigma2=100,lowersigma2=0.1,uppersigma2=10000),
  sigma2_Linf=list(startsigma2=100,lowersigma2=0.1,uppersigma2=100000),	
  sigma2_K=list(startsigma2=0.5,lowersigma2=1e-8,uppersigma2=10))


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
