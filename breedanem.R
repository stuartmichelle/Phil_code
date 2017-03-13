# a script to determine how many anemones visited and breeding pairs to predict candidate anems for cecil and john paul spring 2017
#install.packages("chron")

source("conleyte.R")
leyte <- conleyte()

# how many tagged anemones were observed in 2016?
suppressWarnings(dive <- leyte %>% tbl("diveinfo") %>% filter(date > "2016-01-01" & date < "2017-01-01")  %>% collect())
anem <- leyte %>% tbl("anemones")  %>% filter(dive_table_id %in% dive$id) %>% collect()
# nrow(anem) #1316

# how many of those anems have a fish with an orange tail?
orange <- leyte  %>% tbl("clownfish")  %>% filter(anem_table_id %in% anem$anem_table_id & col == "O") %>% collect()

# how many of those anems have a fish without an orange tail but 7cm or larger?
large <- leyte  %>% tbl("clownfish")  %>% filter(anem_table_id %in% anem$anem_table_id & col != "O" & size >= 7.0) %>% collect()

# join the 2 results
combine <- rbind(orange, large)

# remove duplicate anems
nodups <- distinct(combine, anem_table_id) %>% select(anem_table_id)
# nrow(nodups) #331

# select only those anemones
anem2 <- left_join(nodups, anem, by = "anem_table_id") %>% select(anem_table_id, dive_table_id, anemobs, anem_id, old_anem_id)

# remove any anems that do not have an id
anem2 <- anem2[!is.na(anem2$anem_id), ]

# find the sites for anems
suppressWarnings(site <- leyte %>% tbl("diveinfo") %>% filter(id %in% anem2$dive_table_id) %>% select(id, name) %>% collect())

anem2 <- left_join(anem2, site, by = c("dive_table_id" = "id"))

# how many anems at each site?
sites <- as.data.frame(unique(anem2$name))
sites$num_anem <- NA
names(sites) <- c("site", "num_anem")
for(i in 1:nrow(sites)){
  sites$num_anem[i] <- sum(anem2$name == sites$site[i])
}

# how many dives for each site, assuming 5 anemones per dive?
sites$dives <- ceiling(sites$num_anem/5)

# how many dives total?
tot <- sum(sites$dives)

# what proportion is each site?
sites$prop <- sites$dives/tot

write.csv(sites, file = paste(Sys.Date(), "anems_by_site.csv", sep = ""), row.names = F)

# generate random list of site visits
# use randomediveCal.R
# This script will generate a random calendar csv for dives ---------------

# for the sites where we do more than one dive, fill in the number of dives you want to do in the rep list (for example, to do 6 dives at Palanas <- rep("Palanas", 6))

start_date <- "03/14/2017"
end_date <- "04/21/2017"
# no_dive_dates <- NULL # days off ex: "06/17/2016"
agenda <- as.character(rep(sites$site, sites$dives))

dts <- chron::seq.dates(start_date, end_date) # a list of all the dates from start to end
weekdts <- weekdays(dts) # converts those dates to days of the week, Tue, Wed, etc.
dates <- dts[weekdts!="Sat"& weekdts!="Sun"] # remove weekends and days off, returns a list of numerical dates
dates <- sort(c(dates,dates)) # doubles all of the days for 2 dives per day (sort(c(dates, dates, dates))) for 3 dives per day
len_dates <- length(dates) # the length of the list (number of values)
len_agenda <- length(agenda) # the length of the list (number of values)
mult <- floor(len_dates/len_agenda) # divide number of slots by number of people and round down (floor)
dive_sched <- NA # create an empty object
# for(i in 1:mult){
  # temp<-sample(agenda, len_agenda,replace=FALSE) # take a sample of the site pool len_sites number of times
  # dive_sched<-c(dive_sched, temp)
# }

# want to use the plan we came up with earlier
dive_sched[1] <- "Palanas"
dive_sched[3:4] <- "Wangag"
dive_sched[5] <- "Hicgop South"
dive_sched[6] <- "Caridad Cemetary"
dive_sched[7:8] <- "Magbangon"
dive_sched[9:10] <- "Tamakin Dacot"
dive_sched[11] <- "Visca"
dive_sched[12] <- "Gabas"
dive_sched[13] <- "Poroc Rose"
dive_sched[14] <- "Poroc San Flower"
dive_sched[15:16] <- "Palanas"
dive_sched[17:18] <- "Wangag"
dive_sched[19:20] <- "Magbangon"
dive_sched[21:22] <- "Sitio Baybayon"
dive_sched[23:24] <- "Magbangon"
dive_sched[25] <- "Elementary School"
dive_sched[26] <- "San Agustin"
dive_sched[27:28] <- "Sitio Baybayon"
dive_sched[29:30] <- "Palanas"
dive_sched[31:32] <- "Cabatuan"
dive_sched[33:34] <- "Palanas"
dive_sched[35:36] <- "Haina"
dive_sched[37:38] <- "Palanas"
dive_sched[39:40] <- "Magbangon"
dive_sched[41:42] <- "Wangag"
dive_sched[43:44] <- "Sitio Baybayon"
dive_sched[45:46] <- "Wangag"
dive_sched[47:48] <- "Tamakin Dacot"
dive_sched[49:50] <- "Palanas"
dive_sched[51:52] <- "Wangag"
dive_sched[53:54] <- "Palanas"
dive_sched[55] <- "Palanas"
dive_sched[56] <- "Magbangon"
dive_sched[57:58] <- "Sitio Baybayon"


# add to google calendar
# nms is the format specified by Google calendars for csv import
# nms<-c('Subject','Start Date','Start Time','End Date','End Time','All Day Event','Description','Location','Private')
nms<-c('Subject','Start Date','End Date','All Day Event','Description','Private')

len_names <- length(nms)
mat <- matrix(nrow=len_dates,ncol=len_names) # create a matrix that is the number of rows for each date and the number of columns for each of the google cal fields
mat <- data.frame(mat) # convert the matrix to a data frame
colnames(mat)<-nms # name the data frame columns the google cal names
mat$Subject <- dive_sched
mat$"Start Date" <- dates
mat$"End Date" <- dates
mat$"All Day Event" <- "TRUE"
mat$Description <- "Breeding Dive Schedule"
mat$Private <- "FALSE"
# start_times <- c("12:15:00 PM","2:30:00 PM") # all day event
# end_times <- c("1:15:00 PM","3:20:00 PM") # all day event
# mat$"Start Time" <- start_times # all day event
# mat$"End Time" <- end_times # all day event
# mat$Location <- ta_location # site name is location

# This loop makes a csv for each site with only the date of that s --------

# for(i in 1:len_sites){
#   filename<-paste(sites[i],".csv",sep="")
#   temp<-mat[mat$Subject==sites[i],]
#   write.csv(temp,file=filename,quote=FALSE,row.names=FALSE)
# }
filename <- "BreedingDiveSchedule.csv"
write.csv(mat, file=filename, quote=FALSE, row.names=FALSE)
# open the google calendar webpage and choose import from other calendars
