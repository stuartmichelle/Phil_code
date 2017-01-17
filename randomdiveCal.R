
# This script will generate a random calendar csv for dives ---------------

# for the sites where we do more than one dive, fill in the number of dives you want to do in the rep list (for example, to do 6 dives at Palanas <- rep("Palanas", 6))

start_date <- "02/15/2017"
end_date <- "03/29/2017"
no_dive_dates <- NULL # days off ex: "06/17/2016"
Palanas <- rep("Palanas", 6)
Wangag <- rep("Wangag", 6)
Magbangon <- rep("Magbangon", 6)
TamakinDacot <- rep("Tamakin Dacot", 2)
Haina <- rep("Haina", 2)
SitioBaybayon <- rep("Sitio Baybayon", 6)
# Transects <- rep("Transects", 20) # this is a hold all for the remainder of dives, includes extra cushion days

sites <- c("Data Logger",Transects, Palanas, Wangag, Magbangon, "Cabatuon", "Caridad Cemetary", "Caridad", "Hicgop", "Hicgop South", "Elementary School", "Sitio Tugas", "Sitio Lonas", "San Agustin", "Poroc San Flower", "Poroc Rose", "Visca", "Gabas", TamakinDacot, Haina, SitioBaybayon)

dts <- seq.dates(start_date, end_date) # a list of all the dates from start to end
weekdts <- weekdays(dts) # converts those dates to days of the week, Tue, Wed, etc.
dates <- dts[weekdts!="Wed"&!as.character(dts)%in%no_dive_dates] # remove weekends and days off, returns a list of numerical dates
# dates <- c(dates,double_days) # doubles the days when you need 2 people
dates <- sort(c(dates,dates)) # doubles all of the days for 2 dives per day (sort(c(dates, dates, dates))) for 3 dives per day


len_dates <- length(dates) # the length of the list (number of values)
len_sites <- length(sites) # the length of the list (number of values)
mult <- floor(len_dates/len_sites) # divide number of slots by number of people and round down (floor)
temp <- rep(NA, len_tas) # make a list of repeated NAs as long as len_tas
dive_sched <- NA # create an empty object
for(i in 1:mult){
  temp<-sample(sites, len_sites,replace=FALSE) # take a sample of the site pool len_sites number of times
  dive_sched<-c(dive_sched, temp)
}

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
  mat$Description <- "Philippines Dive Schedule"
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
  filename <- "LeyteDiveSchedule.csv"
  write.csv(mat, file=filename, quote=FALSE, row.names=FALSE)
