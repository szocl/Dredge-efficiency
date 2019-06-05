rm(list=ls())

# read in iVMS file
# add location column (Isle of Man/ Wales/ other - 0-3, 3-12, outside)
# Define individual trips and assign a TRIP_ID
# add column for number of dredges used, based on location
# remove data between May and October in the Isle of Man
# check with Mark data for other areas is all king scallops
# 

# create manual table of bags per tow by tow date time
# Assign TRIP_ID, based on date and iVMS data
# calculate tow length (metres) and area fished M2) using tow in/out times and coordinates
# link iVMS and logbook data to calculate weight of scallops caught per m2 (standard bag weight??)

setwd("C:/Users/oss621/OneDrive - Bangor University/Laptop/MDrive/BLUEFISH/VMS & Landings/iVMS_data")

vmstoolsPackages <-
  c("cluster","data.table","doBy","maps","mapdata","maptools","PBSmapping","sp")
for(i in vmstoolsPackages) try(install.packages(pkgs=i,repos=getOption("repos")))

install.packages("C:/Users/oss621/OneDrive - Bangor University/Laptop/Documents/R/win-library/3.4/vmstools.zip")

library(vmstools) 
library(stringr)

## read in the data and format

tacsat <- read.csv("Harmoni 16_17.csv")

# rename the columns
colnames(tacsat)
colnames(tacsat) <- c("VE_REF", "SI_DATE", "SI_LONG", "SI_LATI", "SI_SP", "SI_HE", "SI_SOURCE", "poll.id", "received.date")
head(tacsat)

# remove unnecessary columns
tacsat <- subset(tacsat[,-(7:8)])

str(tacsat)
summary(tacsat) # the data contains 7769 rows


## create seperate date/time columns so you can see how many days fishing occurs

# convert SI_DATE from factor to character
tacsat$SI_DATE <- str_sub(as.character(tacsat$SI_DATE),1,str_length(as.character(tacsat$SI_DATE)))
#add the :00 so in correct format
tacsat$SI_DATE <- paste0(tacsat$SI_DATE, "", ":00", sep="")
class(tacsat$SI_DATE)

#Convert to date
tacsat$SI_DATE <- as.POSIXct(tacsat$SI_DATE, format = "%d/%m/%Y %H:%M:%S", tz= "GMT")
# create date and time columns
tacsat$date <- as.Date(tacsat$SI_DATE, format="%d/%m/%Y")
tacsat$time <- strftime(tacsat$SI_DATE, format="%H:%M:%S")
head(tacsat)

unique(tacsat$date) # 54 days



## add location for each tow

# read in a shapefile for IoM territorial sea
#read in 3-12 shapefile

library(rgdal)

IOM312nm <- readOGR("C:/Users/oss621/OneDrive - Bangor University/Laptop/MDrive/BLUEFISH/VMS & Landings/IOM_SCE_1718/VMS/IoM_3_12M_marlim_Arc_R.shp",verbose=TRUE)

#read in IOM shapefile
IOM <- readOGR("C:/Users/oss621/OneDrive - Bangor University/Laptop/MDrive/BLUEFISH/VMS & Landings/IOM_SCE_1718/VMS/IOM_WGS.shp",verbose=TRUE)


#Convert the spatial polygons data frame to spatial polygons
landPOLS <- as(IOM,"SpatialPolygons") #hmm, that's easy and simple...
terr <- as(IOM312nm, "SpatialPolygons")

tacsat_sp <- tacsat # create a copy of the data
coordinates(tacsat_sp) <- c("SI_LONG", "SI_LATI") # retains the data slot
str(tacsat_sp) # 7769 records
# add an ID column
tacsat_sp@data$id <- 1:nrow(tacsat_sp)
head(tacsat_sp@data$id)


proj4string(landPOLS)
proj4string(terr)
proj4string(tacsat_sp) <- proj4string(landPOLS)


plot(tacsat_sp)
plot(landPOLS, add=TRUE)
plot(terr, add=TRUE)

# check no points on land etc

# 5-aside dredge are permittted in 0-3 nm zone
# 7-aside dredges are permitted in 3-12nm zone


pts_in <- tacsat[!is.na(over(tacsat_sp,landPOLS)),] 
head(pts_in) # 0 records - so assume none on land, unless the code did not work?

plot(landPOLS)
#plot(pts_in, add = T)

pts_out <- tacsat[is.na(over(tacsat_sp,landPOLS)),] #7769
head(pts_out)


#tacsat <- as.data.frame(pts_out) # only need to run if there were points on land
# now identify points in 0-3 and 3-12 zone....

# now see which points are in the 3-12 zone

#pts_in <- tacsat_sp[!is.na(over(tacsat_sp, terr)),]
#pts_in <- tacsat_sp[!is.na(raster::intersect(tacsat_sp, terr)),]
pts_in <- raster::intersect(tacsat_sp, terr)
proj4string(pts_in)
plot(pts_in)
plot(terr, add=T)
pts_in@data$location <- "3..12"

ptsin_id <- unique(pts_in@data$id) # 1502 points in the 3-12 zone

# and which points are out of the 3-12 zone...
# 53.921642 # latitude for N/S divide between points outside terr and those in 0-3nm

pts_out <- tacsat_sp[is.na(over(tacsat_sp,terr)),] #points outside terr sea or in 0-3 zone
head(pts_out) # 6267 records
plot(pts_out)
plot(landPOLS, add=T)

pts_out@data$location <- as.character("0-3orOUT")
class(pts_out@data$location) # "character"

pts_out_copy <- as.data.frame(pts_out, stringsAsFactors = FALSE)
head(pts_out_copy)
str(pts_out_copy)


min(pts_out_copy$SI_LATI) #52.53617
max(pts_out_copy$SI_LATI) #54.25509


for (i in 1:nrow(pts_out_copy)) {
if(pts_out_copy$SI_LATI[i] > 53.921642)
   { pts_out_copy[i, "location"] <- "0..3" }
else if (pts_out_copy$SI_LATI[i] <= 53.921642)
   { pts_out_copy[i, "location"] <- "out"}
}

unique(pts_out_copy$location) # [1] "0..3" "out"
head(pts_out_copy)
sum(is.na(pts_out_copy$location)) # 0 NAs

pts_out <- pts_out_copy



# and join the points_in data
pts_in <- as.data.frame(pts_in, stringsAsFactors = FALSE)

#join the two datasets together
new <- rbind(pts_in, pts_out)
class(new)
str(new)
head(new)
unique(new$location) # "0..3"  "out"  "3..12"

# check the data
nrow(new)
head(new)
unique(new$location)  
new$dredges <- as.numeric(0)
class(new$dredges)
  
# Add a column for dredge number and assign based on location 
##### THIS MAY VARY BY VESSEL _ double check interview notes

for (i in 1:nrow(new)) {
  if(new$location[i] == "0..3") 
    { new[i, "dredges"] <- 10 }
  else if(new$location[i] == "3..12") 
    { new[i, "dredges"] <- 14 }
  else if(new$location[i] == "out")  
    { new[i, "dredges"] <- 16 } ### check with Mark if this is correct for rest of Irish Sea
}

unique(new$dredges) # 10 16 14

##### now to look at vmstools functions #####
##### Can restart at this point with 'new' file if script below errors

tacsat <- new


head(tacsat)
colnames(tacsat) <- c("VE_REF", "TIME_DATE", "SI_LONG", "SI_LATI", "SI_SP", "SI_HE", "received.date", "SI_DATE", "SI_TIME", "id", "location", "dredges")

#coordinates(tacsat) <- c("SI_LONG", "SI_LATI")
#plot(tacsat)
#plot(IOM, add=TRUE)

tacsat <- formatTacsat(tacsat)

# check for duplicate records

tacsat$SI_DATIM <- as.POSIXct(paste(tacsat$SI_DATE,tacsat$SI_TIME,sep=" "), 
                              tz="GMT", format="%Y-%m-%d  %H:%M:%S") # create a proper date-time stamp column by 'pasting' together VE_REF, the location and the date/time columns. This makes it straightforward to check for duplicates.
uniqueTacsat    <- paste(tacsat$VE_REF,tacsat$SI_LATI,tacsat$SI_LONG,tacsat$SI_DATIM) 
uniqueTacsat[1:5] # check what the paste function did.

print(nrow(tacsat))

tacsat          <- tacsat[!duplicated(uniqueTacsat),] #get rid of the duplicates
print(nrow(tacsat)) # 700 duplicate records have been removed

# already checked for points on land (above)

# check speed frequencies and remove speeds over 4 knots (steaming, based on histogram of values)

min(tacsat$SI_SP) # 0
max(tacsat$SI_SP) # 12.24

hist(tacsat$SI_SP,breaks=100) #are there any outlying results?
spThres <- 4 #I decide that 4knots is the maximum speed allowed in the dataset
idx <- which(tacsat$SI_SP > spThres)
tacsat <- tacsat[-idx,]
# and min speed for fishing is 2 knots
spThres <- 2
idx <- which(tacsat$SI_SP < spThres)
tacsat <- tacsat[-idx,]

print(nrow(tacsat)) # 1618 (this covers 27 days of fishing)
# 1618/27 = 60 records per day 
# each record is approx 10 mins apart - 600 mins in total - 10 hours of fishing on average - this seems correct

# remove points in harbours

data(euharbours)
#note linux users please use data(harbours)
idx <- pointInHarbour(tacsat$SI_LONG,tacsat$SI_LATI,harbours,saveHarbourList=F)
pih <- tacsat[which(idx==1),]
table(idx)
#Overwrite the tacsat file with only those records which are not in harbour
tacsat <- tacsat[which(idx==0),]


library(maps);library(mapdata)
X11()
map("worldHires",res=0,fill=T,col="darkgreen",xlim=c(-8,0),ylim=c(52,55)); map.axes()
points(x=pih$SI_LONG,y=pih$SI_LATI,col="red",pch=19,cex=0.5) # seems to remove a point in the middle of the Irish Sea - strange!

# remove records with time intervals <5 mins

tacsat <- sortTacsat(tacsat) #sort the data by vessel and time
tacsatp<- intervalTacsat(tacsat,level="vessel",fill.na=T)

hist(tacsatp$INTV,breaks=1000,xlim=c(0,50)) # apparently there is a large number of records with a very small interval rate (close to zero)

#remove interval rates lower than 5 minutes ??
#intThres <- 5
#tacsat   <- tacsatp[which(tacsatp$INTV > intThres),]
tacsat <- tacsatp
head(tacsat)

unique(tacsat$SI_DATE)
coordinates(tacsat) <- c("SI_LONG", "SI_LATI")

# save the data as a shapefile
#writeOGR(tacsat, dsn = "C:/Users/oss621/OneDrive - Bangor University/Laptop/MDrive/BLUEFISH/VMS & Landings/iVMS_data/shapefiles", layer = "Harmoni_1617_all", driver = "ESRI Shapefile")

# now need to manually enter logbook data!!!

#tacsatdec18 <- subset(tacsat, tacsat$SI_DATE=="2016-12-18") 
#head(tacsatdec18)
#tail(tacsatdec18)

#entered all Harmoni data for 2016 
#now link to VMS data by date/time

# to start with analyse one day of data from iVMS and DCR (daily catch return)

# create shapefile so can check dates/times in ArcMap
# writeOGR(tacsatdec15, dsn = "C:/Users/oss621/OneDrive - Bangor University/Laptop/MDrive/BLUEFISH/VMS & Landings/iVMS_data/shapefiles", layer = "tacsatdec15", driver = "ESRI Shapefile")

# I think I have the 24hr times correct
# recreate the tacsat15 data

#tacsatdec18 <- subset(tacsat, tacsat$SI_DATE=="2016-12-18") 
#head(tacsatdec18)
#nrow(tacsatdec18) #144 rows

#coordinates(tacsatdec18) <- c("SI_LONG", "SI_LATI")
#writeOGR(tacsatdec18, dsn = "C:/Users/oss621/OneDrive - Bangor University/Laptop/MDrive/BLUEFISH/VMS & Landings/iVMS_data/shapefiles", layer = "tacsatdec18", driver = "ESRI Shapefile")


dcr_dec <- read.csv("C:/Users/oss621/OneDrive - Bangor University/Laptop/MDrive/BLUEFISH/VMS & Landings/iVMS_data/logbooks/Harmoni_log_2016.csv")
names(dcr_dec)
# [1] "Date"     "tow"      "time_out" "bags"     "WEIGHTKG"



# add a time interval to the logbook data, for later calculations
# create a date/time column

dcr_dec$DateTime <- paste0(dcr_dec$Date, " ", dcr_dec$time_out, sep="")
class(dcr_dec$DateTime) # character
#Convert to date
dcr_dec$DateTime <- as.POSIXct(dcr_dec$DateTime, format = "%d/%m/%Y %H:%M", tz= "GMT")
class(dcr_dec$DateTime) # "POSIXct" "POSIXt" 

dcr_dec$INDEX <- 0

dcr_dec$tdiff <- unlist(tapply(dcr_dec$DateTime, INDEX = dcr_dec$INDEX, FUN = function(x) c(0, `units<-`(diff(x), "mins"))))
#plot the values to identify non-fishing threshold
#dcr_xxx <- subset(dcr_dec, dcr_dec$tdiff <500)
#hist(dcr_xxx$tdiff, breaks = 20)

# if tdiff >150, replace with the next value (as this is the start of a fishing trip and assume that the first tow is the same length as the second tow)
# the code below is not working, so write the data to file and do this manually for now!!


library(data.table)

for (i in 1:nrow(dcr_dec)) {
  if(dcr_dec$tdiff[i] > 150)
  { data.table::shift(dcr_dec$tdiff[i], n=1L, fill=NA, type="lead", give.names=FALSE) }
  else if (dcr_dec$tdiff[i] <= 150)
  { dcr_dec$tdiff[i] <- dcr_dec$tdiff[i]}
}

### now, this is not working
### try something else

library(dplyr)
data.frame(dcr_dec$tdiff, dplyr::lag(dcr_dec$tdiff))

for (i in 1:nrow(dcr_dec)) {
  if(dcr_dec$tdiff[i] > 150)
  { dplyr::lag(dcr_dec$tdiff[120]) }
  else { dcr_dec$tdiff[i] <- dcr_dec$tdiff[i]}
}


# data.table::shift(dcr_dec$tdiff, n=1L, fill=NA, type="lead", give.names=FALSE) 
# this works! now put in loop...

# not working, save to Excel - fill values manually and re-load data

dcr_dec <- read.csv("C:/Users/oss621/OneDrive - Bangor University/Laptop/MDrive/BLUEFISH/VMS & Landings/iVMS_data/logbooks/Harmoni_log_2016_2.csv")


     
#write.csv(dcr_dec, "C:/Users/oss621/OneDrive - Bangor University/Laptop/MDrive/BLUEFISH/VMS & Landings/iVMS_data/logbooks/Harmoni_log_2016_2.csv")


# read in the data again

dcr_dec <- read.csv("C:/Users/oss621/OneDrive - Bangor University/Laptop/MDrive/BLUEFISH/VMS & Landings/iVMS_data/logbooks/Harmoni_log_2016_2.csv")

colnames(dcr_dec)

dcr_dec$towtime <- dcr_dec$tdiff_2-10
# remove tdiff column and X column
dcr_dec <- dcr_dec[,-1]
dcr_dec <- dcr_dec[,-8]

# create a tow-start column
# convert tow length to hours

library(chron)

mins <- dcr_dec$towtime
#days <- mins %/% (24 * 60)
#dcr_dec$ct <- as.POSIXlt(mins, format = "%d/%m/%Y %H:%M:%S")
head(dcr_dec)

# substract tow time in seconds
dcr_dec$time_in <- as.POSIXct(dcr_dec$DateTime, format = "%d/%m/%Y %H:%M") - as.integer(dcr_dec$towtime*60)

# rename DateTime column (first remove 'time_out' column)
dcr_dec <- subset(dcr_dec, select=-c(time_out))
head(dcr_dec)

colnames(dcr_dec)[colnames(dcr_dec)=="DateTime"] <- "time_out"

# manually correct the first row 'time_in' and 'towtime'
dcr_dec$time_in[1] <- as.POSIXct("2016-11-07 07:10:00 GMT")
dcr_dec$towtime[1] <- 50

min(dcr_dec$towtime) # 20

head(dcr_dec)

#########################################

# now to link the VMS and logbook data, by merging all iVMS records that lie within the time_in and time_out values

class(dcr_dec$time_in) # "POSIXct" "POSIXt" 
class(dcr_dec$time_out) # factor
dcr_dec$time_out <- as.POSIXct(dcr_dec$time_out, format = "%d/%m/%Y %H:%M")

# tacsat should be a data.frame (not a spatial object) (I think!!)

class(tacsat)
tacsat <- as.data.frame(tacsat)
colnames(tacsat)

tacsat <- tacsat[, -10] # remove the id column
tacsat$ID <- paste0(tacsat$SI_LONG, " ", tacsat$SI_LATI, sep = "") # create a new ID column using lat/long as the ref


https://www.r-bloggers.com/in-between-a-rock-and-a-conditional-join/

#install.packages("sqldf") 
#install.packages("RSQLite")
#install.packages("digest")
library(sqldf)
library(RSQLite)
library(digest)

colnames(tacsat)
#sqldf('SELECT Record, SomeValue, ValueOfInterest 
#      FROM myData 
#      LEFT JOIN linkTable ON SomeValue BETWEEN LowerBound and UpperBound')

final <- sqldf('SELECT ID, SI_DATIM, WEIGHTKG 
      FROM tacsat 
      LEFT JOIN dcr_dec ON SI_DATIM BETWEEN time_in and time_out')

# Need to repeat this to get the tow time (in minutes)

finala <- sqldf('SELECT ID, SI_DATIM, towtime 
      FROM tacsat 
      LEFT JOIN dcr_dec ON SI_DATIM BETWEEN time_in and time_out')


tacsatf <- merge(tacsat, final, by="ID")
colnames(tacsatf)
#[1] "ID"            "VE_REF"        "TIME_DATE"     "SI_LONG"       "SI_LATI"      
#[6] "SI_SP"         "SI_HE"         "received.date" "SI_DATE"       "SI_TIME"      
#[11] "location"      "dredges"       "SI_DATIM.x"    "INTV"          "SI_DATIM.y"   
#[16] "WEIGHTKG"
#remove unwanted columns
tacsatf <- tacsatf[, -15]

tacsatf <- merge(tacsatf, finala, by="ID")
colnames(tacsatf)
tacsatf <- tacsatf[, -16]
tacsatf <- tacsatf[, -8]
head(tacsatf)


sum(is.na(tacsatf$WEIGHTKG)) 
246/1617 # 0.15 or 15%
# 254 iVMS records (16%) have no CPUE value - this is equivalent to 10 mins in every hour. Assume these are non-fishing points, so remove.
# Then can add wind speed and wave height to the data! Almost there...!!

tacsatf <- na.omit(tacsatf) # remove NA records


# need to calculate area swept per tow and CPUE. As the iVMS data does not give clear gear in/out times, calculate CPUE from mean fishing speed (2.97 knots)
hist(tacsatf$SI_SP)
mean(tacsatf$SI_SP) # 2.97

# Calculate CPUE
# 2.97 knots (average speed in dataset) is equal to 1.5279 metres per second
# calculate swept area based on total dredges (dredges*0.76m*((1.5279*60)*time))
tacsatf$sweptarea <- (tacsatf$dredges*0.76)*((1.5279*60)*tacsatf$towtime) # swept area in metres squared
tacsatf$CPUE <- tacsatf$WEIGHTKG / tacsatf$sweptarea

min(tacsatf$CPUE) # 0
max(tacsatf$CPUE) # 0.0133 - 1.3 scallops per 100m2 (pretty average for Isle of Man!)



# write the data to file as a dataframe....
write.csv(tacsatf, "C:/Users/oss621/OneDrive - Bangor University/Laptop/MDrive/BLUEFISH/VMS & Landings/iVMS_data/CPUE files/Harmoni_CPUE_2016.csv")

#.....and a shapefile
coordinates(tacsatf) <- c("SI_LONG", "SI_LATI")
writeOGR(tacsatf, dsn = "C:/Users/oss621/OneDrive - Bangor University/Laptop/MDrive/BLUEFISH/VMS & Landings/iVMS_data/shapefiles", layer = "Harmoni_CPUE_16", driver = "ESRI Shapefile")

as.data.frame(tacsatf)
head(tacsatf)


###### notes

# there are hardly any low speed values in the VMS, so a standard analysis would classify all time as fishing, whereas each haul and deployment takes c. 10 mins.
# in this case, I have calculated the distance of tow by ((time interval)-10 mins) * meanfishingspeed





# extract a habitat value for each tow (take a mean of all lat/longs for the tow??)

# need to extract habitat data for each tow (midpoint, based on start/end coordinates)

# where tow overlaps midnight, always assign to the next day? (not perfect but makes things more simple - state this in methods?)

