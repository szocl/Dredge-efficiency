
setwd("C:/Users/oss621/OneDrive - Bangor University/Laptop/MDrive/BLUEFISH/netCDF_data/waveheight")

#read in netcdf slice (tif file)
library(raster)
library(tools)

waveheightfiles <- list.files(pattern= "*.tif")  # tif files created in wave_netcdf.R file
filens <- file_path_sans_ext(waveheightfiles)
list <- strsplit(filens,"_")
id <- sapply(list,tail,1)


as.matrix(id)

# need to lookup id in tacsatf....
tacsatf <- tacsat

df2 <- cbind(id, tacsatf)

# read in the raster to extract wave height value

library(rgdal)


str_name <- paste('waveheight_', df2$id[1], ".tif", sep="")  
imported_raster <- raster(str_name)

#waveheightfiles[i]    ### rasters (.tifs)
#df2[i,1]              ### tow id's



# extract value for point of interest (poi)

#lon_sta <- df2$lon[1] # poi lon
#lat_sta <- df2$lat[1] # poi lat
#xy <- cbind(lon_sta, lat_sta) # bind station coordinates together
#v <- extract(imported_raster, xy) # extract wave height at poi

setwd("C:/Users/oss621/OneDrive - Bangor University/Laptop/MDrive/BLUEFISH/netCDF_data/waveheight")

install.packages("svMisc")
library(svMisc)

#tacsatf <- read.csv("C:/Users/oss621/OneDrive - Bangor University/Laptop/MDrive/BLUEFISH/VMS & Landings/IOM_SCE_1718/tacsatf_windsp.csv")
head(tacsatf)

#tacsatf$wh=NA

for (i in 1:nrow(tacsatf)) {
  
  imported_raster <- raster(paste("waveheight_",tacsatf$wh_id[i],".tif",sep=""))
  lon_sta <- tacsatf$SI_LONG[i] # poi lon
  lat_sta <- tacsatf$SI_LATI[i] # poi lat
  xy <- cbind(lon_sta, lat_sta)
  tacsatf$wh[i]<- extract(imported_raster, xy)
  
  
}
head(tacsatf)
sum(is.na(tacsatf$wh)) # 20106

# NAs begin at tacsatf[11188,]
#35343 - row 10258
imported_raster <- raster(paste("waveheight_",tacsatf$wh_id[18527],".tif",sep=""))
lon_sta <- tacsatf$SI_LONG[18527] # poi lon
lat_sta <- tacsatf$SI_LATI[18527] # poi lat
xy <- cbind(lon_sta, lat_sta)
tacsatf$wh[18527]<- extract(imported_raster, xy) #NA
plot(imported_raster)
points(xy) # raster is in the wrong projection
proj4string(imported_raster)


#test for wind speed raster
imported_raster <- raster(paste("windspeed_",tacsatf$ws_id[18527],".tif",sep=""))
lon_sta <- tacsatf$SI_LONG[18527] # poi lon
lat_sta <- tacsatf$SI_LATI[18527] # poi lat
xy <- cbind(lon_sta, lat_sta)
f <- tacsatf$ws[18527]<- extract(imported_raster, xy)
f
plot(imported_raster)
proj4string(imported_raster)

# now repeat for wave period

tacsatf$wp=NA

# setwd("C:/Users/oss621/OneDrive - Bangor University/Laptop/MDrive/BLUEFISH/netCDF_data/waveperiod")


#str_name <- paste('waveperiod_', tacsatf$wh_id[1], ".tif", sep="")  
#imported_raster <- raster(str_name)

for (x in 1:nrow(tacsatf)) {
  
  
  imported_raster <- raster(paste("waveperiod_",tacsatf$wh_id[x],".tif",sep=""))
  lon_sta <- tacsatf$SI_LONG[x] # poi lon
  lat_sta <- tacsatf$SI_LATI[x] # poi lat
  xy <- cbind(lon_sta, lat_sta)
  tacsatf$wp[x]<- extract(imported_raster, xy)
  
  
}
head(tacsatf)

#write the data to file (as it takes a while for the loops to run!)
write.csv(tacsatf, "C:/Users/oss621/OneDrive - Bangor University/Laptop/MDrive/BLUEFISH/netCDF_data/wh_wp_harmoni_2016.csv")

##########

# now add a habitat category for each iVMS point

