#=============================================================================#
# Name   : CC_getSurfaceSerie
# Author : Jorge Flores & Jorge Tam
# Date   : 
# Version:
# Aim    : 
# URL    : 
#=============================================================================#
library(ncdf4)
library(fields)
library(maps)
library(mapdata)

dirpath <- 'G:/Manon/RCP8.5_2011_2100/Avg/'
# vari_name <- read.table('')


nc_files <- list.files(path = dirpath, pattern = '.*\\.nc', recursive = T, full.names = T)

nc <- nc_open(nc_files[1])
lon <- ncvar_get(nc, 'lon_rho')[,1]
lat <- ncvar_get(nc, 'lat_rho')[1,]
vari <- ncvar_get(nc, 'temp'); dim(vari)
nc_close(nc)
# 
# x11();image.plot(lon, lat, vari[,,32,1], zlim = c(15,30))
# map(add = T)
# 
# lon_in <- -82
# a <- which.min(abs(lon - lon_in))
# lon[a]
# 
# lat_in <- -6.5
# b <- which.min(abs(lat - lat_in))
# lat[b]

# vari_name <- c('temp','salt','O2','CACO3','NANO','ZOO','MESO','NCHL','DCHL','NO3','NH4', 'DIA')
vari_name <- 'DIA'

year_in <- 2011
year_on <- 2100
for(var_ind in vari_name){
  
  vari_serie <- array(dim = c(length(lon), length(lat), ((year_on-year_in+1)*12) ))
  count <- 0
  for(year in year_in:year_on){
    for(month in 1:12){
      nc_name <- paste0(dirpath, 'roms_avg_Y',year, 'M', month,'.nc')
      nc <- nc_open(nc_name)
      
      vari <- ncvar_get(nc, var_ind)[,,32,]
      vari <- apply(X = vari, MARGIN = c(1,2), FUN = mean)
      
      count <- count + 1
      vari_serie[,,count] <- vari
      nc_close(nc)
      print(nc_name)
    }
  }
  save(vari_serie, file = paste0(dirpath, var_ind, '.RData'))
}

