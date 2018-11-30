#=============================================================================#
# Name   : CC_plotSurfaceSerie
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

vars <- read.table('G:/Manon/var_names.csv', sep = ';', header = T)
vars$variables <- as.vector(vars$variables)
vars$main <- as.vector(vars$main)
vars$ylabs <- as.vector(vars$ylabs)

dirpath <- 'G:/Manon/RCP8.5_2011_2100/Avg/'
nc_files <- list.files(path = dirpath, pattern = '.*\\.nc', recursive = T, full.names = T)

nc <- nc_open(nc_files[1])
lon <- ncvar_get(nc, 'lon_rho')[,1]
lat <- ncvar_get(nc, 'lat_rho')[1,]
nc_close(nc)

fechas <- seq(from = as.Date('2011-01-01'), length.out = length(nc_files), by = 'months')
# x11()
# image.plot(lon, lat, vari[,,32,1], zlim = c(15,30))
# map('worldHires', add=T, fill=T, col='gray')

# Huacho lon = -77.6349; lat = -11.1102
# Mancora lon = -81.0733; lat = -4.0912
names_zones <- c('Mancora','Huacho')
lon_in <- c(-81.0733, -77.6349)
lat_in <- c(-04.0912, -11.1102)

for(i in 1:dim(vars)[1]){
  variables <- vars[i,1]
  main <- vars[i,2]
  ylabs <- vars[i,3]
  
  var_name <- variables
  load(file = paste0(dirpath, var_name, '.RData'))
  
  serie_all <- NULL
  for(j in 1:length(lon_in)){
    a <- which.min(abs(lon - lon_in[j])); lon[a]
    b <- which.min(abs(lat - lat_in[j])); lat[b]
    
    get_serie <- vari_serie[a,b,]
    serie_all <- cbind(serie_all, get_serie)
  }
  ylim <- range(serie_all)
  
  png(filename = paste0(dirpath, var_name,'.png'), width = 1050, height = 650, res = 120)
  par(mfrow = c(length(lon_in),1), mar = c(2.,4.5,1.5,1))
  
  for(j in 1:length(lon_in)){
    plot(fechas, serie_all[,j], type = 'l', ylim = ylim, main = '', ylab = '', xlab = '',lwd = 2, yaxt = 'n', font = 2)
    axis(2, font = 2, las = 2)
    mtext(text = ylabs, side = 2, line = 3, font = 2)
    if (j == 1) mtext(text = main, side = 3, line = 0.5, font = 2, adj = 1)
    legend('topleft', legend = names_zones[j], bty = 'n', text.font = 2)
    box(lwd = 2)
  }
  dev.off()
  
  df <- cbind(as.character(fechas), serie_all)
  colnames(df) <- c('Date', names_zones)
  write.table(x = df, file = paste0(dirpath, var_name, '.csv'), sep = ';', row.names = F, col.names = F)
}
