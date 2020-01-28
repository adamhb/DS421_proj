#this script unzips each annual mtbs burn severity tif file from within the nested file structure that is downloaded from the mtbs.org webiste (annual mosaics). It places the unzipped tif files all in the same folder.

#Part 2. It also 


library(raster)
library(sf)


setwd("C:/Users/ahanb/OneDrive/Documents/DS421/project/data")

klamath_ecoregion <- shapefile('klamath_ecoregion.shp')

base_workingD <- "C:/Users/ahanb/OneDrive/Documents/DS421/project/data/mtbs_burn_severity_mosaics_1984_2017/composite_data/MTBS_BSmosaics/"

for(year in 1984:2017){
  
  year_char <- as.character(year)
  
  setwd(paste0(base_workingD,year_char))
  
  unzip(paste0('mtbs_CA_',year_char,'.zip'),
        exdir = "C:/Users/ahanb/OneDrive/Documents/DS421/project/data/mtbs_burn_severity_mosaics_1984_2017")
}




#Part 2. Getting a polygon and extent object of the study area. The study area is the Klamath Mountains ecoregion with a 55 km buffer around it to capture all fires that touch the Klamath Mountains ecoregion.
setwd( "C:/Users/ahanb/OneDrive/Documents/DS421/project/data/mtbs_burn_severity_mosaics_1984_2017")


files <- list.files(pattern = '.tif$')

r_1984 <- raster('mtbs_CA_1984.tif')
r_1994 <- raster('mtbs_CA_1994.tif')

NAD83 <- crs(r_1984)

klamath_ecoregion_NAD83 <- st_transform(x = st_as_sf(klamath_ecoregion), crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0
+y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")


#this defines the study area
klamath_fire_extent <- extent(st_buffer(klamath_ecoregion_NAD83, dist = 55000))
#these extent corners are then used in the next script (a gdal script) to define the bounding area of the stacked tif files.















