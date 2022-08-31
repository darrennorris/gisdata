#
#crop mapbiomas data
library(plyr)
library(tidyverse)
library(terra)
library(sf)
library(readxl)
memory.limit(30000)

#Municipality polygons
longname <- "vector/brazil_ninestate_municipalities/ninestate_muni.shp"
sf_munis <- sf::st_read(longname) 

#works one folder at a time
get_files <- function(folder_name = NA) {
  library(tidyverse)
  folder_location <- folder_name
  in_files <- list.files(folder_location, 
                         pattern = ".tif", full.names = TRUE)
  data.frame(folder_id = folder_location, file_id = in_files) %>%  
    group_by(folder_id, file_id) %>% 
    summarise(file_count = n()) %>% 
    ungroup() -> df_muni_tif
  return(df_muni_tif)
}
infolder <- "mapbiomas_ge/state_cover/AP"
df_muni_tif <- get_files(folder_name = infolder)
#update
df_muni_tif %>% 
  mutate(state_code = str_sub(folder_id, -2, -1), 
         ) -> df_muni_tif
#extent from 50 km arouund river upstream of cachoeira caldeirao
meuSIG <- file.choose()
#"C:\\Users\\Darren\\Documents\\gisdata\\vector\\rivers.GPKG"
rsl <- st_read(meuSIG, layer = "centerline")
rsl_50km <- st_union(st_buffer(rsl, dist=50000))
myexent <- ext(vect(rsl_50km)) 

state_proj <- function(x, state_id = NA, 
                        sf_state = NA) {
  library(plyr)
  library(tidyverse)
  library(terra)
  library(sf)
  library(stringi)
  
  state_sigla = x$state_code 
  rin <- x$file_id 
  rbig <- terra::rast(rin)
  layer_name <- names(rbig)
  layer_year <- stri_sub(layer_name,-4,-1)
  #States
  #sf_state %>% 
  #  filter(SIGLA_UF == state_sigla)  %>% 
  #  st_transform(crs = crs(rbig)) -> sf_state
  #e2 <- ext(vect(sf_state)) 
  
  #garimpo <- data.frame(nome = "garimpo do Lourenço", 
  #                      coord_x = -51.630871, 
  #                      coord_y = 2.318514)
  #Converter para objeto espacial
  #sf_garimpo <- st_as_sf(garimpo, 
   #                      coords = c("coord_x", "coord_y"),
   #                      crs = 4326)
  #sf_garimpo_utm <- st_transform(sf_garimpo, crs = 31976)
  #sf_garimpo_20km <- st_buffer(sf_garimpo_utm, dist=20000) %>% 
  #  st_transform(crs = 4326)
  #e2 <- ext(vect(sf_garimpo_20km)) 
  e2 <- myexent
  
  #Crop
  rtest_mask <- crop(rbig, e2, snap="out")
  #rtest_mask <- rbig
  rm("rbig") 
  
  #Project
  #new_crs <- "+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs"
  new_crs_utm <- "epsg:31976"
  rtest_mask <- project(rtest_mask, new_crs_utm, method = "near")
  #rbig <- project(rtest_mask, new_crs, method = "near")
  #Export
  folder <- paste(state_sigla,"_", "equalarea", sep ="")
  folder_utm <- paste(state_sigla,"_", "utm", sep ="")
  folder_utm_rio <- paste(state_sigla,"_", "utm_rio", sep ="")
  folder_path <- paste("mapbiomas_ge/state_cover/", folder_utm_rio, sep = "")
  outfile <- paste("ea_cover_", state_sigla,"_", 
                   layer_year, ".tif", sep = "")
  outfile_utm <- paste("utm_cover_", state_sigla,"_", 
                       "rio_",
                   layer_year, ".tif", sep = "")
  f <- file.path(folder_path, outfile_utm)
  writeRaster(rtest_mask, f, datatype = "INT2U", overwrite = TRUE)
  #writeRaster(rtest_mask, outfile, datatype = "INT2U", overwrite = TRUE)
  
  #clear temporary files
  tmpFiles(current =TRUE, remove = TRUE) 
  
  endtime <- Sys.time() 
  textout <- paste(outfile_utm, ": ", endtime, sep="")
  print(textout)
}
#run 11:28
plyr::a_ply(df_muni_tif[2:36,], .margins = 1, 
            .fun = state_proj)

rtest <- "mapbiomas_ge/state_cover/AP_utm_rio/utm_cover_AP_rio_1985.tif"
plot(rast(rtest))
file.choose()
rin <- "C:\\Users\\Darren\\Documents\\2022_Norris_gdp_deforestaion\\AmazonConservation\\mapbiomas_ge\\state_cover\\AP_utm\\utm_cover_AP_lorenco_1985.tif"
plot(rast(rin))
#parallel example
library(doParallel)
## number of cores
cores <- detectCores()
cores
usecores <- cores -1
usecores <- cores # if there is enough free memory etc....
## register
cl <- makeCluster(usecores)
registerDoParallel(cl)
#11:55
#inid <- seq(6, 36, by=2)
plyr::a_ply(df_muni_tif[2:36, ], .margins = 1, 
            .fun = state_proj, .parallel = TRUE)

#9:18 - 10:12 #Amazonas = 1 hour
#10:24 - 11:23 1 hour for 3, 12 hours for 36
seldone <- which(df_muni_tif$ayear %in% c(2013, 2014, 2015))
plyr::a_ply(df_muni_tif[-seldone, ], .margins = 1, 
            .fun = state_cover, sf_state = sf_instate , .parallel = TRUE)
stopCluster(usecores)

#This way take a very long time for 36 years
# See options for parallel processing
# https://www.gis-blog.com/increasing-the-speed-of-raster-processing-with-r-part-23-parallelisation/
rin <- "F:\\mapbiomas\\brasil_coverage_1985.tif"
rbig <- terra::rast(rin)
layer_name <- names(rbig)
layer_year <- substr(layer_name, 17, 20)

state_id <- "TO"
#States
sf_instate <- st_read("vector\\ninestate_poly.shp") %>% 
  filter(SIGLA_UF == state_id)  %>% 
  st_transform(crs = crs(rbig))
e2 <- ext(vect(sf_instate))

#parallel example
library(doParallel)
## number of cores
cores <- detectCores()
cores
usecores <- cores -1
## register
cl <- makeCluster(usecores)
registerDoParallel(cl)

#Crop
rtest_mask <- crop(rbig, e2, snap="out")
rm("rbig") 

#Project
#EPSG:102033
new_crs <- "+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs"
rtest_mask <- project(rtest_mask, new_crs, method = "near")

#Export
outfile <- paste("state_coverage_", state_id,"_", layer_year, ".tif", sep = "")
#f <- file.path("E:/mapbiomas/nine_states", outfile)
writeRaster(rtest_mask, outfile, datatype = "INT2U", overwrite = TRUE)

#clear temporary files
tmpFiles(current =TRUE, remove = TRUE)

stopCluster(usecores)
