#river extent
library(plyr)
library(tidyverse)
library(sf)
library(sp)
library(raster)
library(terra)
library(ggspatial)
meuSIG <- "inst/vector/rivers.GPKG"
rsl <- sf::st_read(meuSIG, layer = "centerline")
rsl_50km <- st_union(st_buffer(rsl, dist = 50000))
ggplot(rsl_50km) + 
  geom_sf() + 
  geom_sf(data = rsl)

myexent <- ext(vect(rsl_50km)) 


# example 
#Get adjacent cell numbers
rin <- "C:\\Users\\user\\Documents\\Articles\\gis_layers\\gisdata\\inst\\raster\\mapbiomas_AP_utm_rio\\utm_cover_AP_rio_2020.tif"
r <- rast(rin)
 #coordinates of point
island <- data.frame(nome = "island", 
                     coord_x = -51.405912, 
                     coord_y = 0.726236)
#Converter para objeto espacial
sf_island <- st_as_sf(island, 
                     coords = c("coord_x", "coord_y"),
                      crs = 4326)
sf_island_utm <- st_transform(sf_island, crs = 31976)
sf_island_110m <- st_buffer(sf_island_utm, dist=90)
e2 <- ext(vect(sf_island_110m)) 

rsmall <- crop(r, e2, snap="out") 
plot(rsmall)
names(rsmall) <- "mapbiomas_2020"
writeRaster(rsmall, "inst/raster/amostra_mapbiomas_2020.tif", datatype = "INT2U", overwrite = TRUE)


classe_valor <- c(3, 12, 33)
classe_legenda <- c("Formação Florestal", 
                   "Formação Campestre", "Rio, Lago e Oceano")
classe_cores <- c("#006400", "#B8AF4F", "#0000FF") 
mycoltab <- data.frame(valor = classe_valor, 
                       cor = classe_cores)
coltab(rsmall) <- mycoltab
plot(rsmall)

rsmall_df <- as.data.frame(rsmall, xy = TRUE)

cls <- data.frame(id=c(3,12,33), 
                  classe=c("Formação Florestal", 
                           "Formação Campestre", "Rio, Lago e Oceano"))
levels(rsmall) <- cls
ggplot() +
  layer_spatial(rsmall) +
  #geom_raster(aes(x = x, y = y, 
  #                fill = factor(mapbiomas_2020))) + 
  scale_fill_manual("classe", 
                      values = classe_cores,
                      labels = classe_legenda) + 
  geom_text(data = rsmall_df, aes(x = x, y = y, 
                label = mapbiomas_2020)) 
