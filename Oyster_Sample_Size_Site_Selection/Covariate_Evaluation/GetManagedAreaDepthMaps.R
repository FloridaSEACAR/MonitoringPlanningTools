# CZ529 Tasks 1a and 3a
# Full resolution versions are for 1a where depth is used as a covariate
# Subsampled versions are for display in Shiny/Leaflet
# Helper functions to 
# get depth maps for managed areas
# subsample to make them reasonable for Leaflet
# make valiues above sea level transparent (NA)

library(tidyverse)
library(magrittr)
library(here)
library(sf)
library(terra)


All_Managed_Areas = c(
"Estero Bay Aquatic Preserve"
##, "Guana Tolomato Matanzas National Estuarine Research Reserve"  # deal with this separately
, "Guana River Marsh Aquatic Preserve"
, "Apalachicola Bay Aquatic Preserve" 
, "Apalachicola National Estuarine Research Reserve" 
, "Indian River-Vero Beach to Ft. Pierce Aquatic Preserve"
, "Jensen Beach to Jupiter Inlet Aquatic Preserve"
, "Lemon Bay Aquatic Preserve"
, "Pine Island Sound Aquatic Preserve"
)

SEACAR_GIS_Processing_Layers_ORCP_Managed_Areas_Apr2025 = sf::st_read(dsn = here("./data/SEACAR_GIS_Processing_Layers.gdb"), layer ="ORCP_Managed_Areas_Apr2025") 
AllDepthShapefiles = st_read(here("/Users/johnhandley/Documents/PaleontologyResearch/Oysters/HOBS/Contract Work 2/Digital Elevation Models/NCEI_ninth_Topobathy_2014_8483/tileindex_NCEI_ninth_Topobathy_2014/tileindex_NCEI_ninth_Topobathy_2014.shp")) 

for( Managed_Area in All_Managed_Areas){
print(Managed_Area)
  sf_use_s2(FALSE) # turns off spherical geometry
SEACAR_GIS_Processing_Layers_ORCP_Managed_Areas_Apr2025 %>% filter(LONG_NAME == Managed_Area) %>% 
  sf::st_transform(., crs = st_crs("WGS84")) -> Managed_Area_sf
Managed_Area_depth_tileindex = st_intersects(sf::st_transform(AllDepthShapefiles, crs = st_crs("WGS84")), Managed_Area_sf)
pos = which(as.matrix(Managed_Area_depth_tileindex)[,1]) # positions of intersection

depth_tile_names = st_drop_geometry(AllDepthShapefiles[pos,"url"]) # these are the filenames of the intersecting depth maps
#get these rasters and merge them.

r =  project(rast(depth_tile_names[1,1]),"WGS84")

if( length(pos) > 1){
  for( ii in 2:length(pos)){
    r = terra::merge(r, project(rast(depth_tile_names[ii,1]),"WGS84") )
     }
}

 
# write out full resolution version for covariate analysis
  terra::writeRaster(r, filename =  here(paste0("./code/data/",paste(gsub(" ","_",Managed_Area),"full_res_depth_map.tif", sep = "_"))), overwrite=TRUE)
# mask to managed area and subsample
   
   r = aggregate(terra::mask( r, vect(Managed_Area_sf)), fact=ceiling(min(nrow(r),ncol(r))/1024), fun="mean")
   r = terra::classify(r,cbind(0,Inf,NA))
   terra::writeRaster(r, filename =  here(paste0("./code/data/",paste(gsub(" ","_",Managed_Area),"depth_map.tif", sep = "_"))))


}

# Special processing for "Guana Tolomato Matanzas National Estuarine Research Reserve" to get depth maps for all the sub areas

GTM_sub_areas = c("Guana_River","Salt_Run", "Tolomato_River","St_Augustine","Pellicer_Flats","Butler_Beach","Fort_Matanzas")
Managed_Area = "Guana Tolomato Matanzas National Estuarine Research Reserve"
gtm_managed_area_polygons_Guana_River = st_transform(st_read(here("./data/GTM_Oyster_Monitoring_Region_Polygons"), layer = "Guana_River"), "+proj=longlat +ellps=WGS84 +datum=WGS84")
gtm_managed_area_polygons_Salt_Run =  st_transform(st_read(here("./data/GTM_Oyster_Monitoring_Region_Polygons"), layer = "Salt_Run"), "+proj=longlat +ellps=WGS84 +datum=WGS84")
gtm_managed_area_polygons_Tolomato_River =  st_transform(st_read(here("./data/GTM_Oyster_Monitoring_Region_Polygons"), layer = "Tolomato_River"), "+proj=longlat +ellps=WGS84 +datum=WGS84")
gtm_managed_area_polygons_St_Augustine =  st_transform(st_read(here("./data/GTM_Oyster_Monitoring_Region_Polygons"), layer = "St_Augustine"), "+proj=longlat +ellps=WGS84 +datum=WGS84")
gtm_managed_area_polygons_Pellicer_Flats =  st_transform(st_read(here("./data/GTM_Oyster_Monitoring_Region_Polygons"), layer = "Pellicer_Flats"), "+proj=longlat +ellps=WGS84 +datum=WGS84")
gtm_managed_area_polygons_Butler_Beach =  st_transform(st_read(here("./data/GTM_Oyster_Monitoring_Region_Polygons"), layer = "Butler_Beach"), "+proj=longlat +ellps=WGS84 +datum=WGS84")
gtm_managed_area_polygons_Fort_Matanzas =  st_transform(st_read(here("./data/GTM_Oyster_Monitoring_Region_Polygons"), layer = "Fort_Matanzas"), "+proj=longlat +ellps=WGS84 +datum=WGS84")



for( gtm_sub_area in GTM_sub_areas){
  print(gtm_sub_area)
  sf_use_s2(FALSE) # turns off spherical geometry
  SEACAR_GIS_Processing_Layers_ORCP_Managed_Areas_Apr2025 %>% filter(LONG_NAME == Managed_Area) %>% 
    sf::st_transform(., crs = st_crs("WGS84")) -> Managed_Area_sf
  Managed_Area_sf = st_intersection(Managed_Area_sf, st_transform(st_read(here("./data/GTM_Oyster_Monitoring_Region_Polygons"), layer = gtm_sub_area), "+proj=longlat +ellps=WGS84 +datum=WGS84"))
  Managed_Area_depth_tileindex = st_intersects(sf::st_transform(AllDepthShapefiles, crs = st_crs("WGS84")), Managed_Area_sf)
  pos = which(as.matrix(Managed_Area_depth_tileindex)[,1]) # positions of intersection
  
  depth_tile_names = st_drop_geometry(AllDepthShapefiles[pos,"url"]) # these are the filenames of the intesecting depth maps
  #get these rasters
  
  r =  project(rast(depth_tile_names[1,1]),"WGS84")
  
  if( length(pos) > 1){
    for( ii in 2:length(pos)){
      r = terra::merge(r, project(rast(depth_tile_names[ii,1]),"WGS84") )
    }
  }
  
  # write out full resolution version for covariate analysis
  terra::writeRaster(r, filename =  here(paste0("./code/data/",paste(gsub(" ","_",Managed_Area),gtm_sub_area,"full_res_depth_map.tif", sep = "_"))), overwrite=TRUE)
  
  # mask to managed area and subsample
  r = aggregate(terra::mask( r, vect(Managed_Area_sf)), fact=ceiling(min(nrow(r),ncol(r))/1024), fun="mean")
  r = terra::classify(r,cbind(0,Inf,NA))
  terra::writeRaster(r, filename =  here(paste0("./code/data/",paste(gsub(" ","_",Managed_Area),gtm_sub_area,"depth_map.tif", sep = "_"))))

}

