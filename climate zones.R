library(terra)

zones<-rast("GIS/Climate zones/ClassifiedSumSWGPrecandTas.tif")

# zones_shp<-as.polygons(zones,aggregate=TRUE,values=TRUE,na.rm=TRUE)
# plot(zones_shp)
# zones_shp
# 
# writeVector(zones_shp,"GIS/Akshay climate zones/ClassifiedSumSWGPrecandTas polygonised/PrecandTas_zones.shp")

unique(zones)
levels(zones)<-data.frame(id=1:37,zone=11:47)
plot(zones)

res<-rast(nrows=nrow(zones)*9,ncols=ncol(zones)*9,crs=crs(zones),ext=ext(zones))

zones_res<-resample(zones,res,method="near")

landcov<-vect("GIS/Land cover/2017D/2017D.shp")

zones_res<-crop(zones_res,landcov)

zones_res

plot(zones_res)

writeRaster(zones_res,"GIS/Climate zones/clim_zone_1ha.tif",overwrite=TRUE)
