#Title: JPB1 2mo EPA Pollution Spatial Interpolation Script
#File: JPB1 2mo EPA Pollution Spatial Interpolation Script 2023-01-04
#Authors: Liam North

#NOTE: Although labels specify NO2 as pollutant being interpolated, any EPA pollutant can be used for this script. The other 2 pollutants I analysed were PM2.5 and O3. DF7 is where you import the EPA pollutant dataset of interest.

#REFERRENCES###############################################################
  #https://bookdown.org/igisc/EnvDataSci/spatial-interpolation.html

#LOAD PACKAGES#############################################################
pacman::p_load(pacman, party, psych, rio, tidyverse, car, viridis, hrbrthemes, fmsb, phylin, sf, gstat, automap, tigris, stars, maps, mapdata, ggmap, rgeos, raster, sp, spatstat, tmap, rgdal, terra, stats, tidyterra, epiDisplay)
options(tigris_use_cache = TRUE)

#DATA IMPORT###############################################################
#DF2 for Spatial Interpolation and Plotting of LA Addresses
df2 <- subset(df1,address_city == 1,select=c('address_LAT','address_LONG'))
df2

#DF3 for Spatial Interpolation and Plotting of Boston Addresses
df3 <- subset(df1,address_city == 0,select=c('address_LAT','address_LONG'))
df3

#DF4 for EPA NO2 Sensor Location and Info
df4 <- data.frame(read.csv(file.choose()))
head(df4)
tail(df4)
as_tibble(df4)

#LA NO2 Sensor Sensor Location and Info
df5 <- subset(df4, sensor_city == 1)
df5.1 <- subset(df5, NO2.measured. == 1, select=c('sensor_LAT','sensor_LONG'))
df5.2 <- subset(df5, PM2.5.measured. == 1, select=c('sensor_LAT','sensor_LONG'))
df5.3 <- subset(df5, Ozone.measured. == 1, select=c('sensor_LAT','sensor_LONG'))


#LA NO2 Sensor Sensor Location and Info
df6 <- subset(df4, sensor_city == 0)
df6.1 <- subset(df6, NO2.measured. == 1, select=c('sensor_LAT','sensor_LONG'))
df6.2 <- subset(df6, PM2.5.measured. == 1, select=c('sensor_LAT','sensor_LONG'))
df6.3 <- subset(df6, Ozone.measured. == 1, select=c('sensor_LAT','sensor_LONG'))

#DF7 for EPA NO2 2016, 2017, AND 2018 Data
df7 <- data.frame(read.csv(file.choose()))
head(df7)
tail(df7)
as.tibble(df7)

#PLOT POLLUTION SENSORS LA#################################################
#Plot LA Participants, with urban + zcta overlays
#Make LA Participant LATs,LONGs into sf object
la_sf <- st_as_sf(df2, coords = c('address_LONG', 'address_LAT'), crs = 4326)
st_combine(la_sf)

#Create LA zctas
la_zctas <- zctas(
  cb = TRUE, 
  starts_with = c("900", "901", "902", "903", "904", "905", "906", "907", "908", "909", "910", "911", "912", "913","914", "915", "916", "917", "918", "919", "920", "921", "922", "923", "924", "925", "926", "927", "928", "929", "930", "931", "932", "933", "934", "935", "917", "935"),
  year = 2020
)
la_zctas <- st_transform(la_zctas, crs = 4326)
la_zctas

#Specifying LA urban areas.
uas <- urban_areas() %>% filter(str_detect(NAME10, "Los Angeles"))
uas <- st_transform(uas, crs = 4326)
st_crs(uas)
st_combine(uas)

#Make LA NO2 EPA sensor locations into sf object
la_no2_sf <- st_as_sf(df5.1, coords = c('sensor_LONG', 'sensor_LAT'), crs = 4326)
st_combine(la_no2_sf)

#Make LA PM2.5 EPA sensor locations into sf object
la_pm2.5_sf <- st_as_sf(df5.2, coords = c('sensor_LONG', 'sensor_LAT'), crs = 4326)
st_combine(la_pm2.5_sf)

#Make LA O3 EPA sensor locations into sf object
la_O3_sf <- st_as_sf(df5.3, coords = c('sensor_LONG', 'sensor_LAT'), crs = 4326)
st_combine(la_O3_sf)


#PLOT!
ggplot() +
  geom_sf(data = uas)+
  geom_sf(data = la_zctas, fill = '#ffffff')+
  #geom_sf(data = la_sf, aes(), size = 2, color = '#e31a1c')+
  #geom_sf(data = la_no2_sf, size = 3, color ='#9e0142', shape = 15)+
  geom_sf(data = la_pm2.5_sf, size = 4, color ='#238b45', shape = 19)+
  #geom_sf(data = la_O3_sf, size = 5, color ='#3690c0', shape = 18)+
  coord_sf(xlim = c(-118.65, -117.3), ylim = c(33.8, 34.4))+
  theme(plot.title = element_text(size=26), axis.text = element_text(size = 14),axis.title = element_text(size = 20), panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  ggtitle("Los Angeles EPA Sensor Locations")

#DETERMINE AVG DISTANCE ENTRE LA PARTICIPANTS AND CLOSEST SENSOR###########
#Figure out average distance between LA participants and closest NO2 sensor
count(la_sf)
count(la_no2_sf)
matrix_la_no2 <- rbind(la_sf, la_no2_sf)
matrix_la_no2
dist_matrix_la_no2 <- as.matrix(st_distance(matrix_la_no2))
dist_matrix_la_no2
dist_matrix_la_no2 <- dist_matrix_la_no2[0:39, 39:63]
dist_matrix_la_no2
apply( dist_matrix_la_no2, 1, function(x) mean( x[order(x)][2:2] ) )
mean(apply( dist_matrix_la_no2, 1, function(x) mean( x[order(x)][2:2] ) ))

#Figure out average distance between LA participants and closest PM2.5 sensor
count(la_sf)
count(la_pm2.5_sf)
matrix_la_pm2.5 <- rbind(la_sf, la_pm2.5_sf)
matrix_la_pm2.5
dist_matrix_la_pm2.5 <- as.matrix(st_distance(matrix_la_pm2.5))
dist_matrix_la_pm2.5
dist_matrix_la_pm2.5 <- dist_matrix_la_pm2.5[0:39, 39:55]
dist_matrix_la_pm2.5
apply( dist_matrix_la_pm2.5, 1, function(x) mean( x[order(x)][2:2] ) )
mean(apply( dist_matrix_la_pm2.5, 1, function(x) mean( x[order(x)][2:2] ) ))

#PLOT POLLUTION SENSORS BOSTON#############################################
#Plot Boston Participants, with urban + zcta overlays
#Make Boston Participant LATs,LONGs into sf object
boston_sf <- st_as_sf(df3, coords = c('address_LONG', 'address_LAT'), crs = 4326)

#create boston zctas
boston_zctas <- zctas(
  cb = TRUE, 
  starts_with = c("009", "010", "011", "012", "013", "014", "015", "016", "017", "018", "019", "020", "021", "022", "023","024", "025", "026", "027", "028", "029", "030", "031", "032", "033", "034", "035", "036", "037", "038", "060", "061", "062"),
  year = 2020
)
boston_zctas <- st_transform(boston_zctas, crs = 4326)
boston_zctas

#Specifying Boston urban areas.
uas <- urban_areas() %>% filter(str_detect(NAME10, "Boston"))
uas <- st_transform(uas, crs = 4326)

#Make LA NO2 EPA sensor locations into sf object
boston_no2_sf <- st_as_sf(df6.1, coords = c('sensor_LONG', 'sensor_LAT'), crs = 4326)
st_combine(la_no2_sf)

#Make LA PM2.5 EPA sensor locations into sf object
boston_pm2.5_sf <- st_as_sf(df6.2, coords = c('sensor_LONG', 'sensor_LAT'), crs = 4326)
st_combine(la_pm2.5_sf)

#Make LA O3 EPA sensor locations into sf object
boston_O3_sf <- st_as_sf(df6.3, coords = c('sensor_LONG', 'sensor_LAT'), crs = 4326)
st_combine(la_O3_sf)


#PLOT!
ggplot() +
  #geom_sf(data = uas)+
  geom_sf(data = boston_zctas, fill = '#ffffff')+
  geom_sf(data = boston_sf, aes(), size = 2, color = '#e31a1c')+
  geom_sf(data = boston_no2_sf, size = 3, color ='#9e0142', shape = 15)+
  #geom_sf(data = boston_pm2.5_sf, size = 4, color ='#238b45', shape = 19)+
  #geom_sf(data = boston_O3_sf, size = 5, color ='#3690c0', shape = 18)+
  coord_sf(xlim = c(-72.5, -70.5), ylim = c(41.8, 43))+
  theme(plot.title = element_text(size=26), axis.text = element_text(size = 14),axis.title = element_text(size = 20), panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  ggtitle("Boston O3 EPA Sensor Locations")

#DETERMINE AVG DISTANCE ENTRE BOSTON PARTICIPANTS AND CLOSEST SENSOR#######
#Figure out average distance between Boston participants and closest NO2 sensor
count(boston_sf)
count(boston_no2_sf)
matrix_boston_no2 <- rbind(boston_sf, boston_no2_sf)
matrix_boston_no2
dist_matrix_boston_no2 <- as.matrix(st_distance(matrix_boston_no2))
dist_matrix_boston_no2
dist_matrix_boston_no2 <- dist_matrix_boston_no2[0:40, 40:51]
dist_matrix_boston_no2
apply( dist_matrix_boston_no2, 1, function(x) mean( x[order(x)][2:2] ) )
mean(apply( dist_matrix_boston_no2, 1, function(x) mean( x[order(x)][2:2] ) ))

#Figure out average distance between LA participants and closest PM2.5 sensor
count(boston_sf)
count(boston_pm2.5_sf)
matrix_boston_pm2.5 <- rbind(boston_sf, boston_pm2.5_sf)
matrix_boston_pm2.5
dist_matrix_boston_pm2.5 <- as.matrix(st_distance(matrix_boston_pm2.5))
dist_matrix_boston_pm2.5
dist_matrix_boston_pm2.5 <- dist_matrix_boston_pm2.5[0:40, 40:57]
dist_matrix_boston_pm2.5
apply( dist_matrix_boston_pm2.5, 1, function(x) mean( x[order(x)][2:2] ) )
mean(apply( dist_matrix_boston_pm2.5, 1, function(x) mean( x[order(x)][2:2] ) ))

#INTERPOLATE LA NO2 POLLUTION VALUES#######################################
# Load LA NO2 data
df7_NO2_LA <- subset(df7, State.Code == 6, select=c('Longitude','Latitude', 'Arithmetic.Mean', 'Date.Local'))
df7_NO2_LA

# Specifying LA urban areas.
uas <- urban_areas() %>% filter(str_detect(NAME10, "Los Angeles"))
uas <- st_transform(uas, crs = 4326)
st_crs(uas)
st_combine(uas)

la_no2_sf <- st_as_sf(df7_NO2_LA, coords = c('Longitude', 'Latitude'), crs = 4326)
la_zctas <- st_transform(la_zctas, crs=4326)
st_combine(la_zctas)

ggplot() +
  geom_sf(data = uas)+
  geom_sf(data=la_zctas, fill = '#ffffff') + 
  geom_sf(data=la_no2_sf, aes(col=Arithmetic.Mean), size = 4)+
  coord_sf(xlim = c(-118.65, -117.2), ylim = c(33.8, 34.6))+
  theme(plot.title = element_text(size=26), axis.text = element_text(size = 18),axis.title = element_text(size = 20), panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  ggtitle("JPB1 Los Angeles EPA Sensors NO2 Reading")

# proximity (Voronoi/Thiessen) polygons
la_no2_sf_V <- vect(la_no2_sf)
v <- voronoi(la_no2_sf_V)
plot(v)
points(la_no2_sf_V)

V_la_no2_sf <- crop(v, vect(st_union(la_zctas)))
plot(V_la_no2_sf, "Arithmetic.Mean")

# rasterize
r <- rast(V_la_no2_sf, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_la_no2_sf, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d <- data.frame(geom(la_no2_sf_V)[,c("x", "y")], as.data.frame(la_no2_sf_V))
head(d)

# create model
gs <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d, nmax=Inf, set=list(idp=2))

# run idw
idw <- interpolate(r, gs, debug.level=0)

# plot idw
idwr <- mask(idw, vr)
plot(idwr, 1, xlim = c(-118.65, -117.3), ylim = c(33.8, 34.4), main = "Los Angeles Interpolated PM2.5 Concentration Data (µg/m3)")
plot(la_zctas, col="transparent", add=T)

#plot idw ggplot

ggplot() +
  #geom_sf(data = uas)+
  geom_spatraster(data = idw, aes(fill = var1.pred))+
  geom_sf(data = la_zctas, fill = NA)+
  #geom_sf(data = la_sf, aes(), size = 2, color = '#e31a1c')+
  #geom_sf(data = la_no2_sf, size = 3, color ='#9e0142', shape = 15, alpha = 0.8)+
  #geom_sf(data = la_pm2.5_sf, size = 4, color ='#c51b7d', shape = 19, alpha = 1)+
  #geom_sf(data = la_O3_sf, size = 3, color ='#1a9850', shape = 18)+
  coord_sf(xlim = c(-118.65, -117.3), ylim = c(33.8, 34.4))+
  theme(plot.title = element_text(size=26), axis.text = element_text(size = 14),axis.title = element_text(size = 20), panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  ggtitle("Los Angeles Interpolated PM2.5 Exposure")+
  scale_fill_distiller(palette = "YlOrRd", trans = "reverse")+
  labs(fill = "PM2.5 Exposure\n(µg/m3)")

#02s003 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "02-s003"]
df1$X2mo_visit_date[df1$recordID == "02-s003"]

df7_NO2_LA_02s003 <- df7_NO2_LA[df7_NO2_LA$Date.Local >= "2016-09-18" & df7_NO2_LA$Date.Local <= "2016-10-18", ]
df7_NO2_LA_02s003

# Convert dataframe into sf object
sf_NO2_LA_02s003 <- st_as_sf(df7_NO2_LA_02s003, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_LA_02s003_V <- vect(sf_NO2_LA_02s003)
v <- voronoi(sf_no2_LA_02s003_V)
plot(v)
points(sf_no2_LA_02s003_V)

V_no2_LA_02s003 <- crop(v, vect(st_union(la_zctas)))
plot(V_no2_LA_02s003, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_LA_02s003, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_LA_02s003, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_02s003_no2 <- data.frame(geom(sf_no2_LA_02s003_V)[,c("x", "y")], as.data.frame(sf_no2_LA_02s003_V))
head(d_02s003_no2)

# run gstat model
gs_02s003_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_02s003_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_02s003_no2 <- interpolate(r, gs_02s003_no2, debug.level=0)

# rasterize idw output
idw_02s003_no2.r <-raster(idw_02s003_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "02-s003"]
df1$address_LAT[df1$recordID == "02-s003"]

p_02s003_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "02-s003"], df1$address_LAT[df1$recordID == "02-s003"]), ncol =2))
proj4string(p_02s003_no2) <- projection(idw_02s003_no2.r)

# extract estimated NO2 value 
extract(idw_02s003_no2.r, p_02s003_no2)

#02s004 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "02-s004"]
df1$X2mo_visit_date[df1$recordID == "02-s004"]

df7_NO2_LA_02s004 <- df7_NO2_LA[df7_NO2_LA$Date.Local >= "2016-09-18" & df7_NO2_LA$Date.Local <= "2016-10-18", ]
df7_NO2_LA_02s004

# Convert dataframe into sf object
sf_NO2_LA_02s004 <- st_as_sf(df7_NO2_LA_02s004, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_LA_02s004_V <- vect(sf_NO2_LA_02s004)
v <- voronoi(sf_no2_LA_02s004_V)
plot(v)
points(sf_no2_LA_02s004_V)

V_no2_LA_02s004 <- crop(v, vect(st_union(la_zctas)))
plot(V_no2_LA_02s004, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_LA_02s004, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_LA_02s004, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_02s004_no2 <- data.frame(geom(sf_no2_LA_02s004_V)[,c("x", "y")], as.data.frame(sf_no2_LA_02s004_V))
head(d_02s004_no2)

# run gstat model
gs_02s004_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_02s004_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_02s004_no2 <- interpolate(r, gs_02s004_no2, debug.level=0)

# rasterize idw output
idw_02s004_no2.r <-raster(idw_02s004_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "02-s004"]
df1$address_LAT[df1$recordID == "02-s004"]

p_02s004_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "02-s004"], df1$address_LAT[df1$recordID == "02-s004"]), ncol =2))
proj4string(p_02s004_no2) <- projection(idw_02s004_no2.r)

# extract estimated NO2 value 
extract(idw_02s004_no2.r, p_02s004_no2)

#02s005 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "02-s005"]
df1$X2mo_visit_date[df1$recordID == "02-s005"]

df7_NO2_LA_02s005 <- df7_NO2_LA[df7_NO2_LA$Date.Local >= "2016-09-25" & df7_NO2_LA$Date.Local <= "2016-10-25", ]
df7_NO2_LA_02s005

# Convert dataframe into sf object
sf_NO2_LA_02s005 <- st_as_sf(df7_NO2_LA_02s005, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_LA_02s005_V <- vect(sf_NO2_LA_02s005)
v <- voronoi(sf_no2_LA_02s005_V)
plot(v)
points(sf_no2_LA_02s005_V)

V_no2_LA_02s005 <- crop(v, vect(st_union(la_zctas)))
plot(V_no2_LA_02s005, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_LA_02s005, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_LA_02s005, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_02s005_no2 <- data.frame(geom(sf_no2_LA_02s005_V)[,c("x", "y")], as.data.frame(sf_no2_LA_02s005_V))
head(d_02s005_no2)

# run gstat model
gs_02s005_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_02s005_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_02s005_no2 <- interpolate(r, gs_02s005_no2, debug.level=0)

# rasterize idw output
idw_02s005_no2.r <-raster(idw_02s005_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "02-s005"]
df1$address_LAT[df1$recordID == "02-s005"]

p_02s005_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "02-s005"], df1$address_LAT[df1$recordID == "02-s005"]), ncol =2))
proj4string(p_02s005_no2) <- projection(idw_02s005_no2.r)

# extract estimated NO2 value 
extract(idw_02s005_no2.r, p_02s005_no2)

#02s009 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "02-s009"]
df1$X2mo_visit_date[df1$recordID == "02-s009"]

df7_NO2_LA_02s009 <- df7_NO2_LA[df7_NO2_LA$Date.Local >= "2016-10-10" & df7_NO2_LA$Date.Local <= "2016-11-09", ]
df7_NO2_LA_02s009

# Convert dataframe into sf object
sf_NO2_LA_02s009 <- st_as_sf(df7_NO2_LA_02s009, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_LA_02s009_V <- vect(sf_NO2_LA_02s009)
v <- voronoi(sf_no2_LA_02s009_V)
plot(v)
points(sf_no2_LA_02s009_V)

V_no2_LA_02s009 <- crop(v, vect(st_union(la_zctas)))
plot(V_no2_LA_02s009, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_LA_02s009, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_LA_02s009, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_02s009_no2 <- data.frame(geom(sf_no2_LA_02s009_V)[,c("x", "y")], as.data.frame(sf_no2_LA_02s009_V))
head(d_02s009_no2)

# run gstat model
gs_02s009_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_02s009_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_02s009_no2 <- interpolate(r, gs_02s009_no2, debug.level=0)

# rasterize idw output
idw_02s009_no2.r <-raster(idw_02s009_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "02-s009"]
df1$address_LAT[df1$recordID == "02-s009"]

p_02s009_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "02-s009"], df1$address_LAT[df1$recordID == "02-s009"]), ncol =2))
proj4string(p_02s009_no2) <- projection(idw_02s009_no2.r)

# extract estimated NO2 value 
extract(idw_02s009_no2.r, p_02s009_no2)

#02s012 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "02-s012"]
df1$X2mo_visit_date[df1$recordID == "02-s012"]

df7_NO2_LA_02s012 <- df7_NO2_LA[df7_NO2_LA$Date.Local >= "2016-10-19" & df7_NO2_LA$Date.Local <= "2016-11-18", ]
df7_NO2_LA_02s012

# Convert dataframe into sf object
sf_NO2_LA_02s012 <- st_as_sf(df7_NO2_LA_02s012, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_LA_02s012_V <- vect(sf_NO2_LA_02s012)
v <- voronoi(sf_no2_LA_02s012_V)
plot(v)
points(sf_no2_LA_02s012_V)

V_no2_LA_02s012 <- crop(v, vect(st_union(la_zctas)))
plot(V_no2_LA_02s012, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_LA_02s012, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_LA_02s012, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_02s012_no2 <- data.frame(geom(sf_no2_LA_02s012_V)[,c("x", "y")], as.data.frame(sf_no2_LA_02s012_V))
head(d_02s012_no2)

# run gstat model
gs_02s012_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_02s012_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_02s012_no2 <- interpolate(r, gs_02s012_no2, debug.level=0)

# rasterize idw output
idw_02s012_no2.r <-raster(idw_02s012_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "02-s012"]
df1$address_LAT[df1$recordID == "02-s012"]

p_02s012_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "02-s012"], df1$address_LAT[df1$recordID == "02-s012"]), ncol =2))
proj4string(p_02s012_no2) <- projection(idw_02s012_no2.r)

# extract estimated NO2 value 
extract(idw_02s012_no2.r, p_02s012_no2)

#02s014 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "02-s014"]
df1$X2mo_visit_date[df1$recordID == "02-s014"]

df7_NO2_LA_02s014 <- df7_NO2_LA[df7_NO2_LA$Date.Local >= "2016-11-02" & df7_NO2_LA$Date.Local <= "2016-12-02", ]
df7_NO2_LA_02s014

# Convert dataframe into sf object
sf_NO2_LA_02s014 <- st_as_sf(df7_NO2_LA_02s014, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_LA_02s014_V <- vect(sf_NO2_LA_02s014)
v <- voronoi(sf_no2_LA_02s014_V)
plot(v)
points(sf_no2_LA_02s014_V)

V_no2_LA_02s014 <- crop(v, vect(st_union(la_zctas)))
plot(V_no2_LA_02s014, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_LA_02s014, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_LA_02s014, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_02s014_no2 <- data.frame(geom(sf_no2_LA_02s014_V)[,c("x", "y")], as.data.frame(sf_no2_LA_02s014_V))
head(d_02s014_no2)

# run gstat model
gs_02s014_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_02s014_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_02s014_no2 <- interpolate(r, gs_02s014_no2, debug.level=0)

# rasterize idw output
idw_02s014_no2.r <-raster(idw_02s014_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "02-s014"]
df1$address_LAT[df1$recordID == "02-s014"]

p_02s014_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "02-s014"], df1$address_LAT[df1$recordID == "02-s014"]), ncol =2))
proj4string(p_02s014_no2) <- projection(idw_02s014_no2.r)

# extract estimated NO2 value 
extract(idw_02s014_no2.r, p_02s014_no2)

#02s015 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "02-s015"]
df1$X2mo_visit_date[df1$recordID == "02-s015"]

df7_NO2_LA_02s015 <- df7_NO2_LA[df7_NO2_LA$Date.Local >= "2016-11-08" & df7_NO2_LA$Date.Local <= "2016-12-08", ]
df7_NO2_LA_02s015

# Convert dataframe into sf object
sf_NO2_LA_02s015 <- st_as_sf(df7_NO2_LA_02s015, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_LA_02s015_V <- vect(sf_NO2_LA_02s015)
v <- voronoi(sf_no2_LA_02s015_V)
plot(v)
points(sf_no2_LA_02s015_V)

V_no2_LA_02s015 <- crop(v, vect(st_union(la_zctas)))
plot(V_no2_LA_02s015, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_LA_02s015, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_LA_02s015, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_02s015_no2 <- data.frame(geom(sf_no2_LA_02s015_V)[,c("x", "y")], as.data.frame(sf_no2_LA_02s015_V))
head(d_02s015_no2)

# run gstat model
gs_02s015_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_02s015_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_02s015_no2 <- interpolate(r, gs_02s015_no2, debug.level=0)

# rasterize idw output
idw_02s015_no2.r <-raster(idw_02s015_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "02-s015"]
df1$address_LAT[df1$recordID == "02-s015"]

p_02s015_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "02-s015"], df1$address_LAT[df1$recordID == "02-s015"]), ncol =2))
proj4string(p_02s015_no2) <- projection(idw_02s015_no2.r)

# extract estimated NO2 value 
extract(idw_02s015_no2.r, p_02s015_no2)

#02s016 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "02-s016"]
df1$X2mo_visit_date[df1$recordID == "02-s016"]

df7_NO2_LA_02s016 <- df7_NO2_LA[df7_NO2_LA$Date.Local >= "2016-11-13" & df7_NO2_LA$Date.Local <= "2016-12-13", ]
df7_NO2_LA_02s016

# Convert dataframe into sf object
sf_NO2_LA_02s016 <- st_as_sf(df7_NO2_LA_02s016, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_LA_02s016_V <- vect(sf_NO2_LA_02s016)
v <- voronoi(sf_no2_LA_02s016_V)
plot(v)
points(sf_no2_LA_02s016_V)

V_no2_LA_02s016 <- crop(v, vect(st_union(la_zctas)))
plot(V_no2_LA_02s016, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_LA_02s016, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_LA_02s016, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_02s016_no2 <- data.frame(geom(sf_no2_LA_02s016_V)[,c("x", "y")], as.data.frame(sf_no2_LA_02s016_V))
head(d_02s016_no2)

# run gstat model
gs_02s016_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_02s016_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_02s016_no2 <- interpolate(r, gs_02s016_no2, debug.level=0)

# rasterize idw output
idw_02s016_no2.r <-raster(idw_02s016_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "02-s016"]
df1$address_LAT[df1$recordID == "02-s016"]

p_02s016_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "02-s016"], df1$address_LAT[df1$recordID == "02-s016"]), ncol =2))
proj4string(p_02s016_no2) <- projection(idw_02s016_no2.r)

# extract estimated NO2 value 
extract(idw_02s016_no2.r, p_02s016_no2)

#02s017 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "02-s017"]
df1$X2mo_visit_date[df1$recordID == "02-s017"]

df7_NO2_LA_02s017 <- df7_NO2_LA[df7_NO2_LA$Date.Local >= "2016-11-13" & df7_NO2_LA$Date.Local <= "2016-12-13", ]
df7_NO2_LA_02s017

# Convert dataframe into sf object
sf_NO2_LA_02s017 <- st_as_sf(df7_NO2_LA_02s017, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_LA_02s017_V <- vect(sf_NO2_LA_02s017)
v <- voronoi(sf_no2_LA_02s017_V)
plot(v)
points(sf_no2_LA_02s017_V)

V_no2_LA_02s017 <- crop(v, vect(st_union(la_zctas)))
plot(V_no2_LA_02s017, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_LA_02s017, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_LA_02s017, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_02s017_no2 <- data.frame(geom(sf_no2_LA_02s017_V)[,c("x", "y")], as.data.frame(sf_no2_LA_02s017_V))
head(d_02s017_no2)

# run gstat model
gs_02s017_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_02s017_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_02s017_no2 <- interpolate(r, gs_02s017_no2, debug.level=0)

# rasterize idw output
idw_02s017_no2.r <-raster(idw_02s017_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "02-s017"]
df1$address_LAT[df1$recordID == "02-s017"]

p_02s017_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "02-s017"], df1$address_LAT[df1$recordID == "02-s017"]), ncol =2))
proj4string(p_02s017_no2) <- projection(idw_02s017_no2.r)

# extract estimated NO2 value 
extract(idw_02s017_no2.r, p_02s017_no2)

#02s018 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "02-s018"]
df1$X2mo_visit_date[df1$recordID == "02-s018"]

df7_NO2_LA_02s018 <- df7_NO2_LA[df7_NO2_LA$Date.Local >= "2016-11-01" & df7_NO2_LA$Date.Local <= "2016-12-01", ]
df7_NO2_LA_02s018

# Convert dataframe into sf object
sf_NO2_LA_02s018 <- st_as_sf(df7_NO2_LA_02s018, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_LA_02s018_V <- vect(sf_NO2_LA_02s018)
v <- voronoi(sf_no2_LA_02s018_V)
plot(v)
points(sf_no2_LA_02s018_V)

V_no2_LA_02s018 <- crop(v, vect(st_union(la_zctas)))
plot(V_no2_LA_02s018, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_LA_02s018, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_LA_02s018, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_02s018_no2 <- data.frame(geom(sf_no2_LA_02s018_V)[,c("x", "y")], as.data.frame(sf_no2_LA_02s018_V))
head(d_02s018_no2)

# run gstat model
gs_02s018_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_02s018_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_02s018_no2 <- interpolate(r, gs_02s018_no2, debug.level=0)

# rasterize idw output
idw_02s018_no2.r <-raster(idw_02s018_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "02-s018"]
df1$address_LAT[df1$recordID == "02-s018"]

p_02s018_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "02-s018"], df1$address_LAT[df1$recordID == "02-s018"]), ncol =2))
proj4string(p_02s018_no2) <- projection(idw_02s018_no2.r)

# extract estimated NO2 value 
extract(idw_02s018_no2.r, p_02s018_no2)

#02s019 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "02-s019"]
df1$X2mo_visit_date[df1$recordID == "02-s019"]

df7_NO2_LA_02s019 <- df7_NO2_LA[df7_NO2_LA$Date.Local >= "2016-11-20" & df7_NO2_LA$Date.Local <= "2016-12-20", ]
df7_NO2_LA_02s019

# Convert dataframe into sf object
sf_NO2_LA_02s019 <- st_as_sf(df7_NO2_LA_02s019, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_LA_02s019_V <- vect(sf_NO2_LA_02s019)
v <- voronoi(sf_no2_LA_02s019_V)
plot(v)
points(sf_no2_LA_02s019_V)

V_no2_LA_02s019 <- crop(v, vect(st_union(la_zctas)))
plot(V_no2_LA_02s019, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_LA_02s019, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_LA_02s019, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_02s019_no2 <- data.frame(geom(sf_no2_LA_02s019_V)[,c("x", "y")], as.data.frame(sf_no2_LA_02s019_V))
head(d_02s019_no2)

# run gstat model
gs_02s019_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_02s019_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_02s019_no2 <- interpolate(r, gs_02s019_no2, debug.level=0)

# rasterize idw output
idw_02s019_no2.r <-raster(idw_02s019_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "02-s019"]
df1$address_LAT[df1$recordID == "02-s019"]

p_02s019_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "02-s019"], df1$address_LAT[df1$recordID == "02-s019"]), ncol =2))
proj4string(p_02s019_no2) <- projection(idw_02s019_no2.r)

# extract estimated NO2 value 
extract(idw_02s019_no2.r, p_02s019_no2)

#02s020 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "02-s020"]
df1$X2mo_visit_date[df1$recordID == "02-s020"]

df7_NO2_LA_02s020 <- df7_NO2_LA[df7_NO2_LA$Date.Local >= "2016-11-22" & df7_NO2_LA$Date.Local <= "2016-12-22", ]
df7_NO2_LA_02s020

# Convert dataframe into sf object
sf_NO2_LA_02s020 <- st_as_sf(df7_NO2_LA_02s020, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_LA_02s020_V <- vect(sf_NO2_LA_02s020)
v <- voronoi(sf_no2_LA_02s020_V)
plot(v)
points(sf_no2_LA_02s020_V)

V_no2_LA_02s020 <- crop(v, vect(st_union(la_zctas)))
plot(V_no2_LA_02s020, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_LA_02s020, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_LA_02s020, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_02s020_no2 <- data.frame(geom(sf_no2_LA_02s020_V)[,c("x", "y")], as.data.frame(sf_no2_LA_02s020_V))
head(d_02s020_no2)

# run gstat model
gs_02s020_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_02s020_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_02s020_no2 <- interpolate(r, gs_02s020_no2, debug.level=0)

# rasterize idw output
idw_02s020_no2.r <-raster(idw_02s020_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "02-s020"]
df1$address_LAT[df1$recordID == "02-s020"]

p_02s020_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "02-s020"], df1$address_LAT[df1$recordID == "02-s020"]), ncol =2))
proj4string(p_02s020_no2) <- projection(idw_02s020_no2.r)

# extract estimated NO2 value 
extract(idw_02s020_no2.r, p_02s020_no2)

#02s021 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "02_s021"]
df1$X2mo_visit_date[df1$recordID == "02_s021"]

df7_NO2_LA_02s021 <- df7_NO2_LA[df7_NO2_LA$Date.Local >= "2016-12-11" & df7_NO2_LA$Date.Local <= "2017-01-10", ]
df7_NO2_LA_02s021

# Convert dataframe into sf object
sf_NO2_LA_02s021 <- st_as_sf(df7_NO2_LA_02s021, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_LA_02s021_V <- vect(sf_NO2_LA_02s021)
v <- voronoi(sf_no2_LA_02s021_V)
plot(v)
points(sf_no2_LA_02s021_V)

V_no2_LA_02s021 <- crop(v, vect(st_union(la_zctas)))
plot(V_no2_LA_02s021, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_LA_02s021, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_LA_02s021, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_02s021_no2 <- data.frame(geom(sf_no2_LA_02s021_V)[,c("x", "y")], as.data.frame(sf_no2_LA_02s021_V))
head(d_02s021_no2)

# run gstat model
gs_02s021_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_02s021_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_02s021_no2 <- interpolate(r, gs_02s021_no2, debug.level=0)

# rasterize idw output
idw_02s021_no2.r <-raster(idw_02s021_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "02_s021"]
df1$address_LAT[df1$recordID == "02_s021"]

p_02s021_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "02_s021"], df1$address_LAT[df1$recordID == "02_s021"]), ncol =2))
proj4string(p_02s021_no2) <- projection(idw_02s021_no2.r)

# extract estimated NO2 value 
extract(idw_02s021_no2.r, p_02s021_no2)

#02s022 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "02_s022"]
df1$X2mo_visit_date[df1$recordID == "02_s022"]

df7_NO2_LA_02s022 <- df7_NO2_LA[df7_NO2_LA$Date.Local >= "2016-12-13" & df7_NO2_LA$Date.Local <= "2017-01-12", ]
df7_NO2_LA_02s022

# Convert dataframe into sf object
sf_NO2_LA_02s022 <- st_as_sf(df7_NO2_LA_02s022, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_LA_02s022_V <- vect(sf_NO2_LA_02s022)
v <- voronoi(sf_no2_LA_02s022_V)
plot(v)
points(sf_no2_LA_02s022_V)

V_no2_LA_02s022 <- crop(v, vect(st_union(la_zctas)))
plot(V_no2_LA_02s022, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_LA_02s022, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_LA_02s022, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_02s022_no2 <- data.frame(geom(sf_no2_LA_02s022_V)[,c("x", "y")], as.data.frame(sf_no2_LA_02s022_V))
head(d_02s022_no2)

# run gstat model
gs_02s022_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_02s022_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_02s022_no2 <- interpolate(r, gs_02s022_no2, debug.level=0)

# rasterize idw output
idw_02s022_no2.r <-raster(idw_02s022_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "02_s022"]
df1$address_LAT[df1$recordID == "02_s022"]

p_02s022_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "02_s022"], df1$address_LAT[df1$recordID == "02_s022"]), ncol =2))
proj4string(p_02s022_no2) <- projection(idw_02s022_no2.r)

# extract estimated NO2 value 
extract(idw_02s022_no2.r, p_02s022_no2)


#02s024 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "02_s024"]
df1$X2mo_visit_date[df1$recordID == "02_s024"]

df7_NO2_LA_02s024 <- df7_NO2_LA[df7_NO2_LA$Date.Local >= "2016-12-19" & df7_NO2_LA$Date.Local <= "2017-01-18", ]
df7_NO2_LA_02s024

# Convert dataframe into sf object
sf_NO2_LA_02s024 <- st_as_sf(df7_NO2_LA_02s024, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_LA_02s024_V <- vect(sf_NO2_LA_02s024)
v <- voronoi(sf_no2_LA_02s024_V)
plot(v)
points(sf_no2_LA_02s024_V)

V_no2_LA_02s024 <- crop(v, vect(st_union(la_zctas)))
plot(V_no2_LA_02s024, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_LA_02s024, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_LA_02s024, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_02s024_no2 <- data.frame(geom(sf_no2_LA_02s024_V)[,c("x", "y")], as.data.frame(sf_no2_LA_02s024_V))
head(d_02s024_no2)

# run gstat model
gs_02s024_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_02s024_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_02s024_no2 <- interpolate(r, gs_02s024_no2, debug.level=0)

# rasterize idw output
idw_02s024_no2.r <-raster(idw_02s024_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "02_s024"]
df1$address_LAT[df1$recordID == "02_s024"]

p_02s024_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "02_s024"], df1$address_LAT[df1$recordID == "02_s024"]), ncol =2))
proj4string(p_02s024_no2) <- projection(idw_02s024_no2.r)

# extract estimated NO2 value 
extract(idw_02s024_no2.r, p_02s024_no2)

#02s025 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "02_s025"]
df1$X2mo_visit_date[df1$recordID == "02_s025"]

df7_NO2_LA_02s025 <- df7_NO2_LA[df7_NO2_LA$Date.Local >= "2017-01-15" & df7_NO2_LA$Date.Local <= "2017-02-14", ]
df7_NO2_LA_02s025

# Convert dataframe into sf object
sf_NO2_LA_02s025 <- st_as_sf(df7_NO2_LA_02s025, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_LA_02s025_V <- vect(sf_NO2_LA_02s025)
v <- voronoi(sf_no2_LA_02s025_V)
plot(v)
points(sf_no2_LA_02s025_V)

V_no2_LA_02s025 <- crop(v, vect(st_union(la_zctas)))
plot(V_no2_LA_02s025, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_LA_02s025, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_LA_02s025, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_02s025_no2 <- data.frame(geom(sf_no2_LA_02s025_V)[,c("x", "y")], as.data.frame(sf_no2_LA_02s025_V))
head(d_02s025_no2)

# run gstat model
gs_02s025_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_02s025_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_02s025_no2 <- interpolate(r, gs_02s025_no2, debug.level=0)

# rasterize idw output
idw_02s025_no2.r <-raster(idw_02s025_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "02_s025"]
df1$address_LAT[df1$recordID == "02_s025"]

p_02s025_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "02_s025"], df1$address_LAT[df1$recordID == "02_s025"]), ncol =2))
proj4string(p_02s025_no2) <- projection(idw_02s025_no2.r)

# extract estimated NO2 value 
extract(idw_02s025_no2.r, p_02s025_no2)

#02s027 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "02_s027"]
df1$X2mo_visit_date[df1$recordID == "02_s027"]

df7_NO2_LA_02s027 <- df7_NO2_LA[df7_NO2_LA$Date.Local >= "2017-01-17" & df7_NO2_LA$Date.Local <= "2017-02-16", ]
df7_NO2_LA_02s027

# Convert dataframe into sf object
sf_NO2_LA_02s027 <- st_as_sf(df7_NO2_LA_02s027, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_LA_02s027_V <- vect(sf_NO2_LA_02s027)
v <- voronoi(sf_no2_LA_02s027_V)
plot(v)
points(sf_no2_LA_02s027_V)

V_no2_LA_02s027 <- crop(v, vect(st_union(la_zctas)))
plot(V_no2_LA_02s027, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_LA_02s027, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_LA_02s027, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_02s027_no2 <- data.frame(geom(sf_no2_LA_02s027_V)[,c("x", "y")], as.data.frame(sf_no2_LA_02s027_V))
head(d_02s027_no2)

# run gstat model
gs_02s027_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_02s027_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_02s027_no2 <- interpolate(r, gs_02s027_no2, debug.level=0)

# rasterize idw output
idw_02s027_no2.r <-raster(idw_02s027_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "02_s027"]
df1$address_LAT[df1$recordID == "02_s027"]

p_02s027_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "02_s027"], df1$address_LAT[df1$recordID == "02_s027"]), ncol =2))
proj4string(p_02s027_no2) <- projection(idw_02s027_no2.r)

# extract estimated NO2 value 
extract(idw_02s027_no2.r, p_02s027_no2)

#02s029 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "02_s029"]
df1$X2mo_visit_date[df1$recordID == "02_s029"]

df7_NO2_LA_02s029 <- df7_NO2_LA[df7_NO2_LA$Date.Local >= "2017-01-24" & df7_NO2_LA$Date.Local <= "2017-02-23", ]
df7_NO2_LA_02s029

# Convert dataframe into sf object
sf_NO2_LA_02s029 <- st_as_sf(df7_NO2_LA_02s029, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_LA_02s029_V <- vect(sf_NO2_LA_02s029)
v <- voronoi(sf_no2_LA_02s029_V)
plot(v)
points(sf_no2_LA_02s029_V)

V_no2_LA_02s029 <- crop(v, vect(st_union(la_zctas)))
plot(V_no2_LA_02s029, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_LA_02s029, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_LA_02s029, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_02s029_no2 <- data.frame(geom(sf_no2_LA_02s029_V)[,c("x", "y")], as.data.frame(sf_no2_LA_02s029_V))
head(d_02s029_no2)

# run gstat model
gs_02s029_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_02s029_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_02s029_no2 <- interpolate(r, gs_02s029_no2, debug.level=0)

# rasterize idw output
idw_02s029_no2.r <-raster(idw_02s029_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "02_s029"]
df1$address_LAT[df1$recordID == "02_s029"]

p_02s029_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "02_s029"], df1$address_LAT[df1$recordID == "02_s029"]), ncol =2))
proj4string(p_02s029_no2) <- projection(idw_02s029_no2.r)

# extract estimated NO2 value 
extract(idw_02s029_no2.r, p_02s029_no2)

#02s030 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "02_s030"]
df1$X2mo_visit_date[df1$recordID == "02_s030"]

df7_NO2_LA_02s030 <- df7_NO2_LA[df7_NO2_LA$Date.Local >= "2017-02-07" & df7_NO2_LA$Date.Local <= "2017-03-09", ]
df7_NO2_LA_02s030

# Convert dataframe into sf object
sf_NO2_LA_02s030 <- st_as_sf(df7_NO2_LA_02s030, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_LA_02s030_V <- vect(sf_NO2_LA_02s030)
v <- voronoi(sf_no2_LA_02s030_V)
plot(v)
points(sf_no2_LA_02s030_V)

V_no2_LA_02s030 <- crop(v, vect(st_union(la_zctas)))
plot(V_no2_LA_02s030, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_LA_02s030, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_LA_02s030, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_02s030_no2 <- data.frame(geom(sf_no2_LA_02s030_V)[,c("x", "y")], as.data.frame(sf_no2_LA_02s030_V))
head(d_02s030_no2)

# run gstat model
gs_02s030_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_02s030_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_02s030_no2 <- interpolate(r, gs_02s030_no2, debug.level=0)

# rasterize idw output
idw_02s030_no2.r <-raster(idw_02s030_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "02_s030"]
df1$address_LAT[df1$recordID == "02_s030"]

p_02s030_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "02_s030"], df1$address_LAT[df1$recordID == "02_s030"]), ncol =2))
proj4string(p_02s030_no2) <- projection(idw_02s030_no2.r)

# extract estimated NO2 value 
extract(idw_02s030_no2.r, p_02s030_no2)

#02s031 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "02_s031"]
df1$X2mo_visit_date[df1$recordID == "02_s031"]

df7_NO2_LA_02s031 <- df7_NO2_LA[df7_NO2_LA$Date.Local >= "2017-03-07" & df7_NO2_LA$Date.Local <= "2017-04-06", ]
df7_NO2_LA_02s031

# Convert dataframe into sf object
sf_NO2_LA_02s031 <- st_as_sf(df7_NO2_LA_02s031, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_LA_02s031_V <- vect(sf_NO2_LA_02s031)
v <- voronoi(sf_no2_LA_02s031_V)
plot(v)
points(sf_no2_LA_02s031_V)

V_no2_LA_02s031 <- crop(v, vect(st_union(la_zctas)))
plot(V_no2_LA_02s031, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_LA_02s031, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_LA_02s031, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_02s031_no2 <- data.frame(geom(sf_no2_LA_02s031_V)[,c("x", "y")], as.data.frame(sf_no2_LA_02s031_V))
head(d_02s031_no2)

# run gstat model
gs_02s031_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_02s031_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_02s031_no2 <- interpolate(r, gs_02s031_no2, debug.level=0)

# rasterize idw output
idw_02s031_no2.r <-raster(idw_02s031_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "02_s031"]
df1$address_LAT[df1$recordID == "02_s031"]

p_02s031_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "02_s031"], df1$address_LAT[df1$recordID == "02_s031"]), ncol =2))
proj4string(p_02s031_no2) <- projection(idw_02s031_no2.r)

# extract estimated NO2 value 
extract(idw_02s031_no2.r, p_02s031_no2)


#02s032 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "02_s032"]
df1$X2mo_visit_date[df1$recordID == "02_s032"]

df7_NO2_LA_02s032 <- df7_NO2_LA[df7_NO2_LA$Date.Local >= "2017-03-12" & df7_NO2_LA$Date.Local <= "2017-04-11", ]
df7_NO2_LA_02s032

# Convert dataframe into sf object
sf_NO2_LA_02s032 <- st_as_sf(df7_NO2_LA_02s032, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_LA_02s032_V <- vect(sf_NO2_LA_02s032)
v <- voronoi(sf_no2_LA_02s032_V)
plot(v)
points(sf_no2_LA_02s032_V)

V_no2_LA_02s032 <- crop(v, vect(st_union(la_zctas)))
plot(V_no2_LA_02s032, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_LA_02s032, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_LA_02s032, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_02s032_no2 <- data.frame(geom(sf_no2_LA_02s032_V)[,c("x", "y")], as.data.frame(sf_no2_LA_02s032_V))
head(d_02s032_no2)

# run gstat model
gs_02s032_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_02s032_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_02s032_no2 <- interpolate(r, gs_02s032_no2, debug.level=0)

# rasterize idw output
idw_02s032_no2.r <-raster(idw_02s032_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "02_s032"]
df1$address_LAT[df1$recordID == "02_s032"]

p_02s032_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "02_s032"], df1$address_LAT[df1$recordID == "02_s032"]), ncol =2))
proj4string(p_02s032_no2) <- projection(idw_02s032_no2.r)

# extract estimated NO2 value 
extract(idw_02s032_no2.r, p_02s032_no2)

#02s033 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "02_s033"]
df1$X2mo_visit_date[df1$recordID == "02_s033"]

df7_NO2_LA_02s033 <- df7_NO2_LA[df7_NO2_LA$Date.Local >= "2017-03-14" & df7_NO2_LA$Date.Local <= "2017-04-13", ]
df7_NO2_LA_02s033

# Convert dataframe into sf object
sf_NO2_LA_02s033 <- st_as_sf(df7_NO2_LA_02s033, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_LA_02s033_V <- vect(sf_NO2_LA_02s033)
v <- voronoi(sf_no2_LA_02s033_V)
plot(v)
points(sf_no2_LA_02s033_V)

V_no2_LA_02s033 <- crop(v, vect(st_union(la_zctas)))
plot(V_no2_LA_02s033, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_LA_02s033, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_LA_02s033, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_02s033_no2 <- data.frame(geom(sf_no2_LA_02s033_V)[,c("x", "y")], as.data.frame(sf_no2_LA_02s033_V))
head(d_02s033_no2)

# run gstat model
gs_02s033_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_02s033_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_02s033_no2 <- interpolate(r, gs_02s033_no2, debug.level=0)

# rasterize idw output
idw_02s033_no2.r <-raster(idw_02s033_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "02_s033"]
df1$address_LAT[df1$recordID == "02_s033"]

p_02s033_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "02_s033"], df1$address_LAT[df1$recordID == "02_s033"]), ncol =2))
proj4string(p_02s033_no2) <- projection(idw_02s033_no2.r)

# extract estimated NO2 value 
extract(idw_02s033_no2.r, p_02s033_no2)

#02s034 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "02_s034"]
df1$X2mo_visit_date[df1$recordID == "02_s034"]

df7_NO2_LA_02s034 <- df7_NO2_LA[df7_NO2_LA$Date.Local >= "2017-03-14" & df7_NO2_LA$Date.Local <= "2017-04-13", ]
df7_NO2_LA_02s034

# Convert dataframe into sf object
sf_NO2_LA_02s034 <- st_as_sf(df7_NO2_LA_02s034, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_LA_02s034_V <- vect(sf_NO2_LA_02s034)
v <- voronoi(sf_no2_LA_02s034_V)
plot(v)
points(sf_no2_LA_02s034_V)

V_no2_LA_02s034 <- crop(v, vect(st_union(la_zctas)))
plot(V_no2_LA_02s034, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_LA_02s034, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_LA_02s034, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_02s034_no2 <- data.frame(geom(sf_no2_LA_02s034_V)[,c("x", "y")], as.data.frame(sf_no2_LA_02s034_V))
head(d_02s034_no2)

# run gstat model
gs_02s034_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_02s034_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_02s034_no2 <- interpolate(r, gs_02s034_no2, debug.level=0)

# rasterize idw output
idw_02s034_no2.r <-raster(idw_02s034_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "02_s034"]
df1$address_LAT[df1$recordID == "02_s034"]

p_02s034_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "02_s034"], df1$address_LAT[df1$recordID == "02_s034"]), ncol =2))
proj4string(p_02s034_no2) <- projection(idw_02s034_no2.r)

# extract estimated NO2 value 
extract(idw_02s034_no2.r, p_02s034_no2)

#02s035 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "02_s035"]
df1$X2mo_visit_date[df1$recordID == "02_s035"]

df7_NO2_LA_02s035 <- df7_NO2_LA[df7_NO2_LA$Date.Local >= "2017-03-28" & df7_NO2_LA$Date.Local <= "2017-04-27", ]
df7_NO2_LA_02s035

# Convert dataframe into sf object
sf_NO2_LA_02s035 <- st_as_sf(df7_NO2_LA_02s035, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_LA_02s035_V <- vect(sf_NO2_LA_02s035)
v <- voronoi(sf_no2_LA_02s035_V)
plot(v)
points(sf_no2_LA_02s035_V)

V_no2_LA_02s035 <- crop(v, vect(st_union(la_zctas)))
plot(V_no2_LA_02s035, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_LA_02s035, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_LA_02s035, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_02s035_no2 <- data.frame(geom(sf_no2_LA_02s035_V)[,c("x", "y")], as.data.frame(sf_no2_LA_02s035_V))
head(d_02s035_no2)

# run gstat model
gs_02s035_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_02s035_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_02s035_no2 <- interpolate(r, gs_02s035_no2, debug.level=0)

# rasterize idw output
idw_02s035_no2.r <-raster(idw_02s035_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "02_s035"]
df1$address_LAT[df1$recordID == "02_s035"]

p_02s035_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "02_s035"], df1$address_LAT[df1$recordID == "02_s035"]), ncol =2))
proj4string(p_02s035_no2) <- projection(idw_02s035_no2.r)

# extract estimated NO2 value 
extract(idw_02s035_no2.r, p_02s035_no2)

#02s036 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "02_s036"]
df1$X2mo_visit_date[df1$recordID == "02_s036"]

df7_NO2_LA_02s036 <- df7_NO2_LA[df7_NO2_LA$Date.Local >= "2017-04-16" & df7_NO2_LA$Date.Local <= "2017-05-16", ]
df7_NO2_LA_02s036

# Convert dataframe into sf object
sf_NO2_LA_02s036 <- st_as_sf(df7_NO2_LA_02s036, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_LA_02s036_V <- vect(sf_NO2_LA_02s036)
v <- voronoi(sf_no2_LA_02s036_V)
plot(v)
points(sf_no2_LA_02s036_V)

V_no2_LA_02s036 <- crop(v, vect(st_union(la_zctas)))
plot(V_no2_LA_02s036, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_LA_02s036, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_LA_02s036, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_02s036_no2 <- data.frame(geom(sf_no2_LA_02s036_V)[,c("x", "y")], as.data.frame(sf_no2_LA_02s036_V))
head(d_02s036_no2)

# run gstat model
gs_02s036_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_02s036_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_02s036_no2 <- interpolate(r, gs_02s036_no2, debug.level=0)

# rasterize idw output
idw_02s036_no2.r <-raster(idw_02s036_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "02_s036"]
df1$address_LAT[df1$recordID == "02_s036"]

p_02s036_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "02_s036"], df1$address_LAT[df1$recordID == "02_s036"]), ncol =2))
proj4string(p_02s036_no2) <- projection(idw_02s036_no2.r)

# extract estimated NO2 value 
extract(idw_02s036_no2.r, p_02s036_no2)

#02s038 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "02_s038"]
df1$X2mo_visit_date[df1$recordID == "02_s038"]

df7_NO2_LA_02s038 <- df7_NO2_LA[df7_NO2_LA$Date.Local >= "2017-05-21" & df7_NO2_LA$Date.Local <= "2017-06-20", ]
df7_NO2_LA_02s038

# Convert dataframe into sf object
sf_NO2_LA_02s038 <- st_as_sf(df7_NO2_LA_02s038, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_LA_02s038_V <- vect(sf_NO2_LA_02s038)
v <- voronoi(sf_no2_LA_02s038_V)
plot(v)
points(sf_no2_LA_02s038_V)

V_no2_LA_02s038 <- crop(v, vect(st_union(la_zctas)))
plot(V_no2_LA_02s038, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_LA_02s038, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_LA_02s038, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_02s038_no2 <- data.frame(geom(sf_no2_LA_02s038_V)[,c("x", "y")], as.data.frame(sf_no2_LA_02s038_V))
head(d_02s038_no2)

# run gstat model
gs_02s038_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_02s038_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_02s038_no2 <- interpolate(r, gs_02s038_no2, debug.level=0)

# rasterize idw output
idw_02s038_no2.r <-raster(idw_02s038_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "02_s038"]
df1$address_LAT[df1$recordID == "02_s038"]

p_02s038_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "02_s038"], df1$address_LAT[df1$recordID == "02_s038"]), ncol =2))
proj4string(p_02s038_no2) <- projection(idw_02s038_no2.r)

# extract estimated NO2 value 
extract(idw_02s038_no2.r, p_02s038_no2)

#02s040 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "02_s040"]
df1$X2mo_visit_date[df1$recordID == "02_s040"]

df7_NO2_LA_02s040 <- df7_NO2_LA[df7_NO2_LA$Date.Local >= "2017-06-18" & df7_NO2_LA$Date.Local <= "2017-07-18", ]
df7_NO2_LA_02s040

# Convert dataframe into sf object
sf_NO2_LA_02s040 <- st_as_sf(df7_NO2_LA_02s040, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_LA_02s040_V <- vect(sf_NO2_LA_02s040)
v <- voronoi(sf_no2_LA_02s040_V)
plot(v)
points(sf_no2_LA_02s040_V)

V_no2_LA_02s040 <- crop(v, vect(st_union(la_zctas)))
plot(V_no2_LA_02s040, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_LA_02s040, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_LA_02s040, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_02s040_no2 <- data.frame(geom(sf_no2_LA_02s040_V)[,c("x", "y")], as.data.frame(sf_no2_LA_02s040_V))
head(d_02s040_no2)

# run gstat model
gs_02s040_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_02s040_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_02s040_no2 <- interpolate(r, gs_02s040_no2, debug.level=0)

# rasterize idw output
idw_02s040_no2.r <-raster(idw_02s040_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "02_s040"]
df1$address_LAT[df1$recordID == "02_s040"]

p_02s040_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "02_s040"], df1$address_LAT[df1$recordID == "02_s040"]), ncol =2))
proj4string(p_02s040_no2) <- projection(idw_02s040_no2.r)

# extract estimated NO2 value 
extract(idw_02s040_no2.r, p_02s040_no2)

#02s041 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "02_s041"]
df1$X2mo_visit_date[df1$recordID == "02_s041"]

df7_NO2_LA_02s041 <- df7_NO2_LA[df7_NO2_LA$Date.Local >= "2017-06-18" & df7_NO2_LA$Date.Local <= "2017-07-18", ]
df7_NO2_LA_02s041

# Convert dataframe into sf object
sf_NO2_LA_02s041 <- st_as_sf(df7_NO2_LA_02s041, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_LA_02s041_V <- vect(sf_NO2_LA_02s041)
v <- voronoi(sf_no2_LA_02s041_V)
plot(v)
points(sf_no2_LA_02s041_V)

V_no2_LA_02s041 <- crop(v, vect(st_union(la_zctas)))
plot(V_no2_LA_02s041, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_LA_02s041, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_LA_02s041, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_02s041_no2 <- data.frame(geom(sf_no2_LA_02s041_V)[,c("x", "y")], as.data.frame(sf_no2_LA_02s041_V))
head(d_02s041_no2)

# run gstat model
gs_02s041_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_02s041_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_02s041_no2 <- interpolate(r, gs_02s041_no2, debug.level=0)

# rasterize idw output
idw_02s041_no2.r <-raster(idw_02s041_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "02_s041"]
df1$address_LAT[df1$recordID == "02_s041"]

p_02s041_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "02_s041"], df1$address_LAT[df1$recordID == "02_s041"]), ncol =2))
proj4string(p_02s041_no2) <- projection(idw_02s041_no2.r)

# extract estimated NO2 value 
extract(idw_02s041_no2.r, p_02s041_no2)

#02s042 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "02_s042"]
df1$X2mo_visit_date[df1$recordID == "02_s042"]

df7_NO2_LA_02s042 <- df7_NO2_LA[df7_NO2_LA$Date.Local >= "2017-07-16" & df7_NO2_LA$Date.Local <= "2017-08-15", ]
df7_NO2_LA_02s042

# Convert dataframe into sf object
sf_NO2_LA_02s042 <- st_as_sf(df7_NO2_LA_02s042, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_LA_02s042_V <- vect(sf_NO2_LA_02s042)
v <- voronoi(sf_no2_LA_02s042_V)
plot(v)
points(sf_no2_LA_02s042_V)

V_no2_LA_02s042 <- crop(v, vect(st_union(la_zctas)))
plot(V_no2_LA_02s042, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_LA_02s042, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_LA_02s042, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_02s042_no2 <- data.frame(geom(sf_no2_LA_02s042_V)[,c("x", "y")], as.data.frame(sf_no2_LA_02s042_V))
head(d_02s042_no2)

# run gstat model
gs_02s042_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_02s042_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_02s042_no2 <- interpolate(r, gs_02s042_no2, debug.level=0)

# rasterize idw output
idw_02s042_no2.r <-raster(idw_02s042_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "02_s042"]
df1$address_LAT[df1$recordID == "02_s042"]

p_02s042_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "02_s042"], df1$address_LAT[df1$recordID == "02_s042"]), ncol =2))
proj4string(p_02s042_no2) <- projection(idw_02s042_no2.r)

# extract estimated NO2 value 
extract(idw_02s042_no2.r, p_02s042_no2)

#02s043 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "02_s043"]
df1$X2mo_visit_date[df1$recordID == "02_s043"]

df7_NO2_LA_02s043 <- df7_NO2_LA[df7_NO2_LA$Date.Local >= "2017-07-18" & df7_NO2_LA$Date.Local <= "2017-08-17", ]
df7_NO2_LA_02s043

# Convert dataframe into sf object
sf_NO2_LA_02s043 <- st_as_sf(df7_NO2_LA_02s043, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_LA_02s043_V <- vect(sf_NO2_LA_02s043)
v <- voronoi(sf_no2_LA_02s043_V)
plot(v)
points(sf_no2_LA_02s043_V)

V_no2_LA_02s043 <- crop(v, vect(st_union(la_zctas)))
plot(V_no2_LA_02s043, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_LA_02s043, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_LA_02s043, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_02s043_no2 <- data.frame(geom(sf_no2_LA_02s043_V)[,c("x", "y")], as.data.frame(sf_no2_LA_02s043_V))
head(d_02s043_no2)

# run gstat model
gs_02s043_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_02s043_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_02s043_no2 <- interpolate(r, gs_02s043_no2, debug.level=0)

# rasterize idw output
idw_02s043_no2.r <-raster(idw_02s043_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "02_s043"]
df1$address_LAT[df1$recordID == "02_s043"]

p_02s043_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "02_s043"], df1$address_LAT[df1$recordID == "02_s043"]), ncol =2))
proj4string(p_02s043_no2) <- projection(idw_02s043_no2.r)

# extract estimated NO2 value 
extract(idw_02s043_no2.r, p_02s043_no2)

#02s045 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "02_s045"]
df1$X2mo_visit_date[df1$recordID == "02_s045"]

df7_NO2_LA_02s045 <- df7_NO2_LA[df7_NO2_LA$Date.Local >= "2017-07-25" & df7_NO2_LA$Date.Local <= "2017-08-24", ]
df7_NO2_LA_02s045

# Convert dataframe into sf object
sf_NO2_LA_02s045 <- st_as_sf(df7_NO2_LA_02s045, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_LA_02s045_V <- vect(sf_NO2_LA_02s045)
v <- voronoi(sf_no2_LA_02s045_V)
plot(v)
points(sf_no2_LA_02s045_V)

V_no2_LA_02s045 <- crop(v, vect(st_union(la_zctas)))
plot(V_no2_LA_02s045, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_LA_02s045, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_LA_02s045, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_02s045_no2 <- data.frame(geom(sf_no2_LA_02s045_V)[,c("x", "y")], as.data.frame(sf_no2_LA_02s045_V))
head(d_02s045_no2)

# run gstat model
gs_02s045_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_02s045_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_02s045_no2 <- interpolate(r, gs_02s045_no2, debug.level=0)

# rasterize idw output
idw_02s045_no2.r <-raster(idw_02s045_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "02_s045"]
df1$address_LAT[df1$recordID == "02_s045"]

p_02s045_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "02_s045"], df1$address_LAT[df1$recordID == "02_s045"]), ncol =2))
proj4string(p_02s045_no2) <- projection(idw_02s045_no2.r)

# extract estimated NO2 value 
extract(idw_02s045_no2.r, p_02s045_no2)

#02s046 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "02_s046"]
df1$X2mo_visit_date[df1$recordID == "02_s046"]

df7_NO2_LA_02s046 <- df7_NO2_LA[df7_NO2_LA$Date.Local >= "2017-08-22" & df7_NO2_LA$Date.Local <= "2017-09-21", ]
df7_NO2_LA_02s046

# Convert dataframe into sf object
sf_NO2_LA_02s046 <- st_as_sf(df7_NO2_LA_02s046, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_LA_02s046_V <- vect(sf_NO2_LA_02s046)
v <- voronoi(sf_no2_LA_02s046_V)
plot(v)
points(sf_no2_LA_02s046_V)

V_no2_LA_02s046 <- crop(v, vect(st_union(la_zctas)))
plot(V_no2_LA_02s046, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_LA_02s046, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_LA_02s046, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_02s046_no2 <- data.frame(geom(sf_no2_LA_02s046_V)[,c("x", "y")], as.data.frame(sf_no2_LA_02s046_V))
head(d_02s046_no2)

# run gstat model
gs_02s046_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_02s046_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_02s046_no2 <- interpolate(r, gs_02s046_no2, debug.level=0)

# rasterize idw output
idw_02s046_no2.r <-raster(idw_02s046_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "02_s046"]
df1$address_LAT[df1$recordID == "02_s046"]

p_02s046_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "02_s046"], df1$address_LAT[df1$recordID == "02_s046"]), ncol =2))
proj4string(p_02s046_no2) <- projection(idw_02s046_no2.r)

# extract estimated NO2 value 
extract(idw_02s046_no2.r, p_02s046_no2)

#02s047 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "02_s047"]
df1$X2mo_visit_date[df1$recordID == "02_s047"]

df7_NO2_LA_02s047 <- df7_NO2_LA[df7_NO2_LA$Date.Local >= "2017-09-03" & df7_NO2_LA$Date.Local <= "2017-10-03", ]
df7_NO2_LA_02s047

# Convert dataframe into sf object
sf_NO2_LA_02s047 <- st_as_sf(df7_NO2_LA_02s047, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_LA_02s047_V <- vect(sf_NO2_LA_02s047)
v <- voronoi(sf_no2_LA_02s047_V)
plot(v)
points(sf_no2_LA_02s047_V)

V_no2_LA_02s047 <- crop(v, vect(st_union(la_zctas)))
plot(V_no2_LA_02s047, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_LA_02s047, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_LA_02s047, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_02s047_no2 <- data.frame(geom(sf_no2_LA_02s047_V)[,c("x", "y")], as.data.frame(sf_no2_LA_02s047_V))
head(d_02s047_no2)

# run gstat model
gs_02s047_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_02s047_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_02s047_no2 <- interpolate(r, gs_02s047_no2, debug.level=0)

# rasterize idw output
idw_02s047_no2.r <-raster(idw_02s047_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "02_s047"]
df1$address_LAT[df1$recordID == "02_s047"]

p_02s047_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "02_s047"], df1$address_LAT[df1$recordID == "02_s047"]), ncol =2))
proj4string(p_02s047_no2) <- projection(idw_02s047_no2.r)

# extract estimated NO2 value 
extract(idw_02s047_no2.r, p_02s047_no2)

#02s048 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "02_s048"]
df1$X2mo_visit_date[df1$recordID == "02_s048"]

df7_NO2_LA_02s048 <- df7_NO2_LA[df7_NO2_LA$Date.Local >= "2017-09-05" & df7_NO2_LA$Date.Local <= "2017-10-05", ]
df7_NO2_LA_02s048

# Convert dataframe into sf object
sf_NO2_LA_02s048 <- st_as_sf(df7_NO2_LA_02s048, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_LA_02s048_V <- vect(sf_NO2_LA_02s048)
v <- voronoi(sf_no2_LA_02s048_V)
plot(v)
points(sf_no2_LA_02s048_V)

V_no2_LA_02s048 <- crop(v, vect(st_union(la_zctas)))
plot(V_no2_LA_02s048, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_LA_02s048, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_LA_02s048, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_02s048_no2 <- data.frame(geom(sf_no2_LA_02s048_V)[,c("x", "y")], as.data.frame(sf_no2_LA_02s048_V))
head(d_02s048_no2)

# run gstat model
gs_02s048_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_02s048_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_02s048_no2 <- interpolate(r, gs_02s048_no2, debug.level=0)

# rasterize idw output
idw_02s048_no2.r <-raster(idw_02s048_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "02_s048"]
df1$address_LAT[df1$recordID == "02_s048"]

p_02s048_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "02_s048"], df1$address_LAT[df1$recordID == "02_s048"]), ncol =2))
proj4string(p_02s048_no2) <- projection(idw_02s048_no2.r)

# extract estimated NO2 value 
extract(idw_02s048_no2.r, p_02s048_no2)

#02s049 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "02_s049"]
df1$X2mo_visit_date[df1$recordID == "02_s049"]

df7_NO2_LA_02s049 <- df7_NO2_LA[df7_NO2_LA$Date.Local >= "2017-10-22" & df7_NO2_LA$Date.Local <= "2017-11-21", ]
df7_NO2_LA_02s049

# Convert dataframe into sf object
sf_NO2_LA_02s049 <- st_as_sf(df7_NO2_LA_02s049, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_LA_02s049_V <- vect(sf_NO2_LA_02s049)
v <- voronoi(sf_no2_LA_02s049_V)
plot(v)
points(sf_no2_LA_02s049_V)

V_no2_LA_02s049 <- crop(v, vect(st_union(la_zctas)))
plot(V_no2_LA_02s049, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_LA_02s049, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_LA_02s049, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_02s049_no2 <- data.frame(geom(sf_no2_LA_02s049_V)[,c("x", "y")], as.data.frame(sf_no2_LA_02s049_V))
head(d_02s049_no2)

# run gstat model
gs_02s049_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_02s049_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_02s049_no2 <- interpolate(r, gs_02s049_no2, debug.level=0)

# rasterize idw output
idw_02s049_no2.r <-raster(idw_02s049_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "02_s049"]
df1$address_LAT[df1$recordID == "02_s049"]

p_02s049_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "02_s049"], df1$address_LAT[df1$recordID == "02_s049"]), ncol =2))
proj4string(p_02s049_no2) <- projection(idw_02s049_no2.r)

# extract estimated NO2 value 
extract(idw_02s049_no2.r, p_02s049_no2)

#02s050 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "02_s050"]
df1$X2mo_visit_date[df1$recordID == "02_s050"]

df7_NO2_LA_02s050 <- df7_NO2_LA[df7_NO2_LA$Date.Local >= "2017-11-12" & df7_NO2_LA$Date.Local <= "2017-12-12", ]
df7_NO2_LA_02s050

# Convert dataframe into sf object
sf_NO2_LA_02s050 <- st_as_sf(df7_NO2_LA_02s050, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_LA_02s050_V <- vect(sf_NO2_LA_02s050)
v <- voronoi(sf_no2_LA_02s050_V)
plot(v)
points(sf_no2_LA_02s050_V)

V_no2_LA_02s050 <- crop(v, vect(st_union(la_zctas)))
plot(V_no2_LA_02s050, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_LA_02s050, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_LA_02s050, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_02s050_no2 <- data.frame(geom(sf_no2_LA_02s050_V)[,c("x", "y")], as.data.frame(sf_no2_LA_02s050_V))
head(d_02s050_no2)

# run gstat model
gs_02s050_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_02s050_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_02s050_no2 <- interpolate(r, gs_02s050_no2, debug.level=0)

# rasterize idw output
idw_02s050_no2.r <-raster(idw_02s050_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "02_s050"]
df1$address_LAT[df1$recordID == "02_s050"]

p_02s050_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "02_s050"], df1$address_LAT[df1$recordID == "02_s050"]), ncol =2))
proj4string(p_02s050_no2) <- projection(idw_02s050_no2.r)

# extract estimated NO2 value 
extract(idw_02s050_no2.r, p_02s050_no2)

#02s051 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "02_s051"]
df1$X2mo_visit_date[df1$recordID == "02_s051"]

df7_NO2_LA_02s051 <- df7_NO2_LA[df7_NO2_LA$Date.Local >= "2017-12-10" & df7_NO2_LA$Date.Local <= "2018-01-09", ]
df7_NO2_LA_02s051

# Convert dataframe into sf object
sf_NO2_LA_02s051 <- st_as_sf(df7_NO2_LA_02s051, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_LA_02s051_V <- vect(sf_NO2_LA_02s051)
v <- voronoi(sf_no2_LA_02s051_V)
plot(v)
points(sf_no2_LA_02s051_V)

V_no2_LA_02s051 <- crop(v, vect(st_union(la_zctas)))
plot(V_no2_LA_02s051, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_LA_02s051, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_LA_02s051, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_02s051_no2 <- data.frame(geom(sf_no2_LA_02s051_V)[,c("x", "y")], as.data.frame(sf_no2_LA_02s051_V))
head(d_02s051_no2)

# run gstat model
gs_02s051_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_02s051_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_02s051_no2 <- interpolate(r, gs_02s051_no2, debug.level=0)

# rasterize idw output
idw_02s051_no2.r <-raster(idw_02s051_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "02_s051"]
df1$address_LAT[df1$recordID == "02_s051"]

p_02s051_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "02_s051"], df1$address_LAT[df1$recordID == "02_s051"]), ncol =2))
proj4string(p_02s051_no2) <- projection(idw_02s051_no2.r)

# extract estimated NO2 value 
extract(idw_02s051_no2.r, p_02s051_no2)

#02s052 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "02_s052"]
df1$X2mo_visit_date[df1$recordID == "02_s052"]

df7_NO2_LA_02s052 <- df7_NO2_LA[df7_NO2_LA$Date.Local >= "2017-12-26" & df7_NO2_LA$Date.Local <= "2018-01-25", ]
df7_NO2_LA_02s052

# Convert dataframe into sf object
sf_NO2_LA_02s052 <- st_as_sf(df7_NO2_LA_02s052, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_LA_02s052_V <- vect(sf_NO2_LA_02s052)
v <- voronoi(sf_no2_LA_02s052_V)
plot(v)
points(sf_no2_LA_02s052_V)

V_no2_LA_02s052 <- crop(v, vect(st_union(la_zctas)))
plot(V_no2_LA_02s052, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_LA_02s052, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_LA_02s052, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_02s052_no2 <- data.frame(geom(sf_no2_LA_02s052_V)[,c("x", "y")], as.data.frame(sf_no2_LA_02s052_V))
head(d_02s051_no2)

# run gstat model
gs_02s052_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_02s052_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_02s052_no2 <- interpolate(r, gs_02s052_no2, debug.level=0)

# rasterize idw output
idw_02s052_no2.r <-raster(idw_02s052_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "02_s052"]
df1$address_LAT[df1$recordID == "02_s052"]

p_02s052_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "02_s052"], df1$address_LAT[df1$recordID == "02_s052"]), ncol =2))
proj4string(p_02s052_no2) <- projection(idw_02s052_no2.r)

# extract estimated NO2 value 
extract(idw_02s052_no2.r, p_02s052_no2)

#02s054 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "02_s054"]
df1$X2mo_visit_date[df1$recordID == "02_s054"]

df7_NO2_LA_02s054 <- df7_NO2_LA[df7_NO2_LA$Date.Local >= "2018-01-28" & df7_NO2_LA$Date.Local <= "2018-02-27", ]
df7_NO2_LA_02s054

# Convert dataframe into sf object
sf_NO2_LA_02s054 <- st_as_sf(df7_NO2_LA_02s054, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_LA_02s054_V <- vect(sf_NO2_LA_02s054)
v <- voronoi(sf_no2_LA_02s054_V)
plot(v)
points(sf_no2_LA_02s054_V)

V_no2_LA_02s054 <- crop(v, vect(st_union(la_zctas)))
plot(V_no2_LA_02s054, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_LA_02s054, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_LA_02s054, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_02s054_no2 <- data.frame(geom(sf_no2_LA_02s054_V)[,c("x", "y")], as.data.frame(sf_no2_LA_02s054_V))
head(d_02s054_no2)

# run gstat model
gs_02s054_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_02s054_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_02s054_no2 <- interpolate(r, gs_02s054_no2, debug.level=0)

# rasterize idw output
idw_02s054_no2.r <-raster(idw_02s054_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "02_s054"]
df1$address_LAT[df1$recordID == "02_s054"]

p_02s054_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "02_s054"], df1$address_LAT[df1$recordID == "02_s054"]), ncol =2))
proj4string(p_02s054_no2) <- projection(idw_02s054_no2.r)

# extract estimated NO2 value 
extract(idw_02s054_no2.r, p_02s054_no2)

#INTERPOLATE BOSTON NO2 POLLUTION VALUES###################################

# Load LA NO2 data
df7_NO2_BOSTON <- subset(df7, State.Code > 6, select=c('Longitude','Latitude', 'Arithmetic.Mean', 'Date.Local'))
df7_NO2_BOSTON

# Specifying LA urban areas.
uas <- urban_areas() %>% filter(str_detect(NAME10, "Boston"))
uas <- st_transform(uas, crs = 4326)
st_crs(uas)
st_combine(uas)

boston_no2_sf <- st_as_sf(df7_NO2_BOSTON, coords = c('Longitude', 'Latitude'), crs = 4326)
boston_zctas <- st_transform(boston_zctas, crs=4326)
st_combine(boston_zctas)

ggplot() +
  geom_sf(data = uas)+
  geom_sf(data=boston_zctas, fill = '#ffffff') + 
  geom_sf(data=boston_no2_sf, aes(col=Arithmetic.Mean), size = 4)+
  coord_sf(xlim = c(-72.5, -70.5), ylim = c(41.8, 43))+
  theme(plot.title = element_text(size=26), axis.text = element_text(size = 18),axis.title = element_text(size = 20), panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  ggtitle("JPB1 Boston EPA Sensors NO2 Reading")

# proximity (Voronoi/Thiessen) polygons
boston_no2_sf_V <- vect(boston_no2_sf)
v <- voronoi(boston_no2_sf_V)
plot(v)
points(boston_no2_sf_V)

V_boston_no2_sf <- crop(v, vect(st_union(boston_zctas)))
plot(V_boston_no2_sf, "Arithmetic.Mean")

# rasterize
r <- rast(V_boston_no2_sf, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_boston_no2_sf, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d <- data.frame(geom(boston_no2_sf_V)[,c("x", "y")], as.data.frame(boston_no2_sf_V))
head(d)

# create model
gs <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d, nmax=Inf, set=list(idp=2))

# run idw
idw <- interpolate(r, gs, debug.level=0)

# plot idw
idwr <- mask(idw, vr)
plot(idwr, 1)

ggplot() +
  #geom_sf(data = uas)+
  geom_spatraster(data = idw, aes(fill = var1.pred))+
  geom_sf(data = boston_zctas, fill = NA)+
  #geom_sf(data = boston_sf, aes(), size = 2, color = '#e31a1c')+
  #geom_sf(data = boston_no2_sf, size = 3, color ='#9e0142', shape = 15, alpha = 0.8)+
  #geom_sf(data = boston_pm2.5_sf, size = 4, color ='#c51b7d', shape = 19, alpha = 1)+
  #geom_sf(data = boston_O3_sf, size = 3, color ='#1a9850', shape = 18)+
  coord_sf(xlim = c(-72.5, -70.5), ylim = c(41.8, 43))+
  theme(plot.title = element_text(size=26), axis.text = element_text(size = 14),axis.title = element_text(size = 20), panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  ggtitle("Boston Interpolated PM2.5 Exposure")+
  scale_fill_distiller(palette = "YlOrRd", trans = "reverse")+
  labs(fill = "PM2.5 Exposure\n(µg/m3)")

#01s001 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "01_s001"]
df1$X2mo_visit_date[df1$recordID == "01_s001"]

df7_NO2_BOSTON_01s001 <- df7_NO2_BOSTON[df7_NO2_BOSTON$Date.Local >= "2016-01-10" & df7_NO2_BOSTON$Date.Local <= "2016-02-09", ]
df7_NO2_BOSTON_01s001

# Convert dataframe into sf object
sf_no2_BOSTON_01s001 <- st_as_sf(df7_NO2_BOSTON_01s001, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_BOSTON_01s001_V <- vect(sf_no2_BOSTON_01s001)
v <- voronoi(sf_no2_BOSTON_01s001_V)
plot(v)
points(sf_no2_BOSTON_01s001_V)

V_no2_BOSTON_01s001 <- crop(v, vect(st_union(boston_zctas)))
plot(V_no2_BOSTON_01s001, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_BOSTON_01s001, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_BOSTON_01s001, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_01s001_no2 <- data.frame(geom(sf_no2_BOSTON_01s001_V)[,c("x", "y")], as.data.frame(sf_no2_BOSTON_01s001_V))
head(d_01s001_no2)

# run gstat model
gs_01s001_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_01s001_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_01s001_no2 <- interpolate(r, gs_01s001_no2, debug.level=0)

# rasterize idw output
idw_01s001_no2.r <-raster(idw_01s001_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "01_s001"]
df1$address_LAT[df1$recordID == "01_s001"]

p_01s001_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "01_s001"], df1$address_LAT[df1$recordID == "01_s001"]), ncol =2))
proj4string(p_01s001_no2) <- projection(idw_01s001_no2.r)

# extract estimated NO2 value 
extract(idw_01s001_no2.r, p_01s001_no2)

#01s003 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "01_s003"]
df1$X2mo_visit_date[df1$recordID == "01_s003"]

df7_NO2_BOSTON_01s003 <- df7_NO2_BOSTON[df7_NO2_BOSTON$Date.Local >= "2016-01-18" & df7_NO2_BOSTON$Date.Local <= "2016-02-17", ]
df7_NO2_BOSTON_01s003

# Convert dataframe into sf object
sf_no2_BOSTON_01s003 <- st_as_sf(df7_NO2_BOSTON_01s003, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_BOSTON_01s003_V <- vect(sf_no2_BOSTON_01s003)
v <- voronoi(sf_no2_BOSTON_01s003_V)
plot(v)
points(sf_no2_BOSTON_01s003_V)

V_no2_BOSTON_01s003 <- crop(v, vect(st_union(boston_zctas)))
plot(V_no2_BOSTON_01s003, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_BOSTON_01s003, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_BOSTON_01s003, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_01s003_no2 <- data.frame(geom(sf_no2_BOSTON_01s003_V)[,c("x", "y")], as.data.frame(sf_no2_BOSTON_01s003_V))
head(d_01s003_no2)

# run gstat model
gs_01s003_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_01s003_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_01s003_no2 <- interpolate(r, gs_01s003_no2, debug.level=0)

# rasterize idw output
idw_01s003_no2.r <-raster(idw_01s003_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "01_s003"]
df1$address_LAT[df1$recordID == "01_s003"]

p_01s003_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "01_s003"], df1$address_LAT[df1$recordID == "01_s003"]), ncol =2))
proj4string(p_01s003_no2) <- projection(idw_01s003_no2.r)

# extract estimated NO2 value 
extract(idw_01s003_no2.r, p_01s003_no2)

#01s004 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "01_s004"]
df1$X2mo_visit_date[df1$recordID == "01_s004"]

df7_NO2_BOSTON_01s004 <- df7_NO2_BOSTON[df7_NO2_BOSTON$Date.Local >= "2016-01-23" & df7_NO2_BOSTON$Date.Local <= "2016-02-22", ]
df7_NO2_BOSTON_01s004

# Convert dataframe into sf object
sf_no2_BOSTON_01s004 <- st_as_sf(df7_NO2_BOSTON_01s004, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_BOSTON_01s004_V <- vect(sf_no2_BOSTON_01s004)
v <- voronoi(sf_no2_BOSTON_01s004_V)
plot(v)
points(sf_no2_BOSTON_01s004_V)

V_no2_BOSTON_01s004 <- crop(v, vect(st_union(boston_zctas)))
plot(V_no2_BOSTON_01s004, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_BOSTON_01s004, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_BOSTON_01s004, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_01s004_no2 <- data.frame(geom(sf_no2_BOSTON_01s004_V)[,c("x", "y")], as.data.frame(sf_no2_BOSTON_01s004_V))
head(d_01s004_no2)

# run gstat model
gs_01s004_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_01s004_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_01s004_no2 <- interpolate(r, gs_01s004_no2, debug.level=0)

# rasterize idw output
idw_01s004_no2.r <-raster(idw_01s004_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "01_s004"]
df1$address_LAT[df1$recordID == "01_s004"]

p_01s004_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "01_s004"], df1$address_LAT[df1$recordID == "01_s004"]), ncol =2))
proj4string(p_01s004_no2) <- projection(idw_01s004_no2.r)

# extract estimated NO2 value 
extract(idw_01s004_no2.r, p_01s004_no2)

#01s005 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "01_s005"]
df1$X2mo_visit_date[df1$recordID == "01_s005"]

df7_NO2_BOSTON_01s005 <- df7_NO2_BOSTON[df7_NO2_BOSTON$Date.Local >= "2016-01-30" & df7_NO2_BOSTON$Date.Local <= "2016-02-29", ]
df7_NO2_BOSTON_01s005

# Convert dataframe into sf object
sf_no2_BOSTON_01s005 <- st_as_sf(df7_NO2_BOSTON_01s005, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_BOSTON_01s005_V <- vect(sf_no2_BOSTON_01s005)
v <- voronoi(sf_no2_BOSTON_01s005_V)
plot(v)
points(sf_no2_BOSTON_01s005_V)

V_no2_BOSTON_01s005 <- crop(v, vect(st_union(boston_zctas)))
plot(V_no2_BOSTON_01s005, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_BOSTON_01s005, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_BOSTON_01s005, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_01s005_no2 <- data.frame(geom(sf_no2_BOSTON_01s005_V)[,c("x", "y")], as.data.frame(sf_no2_BOSTON_01s005_V))
head(d_01s005_no2)

# run gstat model
gs_01s005_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_01s005_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_01s005_no2 <- interpolate(r, gs_01s005_no2, debug.level=0)

# rasterize idw output
idw_01s005_no2.r <-raster(idw_01s005_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "01_s005"]
df1$address_LAT[df1$recordID == "01_s005"]

p_01s005_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "01_s005"], df1$address_LAT[df1$recordID == "01_s005"]), ncol =2))
proj4string(p_01s005_no2) <- projection(idw_01s005_no2.r)

# extract estimated NO2 value 
extract(idw_01s005_no2.r, p_01s005_no2)

#01s006 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "01_s006"]
df1$X2mo_visit_date[df1$recordID == "01_s006"]

df7_NO2_BOSTON_01s006 <- df7_NO2_BOSTON[df7_NO2_BOSTON$Date.Local >= "2016-02-07" & df7_NO2_BOSTON$Date.Local <= "2016-03-08", ]
df7_NO2_BOSTON_01s006

# Convert dataframe into sf object
sf_no2_BOSTON_01s006 <- st_as_sf(df7_NO2_BOSTON_01s006, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_BOSTON_01s006_V <- vect(sf_no2_BOSTON_01s006)
v <- voronoi(sf_no2_BOSTON_01s006_V)
plot(v)
points(sf_no2_BOSTON_01s006_V)

V_no2_BOSTON_01s006 <- crop(v, vect(st_union(boston_zctas)))
plot(V_no2_BOSTON_01s006, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_BOSTON_01s006, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_BOSTON_01s006, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_01s006_no2 <- data.frame(geom(sf_no2_BOSTON_01s006_V)[,c("x", "y")], as.data.frame(sf_no2_BOSTON_01s006_V))
head(d_01s006_no2)

# run gstat model
gs_01s006_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_01s006_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_01s006_no2 <- interpolate(r, gs_01s006_no2, debug.level=0)

# rasterize idw output
idw_01s006_no2.r <-raster(idw_01s006_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "01_s006"]
df1$address_LAT[df1$recordID == "01_s006"]

p_01s006_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "01_s006"], df1$address_LAT[df1$recordID == "01_s006"]), ncol =2))
proj4string(p_01s006_no2) <- projection(idw_01s006_no2.r)

# extract estimated NO2 value 
extract(idw_01s006_no2.r, p_01s006_no2)

#01s010 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "01_s010"]
df1$X2mo_visit_date[df1$recordID == "01_s010"]

df7_NO2_BOSTON_01s010 <- df7_NO2_BOSTON[df7_NO2_BOSTON$Date.Local >= "2016-04-18" & df7_NO2_BOSTON$Date.Local <= "2016-05-18", ]
df7_NO2_BOSTON_01s010

# Convert dataframe into sf object
sf_no2_BOSTON_01s010 <- st_as_sf(df7_NO2_BOSTON_01s010, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_BOSTON_01s010_V <- vect(sf_no2_BOSTON_01s010)
v <- voronoi(sf_no2_BOSTON_01s010_V)
plot(v)
points(sf_no2_BOSTON_01s010_V)

V_no2_BOSTON_01s010 <- crop(v, vect(st_union(boston_zctas)))
plot(V_no2_BOSTON_01s010, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_BOSTON_01s010, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_BOSTON_01s010, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_01s010_no2 <- data.frame(geom(sf_no2_BOSTON_01s010_V)[,c("x", "y")], as.data.frame(sf_no2_BOSTON_01s010_V))
head(d_01s010_no2)

# run gstat model
gs_01s010_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_01s010_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_01s010_no2 <- interpolate(r, gs_01s010_no2, debug.level=0)

# rasterize idw output
idw_01s010_no2.r <-raster(idw_01s010_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "01_s010"]
df1$address_LAT[df1$recordID == "01_s010"]

p_01s010_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "01_s010"], df1$address_LAT[df1$recordID == "01_s010"]), ncol =2))
proj4string(p_01s010_no2) <- projection(idw_01s010_no2.r)

# extract estimated NO2 value 
extract(idw_01s010_no2.r, p_01s010_no2)

#01s011 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "01_s011"]
df1$X2mo_visit_date[df1$recordID == "01_s011"]

df7_NO2_BOSTON_01s011 <- df7_NO2_BOSTON[df7_NO2_BOSTON$Date.Local >= "2016-04-27" & df7_NO2_BOSTON$Date.Local <= "2016-05-27", ]
df7_NO2_BOSTON_01s011

# Convert dataframe into sf object
sf_no2_BOSTON_01s011 <- st_as_sf(df7_NO2_BOSTON_01s011, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_BOSTON_01s011_V <- vect(sf_no2_BOSTON_01s011)
v <- voronoi(sf_no2_BOSTON_01s011_V)
plot(v)
points(sf_no2_BOSTON_01s011_V)

V_no2_BOSTON_01s011 <- crop(v, vect(st_union(boston_zctas)))
plot(V_no2_BOSTON_01s011, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_BOSTON_01s011, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_BOSTON_01s011, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_01s011_no2 <- data.frame(geom(sf_no2_BOSTON_01s011_V)[,c("x", "y")], as.data.frame(sf_no2_BOSTON_01s011_V))
head(d_01s011_no2)

# run gstat model
gs_01s011_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_01s011_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_01s011_no2 <- interpolate(r, gs_01s011_no2, debug.level=0)

# rasterize idw output
idw_01s011_no2.r <-raster(idw_01s011_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "01_s011"]
df1$address_LAT[df1$recordID == "01_s011"]

p_01s011_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "01_s011"], df1$address_LAT[df1$recordID == "01_s011"]), ncol =2))
proj4string(p_01s011_no2) <- projection(idw_01s011_no2.r)

# extract estimated NO2 value 
extract(idw_01s011_no2.r, p_01s011_no2)

#01s013 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "01_s013"]
df1$X2mo_visit_date[df1$recordID == "01_s013"]

df7_NO2_BOSTON_01s013 <- df7_NO2_BOSTON[df7_NO2_BOSTON$Date.Local >= "2016-06-12" & df7_NO2_BOSTON$Date.Local <= "2016-07-12", ]
df7_NO2_BOSTON_01s013

# Convert dataframe into sf object
sf_no2_BOSTON_01s013 <- st_as_sf(df7_NO2_BOSTON_01s013, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_BOSTON_01s013_V <- vect(sf_no2_BOSTON_01s013)
v <- voronoi(sf_no2_BOSTON_01s013_V)
plot(v)
points(sf_no2_BOSTON_01s013_V)

V_no2_BOSTON_01s013 <- crop(v, vect(st_union(boston_zctas)))
plot(V_no2_BOSTON_01s013, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_BOSTON_01s013, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_BOSTON_01s013, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_01s013_no2 <- data.frame(geom(sf_no2_BOSTON_01s013_V)[,c("x", "y")], as.data.frame(sf_no2_BOSTON_01s013_V))
head(d_01s013_no2)

# run gstat model
gs_01s013_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_01s013_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_01s013_no2 <- interpolate(r, gs_01s013_no2, debug.level=0)

# rasterize idw output
idw_01s013_no2.r <-raster(idw_01s013_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "01_s013"]
df1$address_LAT[df1$recordID == "01_s013"]

p_01s013_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "01_s013"], df1$address_LAT[df1$recordID == "01_s013"]), ncol =2))
proj4string(p_01s013_no2) <- projection(idw_01s013_no2.r)

# extract estimated NO2 value 
extract(idw_01s013_no2.r, p_01s013_no2)

#01s015 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "01_s015"]
df1$X2mo_visit_date[df1$recordID == "01_s015"]

df7_NO2_BOSTON_01s015 <- df7_NO2_BOSTON[df7_NO2_BOSTON$Date.Local >= "2016-06-18" & df7_NO2_BOSTON$Date.Local <= "2016-07-18", ]
df7_NO2_BOSTON_01s015

# Convert dataframe into sf object
sf_no2_BOSTON_01s015 <- st_as_sf(df7_NO2_BOSTON_01s015, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_BOSTON_01s015_V <- vect(sf_no2_BOSTON_01s015)
v <- voronoi(sf_no2_BOSTON_01s015_V)
plot(v)
points(sf_no2_BOSTON_01s015_V)

V_no2_BOSTON_01s015 <- crop(v, vect(st_union(boston_zctas)))
plot(V_no2_BOSTON_01s015, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_BOSTON_01s015, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_BOSTON_01s015, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_01s015_no2 <- data.frame(geom(sf_no2_BOSTON_01s015_V)[,c("x", "y")], as.data.frame(sf_no2_BOSTON_01s015_V))
head(d_01s015_no2)

# run gstat model
gs_01s015_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_01s015_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_01s015_no2 <- interpolate(r, gs_01s015_no2, debug.level=0)

# rasterize idw output
idw_01s015_no2.r <-raster(idw_01s015_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "01_s015"]
df1$address_LAT[df1$recordID == "01_s015"]

p_01s015_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "01_s015"], df1$address_LAT[df1$recordID == "01_s015"]), ncol =2))
proj4string(p_01s015_no2) <- projection(idw_01s015_no2.r)

# extract estimated NO2 value 
extract(idw_01s015_no2.r, p_01s015_no2)

#01s016 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "01_s016"]
df1$X2mo_visit_date[df1$recordID == "01_s016"]

df7_NO2_BOSTON_01s016 <- df7_NO2_BOSTON[df7_NO2_BOSTON$Date.Local >= "2016-07-10" & df7_NO2_BOSTON$Date.Local <= "2016-08-09", ]
df7_NO2_BOSTON_01s016

# Convert dataframe into sf object
sf_no2_BOSTON_01s016 <- st_as_sf(df7_NO2_BOSTON_01s016, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_BOSTON_01s016_V <- vect(sf_no2_BOSTON_01s016)
v <- voronoi(sf_no2_BOSTON_01s016_V)
plot(v)
points(sf_no2_BOSTON_01s016_V)

V_no2_BOSTON_01s016 <- crop(v, vect(st_union(boston_zctas)))
plot(V_no2_BOSTON_01s016, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_BOSTON_01s016, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_BOSTON_01s016, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_01s016_no2 <- data.frame(geom(sf_no2_BOSTON_01s016_V)[,c("x", "y")], as.data.frame(sf_no2_BOSTON_01s016_V))
head(d_01s016_no2)

# run gstat model
gs_01s016_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_01s016_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_01s016_no2 <- interpolate(r, gs_01s016_no2, debug.level=0)

# rasterize idw output
idw_01s016_no2.r <-raster(idw_01s016_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "01_s016"]
df1$address_LAT[df1$recordID == "01_s016"]

p_01s016_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "01_s016"], df1$address_LAT[df1$recordID == "01_s016"]), ncol =2))
proj4string(p_01s016_no2) <- projection(idw_01s016_no2.r)

# extract estimated NO2 value 
extract(idw_01s016_no2.r, p_01s016_no2)

#01s018 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "01_s018"]
df1$X2mo_visit_date[df1$recordID == "01_s018"]

df7_NO2_BOSTON_01s018 <- df7_NO2_BOSTON[df7_NO2_BOSTON$Date.Local >= "2016-07-30" & df7_NO2_BOSTON$Date.Local <= "2016-08-29", ]
df7_NO2_BOSTON_01s018

# Convert dataframe into sf object
sf_no2_BOSTON_01s018 <- st_as_sf(df7_NO2_BOSTON_01s018, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_BOSTON_01s018_V <- vect(sf_no2_BOSTON_01s018)
v <- voronoi(sf_no2_BOSTON_01s018_V)
plot(v)
points(sf_no2_BOSTON_01s018_V)

V_no2_BOSTON_01s018 <- crop(v, vect(st_union(boston_zctas)))
plot(V_no2_BOSTON_01s018, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_BOSTON_01s018, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_BOSTON_01s018, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_01s018_no2 <- data.frame(geom(sf_no2_BOSTON_01s018_V)[,c("x", "y")], as.data.frame(sf_no2_BOSTON_01s018_V))
head(d_01s018_no2)

# run gstat model
gs_01s018_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_01s018_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_01s018_no2 <- interpolate(r, gs_01s018_no2, debug.level=0)

# rasterize idw output
idw_01s018_no2.r <-raster(idw_01s018_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "01_s018"]
df1$address_LAT[df1$recordID == "01_s018"]

p_01s018_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "01_s018"], df1$address_LAT[df1$recordID == "01_s018"]), ncol =2))
proj4string(p_01s018_no2) <- projection(idw_01s018_no2.r)

# extract estimated NO2 value 
extract(idw_01s018_no2.r, p_01s018_no2)

#01s019 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "01_s019"]
df1$X2mo_visit_date[df1$recordID == "01_s019"]

df7_NO2_BOSTON_01s019 <- df7_NO2_BOSTON[df7_NO2_BOSTON$Date.Local >= "2016-08-02" & df7_NO2_BOSTON$Date.Local <= "2016-09-01", ]
df7_NO2_BOSTON_01s019

# Convert dataframe into sf object
sf_no2_BOSTON_01s019 <- st_as_sf(df7_NO2_BOSTON_01s019, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_BOSTON_01s019_V <- vect(sf_no2_BOSTON_01s019)
v <- voronoi(sf_no2_BOSTON_01s019_V)
plot(v)
points(sf_no2_BOSTON_01s019_V)

V_no2_BOSTON_01s019 <- crop(v, vect(st_union(boston_zctas)))
plot(V_no2_BOSTON_01s019, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_BOSTON_01s019, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_BOSTON_01s019, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_01s019_no2 <- data.frame(geom(sf_no2_BOSTON_01s019_V)[,c("x", "y")], as.data.frame(sf_no2_BOSTON_01s019_V))
head(d_01s019_no2)

# run gstat model
gs_01s019_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_01s019_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_01s019_no2 <- interpolate(r, gs_01s019_no2, debug.level=0)

# rasterize idw output
idw_01s019_no2.r <-raster(idw_01s019_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "01_s019"]
df1$address_LAT[df1$recordID == "01_s019"]

p_01s019_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "01_s019"], df1$address_LAT[df1$recordID == "01_s019"]), ncol =2))
proj4string(p_01s019_no2) <- projection(idw_01s019_no2.r)

# extract estimated NO2 value 
extract(idw_01s019_no2.r, p_01s019_no2)

#01s021 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "01_s021"]
df1$X2mo_visit_date[df1$recordID == "01_s021"]

df7_NO2_BOSTON_01s021 <- df7_NO2_BOSTON[df7_NO2_BOSTON$Date.Local >= "2016-08-15" & df7_NO2_BOSTON$Date.Local <= "2016-09-14", ]
df7_NO2_BOSTON_01s021

# Convert dataframe into sf object
sf_no2_BOSTON_01s021 <- st_as_sf(df7_NO2_BOSTON_01s021, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_BOSTON_01s021_V <- vect(sf_no2_BOSTON_01s021)
v <- voronoi(sf_no2_BOSTON_01s021_V)
plot(v)
points(sf_no2_BOSTON_01s021_V)

V_no2_BOSTON_01s021 <- crop(v, vect(st_union(boston_zctas)))
plot(V_no2_BOSTON_01s021, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_BOSTON_01s021, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_BOSTON_01s021, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_01s021_no2 <- data.frame(geom(sf_no2_BOSTON_01s021_V)[,c("x", "y")], as.data.frame(sf_no2_BOSTON_01s021_V))
head(d_01s021_no2)

# run gstat model
gs_01s021_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_01s021_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_01s021_no2 <- interpolate(r, gs_01s021_no2, debug.level=0)

# rasterize idw output
idw_01s021_no2.r <-raster(idw_01s021_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "01_s021"]
df1$address_LAT[df1$recordID == "01_s021"]

p_01s021_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "01_s021"], df1$address_LAT[df1$recordID == "01_s021"]), ncol =2))
proj4string(p_01s021_no2) <- projection(idw_01s021_no2.r)

# extract estimated NO2 value 
extract(idw_01s021_no2.r, p_01s021_no2)

#01s023 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "01_s023"]
df1$X2mo_visit_date[df1$recordID == "01_s023"]

df7_NO2_BOSTON_01s023 <- df7_NO2_BOSTON[df7_NO2_BOSTON$Date.Local >= "2016-09-17" & df7_NO2_BOSTON$Date.Local <= "2016-10-17", ]
df7_NO2_BOSTON_01s023

# Convert dataframe into sf object
sf_no2_BOSTON_01s023 <- st_as_sf(df7_NO2_BOSTON_01s023, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_BOSTON_01s023_V <- vect(sf_no2_BOSTON_01s023)
v <- voronoi(sf_no2_BOSTON_01s023_V)
plot(v)
points(sf_no2_BOSTON_01s023_V)

V_no2_BOSTON_01s023 <- crop(v, vect(st_union(boston_zctas)))
plot(V_no2_BOSTON_01s023, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_BOSTON_01s023, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_BOSTON_01s023, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_01s023_no2 <- data.frame(geom(sf_no2_BOSTON_01s023_V)[,c("x", "y")], as.data.frame(sf_no2_BOSTON_01s023_V))
head(d_01s023_no2)

# run gstat model
gs_01s023_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_01s023_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_01s023_no2 <- interpolate(r, gs_01s023_no2, debug.level=0)

# rasterize idw output
idw_01s023_no2.r <-raster(idw_01s023_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "01_s023"]
df1$address_LAT[df1$recordID == "01_s023"]

p_01s023_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "01_s023"], df1$address_LAT[df1$recordID == "01_s023"]), ncol =2))
proj4string(p_01s023_no2) <- projection(idw_01s023_no2.r)

# extract estimated NO2 value 
extract(idw_01s023_no2.r, p_01s023_no2)

#01s024 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "01_s024"]
df1$X2mo_visit_date[df1$recordID == "01_s024"]

df7_NO2_BOSTON_01s024 <- df7_NO2_BOSTON[df7_NO2_BOSTON$Date.Local >= "2016-09-03" & df7_NO2_BOSTON$Date.Local <= "2016-10-03", ]
df7_NO2_BOSTON_01s024

# Convert dataframe into sf object
sf_no2_BOSTON_01s024 <- st_as_sf(df7_NO2_BOSTON_01s024, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_BOSTON_01s024_V <- vect(sf_no2_BOSTON_01s024)
v <- voronoi(sf_no2_BOSTON_01s024_V)
plot(v)
points(sf_no2_BOSTON_01s024_V)

V_no2_BOSTON_01s024 <- crop(v, vect(st_union(boston_zctas)))
plot(V_no2_BOSTON_01s024, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_BOSTON_01s024, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_BOSTON_01s024, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_01s024_no2 <- data.frame(geom(sf_no2_BOSTON_01s024_V)[,c("x", "y")], as.data.frame(sf_no2_BOSTON_01s024_V))
head(d_01s024_no2)

# run gstat model
gs_01s024_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_01s024_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_01s024_no2 <- interpolate(r, gs_01s024_no2, debug.level=0)

# rasterize idw output
idw_01s024_no2.r <-raster(idw_01s024_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "01_s024"]
df1$address_LAT[df1$recordID == "01_s024"]

p_01s024_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "01_s024"], df1$address_LAT[df1$recordID == "01_s024"]), ncol =2))
proj4string(p_01s024_no2) <- projection(idw_01s024_no2.r)

# extract estimated NO2 value 
extract(idw_01s024_no2.r, p_01s024_no2)

#01s025 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "01_s025"]
df1$X2mo_visit_date[df1$recordID == "01_s025"]

df7_NO2_BOSTON_01s025 <- df7_NO2_BOSTON[df7_NO2_BOSTON$Date.Local >= "2016-09-27" & df7_NO2_BOSTON$Date.Local <= "2016-10-27", ]
df7_NO2_BOSTON_01s025

# Convert dataframe into sf object
sf_no2_BOSTON_01s025 <- st_as_sf(df7_NO2_BOSTON_01s025, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_BOSTON_01s025_V <- vect(sf_no2_BOSTON_01s025)
v <- voronoi(sf_no2_BOSTON_01s025_V)
plot(v)
points(sf_no2_BOSTON_01s025_V)

V_no2_BOSTON_01s025 <- crop(v, vect(st_union(boston_zctas)))
plot(V_no2_BOSTON_01s025, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_BOSTON_01s025, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_BOSTON_01s025, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_01s025_no2 <- data.frame(geom(sf_no2_BOSTON_01s025_V)[,c("x", "y")], as.data.frame(sf_no2_BOSTON_01s025_V))
head(d_01s025_no2)

# run gstat model
gs_01s025_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_01s025_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_01s025_no2 <- interpolate(r, gs_01s025_no2, debug.level=0)

# rasterize idw output
idw_01s025_no2.r <-raster(idw_01s025_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "01_s025"]
df1$address_LAT[df1$recordID == "01_s025"]

p_01s025_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "01_s025"], df1$address_LAT[df1$recordID == "01_s025"]), ncol =2))
proj4string(p_01s025_no2) <- projection(idw_01s025_no2.r)

# extract estimated NO2 value 
extract(idw_01s025_no2.r, p_01s025_no2)

#01s026 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "01_s026"]
df1$X2mo_visit_date[df1$recordID == "01_s026"]

df7_NO2_BOSTON_01s026 <- df7_NO2_BOSTON[df7_NO2_BOSTON$Date.Local >= "2016-10-02" & df7_NO2_BOSTON$Date.Local <= "2016-11-01", ]
df7_NO2_BOSTON_01s026

# Convert dataframe into sf object
sf_no2_BOSTON_01s026 <- st_as_sf(df7_NO2_BOSTON_01s026, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_BOSTON_01s026_V <- vect(sf_no2_BOSTON_01s026)
v <- voronoi(sf_no2_BOSTON_01s026_V)
plot(v)
points(sf_no2_BOSTON_01s026_V)

V_no2_BOSTON_01s026 <- crop(v, vect(st_union(boston_zctas)))
plot(V_no2_BOSTON_01s026, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_BOSTON_01s026, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_BOSTON_01s026, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_01s026_no2 <- data.frame(geom(sf_no2_BOSTON_01s026_V)[,c("x", "y")], as.data.frame(sf_no2_BOSTON_01s026_V))
head(d_01s026_no2)

# run gstat model
gs_01s026_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_01s026_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_01s026_no2 <- interpolate(r, gs_01s026_no2, debug.level=0)

# rasterize idw output
idw_01s026_no2.r <-raster(idw_01s026_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "01_s026"]
df1$address_LAT[df1$recordID == "01_s026"]

p_01s026_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "01_s026"], df1$address_LAT[df1$recordID == "01_s026"]), ncol =2))
proj4string(p_01s026_no2) <- projection(idw_01s026_no2.r)

# extract estimated NO2 value 
extract(idw_01s026_no2.r, p_01s026_no2)

#01s027 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "01_s027"]
df1$X2mo_visit_date[df1$recordID == "01_s027"]

df7_NO2_BOSTON_01s027 <- df7_NO2_BOSTON[df7_NO2_BOSTON$Date.Local >= "2016-10-08" & df7_NO2_BOSTON$Date.Local <= "2016-11-07", ]
df7_NO2_BOSTON_01s027

# Convert dataframe into sf object
sf_no2_BOSTON_01s027 <- st_as_sf(df7_NO2_BOSTON_01s027, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_BOSTON_01s027_V <- vect(sf_no2_BOSTON_01s027)
v <- voronoi(sf_no2_BOSTON_01s027_V)
plot(v)
points(sf_no2_BOSTON_01s027_V)

V_no2_BOSTON_01s027 <- crop(v, vect(st_union(boston_zctas)))
plot(V_no2_BOSTON_01s027, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_BOSTON_01s027, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_BOSTON_01s027, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_01s027_no2 <- data.frame(geom(sf_no2_BOSTON_01s027_V)[,c("x", "y")], as.data.frame(sf_no2_BOSTON_01s027_V))
head(d_01s027_no2)

# run gstat model
gs_01s027_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_01s027_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_01s027_no2 <- interpolate(r, gs_01s027_no2, debug.level=0)

# rasterize idw output
idw_01s027_no2.r <-raster(idw_01s027_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "01_s027"]
df1$address_LAT[df1$recordID == "01_s027"]

p_01s027_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "01_s027"], df1$address_LAT[df1$recordID == "01_s027"]), ncol =2))
proj4string(p_01s027_no2) <- projection(idw_01s027_no2.r)

# extract estimated NO2 value 
extract(idw_01s027_no2.r, p_01s027_no2)

#01s029 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "01_s029"]
df1$X2mo_visit_date[df1$recordID == "01_s029"]

df7_NO2_BOSTON_01s029 <- df7_NO2_BOSTON[df7_NO2_BOSTON$Date.Local >= "2016-10-10" & df7_NO2_BOSTON$Date.Local <= "2016-11-09", ]
df7_NO2_BOSTON_01s029

# Convert dataframe into sf object
sf_no2_BOSTON_01s029 <- st_as_sf(df7_NO2_BOSTON_01s029, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_BOSTON_01s029_V <- vect(sf_no2_BOSTON_01s029)
v <- voronoi(sf_no2_BOSTON_01s029_V)
plot(v)
points(sf_no2_BOSTON_01s029_V)

V_no2_BOSTON_01s029 <- crop(v, vect(st_union(boston_zctas)))
plot(V_no2_BOSTON_01s029, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_BOSTON_01s029, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_BOSTON_01s029, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_01s029_no2 <- data.frame(geom(sf_no2_BOSTON_01s029_V)[,c("x", "y")], as.data.frame(sf_no2_BOSTON_01s029_V))
head(d_01s029_no2)

# run gstat model
gs_01s029_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_01s029_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_01s029_no2 <- interpolate(r, gs_01s029_no2, debug.level=0)

# rasterize idw output
idw_01s029_no2.r <-raster(idw_01s029_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "01_s029"]
df1$address_LAT[df1$recordID == "01_s029"]

p_01s029_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "01_s029"], df1$address_LAT[df1$recordID == "01_s029"]), ncol =2))
proj4string(p_01s029_no2) <- projection(idw_01s029_no2.r)

# extract estimated NO2 value 
extract(idw_01s029_no2.r, p_01s029_no2)

#01s031 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "01_s031"]
df1$X2mo_visit_date[df1$recordID == "01_s031"]

df7_NO2_BOSTON_01s031 <- df7_NO2_BOSTON[df7_NO2_BOSTON$Date.Local >= "2016-10-23" & df7_NO2_BOSTON$Date.Local <= "2016-11-22", ]
df7_NO2_BOSTON_01s031

# Convert dataframe into sf object
sf_no2_BOSTON_01s031 <- st_as_sf(df7_NO2_BOSTON_01s031, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_BOSTON_01s031_V <- vect(sf_no2_BOSTON_01s031)
v <- voronoi(sf_no2_BOSTON_01s031_V)
plot(v)
points(sf_no2_BOSTON_01s031_V)

V_no2_BOSTON_01s031 <- crop(v, vect(st_union(boston_zctas)))
plot(V_no2_BOSTON_01s031, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_BOSTON_01s031, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_BOSTON_01s031, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_01s031_no2 <- data.frame(geom(sf_no2_BOSTON_01s031_V)[,c("x", "y")], as.data.frame(sf_no2_BOSTON_01s031_V))
head(d_01s031_no2)

# run gstat model
gs_01s031_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_01s031_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_01s031_no2 <- interpolate(r, gs_01s031_no2, debug.level=0)

# rasterize idw output
idw_01s031_no2.r <-raster(idw_01s031_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "01_s031"]
df1$address_LAT[df1$recordID == "01_s031"]

p_01s031_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "01_s031"], df1$address_LAT[df1$recordID == "01_s031"]), ncol =2))
proj4string(p_01s031_no2) <- projection(idw_01s031_no2.r)

# extract estimated NO2 value 
extract(idw_01s031_no2.r, p_01s031_no2)

#01s032 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "01_s032"]
df1$X2mo_visit_date[df1$recordID == "01_s032"]

df7_NO2_BOSTON_01s032 <- df7_NO2_BOSTON[df7_NO2_BOSTON$Date.Local >= "2016-10-31" & df7_NO2_BOSTON$Date.Local <= "2016-11-30", ]
df7_NO2_BOSTON_01s032

# Convert dataframe into sf object
sf_no2_BOSTON_01s032 <- st_as_sf(df7_NO2_BOSTON_01s032, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_BOSTON_01s032_V <- vect(sf_no2_BOSTON_01s032)
v <- voronoi(sf_no2_BOSTON_01s032_V)
plot(v)
points(sf_no2_BOSTON_01s032_V)

V_no2_BOSTON_01s032 <- crop(v, vect(st_union(boston_zctas)))
plot(V_no2_BOSTON_01s032, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_BOSTON_01s032, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_BOSTON_01s032, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_01s032_no2 <- data.frame(geom(sf_no2_BOSTON_01s032_V)[,c("x", "y")], as.data.frame(sf_no2_BOSTON_01s032_V))
head(d_01s032_no2)

# run gstat model
gs_01s032_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_01s032_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_01s032_no2 <- interpolate(r, gs_01s032_no2, debug.level=0)

# rasterize idw output
idw_01s032_no2.r <-raster(idw_01s032_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "01_s032"]
df1$address_LAT[df1$recordID == "01_s032"]

p_01s032_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "01_s032"], df1$address_LAT[df1$recordID == "01_s032"]), ncol =2))
proj4string(p_01s032_no2) <- projection(idw_01s032_no2.r)

# extract estimated NO2 value 
extract(idw_01s032_no2.r, p_01s032_no2)

#01s033 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "01_s033"]
df1$X2mo_visit_date[df1$recordID == "01_s033"]

df7_NO2_BOSTON_01s033 <- df7_NO2_BOSTON[df7_NO2_BOSTON$Date.Local >= "2016-11-08" & df7_NO2_BOSTON$Date.Local <= "2016-12-08", ]
df7_NO2_BOSTON_01s033

# Convert dataframe into sf object
sf_no2_BOSTON_01s033 <- st_as_sf(df7_NO2_BOSTON_01s033, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_BOSTON_01s033_V <- vect(sf_no2_BOSTON_01s033)
v <- voronoi(sf_no2_BOSTON_01s033_V)
plot(v)
points(sf_no2_BOSTON_01s033_V)

V_no2_BOSTON_01s033 <- crop(v, vect(st_union(boston_zctas)))
plot(V_no2_BOSTON_01s033, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_BOSTON_01s033, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_BOSTON_01s033, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_01s033_no2 <- data.frame(geom(sf_no2_BOSTON_01s033_V)[,c("x", "y")], as.data.frame(sf_no2_BOSTON_01s033_V))
head(d_01s033_no2)

# run gstat model
gs_01s033_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_01s033_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_01s033_no2 <- interpolate(r, gs_01s033_no2, debug.level=0)

# rasterize idw output
idw_01s033_no2.r <-raster(idw_01s033_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "01_s033"]
df1$address_LAT[df1$recordID == "01_s033"]

p_01s033_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "01_s033"], df1$address_LAT[df1$recordID == "01_s033"]), ncol =2))
proj4string(p_01s033_no2) <- projection(idw_01s033_no2.r)

# extract estimated NO2 value 
extract(idw_01s033_no2.r, p_01s033_no2)

#01s035 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "01_s035"]
df1$X2mo_visit_date[df1$recordID == "01_s035"]

df7_NO2_BOSTON_01s035 <- df7_NO2_BOSTON[df7_NO2_BOSTON$Date.Local >= "2016-12-04" & df7_NO2_BOSTON$Date.Local <= "2017-01-03", ]
df7_NO2_BOSTON_01s035

# Convert dataframe into sf object
sf_no2_BOSTON_01s035 <- st_as_sf(df7_NO2_BOSTON_01s035, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_BOSTON_01s035_V <- vect(sf_no2_BOSTON_01s035)
v <- voronoi(sf_no2_BOSTON_01s035_V)
plot(v)
points(sf_no2_BOSTON_01s035_V)

V_no2_BOSTON_01s035 <- crop(v, vect(st_union(boston_zctas)))
plot(V_no2_BOSTON_01s035, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_BOSTON_01s035, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_BOSTON_01s035, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_01s035_no2 <- data.frame(geom(sf_no2_BOSTON_01s035_V)[,c("x", "y")], as.data.frame(sf_no2_BOSTON_01s035_V))
head(d_01s035_no2)

# run gstat model
gs_01s035_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_01s035_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_01s035_no2 <- interpolate(r, gs_01s035_no2, debug.level=0)

# rasterize idw output
idw_01s035_no2.r <-raster(idw_01s035_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "01_s035"]
df1$address_LAT[df1$recordID == "01_s035"]

p_01s035_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "01_s035"], df1$address_LAT[df1$recordID == "01_s035"]), ncol =2))
proj4string(p_01s035_no2) <- projection(idw_01s035_no2.r)

# extract estimated NO2 value 
extract(idw_01s035_no2.r, p_01s035_no2)

#01s036 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "01_s036"]
df1$X2mo_visit_date[df1$recordID == "01_s036"]

df7_NO2_BOSTON_01s036 <- df7_NO2_BOSTON[df7_NO2_BOSTON$Date.Local >= "2016-12-06" & df7_NO2_BOSTON$Date.Local <= "2017-01-05", ]
df7_NO2_BOSTON_01s036

# Convert dataframe into sf object
sf_no2_BOSTON_01s036 <- st_as_sf(df7_NO2_BOSTON_01s036, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_BOSTON_01s036_V <- vect(sf_no2_BOSTON_01s036)
v <- voronoi(sf_no2_BOSTON_01s036_V)
plot(v)
points(sf_no2_BOSTON_01s036_V)

V_no2_BOSTON_01s036 <- crop(v, vect(st_union(boston_zctas)))
plot(V_no2_BOSTON_01s036, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_BOSTON_01s036, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_BOSTON_01s036, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_01s036_no2 <- data.frame(geom(sf_no2_BOSTON_01s036_V)[,c("x", "y")], as.data.frame(sf_no2_BOSTON_01s036_V))
head(d_01s036_no2)

# run gstat model
gs_01s036_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_01s036_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_01s036_no2 <- interpolate(r, gs_01s036_no2, debug.level=0)

# rasterize idw output
idw_01s036_no2.r <-raster(idw_01s036_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "01_s036"]
df1$address_LAT[df1$recordID == "01_s036"]

p_01s036_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "01_s036"], df1$address_LAT[df1$recordID == "01_s036"]), ncol =2))
proj4string(p_01s036_no2) <- projection(idw_01s036_no2.r)

# extract estimated NO2 value 
extract(idw_01s036_no2.r, p_01s036_no2)

#01s038 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "01_s038"]
df1$X2mo_visit_date[df1$recordID == "01_s038"]

df7_NO2_BOSTON_01s038 <- df7_NO2_BOSTON[df7_NO2_BOSTON$Date.Local >= "2016-12-18" & df7_NO2_BOSTON$Date.Local <= "2017-01-17", ]
df7_NO2_BOSTON_01s038

# Convert dataframe into sf object
sf_no2_BOSTON_01s038 <- st_as_sf(df7_NO2_BOSTON_01s038, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_BOSTON_01s038_V <- vect(sf_no2_BOSTON_01s038)
v <- voronoi(sf_no2_BOSTON_01s038_V)
plot(v)
points(sf_no2_BOSTON_01s038_V)

V_no2_BOSTON_01s038 <- crop(v, vect(st_union(boston_zctas)))
plot(V_no2_BOSTON_01s038, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_BOSTON_01s038, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_BOSTON_01s038, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_01s038_no2 <- data.frame(geom(sf_no2_BOSTON_01s038_V)[,c("x", "y")], as.data.frame(sf_no2_BOSTON_01s038_V))
head(d_01s038_no2)

# run gstat model
gs_01s038_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_01s038_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_01s038_no2 <- interpolate(r, gs_01s038_no2, debug.level=0)

# rasterize idw output
idw_01s038_no2.r <-raster(idw_01s038_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "01_s038"]
df1$address_LAT[df1$recordID == "01_s038"]

p_01s038_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "01_s038"], df1$address_LAT[df1$recordID == "01_s038"]), ncol =2))
proj4string(p_01s038_no2) <- projection(idw_01s038_no2.r)

# extract estimated NO2 value 
extract(idw_01s038_no2.r, p_01s038_no2)

#01s040 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "01_s040"]
df1$X2mo_visit_date[df1$recordID == "01_s040"]

df7_NO2_BOSTON_01s040 <- df7_NO2_BOSTON[df7_NO2_BOSTON$Date.Local >= "2016-12-25" & df7_NO2_BOSTON$Date.Local <= "2017-01-24", ]
df7_NO2_BOSTON_01s040

# Convert dataframe into sf object
sf_no2_BOSTON_01s040 <- st_as_sf(df7_NO2_BOSTON_01s040, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_BOSTON_01s040_V <- vect(sf_no2_BOSTON_01s040)
v <- voronoi(sf_no2_BOSTON_01s040_V)
plot(v)
points(sf_no2_BOSTON_01s040_V)

V_no2_BOSTON_01s040 <- crop(v, vect(st_union(boston_zctas)))
plot(V_no2_BOSTON_01s040, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_BOSTON_01s040, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_BOSTON_01s040, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_01s040_no2 <- data.frame(geom(sf_no2_BOSTON_01s040_V)[,c("x", "y")], as.data.frame(sf_no2_BOSTON_01s040_V))
head(d_01s040_no2)

# run gstat model
gs_01s040_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_01s040_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_01s040_no2 <- interpolate(r, gs_01s040_no2, debug.level=0)

# rasterize idw output
idw_01s040_no2.r <-raster(idw_01s040_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "01_s040"]
df1$address_LAT[df1$recordID == "01_s040"]

p_01s040_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "01_s040"], df1$address_LAT[df1$recordID == "01_s040"]), ncol =2))
proj4string(p_01s040_no2) <- projection(idw_01s040_no2.r)

# extract estimated NO2 value 
extract(idw_01s040_no2.r, p_01s040_no2)

#01s041 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "01_s041"]
df1$X2mo_visit_date[df1$recordID == "01_s041"]

df7_NO2_BOSTON_01s041 <- df7_NO2_BOSTON[df7_NO2_BOSTON$Date.Local >= "2016-12-26" & df7_NO2_BOSTON$Date.Local <= "2017-01-25", ]
df7_NO2_BOSTON_01s041

# Convert dataframe into sf object
sf_no2_BOSTON_01s041 <- st_as_sf(df7_NO2_BOSTON_01s041, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_BOSTON_01s041_V <- vect(sf_no2_BOSTON_01s041)
v <- voronoi(sf_no2_BOSTON_01s041_V)
plot(v)
points(sf_no2_BOSTON_01s041_V)

V_no2_BOSTON_01s041 <- crop(v, vect(st_union(boston_zctas)))
plot(V_no2_BOSTON_01s041, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_BOSTON_01s041, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_BOSTON_01s041, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_01s041_no2 <- data.frame(geom(sf_no2_BOSTON_01s041_V)[,c("x", "y")], as.data.frame(sf_no2_BOSTON_01s041_V))
head(d_01s041_no2)

# run gstat model
gs_01s041_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_01s041_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_01s041_no2 <- interpolate(r, gs_01s041_no2, debug.level=0)

# rasterize idw output
idw_01s041_no2.r <-raster(idw_01s041_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "01_s041"]
df1$address_LAT[df1$recordID == "01_s041"]

p_01s041_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "01_s041"], df1$address_LAT[df1$recordID == "01_s041"]), ncol =2))
proj4string(p_01s041_no2) <- projection(idw_01s041_no2.r)

# extract estimated NO2 value 
extract(idw_01s041_no2.r, p_01s041_no2)

#01s043 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "01_s043"]
df1$X2mo_visit_date[df1$recordID == "01_s043"]

df7_NO2_BOSTON_01s043 <- df7_NO2_BOSTON[df7_NO2_BOSTON$Date.Local >= "2017-01-14" & df7_NO2_BOSTON$Date.Local <= "2017-02-13", ]
df7_NO2_BOSTON_01s043

# Convert dataframe into sf object
sf_no2_BOSTON_01s043 <- st_as_sf(df7_NO2_BOSTON_01s043, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_BOSTON_01s043_V <- vect(sf_no2_BOSTON_01s043)
v <- voronoi(sf_no2_BOSTON_01s043_V)
plot(v)
points(sf_no2_BOSTON_01s043_V)

V_no2_BOSTON_01s043 <- crop(v, vect(st_union(boston_zctas)))
plot(V_no2_BOSTON_01s043, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_BOSTON_01s043, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_BOSTON_01s043, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_01s043_no2 <- data.frame(geom(sf_no2_BOSTON_01s043_V)[,c("x", "y")], as.data.frame(sf_no2_BOSTON_01s043_V))
head(d_01s043_no2)

# run gstat model
gs_01s043_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_01s043_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_01s043_no2 <- interpolate(r, gs_01s043_no2, debug.level=0)

# rasterize idw output
idw_01s043_no2.r <-raster(idw_01s043_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "01_s043"]
df1$address_LAT[df1$recordID == "01_s043"]

p_01s043_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "01_s043"], df1$address_LAT[df1$recordID == "01_s043"]), ncol =2))
proj4string(p_01s043_no2) <- projection(idw_01s043_no2.r)

# extract estimated NO2 value 
extract(idw_01s043_no2.r, p_01s043_no2)

#01s044 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "01_s044"]
df1$X2mo_visit_date[df1$recordID == "01_s044"]

df7_NO2_BOSTON_01s044 <- df7_NO2_BOSTON[df7_NO2_BOSTON$Date.Local >= "2017-01-22" & df7_NO2_BOSTON$Date.Local <= "2017-02-21", ]
df7_NO2_BOSTON_01s044

# Convert dataframe into sf object
sf_no2_BOSTON_01s044 <- st_as_sf(df7_NO2_BOSTON_01s044, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_BOSTON_01s044_V <- vect(sf_no2_BOSTON_01s044)
v <- voronoi(sf_no2_BOSTON_01s044_V)
plot(v)
points(sf_no2_BOSTON_01s044_V)

V_no2_BOSTON_01s044 <- crop(v, vect(st_union(boston_zctas)))
plot(V_no2_BOSTON_01s044, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_BOSTON_01s044, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_BOSTON_01s044, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_01s044_no2 <- data.frame(geom(sf_no2_BOSTON_01s044_V)[,c("x", "y")], as.data.frame(sf_no2_BOSTON_01s044_V))
head(d_01s044_no2)

# run gstat model
gs_01s044_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_01s044_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_01s044_no2 <- interpolate(r, gs_01s044_no2, debug.level=0)

# rasterize idw output
idw_01s044_no2.r <-raster(idw_01s044_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "01_s044"]
df1$address_LAT[df1$recordID == "01_s044"]

p_01s044_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "01_s044"], df1$address_LAT[df1$recordID == "01_s044"]), ncol =2))
proj4string(p_01s044_no2) <- projection(idw_01s044_no2.r)

# extract estimated NO2 value 
extract(idw_01s044_no2.r, p_01s044_no2)

#01s045 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "01_s045"]
df1$X2mo_visit_date[df1$recordID == "01_s045"]

df7_NO2_BOSTON_01s045 <- df7_NO2_BOSTON[df7_NO2_BOSTON$Date.Local >= "2017-02-05" & df7_NO2_BOSTON$Date.Local <= "2017-03-07", ]
df7_NO2_BOSTON_01s045

# Convert dataframe into sf object
sf_no2_BOSTON_01s045 <- st_as_sf(df7_NO2_BOSTON_01s045, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_BOSTON_01s045_V <- vect(sf_no2_BOSTON_01s045)
v <- voronoi(sf_no2_BOSTON_01s045_V)
plot(v)
points(sf_no2_BOSTON_01s045_V)

V_no2_BOSTON_01s045 <- crop(v, vect(st_union(boston_zctas)))
plot(V_no2_BOSTON_01s045, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_BOSTON_01s045, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_BOSTON_01s045, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_01s045_no2 <- data.frame(geom(sf_no2_BOSTON_01s045_V)[,c("x", "y")], as.data.frame(sf_no2_BOSTON_01s045_V))
head(d_01s045_no2)

# run gstat model
gs_01s045_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_01s045_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_01s045_no2 <- interpolate(r, gs_01s045_no2, debug.level=0)

# rasterize idw output
idw_01s045_no2.r <-raster(idw_01s045_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "01_s045"]
df1$address_LAT[df1$recordID == "01_s045"]

p_01s045_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "01_s045"], df1$address_LAT[df1$recordID == "01_s045"]), ncol =2))
proj4string(p_01s045_no2) <- projection(idw_01s045_no2.r)

# extract estimated NO2 value 
extract(idw_01s045_no2.r, p_01s045_no2)

#01s046 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "01_s046"]
df1$X2mo_visit_date[df1$recordID == "01_s046"]

df7_NO2_BOSTON_01s046 <- df7_NO2_BOSTON[df7_NO2_BOSTON$Date.Local >= "2017-02-05" & df7_NO2_BOSTON$Date.Local <= "2017-03-07", ]
df7_NO2_BOSTON_01s046

# Convert dataframe into sf object
sf_no2_BOSTON_01s046 <- st_as_sf(df7_NO2_BOSTON_01s046, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_BOSTON_01s046_V <- vect(sf_no2_BOSTON_01s046)
v <- voronoi(sf_no2_BOSTON_01s046_V)
plot(v)
points(sf_no2_BOSTON_01s046_V)

V_no2_BOSTON_01s046 <- crop(v, vect(st_union(boston_zctas)))
plot(V_no2_BOSTON_01s046, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_BOSTON_01s046, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_BOSTON_01s046, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_01s046_no2 <- data.frame(geom(sf_no2_BOSTON_01s046_V)[,c("x", "y")], as.data.frame(sf_no2_BOSTON_01s046_V))
head(d_01s046_no2)

# run gstat model
gs_01s046_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_01s046_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_01s046_no2 <- interpolate(r, gs_01s046_no2, debug.level=0)

# rasterize idw output
idw_01s046_no2.r <-raster(idw_01s046_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "01_s046"]
df1$address_LAT[df1$recordID == "01_s046"]

p_01s046_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "01_s046"], df1$address_LAT[df1$recordID == "01_s046"]), ncol =2))
proj4string(p_01s046_no2) <- projection(idw_01s046_no2.r)

# extract estimated NO2 value 
extract(idw_01s046_no2.r, p_01s046_no2)

#01s047 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "01_s047"]
df1$X2mo_visit_date[df1$recordID == "01_s047"]

df7_NO2_BOSTON_01s047 <- df7_NO2_BOSTON[df7_NO2_BOSTON$Date.Local >= "2017-02-11" & df7_NO2_BOSTON$Date.Local <= "2017-03-13", ]
df7_NO2_BOSTON_01s047

# Convert dataframe into sf object
sf_no2_BOSTON_01s047 <- st_as_sf(df7_NO2_BOSTON_01s047, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_BOSTON_01s047_V <- vect(sf_no2_BOSTON_01s047)
v <- voronoi(sf_no2_BOSTON_01s047_V)
plot(v)
points(sf_no2_BOSTON_01s047_V)

V_no2_BOSTON_01s047 <- crop(v, vect(st_union(boston_zctas)))
plot(V_no2_BOSTON_01s047, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_BOSTON_01s047, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_BOSTON_01s047, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_01s047_no2 <- data.frame(geom(sf_no2_BOSTON_01s047_V)[,c("x", "y")], as.data.frame(sf_no2_BOSTON_01s047_V))
head(d_01s047_no2)

# run gstat model
gs_01s047_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_01s047_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_01s047_no2 <- interpolate(r, gs_01s047_no2, debug.level=0)

# rasterize idw output
idw_01s047_no2.r <-raster(idw_01s047_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "01_s047"]
df1$address_LAT[df1$recordID == "01_s047"]

p_01s047_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "01_s047"], df1$address_LAT[df1$recordID == "01_s047"]), ncol =2))
proj4string(p_01s047_no2) <- projection(idw_01s047_no2.r)

# extract estimated NO2 value 
extract(idw_01s047_no2.r, p_01s047_no2)

#01s048 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "01_s048"]
df1$X2mo_visit_date[df1$recordID == "01_s048"]

df7_NO2_BOSTON_01s048 <- df7_NO2_BOSTON[df7_NO2_BOSTON$Date.Local >= "2017-02-19" & df7_NO2_BOSTON$Date.Local <= "2017-03-21", ]
df7_NO2_BOSTON_01s048

# Convert dataframe into sf object
sf_no2_BOSTON_01s048 <- st_as_sf(df7_NO2_BOSTON_01s048, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_BOSTON_01s048_V <- vect(sf_no2_BOSTON_01s048)
v <- voronoi(sf_no2_BOSTON_01s048_V)
plot(v)
points(sf_no2_BOSTON_01s048_V)

V_no2_BOSTON_01s048 <- crop(v, vect(st_union(boston_zctas)))
plot(V_no2_BOSTON_01s048, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_BOSTON_01s048, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_BOSTON_01s048, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_01s048_no2 <- data.frame(geom(sf_no2_BOSTON_01s048_V)[,c("x", "y")], as.data.frame(sf_no2_BOSTON_01s048_V))
head(d_01s048_no2)

# run gstat model
gs_01s048_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_01s048_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_01s048_no2 <- interpolate(r, gs_01s048_no2, debug.level=0)

# rasterize idw output
idw_01s048_no2.r <-raster(idw_01s048_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "01_s048"]
df1$address_LAT[df1$recordID == "01_s048"]

p_01s048_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "01_s048"], df1$address_LAT[df1$recordID == "01_s048"]), ncol =2))
proj4string(p_01s048_no2) <- projection(idw_01s048_no2.r)

# extract estimated NO2 value 
extract(idw_01s048_no2.r, p_01s048_no2)

#01s049 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "01_s049"]
df1$X2mo_visit_date[df1$recordID == "01_s049"]

df7_NO2_BOSTON_01s049 <- df7_NO2_BOSTON[df7_NO2_BOSTON$Date.Local >= "2017-02-21" & df7_NO2_BOSTON$Date.Local <= "2017-03-23", ]
df7_NO2_BOSTON_01s049

# Convert dataframe into sf object
sf_no2_BOSTON_01s049 <- st_as_sf(df7_NO2_BOSTON_01s049, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_BOSTON_01s049_V <- vect(sf_no2_BOSTON_01s049)
v <- voronoi(sf_no2_BOSTON_01s049_V)
plot(v)
points(sf_no2_BOSTON_01s049_V)

V_no2_BOSTON_01s049 <- crop(v, vect(st_union(boston_zctas)))
plot(V_no2_BOSTON_01s049, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_BOSTON_01s049, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_BOSTON_01s049, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_01s049_no2 <- data.frame(geom(sf_no2_BOSTON_01s049_V)[,c("x", "y")], as.data.frame(sf_no2_BOSTON_01s049_V))
head(d_01s049_no2)

# run gstat model
gs_01s049_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_01s049_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_01s049_no2 <- interpolate(r, gs_01s049_no2, debug.level=0)

# rasterize idw output
idw_01s049_no2.r <-raster(idw_01s049_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "01_s049"]
df1$address_LAT[df1$recordID == "01_s049"]

p_01s049_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "01_s049"], df1$address_LAT[df1$recordID == "01_s049"]), ncol =2))
proj4string(p_01s049_no2) <- projection(idw_01s049_no2.r)

# extract estimated NO2 value 
extract(idw_01s049_no2.r, p_01s049_no2)

#01s050 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "01_s050"]
df1$X2mo_visit_date[df1$recordID == "01_s050"]

df7_NO2_BOSTON_01s050 <- df7_NO2_BOSTON[df7_NO2_BOSTON$Date.Local >= "2017-02-22" & df7_NO2_BOSTON$Date.Local <= "2017-03-24", ]
df7_NO2_BOSTON_01s050

# Convert dataframe into sf object
sf_no2_BOSTON_01s050 <- st_as_sf(df7_NO2_BOSTON_01s050, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_BOSTON_01s050_V <- vect(sf_no2_BOSTON_01s050)
v <- voronoi(sf_no2_BOSTON_01s050_V)
plot(v)
points(sf_no2_BOSTON_01s050_V)

V_no2_BOSTON_01s050 <- crop(v, vect(st_union(boston_zctas)))
plot(V_no2_BOSTON_01s050, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_BOSTON_01s050, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_BOSTON_01s050, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_01s050_no2 <- data.frame(geom(sf_no2_BOSTON_01s050_V)[,c("x", "y")], as.data.frame(sf_no2_BOSTON_01s050_V))
head(d_01s050_no2)

# run gstat model
gs_01s050_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_01s050_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_01s050_no2 <- interpolate(r, gs_01s050_no2, debug.level=0)

# rasterize idw output
idw_01s050_no2.r <-raster(idw_01s050_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "01_s050"]
df1$address_LAT[df1$recordID == "01_s050"]

p_01s050_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "01_s050"], df1$address_LAT[df1$recordID == "01_s050"]), ncol =2))
proj4string(p_01s050_no2) <- projection(idw_01s050_no2.r)

# extract estimated NO2 value 
extract(idw_01s050_no2.r, p_01s050_no2)

#01s053 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "01_s053"]
df1$X2mo_visit_date[df1$recordID == "01_s053"]

df7_NO2_BOSTON_01s053 <- df7_NO2_BOSTON[df7_NO2_BOSTON$Date.Local >= "2017-03-20" & df7_NO2_BOSTON$Date.Local <= "2017-04-19", ]
df7_NO2_BOSTON_01s053

# Convert dataframe into sf object
sf_no2_BOSTON_01s053 <- st_as_sf(df7_NO2_BOSTON_01s053, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_BOSTON_01s053_V <- vect(sf_no2_BOSTON_01s053)
v <- voronoi(sf_no2_BOSTON_01s053_V)
plot(v)
points(sf_no2_BOSTON_01s053_V)

V_no2_BOSTON_01s053 <- crop(v, vect(st_union(boston_zctas)))
plot(V_no2_BOSTON_01s053, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_BOSTON_01s053, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_BOSTON_01s053, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_01s053_no2 <- data.frame(geom(sf_no2_BOSTON_01s053_V)[,c("x", "y")], as.data.frame(sf_no2_BOSTON_01s053_V))
head(d_01s053_no2)

# run gstat model
gs_01s053_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_01s053_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_01s053_no2 <- interpolate(r, gs_01s053_no2, debug.level=0)

# rasterize idw output
idw_01s053_no2.r <-raster(idw_01s053_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "01_s053"]
df1$address_LAT[df1$recordID == "01_s053"]

p_01s053_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "01_s053"], df1$address_LAT[df1$recordID == "01_s053"]), ncol =2))
proj4string(p_01s053_no2) <- projection(idw_01s053_no2.r)

# extract estimated NO2 value 
extract(idw_01s053_no2.r, p_01s053_no2)

#01s054 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "01_s054"]
df1$X2mo_visit_date[df1$recordID == "01_s054"]

df7_NO2_BOSTON_01s054 <- df7_NO2_BOSTON[df7_NO2_BOSTON$Date.Local >= "2017-03-21" & df7_NO2_BOSTON$Date.Local <= "2017-04-20", ]
df7_NO2_BOSTON_01s054

# Convert dataframe into sf object
sf_no2_BOSTON_01s054 <- st_as_sf(df7_NO2_BOSTON_01s054, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_BOSTON_01s054_V <- vect(sf_no2_BOSTON_01s054)
v <- voronoi(sf_no2_BOSTON_01s054_V)
plot(v)
points(sf_no2_BOSTON_01s054_V)

V_no2_BOSTON_01s054 <- crop(v, vect(st_union(boston_zctas)))
plot(V_no2_BOSTON_01s054, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_BOSTON_01s054, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_BOSTON_01s054, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_01s054_no2 <- data.frame(geom(sf_no2_BOSTON_01s054_V)[,c("x", "y")], as.data.frame(sf_no2_BOSTON_01s054_V))
head(d_01s054_no2)

# run gstat model
gs_01s054_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_01s054_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_01s054_no2 <- interpolate(r, gs_01s054_no2, debug.level=0)

# rasterize idw output
idw_01s054_no2.r <-raster(idw_01s054_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "01_s054"]
df1$address_LAT[df1$recordID == "01_s054"]

p_01s054_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "01_s054"], df1$address_LAT[df1$recordID == "01_s054"]), ncol =2))
proj4string(p_01s054_no2) <- projection(idw_01s054_no2.r)

# extract estimated NO2 value 
extract(idw_01s054_no2.r, p_01s054_no2)

#01s055 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "01_s055"]
df1$X2mo_visit_date[df1$recordID == "01_s055"]

df7_NO2_BOSTON_01s055 <- df7_NO2_BOSTON[df7_NO2_BOSTON$Date.Local >= "2017-03-25" & df7_NO2_BOSTON$Date.Local <= "2017-04-24", ]
df7_NO2_BOSTON_01s055

# Convert dataframe into sf object
sf_no2_BOSTON_01s055 <- st_as_sf(df7_NO2_BOSTON_01s055, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_BOSTON_01s055_V <- vect(sf_no2_BOSTON_01s055)
v <- voronoi(sf_no2_BOSTON_01s055_V)
plot(v)
points(sf_no2_BOSTON_01s055_V)

V_no2_BOSTON_01s055 <- crop(v, vect(st_union(boston_zctas)))
plot(V_no2_BOSTON_01s055, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_BOSTON_01s055, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_BOSTON_01s055, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_01s055_no2 <- data.frame(geom(sf_no2_BOSTON_01s055_V)[,c("x", "y")], as.data.frame(sf_no2_BOSTON_01s055_V))
head(d_01s055_no2)

# run gstat model
gs_01s055_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_01s055_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_01s055_no2 <- interpolate(r, gs_01s055_no2, debug.level=0)

# rasterize idw output
idw_01s055_no2.r <-raster(idw_01s055_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "01_s055"]
df1$address_LAT[df1$recordID == "01_s055"]

p_01s055_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "01_s055"], df1$address_LAT[df1$recordID == "01_s055"]), ncol =2))
proj4string(p_01s055_no2) <- projection(idw_01s055_no2.r)

# extract estimated NO2 value 
extract(idw_01s055_no2.r, p_01s055_no2)

#01s056 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "01_s056"]
df1$X2mo_visit_date[df1$recordID == "01_s056"]

df7_NO2_BOSTON_01s056 <- df7_NO2_BOSTON[df7_NO2_BOSTON$Date.Local >= "2017-03-28" & df7_NO2_BOSTON$Date.Local <= "2017-04-27", ]
df7_NO2_BOSTON_01s056

# Convert dataframe into sf object
sf_no2_BOSTON_01s056 <- st_as_sf(df7_NO2_BOSTON_01s056, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_BOSTON_01s056_V <- vect(sf_no2_BOSTON_01s056)
v <- voronoi(sf_no2_BOSTON_01s056_V)
plot(v)
points(sf_no2_BOSTON_01s056_V)

V_no2_BOSTON_01s056 <- crop(v, vect(st_union(boston_zctas)))
plot(V_no2_BOSTON_01s056, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_BOSTON_01s056, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_BOSTON_01s056, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_01s056_no2 <- data.frame(geom(sf_no2_BOSTON_01s056_V)[,c("x", "y")], as.data.frame(sf_no2_BOSTON_01s056_V))
head(d_01s056_no2)

# run gstat model
gs_01s056_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_01s056_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_01s056_no2 <- interpolate(r, gs_01s056_no2, debug.level=0)

# rasterize idw output
idw_01s056_no2.r <-raster(idw_01s056_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "01_s056"]
df1$address_LAT[df1$recordID == "01_s056"]

p_01s056_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "01_s056"], df1$address_LAT[df1$recordID == "01_s056"]), ncol =2))
proj4string(p_01s056_no2) <- projection(idw_01s056_no2.r)

# extract estimated NO2 value 
extract(idw_01s056_no2.r, p_01s056_no2)

#01s057 NO2 Interpolation 

# identify window for pollution exposure for participant and subset dataframe
df1$X2mo_beginning_pollution_window[df1$recordID == "01_s057"]
df1$X2mo_visit_date[df1$recordID == "01_s057"]

df7_NO2_BOSTON_01s057 <- df7_NO2_BOSTON[df7_NO2_BOSTON$Date.Local >= "2017-04-02" & df7_NO2_BOSTON$Date.Local <= "2017-05-02", ]
df7_NO2_BOSTON_01s057

# Convert dataframe into sf object
sf_no2_BOSTON_01s057 <- st_as_sf(df7_NO2_BOSTON_01s057, coords = c('Longitude', 'Latitude'), crs = 4326)

# proximity (Voronoi/Thiessen) polygons
sf_no2_BOSTON_01s057_V <- vect(sf_no2_BOSTON_01s057)
v <- voronoi(sf_no2_BOSTON_01s057_V)
plot(v)
points(sf_no2_BOSTON_01s057_V)

V_no2_BOSTON_01s057 <- crop(v, vect(st_union(boston_zctas)))
plot(V_no2_BOSTON_01s057, "Arithmetic.Mean")

# rasterize
r <- rast(V_no2_BOSTON_01s057, res = 0.001)  # Builds a blank raster of given dimensions and resolution  
vr <- rasterize(V_no2_BOSTON_01s057, r, "Arithmetic.Mean")
plot(vr)

# Create dataframe 
d_01s057_no2 <- data.frame(geom(sf_no2_BOSTON_01s057_V)[,c("x", "y")], as.data.frame(sf_no2_BOSTON_01s057_V))
head(d_01s057_no2)

# run gstat model
gs_01s057_no2 <- gstat(formula=Arithmetic.Mean~1, locations=~x+y, data=d_01s057_no2, nmax=Inf, set=list(idp=2))

# run idw
idw_01s057_no2 <- interpolate(r, gs_01s057_no2, debug.level=0)

# rasterize idw output
idw_01s057_no2.r <-raster(idw_01s057_no2)

# specify prediction point
df1$address_LONG[df1$recordID == "01_s057"]
df1$address_LAT[df1$recordID == "01_s057"]

p_01s057_no2 <- SpatialPoints(matrix(c(df1$address_LONG[df1$recordID == "01_s057"], df1$address_LAT[df1$recordID == "01_s057"]), ncol =2))
proj4string(p_01s057_no2) <- projection(idw_01s057_no2.r)

# extract estimated NO2 value 
extract(idw_01s057_no2.r, p_01s057_no2)
