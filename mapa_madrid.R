#primero importamos el csv
dg = read.csv('avisa-avisos ciudadanos mensuales recibidos en el ayuntamiento 201704.csv', sep = ';')
#vemos como hay columnas no aportan valor muchs NA por lo tanto 
#vamos a realizar un subset del data.table
dg$TIPO_INCIDENCIA_ID<-NULL
dg$TIPO_INCIDENCIA<-NULL
dg$CANAL_DE_ENTRADA_ID<-NULL
dg$CANAL_DE_ENTRADA<-NULL
dg$SECCION_ID<-NULL
dg$TIPO_DE_VIAL_ID<-NULL
dg$BARRIO_ID<-NULL
dg$ANOMALIA_ID<-NULL
dg$CALIFICADOR<-NULL
dg$COORDENADA_REAL_X<-NULL
dg$COORDENADA_REAL_Y<-NULL
dg$NUMERO<-NULL
#vemos que en el codigo postal existe valores que no se corresponden con 
#el numero indicado asi que se elimina las filas.

#1 pasasmos el valor -1 que aparece a NA y de ahi lo eliminamos

dg$CODIGO_POSTAL[dg$CODIGO_POSTAL < 1] <- NA
#vemos cuantas filas con NA

col.con.NA  <-apply(dg , 1 ,function(x){any(is.na(x))})
#sumanos todos los casos con na
sum(col.con.NA)
#los eliminamos de la lista las filas enteras

dg<- dg[!col.con.NA,]
rm <-elimiminar.na
rm(rm)
rm(elimiminar.na)
rm(col.con.NA)
#vamos hacer lo divertido 
#para ello primero hacemos un subst solo con las coordenas 
corrdenadas <- data.frame(dg$COORDENADA_OFICIAL_X,dg$COORDENADA_OFICIAL_Y)
#una vez que tengamos los datos de las coordenadas entonces lo que haremos sera un atrasformacion de 
#ED50 A WGS84 para ello haremos:

library(rgdal)
GPS.Points=data.frame(Longitude=c(443956.740))#prueba variables sueltas
GPS.Points=cbind(GPS.Points,Latitude=c(4476340.105))

n_chunks=1#dividir los datos en varios bloques
n.points=dim(GPS.Points)[1]
breaks=seq(1,n.points, by=round(n.points/n_chunks))
breaks=c(breaks, n.points) #aqui lo que consigo es que el ultimo punto se meta tambien
ED50<-CRS(paste("+proj=utm +zone=30 +ellps=intl +towgs84=-87,-98,-121,0,0,0,0 +pm=madrid +units=m +no_defs"))
#+proj=lcc +lat_1=40 +lat_0=40 +lon_0=0 +k_0=0.9988085293 +x_0=600000 +y_0=600000 +a=6378298.3 +b=6356657.142669561 +pm=madrid +units=m +no_defs
#+proj=utm +zone=30 +ellps=intl +towgs84=-87,-98,-121,0,0,0,0 +units=m +no_defs
i=1
for (i in 1:(length(breaks)-1)){
  cat('\n','converting points', breaks[i], "to", breaks[i+1])  
  temp.GPS.Points=GPS.Points[breaks[i]:breaks[i+1],]
  # ED50 <- CRS("+init=epsg:4230 +towgs84=-87,-96,-120,0,0,0,0")
  #ED50 <- CRS(paste("+proj=utm +zone=30 +ellps=intl +units=m +no_defs"))
  temp.GPS.Points.Spatial.Data <- SpatialPoints(temp.GPS.Points, 
                                                proj4string = ED50)
  #temp.GPS.Points.UTM.Spatial.Data <- spTransform(temp.GPS.Points.Spatial.Data,
  #                                                CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  temp.GPS.Points.UTM.Spatial.Data<- spTransform(temp.GPS.Points.Spatial.Data, CRS("+proj=longlat +datum=WGS84 +pm=madrid"))
}
DF <- as.data.frame(temp.GPS.Points.UTM.Spatial.Data)#hacemos la conversion del spatial.data a un dataframe.
#+proj=latlong +datum=WGS84 +to +proj=latlong +datum=WGS84 +pm=madrid
#una vez que tengamos todo los datos con la corversion toca hacer un plot con la librearia leaflet
library(leaflet)
leaflet() %>%
  addMarkers(data = DF) %>%
  addTiles()


