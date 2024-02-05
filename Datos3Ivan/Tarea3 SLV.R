### Tarea 2 ###
### Sandra - Ivan ###

### El presente ejercicio difiere un poco del presentado originalmente en virtud de que la base de investigaciones se subsegmento.
### La subsegmentación a la base de investigaciones tiene que ver con que se aplicaron filtros a las investigaciones.
### Se aplicó filtro a para eliminar los delitos anteriores a 2018. El marco legal se modifica constantemente y en ese sentido,
### lo que antes era delito puede ya no serlo en la actualidad. Desde 2012, ha habido 12 cambios al Código Penal de la Ciudad de México.
### Además se hace una segmentación entre delitos que sí pueden ser observados en cámara y aquellos que no pueden ser observados.
### Esta segmentación podría no ser exacta. Se eliminaron algunos delitos como Aborto,


#Paquetes  a utilizar
library(tidyverse)
library(lubridate)
library(rgdal)
library(leaflet)
library(readxl)
library(readr)
library(RCurl)
library(foreign)
library(geosphere)
library(sp)

#Cargamos las bases de datos, en este caso la Carpeta de datos de delitos y la de escuelas que son similes de las cámaras.
investigaciones <- read_csv("da_carpetas-de-investigacion-pgj-cdmx (1).csv")
escuelas <- read.csv("escuelas.csv", encoding="UTF-8", comment.char="#")

#Eliminamos valores que sean cero. solo nos quedamos con registros validos
investigaciones1 <- filter(investigaciones,latitud != 0 & longitud != 0)
escuelas1 <- filter(escuelas,latitud != 0 & longitud != 0)

#Convertimos ao_hechos de caracter a factor
investigaciones1$ao_hechos  <- as.factor(investigaciones1$ao_hechos)

# Se crea un grafico con los delitos por año
ggplot(investigaciones1, aes(x = ao_hechos)) +
  geom_bar() +
  theme_minimal() +
  labs(title = "Delitos por año",
       x = "Años",
       y = "Delitos")

#Revisando la base observamos que hay valores de años muy viejos, se acorta la base a que los delitos hayan pasado despues de 2017
investigaciones1 <- filter(investigaciones1,ao_inicio >= 2017 )

#Creamos la variable fecha que es una unión de año con mes
investigaciones1$fecha <- paste0(investigaciones1$ao_hechos, investigaciones1$mes_hechos)

#Convertimos fecha de caracter a factor
investigaciones1$fecha  <- as.factor(investigaciones1$fecha)

#Convertimos delito de caracter a factor
investigaciones1$delito <- as.factor(investigaciones1$delito)

levels(investigaciones1$delito)

investigaciones1$delitoxcamara <- ifelse(investigaciones1$delito %in% c("ULTRAJES","TRATA DE PERSONAS","TORTURA","SUSTRACCION DE MENORES","ROBO DE VEHICULO DE SERVICIO PARTICULAR CON VIOLENCIA","ROBO DE ACCESORIOS DE AUTO","ROBO DE MOTOCICLETA CON VIOLENCIA","ROBO DE ALHAJAS","ROBO A TRANSEUNTE DE CELULAR CON VIOLENCIA","ROBO A PASAJERO A BORDO DE TRANSPORTE PÚBLICO CON VIOLENCIA","ROBO A PASAJERO / CONDUCTOR DE TAXI CON VIOLENCIA","ROBO A NEGOCIO CON VIOLENCIA","ROBO A CASA HABITACION CON VIOLENCIA","PLAGIO O SECUESTRO","NARCOMENUDEO POSESIÓN CON FINES DE VENTA, COMERCIO Y SUMINISTRO","LESIONES INTENCIONALES" ,"LESIONES CULPOSAS","HOMICIDIO POR ARMA DE FUEGO","HOMICIDIO POR AHORCAMIENTO","HOMICIDIO CULPOSO POR ARMA DE FUEGO","FRAUDE","FEMINICIDIO","EXTORSION","DESAPARICION FORZADA DE PERSONAS","DELITOS ELECTORALES","DELITOS CONTRA LA SALUD","DELITOS AMBIENTALES","CONTAMINACIÓN O RESIDUOS","ABUSO SEXUAL","ACOSO SEXUAL","VIOLACION","ACOSO SEXUAL","VIOLACION EQUIPARADA","VIOLACION TUMULTUARIA"), 1, 0)
investigaciones1 <- filter(investigaciones1, delitoxcamara != 0)


#### 1. ¿Cuántos delitos son observados por, al menos una cámara? ####

total0 <- c() #Definimos un vector columna vación

#Hacemos un for para todas las "investigaciones" para cada uno de las "camaras"
a <- now()
for(i in 1:73404){
  vec <- 0 #cuenta delitos totales
  for(j in 1:nrow(escuelas1)){
    if((distHaversine(c(escuelas1$longitud[j],escuelas1$latitud[j]),c(investigaciones1$longitud[i],investigaciones1$latitud[i]), r=6378137) < 100)){
      vec <- vec + 1 #Encontramos un delito mas
    }
  }
  total0 <- rbind(total0,vec) #Acumulamos los delitos cercanos por cada uno
  print(i)
}

#Medimos el tiempo de procesamiento
a_ciclo <- now() - a
a_ciclo

#Delitos cercanos (a menos de r metros) a estos puntos
total0

numero_de_unos <- sum(total0 == 0)


#### 2. ¿Cuántas cámaras observan, al menos un delito? #####

### El resultado sería que 39 "camaras" no registran un delito grave.

total0 <- c() #Definimos un vector columna vación

#Hacemos un for para todas las "camaras" para cada uno de los delitos
a <- now()
for(i in 1:3){
  vec <- 0 #cuenta delitos totales
  for(j in 1:nrow(investigaciones1)){
    if((distHaversine(c(investigaciones1$longitud[j],investigaciones1$latitud[j]),c(escuelas1$longitud[i],escuelas1$latitud[i]), r=6378137) < 100)){
      vec <- vec + 1 #Encontramos un delito mas
    }
  }
  total0 <- rbind(total,vec) #Acumulamos los delitos cercanos por cada uno
  print(i)
}

#Medimos el tiempo de procesamiento
a_ciclo <- now() - a
a_ciclo

#Delitos cercanos (a menos de r metros) a estos puntos
total0


#### 3. ¿Cuál es la cámara que observa más delitos? #####

#### La camará que más delitos observa es el INSTITUTO COMERCIAL MENDOZA que esta en la Gustavo A Madero.

total <- c()

a <- now()
for(i in 1:365){
  vec <- 0 #cuenta delitos totales
  for(j in 1:nrow(investigaciones1)){
    if((distHaversine(c(investigaciones1$longitud[j],investigaciones1$latitud[j]),c(escuelas1$longitud[i],escuelas1$latitud[i]), r=6378137) < 100)){
      vec <- vec + 1 #Encontramos un delito mas
    }
  }
  total <- rbind(total,vec) #Acumulamos los delitos cercanos por cada uno
  print(i)
}

#Medimos el tiempo de procesamiento
a_ciclo <- now() - a
a_ciclo

#Delitos cercanos (a menos de r metros) a estos puntos
total


### 4. ¿Cuántos delitos no son observados por, al menos una cámara? ####



##################################################################
########################################################################
######## Método proyección UTM y valor absoluto

#Primero, convertimos todos los puntos a UTM Mercator
#Todos los puntos de la CDMX están en UTM zone 14
UTM <- '14'
d <- data.frame(lon=investigaciones1$longitud, lat=investigaciones1$latitud)
coordinates(d) <- c("lon", "lat")

#Estan en lat,lon entonces declaramos la proyeccion usual
sputm <- SpatialPoints(d, proj4string=CRS("+proj=longlat +datum=WGS84"))

#Todos los puntos viven en la CDMX
proyeccion<-CRS("+proj=utm +zone=14 +datum=WGS84 +units=m +no_defs ") 

#Transformamos los datos
spgeo <- spTransform(sputm, proyeccion)
spgeo<-as.data.frame(spgeo)
colnames(spgeo) <- c("lon_UTM","lat_UTM")#Tenemos un equivalente como "lat,lon" en coordendas en el plano cartesiano

########################################################################
#Proceso valor absoluto

#Cargamos la distancia euclidiana
euclidean <- function(a, b) sqrt(sum((a - b)^2))

total2 <- c()#auxiliar para guardar


#####################################################################################
#Antes de hacer un ciclo, hagamos ciertas operaciones para el primer delito
i <- 1

aux <- 0#Contador de delitos dentro del buffer

#Delitos dentro del cuadrado, con base en el valor absoluto
vec1 <- which( abs(spgeo$lat_UTM[i] - spgeo$lat_UTM) < 100)
vec2 <- which( abs(spgeo$lon_UTM[i] - spgeo$lon_UTM) < 100)

#Si estan en ambos indices, implica que estan dentro del cuadrado de lado r y centro en c(spgeo$lat_UTM[i],spgeo$lon_UTM[i])
vec <- intersect(vec1,vec2)

#Pregunta:
vec
#¿Estos delitos están a menos de 100 metros del delito 1?
#¿Sí o no por qué?
#Explicación
#Excalidraw...

#####################################################################################






#####################################################################################
#Ciclo con los 5 primeros delitos
b <- now()

for(i in 1:5){
  aux <- 0#Contador de delitos dentro del buffer
  
  #Delitos dentro del cuadrado, con base en el valor absoluto
  vec1 <- which( abs(spgeo$lat_UTM[i] - spgeo$lat_UTM) < 100)
  vec2 <- which( abs(spgeo$lon_UTM[i] - spgeo$lon_UTM) < 100)
  
  #Si estan en ambos indices, implica que estan dentro del cuadrado de lado r y centro en c(spgeo$lat_UTM[i],spgeo$lon_UTM[i])
  vec <- intersect(vec1,vec2)
  
  #Ahora necesito descartar aquellos que están fuera del círculo de radio r y centro en c(spgeo$lat_UTM[i],spgeo$lon_UTM[i])
  if(length(vec) > 0){ #Solo verificamos que hay, al menos, un punto dentro del cuadrado
    for(j in 1:length(vec)){
      if( (euclidean(c(spgeo$lon_UTM[i],spgeo$lat_UTM[i]),c(spgeo$lon_UTM[vec[j]],spgeo$lat_UTM[vec[j]]))  < 100) ){
        aux <- aux + 1 #Confirmamos un delito dentro del circulo
      }
    }
  }
  #Acumulamos el numero de delitos en el buffer de cada uno
  total2 <- rbind(total2,aux)
  print(i)
}

#Medimos el tiempo de procesamiento
b_ciclo <- now() - b
b_ciclo


#####Notemos lo siguiente:
# ¿Por qué crees que ocurren estas diferencias?
cbind(total,total2) %>% View()



##################################################################

