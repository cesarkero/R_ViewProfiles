library(arulesViz)
library(lidR)
require(maptools)
require(rgdal)
require(ggplot2)
library(raster)
library(leaflet)
library(mapview)
if (!require(rgrass7)) stop("rgrass7 PACKAGE MISSING")
library("rgrass7")
library(sp)
library(sp)
library(rgeos)

epsg <- "+init=epsg:25829"
shpdir <- "Z:/De sastre/CAC/github/LIDAR_products/SHP/Viewshed_positions/Points.shp"
rasterdir <- "Z:/De sastre/CAC/github/LIDAR_products/Raster/raster1.tif"
shp <- readOGR(shpdir)
r <- raster(rasterdir)
plot(r)

#todo el proceso se basa en Terrain_profiles pero aquí solo se detectará si 
#existe contacto con la línea de perfil a lo largo de la línea de intervisibilidad
#no es practico en largos datasets pero es lo que hay
#la idea es realizar los cálculos desde una posición para cada uno de los cuadros raster
#por ejemplo, en un raster de 5m y 30km de lado se realizarían 36.000.000 de cálculos para cada punto
rp <- rasterToPoints(r)
class(rp)

#crear iterador encadenado: de punto y celda en la matriz raster
##valores de cuadro 1
rp[1,]

##valores de punto 1
str(shp[1,1])
shp[1,1]@coords
##crear linea entre puntos
###será necesario crear un data frame con las columasn x e y e los puntos a unir
l1 <- rbind(c(rp[1,1],rp[1,2]), c(shp[1,1]@coords))
l1r <- round(l1,1)
## Crear una linea entre el punto 1 y la celda 1
Sl1 <- coords2Lines(l1r, ID="1")
plot(Sl1)

#extraer los valores raster a lo largo de la línea
##ojo, como la linea se ha creado a partir del punto del raster primero y el punto de avistamiento 
##despues, la extracción de datos del raster también sigue ese orden de creación
profile <- extract(r,Sl1)

#calcular si hay puntos de contacto a lo largo del perfil
#       si hay contacto valor 0 (no visibilidad)
#       si no hay contacto valor 1 (contacto visual)
#calculate points along a line (each 1m so n = spatialLinesLengths)
SpatialLinesLengths(shp)[[1]]
n1 <- round(SpatialLinesLengths(shp)[[1]])
n2 <- round(SpatialLinesLengths(shp)[[2]])
#el calculo de puntos debe ser +1para recoger puntos hsta el final
p1 <- spsample(shp1, n=n1+1, type="regular")
p2 <- spsample(shp2, n=n2+1, type="regular")
#calcular perfiles desde los puntos anteriores (extraer valores del raster)
profile1 <- extract(r,p1)
profile2 <- extract(r,p2)
#add column for m in x axis
data.frame(c(0:n1))
p1_df <- cbind(data.frame(c(0:(n1))), data.frame(profile1)) #puntos a data.frame
p2_df <- cbind(data.frame(c(0:(n2))), data.frame(profile2)) #puntos a data.frame
#cambiar nombres a data frame
colnames(p1_df) <- c("distancia","altitud")
colnames(p2_df) <- c("distancia","altitud")
p1_min <- min(p1_df$altitud)
p1_max <- max(p1_df$altitud)
p2_min <- min(p2_df$altitud)
p2_max <- max(p2_df$altitud)















#extrae en orden los pixeles desde el inicio del shp de linea hsta el final
profiles <- extract(r,shp)
str(profiles)
p1 <- data.frame(profiles[[1]])
class(profiles[[1]])
plot(profiles[[1]])
plot(profiles[[2]])
shp1 <- shp[shp$id==1,]
shp2 <- shp[shp$id==2,]
#longitud de las líneas
SpatialLinesLengths(shp)

#calculate points along a line (each 1m so n = spatialLinesLengths)
n1 <- round(SpatialLinesLengths(shp)[[1]])
n2 <- round(SpatialLinesLengths(shp)[[2]])
#el calculo de puntos debe ser +1para recoger puntos hsta el final
p1 <- spsample(shp1, n=n1+1, type="regular")
p2 <- spsample(shp2, n=n2+1, type="regular")
#calcular perfiles desde los puntos anteriores (extraer valores del raster)
profile1 <- extract(r,p1)
profile2 <- extract(r,p2)
#add column for m in x axis
data.frame(c(0:n1))
p1_df <- cbind(data.frame(c(0:(n1))), data.frame(profile1)) #puntos a data.frame
p2_df <- cbind(data.frame(c(0:(n2))), data.frame(profile2)) #puntos a data.frame
#cambiar nombres a data frame
colnames(p1_df) <- c("distancia","altitud")
colnames(p2_df) <- c("distancia","altitud")
p1_min <- min(p1_df$altitud)
p1_max <- max(p1_df$altitud)
p2_min <- min(p2_df$altitud)
p2_max <- max(p2_df$altitud)
#plot profile
#area plot
gg101 <- ggplot(p1_df, aes(x=distancia, y=altitud)) +
        geom_area(aes(alpha=0.5))+
        scale_color_manual(values = c("#00AFBB")) +
        scale_fill_manual(values = c("#00AFBB"))+
        expand_limits(y=c(0, 50))+
        labs(x = "distancia",
             y = "altitud",
             title = "Perfil 1")+
        scale_y_continuous(limits = c(0,400)) +       
        theme_minimal()
gg101
#line plot
gg102 <- ggplot(p1_df, aes(x=distancia, y=altitud)) +
        geom_line()+
        labs(x = "distancia",
             y = "altitud",
             title = "Perfil 1")+
        scale_y_continuous(limits = c(p1_min-50,p1_max+50)) +       
        theme_minimal()
gg102

#____________________________________________
#CÁLCULO DE RECTA DE INTERVISIBILIDAD ENTRE PUNTOS
#line plot con linea de intercepcion entre punto inicial y punto final
#pendiente calculada a partir dela fórmua (y2-y1)/(x2-x1)
y2 <- p1_df$altitud[nrow(p1_df)] +1.8  #sumar aquí la altura del observador
y1 <- p1_df$altitud[1] +20 #sumar aquí la altura del objeto
x2 <- p1_df$distancia[nrow(p1_df)]
x1 <- p1_df$distancia[1]
intercept <- y1 
slope <- (y2-y1)/(x2-x1)
#hay que transformar esta recta en un data.frame de distancia y altitud para 
#calcular después los puntos de intersección con sp package
#ecuación de la recta 'y = intercept + slope*x'
dim(p1_df)
alt_list <- c()
for (i in c(1:nrow(p1_df))) {
        y <- intercept + slope * i
        alt_list <- append(alt_list,y)
}
intervis <- cbind(data.frame(c(0:(n1))), data.frame(alt_list)) #puntos a data.frame
#cambiar nombres a data frame
colnames(intervis) <- c("distancia","altitud")

#__________________________________________________________
#SUPER PLOT CON PERFIL Y LINEA DE INTERVISIBILIDAD CON PUNTOS DE CONTACTO
#plot
gg103 <- ggplot(p1_df, aes(x=distancia, y=altitud)) +
        geom_line()+
        labs(x = "distancia",
             y = "altitud",
             title = "Perfil 1")+
        scale_y_continuous(limits = c(p1_min-100,p1_max+100)) +
        geom_abline(intercept=intercept, slope=slope) +
        theme_gray()
gg103

#find intersection points (sp package)
library(sp)
library(rgeos)
# convert to a sp object (spatial lines)
l1 <- Line(p1_df) #linea de perfil
l2 <- Line(intervis) #línea recta de intervisibilidad
ll1 <- Lines(list(l1), ID = "1")
ll2 <- Lines(list(l2), ID = "1")
sl1 <- SpatialLines(list(ll1), proj4string = CRS("+init=epsg:25829"))
sl2 <- SpatialLines(list(ll2), proj4string = CRS("+init=epsg:25829"))

# Calculate locations where spatial lines intersect
int.pts <- gIntersection(sl1, sl2, byid = TRUE)

#transformar coordenadas en df para meter en el plot (OPTIMIZAR)
if (is.null(int.pts)){
        #ejecutar plot de lineas sin puntos de contacto
        gg104 <- ggplot(p1_df, aes(x=distancia, y=altitud)) +
                geom_line()+
                labs(x = "distancia",
                     y = "altitud",
                     title = "Perfil 1")+
                scale_y_continuous(limits = c(p1_min-100,p1_max+100)) +
                geom_abline(intercept=intercept, slope=slope) +
                theme_gray()
        gg104
} else {
        #mostrar perfil y linea de puntos y contactos
        int.coords <- as.table(int.pts@coords)
        int.coords2 <- data.frame(int.coords)
        xdist <- int.coords2$Freq[int.coords2$Var2=="x"]
        yalt <- int.coords2$Freq[int.coords2$Var2=="y"]
        intervisP <- data.frame(cbind(xdist,yalt))
        colnames(intervisP) <- c("distancia","altitud")
        
        gg104 <- ggplot(p1_df, aes(x=distancia, y=altitud)) +
                geom_line()+
                labs(x = "distancia",
                     y = "altitud",
                     title = "Perfil 1")+
                scale_y_continuous(limits = c(p1_min-100,p1_max+100)) +
                geom_abline(intercept=intercept, slope=slope) +
                geom_point(data=intervisP, aes(colour = "red", size = 0.5))+
                theme_gray()
        gg104
}



#setting processing times
t0<-Sys.time()
#insert here your code
tf <- Sys.time()
t <- tf-t0
t[[1]] #seconds of processing

#set original wd
setwd(Owd)