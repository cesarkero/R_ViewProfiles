#QGIS PARAMETERS
##Perfiles=group
##shpdirA=file
##shpdirB=file
##rasterdir=file
##outpdf=folder
##alturaA=number 1.8
##alturaB=number 180.0

# install and load packages
#source("Z:/De sastre/CAC/github/DEM_LIDAR_products/Base.R")

#setting environment
library(gridExtra)
library(raster)
library(lidR)
library(maptools)
library(rgdal)
library(ggplot2)
library(raster)
library(sp)
library(rstudioapi)
library(leaflet)
library(mapview)
library(rgeos)

# PARAMETERS R
shpdirA <- "Z:/Proxectos/603_Mod_PE_SFaro/5_GIS/shp/Posiciones_infografias.shp" #capa de obervadores
shpdirB <- "Z:/Proxectos/603_Mod_PE_SFaro/5_GIS/shp/Aeros_406_originales_V112.shp" #capa de elementos
rasterdir <- "Z:/Proxectos/603_Mod_PE_SFaro/5_GIS/raster/MDT05_05km_mask.tif"
outpdf <- "Z:/De sastre/CAC/github/DEM_LIDAR_products/OUTPUT/Terrain_profiles_points"
alturaA <- 1.8 #altura de observador
alturaB <- 180.0 #altura del elemento

#leer archivos
shpA <- readOGR(shpdirA)
shpB <- readOGR(shpdirB)
r <- raster(rasterdir)

# _________________________________________
# PLOT OF POINTS OVER RASTER 3D
# extraer cota de los puntos de las capas
# tabla de posiciones x, y, z

# plot3d(r, add=T, adjust=F)
# cotasA <- extract(r, shpA)
# cotasB <- extract(r, shpB)
# posA <- cbind(coordinates(shpA),cotasA)
# posB <- cbind(coordinates(shpB),cotasB)
# points3d(posA, col="red", pointstyle = "s", transparency=0, hyperlinks=NULL, scale=5)
# points3d(posB, col="blue", pointstyle = "s", transparency=0, hyperlinks=NULL, scale=5)


# _____________________________________________________________________________
# PROFILE TOOL
# Define pdf where plots will be attached
pdf(paste0(outpdf,"/","Perfiles.pdf"),
    onefile=T,
    width=10, height=5, title="Perfiles de visibilidad")

# rows and collums per page (doesnt work yet)
# par does not work with ggplot
par(mfrow=(c(4,1)))
par(mar = c(5,4,4,5)+.1)

# list of plots
# plots <- list()
# pi <- 1

# iterate over A points
for (iA in 1:nrow(shpA)) {
        # iterate over B points
        for (iB in 1:nrow(shpB)){
                # Create line between points
                l <- rbind(c(shpA[iA,1]@coords[1:2]), c(shpB[iB,1]@coords[1:2]))
                lr <- round(l,1)
                shp <- coords2Lines(lr, ID=paste0(iA,"_",iB))
                crs(shp) <- crs(shpA)
                perfil <- paste("Perfil",names(shp))
                
                # Round ditance
                n <- round(SpatialLinesLengths(shp)[[1]])
                
                # calculate points along a line (each 1m so n = spatialLinesLengths)
                # point calculation should be +1 to get point until the end of the line
                # example, if distance is 100 metros, point sample should be 101
                p <- spsample(shp, n=n+1, type="regular")
                
                # calculate profiles by points before (extract raster values)
                profile <- extract(r,p)
                
                # add column for m in x axis
                p_df <- cbind(data.frame(c(0:(n))), data.frame(profile)) #puntos a data frame
                
                # cambiar nombres a data frame
                colnames(p_df) <- c("Distancia","Altitud")
                
                # calculate min and max of Altitud to define visible range in plot
                p_min <- min(p_df$Altitud)
                p_max <- max(p_df$Altitud)
                
                #____________________________________________
                # CALCULATE STRAIGHT LINE BETWEEN POINTS
                # calculate line fuction and slope by (y2-y1)/(x2-x1)
                y2 <- p_df$Altitud[nrow(p_df)] + alturaB  #add here heigth of object B
                y1 <- p_df$Altitud[1] + alturaA #add here height of viewer A
                x2 <- p_df$Distancia[nrow(p_df)]
                x1 <- p_df$Distancia[1]
                intercept <- y1 
                slope <- (y2-y1)/(x2-x1)
                
                # transfor this line into data frame of Dist and Alt
                # calculate intersection point between profile and line of sight 
                alt_list <- c()
                for (i in c(1:nrow(p_df))) {
                        y <- intercept + slope * i # line equation
                        alt_list <- append(alt_list,y)
                }
                intervis <- cbind(data.frame(c(0:(n))), data.frame(alt_list)) #points to data frame
                
                # change names
                colnames(intervis) <- c("Distancia","Altitud")
                
                #__________________________________________________________
                # PROFILE PLOT WITH INTERVISIBILITY LINES 
                # convert to a sp object (spatial lines)
                l1 <- Line(p_df) #profile line
                l2 <- Line(intervis) #intervisibility line
                ll1 <- Lines(list(l1), ID = "1")
                ll2 <- Lines(list(l2), ID = "1")
                sl1 <- SpatialLines(list(ll1), proj4string = CRS("+init=epsg:25829"))
                sl2 <- SpatialLines(list(ll2), proj4string = CRS("+init=epsg:25829"))
                
                # calculate intersection points (blocked line of sight)
                int.pts <- gIntersection(sl1, sl2, byid = TRUE)
                
                #______________________________________________________
                # coordinates x and y for start and end points from data frame
                xA <- p_df[1,1]
                yA <- p_df[1,2]
                xB <- p_df[dim(p_df)[1],1]
                yB <- p_df[dim(p_df)[1],2]
                
                # visibility plot
                if (is.null(int.pts)){
                        # plot without interceptions between profile and line of sight
                        gg104 <- ggplot(p_df, aes(x=Distancia, y=Altitud)) +
                                geom_line()+
                                labs(x = "Distancia",
                                     y = "Altitud",
                                     title = perfil)+
                                scale_y_continuous(limits = c(p_min-50,p_max+200)) +
                                geom_abline(intercept=intercept, slope=slope)+
                                # add segments to represent A and B
                                geom_segment(aes(x=xA, y=yA, xend=xA, yend=yA+alturaA), colour="red")+
                                geom_segment(aes(x=xB, y=yB, xend=xB, yend=yB+alturaB), colour="red")+
                                annotate(geom="text", x=xA, y=yA+alturaA+20, label="A")+
                                annotate(geom="text", x=xB, y=yB+alturaB+20, label="B")+
                                guides(fill="none")+
                                theme_gray()
                        #plot gg104 or plot inside a list
                        plot(gg104)
                        # plots[[pi]] <- gg104
                        # pi<-pi+1 
                        
                } else {
                        # Plot with interception
                        #________________________________________
                        # Add first coordinate of blocked line of sight
                        # P = intx/Distancia total entre puntos (se usara n para simplificar)
                        # x = x1 + (x2-x1)*P
                        # y = y1 + (y2-y1)*P
                        int.pts[1,1]@coords[1,1]
                        intx <- int.pts[1,1]@coords[1,1]
                        P = intx/n
                        contactoX1 <- shpA[iA,1]@coords[1] + (shpB[iB,1]@coords[1]-shpA[iA,1]@coords[1])*P
                        contactoY1 <- shpA[iA,1]@coords[2] + (shpB[iB,1]@coords[2]-shpA[iA,1]@coords[2])*P
                        contacto1 <- paste(round(contactoX1),",",round(contactoY1))
                        #_____________________________________________________
                        
                        # Profile and line of sight
                        int.coords <- as.table(int.pts@coords)
                        int.coords2 <- data.frame(int.coords)
                        xdist <- int.coords2$Freq[int.coords2$Var2=="x"]
                        yalt <- int.coords2$Freq[int.coords2$Var2=="y"]
                        intervisP <- data.frame(cbind(xdist,yalt))
                        colnames(intervisP) <- c("Distancia","Altitud")
                        
                        gg104 <- ggplot(p_df, aes(x=Distancia, y=Altitud)) +
                                geom_line()+
                                labs(x = "Distancia",
                                     y = "Altitud",
                                     title = perfil)+
                                scale_y_continuous(limits = c(p_min-50,p_max+200)) +
                                geom_abline(intercept=intercept, slope=slope) +
                                geom_point(data=intervisP, aes(colour = "red", size = 0.5))+
                                # add segments to represent A and B
                                geom_segment(aes(x=xA, y=yA, xend=xA, yend=yA+alturaA), color="red")+
                                geom_segment(aes(x=xB, y=yB, xend=xB, yend=yB+alturaB), color="red")+
                                annotate(geom="text", x=xA, y=yA+alturaA+20, label="A")+
                                annotate(geom="text", x=xB, y=yB+alturaB+20, label="B")+
                                # Add first coordinate of blocked line of sight
                                annotate(geom="text", 
                                         x=int.pts[1]@coords[1], y= int.pts[1]@coords[2],
                                         label=contacto1, 
                                         angle=90, size=3, vjust=0, hjust=-0.15) +
                                guides(fill="none")+
                                theme_gray()
                        
                        #plot gg104 or plot inside a list
                        plot(gg104)
                        # plots[[pi]] <- gg104
                        # pi<-pi+1 
                       
                }
        }
}

# close and save pdf
dev.off() 
