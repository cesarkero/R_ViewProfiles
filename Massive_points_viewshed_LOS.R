install.packages("whitebox", repos="http://R-Forge.R-project.org")
library(whitebox)
library(rgdal)
library(stringr)
library(sp)
library(mapview)
library(raster)
library(rgeos)
library(base)
library(ggplot2)
library(base)

#access to saga tools
library(RSAGA)
work_env <- rsaga.env() 
rsaga.get.libraries() 
rsaga.get.modules('ta_profiles')
rsaga.get.usage('ta_profiles', 4)

dem <- "./DEM/DEM.tif"
r <- raster(dem)
shpA <- readOGR("./PA", "PA")
shpB <- readOGR("./PB", "PB")
alturaA <- 20
alturaB <- 200

t0 <- Sys.time()
counter <- 0
#---------------------------------------------------------------------------------
# Other method of LOS
for (iA in 1:nrow(shpA)) {
    # iterate over B points
    for (iB in 1:nrow(shpB)){
        iA = 1
        iB = 1
        # Create line between points
        l <- rbind(c(shpA[iA,1]@coords[1:2]), c(shpB[iB,1]@coords[1:2]))
        shp <- coords2Lines(l, ID=paste0(iA,"_",iB))
        crs(shp) <- crs(shpA)
        
        corAstring <- toString(base::round(l[1,])) #coords A string
        corBstring <- toString(base::round(l[2,])) #coords B string
        
        # name of the profile
        perfil <- paste("Perfil",names(shp), "de", corAstring, "a", corBstring)
        
        # Line distance
        n <- SpatialLinesLengths(shp)[[1]]
        
        rsaga.import.gdal(dem) #guardará un archivo gri junto al dem original
        rsaga.geoprocessor('ta_profiles', module = 4, 
                            env = work_env,
                            param = list(DEM = dem,
                                         VALUES = 'ID',
                                         LINES = shp,
                                         NAME = 'ID',
                                         PROFILE = './output/profile.shp',
                                         PROFILES = './output/profiles.shp',
                                         SPLIT = 'FALSE'))
        
        # calculate points along a line (each 1m so n = spatialLinesLengths)
        # point calculation should be +1 to get point until the end of the line
        # example, if distance is 100 metros, point sample should be 101
        p <- spsample(shp, n = n, type="regular")
        p
        # calculate profiles by points before (extract raster values)
        profile <- extract(r,p)
        profile <- extract(r, shp)
        
        # add column for m in x axis
        p_df <- cbind(data.frame(c(0:(n))), data.frame(profile)) #puntos a data frame
        
        # cambiar nombres a data frame
        colnames(p_df) <- c("Distancia","Altitud")
        
        # calculate min and max of Altitud to define visible range in plot
        p_min <- base::min(p_df$Altitud)
        p_max <- base::max(p_df$Altitud)
        
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
        
        counter <- counter+1
        
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
                labs(x = "Distancia", y = "Altitud", title = perfil)+
                scale_y_continuous(limits = c(p_min-50,p_max+500)) +
                geom_abline(intercept=intercept, slope=slope)+
                # add segments to represent A and B
                geom_segment(aes(x=xA, y=yA, xend=xA, yend=yA+alturaA), colour="red")+
                geom_segment(aes(x=xB, y=yB, xend=xB, yend=yB+alturaB), colour="red")+
                annotate(geom="text", x=xA, y=yA+alturaA+20, label="A")+
                annotate(geom="text", x=xA, y=yA+alturaA+150,
                         label=toString(base::round(l[1,])), angle=90, color="blue", size = 3)+
                annotate(geom="text", x=xB, y=yB+alturaB+20, label="B")+
                annotate(geom="text", x=xB, y=yB+alturaB+150,
                         label=toString(base::round(l[2,])), angle=90, color="blue", size = 3)+
                guides(fill="none")+
                theme_gray()
            #plot gg104 or plot inside a list
            # plot(gg104)
            # plots[[pi]] <- gg104
            # pi<-pi+1 
            
        } else {
            # Plot with interception
            #________________________________________
            # Add first coordinate of blocked line of sight
            # P = intx/Distancia total entre puntos (se usara n para simplificar)
            # x = x1 + (x2-x1)*P
            # y = y1 + (y2-y1)*P
            intx1 <- int.pts[1]@coords[1]
            P1 = intx1/n
            contactoX1 <- shpA[iA,1]@coords[1] + (shpB[iB,1]@coords[1]-shpA[iA,1]@coords[1])*P1
            contactoY1 <- shpA[iA,1]@coords[2] + (shpB[iB,1]@coords[2]-shpA[iA,1]@coords[2])*P1
            contacto1 <- paste(base::round(contactoX1),",",base::round(contactoY1))
            
            intx2 <- int.pts[length(int.pts)]@coords[1]
            P2 = intx2/n
            contactoX2 <- shpA[iA,1]@coords[1] + (shpB[iB,1]@coords[1]-shpA[iA,1]@coords[1])*P2
            contactoY2 <- shpA[iA,1]@coords[2] + (shpB[iB,1]@coords[2]-shpA[iA,1]@coords[2])*P2
            contacto2 <- paste(base::round(contactoX2),",",base::round(contactoY2))
            
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
                labs(x = "Distancia", y = "Altitud", title = perfil,
                     subtitle = "")+
                scale_y_continuous(limits = c(p_min-50,p_max+500)) +
                geom_abline(intercept=intercept, slope=slope) +
                geom_point(data=intervisP, aes(colour = "red", size = 0.5))+
                # add segments to represent A and B
                geom_segment(aes(x=xA, y=yA, xend=xA, yend=yA+alturaA), color="red")+
                geom_segment(aes(x=xB, y=yB, xend=xB, yend=yB+alturaB), color="red")+
                annotate(geom="text", x=xA, y=yA+alturaA+20, label="A")+
                annotate(geom="text", x=xA, y=yA+alturaA+150,
                         label=toString(base::round(l[1,])), angle=90, color="blue", size = 3)+
                annotate(geom="text", x=xB, y=yB+alturaB+20, label="B")+
                annotate(geom="text", x=xB, y=yB+alturaB+150,
                         label=toString(base::round(l[2,])), angle=90, color="blue", size = 3)+
                # Add first coordinate of each blocked line of sight
                annotate(geom="text", x=int.pts[1]@coords[1], y= int.pts[1]@coords[2],
                         label=contacto1, angle=90, size=3, vjust=0, hjust=-0.15) +
                annotate(geom="text", x=int.pts[length(int.pts)]@coords[1], y= int.pts[length(int.pts)]@coords[2],
                         label=contacto2, angle=90, size=3, vjust=0, hjust=-0.15)+
                guides(fill="none")+
                theme(legend.position = "none", plot.title = element_text(size=10, face = "bold"))
            
            #plot gg104 or plot inside a list
            # plot(gg104)
            # plots[[pi]] <- gg104
            # pi<-pi+1 
        }
    }
}
t1 <- Sys.time()
difftime(t1,t0)
counter