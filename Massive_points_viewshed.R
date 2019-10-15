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

dem <- "./DEM/DEM.tif"
r <- raster(dem)
shpA <- readOGR("./PA", "PA")
shpB <- readOGR("./PB", "PB")
alturaA <- 20
alturaB <- 200

#CreateCounter creates a counter adding lef 0 until desired length
CreateCounter <- function(n,l=nchar(as.integer(n))){
  counter <- c(1:n) # Create a counter
  counter <- str_replace_all(str_pad(sapply(counter,toString),l,"left")," ","0")
  return (counter)
}
counter <- CreateCounter(nrow(shpB)) #create counter

# Function to create a viewshed for each point (genera un raster de viewshed por punto)
for(i in 1:nrow(shpB)) { 
  p <- shp[i,]
  c <- counter[i]
  output <- paste0("./output/viewshed_", c,".tif")
  viewshed(dem, p, output, height = 200, verbose_mode = FALSE)
}
