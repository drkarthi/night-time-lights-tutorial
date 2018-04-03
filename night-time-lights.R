library(doParallel)
library(foreach)
library(raster)
library(sp)
library(rgdal)
library(ggmap)
library(plotly)

imagery = "./imagery"

# load the imagery file
tifs = list.files(imagery, pattern = "\\.tif")
rast <- raster(paste0(imagery, "/", tifs[1]))

# set the wgs84 projection for the raster
wgs84 <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(rast) <- CRS(wgs84)

# Data downloaded from http://mcdc.missouri.edu/data/popests/CBSA-EST2014-alldata.csv
msa_pop <- read.csv("CBSA-EST2014-alldata.csv")
msa_pop <- msa_pop[msa_pop$LSAD=="Metropolitan Statistical Area",]
msa_pop <- msa_pop[order(msa_pop$POPESTIMATE2014),]
msa_pop$NAME <- as.character(msa_pop$NAME) 

# top 35 cities
cities <- c("New York, NY", "Los Angeles, CA","Chicago, IL", "Houston, TX",
            "Philadelphia, PA", "Phoenix, AZ", "San Antonio, TX", "San Diego, CA",     
            "Dallas, TX", "San Jose, CA", "Austin, TX", "Jacksonville, FL",
            "San Francisco, CA", "Indianapolis, IN", "Columbus, OH", "Fort Worth, TX",
            "Charlotte, NC", "Detroit, MI", "El Paso, TX", "Seattle, WA",
            "Denver, CO","Washington, DC", "Memphis, TN", "Boston, MA",
            "Nashville, TN", "Baltimore, MD", "Oklahoma City, OK", "Portland, OR",
            "Las Vegas, NV", "Louisville, KY","Milwaukee, WI","Albuquerque, NM",
            "Tucson, AZ","Fresno, CA","Sacramento, CA")

# Population map within each city

# Set graph layout
par(mai=c(0,0,0,0),mfrow = c(7,5),bg='#001a4d', bty='n')

coords <- data.frame()

for(i in 1:length(cities)){

  temp_coord <- geocode(cities[i], source = "google")
  coords <- rbind(coords,temp_coord)
  
  # frame around the city
  e <- extent(temp_coord$lon - 1, temp_coord$lon + 1,
              temp_coord$lat - 0.25, temp_coord$lat + 0.25)
  # divide raster based on these frames
  rc <- crop(rast, e)
  
  sampled <- as.vector(rc)
  clusters <- 15
  clust <- kmeans(sampled,clusters)$cluster
  combined <- as.data.frame(cbind(sampled,clust))
  brk <- sort(aggregate(combined[,1], list(combined[,2]), max)[,2])
  
  #Plots
  plot(rc, breaks=brk, col=colorRampPalette(c("#001a4d","#0066FF", "yellow"))(clusters), 
       legend=F,yaxt='n',xaxt='n',frame = F, asp=1.5)
  text(temp_coord$lon ,temp_coord$lat + 0.15,
       substr(cities[i],1,regexpr(",",cities[i])-1), 
       col="white", cex=1.25)
  
  rm(combined)
}

# Population map comparing the different cities

#Set layout
par(mai=c(0,0,0,0),mfrow = c(7,5),bg='#001a4d', bty='n')

#Run clustering
set.seed(123) #set seed for reproducibility
sampled <- sample(rast, 20000) #sample 20,000 pixels
clusters <- 15 ##15 clusters
clust <- kmeans(sampled,clusters)$cluster
combined <- as.data.frame(cbind(sampled,clust))
brk <- sort(aggregate(combined[,1], list(combined[,2]), max)[,2])

##Loop through each city
for(i in 1:length(cities)){
  
  temp_coord <- coords[i,] ##re-use the coordinates 
  e <- extent(temp_coord$lon - 1, temp_coord$lon + 1,
              temp_coord$lat - 0.25, temp_coord$lat + 0.25)
  rc <- crop(rast, e)    
  
  #Plots
  plot(rc, breaks=brk, col=colorRampPalette(c("#001a4d","#0066FF", "yellow"))(clusters), 
       legend=F,yaxt='n',xaxt='n',frame = F, asp=1.5)
  text(temp_coord$lon ,temp_coord$lat + 0.15,
       substr(cities[i],1,regexpr(",",cities[i])-1), 
       col="white", cex=1.25)
  
  rm(combined)
}