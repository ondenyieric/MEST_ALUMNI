library(maps)
library(ggplot2)
library(ggmap)

world_map <- map_data("world")

for(i in 1:nrow(CLEAN_MEST_DATAFRAME))
{
  # Print("Working...")
  result <- geocode(CLEAN_MEST_DATAFRAME$subline_2[i], output = "latlona", source = "google")
  CLEAN_MEST_DATAFRAME$lon[i] <- as.numeric(result[1])
  CLEAN_MEST_DATAFRAME$lat[i] <- as.numeric(result[2])
  CLEAN_MEST_DATAFRAME$geoAddress[i] <- as.character(result[3])
}

# Write CSV in R
write.csv(CLEAN_MEST_DATAFRAME, file = "CLEAN_MEST_DATAFRAME_WITH_LOCATION.csv")
CLEAN_MEST_DATAFRAME_WITH_LOCATION<-read.csv("CLEAN_MEST_DATAFRAME_WITH_LOCATION.csv")


#new_CLEAN_MEST_DATAFRAME<-CLEAN_MEST_DATAFRAME
p <- ggplot() + coord_fixed() +
  xlab("") + ylab("")
  
#Add map to base plot
base_world_messy <- p + geom_polygon(data=world_map, aes(x=long, y=lat, group=group), 
                               colour="light green", fill="light green")

base_world_messy

#Strip the map down so it looks super clean (and beautiful!)
cleanup <- 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = 'white', colour = 'white'), 
        axis.line = element_line(colour = "white"), legend.position="none",
        axis.ticks=element_blank(), axis.text.x=element_blank(),
        axis.text.y=element_blank())

base_world <- base_world_messy + cleanup

base_world

map_data <- 
  base_world +
  geom_point(data=CLEAN_MEST_DATAFRAME_WITH_LOCATION, 
             aes(x=lon, y=lat), colour="Deep Pink", 
             fill="Blue",pch=21, size=10, alpha=I(0.7))

map_data
