# Iowa COVID Assessment Map
# Map color codes the counties of Iowa by the 6 regions being used by the Iowa Dept Public Health
# Map created on April 6, 2020 by GM Wilkinson 
# Please suggest changes and 

#Required libraries
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(dplyr)
library(scales)

#name the built-in state dataset in map_data to 'states'
states <- map_data("state")

#select only the Iowa information from the states dataset
ia_state <- subset(states, region == "iowa")

#name the built-in county dataset in mapdata to 'counties'
counties <- map_data("county")

#select only the Iowa information from the states dataset
ia_county <- subset(counties, region == "iowa")

#Read in the file "IowaCountyNames.csv" before proceeding
# region colors
colors = read.csv("IowaCountyNames.csv") %>% mutate(subregion = as.character(subregion))

#Join the colors by subregion to the spatial data
ia_county = left_join(ia_county, colors)

#Create a color pallette
v_colors =  viridis::viridis(12)

#Assign each region a color 
my_colors= c("a"=v_colors[8], "b"=v_colors[4], "c"=v_colors[7], "d" = v_colors[5],
             "e"=v_colors[3], "f"=v_colors[6])

#Aggregate the county names and find the county centroid
cnames <- aggregate(cbind(long, lat) ~ subregion, data=ia_county, 
                    FUN=function(x)mean(range(x)))


#plot the map
ggplot(ia_county, aes(long, lat)) +  
  geom_polygon(aes(group=group, fill=covid_region), colour='black') +
  scale_fill_manual(breaks = unique(ia_county$covid_region), values= my_colors) +
  
  geom_text(data=cnames, aes(long, lat, label = subregion), size=3.5, colour="white") + 
  
  coord_map() + theme_nothing() + theme(plot.background = element_rect(fill=NA)) + 
  theme(legend.position = "right")

  