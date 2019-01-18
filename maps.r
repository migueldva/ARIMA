####Map for Merida


library(maptools)
library(raster)
library(plyr)
library(ggplot2)
library(tidyverse)



###Get map's data.
US <- getData("GADM", country = "US", level = 1)
US <- US[!(US@data$NAME_1 %in% c("Alaska", "Hawaii")),]
Mexico_UTM <- getData("GADM", country = "MX", level = 1)


###Make Mexico's data frame for ggplot
NAME_1<- Mexico_UTM@data$NAME_1
count<-sample(1:1000,32)     

count_df<-data.frame(NAME_1, count)

Mexico_UTM@data$id <- rownames(Mexico_UTM@data)
Mexico_UTM@data <- join(Mexico_UTM@data, count_df, by="NAME_1")
Mexico_df <- fortify(Mexico_UTM)
Mexico_df <- join(Mexico_df,Mexico_UTM@data, by="id")


###Make US's data frame for ggplot
NAME_1<- US@data$NAME_1
count_2<-sample(1:1000,49)     

count_df_2<-data.frame(NAME_1, count_2)

US@data$id <- rownames(US@data)
US@data <- join(US@data, count_df_2, by="NAME_1")
US_df <- fortify(US)
US_df <- join(US_df,US@data, by="id")


###Read airport location's

air_directory <- read_excel(path = paste0(path,"/Base_File.xlsx"), sheet = 5,
                            col_names = F)
air_directory <- na.omit(air_directory[,1:8])

names(air_directory) <- c("airport", "iata", "country.iso", "city", "continent.iso", 
                          "lat", "long", "company")
air_directory_mid <- air_directory %>% filter(country.iso == "US" | country.iso == "MX") %>%
                      filter(!(iata %in% c("ANC", "HNL")))


ggplot() + 
  geom_polygon(data = Mexico_df, aes(x = long,y = lat,group = group)) +
  geom_path(data = Mexico_df, aes(long, lat, group = group), color = "gray28") +
  geom_polygon(data = US_df, aes(x = long,y = lat,group = group)) +
  geom_path(data = US_df, aes(long, lat, group = group), color = "gray28") +
  geom_point(data = air_directory_mid, aes(lat, long, fill = company), color = "black", shape = 21, size = 4)
