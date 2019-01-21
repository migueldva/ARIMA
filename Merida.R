# Plotting Merida's domestic destinations


#NOTE: IT'S REQUIRED TO RUN THE map_script.r in order for this script to function properly.

#Required libraries:
library(maptools)
library(raster)
library(plyr)
library(ggplot2)
library(readxl)
library(tidyverse)

path <- "C:/Users/migue/Documents/JFF/Airports/Files"


#Read passenger traffic:

flights.domesc <- read_excel(path = paste0(path,"/burup.xlsx"), sheet = 1)
flights.int <- read_excel(path = paste0(path,"/burup.xlsx"), sheet = 2)
pax.domesc <- read_excel(path = paste0(path,"/burup.xlsx"), sheet = 3)
pax.int <- read_excel(path = paste0(path,"/burup.xlsx"), sheet = 4)


#Arrange the airport directory, filter only Mexican airports.

airport.directory <- airport.directory[,1:9] %>% na.omit() 
names(airport.directory) <- c("airport", "iata", "country.iso", "city", "continent.iso", 
                          "long", "lat", "company", "state")

airport.directory.mx <- airport.directory %>% filter(country.iso == "MX")




#Organize Merida's passenger traffic:

merida.origin <- pax.domesc %>% 
                  filter(`Destination (IATA)` == "MID" & Year == 2017) %>%
                  group_by(`Origin (IATA)`, Year) %>%
                  summarise(pax = sum(Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec)) 

#Get the state name of each airport
names(merida.origin) <- c("iata", "Year", "pax")
merida.origin <-  merge(merida.origin, airport.directory.mx[,c(2,9)], by = "iata", all.y = T) %>%  
                  group_by(state) %>%
                  summarise(pax = sum(na.omit(pax)))
  

names(merida.origin) <- c("NAME_1", "pax")

merida.origin$pax <- (merida.origin$pax/sum(merida.origin$pax))*100

     
#Get map data, and format according to needs:
Mexico <- getData(name = "GADM", country = "MX", level = 1)

Mexico@data$id <- rownames(Mexico@data)

#Merge map's data with Merida's data
Mexico@data <- merge(Mexico@data, merida.origin, by="NAME_1", all = T)
Mexico@data$pax[is.na(Mexico@data$pax)] <- 0
Mexico@data$pax[Mexico@data$pax == 0] <- 100
Mexico@data$pax[Mexico@data$pax > 0 & Mexico@data$pax <= 5] <- 200
Mexico@data$pax[Mexico@data$pax > 5 & Mexico@data$pax <= 10] <- 300
Mexico@data$pax[Mexico@data$pax > 70 & Mexico@data$pax < 100] <- 400
Mexico_df <- fortify(Mexico)
Mexico_df <- join(Mexico_df,Mexico@data, by="id")

Mexico_df$pax <- as.factor(Mexico_df$pax)


ggplot() + 
  geom_polygon(data = Mexico_df, aes(long, lat, group = group, fill = pax)) +
  geom_path(data = Mexico_df, aes(long, lat, group = group), color = "gray28") +
  geom_point(data = airport.directory.mx, aes(long, lat, color = company), size = 2.5) +
  scale_fill_brewer(palette = "Blues", labels = c("0%", "0% - 5%", "5% - 10%", "> 70%"), name = "% of domestic traffic") +
  scale_color_manual(values = c("#4D3C2F","#528D09", "#C07156", "#E8C767", "#A43725"), name = "Airport operated by:") +
  geom_text(aes(-89.65770, 20.93700 + 0.5, label = "MID"), size = 4) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        text = element_text(size = 11))

  


Mexico_UTM <- getData("GADM", country = "MX", level = 1)

limits = c(min(merida.origin$pax[merida.origin$pax != 0]), max(merida.origin$pax))

