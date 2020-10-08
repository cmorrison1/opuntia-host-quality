####################################################################################################
####################################################################################################
################################### Map Making Script #############################################
####################################################################################################
####################################################################################################


## adapted from script by Luis Santiago-Rosario
## PhD Candidate, Harms Lab, Louisiana State University

getwd()
setwd("~/Desktop/oppuntia.cactoblastis/Oppuntia Host Quality DBs")


#packages needed
install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel",
                   "ggspatial", "libwgeom", "sf", "rnaturalearth", 
                   "rnaturalearthdata"))
install.packages("rgeos")
install.packages("maps")
install.packages("viridis") #this one is a good one for giving colors that is colorblind friendly. Look for the package info.
install.packages("ggspatial")
install.packages('lwgeom')
install.packages('rcartocolor')
install.packages('cowplot')
install.packages('cowplot')






#sites data points
sites<-read.table(file.choose(),header = T) #here you need the coordinates plus the variables you wish to map.
attach(sites)
head(sites)



# bring world map into memory
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
# bring USA state map into memory 
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
head(states)
# bring Texas county map into memory
counties <- st_as_sf(map("county", plot = FALSE, fill = TRUE))
counties <- subset(counties, grepl("texas", counties$ID))
counties$area <- as.numeric(st_area(counties))
head(counties)


# inset frame object
pol<-data.frame(xmin=-99.0,xmax=-94.5 ,ymin=27.75 ,ymax=31.0)

### Texas map with inset of sites
ggm1<- ggplot(data = world) +
  geom_sf() +
  geom_sf(data=states,fill=NA) + 
  xlab('') +
  ylab('') +
  geom_label(data = states, aes(-100.5,32.25, label = "Texas"), size = 7, 
             fontface = 1, 
             nudge_y = states$nudge_y) +
  geom_rect(data = pol, aes(xmin = xmin,
                            xmax = xmax,
                            ymin = ymin,
                            ymax = ymax),
            alpha=0, colour="red", 
            size = 1, 
            linetype=1) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.2, "in"), pad_y = unit(0.4, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-108.0, -92.0), ylim = c(24.5, 38.0), expand = FALSE) + 
  theme(plot.margin = margin(t = 20, r = 5, b = 5, l =  0, unit = "pt"))
#

### PLOT Texas insert with Field Sites

# Texas Sampling Locations
travis<- data.frame(longitude = c(-97.8403), 
                    latitude = c(30.16197))
matagorda<- data.frame(longitude = c(-96.05985277777778), 
                       latitude = c(28.67887))

ggm2<-ggplot(data = world) +
  geom_sf() +
  geom_sf(data=states,fill=NA) + 
  geom_sf(data=counties,fill=NA) +
  xlab('') +
  ylab('') +
  geom_label(data = states, aes(-96.15, 29.0, label = "Matagorda County Site"), size = 3.5, 
             fontface = "bold", 
             nudge_y = states$nudge_y) +
  geom_label(data = states, aes(-97.9, 30.5, label = "Travis County Site"), size = 3.5, 
             fontface = "bold", 
             nudge_y = states$nudge_y) +
  geom_point(data = travis, aes(x = longitude, y = latitude), size = 6, 
             shape = 16, fill = "blue") +
  geom_point(data = matagorda, aes(x = longitude, y = latitude), size = 6, 
             shape = 16, fill = "red3") +
  annotation_scale(location = "br", width_hint = 0.4) +
  coord_sf(xlim = c(-99.0, -94.5), ylim = c(27.75, 31.0), expand = FALSE) + 
  theme(plot.margin = margin(t = 20, r = 15, b = 5, l =  0, unit = "pt"))
ggm2
#


### Insert without field sites 
austin<- data.frame(longitude = c(-97.7430573), 
                       latitude = c(30.2671509))
	

ggm3<-ggplot(data = world) +
  geom_sf() +
  geom_sf(data=states,fill=NA) + 
  geom_sf(data=counties,fill=NA) +
  xlab('') +
  ylab('') +
  geom_point(data = travis, aes(x = longitude, y = latitude), size = 6, 
             shape = 16, fill = "blue") +
  geom_label(data = states, aes(-97.85, 30.33, label = "Austin"), size = 4.0, 
             fontface = "bold", 
             nudge_y = states$nudge_y) +
  annotation_scale(location = "br", width_hint = 0.4) +
  coord_sf(xlim = c(-99.0, -94.5), ylim = c(27.75, 31.0), expand = FALSE) + 
  theme(plot.margin = margin(t = 20, r = 15, b = 5, l =  0, unit = "pt"))
ggm3

### Create a MASTER figure by combining the all the maps
library(ggpubr)
figure<-ggarrange(ggm1, ggm2,nrow=1,ncol=2)
                  
figure
annotate_figure(figure,
                top=text_grob("A. Field Sites Location",
                             size = 17,vjust=4.0,hjust=0.5))

#gg_inset_map1 = ggdraw() +
#  draw_plot(ggm2) +
#  draw_plot(ggm1, x = -95, y = 30, 
#            width = 10, height = 10)
#gg_inset_map1



####################################################################################################
####################################################################################################
