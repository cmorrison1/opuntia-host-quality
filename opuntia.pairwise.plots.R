####################################################################################################
####################################################################################################
################### Script for Opuntia Host QUality Pairwise Comparison Plots ####################
####################################################################################################
####################################################################################################


# Colin Richard Morrison 
# PhD Candidate
# The University of Texas at Austin 
# Department of Integrative Biology 
# Graduate Program In Ecology, Evolution and Behavior
# crmorrison@utexas.edu


###---- Opuntia Host Plant Quality - Pyralid caterpillar Project - Invasive Species Lab, BFL ----###
####################################################################################################


getwd()
setwd("~/Desktop/oppuntia.cactoblastis/Oppuntia Host Quality DBs")



### ------------------------------------------------------------------------------------
### Pairwise comparison of non-infested and infested Opuntia engelmannii individuals  
### ------------------------------------------------------------------------------------

### --- Read data into memory
### cleaned up master DFs with all host quality variables 

# Melitara - Travis County O. engelmannii population
mel<-load(file="mel.pair.comp_RData")
mel

# C. cactorum - Matagorda County O. engelmannii population
cac<-load("cac.pair.comp_RData")
cac


# Pairwise Differences files for Supplemental Figures 2 & 3
diffs<-load("difference.files_2-17-20")
diffs

### ------------------------------------------------------------------------------------
### ------------------------------------------------------------------------------------
### ---------------- PROTEINS  
### ------------------------------------------------------------------------------------

### --- TRAVIS COUNTY POPULATION MELITARA --- ###
### ------------------------------------------------------------------------------------


### ---- FIGURE 5A: Travis [PROTEIN] PLOT----- ###
library(ggplot2)
Traw<-ggplot(comp[-c(12,25),],aes(y=protein, x=pair,fill=herbivores)) + 
  geom_bar(position="dodge", 
           stat="identity",
           col="black",
           width=0.7) +
  ggtitle(expression(bold("A."))) + 
  labs(x = "",
       y = expression(paste("protein (", mu, "g/mg)")),
       fill = "Herbivore Presence")  + 
  ylab(expression(paste("protein (", mu, "g/mg)"))) +
  xlab("") + 
  scale_fill_manual(values=c("blue", "green")) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey50"),
        plot.title = element_text(size=15,vjust=1,hjust=-0.12),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = 45,hjust=1.0),
        axis.title.y = element_text(margin = margin(t = 20,r = 20,b = 20, l = 20)),
        axis.title.x = element_text(margin = margin(t = 20)),
        legend.position = "none",
        plot.margin = margin(t = 0, r = 15, b = 0, l = -10, unit = "pt"),
  )
Traw


#### ---- SUPPLEMENTAL FIGURE 4A: Travis [PROTEIN] Differences between pairs----- ###
### ------------------------------------------------------------------------------------
Tdiff<-ggplot(comp2,aes(x = pair,y = diff)) +
  geom_bar(stat = "identity",
           fill = "grey50",
           colour = "black",
           width = 0.6) + 
  ggtitle(expression(bold("A."))) + 
  labs(x = "",
       y = expression(paste("pairwise difference (", mu, "g/mg) ")))  +
  annotate(geom="text", x=2.6, y=0.059, 
           label=(expression(paste(italic("P "), "= 0.58"))),
           color="black",size=4) + 
  annotate(geom="text", x=2.6, y=0.069, label="paired t-test",
           color="black",size=4) + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey50"),
        plot.title = element_text(size=15,vjust=1,hjust=-0.12),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = 45,hjust=1.0),
        axis.title.y = element_text(margin = margin(t = 20,r = 20,b = 20, l = 20)),
        axis.title.x = element_text(margin = margin(t = 20)),
        plot.margin = margin(t = 0, r = 15, b = 0, l = -10, unit = "pt"),
  )
Tdiff





### --- MATAGORDA COUNTY POPULATION CACTOBLASTIS --- ###
### ------------------------------------------------------------------------------------


#### ---- FIGURE 6A: Cactoblastis Matagorda [PROTEIN] PLOT----- ###
library(ggplot2)
Mraw<-ggplot(Ccomp,aes(y=protein, x=pair,fill=herbivores)) + 
  geom_bar(position="dodge", 
           stat="identity",
           col="black",
           width=0.7) +
  ggtitle(expression(bold("A."))) + 
  scale_fill_manual(values=c("red3", "green")) +
  labs(x = "",
       y = expression(paste("protein (", mu, "g/mg)")),
       fill = "Herbivore Presence")  + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey50"),
        plot.title = element_text(size=15,vjust=1,hjust=-0.12),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = 45,hjust=1.0),
        axis.title.y = element_text(margin = margin(t = 20,r = 20,b = 20, l = 20)),
        axis.title.x = element_text(margin = margin(t = 20)),
        legend.position = "none",
        plot.margin = margin(t = 0, r = 15, b = 0, l = -10, unit = "pt"),
  )
Mraw
  
#### ---- SUPPLEMENTAL FIGURE 5A: Matagorda [PROTEIN] Differences between pairs----- ###
Mdiff<-ggplot(Ccomp2,aes(x = pair,y = diff)) +
  geom_bar(stat = "identity",
           fill = "grey50",
           colour = "black",
           width = 0.6) + 
  ggtitle(expression(bold("A."))) + 
  labs(x = "",
       y = expression(paste("pairwise difference (", mu, "g/mg) ")))  +
  annotate(geom="text", x=1.7, y=-0.07, 
           label=(expression(paste(italic("P "), "= 0.64"))),
           color="black",size=4) + 
  annotate(geom="text", x=1.7, y=-0.06, label="paired t-test",
           color="black",size=4) + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey50"),
        plot.title = element_text(size=15,vjust=1,hjust=-0.12),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = 45,hjust=1.0),
        axis.title.y = element_text(margin = margin(t = 20,r = 20,b = 20, l = 20)),
        axis.title.x = element_text(margin = margin(t = 20)),
        plot.margin = margin(t = 0, r = 15, b = 0, l = -10, unit = "pt"),
  )
Mdiff


### ------------------------------------------------------------------------------------
### ------------------------------------------------------------------------------------
### ---------------- CARBS  
### ------------------------------------------------------------------------------------


### --- TRAVIS COUNTY POPULATION MELITARA --- ###
### ------------------------------------------------------------------------------------

#### ----  FIGURE 5B: Travis [CARB] comparison plot ----- ###
library(ggplot2)
Traw2<-ggplot(comp[-c(12,25),],aes(y=carbohydrate, x=pair,fill=herbivores)) + 
  geom_bar(position="dodge", 
           stat="identity",
           col="black",
           width=0.7) +
  ggtitle(expression(bold("B."))) + 
  labs(x = "",
       y = expression(paste("carbohydrates (", mu, "g/mg)")),
       fill = "Herbivore Presence")  + 
  scale_fill_manual(values=c("blue", "green")) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey50"),
        plot.title = element_text(size=15,vjust=1,hjust=-0.12),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = 45,hjust=1.0),
        axis.title.y = element_text(margin = margin(t = 20,r = 20,b = 20, l = 20)),
        axis.title.x = element_text(margin = margin(t = 20)),
        legend.position = "none",
        plot.margin = margin(t = 0, r = 15, b = 0, l = -10, unit = "pt"),
  )
Traw2

#### ---- SUPPLEMENTAL FIGURE 4B: Travis [CARB] Differences between pairs----- ###
Tdiff2<-ggplot(comp3,aes(x = pair,y = diff)) +
  geom_bar(stat = "identity",
           fill = "grey50",
           colour = "black",
           width = 0.6) + 
  ggtitle(expression(bold("B."))) + 
  labs(x = "",
       y = expression(paste("pairwise difference (", mu, "g/mg) ")))   +
  annotate(geom="text", x=2.6, y=243, 
           label=(expression(paste(italic("P "), "= 0.72"))),
           color="black",size=4) + 
  annotate(geom="text", x=2.6, y=268, label="paired t-test",
           color="black",size=4) + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey50"),
        plot.title = element_text(size=15,vjust=1,hjust=-0.12),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = 45,hjust=1.0),
        axis.title.y = element_text(margin = margin(t = 20,r = 20,b = 20, l = 20)),
        axis.title.x = element_text(margin = margin(t = 20)),
        plot.margin = margin(t = 0, r = 15, b = 0, l = -10, unit = "pt"),
  )
Tdiff2


### --- MATAGORDA COUNTY POPULATION CACTOBLASTIS --- ###
### ------------------------------------------------------------------------------------


#### ---- FIGURE 6B: Matagorda Cactoblastis [CARB] PLOT----- ###
library(ggplot2)
Mraw2<-ggplot(Ccomp,aes(y=carbohydrate, x=pair,fill=herbivores)) + 
  geom_bar(position="dodge", 
           stat="identity",
           col="black",
           width=0.7) +
  ggtitle(expression(bold("B."))) + 
  labs(x = "",
       y = expression(paste("carbohydrates (", mu, "g/mg)")),
       fill = "Herbivore Presence")  + 
  scale_fill_manual(values=c("red3", "green")) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey50"),
        plot.title = element_text(size=15,vjust=1,hjust=-0.12),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = 45,hjust=1.0),
        axis.title.y = element_text(margin = margin(t = 20,r = 20,b = 20, l = 20)),
        axis.title.x = element_text(margin = margin(t = 20)),
        legend.position = "none",
        plot.margin = margin(t = 0, r = 15, b = 0, l = -10, unit = "pt"),
  )
Mraw2

#### ---- SUPPLEMENTAL FIGURE 5B: [CARB] Differences between pairs----- ###
Mdiff2<-ggplot(Ccomp3,aes(x = pair,y = diff)) +
  geom_bar(stat = "identity",
           fill = "grey50",
           colour = "black",
           width = 0.6) + 
  ggtitle(expression(bold("B."))) + 
  labs(x = "",
       y = expression(paste("pairwise difference (", mu, "g/mg) ")))   +
  annotate(geom="text", x=1.6, y=-165, 
           label=(expression(paste(italic("P "), "= 0.56"))),
           color="black",size=4) + 
  annotate(geom="text", x=1.6, y=-135, label="paired t-test",
           color="black",size=4) + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey50"),
        plot.title = element_text(size=15,vjust=1,hjust=-0.12),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = 45,hjust=1.0),
        axis.title.y = element_text(margin = margin(t = 20,r = 20,b = 20, l = 20)),
        axis.title.x = element_text(margin = margin(t = 20)),
        plot.margin = margin(t = 0, r = 15, b = 0, l = -10, unit = "pt"),
  )
Mdiff2

### ------------------------------------------------------------------------------------
### ------------------------------------------------------------------------------------
### ---------------- Fatty Acids
### ------------------------------------------------------------------------------------



### --- TRAVIS COUNTY POPULATION MELITARA --- ###
### ------------------------------------------------------------------------------------

#### ---- FIGURE 5C: [FA] PLOT----- ###
library(ggplot2)
# Panel A: Raw concentration data for each member of a pair, grouped by pairs
FATpercent<-(fa3$mg.mgAVG)*100
Traw3<-ggplot(comp[-c(12,25),],aes(y=fatty.acid, x=pair,fill=herbivores)) + 
  geom_bar(position="dodge", 
           stat="identity",
           col="black",
           width=0.7) +
  ggtitle(expression(bold("C."))) + 
  labs(x = "",
       y = "fatty acids (% dry weight)",
       fill = "Herbivore Presence")  + 
  scale_fill_manual(values=c("blue", "green")) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey50"),
        plot.title = element_text(size=15,vjust=1,hjust=-0.12),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = 45,hjust=1.0),
        axis.title.y = element_text(margin = margin(t = 20,r = 20,b = 20, l = 20)),
        axis.title.x = element_text(margin = margin(t = 20)),
        legend.position = "none",
        plot.margin = margin(t = 0, r = 15, b = 0, l = -10, unit = "pt"),
  )
Traw3

#### ---- SUPPLEMENTAL FIGURE 4C: [FA] Differences between pairs----- ###
Tdiff3<-ggplot(comp4,aes(x = pair,y = diff)) +
  geom_bar(stat = "identity",
           fill = "grey50",
           colour = "black",
           width = 0.6) + 
  ggtitle(expression(bold("C."))) + 
  labs(x = "",
       y = "pairwise difference (%)")  +
  annotate(geom="text", x=2.6, y=0.0145, 
           label=(expression(paste(italic("P "), "= 0.28"))),
           color="black",size=4) + 
  annotate(geom="text", x=2.6, y=0.017, label="paired t-test",
           color="black",size=4) + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey50"),
        plot.title = element_text(size=15,vjust=1,hjust=-0.12),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = 45,hjust=1.0),
        axis.title.y = element_text(margin = margin(t = 20,r = 20,b = 20, l = 20)),
        axis.title.x = element_text(margin = margin(t = 20)),
        plot.margin = margin(t = 0, r = 15, b = 0, l = -10, unit = "pt"),
  )
Tdiff3


### --- MATAGORDA COUNTY POPULATION CACTOBLASTIS --- ###
### ------------------------------------------------------------------------------------

#### ---- FIGURE 6C: Matagorda Cactoblastis [fatty acid] PLOT----- ###
library(ggplot2)
Mraw3<-ggplot(Ccomp,aes(y=fatty.acid, x=pair,fill=herbivores)) + 
  geom_bar(position="dodge", 
           stat="identity",
           col="black",
           width=0.7) +
  ggtitle(expression(bold("C."))) + 
  labs(x = "",
       y = "fatty acids (% dry weight)",
       fill = "Herbivore Presence")  + 
  scale_fill_manual(values=c("red3", "green")) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey50"),
        plot.title = element_text(size=15,vjust=1,hjust=-0.12),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = 45,hjust=1.0),
        axis.title.y = element_text(margin = margin(t = 20,r = 20,b = 20, l = 20)),
        axis.title.x = element_text(margin = margin(t = 20)),
        legend.position = "none",
        plot.margin = margin(t = 0, r = 15, b = 0, l = -10, unit = "pt"),
  )
Mraw3

#### ---- SUPPLEMENTAL FIGURE 5C: [fatty acid] Differences between pairs----- ###
Mdiff3<-ggplot(Ccomp4,aes(x = pair,y = diff)) +
  geom_bar(stat = "identity",
           fill = "grey50",
           colour = "black",
           width = 0.6) + 
  ggtitle(expression(bold("C."))) + 
  labs(x = "",
       y = "pairwise difference (%)")   +
  annotate(geom="text", x=1.7, y=0.0059, 
           label=(expression(paste(italic("P "), "= 0.18"))),
           color="black",size=4) + 
  annotate(geom="text", x=1.7, y=0.0065, label="paired t-test",
           color="black",size=4) + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey50"),
        plot.title = element_text(size=15,vjust=1,hjust=-0.12),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = 45,hjust=1.0),
        axis.title.y = element_text(margin = margin(t = 20,r = 20,b = 20, l = 20)),
        axis.title.x = element_text(margin = margin(t = 20)),
        plot.margin = margin(t = 0, r = 15, b = 0, l = -10, unit = "pt"),
  )
Mdiff3

### ------------------------------------------------------------------------------------
### ------------------------------------------------------------------------------------
### ---------------- FIBER  
### ------------------------------------------------------------------------------------


### --- TRAVIS COUNTY POPULATION MELITARA --- ###
### ------------------------------------------------------------------------------------

#### ---- FIGURE 5D: % Fiber PLOT----- ###
library(ggplot2)
Traw4<-ggplot(comp,aes(y=fiber, x=pair,fill=herbivores)) + 
  geom_bar(position="dodge", 
           stat="identity",
           col="black",
           width=0.7) +
  ggtitle(expression(bold("D."))) + 
  labs(x = "",
       y = "fiber (% dry weight)",
       fill = "Herbivore Presence")  + 
  scale_fill_manual(values=c("blue", "green")) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey50"),
        plot.title = element_text(size=15,vjust=1,hjust=-0.12),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = 45,hjust=1.0),
        axis.title.y = element_text(margin = margin(t = 20,r = 20,b = 20, l = 20)),
        axis.title.x = element_text(margin = margin(t = 20)),
        legend.position = "none",
        plot.margin = margin(t = 0, r = 15, b = 0, l = -10, unit = "pt"),
  )
Traw4

#### ---- SUPPLEMENTAL FIGURE 4D: Fiber Differences between pairs----- ###
Tdiff4<-ggplot(comp5,aes(x = pair,y = diff)) +
  geom_bar(stat = "identity",
           fill = "grey50",
           colour = "black",
           width = 0.6) + 
  ggtitle(expression(bold("D."))) + 
  labs(x = "",
       y = "pairwise difference (%)")  +
  annotate(geom="text", x=2.6, y=0.045, 
           label=(expression(paste(italic("P "), "= 0.59"))),
           color="black",size=4) + 
  annotate(geom="text", x=2.6, y=0.053, label="paired t-test",
           color="black",size=4) + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey50"),
        plot.title = element_text(size=15,vjust=1,hjust=-0.12),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = 45,hjust=1.0),
        axis.title.y = element_text(margin = margin(t = 20,r = 20,b = 20, l = 20)),
        axis.title.x = element_text(margin = margin(t = 20)),
        plot.margin = margin(t = 0, r = 15, b = 0, l = -10, unit = "pt"),
  )
Tdiff4


### --- MATAGORDA COUNTY POPULATION CACTOBLASTIS --- ###

#### ---- FIGURE 6D: Matagorda Cactoblastis FIBER PLOT----- ###
library(ggplot2)
Mraw4<-ggplot(Ccomp,aes(y=fiber, x=pair,fill=herbivores)) + 
  geom_bar(position="dodge", 
           stat="identity",
           col="black",
           width=0.7) +
  ggtitle(expression(bold("D."))) + 
  labs(x = "",
       y = "fiber (% dry weight)",
       fill = "Herbivore Presence")  + 
  scale_fill_manual(values=c("red3", "green")) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey50"),
        plot.title = element_text(size=15,vjust=1,hjust=-0.12),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = 45,hjust=1.0),
        axis.title.y = element_text(margin = margin(t = 20,r = 20,b = 20, l = 20)),
        axis.title.x = element_text(margin = margin(t = 20)),
        legend.position = "none",
        plot.margin = margin(t = 0, r = 15, b = 0, l = -10, unit = "pt"),
  )
Mraw4

#### ---- SUPPLEMENTAL FIGURE 5D: Fiber Differences between pairs----- ###
Mdiff4<-ggplot(Ccomp5,aes(x = pair,y = diff)) +
  geom_bar(stat = "identity",
           fill = "grey50",
           colour = "black",
           width = 0.6) + 
  ggtitle(expression(bold("D."))) + 
  labs(x = "",
       y = "pairwise difference (%)")   +
  annotate(geom="text", x=1.6, y=0.024, 
           label=(expression(paste(italic("P "), "= 0.18"))),
           color="black",size=4) + 
  annotate(geom="text", x=1.6, y=0.03, label="paired t-test",
           color="black",size=4) + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey50"),
        plot.title = element_text(size=15,vjust=1,hjust=-0.12),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = 45,hjust=1.0),
        axis.title.y = element_text(margin = margin(t = 20,r = 20,b = 20, l = 20)),
        axis.title.x = element_text(margin = margin(t = 20)),
        plot.margin = margin(t = 0, r = 15, b = 0, l = -10, unit = "pt"),
  )
Mdiff4


### ------------------------------------------------------------------------------------
### ------------------------------------------------------------------------------------
### ---------------- WATER CONTENT   
### ------------------------------------------------------------------------------------


### --- TRAVIS COUNTY POPULATION MELITARA --- ###
### ------------------------------------------------------------------------------------

#### ---- FIGURE 5E: % WATER PLOT----- ###
library(ggplot2)
Traw5<-ggplot(comp,aes(y=water, x=pair,fill=herbivores)) + 
  geom_bar(position="dodge", 
           stat="identity",
           col="black",
           width=0.7) +
  ggtitle(expression(bold("E."))) + 
  labs(x = "local pair",
       y = "water (%)",
       fill = "Herbivore Presence")  + 
  scale_fill_manual(values=c("blue", "green")) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey50"),
        plot.title = element_text(size=15,vjust=1,hjust=-0.12),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = 45,hjust=1.0),
        axis.title.y = element_text(margin = margin(t = 20,r = 20,b = 20, l = 20)),
        axis.title.x = element_text(margin = margin(t = 20)),
        legend.position = "none",
        plot.margin = margin(t = 0, r = 15, b = 0, l = -10, unit = "pt"),
  )
Traw5

#### ---- SUPPLEMENTAL FIGURE 4E: Water Content Differences between pairs----- ###
Tdiff5<-ggplot(comp6,aes(x = pair,y = diff)) +
  geom_bar(stat = "identity",
           fill = "grey50",
           colour = "black",
           width = 0.6) + 
  ggtitle(expression(bold("E."))) + 
  labs(x = "local pair",
       y = "pairwise difference (%)")  +
  annotate(geom="text", x=2.6, y=0.056, 
           label=(expression(paste(italic("P "), "= 0.82"))),
           color="black",size=4) + 
  annotate(geom="text", x=2.6, y=0.062, label="paired t-test",
           color="black",size=4) + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey50"),
        plot.title = element_text(size=15,vjust=1,hjust=-0.12),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = 45,hjust=1.0),
        axis.title.y = element_text(margin = margin(t = 20,r = 20,b = 20, l = 20)),
        axis.title.x = element_text(margin = margin(t = 20)),
        plot.margin = margin(t = 0, r = 15, b = 0, l = -10, unit = "pt"),
  )
Tdiff5


### --- MATAGORDA COUNTY POPULATION CACTOBLASTIS --- ###


#### ---- FIGURE 6E: Matagorda Cactoblastis WATER PLOT----- ###
library(ggplot2)
Mraw5<-ggplot(Ccomp,aes(y=water, x=pair,fill=herbivores)) + 
  geom_bar(position="dodge", 
           stat="identity",
           col="black",
           width=0.7) +
  ggtitle(expression(bold("E."))) + 
  labs(x = "local pair",
       y = "water (%)",
       fill = "Herbivore Presence")  + 
  scale_fill_manual(values=c("red3", "green")) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey50"),
        plot.title = element_text(size=15,vjust=1,hjust=-0.12),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = 45,hjust=1.0),
        axis.title.y = element_text(margin = margin(t = 20,r = 20,b = 20, l = 20)),
        axis.title.x = element_text(margin = margin(t = 20)),
        legend.position = "none",
        plot.margin = margin(t = 0, r = 15, b = 0, l = -10, unit = "pt"),
  )
Mraw5

#### ---- SUPPLEMENTAL FIGURE 5E: Matagorda Water Differences between pairs----- ###
Mdiff5<-ggplot(Ccomp6,aes(x = pair,y = diff)) +
  geom_bar(stat = "identity",
           fill = "grey50",
           colour = "black",
           width = 0.6) + 
  ggtitle(expression(bold("E."))) + 
  labs(x = "local pair",
       y = "pairwise difference (%)")   +
  annotate(geom="text", x=1.6, y=-0.032, 
           label=(expression(paste(italic("P "), "= 0.23"))),
           color="black",size=4) + 
  annotate(geom="text", x=1.6, y=-0.026, label="paired t-test",
           color="black",size=4) + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey50"),
        plot.title = element_text(size=15,vjust=1,hjust=-0.12),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = 45,hjust=1.0),
        axis.title.y = element_text(margin = margin(t = 20,r = 20,b = 20, l = 20)),
        axis.title.x = element_text(margin = margin(t = 20)),
        plot.margin = margin(t = 0, r = 15, b = 0, l = -10, unit = "pt"),
  )
Mdiff5




### ---- COMMON Melitara Legend - Travis Co. Plots (Figure 5)
# I will save this as a plot and include it in the multipanel figure
library(cowplot)
library(grid)
library(gridExtra)
Tlegend <- cowplot::get_legend(Traw)
grid.newpage()
grid.draw(Tlegend)

### ---- COMMON Cactoblastis Legend - Matagorda Co. Plots (Figure 6)
# I will save this as a plot and include it in the multipanel figure
Mlegend <- cowplot::get_legend(Mraw)
grid.newpage()
grid.draw(Mlegend)



### save all the FIGURE 5 and 6 plots for the multipanel Figures 
save(Traw,Traw2,Traw3,Traw4,Traw5,Tlegend,
     Mraw,Mraw2,Mraw3,Mraw4,Mraw5,Mlegend,
     file="fig5and6_3-28-20_RData")


### save all the supplemental FI 4 and 5 plots for the multipanel Figures 
save(Tdiff,Tdiff2,Tdiff3,Tdiff4,Tdiff5,
     Mdiff,Mdiff2,Mdiff3,Mdiff4,Mdiff5,
     file="supplemental4and5_3-28-20_RData")


### ------- FIGURE 5 - Multiple Panel Figure Tavis County Pairwise
pnames<-load(file="fig5and6_3-28-20_RData")
pnames

library(ggpubr)
FIG5<-ggarrange(Traw,Traw2,Traw3,
                Traw4,Traw5,Tlegend,
                nrow=2,ncol=3)
FIG5
#

### ------- FIGURE 6 - Multiple Panel Figure Matagora County Pairwise
FIG6<-ggarrange(Mraw,Mraw2,Mraw3,
                Mraw4,Mraw5,Mlegend,
                nrow=2,ncol=3)
FIG6
#




snames<-load(file="supplemental4and5_3-28-20_RData")
snames
### ------- Supplemental FIGURE 4 - Multiple Panel Figure Travis Paired T-test Graph
S4<-ggarrange(Tdiff,Tdiff2,
              Tdiff3,Tdiff4,Tdiff5,
              nrow=2,ncol=3)
S4
#


### ------- Supplemental FIGURE 5 - Multiple Panel Figure Matagorda Paired T-test Graph
S5<-ggarrange(Mdiff,Mdiff2,
              Mdiff3,Mdiff4,Mdiff5,
              nrow=2,ncol=3)
S5
#




# ----- CITE R Core Team 
citation()

#To cite R in publications use:
  
#  R Core Team (2018). R: A language and environment for statistical
# computing. R Foundation for Statistical Computing, Vienna, Austria.
# URL https://www.R-project.org/.

#################################################################################
#################################################################################
