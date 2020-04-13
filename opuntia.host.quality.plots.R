#################################################################################
#################################################################################
################# Script for Opuntia Host Quality Plots ########################
#################################################################################
#################################################################################


# Colin Richard Morrison 
# PhD Candidate
# The University of Texas at Austin 
# Department of Integrative Biology 
# Graduate Program In Ecology, Evolution and Behavior
# crmorrison@utexas.edu


### ---- Opuntia Host Plant Quality - Pyralid caterpillar Project - Invasive Species Lab, BFL ----###
### ---- Comparison of Opuntia species Host Quality --------------------------------------------- ###
#####################################################################################################


getwd()
setwd("~/Desktop/oppuntia.cactoblastis/Oppuntia Host Quality DBs")



### ------------------------------------------------------------------------------------
### FIGURE 2A: [Protein] Species Comparison 

# read in data
prot=read.csv("protein.csv")
nrow(prot) # 276
# remove outliers
prot2<-prot[-c(3,147,216,249),]
nrow(prot2) # 272
# subset for just new (terminal) cladodes 
prot3<-prot2[which(prot2$age == 'N'), names(prot2) 
            %in% c('Sample','species','age','herbivores','dry.g','ug.mg','avg.ug.mg')]
nrow(prot3)
colnames(prot2)
head(prot2)

prot3$avg.ug.mg<-as.numeric(as.character(prot3$avg.ug.mg))
levels(prot2$herbivores)

# plot
library(ggplot2)
protein<-ggplot(prot3, aes(x=species, y=avg.ug.mg,color='')) + 
  geom_boxplot(width=0.5,color='black') + guides(fill=FALSE) +
  geom_jitter(position=position_jitter(0.1),color='black') + 
  ggtitle(expression(bold("A."))) +
  xlab("") + 
  theme(plot.title = element_text(size = rel(1.5),hjust=0.5)) + 
  ylab(expression(paste("protein (", mu, "g/mg)"))) +
  ylim(0.0,0.25) +
  scale_x_discrete(labels=c("O. engelmannii","O. ficus-indica",
                            "O. macrorhiza","O. stricta")) +
  annotate(geom="text", x=1, y=0.2, label="A",
           color="black",size=4) +
  annotate(geom="text", x=2, y=0.22, label="B",
           color="black",size=4) +
  annotate(geom="text", x=3, y=0.19, label="AB",
           color="black",size=4) +
  annotate(geom="text", x=4, y=0.11, label="A",
           color="black",size=4) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "none",
        plot.title = element_text(size=15,vjust=1,hjust=-0.12),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(face = "italic", angle = 45,hjust=1.0,color="black"),
        axis.title.y = element_text(margin = margin(t = 20,r = 20,b = 20, l = 20)),
        axis.title.x = element_text(margin = margin(t = 20)),
        plot.margin = margin(t = 0, r = 20, b = -15, l = 0, unit = "pt"))

protein

# filter dataframe to get C. cactorum data points to be highlighted
#cacto<-prot2[which(prot2$herbivores == 'cactoblastis'), names(prot2) 
#              %in% c('Sample','species','herbivores','mass.mg','ug.mg','avg.ug.mg')]
#cacto$avg.ug.mg<-as.numeric(as.character(cacto$avg.ug.mg))

# plot FIG.3A again with extra layer of red points showcasing C. cactorum infested plants
#protein2<-prot3 %>% 
#  ggplot(aes(x=species, y=avg.ug.mg,color='')) + 
#  geom_boxplot(width=0.5,color='black') + guides(fill=FALSE) +
#  geom_jitter(position=position_jitter(0.1),color='black') + 
#  geom_point(data=cacto, aes(x=species, y=avg.ug.mg), color='red',size=2) +
#  ggtitle("A. Protein Concentration") +
#  xlab("") + 
#  theme(plot.title = element_text(size = rel(1.5),hjust=0.5)) + 
#  ylab(expression(paste("protein (", mu, "g/mg)"))) +
#  ylim(0.0,0.03) +
#  annotate(geom="text", x=1, y=0.028, label="A",
#           color="black",size=5) +
#  annotate(geom="text", x=2, y=0.019, label="A",
#           color="black",size=5) +
#  annotate(geom="text", x=3, y=0.022, label="A",
#           color="black",size=5) +
#  annotate(geom="text", x=4, y=0.0185, label="A",
#           color="black",size=5) +
#  theme(panel.grid.major = element_blank(), 
#        panel.grid.minor = element_blank(),
#        panel.background = element_blank(),
#        plot.subtitle = element_text(size=14,hjust = 0.5,vjust=8),
#        legend.position = "none",
#        plot.title = element_text(size=14,vjust=8),
#        axis.line = element_line(colour = "black"),
#        axis.text.x = element_text(face = "italic", angle = 70,hjust=1),
#        axis.title.y = element_text(margin = margin(t = 20,r = 20,b = 20, l = 20)),
#        axis.title.x = element_text(margin = margin(t = 20)),
#        plot.margin = margin(t = 30, r = 20, b = 20, l = 0, unit = "pt")) 
#  
#protein2
# 

### ------------------------------------------------------------------------------------
### FIGURE 2B: Digestible [Carbohydrate] Species Comparison 

# read in data
carb=read.csv("carb.csv")
colnames(carb)
# subset for just new (terminal) cladodes 
carb2<-carb[which(carb$age == 'N'), names(carb) 
            %in% c('Sample','species','herbivores','dry','ug.mg','avg.ug.mg')]
carb2$avg.ug.mg<-as.numeric(as.character(carb2$avg.ug.mg))

# plot
library(ggplot2)
carb<-ggplot(carb2, aes(x=species, y=avg.ug.mg,color='')) + 
  geom_boxplot(width=0.5,color='black') + guides(fill=FALSE) +
  geom_jitter(position=position_jitter(0.1),color='black') +
  # geom_point(data=cacto, aes(x=species, y=avg.ug.mg), color='red',size=2) +
  ggtitle(expression(bold("B."))) +
  xlab("species") + 
  scale_y_continuous(limits = c()) +
  theme(plot.title = element_text(size = rel(1.5),hjust=0.5)) + 
  ylab(expression(paste("carbohydrates (", mu, "g/mg)"))) +
  ylim(0.0,725) +
  scale_x_discrete(labels=c("O. engelmannii","O. ficus-indica",
                            "O. macrorhiza","O. stricta")) +
  annotate(geom="text", x=1, y=725.0, label="A",
           color="black",size=4) +
  annotate(geom="text", x=2, y=435.0, label="B",
           color="black",size=4) +
  annotate(geom="text", x=3, y=545.0, label="A",
           color="black",size=4) +
  annotate(geom="text", x=4, y=495.0, label="A",
           color="black",size=4) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "none",
        plot.title = element_text(size=15,vjust=1,hjust=-0.12),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(face = "italic", angle = 45,hjust=1.0,color="black"),
        #axis.text.x = element_blank(),
        axis.title.y = element_text(margin = margin(t = 20,r = 20,b = 20, l = 20)),
        axis.title.x = element_text(margin = margin(t = 20)),
        plot.margin = margin(t = 0, r = 20, b = -15, l = 0, unit = "pt")) 

carb


### ------------------------------------------------------------------------------------
### FIGURE 2C: [FATTY ACID] Species Comparison 

# read in data frame
fa=read.csv("fat.csv")
colnames(fa)
# subset for just new (terminal) cladodes 
fa2<-fa[which(fa$age == 'N'), names(fa) %in% c('Sample','species','herbivores','dry','mg.mg','mg.mgAVG')]
fa3<-fa2[-c(3),]
fa3$mg.mgAVG<-as.numeric(as.character(fa3$mg.mgAVG))
FATpercent<-(fa3$mg.mgAVG)*100


# plot
library(ggplot2)
fat<-ggplot(fa3, aes(x=species, y=FATpercent,color='')) + 
  geom_boxplot(width=0.5,color='black') + guides(fill=FALSE) +
  geom_jitter(position=position_jitter(0.1),color='black') +
  # geom_point(data=cacto, aes(x=species, y=mg.mgAVG), color='red',size=2) +
  ggtitle(expression(bold("C."))) +
  xlab("") + 
  ylim(0,6.0) + 
  annotate(geom="text", x=1, y=6.0, label="A",
           color="black",size=4) +
  annotate(geom="text", x=2, y=3.3, label="AB",
           color="black",size=4) +
  annotate(geom="text", x=3, y=3.2, label="AB",
           color="black",size=4) +
  annotate(geom="text", x=4, y=1.9, label="B",
           color="black",size=4) +
  scale_x_discrete(labels=c("O. engelmannii","O. ficus-indica",
                            "O. macrorhiza","O. stricta")) +
  theme(plot.title = element_text(size = rel(1.5),hjust=0.5)) + 
  ylab(expression(paste("fatty acids (% dry weight)"))) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "none",
        plot.title = element_text(size=15,vjust=1,hjust=-0.12),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(face = "italic", angle = 45,hjust=1.0,color="black"),
        #axis.text.x = element_blank(),
        axis.title.y = element_text(margin = margin(t = 20,r = 20,b = 20, l = 20)),
        axis.title.x = element_text(margin = margin(t = 20)),
        plot.margin = margin(t = 0, r = 20, b = -15, l = 0, unit = "pt")) 

fat

### ------------------------------------------------------------------------------------
### FIGURE 2D: Intraspecific [FIBER] 
library(ggplot2)

# read in data frame
fib=read.csv("fiber.csv")

# fib<-fib[,c(1,2,3,5,10)] # just columns of interest for ease of subsetting
# remove O. englemannii outliers detected in dataQC script
fib2<-fib[-c(16,21,90,91,92,93,95,96),]
# subset for just new (terminal) cladodes 
colnames(fib2)
fib3<-fib2[which(fib2$age == 'N'), names(fib2) 
           %in% c('Sample','species','pair','age',
                  'herbivore','dry.g','fiber.g','concentration')]
# convert water.content data to non-decimal percentages
FIBpercent<-(fib3$concentration)*100

# plot
fiber<-ggplot(fib3, aes(x=species, y=FIBpercent,color='')) + 
  geom_boxplot(width=0.5,color='black') + guides(fill=FALSE) +
  geom_jitter(position=position_jitter(0.1),color='black') + 
  ggtitle(expression(bold("D."))) +
  theme(plot.title = element_text(size = rel(1.5),hjust=0.5)) + 
  ylab(expression(paste("fiber (% dry weight)"))) +
  ylim(0,100) + 
  xlab("") + 
  scale_x_discrete(labels=c("O. engelmannii","O. ficus-indica",
                            "O. macrorhiza","O. stricta")) +
  annotate(geom="text", x=1, y=78, label="A",
           color="black",size=4) +
  annotate(geom="text", x=2, y=46, label="B",
           color="black",size=4) +
  annotate(geom="text", x=3, y=51, label="B",
           color="black",size=4) +
  annotate(geom="text", x=4, y=75, label="AB",
           color="black",size=4) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "none",
        plot.title = element_text(size=15,vjust=1,hjust=-0.12),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(face = "italic", angle = 45,hjust=1),
        axis.title.y = element_text(margin = margin(t = 20,r = 20,b = 20, l = 20)),
        axis.title.x = element_text(margin = margin(t = 20)),
        plot.margin = margin(t = 0, r = 20, b = -15, l = 0, unit = "pt")) 

fiber

  
### ------------------------------------------------------------------------------------
### FIGURE 2E: Water Content

# read in data 
h2o=read.csv('water.csv')
colnames(h2o)

h2o2<-h2o[which(h2o$age == 'N'), names(h2o) %in% c('unique','species','herbivores','pair','water.content')]
colnames(h2o)
head(h2o3)
h2o2$water.content<-as.numeric(as.character(h2o2$water.content))
# remove outliers 
h2o3<-h2o2[-c(4:6,49),]
# convert water.content data to non-decimal percentages
WATpercent<-(h2o3$water.content)*100 

# plot
library(ggplot2)
water<-ggplot(h2o3, aes(x=species, y=WATpercent,color='')) + 
  geom_boxplot(width=0.5,color='black') + guides(fill=FALSE) +
  geom_jitter(position=position_jitter(0.1),color='black') + 
  ggtitle(expression(bold("E."))) +
  theme(plot.title = element_text(size = rel(1.5),hjust=0.5)) + 
  ylab("water content (%)") +
  xlab("species") +
  ylim(80,100) +
  scale_x_discrete(labels=c("O. engelmannii","O. ficus-indica",
                            "O. macrorhiza","O. stricta")) +
  annotate(geom="text", x=1, y=92, label="A",
           color="black",size=4) +
  annotate(geom="text", x=2, y=98, label="B",
           color="black",size=4) +
  annotate(geom="text", x=3, y=92, label="A",
           color="black",size=4) +
  annotate(geom="text", x=4, y=92, label="A",
           color="black",size=4) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "none",
        plot.title = element_text(size=15,vjust=1,hjust=-0.12),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(face = "italic", angle = 45,hjust=1),
        axis.title.y = element_text(margin = margin(t = 20,r = 20,b = 20, l = 20)),
        axis.title.x = element_text(margin = margin(t = 20)),
        plot.margin = margin(t = 0, r = 20, b = -15, l = 0, unit = "pt")) 

water


### ------------------------------------------------------------------------------------
### FIGURE 2F: Cuticle Thickness

# read in data 
cut<-read.csv("cuticle.csv")
cut2<-cut[-c(12,15),] # huge outliers from BFL O.englemannii population 
colnames(cut)

# plot
library(ggplot2)
thick<-ggplot(cut2, aes(x=species, y=thickAVG,color='')) + 
  geom_boxplot(width=0.5,color='black') + guides(fill=FALSE) +
  geom_jitter(position=position_jitter(0.1),color='black') + 
  ggtitle(expression(bold("F."))) +
  theme(plot.title = element_text(size = rel(1.5),hjust=0.5)) + 
  ylab(expression(paste("thickness (", mu, "m)"))) +
  annotate(geom="text", x=1, y=2.225, label="A",
           color="black",size=4) +
  annotate(geom="text", x=2, y=1.9, label="BC",
           color="black",size=4) +
  annotate(geom="text", x=3, y=1.95, label="AB",
           color="black",size=4) +
  annotate(geom="text", x=4, y=1.8, label="C",
           color="black",size=4) +
  xlab("") +
  ylim(1.25,2.25) +
  scale_x_discrete(labels=c("O. engelmannii","O. ficus-indica",
                            "O. macrorhiza","O. stricta")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "none",
        plot.title = element_text(size=15,vjust=1,hjust=-0.12),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(face = "italic", angle = 45,hjust=1.0),
        axis.title.y = element_text(margin = margin(t = 20,r = 20,b = 20, l = 20)),
        axis.title.x = element_text(margin = margin(t = 20)),
        plot.margin = margin(t = 0, r = 20, b = -15, l = 0, unit = "pt")) 

thick

#   scale_y_continuous(breaks=c(70.0,80.0, 90.0, 100.0)) +

# -- save each individual plot for the manuscript figure that combines all 4 ##
save(protein,carb,fat,fiber,water,thick,file="fig3_3-27-20_RData")


### ------------------------------------------------------------------------------------
### ------------------------------------------------------------------------------------
### FIGURES 3 & 4: Opuntia host quality plots (2 groups of 3 variable plots)
### ------------------------------------------------------------------------------------
### ------------------------------------------------------------------------------------

names=load("fig3_3-27-20_RData")
names


#install.packages('cowplot')
library(cowplot)
protein
carb
fat
fiber
water
thick


### ---- FIGURE 4
library(ggpubr)
FIG2<-ggarrange(protein,carb,fat,
                fiber,water,thick
                 ,nrow=2,ncol=3)
FIG2

annotate_figure(FIG2,bottom= text_grob("species",
                                  hjust=0.5,size=12,vjust=-0.5))


# annotate_figure(plot1,bottom=text_grob("X-Axis", color = "black",
#                            hjust = 1, x = 1,  size = 10))

# library(gridExtra)
# grid.arrange(protein,carb,fat,fiber,water,thick, 
#             bottom = "Title",ncol=3, nrow=2)


### ------------------------------------------------------------------------------------
### ------------------------------------------------------------------------------------
### CITE R Core Team 
citation()

#To cite R in publications use:

#  R Core Team (2018). R: A language and environment for statistical
# computing. R Foundation for Statistical Computing, Vienna, Austria.
# URL https://www.R-project.org/.


#################################################################################
#################################################################################

