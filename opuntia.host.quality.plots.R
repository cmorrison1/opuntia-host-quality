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
  ylab("protein (% dry mass)") +
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
  ylab("carbohydrate (% dry mass)") +
  ylim(0.0,0.725) +
  scale_x_discrete(labels=c("O. engelmannii","O. ficus-indica",
                            "O. macrorhiza","O. stricta")) +
  annotate(geom="text", x=1, y=0.725, label="A",
           color="black",size=4) +
  annotate(geom="text", x=2, y=0.435, label="B",
           color="black",size=4) +
  annotate(geom="text", x=3, y=0.545, label="A",
           color="black",size=4) +
  annotate(geom="text", x=4, y=0.495, label="A",
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


# plot
library(ggplot2)
fat<-ggplot(fa3, aes(x=species, y=mg.mgAVG,color='')) + 
  geom_boxplot(width=0.5,color='black') + guides(fill=FALSE) +
  geom_jitter(position=position_jitter(0.1),color='black') +
  # geom_point(data=cacto, aes(x=species, y=mg.mgAVG), color='red',size=2) +
  ggtitle(expression(bold("C."))) +
  xlab("") + 
  ylim(0,0.06) + 
  annotate(geom="text", x=1, y=0.06, label="A",
           color="black",size=4) +
  annotate(geom="text", x=2, y=0.033, label="AB",
           color="black",size=4) +
  annotate(geom="text", x=3, y=0.032, label="AB",
           color="black",size=4) +
  annotate(geom="text", x=4, y=0.019, label="B",
           color="black",size=4) +
  scale_x_discrete(labels=c("O. engelmannii","O. ficus-indica",
                            "O. macrorhiza","O. stricta")) +
  theme(plot.title = element_text(size = rel(1.5),hjust=0.5)) + 
  ylab(expression(paste("fatty acid (% dry mass)"))) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "none",
        plot.title = element_text(size=15,vjust=1,hjust=-0.12),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(face = "italic", angle = 45,hjust=1.0,color="white"),
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

# plot
fiber<-ggplot(fib3, aes(x=species, y=concentration,color='')) + 
  geom_boxplot(width=0.5,color='black') + guides(fill=FALSE) +
  geom_jitter(position=position_jitter(0.1),color='black') + 
  ggtitle(expression(bold("D."))) +
  theme(plot.title = element_text(size = rel(1.5),hjust=0.5)) + 
  ylab(expression(paste("fiber (% dry mass)"))) +
  ylim(0,1) + 
  xlab("") + 
  scale_x_discrete(labels=c("O. engelmannii","O. ficus-indica",
                            "O. macrorhiza","O. stricta")) +
  annotate(geom="text", x=1, y=0.78, label="A",
           color="black",size=4) +
  annotate(geom="text", x=2, y=0.46, label="B",
           color="black",size=4) +
  annotate(geom="text", x=3, y=0.51, label="B",
           color="black",size=4) +
  annotate(geom="text", x=4, y=0.75, label="AB",
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
water<-ggplot(h2o3, aes(x=species, y=water.content,color='')) + 
  geom_boxplot(width=0.5,color='black') + guides(fill=FALSE) +
  geom_jitter(position=position_jitter(0.1),color='black') + 
  ggtitle(expression(bold("E."))) +
  theme(plot.title = element_text(size = rel(1.5),hjust=0.5)) + 
  ylab("water content (%)") +
  xlab("") +
  ylim(0.8,1) +
  scale_x_discrete(labels=c("O. engelmannii","O. ficus-indica",
                            "O. macrorhiza","O. stricta")) +
  annotate(geom="text", x=1, y=0.92, label="A",
           color="black",size=4) +
  annotate(geom="text", x=2, y=0.98, label="B",
           color="black",size=4) +
  annotate(geom="text", x=3, y=0.92, label="A",
           color="black",size=4) +
  annotate(geom="text", x=4, y=0.92, label="A",
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
save(protein,carb,fat,fiber,water,thick,file="fig2_6-24-20_RData")


### ------------------------------------------------------------------------------------
### ------------------------------------------------------------------------------------
### FIGURES 3 & 4: Opuntia host quality plots (2 groups of 3 variable plots)
### ------------------------------------------------------------------------------------
### ------------------------------------------------------------------------------------

names=load("fig2_6-24-20_RData")
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



### ----------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------
### FIGURE 3 (for revision 1): Opuntia Nutritional Geometry - protein:carbohydrate rations
### ----------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------

carb=read.csv("carb.csv") # carbohydrates 
prot=read.csv("protein.csv") # protein

carb2<-carb[-c(3,147,216,249),][which(carb[-c(3,147,216,249),]$age == 'N'), 
                                names(carb[-c(3,147,216,249),])
                                %in% c('Sample','species','avg.ug.mg')]
prot2<-prot[-c(3,147,216,249),][which(prot[-c(3,147,216,249),]$age == 'N'), 
                                names(prot[-c(3,147,216,249),]) 
                                %in% c('Sample','species','avg.ug.mg')]
pc<-cbind.data.frame(prot2,carb2)
pc<-pc[-c(4,5)]
names(pc)[1]<-"sample"
names(pc)[3]<-"protein"
names(pc)[4]<-"carbohydrate"
pc$protein<-as.numeric(as.character(pc$protein))
pc$carbohydrate<-as.numeric(as.character(pc$carbohydrate))

### --- Subsetting for all FOUR Opuntia Species 
levels(pc$species)
# [1] "engelmannii"  "ficus-indica" "macrorhiza"   "stricta"
eng<-pc[which(pc$species == 'engelmannii'), names(pc) %in% c('sample','species','protein','carbohydrate')]
ficus<-pc[which(pc$species == 'ficus-indica'), names(pc) %in% c('sample','species','protein','carbohydrate')]
macro<-pc[which(pc$species == 'macrorhiza'), names(pc) %in% c('sample','species','protein','carbohydrate')]
strict <-pc[which(pc$species == 'stricta'), names(pc) %in% c('sample','species','protein','carbohydrate')]
### 

### make new dataframe with total macronutrients (carbs + protein)
macronutrient<-(pc$protein+pc$carbohydrate)
protein<-pc$protein
carbohydrate<-pc$carbohydrate
pc2<-cbind.data.frame(pc,protein,carbohydrate,macronutrient)
head(pc2)
pc3<-pc2[,c(1,2,5,6,7)]
### summary statistics for the bar chart 
library(plyr)
pc4<-na.omit(pc3)
macros <- ddply(pc4, c("species"), summarise,
                Nmacro    = length(macronutrient),
                Nprot    = length(protein),
                Ncarb    = length(carbohydrate),
                meanMacro = mean(macronutrient),
                meanProt = mean(protein),
                meanCarb = mean(carbohydrate),
                sdMacro   = sd(macronutrient),
                sdProt   = sd(protein),
                sdCarb   = sd(carbohydrate),
                seMacro   = sdMacro / sqrt(Nmacro),
                seProt   = sdProt / sqrt(Nprot),
                seCarb   = sdCarb / sqrt(Ncarb)
)
macros


### --- Subsetting for Travis & Matagorda County O. engelmannii Samples
travis=eng[grepl("^(B|W)",eng$sample),]
travis<-na.omit(travis)
county<-c("Travis Co.","Travis Co. Mel","Travis Co.","Travis Co. Mel","Travis Co.",
          "Travis Co. Mel","Travis Co.","Travis Co. Mel","Travis Co.","Travis Co. Mel",
          "Travis Co.","Travis Co. Mel", "Travis Co.","Travis Co. Mel","Travis Co.",
          "Travis Co. Mel","Travis Co.","Travis Co. Mel","Travis Co.","Travis Co. Mel",
          "Travis Co.","Travis Co.","Travis Co.","Travis Co. Mel")
travis<-cbind.data.frame(travis,county)
mata=eng[grepl("^M(N|C)",eng$sample),]
mata<-na.omit(mata)
county<-c("Matagorda Co. Cacto","Matagorda Co. Cacto","Matagorda Co. Cacto",
          "Matagorda Co. Cacto","Matagorda Co. Cacto","Matagorda Co. Cacto",
          "Matagorda Co.","Matagorda Co.","Matagorda Co.","Matagorda Co.",
          "Matagorda Co.","Matagorda Co.","Matagorda Co.")
mata<-cbind.data.frame(mata,county)
pops<-rbind.data.frame(travis,mata)
pops

Ntrav=travis[grepl("^(B|W)N",travis$sample),]
Mtrav=travis[grepl("^(B|W)M",travis$sample),]
Nmata=mata[grepl("^MN",mata$sample),]
Cmata=mata[grepl("^MC",mata$sample),]
herbs<-rbind.data.frame(Ntrav,Mtrav,Nmata,Cmata)
herbs
###

### make new dataframe with total macronutrients (carbs + protein)
macronutrient<-(pops$protein+pops$carbohydrate)
protein<-pops$protein
carbohydrate<-pops$carbohydrate
pops2<-cbind.data.frame(pops,protein,carbohydrate,macronutrient)
head(pops2)
pops3<-pops2[,c(1,2,5,6,7,8)]
### summary statistics for the bar chart 
macros2 <- ddply(pops3, c("county"), summarise,
                 Nmacro    = length(macronutrient),
                 Nprot    = length(protein),
                 Ncarb    = length(carbohydrate),
                 meanMacro = mean(macronutrient),
                 meanProt = mean(protein),
                 meanCarb = mean(carbohydrate),
                 sdMacro   = sd(macronutrient),
                 sdProt   = sd(protein),
                 sdCarb   = sd(carbohydrate),
                 seMacro   = sdMacro / sqrt(Nmacro),
                 seProt   = sdProt / sqrt(Nprot),
                 seCarb   = sdCarb / sqrt(Ncarb)
)
macros2


### --- Panel A: Opuntia species comparison (nutritional rails)
par(mfrow=c(1,2))
plot(carbohydrate~protein, data=eng,
     ylab="carbohydrate (% dry mass)",
     xlab="",
     ylim=c(0.01,0.8),xlim=c(0.01,0.3),pch=19,col="#440154FF")
points(carbohydrate~protein, data=ficus,pch=19,col="coral2")
points(carbohydrate~protein, data=macro,pch=19,col="#9FDA3AFF")
points(carbohydrate~protein, data=strict,pch=19,col="#35B779FF")
abline(a=0, b=0.2,lty=2,lwd=2,col="grey60")
abline(a=0, b=1.0,lty=2,lwd=2,col="black")
abline(a=0, b=2.0,lty=2,lwd=2,col="grey60")
abline(a=0, b=4.0,lty=2,lwd=2,col="grey60")
abline(a=0, b=8.0,lty=2,lwd=2,col="grey60")
text(x=0.23,y=0.018,labels="C:P = 0.2",cex=1.1,col="grey60")
text(x=0.24,y=0.2,labels="C:P = 1",cex=1.1,col="black")
text(x=0.235,y=0.41,labels="C:P = 2",cex=1.1,col="grey60")
text(x=0.185,y=0.63,labels="C:P = 4",cex=1.1,col="grey60")
text(x=0.115,y=0.71,labels="C:P = 8",cex=1.1,col="grey60")
legend('topright', 
       legend=c("O. engelmannii","O. ficus-indica","O. macrorhiza","O. stricta"), 
       pch=c(19,19,19,19),col = c("#440154FF", "coral2", "#9FDA3AFF", "#35B779FF"), 
       cex=1.0,text.font=3)
mtext('A.',side=1,line=-24.0,adj=0,
      outer = FALSE,cex=1.5,font=2)

### --- Panel B: Travis vs Matagorda O. engelmannii population comparison (nutritional rails)
plot(carbohydrate~protein, data=Ntrav,
     ylab="",
     xlab="",
     ylim=c(0.01,0.8),xlim=c(0.01,0.3),pch=1,lwd=2,col="blue")
points(carbohydrate~protein, data=Mtrav,pch=19,col="blue")
points(carbohydrate~protein, data=Nmata,pch=1,lwd=2,col="red")
points(carbohydrate~protein, data=Cmata,pch=19,col="red")
abline(a=0, b=0.2,lty=2,lwd=2,col="grey60")
abline(a=0, b=1.0,lty=2,lwd=2,col="black")
abline(a=0, b=2.0,lty=2,lwd=2,col="grey60")
abline(a=0, b=4.0,lty=2,lwd=2,col="grey60")
abline(a=0, b=8.0,lty=2,lwd=2,col="grey60")
text(x=0.23,y=0.018,labels="C:P = 0.2",cex=1.1,col="grey60")
text(x=0.24,y=0.2,labels="C:P = 1",cex=1.1,col="black")
text(x=0.235,y=0.41,labels="C:P = 2",cex=1.1,col="grey60")
text(x=0.185,y=0.63,labels="C:P = 4",cex=1.1,col="grey60")
text(x=0.115,y=0.71,labels="C:P = 8",cex=1.1,col="grey60")
legend('topright', 
       legend=c("Travis Co.","Matagorda Co."), 
       pch=c(19,19),col = c("blue", "red"), 
       cex=1.0,text.font=1)
mtext('B.',side=1,line=-24.0,adj=0,
      outer = FALSE,cex=1.5,font=2)
mtext('protein (% dry mass)',side=1,line=3.0,adj=-0.4,
      outer = FALSE,cex=1.0,font=1)


### --- Panel C: Opuntia species comparison (macronutrient content {%protein+%carbohydrate})
# subset the macronutrient groups to plot in 'grouped' barplots
x<-macros[,c("meanCarb","meanProt")]
x<-t(x)
# create vector of SEs for each host plant
ses<-c(0.0208,0.0548,0.0172,0.0188)
# save macronutrient means as base scaffold to overlay SEs on
barCenters <- barplot(macros$meanMacro,ylim = c(0,100),col="white")

par(mfrow=c(1,2))
barplot(x,ylim = c(0,1),col = c("white", "grey"),
        names.arg = c(expression(italic("O. engelmannii"), italic("O. ficus-indica"), 
                                 italic("O. macrorhiza"),italic("O. stricta"))),
        border = c("black", "black", "black","black"),
        ylab = "macronutrient (% dry mass)",xlab = "species")
# add the error bars to the grouped barchard
arrows(barCenters, macros$meanMacro-ses, barCenters, 
       macros$meanMacro+ses, lwd=1, angle=90, code=3)
text(x=0.7,y=0.5,labels ='A',text.font=2,cex=1.2)
text(x=1.9,y=0.45,labels ='A',text.font=2,cex=1.2)
text(x=3.1,y=0.5,labels ='A',text.font=2,cex=1.2)
text(x=4.3,y=0.45,labels ='A',text.font=2,cex=1.2)
legend('topright', 
       legend=c("% protein","% carbohydrate"), 
       pch=c(22,22), pt.bg = c("grey","white"), col = c("black","black"),
       cex=1.0,text.font=1)
mtext('C.',side=1,line=-24.0,adj=0,
      outer = FALSE,cex=1.5,font=2)
box()


### --- Panel D: Travis vs Matagorda O. engelmannii pop comparison {%protein+%carbohydrate}
# subset the macronutrient groups to plot in 'grouped' barplots
x2<-macros2[,c("meanCarb","meanProt")]
x2<-t(x2)
# create vector of SEs for each host plant
ses2<-c(0.0389,0.0393,0.0302,0.0349)
# save macronutrient means as base scaffold to overlay SEs on
barCenters2 <- barplot(macros2$meanMacro,ylim = c(0,1),col="white")
# plot
barplot(x2,ylim = c(0,1),col = c("white", "grey"),
        names.arg = c("no larvae",expression(italic("Melitara")),
                      "no larvae",expression(italic("Cactoblastis"))),
        border = c("black", "black"),
        ylab = "",xlab = "herbivore presence")
# add the error bars to the grouped barchard
arrows(barCenters2, macros2$meanMacro-ses2, barCenters2, 
       macros2$meanMacro+ses2, lwd=1, angle=90, code=3)
# line segments and text
abline(v=2.5,lty=3,lwd=2,col="black")
text(x=1.4,y=0.74,labels =expression(bold('Travis County')),cex=0.9,col="gray60")
text(x=1.4,y=0.71,labels =expression(bold("population")),text.font=2,cex=0.9,col="gray60")
text(x=3.7,y=0.74,labels =expression(bold('Matagorda County')),text.font=2,cex=0.9,col="gray60")
text(x=3.7,y=0.71,labels =expression(bold("population")),text.font=2,cex=0.9,col="gray60")
segments(x0=1.4,y0=0.84,x1=3.7,y1=0.84,lwd=2,col="gray60")
segments(x0=1.4,y0=0.84,x1=1.4,y1=0.81,lwd=2,col="gray60")
segments(x0=3.7,y0=0.84,x1=3.7,y1=0.81,lwd=2,col="gray60")
text(x=1.4,y=0.78,labels =expression(bold('A')),cex=1.1,col="gray60")
text(x=3.7,y=0.78,labels =expression(bold('B')),cex=1.1,col="gray60")
segments(x0=0.7,y0=0.52,x1=1.9,y1=0.52,lwd=1.5,col="black")
segments(x0=0.7,y0=0.52,x1=0.7,y1=0.5,lwd=1.5,col="black")
segments(x0=1.9,y0=0.52,x1=1.9,y1=0.46,lwd=1.5,col="black")
text(x=1.4,y=0.54,labels ='n.s.',text.font=2,cex=0.9)
segments(x0=3.1,y0=0.61,x1=4.3,y1=0.61,lwd=1.5,col="black")
segments(x0=3.1,y0=0.61,x1=3.1,y1=0.59,lwd=1.5,col="black")
segments(x0=4.3,y0=0.61,x1=4.3,y1=0.5,lwd=1.5,col="black")
text(x=3.7,y=0.63,labels ='n.s.',text.font=2,cex=0.9)
legend('topright', 
       legend=c("% protein","% carbohydrate"), 
       pch=c(22,22), pt.bg = c("grey","white"), col = c("black","black"),
       cex=1.0,text.font=1)
mtext('D.',side=1,line=-24.0,adj=0,
      outer = FALSE,cex=1.5,font=2)
box()




### Save all these data objects for easy recall later
save(pc,pc2,pc3,pc4,macros,macros2,pops,pops2,pops3,
     Ntrav,Mtrav,Nmata,Cmata,travis,mata,eng,macro,
     ficus,strict,pops,file='geometry.data_6-24-20_RData')




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

