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



### read dataframes into memory 
cut=read.csv("cuticle.csv") # cuticle thickness
FA=read.csv("fat.csv") # fatty acid
carb=read.csv("carb.csv") # carbohydrates 
prot=read.csv("protein.csv") # protein
fib=read.csv("fiber.csv") # fiber 
h2o=read.csv("water.csv") # water content 
# 
### Save nutient Dfs with outliers removed in one file for easy uploading to other scripts
# cut<-cut[-c(12,15),] 
# FA<-FA[-c(3),]
# carb=read.csv("carb.csv")
# prot<-prot[-c(3,147,216,249),]
#fib<-fib[-c(16,21,90,91,92,93,95,96),]
# h2o=read.csv("water.csv")
cut2<-cut[-c(12,15),][which(cut[-c(12,15),]$age == 'N'), names(cut[-c(12,15),]) 
                      %in% c('Sample','pair','herbivores','thickAVG')]
FA2<-FA[-c(3),][which(FA[-c(3),]$age == 'N'), names(FA[-c(3),]) 
                %in% c('sample','pair','herbivores','mg.mgAVG')]
carb2<-carb[which(carb$age == 'N'), names(carb) 
            %in% c('Sample','pair','herbivores','avg.ug.mg')]
prot2<-prot[-c(3,147,216,249),][which(prot[-c(3,147,216,249),]$age == 'N'), 
                                names(prot[-c(3,147,216,249),]) 
                                %in% c('Sample','pair','herbivores','avg.ug.mg')]
fib2<-fib[-c(16,21,90,91,92,93,95,96),][which(fib[-c(16,21,90,91,92,93,95,96),]$age == 'N'),
                                        names(fib[-c(16,21,90,91,92,93,95,96),]) 
                                        %in% c('Sample','pair','herbivore','tissue.content')]
h2o2<-h2o[which(h2o$age == 'N'), names(h2o) 
          %in% c('unique','pair','herbivores','water.content')]

save(cut2,FA2,carb2,prot2,fib2,h2o2,file="hosts_2-14-20_RData") #


### --- Assess data frequency distribution, normality, and outliers 
#install.packages("goftest")
library(goftest)
ad.test()



### ------------------------------------------------------------------------------------
### ------------ Cuticle Thickness 
hist(cut$thickAVG)  # reverse j-shaped (heavy right skew) -> GAMMA???
c<-na.omit(cut$thickAVG)
ad.test(c) # p-value = 1.622e-05 -> not normally distributed 
qqpoints<-qqnorm(cut$thickAVG)
qqline(cut$thickAVG)
identify(results)
identify(qqpoints)
qqpoints$y[12] # 3.111111 BFLoe01
qqpoints$y[15] # 2.838889 BFLoe02
qqpoints$y[18] # 2.333333 BFLoe03 ------> These outliers are all from the same population.
# samples representing 12,15, and 18 will be deleted from analyses and visualizations


### ------------------------------------------------------------------------------------
### ---------------- Fatty Acids 
FA$mg.mgAVG<-as.numeric(as.character(FA$mg.mgAVG))
hist(FA$mg.mgAVG) # GAUSSIAN DIST.
f<-na.omit(FA$mg.mgAVG)
ad.test(f) # p-value = 8.571e-06 -> not normally distributed
qqpoints<-qqnorm(FA$mg.mgAVG,xlim=c(-3,3),ylim=c(0,0.08))
qqline(FA$mg.mgAVG)
identify(results)
identify(qqpoints)
# serious outlier, not even close
qqpoints$y[3] # 0.07408012 cycad3
# borderline outliers - no reason to think these are atypical
qqpoints$y[78] # 0.004338482 BFLOM03-1
qqpoints$y[72] # 0.005017428 BFLOM02-1
qqpoints$y[51] # 0.00775814 BM08-3
qqpoints$y[63] # 0.00868449 BM10-3
qqpoints$y[24] # 0.05444127 BN04-3
qqpoints$y[18] # 0.05428144 BN03-3
qqpoints$y[27] # 0.05118172 BM04-3
qqpoints$y[21] # 0.04856635 BM03-3
qqpoints$y[12] # 0.04446624 BM02-1

FA2<-FA[-c(3),] # outliers See previous annotation
plot(FA2$mg.mgAVG~FA2$species)
hist(FA2$mg.mgAVG) # reverse J-shape, much tigher than before
# just new cladodes
FA3<-FA2[which(FA2$age == 'N'), names(FA2) 
         %in% c('Sample','species','herbivores','sample','dry.mg','mg.mg','mg.mgAVG')]


### Cleveland Plot of O. engelmannii samples
# This is the most variable Opuntia sp. assayed
eng<-FA3[which(FA3$species == 'engelmannii'), names(FA3) 
         %in% c('Sample','species','herbivores','sample','dry.mg','mg.mg','mg.mgAVG')]
eng2<-na.omit(eng)

library(ggplot2)
cleve<-ggplot(eng2, aes(mg.mgAVG, sample)) +
  geom_point() + 
  ggtitle("Cleveland Plot: O. engelmannii Samples") +
  xlab("fatty acid (mg/mg dry wt)"
) 
cleve


### ------------------------------------------------------------------------------------
### ---------------- Carbohydrates 
carb$avg.ug.mg<-as.numeric(as.character(carb$avg.ug.mg))
hist(carb$avg.ug.mg) #  GAUSSIAN DIST. 
c2<-na.omit(carb$avg.ug.mg)
ad.test(c2) # p-value = 8.451e-06 -> not normally distributed
qqpoints<-qqnorm(carb$avg.ug.mg)
qqline(carb$avg.ug.mg)
identify(results)
identify(qqpoints)
# borderline outliers (distribution is a bell curve)
qqpoints$y[147] # 677.6482 -> WM01
qqpoints$y[24] # 643.6499 -> BN04
qqpoints$y[90] # 73.12882 -> TFI01
qqpoints$y[87] # 46.55053 -> WFI01


### ------------------------------------------------------------------------------------
### --------------- Fiber 
# fib<-fib[,c(1,2,3,5,10)]

fib$concentration<-as.numeric(as.character(fib$concentration))
hist(fib$concentration) # reverse j-shaped (heavy right skew) -> GAMMA??? 
f2<-na.omit(fib$concentration)
ad.test(f2) # p-value = 7.595e-06 -> not normally distributed
qqpoints<-qqnorm(fib$concentration)
qqline(fib$concentration)
identify(results)
identify(qqpoints)
qqpoints$y[21] # 1.756 BN08
qqpoints$y[92] #  1.01 MN05
qqpoints$y[93] # 0.957 MC05
qqpoints$y[96] # 0.931 MN07
qqpoints$y[95] # 0.914 MC06
qqpoints$y[91] # 0.841 MC04
qqpoints$y[16] #  0.815 BM05
qqpoints$y[90] # 0.839 MN04
## samples 16,21,91,92,93,95,96 were not processed correctly -> unasable data -> OMIT from analysis & plots 

fib2<-fib[-c(16,21,90,91,92,93,95,96),] # outliers See previous annotation
hist(fib2$concentration) # reverse J-shape, much tigher than before 


### ------------------------------------------------------------------------------------
### --------------- Water 
h2o$water.content<-as.numeric(as.character(h2o$water.content))
hist(h2o$water.content)  #  GAUSSIAN DIST.
h<-na.omit(h2o$water.content)
ad.test(h) # p-value = 1.714e-05 -> not normally distributed 
qqpoints<-qqnorm(h2o$water.content, ylim = c(0.7,1.0),xlim=c(-3,2.5))
h2o
qqline(h2o$water.content)
identify(results)
identify(qqpoints)
qqpoints$y[71] # 0.7210265


### ------------------------------------------------------------------------------------
### --------------- Protein 
prot$avg.ug.mg<-as.numeric(as.character(prot$avg.ug.mg))  #  GAUSSIAN DIST.
hist(prot$avg.ug.mg) #  GAUSSIAN DIST.
p<-prot$avg.ug.mg
p2<-na.omit(p)
ad.test(p2) # p-value = 8.451e-06 -> not normally distributed 
qqpoints<-qqnorm(prot$avg.ug.mg, pch = 1, frame = FALSE,ylim=c(0,0.4),xlim=c(-3,3))
qqline(prot$avg.ug.mg, col = "steelblue", lwd = 2)
# look at outliers 
identify(results)
identify(qqpoints)
qqpoints$y[216] # 0.3844156 sos04
qqpoints$y[3]  # 0.3617406 cycad3
qqpoints$y[147] # 0.2588779 WM03HIGH
qqpoints$y[249] # 0.2333015 MC05

prot2<-prot[-c(3,147,216,249),]
nrow(prot2) # 274
nrow(prot) # 276
par(mfrow=c(1,2))
hist(prot$avg.ug.mg)
hist(prot2$avg.ug.mg)










#################################################################################
#################################################################################
