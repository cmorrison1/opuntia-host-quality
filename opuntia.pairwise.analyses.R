####################################################################################################
####################################################################################################
################### Script for Opuntia Host Quality Pairwise Comparison Plots ####################
####################################################################################################
####################################################################################################


# Colin Richard Morrison 
# PhD Candidate
# The University of Texas at Austin 
# Department of Integrative Biology 
# Graduate Program In Ecology, Evolution and Behavior
# crmorrison@utexas.edu


### ------------------------------------------------------------------------------------
### ------------------------------------------------------------------------------------
### Pairwise comparison of non-infested and infested Opuntia engelmannii individuals  
### ------------------------------------------------------------------------------------
### ------------------------------------------------------------------------------------



getwd()
setwd("~/Desktop/oppuntia.cactoblastis/Oppuntia Host Quality DBs")



### ------------------------------------------------------------------------------------
### ------------------------------------------------------------------------------------
### ---------------------  SUBSETTING FOR ANALYSIS -------------------------------------
### ------------------------------------------------------------------------------------
### ------------------------------------------------------------------------------------

### --- Read data into memory
### cleaned up master DFs with all host quality variables 

FA=read.csv("fat.csv") # fatty acid
carb=read.csv("carb.csv") # carbohydrates 
prot=read.csv("protein.csv") # protein
fib=read.csv("fiber.csv") # fiber 
h2o=read.csv("water.csv") # water content 

## --- Subset for New Cladodes only
### ------------------------------------------------------------------------------------
FA2<-FA[-c(3),][which(FA[-c(3),]$age == 'N'), names(FA[-c(3),]) 
                %in% c('sample','pair','herbivores','mg.mgAVG')]
carb2<-carb[which(carb$age == 'N'), names(carb) 
            %in% c('Sample','pair','herbivores','avg.ug.mg')]
prot2<-prot[-c(3,216),][which(prot[-c(3,216),]$age == 'N'), 
                        names(prot[-c(3,216),]) 
                        %in% c('Sample','pair','herbivores','avg.ug.mg')]
fib2<-fib[which(fib$age == 'N'),names(fib) 
          %in% c('Sample','pair','herbivore','tissue.content')]
h2o2<-h2o[which(h2o$age == 'N'), names(h2o) 
          %in% c('unique','pair','herbivores','water.content')]

save(cut2,FA2,carb2,prot2,fib2,h2o2,file="hosts_2-17-20_RData") #

### --- Subset for particular population
### ------------------------------------------------------------------------------------

### --- MELITARA - Travis County O. engelmannii Population --- ###
### subset individual DF for only non-infested and infested samples Bauerle O. engelmannii samples
Ncarb=carb2[grepl("^(B|W)N",carb2$Sample),]
Mcarb=carb2[grepl("^(B|W)M",carb2$Sample),]
carb3=rbind.data.frame(Ncarb,Mcarb)
names(carb3)[4]<-"carbohydrate" # carbohydrate concentration in (ug/mg)  
carb3
#
Nfa=FA2[grepl("^(B|W)N",FA2$sample),]
Mfa=FA2[grepl("^(B|W)M",FA2$sample),]
FA3=rbind.data.frame(Nfa,Mfa)
names(FA3)[1]<-"Sample"
names(FA3)[4]<-"fatty.acid"
FA3<-FA3[,-c(2:3)]
FA3
#
Nprot=prot2[grepl("^(B|W)N",prot2$Sample),]
Mprot=prot2[grepl("^(B|W)M",prot2$Sample),]
prot3=rbind.data.frame(Nprot,Mprot)
names(prot3)[4]<-"protein" # protein concentration in (ug/mg)   
prot3<-prot3[,-c(2:3)]
prot3
#
Nfib=fib2[grepl("^(B|W)N",fib2$Sample),]
Mfib=fib2[grepl("^(B|W)M",fib2$Sample),]
fib3=rbind.data.frame(Nfib,Mfib)
fib3
nrow(fib.samps) # 22
fib.samps<-c("BN01-3","BN02-3","BN03-3","BN04-3","BN05-3","BN06-3","BN07-3","BN08-3","BN09-3",
             "BN10-3","WN01HIGH_3","WN02HIGH_3","WN03HIGH_3","BM01-3","BM02-3",
             "BM03-3","BM04-3","BM05-3","BM06-3","BM07-3","BM08-3","BM09-3","BM10-3",
             "WM01HIGH_3","WM02HIGH_3","WM03HIGH_3")
fib4<-cbind(fib.samps,fib3)
fib4<-fib4[,-c(2:4)]
names(fib4)[1]<-"Sample"
names(fib4)[2]<-"fiber" 
fib4
#
colnames(h2o2)
Nh2o=h2o2[grepl("^(B|W)N",h2o2$unique),]
Mh2o=h2o2[grepl("^(B|W)M",h2o2$unique),]
h2o3=rbind.data.frame(Nh2o,Mh2o)
names(h2o3)[4]<-"water" 
h2o3
h2o.samps<-c("BN01-3","BN02-3","BN03-3","BN04-3","BN05-3","BN06-3","BN07-3","BN08-3",
             "BN09-3","BN10-3","WN01HIGH_3","WN02HIGH_3","WN03HIGH_3","BM01-3","BM02-3",
             "BM03-3","BM04-3","BM05-3","BM06-3","BM07-3","BM08-3","BM09-3","BM10-3",
             "WM01HIGH_3","WM02HIGH_3","WM03HIGH_3")
h2o4<-cbind(h2o.samps,h2o3)
names(h2o4)[1]<-"Sample" 
h2o4<-h2o4[,-c(2:4)]
h2o4
#
Ncut=cut2[grepl("^(B|W)N",cut2$Sample),]
Mcut=cut2[grepl("^(B|W)M",cut2$Sample),]
cut3=rbind.data.frame(Ncut,Mcut)
cut3<-cut3[,-c(2:3)]
names(cut3)[2]<-"cuticle" # carbohydrate concentration in (ug/mg)  
cut3
#

### combine new variable objects into master MELITARA DF for analyses and plots.
master1<-merge(carb3,FA3, by = "Sample", all = TRUE)
master2<-merge(master1,prot3, by = "Sample", all = TRUE)
master3<-merge(master2,fib4, by = "Sample", all = TRUE)
master4<-merge(master3,h2o4, by = "Sample", all = TRUE)
master<-merge(master4,cut3, by = "Sample", all = TRUE)

colnames(master)
names(master)[1]<-"sample"
master
# save these data for easy upload later in the workflow
save(carb3,fa3,prot3,fib4,h2o4,cut3,master,file="meldata_2-17-20_RData")

master$sample<-as.factor(master$sample)
master$pair<-as.factor(master$pair)
master$herbivores<-as.factor(master$herbivores)
master$carbohydrate<-as.numeric(as.character(master$carbohydrate))
master$fatty.acid<-as.numeric(as.character(master$fatty.acid))
master$protein<-as.numeric(as.character(master$protein))
master$fiber<-as.numeric(as.character(master$fiber))
master$water<-as.numeric(as.character(master$water))
master$cuticle<-as.numeric(as.character(master$cuticle))
# omit rows with all NAs, but keep rows with some NAs
nrow(master)# 78
master2<-master[rowSums( is.na(master) ) <=4, ]
nrow(master2) # 27 
# assign vector of simple clean names to each observation 
row.names(master2)<-c("BM01","BM02","BM03","BM04","BM05","BM06",
                      "BM07","BM08","BM09","BM10","BN01","BN02","BN03","BN04","BN05","BN06",
                      "BN07","BN08","BN09","BN10","WMO1","WMO2","WM03","WN01","WN02","WN03")
row.names<-c(row.names(master2))
master2<-master2[,-c(1)] # remove old sample names 
master2
nrow(master2) # 26
### subset dataset for active indivuals and quantitative variables to be analyzed
# this is the dataframe that will be fed to the PCA and correlation functions
master.active<-master2[,3:7]
# alter names for aesthetics on graphs
names(master.active)[1]<-"[carbohydrate]"
names(master.active)[2]<-"[fatty acid]"
names(master.active)[3]<-"[protein]"
names(master.active)[4]<-"% fiber"
names(master.active)[5]<-"% water"
master.active
# replace lower case melitara/no melitara strings with capitolized words for plot
head(master2)
master2$herbivores <- gsub('melitara', 'Melitara', master2$herbivores)
master2$herbivores <- gsub('no', 'no Melitara', master2$herbivores)
master2$herbivores
# save files again 
save(carb3,FA3,prot3,fib4,h2o4,cut3,
     master,master2,master.active,file="mel.comp_2-17-20_RData")



### --- C. cactorum from Matagorda County O. engelmannii population
### ------------------------------------------------------------------------------------
NCcarb=carb2[grepl("^MN",carb2$Sample),]
Ccarb=carb2[grepl("^MC",carb2$Sample),]
Ccarb3=rbind.data.frame(NCcarb,Ccarb)
names(Ccarb3)[4]<-"carbohydrate" # carbohydrate concentration in (ug/mg)  
Ccarb3
#
NCfa=FA2[grepl("^MN",FA2$sample),]
Cfa=FA2[grepl("^MC",FA2$sample),]
CFA3=rbind.data.frame(NCfa,Cfa)
names(CFA3)[1]<-"Sample"
names(CFA3)[4]<-"fatty.acid"
CFA3<-CFA3[,-c(2:3)]
CFA3
#
NCprot=prot2[grepl("^MN",prot2$Sample),]
Cprot=prot2[grepl("^MC",prot2$Sample),]
Cprot3=rbind.data.frame(NCprot,Cprot)
names(Cprot3)[4]<-"protein" # protein concentration in (ug/mg)   
Cprot3<-Cprot3[,-c(2:3)]
Cprot3
#
NCfib=fib2[grepl("^MN",fib2$Sample),]
Cfib=fib2[grepl("^MC",fib2$Sample),]
Cfib3=rbind.data.frame(NCfib,Cfib)
Cfib3
nrow(Cfib3) # 14
Cfib.samps<-c("MN01-3","MN02-3","MN03-3","MN04-3","MN05-3","MN06-3","MN07-3",
              "MC01-3","MC02-3","MC03-3","MC04-3","MC05-3","MC06-3","MC07-3")
Cfib4<-cbind(Cfib.samps,Cfib3)
Cfib4<-Cfib4[,-c(2:4)]
names(Cfib4)[1]<-"Sample"
names(Cfib4)[2]<-"fiber" 
Cfib4
#
NCh2o=h2o2[grepl("^MN",h2o2$unique),]
Ch2o=h2o2[grepl("^MC",h2o2$unique),]
Ch2o3=rbind.data.frame(NCh2o,Ch2o)
names(Ch2o3)[4]<-"water" 
Ch2o3
nrow(Ch2o3) # 14
Ch2o.samps<-c("MN01-3","MN02-3","MN03-3","MN04-3","MN05-3","MN06-3","MN07-3",
              "MC01-3","MC02-3","MC03-3","MC04-3","MC05-3","MC06-3","MC07-3")
Ch2o4<-cbind(Ch2o.samps,Ch2o3)
names(Ch2o4)[1]<-"Sample" 
Ch2o4<-Ch2o4[,-c(2:4)]
Ch2o4
#
NCcut=cut2[grepl("^MN",cut2$Sample),]
Ccut=cut2[grepl("^MC",cut2$Sample),]
Ccut3=rbind.data.frame(NCcut,Ccut)
Ccut3<-Ccut3[,-c(2:3)]
names(Ccut3)[2]<-"cuticle" # carbohydrate concentration in (ug/mg)  
Ccut3
#
### combine into master CACTOBLASTIS DF for analyses and plotting.
Cmaster1<-merge(Ccarb3,CFA3, by = "Sample", all = TRUE)
Cmaster2<-merge(Cmaster1,Cprot3, by = "Sample", all = TRUE)
Cmaster3<-merge(Cmaster2,Cfib4, by = "Sample", all = TRUE)
Cmaster4<-merge(Cmaster3,Ch2o4, by = "Sample", all = TRUE)
Cmaster<-merge(Cmaster4,Ccut3, by = "Sample", all = TRUE)
names(Cmaster)[1]<-"sample"
Cmaster

# save these data for easy upload later in the workflow
save(Ccarb3,CFA3,Cprot3,Cfib4,Ch2o4,Ccut3,Cmaster,file="cacdata_2-17-20_RData")

# set variables as factorial or numeric
Cmaster$sample<-as.factor(Cmaster$sample)
Cmaster$pair<-as.factor(Cmaster$pair)
Cmaster$herbivores<-as.factor(Cmaster$herbivores)
Cmaster$carbohydrate<-as.numeric(as.character(Cmaster$carbohydrate))
Cmaster$fatty.acid<-as.numeric(as.character(Cmaster$fatty.acid))
Cmaster$protein<-as.numeric(as.character(Cmaster$protein))
Cmaster$fiber<-as.numeric(as.character(Cmaster$fiber))
Cmaster$water<-as.numeric(as.character(Cmaster$water))

# omit rows with all NAs, but keep rows with some NAs
nrow(Cmaster)# 42
Cmaster2<-Cmaster[rowSums( is.na(Cmaster) ) <=4, ]
nrow(Cmaster2) # 14 
# assign vector of simple clean names to each observation 
row.names(Cmaster2)<-c("MC01","MC02","MC03","MC04","MC05","MC06","MC07",
                       "MN01","MN02","MN03","MN04","MN05","MN06","MN07")
row.names<-c(row.names(Cmaster2))
Cmaster2<-Cmaster2[,-c(1)] # remove old sample names 
Cmaster2
nrow(Cmaster2) # 14
### subset dataset for active indivuals and quantitative variables to be analyzed
Cmaster.active<-Cmaster2[,3:7]
# alter names for aesthetics on graphs
names(Cmaster.active)[1]<-"[carbohydrate]"
names(Cmaster.active)[2]<-"[fatty acid]"
names(Cmaster.active)[3]<-"[protein]"
names(Cmaster.active)[4]<-"% fiber"
names(Cmaster.active)[5]<-"% water"
Cmaster.active
# replace lower case melitara/no melitara strings with capitolized words for plot
head(Cmaster2)
Cmaster2$herbivores <- gsub('cactoblastis', 'Cactoblastis', Cmaster2$herbivores)
Cmaster2$herbivores <- gsub('no cactoblastis', 'no Cactoblastis', Cmaster2$herbivores)
Cmaster2$herbivores
# save files again 
save(Ccarb3,CFA3,Cprot3,Cfib4,Ch2o4,Ccut3,
     Cmaster,Cmaster2,Cmaster.active,file="cac.comp_2-17-20_RData")


### ------------------------------------------------------------------------------------
### ---- Melitara VS Travis County O. englemannii DFs
### Subset the data for all pairwise analyses of host quality - MELITARA occupancy

row.names<-c("BM01","BM02","BM03","BM04","BM05","BM06",
             "BM07","BM08","BM09","BM10","BN01","BN02","BN03","BN04","BN05","BN06",
             "BN07","BN08","BN09","BN10","WMO1","WMO2","WM03","WN01","WN02","WN03")
nrow(master2)
pair<-cbind.data.frame(row.names,master2)
head(pair)
names(pair)[1]<-"sample"

### subset values for non-infested and infested MELITARA samples
N=pair[grepl("^(B|W)N",pair$sample),]
nrow(N) # 13
M=pair[grepl("^(B|W)M",pair$sample),]
nrow(M) # 13
### combine subset df objects for the grouped barchart plots
comp<-rbind.data.frame(N,M)
head(comp)
nrow(comp) # 26

# save subset objects for later 
save(pair,N,M,comp,row.names,master2, file="mel.pair.comp_RData")



### ------------------------------------------------------------------------------------
### ---- C. cactorum VS Matagorda County O. englemannii DFs
### Subset the data for  pairwise analyses of Matagorda population host quality - CACTOBLASTIS occupancy

Crow.names<-c("MC01","MC02","MC03","MC04","MC05","MC06","MC07",
              "MN01","MN02","MN03","MN04","MN05","MN06","MN07")
nrow(Cmaster2)
Cpair<-cbind.data.frame(Crow.names,Cmaster2)
head(Cpair)
names(Cpair)[1]<-"sample"

### subset values for non-infested and infested CACTOBLASTIS samples
NC=Cpair[grepl("^MN",Cpair$sample),]
nrow(NC) # 7
C=Cpair[grepl("^MC",Cpair$sample),]
nrow(C) # 7
### combine subset df objects for the grouped barchart plots
Ccomp<-rbind.data.frame(NC,C)
head(Ccomp)
nrow(Ccomp) # 14

# save subset objects for later 
save(Cpair,NC,C,Ccomp,Crow.names,Cmaster2, file="cac.pair.comp_RData")



### ------------------------------------------------------------------------------------
### ------------------------------------------------------------------------------------
### --------------------------------  ANALYSES -----------------------------------------
### ------------------------------------------------------------------------------------
### ------------------------------------------------------------------------------------

### Wilcoxon Rank Sum Test (= Mann-Whitney U Test)
### ------------------------------------------------------------------------------------
# Goal: determine if more individual cacti within a population had higher or lower host quality measure. 
#       This non=arametirc sign-rank test will be done for all host quality variables measured in this study. 
# STHDA resource on correlation matrix background and R code:
browseURL("http://www.sthda.com/english/wiki/unpaired-two-samples-wilcoxon-test-in-r")

wilcox.test()


### --- Matagorda County Pop:
cac<-load(file="cac.pair.comp_RData")
head(Ccomp)

### Matagorda Protein Rank Sum
# testing that cacti without Cactoblastic have more protein
wilcox.test(protein~herbivores,data=Ccomp,alternative="greater")
# W = 16, p-value = 0.8703

### Matagorda Carb Rank Sum
wilcox.test(carbohydrate~herbivores,data=Ccomp,alternative="greater")
# W = 17, p-value = 0.8412

### Matagorda FA Rank Sum
wilcox.test(fatty.acid~herbivores,data=Ccomp,alternative="greater")
# W = 16, p-value = 0.8703

### Matagorda FIBER Rank Sum
wilcox.test(fiber~herbivores,data=Ccomp,alternative="greater")
# W = 34, p-value = 0.1297

### Matagorda Water Rank Sum
wilcox.test(water~herbivores,data=Ccomp,alternative="greater")
# W = 15, p-value = 0.8957
#


### --- Travis County Pop:
mel<-load(file="mel.pair.comp_RData")
head(comp)

### Travis Protein Rank Sum
wilcox.test(protein~herbivores,data=comp,alternative="greater")
# W = 68, p-value = 0.7129

### Travis Carb Rank Sum
wilcox.test(carbohydrate~herbivores,data=comp,alternative="greater")
# W = 75, p-value = 0.5741

### Travis FA Rank Sum
wilcox.test(fatty.acid~herbivores,data=comp,alternative="greater")
# W = 76, p-value = 0.5531

### Travis FIBER Rank Sum
wilcox.test(fiber~herbivores,data=comp,alternative="greater")
# W = 78, p-value = 0.6379

### Travis Water Rank Sum
wilcox.test(water~herbivores,data=comp,alternative="greater")
# W = 91, p-value = 0.3812
#



### ------- PAIRED T-Test: 
# A paired t-test is used to compare two population means where you have two samples in
#   which observations in one sample can be paired with observations in the other sample. 
# Use paired t–test when there is 1 measurement (e.g. [protein]) and 2 nominal (e.g. BN*** and BM***) variables.
#    The most common design is that one nominal variable represents individual organisms (e.g. cladode 1 and 2),
#     while the other is "before" and "after" some treatment. .
#   Examples of where this might occur are:
#     A comparison of two different treatments where the measurements are applied to the same subjects. 
### How to do the test:
# The first step in a paired t–test is to calculate the difference for each pair. 
# Then you use a one-sample t–test to compare the mean difference to 0. 

# Ho = mean difference between paired cladode samples is 0.
# Ha = mean difference significantly different from 0 (positive or negative).
# paired t–test ASSUMPTIONS: the differences between pairs are normally distributed,
#   NOT ASSUMING that observations within each group are normal. The data are continuous.



###########################
### PROTEIN ################
############################

# Travis County Pop:
N2<-N[,c(2,6)] 
names(N2)[2]<-"N"
nrow(N2)
N2
M2<-M[,c(6)]  
names(M2)[1]<-"M"
M2
# create new DF for analysis with non-infested and infested in side-by-side columns
comp2=cbind.data.frame(N2,M2)[-c(12),]
nrow(comp2)
head(comp2)
names(comp2)[3]<-"M"
### Remember that the first step in a paired t–test is to calculate the difference for each pair. 
# calculate difference between non-infested and infested cladodes for each pair and store data in new DF column.
# SAMPLE CODE: df$V3 <- df$V1 - df$V2
comp2$diff <- comp2$M - comp2$N
PROTdiff <- comp2$diff
hist(PROTdiff) 
# one-sample t–test to compare the mean difference to 0. 
library(ggpubr)
t.test(PROTdiff, mu = 0, alternative = "two.sided") 
# t = -0.56688, df = 11, p-value = 0.5822

### ------------------------------------------------------------------------------------

# Matagorda County Pop:
NC2<-NC[,c(2,6)] 
names(NC2)[2]<-"NC"
nrow(NC2)
NC2
C2<-C[,c(6)]  
names(C2)[1]<-"C"
C2
# create new DF for analysis with non-infested and infested in side-by-side columns
Ccomp2=cbind.data.frame(NC2,C2)
head(Ccomp2)
names(Ccomp2)[3]<-"C"
### Remember that the first step in a paired t–test is to calculate the difference for each pair. 
# calculate difference between non-infested and infested cladodes for each pair and store data in new DF column.
# SAMPLE CODE: df$V3 <- df$V1 - df$V2
Ccomp2$diff <- Ccomp2$NC - Ccomp2$C
PROTdiff <- Ccomp2$diff
hist(PROTdiff) 
# one-sample t–test to compare the mean difference to 0. 
library(ggpubr)
t.test(PROTdiff, mu = 0, alternative = "two.sided") 
# t = 0.48818, df = 6, p-value = 0.6427


############################
### CARBOHYDRATES###########
############################

# Travis County Pop:
### create new objects of non-infested and infested [FA] samples for the pairwise differences plot
N2<-N[,c(2,4)] 
names(N2)[2]<-"N"
nrow(N2)
N2
M2<-M[,c(4)]  
names(M2)[1]<-"M"
M2
# create new DF for analysis with non-infested and infested in side-by-side columns
comp3=cbind.data.frame(N2,M2)[-c(12),]
head(comp3)
names(comp3)[3]<-"M"
### Remember that the first step in a paired t–test is to calculate the difference for each pair. 
comp3$diff <- comp3$M - comp3$N
CARBdiff <- comp3$diff
hist(CARBdiff) 
# one-sample t–test to compare the mean difference to 0. 
library(ggpubr)
t.test(CARBdiff, mu = 0, alternative = "two.sided") 
# t = 0.36499, df = 11, p-value = 0.722

### ------------------------------------------------------------------------------------

# Matagorda County Pop:
### subset values for Matagorda non-infested and CACTOBLASTIS infested samples
NC2<-NC[,c(2,4)] 
names(NC2)[2]<-"NC"
nrow(NC2)
NC2
C2<-C[,c(4)]  
names(C2)[1]<-"C"
C2
# create new DF for analysis with non-infested and infested in side-by-side columns
Ccomp3=cbind.data.frame(NC2,C2)
head(Ccomp3)
names(Ccomp3)[3]<-"C"
### Remember that the first step in a paired t–test is to calculate the difference for each pair. 
Ccomp3$diff <- Ccomp3$NC - Ccomp3$C
CARBdiff <- Ccomp3$diff
hist(CARBdiff) 
# one-sample t–test to compare the mean difference to 0. 
library(ggpubr)
t.test(CARBdiff, mu = 0, alternative = "two.sided") 
# t = 0.61474, df = 6, p-value = 0.5613


############################
### FATTY ACIDS ###########
############################ 

# Travis County Pop:
### create new objects of non-infested and infested [FA] samples for the pairwise differences plot
N2<-N[,c(2,5)] 
names(N2)[2]<-"N"
nrow(N2)
N2
M2<-M[,c(5)]  
names(M2)[1]<-"M"
M2
# create new DF for analysis with non-infested and infested in side-by-side columns
comp4=cbind.data.frame(N2,M2)[-c(12),]
head(comp4)
names(comp4)[3]<-"M"
### Remember that the first step in a paired t–test is to calculate the difference for each pair. 
# calculate difference between non-infested and infested cladodes for each pair and store data in new DF column.
# SAMPLE CODE: df$V3 <- df$V1 - df$V2
comp4$diff <- comp4$M - comp4$N
FAdiff <- comp4$diff
hist(FAdiff)
#Then you use a one-sample t–test to compare the mean difference to 0 { H(o) theoretical mean}. 
library(ggpubr)
t.test(FAdiff, mu = 0, alternative = "two.sided") 
# t = 1.1485, df = 11, p-value = 0.2751 ---> H(a): true mean is not equal to 0
# THIS IS ANOTHER WAY TO DO THE SAME TEST:
t.test(comp4$N, comp4$M,paired=TRUE,conf.level=0.95)
# t = 1.1485, df = 11, p-value = 0.2751

### ------------------------------------------------------------------------------------

# Matagorda County Pop:
### subset values for Matagorda non-infested and CACTOBLASTIS infested samples
NC2<-NC[,c(2,5)] 
names(NC2)[2]<-"NC"
nrow(NC2)
NC2
C2<-C[,c(5)]  
names(C2)[1]<-"C"
C2
# create new DF for analysis with non-infested and infested in side-by-side columns
Ccomp4=cbind.data.frame(NC2,C2)
head(Ccomp4)
names(Ccomp4)[3]<-"C"
### Remember that the first step in a paired t–test is to calculate the difference for each pair. 
Ccomp4$diff <- Ccomp4$NC - Ccomp4$C
FAdiff <- Ccomp4$diff
hist(FAdiff) 
# one-sample t–test to compare the mean difference to 0. 
library(ggpubr)
t.test(FAdiff, mu = 0, alternative = "two.sided") 
# t = 1.5368, df = 6, p-value = 0.1753


############################
########## FIBER ###########
############################

# Travis County Pop:
### create new objects of non-infested and infested FIBER samples for the pairwise differences plot
N2<-N[,c(2,7)] 
names(N2)[2]<-"N"
N2
M2<-M[,c(7)]  
names(M2)[1]<-"M"
M2
# create new DF for analysis with non-infested and infested in side-by-side columns
comp5=cbind.data.frame(N2,M2)
head(comp5)
names(comp5)[3]<-"M"
# calculate difference between non-infested and infested cladodes for each pair 
comp5$diff <- comp5$M - comp5$N
FIBdiff <- comp5$diff
hist(FIBdiff)
# one-sample t–test to compare the mean difference to 0. 
library(ggpubr)
t.test(FIBdiff, mu = 0, alternative = "two.sided") 
# t = -0.54696, df = 12, p-value = 0.5944

### ------------------------------------------------------------------------------------

# Matagorda County Pop:
### subset values for Matagorda non-infested and CACTOBLASTIS infested samples
NC2<-NC[,c(2,7)] 
names(NC2)[2]<-"NC"
nrow(NC2)
NC2
C2<-C[,c(7)]  
names(C2)[1]<-"C"
C2
# create new DF for analysis with non-infested and infested in side-by-side columns
Ccomp5=cbind.data.frame(NC2,C2)
head(Ccomp5)
names(Ccomp5)[3]<-"C"
### Remember that the first step in a paired t–test is to calculate the difference for each pair. 
Ccomp5$diff <- Ccomp5$NC - Ccomp5$C
FIBdiff <- Ccomp5$diff
hist(FIBdiff) 
# one-sample t–test to compare the mean difference to 0. 
library(ggpubr)
t.test(FIBdiff, mu = 0, alternative = "two.sided") 
# t = -1.3415, df = 6, p-value = 0.2283

############################
######### WATER ###########
############################ 

# Travis County Pop:
### create new objects of non-infested and infested FIBER samples for the pairwise differences plot
N2<-N[,c(2,8)] 
names(N2)[2]<-"N"
N2
M2<-M[,c(8)]  
names(M2)[1]<-"M"
M2
# create new DF for analysis with non-infested and infested in side-by-side columns
comp6=cbind.data.frame(N2,M2)
head(comp6)
names(comp6)[3]<-"M"
# calculate difference between non-infested and infested cladodes for each pair 
comp6$diff <- comp6$M - comp6$N
WATdiff <- comp6$diff
hist(WATdiff)
# one-sample t–test to compare the mean difference to 0. 
library(ggpubr)
t.test(WATdiff, mu = 0, alternative = "two.sided") 
# t = 0.54696, df = 12, p-value = 0.5944

### ------------------------------------------------------------------------------------

# Matagorda County Pop:
NC2<-NC[,c(2,8)] 
names(NC2)[2]<-"NC"
nrow(NC2)
NC2
C2<-C[,c(8)]  
names(C2)[1]<-"C"
C2
# create new DF for analysis with non-infested and infested in side-by-side columns
Ccomp6=cbind.data.frame(NC2,C2)
head(Ccomp6,14)
names(Ccomp6)[3]<-"C"
### Remember that the first step in a paired t–test is to calculate the difference for each pair. 
Ccomp6$diff <- Ccomp6$NC - Ccomp6$C
WATdiff <- Ccomp6$diff
hist(WATdiff) 
# one-sample t–test to compare the mean difference to 0. 
library(ggpubr)
t.test(WATdiff, mu = 0, alternative = "two.sided") 
# t = 1.3415, df = 6, p-value = 0.2283

# Save pairwise difference objects for Supplemental Pairwise Difference plots
save(comp2,Ccomp2,
     comp3,Ccomp3,
     comp4,Ccomp4,
     comp5,Ccomp5,
     comp6,Ccomp6,
     file="difference.files_2-17-20")





# ----- CITE R Core Team 
citation()

#To cite R in publications use:

#  R Core Team (2018). R: A language and environment for statistical
# computing. R Foundation for Statistical Computing, Vienna, Austria.
# URL https://www.R-project.org/.


####################################################################################################

