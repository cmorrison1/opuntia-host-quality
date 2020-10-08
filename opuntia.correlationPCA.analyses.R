################################################################################################################
################################################################################################################
############# Script for Opuntia Host QUality Principle Component Correlation Analyses & Plots ##################
################################################################################################################
################################################################################################################


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
### ------------------------------------------------------------------------------------
### --- Create the master dataframes for Ordination Plots and Analyses
### ------------------------------------------------------------------------------------
### ------------------------------------------------------------------------------------


### --- Melitara from Travis County O. engelmannii opulation
### ------------------------------------------------------------------------------------
# read in separate DFs
hosts=load("hosts_2-14-20_RData")
hosts
# "cut2"  "FA2"   "carb2" "prot2" "fib2"  "h2o2"

### ----- SUBSET THE DATA
# subset the host quality dataframes to maske master DF with all continuous and discrete variables of interest

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
fib.samps<-c("BN01-3","BN02-3","BN03-3","BN04-3","BN05-3","BN06-3","BN07-3","BN09-3",
             "BN10-3","WN01HIGH_3","WN02HIGH_3","WN03HIGH_3","BM01-3","BM02-3",
             "BM03-3","BM04-3","BM06-3","BM07-3","BM08-3","BM09-3","BM10-3",
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
### subset individual DF for only non-infested and infested samples Bauerle O. engelmannii samples
Ncut=cut2[grepl("^(B|W)N",cut2$Sample),]
Mcut=cut2[grepl("^(B|W)M",cut2$Sample),]
cut3=rbind.data.frame(Ncut,Mcut)
cut3<-cut3[,-c(2:3)]
names(cut3)[2]<-"cuticle" # carbohydrate concentration in (ug/mg)  
cut3
#
### combine new variable objects into master MELITARA DF for PCA.
master1<-merge(carb3,FA3, by = "Sample", all = TRUE)
master2<-merge(master1,prot3, by = "Sample", all = TRUE)
master3<-merge(master2,fib4, by = "Sample", all = TRUE)
master4<-merge(master3,h2o4, by = "Sample", all = TRUE)
master<-merge(master4,cut3, by = "Sample", all = TRUE)

colnames(master)
names(master)[1]<-"sample"
master
# save these data for easy upload later in the workflow
save(carb3,fa3,prot3,fib4,h2o4,cut3,master,file="meldata_2-14-20_RData")


### --- C. cactorum from Matagorda County O. engelmannii opulation
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
nrow(Cfib3) # 8
Cfib.samps<-c("MN01-3","MN02-3","MN03-3","MN06-3",
              "MC01-3","MC02-3","MC03-3","MC07-3")
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
### subset individual DF for only non-infested and infested samples Bauerle O. engelmannii samples
NCcut=cut2[grepl("^MN",cut2$Sample),]
Ccut=cut2[grepl("^MC",cut2$Sample),]
Ccut3=rbind.data.frame(NCcut,Ccut)
Ccut3<-Ccut3[,-c(2:3)]
names(Ccut3)[2]<-"cuticle" # carbohydrate concentration in (ug/mg)  
Ccut3
#
### combine into master CACTOBLASTIS DF for PCA.
Cmaster1<-merge(Ccarb3,CFA3, by = "Sample", all = TRUE)
Cmaster2<-merge(Cmaster1,Cprot3, by = "Sample", all = TRUE)
Cmaster3<-merge(Cmaster2,Cfib4, by = "Sample", all = TRUE)
Cmaster4<-merge(Cmaster3,Ch2o4, by = "Sample", all = TRUE)
Cmaster<-merge(Cmaster4,Ccut3, by = "Sample", all = TRUE)
names(Cmaster)[1]<-"sample"
Cmaster

# save these data for easy upload later in the workflow
save(Ccarb3,CFA3,Cprot3,Cfib4,Ch2o4,Ccut3,Cmaster,file="cacdata_2-14-20_RData")





### ------------------------------------------------------------------------------------
### ------------------------------------------------------------------------------------
### --------------------- Data Visualization: PCA PLOTS --------------------------------
### ------------------------------------------------------------------------------------
### ------------------------------------------------------------------------------------

### Principle Comopnents Analysis of Opuntia Host Quality Variables
# Principal component analysis (PCA) allows us to summarize and to visualize the information in a data set 
# containing individuals/observations described by multiple inter-correlated quantitative variables.
#   Principal component analysis is used to extract the important information from a multivariate data table 
#   and to express this information as a set of few new variables called principal components. 
#   with the goal of finding the best summary of the data using a limited number of PCs.

# STHDA resource on PCA background and R code:
browseURL("http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/#r-packages")



### --- MELITARA - Travis County O. engelmannii Population --- ###
### ------------------------------------------------------------------------------------
# load the Melitara master dataframe
quals=load("meldata_2-14-20_RData")
quals # "carb3"  "fa3"    "prot3"  "fib4"   "h2o4"   "cut3"   "master"

head(master)
colnames(master)
# [1] "sample"       "pair"         "herbivores"   "carbohydrate" "fatty.acid"   "protein"     
# [7] "fiber"        "water"        "cuticle"      "fiber"        "water"  

# set variables as factorial or numeric
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
     master,master2,master.active,file="melPCAdata_2-14-20_RData")



### --- CACTOBLASTIS CACTORUM - Matagorda Co O. engelmannii Population --- ###
### -------------------------------------------------------------------------
# load the Cactoblastis master dataframe
quals=load("cacdata_2-14-20_RData")
quals # "Ccarb3"  "CFA3"    "Cprot3"  "Cfib4"   "Ch2o4"   "Ccut3"   "Cmaster"
head(Cmaster)
colnames(Cmaster)
# [1] "sample"       "pair"         "herbivores"   "carbohydrate" "fatty.acid"   "protein"     
# [7] "fiber"        "water"        "cuticle"      "fiber"        "water"  

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
     Cmaster,Cmaster2,Cmaster.active,file="cacPCAdata_2-14-20_RData")





####################################################################################################
### ----------------- Do PCA Analysis on both populations 
####################################################################################################

# Melitara
Mfiles<-load(file="melPCAdata_2-14-20_RData")
Mfiles

# install.packages(c("FactoMineR", "factoextra","missmDA"))
library(FactoMineR)
library(factoextra)
library(missMDA)


### --------------------------------------------------------------
### --- MELITARA - Travis County O. engelmannii Population --- ###
### choose number of dimensions ('package missMDA')
nd = estim_ncpPCA(master.active,ncp.max=5)
nd # 4 dimensions
### impute the data to deal with missing values 
res.comp = imputePCA(master.active,ncp=4)
res.comp
### standardize the data for PCA
# scale.unit: a logical value. If TRUE, the data are scaled to unit variance before the analysis. 
# This standardization to the same scale avoids some variables becoming dominant  
# because of their large measurement units. It makes variable comparable.

### BASIC PLOT of VARIABLE CORRELATIONS and INDIVIDUAL COORDINATES
res.pca <- PCA(master.active, scale.unit = TRUE, ncp = 5,graph = TRUE)
res.pca 
#### ---- VARIABLE CORRELATION PLOTS ----- ###
eig.val <- get_eigenvalue(res.pca)
eig.val
#       eigenvalue variance.percent cumulative.variance.percent
# Dim.1  1.8924081        37.848163                    37.84816
# Dim.2  1.1877657        23.755314                    61.60348
# Dim.3  0.8982705        17.965411                    79.56889
# Dim.4  0.6468379        12.936758                    92.50565
# Dim.5  0.3747177         7.494355                   100.00000

# EIGENVALUE Note:An eigenvalue > 1 indicates that PCs account for more variance than 
# accounted by one of the original variables in standardized data. 
#   This is commonly used as a cutoff point for which PCs are retained. 
#   This holds true only when the data are standardized.

### Extract the results, for variables:
var <- get_pca_var(res.pca)
var

head(var$cos2, 5)
# -- The quality of representation of the variables on factor map is called cos2 
#                   Dim.1       Dim.2     Dim.3      Dim.4       Dim.5
# [carbohydrate] 0.39653833 0.225418855 0.2322886 0.04328661 0.102467583
# [fatty acid]   0.50760196 0.002669801 0.1379971 0.28608705 0.065644071
# [protein]      0.36194613 0.359979462 0.1315844 0.03110418 0.115385798
# % fiber        0.54960200 0.015105161 0.1217314 0.21815924 0.095402178
# % water        0.02397641 0.563987956 0.2619890 0.14874433 0.001302318
write.csv(var$cos2, file="travis.englemannii_var.qual.csv")

var$contrib 
# -- contains the contributions (in percentage) of the variables to the principal components.
#                 Dim.1     Dim.2     Dim.3       Dim.4     Dim.5
#[carbohydrate] 21.554922 19.3134289 26.22980  5.951020 26.9508306
#[fatty acid]   27.592089  0.2287431 15.58250 39.331091 17.2655798
#[protein]      19.674569 30.8423079 14.85838  4.276186 30.3485552
#% fiber        29.875116  1.2941794 13.74579 29.992414 25.0925011
#% water         1.303303 48.3213406 29.58353 20.449289  0.3425333
write.csv(var$contrib, file="travis.englemannii_PC.contributions.csv")

# Coordinates of variables
head(var$coord, 5)
#                   Dim.1      Dim.2       Dim.3       Dim.4
# [carbohydrate]  0.8135036  0.2734949  0.03095002  0.512301943
# [fatty acid]    0.4060594  0.7845389  0.40525126 -0.235341106
# [protein]       0.4722813 -0.6614474  0.58258236 -0.005382324
# % fiber        -0.9434788  0.1064715  0.26449785  0.161743309
# % water         0.9469877 -0.1353932 -0.22738275 -0.175349330

# A high cos2 indicates a good representation of the variable on the principal component. 
# A low cos2 indicates that the variable is not perfectly represented by the PCs. 




### ------------------------------------------------------------------------------------
### ----- FIGURE 3
### Variable correlation plots for host quality varaibles
### ------------------------------------------------------------------------------------

### --- 3A: MELITARA - Travis County O. engelmannii Population --- ###
vars<-fviz_pca_var(res.pca, col.var = "black")
vars2<-ggpubr::ggpar(vars,
              title= "A. Travis County",
              subtitle= "",
              xlab = "PC1 (37.5 %)", 
              ylab = "PC2 (23.1%)"
             ) + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5,vjust=1.0),
        plot.margin = margin(t = 10, r = 15, b = 10, l = 15, unit = "pt"),
        panel.grid.minor = element_blank()
        )
vars2

### -------------------------------------------------------------------------
### --- 3B: CACTOBLASTIS CACTORUM - Matagorda Co O. engelmannii Population --- ###
Cvars<-fviz_pca_var(Cres.pca, col.var = "black",repel=TRUE)
Cvars2<-ggpubr::ggpar(Cvars,
              title="B. Matagorda County",
              subtitle= "",
              xlab = "PC1 (46.7 %)", 
              ylab = "PC2 (28.9 %)"
) + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5,vjust=1.0),
        plot.margin = margin(t = 10, r = 15, b = 10, l = 15, unit = "pt"),
        panel.grid.minor = element_blank()
  )
Cvars2

### Interpetation NOTE:
# Positively correlated variables are grouped together.
# Negatively correlated variables are positioned on opposite sides of the plot origin (opposed quadrants).
# The distance between variables and the origin measures the quality of the variables on the factor map. 
#     Variables that are away from the origin are well represented on the factor map.

#### ---- FIGURE 3 Multipanel figure
library(ggpubr)
pcaVARS<-ggarrange(vars2,Cvars2,nrow=1,ncol=2)
pcaVARS
#annotate_figure(pcaVARS,
#                top=text_grob("Host Variable Correlations",
#                              size = 17,vjust=4.0,hjust=0.5))




### ------------------------------------------------------------------------------------
#### ---- FIGURE 4 - INDIVIDUAL COORDINATE PLOTS ----- ###
### ------------------------------------------------------------------------------------

### --- 4A: MELITARA - Travis County O. engelmannii Population --- ###
# load data into memory
pca<-load("melPCAdata_2-14-20_RData")
pca
# call coordinates, contributions and rep. quality of individuals
ind <- get_pca_ind(res.pca)
ind 
###  simple PCA of all individuals labeled by sample (no groupings)
herb.pca <- fviz_pca_ind(res.pca, scale.unit = TRUE, ncp = 4,graph = FALSE)
herb.pca 
# Plot Individuals colored by factor variable (herbivores in this case)
nrow(herb.pca)
nrow(master2) # 26
# impute NAs before PCA analysis
res.comp = imputePCA(master.active,ncp=4)
res.comp
# compute PCA
herb.pca <- PCA(res.comp, graph = FALSE)
### Visualize PCA
herbs<-fviz_pca_ind(herb.pca,
                    geom.ind = "point", # show points only (nbut not "text")
                    col.ind = master2$herbivores, # color by groups
                    pointsize=2.5,
                    arrowsize=10,
                    palette = c("blue", "green"),
                    mean.point = FALSE, # remove large icons for means
                    addEllipses = TRUE, # add concentration ellipses (95% confidence elipses)
                    #ellipse.type = "convex",
                    )
herbs

#save PCA files for later
save(nd,res.comp,res.pca,eig.val,var,ind,herb.pca,file="melPCA_2-4-20")

# Plot the Melitara Individuals PCA 
Mpca<-ggpubr::ggpar(herbs,
                    title=expression(paste("A. Travis County ", italic("O. englemannii - M. doddalis "))),
                    subtitle="",
                    xlab = "PC1 (44.9 %)", 
                    ylab = "PC2 (23.8%)") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5,vjust=1.0),
        legend.title = element_blank(),
        plot.margin = margin(t = 10, r = 0, b = 25, l = 20, unit = "pt"),
        axis.title.x = element_text(vjust=-5.0,size=12.0),
        axis.title.y = element_text(vjust=5.0,size=12.0),
        panel.grid.minor = element_blank()
  )
Mpca


### ------------------------------------------------------------------------------------
### --- 4B: CACTOBLASTIS CACTORUM - Matagorda Co O. engelmannii Population --- ###
### choose number of dimensions
Cnd = estim_ncpPCA(Cmaster.active,ncp.max=5)
Cnd # 4 dimensions
### impute the data to deal with missing values 
Cres.comp = imputePCA(Cmaster.active,ncp=4)
Cres.comp
### BASIC PLOT of VARIABLE CORRELATIONS and INDIVIDUAL COORDINATES
Cres.pca <- PCA(Cmaster.active, scale.unit = TRUE, ncp = 5,graph = TRUE)
Cres.pca 
#### ---- VARIABLE CORRELATION PLOTS ----- ###
Ceig.val <- get_eigenvalue(Cres.pca)
Ceig.val
#       eigenvalue variance.percent cumulative.variance.percent
# Dim.1  2.3333985        46.667970                    46.66797
# Dim.2  1.4438097        28.876194                    75.54416
# Dim.3  0.5674690        11.349379                    86.89354
# Dim.4  0.4546365         9.092730                    95.98627
# Dim.5  0.2006863         4.013727                   100.00000
### Extract the results, for variables:
Cvar <- get_pca_var(Cres.pca)
Cvar

Cvar$contrib
write.csv(Cvar$contrib, file="mata.englemannii_PC.contributions.csv")
#                    Dim.1       Dim.2        Dim.3     Dim.4     Dim.5
# [carbohydrate] 20.789872 15.23170572  9.010137018 52.580802  2.387484
# [fatty acid]    7.667387 36.43368737 39.578946109 14.847795  1.472185
# [protein]       6.322542 39.67460099 45.098598172  2.305178  6.599081
# % fiber        28.292813  8.63916083  6.311119695 25.739648 31.017259
# % water        36.927387  0.02084508  0.001199006  4.526577 58.523992


Cvar$cos2
write.csv(Cvar$cos2, file="mata.englemannii_var.qual.csv")
#                    Dim.1        Dim.2        Dim.3      Dim.4       Dim.5
# [carbohydrate] 0.4851106 0.2199168426 5.112973e-02 0.23905152 0.004791354
# [fatty acid]   0.1789107 0.5260331072 2.245982e-01 0.06750349 0.002954473
# [protein]      0.1475301 0.5728257319 2.559205e-01 0.01048018 0.013243455
# % fiber        0.6601841 0.1247330409 3.581364e-02 0.11702183 0.062247403
# % water        0.8616631 0.0003009634 6.803984e-06 0.02057947 0.117449658


### Visualize Cactoblastis PCA
# load C. cactorum PCA data
cacti<-load("cacPCA_2-4-20")
cacti
# [1] "Cnd"       "Cres.comp" "Cres.pca"  "Ceig.val"  "Cvar"      "Cind"     
# [7] "Cherb.pca"

Cherbs<-fviz_pca_ind(Cherb.pca,
                    geom.ind = "point", # show points only (nbut not "text")
                    col.ind = Cmaster2$herbivores, # color by groups
                    pointsize=2.5,
                    arrowsize=10,
                    palette = c("red", "green"),
                    mean.point = FALSE, # remove large icons for means
                    addEllipses = TRUE, # add concentration ellipses (95% confidence elipses)
                    #ellipse.type = "convex",
)
Cherbs

# Plot the Cactoblastis Individuals PCA 
Cpca<-ggpubr::ggpar(Cherbs,
                    title=expression(paste("B. Matagorda County ", italic("O. englemannii - C. cactorum "))),
                    subtitle="",
                    xlab = "PC1 (25.3 %)", 
                    ylab = "PC2 (56.0 %)") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5,vjust=1.0),
        legend.title = element_blank(),
        plot.margin = margin(t = 10, r = 0, b = 25, l = 20, unit = "pt"),
        axis.title.x = element_text(vjust=-5.0,size=12.0),
        axis.title.y = element_text(vjust=5.0,size=12.0),
        panel.grid.minor = element_blank()
  )

Cpca


#save PCA files for later
save(Cnd,Cres.comp,Cres.pca,Ceig.val,Cvar,Cind,Cherb.pca,file="cacPCA_2-4-20")

### --- FIGURE 4 multipanel Figure 
library(ggpubr)
pcaINDIV<-ggarrange(Mpca,Cpca,nrow=1,ncol=2)
pcaINDIV




### ------------------------------------------------------------------------------------
### ---- SUPPLEMENTAL FIGURE 2
### Visualize eigenvalues with 'Scree Plot'
### ------------------------------------------------------------------------------------

### --- FIGURE S2A - Travis County O. englemannii
pca<-load("melPCA_2-4-20")
pca

scree<-fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 60))
# -- Stop at 5th PC because this explains 100% of the variation (also R^2 line assymptotes).
# -- 40.5+25.5+17.2+8.7+8.1 = 100%
scree2<-ggpubr::ggpar(scree) + 
  labs(title="",
       subtitle = expression(paste("A. Travis County ", italic("O. englemannii"))),
       x="",
       y="percent variance explained") + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5,vjust=1.0),
        plot.margin = margin(t = 0.5, r = 0, b = 0.75, l = 0.75, unit = "cm"),
        axis.title.x = element_text(vjust=-5.0,size=12.0),
        axis.title.y = element_text(angle = 90,vjust=5.0,size=12.0)
  )

scree2

### ------------------------------------------------------------------------------------
### ---- FIGURE S2B - Matagorda County O. englemannii    
pca2<-load("cacPCA_2-4-20")
pca2

Cscree<-fviz_eig(Cres.pca, addlabels = TRUE, ylim = c(0, 60))
# -- Stop at 5th PC because this explains 100% of the variation (also R^2 line assymptotes).
# -- 40.5+25.5+17.2+8.7+8.1 = 100%

Cscree2<-ggpubr::ggpar(Cscree) + 
  labs(title="",
       subtitle = expression(paste("B. Matagorda County ", italic("O. englemannii"))),
       x="",
       y="") + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5,vjust=1.0),
        plot.margin = margin(t = 0.5, r = 0, b = 0.75, l = 0.75, unit = "cm"),
        axis.title.x = element_text(vjust=-5.0,size=12.0),
        axis.title.y = element_text(angle = 90,vjust=5.0,size=12.0)
  )

Cscree2

### --- Multiple Panel PUBLICATION Figure  
library(ggpubr)
SCREE<-ggarrange(scree2,Cscree2,nrow=1,ncol=2)
SCREE
annotate_figure(SCREE,
                top=text_grob("Pincipal Component Contributions",
                              size = 17,vjust=1.0,hjust=0.5),
                bottom= text_grob("top PCs",
                                  hjust=0.5,size=12,vjust=-0.5))

### ------------------------------------------------------------------------------------
### ---- SUPPLEMENTAL FIGURES 3
# Visualize the quality of representation of the variables on the Factor Map.
### ------------------------------------------------------------------------------------

### --- FIGURE S3A - Travis County O. englemannii
pca<-load("melPCA_2-4-20")
pca

### table of variable representation quality 
head(var$cos2, 5)
#                   Dim.1        Dim.2     Dim.3      Dim.4
# [carbohydrate] 0.6617881 0.07479947 0.0009579038 2.624533e-01
# [fatty acid]   0.1648842 0.61550131 0.1642285816 5.538544e-02
# [protein]      0.2230496 0.43751271 0.3394022013 2.896941e-05
# % fiber        0.8901523 0.01133618 0.0699591131 2.616090e-02
# % water        0.8967856 0.01833131 0.0517029136 3.074739e-02

# install.packages("corrplot")
library(corrplot)

Cscree2<-ggpubr::ggpar(Cscree) + 
  labs(title="",
       subtitle = expression(paste("B. Matagorda County ", italic("O. englemannii"))),
       x="",
       y="") + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5,vjust=1.0),
        plot.margin = margin(t = 0.5, r = 0, b = 0.75, l = 0.75, unit = "cm"),
        axis.title.x = element_text(vjust=-5.0,size=12.0),
        axis.title.y = element_text(angle = 90,vjust=5.0,size=12.0)
  )

corr<-corrplot(var$cos2, is.corr=FALSE) # create cos2 correlogram matrix
corr2 <- as.data.frame(t(corr)) # turn matrix into dataframe so R can plot it
#install.packages('ggcorrplot')

library(ggcorrplot)
Tcorr<-ggcorrplot(corr2, method = "circle",
           outline.color = "black",
           legend.title = "correlation")

Tcorr2<-ggpubr::ggpar(Tcorr)+ 
  labs(title="",
       subtitle = expression(paste("A. Travis County ", italic("O. englemannii"))),
       x="",
       y="") +
  scale_x_discrete(labels=c("1","2","3","4","5")) +
         theme(plot.title = element_text(hjust = 0.5),
               plot.subtitle = element_text(hjust = 0.5,vjust=1.0),
               axis.title.x = element_text(vjust=-10.0,size=12.0),
               axis.title.y = element_text(angle = 90,vjust=8.0,size=12.0),
               axis.text.x = element_text(angle=0),
               plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm")
       )

Tcorr2

#  legend.position = "none",

### ------------------------------------------------------------------------------------
### FIGURE S3B - Matagorda County O. englemannii
pca2<-load("cacPCA_2-4-20")
pca2

### table of variable representation quality 
head(Cvar$cos2, 5)
#                    Dim.1        Dim.2        Dim.3      Dim.4
# [carbohydrate] 0.4851106 0.2199168426 5.112973e-02 0.23905152
# [fatty acid]   0.1789107 0.5260331072 2.245982e-01 0.06750349
# [protein]      0.1475301 0.5728257319 2.559205e-01 0.01048018
# % fiber        0.6601841 0.1247330409 3.581364e-02 0.11702183
# % water        0.8616631 0.0003009634 6.803984e-06 0.02057947

#library(corrplot)
c<-corrplot(Cvar$cos2, is.corr=FALSE) # create cos2 correlogram matrix
c2 <- as.data.frame(t(c)) # turn matrix into dataframe so R can plot it
#library(ggcorrplot)
Mcorr<-ggcorrplot(c2, method = "circle",
           outline.color = "black",
           legend.title = "correlation")

Mcorr2<-ggpubr::ggpar(Mcorr) + 
  labs(title="",
       subtitle = expression(paste("B. Matagorda County ", italic("O. englemannii"))),
       x="",
       y="") + 
  scale_x_discrete(labels=c("1","2","3","4","5")) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5,vjust=1.0),
        axis.title.x = element_text(vjust=-10.0,size=12.0),
        axis.title.y = element_text(angle = 90,vjust=8.0,size=12.0),
        axis.text.x = element_text(angle=0),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm")
  )

Mcorr2


### PLOT muliple panels on one FIGURE
library(ggpubr)
CORRELOGRAM<-ggarrange(Tcorr2,Mcorr2,nrow=2,ncol=1,
                       common.legend = TRUE,
                       legend = "right")
CORRELOGRAM
annotate_figure(CORRELOGRAM,
                top=text_grob("Variable Representation Quality",
                              size = 15,vjust=1.0,hjust=0.5),
                left= text_grob("host variables",
                                  hjust=0.5,size=12,vjust=3.0,rot=90),
                bottom= text_grob("top PCs",
                                hjust=0.5,size=12,vjust=0.5))




### ------------------------------------------------------------------------------------
### ------------------------------------------------------------------------------------
### -------------------------- STATISTICAL ANALYSIS ------------------------------------
### ------------------------------------------------------------------------------------
### ------------------------------------------------------------------------------------

### ANOSIM to analyze similarity between +/- Herbivore populations 
### ------------------------------------------------------------------------------------
### This analysis will reveal statistical significance among herbivore populations 
###   consuming O. englemannii with respect to Host Quality Variables

browseURL('https://rdrr.io/rforge/vegan/man/anosim.html')
# DESCRIPTION: Analysis of similarities (ANOSIM) provides a way to test statistically 
#     whether there is a significant difference between two or more groups of sampling units.

library(vegan)


### ----- Analysis of Travis County O.englemannii +/- Melitara larvae
# load data into memory
mel<-load("melPCAdata_2-14-20_RData")
mel
# check quantitative and qualitative data for ANOSIM
head(master.active) # quantitative data for dissimilarity matrix
nrow(master.active) # 24
head(master2) # contains qualitative grouping data that I want to test
nrow(master2) # 24

### Function anosim operates directly on a dissimilarity matrix. 
### A suitable dissimilarity matrix is produced by functions dist or vegdist.

### turn host quality data into a matrix
Mdissmat<-vegdist(master.active,method="bray",
                 binary = FALSE,diag=TRUE,na.rm = FALSE)
Mdissmat

### run ANOSIM
# here.. dissmat is my  matrix df, master2$herbivores is  grouping info I want to test
anosim(Mdissmat, master2$herbivores, permutations = 9999, distance = "jaccard", 
       parallel = getOption("mc.cores"))

### Bray Curtis Dissimilarity Index
# ANOSIM statistic R: 0.02403 
# Significance: 0.2833 
# Number of permutations: 9999
### Jaccard Dissimilarity Index
# ANOSIM statistic R: 0.02403 
# Significance: 0.283 
# Number of permutations: 9999


### ----- Analysis of Matagorda County O.englemannii +/- C. cactorum larvae
cac<-load(file="cacPCAdata_2-14-20_RData")
cac

head(Cmaster.active) # quantitative data for dissimilarity matrix
nrow(Cmaster.active) # 14
head(Cmaster2) # contains qualitative grouping data that I want to test
nrow(Cmaster2) # 14
### make another matrix
Cdissmat<-vegdist(Cmaster.active,method="bray",
                  binary = FALSE,diag=TRUE,na.rm = FALSE)
Cdissmat

### run ANOSIM
anosim(Cdissmat, Cmaster2$herbivores, permutations = 9999, distance = "jaccard", 
       parallel = getOption("mc.cores"))

### Bray Curtis Dissimilarity Index
# ANOSIM statistic R: 0.05053 
# Significance: 0.2024 
# Number of permutations: 9999
### Jaccard Dissimilarity Index
# ANOSIM statistic R: 0.05053 
# Significance: 0.1934
# Number of permutations: 9999

### ------------------------------------------------------------------------------------
### ----- Comparison of Matagorda and Travis  O.englemannii populations 

# combine host quality DFs
head(master.active)
head(Cmaster.active)
popMaster<-rbind.data.frame(master.active,Cmaster.active)
#
Pdissmat<-vegdist(popMaster,method="bray",
                  binary = FALSE,diag=TRUE,na.rm = FALSE)

# turn 'GROUP' dataframes into travis and matagorda dataframes instead of +/- herbivores
mel<-load("melPCAdata_2-14-20_RData")
master2$herbivores <- gsub('melitara', 'travis', master2$herbivores)
master2$herbivores <- gsub('no melitara', 'travis', master2$herbivores)
master2$herbivores <- gsub('no travis', 'travis', master2$herbivores)

cac<-load(file="cacPCAdata_2-14-20_RData")
Cmaster2$herbivores <- gsub('Cactoblastis', 'matagorda', Cmaster2$herbivores)
Cmaster2$herbivores <- gsub('no Cactoblastis', 'matagorda', Cmaster2$herbivores)
Cmaster2$herbivores <- gsub('no matagorda', 'matagorda', Cmaster2$herbivores)
Cmaster2<-Cmaster2[,-c(8)]
Cmaster2

pops<-rbind.data.frame(master2,Cmaster2)
pops

#
anosim(Pdissmat, pops$herbivores, permutations = 9999, distance = "bray", 
       parallel = getOption("mc.cores"))
### Bray Curtis Dissimilarity Index
# ANOSIM statistic R: 0.8323 ***** 
# Significance: 1e-04 **** 
# Number of permutations: 9999
#### These populations have different host quality. 



### PCA Homogeneity of Dispersion Tests with Bray Curtis Dissimilarity Matrices
### ------------------------------------------------------------------------------------
### Summary: One measure of multivariate dispersion (variance) for a group of samples is to calculate 
###     the average distance of group members to the group centroid in multivariate space. 
betadisper(d, group, type = c("median","centroid"), bias.adjust = FALSE)
### Goal : determine if infested individuals are evenly dispersed throughout the population. 
browseURL("https://rdrr.io/rforge/vegan/man/betadisper.html")

library(vegan)

### ---- Melitara 
mel<-load("melPCAdata_2-14-20_RData")
mel
### turn Melitara host quality data into a matrix
Mdissmat<-vegdist(master.active,method="bray",
                  binary = FALSE,diag=TRUE,na.rm = FALSE)
Mdissmat
# check out factorial Melitara data for grouping argument in betadisper
head(Cmaster2$herbivores,20)
# run multivariate dispersion test 
meldis <- betadisper(Mdissmat, master2$herbivores, type = c("centroid"), bias.adjust = FALSE)
# Average distance to centroid:
#      Melitara   no Melitara 
#       0.05115     0.05258 
anova(meldis)
#           Df     Sum Sq    Mean Sq F value Pr(>F)
# Groups     1 0.0000127 0.00001266  0.0352 0.8528
# Residuals 23 0.0082691 0.00035953 



### --- Cactoblastis 
cac<-load(file="cacPCAdata_2-14-20_RData")
cac
### make another matrix with Cactoblastis host quality data
Cdissmat<-vegdist(Cmaster.active,method="bray",
                  binary = FALSE,diag=TRUE,na.rm = FALSE)
Cdissmat
# check out factorial Cactoblastis data for grouping argument in betadisper
head(Cmaster2$herbivores)
# run multivariate dispersion test 
cacdis <- betadisper(Cdissmat, Cmaster2$herbivores, type = c("centroid"), bias.adjust = FALSE)
# Average distance to centroid:
# Cactoblastis no Cactoblastis 
#   0.10255         0.08451
anova(cacdis)
#           Df   Sum Sq   Mean Sq F value Pr(>F)
# Groups     1 0.001139 0.0011395  0.1625  0.694
# Residuals 12 0.084168 0.0070140 


### Wilcoxon Rank Sum Test (= Mann-Whitney U Test)
### ------------------------------------------------------------------------------------
# Goal: determine if more individual cacti within a population had particular natural enemy damage
# STHDA resource on correlation matrix background and R code:
browseURL("http://www.sthda.com/english/wiki/unpaired-two-samples-wilcoxon-test-in-r")

wilcox.test(x~y,data=,alternative="greater")
# alternative can be either 'less' or 'greater' to determine if non-infested individuals had 
#     less or more of a variable than infested individuals.
### alternative = "greater" is testing Ho that damage scores are greater on plants with Melitara

dam<-read.csv("bauerle.damage.csv")
dam<-dam[c(1:20),]
nrow(dam) # 20

names<-c("no Melitara","no Melitara","no Melitara","no Melitara","no Melitara",
                  "no Melitara","no Melitara","no Melitara","no Melitara","no Melitara",
                  "Melitara","Melitara","Melitara","Melitara","Melitara",
                  "Melitara","Melitara","Melitara","Melitara","Melitara")
dam2<-cbind.data.frame(names,dam)            
head(dam2,20)

### --- Damage Score Analysis
### Melitara
wilcox.test(Melitara.sp.~names,data=dam2,alternative="greater")
# Sanity Check: alternative = "less" prints p-value = 1 (good because that is not true)

### Chelinidia sp. (Coreidae)
wilcox.test(Chelinidia.sp.~names,data=dam2,alternative="greater")
# W = 38, p-value = 0.8299

### Cochineal scale (Dactylopius opuntiae)
wilcox.test(Cochineal.scale~names,data=dam2,alternative="greater",paired=F)
# W = 71.5, p-value = 0.0249*** ---> This means that there are more scale on plants without Melitara

### Phyllosticta Pad Spot fungus
wilcox.test(Phyllosticta.Pad.Spot~names,data=dam2,alternative="greater", paired=F)
# W = 49, p-value = 0.5458

### Anthracnose fungus
wilcox.test(Anthracnose~names,data=dam2,alternative="greater")
# W = 52, p-value = 0.4548

### Cactus Height
wilcox.test(Opuntia.height..ft.~names,data=dam2,alternative="greater")
# W = 69.5, p-value = 0.07221
#


### --- Natural Enemy Presence/Absence  Analysis
pres<-read.csv("bauerle.presence.csv")
head(pres,20)

### Chelinidia sp. (Coreidae)
wilcox.test(Chelinidia.sp.~names,data=pres,alternative="greater")
# W = 50, p-value = 1

### Cochineal scale (Dactylopius opuntiae)
wilcox.test(Cochineal.scale~names,data=pres,alternative="greater",paired=F)
# W = 70, p-value = 0.03181

### Phyllosticta Pad Spot fungus
wilcox.test(Phyllosticta.Pad.Spot~names,data=pres,alternative="greater", paired=F)
# W = 50, p-value = 0.529

### Anthracnose fungus
wilcox.test(Anthracnose~names,data=pres,paired=F,alternative="greater")
# W = 45, p-value = 0.3681
#


### Host Quality Variable -  Multiple Correlation Analyses
### ------------------------------------------------------------------------------------
# Goal: seek correlations between multiple independent variables that predict independent variable herbivory
# STHDA resource on correlation matrix background and R code:
browseURL("http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software")

### --- Correlation Matrix 
### ------------------------------------------------------------------------------------
# Correlation matrices are used to investigate the dependence between multiple variables at the same time. 
# The result is a table containing the correlation coefficients between each variable and the others.

vars=load("PCAdata_2-14-20_RData")
vars
head(master.active)
### Compute correlation matrix with correlation coefficients
res <- cor(master.active)
res # Extract the correlation coefficients
#                 carbohydrates fatty.acid  protein  fiber     water
# carbohydrates    1.00000000  0.3811332  0.02710138 -0.44258259 -0.11802147
# fatty.acid       0.38113318  1.0000000 -0.22031244 -0.47113932 -0.39028548
# protein          0.02710138 -0.2203124  1.00000000  0.37446164 -0.18095140
# fiber           -0.44258259 -0.4711393  0.37446164  1.00000000  0.07961824
# water           -0.11802147 -0.3902855 -0.18095140  0.07961824  1.00000000
### TAKEAWAY(s): weak positive correlations b/w carbs & FAs, protein & fiber
###              weak negative correlations b/w fiber & carbs, fiber & FAs, water & FAs

write.csv(res2$r, file="correlation.matrix.coefficients.csv")
### Compute correlation matrix with significance levels (p-values)
#install.packages("Hmisc")
library("Hmisc")
res2 <- rcorr(as.matrix(master.active))
# res2$r # Extract the correlation coefficients
res2$P # Extract p-values
#             carbohydrates  fatty.acid  protein fiber  water 
# carbohydrates               0.1312     0.9097  0.0578 0.6202
# fatty.acid    0.1312                   0.3955  0.0563 0.1214
# protein       0.9097        0.3955             0.1142 0.4452
# fiber         0.0578        0.0563     0.1142         0.7459
# water         0.6202        0.1214     0.4452  0.7459  
### TAKEAWAY: no statistically significant correlations
###           fiber & carbs, fiber & FA p-values approach 0.05 (in agreement w/ weak neg. correlations above).

write.csv(res2$P, file="correlation.matrix.pvalues.csv")
### Visualize Correlations with a correlogram
# install.packages("corrplot")
library("corrplot")

Correl<-corrplot(res2$r, type = "upper", order = "hclust", 
                 tl.col = "black", tl.srt = 45,
                 title=expression(paste(italic("O. engelmannii"), " Host Quality Correlogram")),
                 mar=c(0,0,3,0)) # pass mar argument to code to avoid title being cut off
Correl         
# Positive correlations are displayed in blue 
# and negative correlations in red color.
### Visualize Correlations with a heat map
install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(res2$r, col = col, symm = TRUE)





#################################################################################
#################################################################################

