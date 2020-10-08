#################################################################################
#################################################################################
################# Script for Opuntia Host Quality Analysis ######################
#################################################################################
#################################################################################


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


# 
cut=read.csv("cuticle.csv") # cuticle thickness
FA=read.csv("fat.csv") # fatty acid
carb=read.csv("carb.csv") # carbohydrates 
prot=read.csv("protein.csv") # protein
fib=read.csv("fiber.csv") # fiber 
h2o=read.csv("water.csv") # water content 



### ------------------------------------------------------------------------------------
### Protein Species Comparison Analysis (FIGURE 3A) 
prot=read.csv("protein.csv")
nrow(prot) # 276
# remove outliers
prot2<-prot[-c(3,147,216,249),]
nrow(prot2) # 272
# subset for just new (terminal) cladodes 
prot3<-prot2[which(prot2$age == 'N'), names(prot2) 
             %in% c('Sample','species','age','herbivores','dry.g','ug.mg','avg.ug.mg')]
prot3$avg.ug.mg<-as.numeric(as.character(prot3$avg.ug.mg))

### Calculate Species protein  Means 
library(plyr)
prot4<-na.omit(prot3)
means <- ddply(prot4, c("species"), summarise,
               N    = length(avg.ug.mg),
               mean = mean(avg.ug.mg),
               sd   = sd(avg.ug.mg),
               se   = sd / sqrt(N)
)
means
#        species  N       mean         sd          se
# 1  engelmannii 37 0.09798154 0.03622369 0.005955138
# 2 ficus-indica  7 0.14952890 0.03210771 0.012135575
# 3   macrorhiza 14 0.10921439 0.03761717 0.010053613
# 4      stricta  9 0.06614645 0.01688156 0.005627185


### ANOVA 
p2=aov(avg.ug.mg ~ species,
       data = prot3)
summary(p2)
#              Df Sum Sq Mean Sq F value Pr(>F)
# species      3 0.02869 0.009562    8.13 0.000117 ***
# Residuals   63 0.07410 0.001176 

### GLM
p1=glm(avg.ug.mg ~ species,
       data = prot3,family="gaussian")
p0=glm(avg.ug.mg ~ 1,
      data = prot2,family="gaussian")
summary(p1)
#                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          0.097982   0.005638  17.378  < 2e-16 ***
# speciesficus-indica  0.051547   0.014136   3.647  0.00054 ***
# speciesmacrorhiza    0.011233   0.010761   1.044  0.30055    
# speciesstricta      -0.031835   0.012747  -2.498  0.01513 *      
---
  #    Null deviance: 0.102786  on 66  degrees of freedom
  # Residual deviance: 0.074099  on 63  degrees of freedom
  # Residual deviance/df = 0.074099 / 63 = 0.001176175
  
library(multcomp)
summary(glht(p1, linfct=mcp(species="Tukey")))
#                                 Estimate Std. Error z value Pr(>|z|)
# ficus-indica - engelmannii == 0  0.05155    0.01414   3.647  0.00157 ** 
# macrorhiza - engelmannii == 0    0.01123    0.01076   1.044  0.71597    
# stricta - engelmannii == 0      -0.03184    0.01275  -2.498  0.05784 .  
# macrorhiza - ficus-indica == 0  -0.04031    0.01588  -2.539  0.05144 .  
# stricta - ficus-indica == 0     -0.08338    0.01728  -4.824  < 0.001 ***
# stricta - macrorhiza == 0       -0.04307    0.01465  -2.939  0.01647 * 

### CHI-SQUARED TEST 
# first make a contingency table for test
ptable <- table(prot3$species,prot3$avg.ug.mg)
chisq.test(ptable)
# X-squared = 201, df = 198, p-value = 0.4272
# X-squared / df =  201/198 = 1.015152

### DEVIANCE
sum(residuals(p1, type = "pearson")^2)
#Residual Deviance: 0.0740986
X2=pchisq(deviance(p0)-deviance(p1),
          df.residual(p0)-df.residual(p1),
          lower.tail=FALSE) # 1
### CHI-SQUARED P-VALUE
p-value=1-X2
# p-value: 3.36832e-08

### LOG-LIKELIHOOD
logLik(p1)
# 'log Lik.' 132.9673 (df=5)

### log-liklihood test of species ~ variable model 
# LLR = -2 * (fit$null.deviance - fit$deviance)
# pchisq(LLR, 2, lower.tail = FALSE)


### ------------------------------------------------------------------------------------
### ------------------------------------------------------------------------------------
### Carbohydrate Species Comparison Analysis (FIGURE 3B) 

carb=read.csv("carb.csv")
head(carb,90)
colnames(carb)
nrow(carb)

# subset for analysis
carb2<-carb[which(carb$age == 'N'), names(carb) 
            %in% c('Sample','species','pair','herbivores','dry.mg','ug.mg','avg.ug.mg')]
nrow(carb2)
# 57
carb2$avg.ug.mg<-as.numeric(as.character(carb2$avg.ug.mg))

### Calculate Species Digestible Carb Means 
library(plyr)
carb3<-na.omit(carb2)
means <- ddply(carb3, c("species"), summarise,
               N    = length(avg.ug.mg),
               mean = mean(avg.ug.mg),
               sd   = sd(avg.ug.mg),
               se   = sd / sqrt(N)
)
means
#        species  N     mean        sd       se
# 1  engelmannii 39 349.6193 130.43388 20.88614
# 2 ficus-indica  8 220.6317 122.68442 43.37549
# 3   macrorhiza 14 328.5409  78.40330 20.95416
# 4      stricta 10 320.6115  64.34342 20.34718

samps<-c(39,10,14,10)
sampleMEAN<-mean(samps)
sampleMEAN # [1] 18.25
sampleSD<-sd(samps) 
sampleSD # [1] 13.44123

### ANOVA
a<-aov(avg.ug.mg~species, data=carb2)
summary(a) 
#             Df Sum Sq Mean Sq F value Pr(>F)
# species      3 110907   36969    2.85 0.0439 *
# Residuals   67 869027   12971 
  
### GLM
c1=glm(avg.ug.mg ~ species,
       data = carb3,family="gaussian")
summary(c1)
#                        Estimate Std. Error t value Pr(>|t|)    
# (Intercept)           349.62      18.24  19.171  < 2e-16 ***
# speciesficus-indica  -128.99      44.20  -2.918  0.00479 ** 
# speciesmacrorhiza     -21.08      35.48  -0.594  0.55448    
# speciesstricta        -29.01      40.37  -0.719  0.47490  

#     Null deviance: 0.037552  on 70  degrees of freedom
# Residual deviance: 0.028011  on 67  degrees of freedom
# Residual deviance / df = 0.028011/67 = 0.0004180746
# AIC: -262.48

library(multcomp)
summary(glht(c1, linfct=mcp(species="Tukey")))
# ficus-indica - engelmannii == 0 -128.988     44.203  -2.918   0.0178 *
# macrorhiza - engelmannii == 0    -21.078     35.483  -0.594   0.9317  
# stricta - engelmannii == 0       -29.008     40.369  -0.719   0.8863  
# macrorhiza - ficus-indica == 0   107.909     50.476   2.138   0.1364  
# stricta - ficus-indica == 0       99.980     54.022   1.851   0.2426  
# stricta - macrorhiza == 0         -7.929     47.154  -0.168   0.9982 

### CHI-SQUARED TEST 
# first make a contingency table for test
ctable <- table(carb2$species,carb2$avg.ug.mg)
chisq.test(ctable)
# X-squared = 171, df = 280, p-value = 0.4211
# X-squared / df =  171/280 = 0.6107143

### DEVIANCE
sum(residuals(c1, type = "pearson")^2)
#Residual Deviance:  0.02801065

# LOG-LIKELIHOOD
logLik(c1)
# 'log Lik.' -434.8866 (df=5)


### ------------------------------------------------------------------------------------
### ------------------------------------------------------------------------------------
### Opuntia caldode FATTY ACID concentration (FIGURE 3C)

FA=read.csv("fat.csv") # fatty acid
head(FA,90)
colnames(FA)

# subset only new cladodes for analysis
FA2<-FA[which(FA$age == 'N'), names(FA) 
        %in% c('sample','species','herbivores','pair','dry.mg','mg.mg','mg.mgAVG')]
nrow(FA3) # 221
FA2$mg.mgAVG<-as.numeric(as.character(FA2$mg.mgAVG))
FA3<-na.omit(FA2[-c(3),])
nrow(FA3) # 67

### Calculate Species Fatty Acid Means 
library(plyr)
means <- ddply(FA3, c("species"), summarise,
               N    = length(mg.mgAVG),
               mean = mean(mg.mgAVG),
               sd   = sd(mg.mgAVG),
               se   = sd / sqrt(N)
)
means
#        species  N      mean         sd         se
# 1  engelmannii 38 0.02223077 0.015211176 0.0024675786
# 2 ficus-indica  7 0.01858574 0.006905102 0.0026098831
# 3   macrorhiza 12 0.01775210 0.008563805 0.0024721577
# 4      stricta 10 0.01065008 0.003107543 0.0009826915

### ANOVA (just to see)
c<-aov(mg.mgAVG~species, data=FA3)
summary(c) 
#             Df  Sum Sq  Mean Sq F value Pr(>F)  
# species      3 0.001107 0.0003690   2.386 0.0774 .
# Residuals   63 0.009741 0.0001546  

## TRY ANOVA on log transformed data 
log<-log(FA3$mg.mgAVG)
cbind.data.frame(log,FA3)
par(mfrow=c(1,2))
hist(FA3$mg.mgAVG)
hist(log)

l<-aov(log~FA3$species)
summary(l) 
#             Df Sum Sq Mean Sq F value Pr(>F)
# FA3$species  3  2.488  0.8294   2.071  0.113
# Residuals   63 25.229  0.4005               

### GLM (untransformed data)
a1=glm(mg.mgAVG ~ species,
       data = FA3,family="gaussian")
#a0=glm(ug.mgAVG ~ 1,
#       data = FA3,family="gaussian")
summary(a1)
#                       Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          0.022231   0.002006  11.081 3.04e-16 ***
# speciesficus-indica -0.003645   0.005087  -0.717   0.4764    
# speciesmacrorhiza   -0.001864   0.004396  -0.424   0.6730    
# speciesstricta      -0.011581   0.004396  -2.635   0.0107 *  
#     Null deviance: 0.0104055  on 64  degrees of freedom
# Residual deviance: 0.0093303  on 61  degrees of freedom
# Res. Deviance / df = 0.0104055/64 = 0.0001625859

### GLM (LOG TRANSFORMED data)
a2=glm(log~FA3$species,family="gaussian")
#a0=glm(ug.mgAVG ~ 1,
#       data = FA3,family="gaussian")
summary(a2)
#                         Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             -4.03808    0.10266 -39.336   <2e-16 ***
# FA3$speciesficus-indica -0.01357    0.26028  -0.052   0.9586    
# FA3$speciesmacrorhiza   -0.16795    0.20955  -0.801   0.4259    
# FA3$speciesstricta      -0.54861    0.22491  -2.439   0.0175 *  
# Null deviance: 27.717  on 66  degrees of freedom
# Residual deviance: 25.229  on 63  degrees of freedom
#### SAME RESULT with TRANSFORMED and UNTRANSFORMED DATA 

library(multcomp)
summary(glht(a1, linfct=mcp(species="Tukey")))
# ficus-indica - engelmannii == 0 -0.003645   0.005087  -0.717   0.8868  
# macrorhiza - engelmannii == 0   -0.001864   0.004396  -0.424   0.9734  
# stricta - engelmannii == 0      -0.011581   0.004396  -2.635   0.0401 *
# macrorhiza - ficus-indica == 0   0.001781   0.006095   0.292   0.9910  
# stricta - ficus-indica == 0     -0.007936   0.006095  -1.302   0.5522  
# stricta - macrorhiza == 0       -0.009717   0.005531  -1.757   0.2861    

### CHI-SQUARED TEST 
# first make a contingency table for test
FA3$mg.mgAVG<-as.numeric(as.character(FA3$mg.mgAVG))
fatable <- table(FA3$species,FA3$mg.mgAVG)
chisq.test(fatable)
# X-squared = 171, df = 264, p-value = 0.4211
# X-squared / df =  171/264 = 0.6477273

### DEVIANCE
sum(residuals(a1, type = "pearson")^2)
#Residual Deviance: 0.009330305

### LOG-LIKELIHOOD
logLik(a1)
# 'log Lik.' 200.4237 (df=5)


### ------------------------------------------------------------------------------------
### ------------------------------------------------------------------------------------
### Opuntia caldode FIBER content (FIGURE 3D)

fib=read.csv("fiber.csv")
head(fib)
colnames(fib2)
nrow(fib) # 82

# remove O. englemannii outliers detected in dataQC script
fib2<-fib[-c(16,21,90,91,92,93,95,96),]


# subset for comparative species analysis
fib3<-fib2[which(fib2$age == 'N'), names(fib2) 
          %in% c('Sample','species','herbivore','dry.g',
                 'fiber.g.','age','concentration')]
nrow(fib3) # 71

### calculte the mean [fiber] for each species 
library(plyr)
fib4<-na.omit(fib3)
means <- ddply(fib4, c("species"), summarise,
               N    = length(concentration),
               mean = mean(concentration),
               sd   = sd(concentration),
               se   = sd / sqrt(N)
)
means
#       species  N      mean         sd         se
#1  englemannii 37        NA         NA         NA
#2 ficus-indica 10 0.2382000 0.06003480 0.01898467
#3   macrorhiza 14 0.2707857 0.07527098 0.02011701
#4      stricta 10 0.3346000 0.18560484 0.05869340

# O. englemannii
eng<-fib3[which(fib3$species == 'englemannii'), names(fib3) %in% c('species','concentration')]
eng2<-na.omit(eng$concentration)
mean(eng2) # 0.3862581

d<-aov(concentration~species, data=fib3)
summary(d) 
#             Df Sum Sq Mean Sq F value Pr(>F)  
# species      3 0.2039 0.06797   3.583 0.0187 *
# Residuals   61 1.1572 0.01897   

### GLM
f1=glm(concentration ~ species,
       data = fib3,family="gaussian")
f0=glm(concentration ~ 1,
       data = fib3,family="gaussian")

summary(f1)
#                       Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          0.38626    0.02450  15.766  < 2e-16 ***
# speciesficus-indica -0.14806    0.04961  -2.985  0.00408 ** 
# speciesmacrorhiza   -0.11547    0.04392  -2.629  0.01082 *  
# speciesstricta      -0.05166    0.04961  -1.041  0.30182   

#             Null deviance: 1.3666  on 64  degrees of freedom
#             Residual deviance: 1.1350  on 61  degrees of freedom
# Resid. Dev. / df = 1.1350/61 = 0.01860656

library(multcomp)
summary(glht(f1, linfct=mcp(species="Tukey")))
#                                 Estimate Std. Error z value Pr(>|z|)  
# ficus-indica - englemannii == 0 -0.13186    0.05009  -2.632   0.0412 *
# macrorhiza - englemannii == 0   -0.11547    0.04435  -2.604   0.0446 *
# stricta - englemannii == 0      -0.05166    0.05009  -1.031   0.7267  
# macrorhiza - ficus-indica == 0   0.01639    0.05703   0.287   0.9915  
# stricta - ficus-indica == 0      0.08020    0.06160   1.302   0.5561  
# stricta - macrorhiza == 0        0.06381    0.05703   1.119   0.6730   

### CHI-SQUARED TEST 
# first make a contingency table for test
fibtable <- table(fib3$species,fib3$concentration)
chisq.test(fibtable)
# X-squared = 185.13, df = 186, p-value = 0.5042
# X-squared / df =  185.13/186 =  0.9953226

### LOG-LIKELIHOOD
logLik(f1)
# 'log Lik.' 39.32158 (df=5)


### ------------------------------------------------------------------------------------
### ------------------------------------------------------------------------------------
### Water content Species Comparison Analysis (FIGURE 3E)

h2o=read.csv('water.csv')
colnames(h3o)

# subset for New cladodes 
h3o<-h2o[which(h2o$age == 'N'), names(h2o) %in% c('unique','species','herbivores','pair','water.content')]
nrow(h3o) # 79
h3o$water.content<-as.numeric(as.character(h3o$water.content))

# remove outliers from BFL O. englemmannii population
h3o<-h3o[-c(4:6,49),] 

### calculte the mean water content for each species 
library(plyr)

means <- ddply(h3o, c("species"), summarise,
               N    = length(water.content),
               mean = mean(water.content),
               sd   = sd(water.content),
               se   = sd / sqrt(N)
)
means
#       species  N      mean         sd          se
#1  engelmannii 41        NA         NA          NA
#2 ficus-indica 10 0.9203884 0.03073360 0.009718816
#3   macrorhiza 14 0.8609244 0.02814829 0.007522947
#4      stricta 10 0.8401933 0.03281562 0.010377211
# O. englemannii
eng<-h3o[which(h3o$species == 'engelmannii'), names(h3o) %in% c('species','water.content')]
eng2<-na.omit(eng$water.content)
mean(eng2) # 0.8581906

### ANOVA 
hist(h3o$water.content) # nice bell curve 
h2=aov(water.content ~ species,
       data = h3o)
summary(h2) 
#             Df  Sum Sq  Mean Sq F value   Pr(>F)    
# species      3 0.03887 0.012957   15.73 6.77e-08 ***
# Residuals   69 0.05683 0.000824 


### GLM 
h1=glm(water.content ~ species,
       data = h3o,family="gaussian")
summary(h1)
# (Intercept)          0.858191   0.004595 186.748  < 2e-16 ***
# speciesficus-indica  0.062198   0.010172   6.114 5.13e-08 ***
# speciesmacrorhiza    0.002734   0.008941   0.306   0.7607    
# speciesstricta      -0.017997   0.010172  -1.769   0.0813 .

#     Null deviance: 0.095698  on 72  degrees of freedom
# Residual deviance: 0.056829  on 69  degrees of freedom
# res. deviance / df = 0.056829/69 = 0.0008236087

summary(glht(h1, linfct=mcp(species="Tukey")))
#                                  Estimate Std. Error z value Pr(>|z|)    
# ficus-indica - engelmannii == 0  0.062198   0.010172   6.114   <1e-04 ***
# macrorhiza - engelmannii == 0    0.002734   0.008941   0.306    0.990    
# stricta - engelmannii == 0      -0.017997   0.010172  -1.769    0.282    
# macrorhiza - ficus-indica == 0  -0.059464   0.011882  -5.004   <1e-04 ***
# stricta - ficus-indica == 0     -0.080195   0.012834  -6.248   <1e-04 ***
# stricta - macrorhiza == 0       -0.020731   0.011882  -1.745    0.294  

### CHI-SQUARED TEST 
# first make a contingency table for test
htable <- table(h3o$species,h3o$water.content)
chisq.test(htable)
# X-squared = 219, df = 216, p-value = 0.4303
# X-squared / df =  219/216 = 1.013889


### LOG-LIKELIHOOD
logLik(h1)
# 'log Lik.' 157.6908 (df=5)


### ------------------------------------------------------------------------------------
### ------------------------------------------------------------------------------------
### Cuticle Thickness Species Comparison Analysis (FIGURE 3F)

cut<-read.csv("cuticle.csv")
head(cut,35)
cut2<-cut[,c(1,2,3,4,5,15)]
colnames(cut2)
plot(cut2$thickAVG~cut2$species)

# huge outliers from BFL O.englemannii population
cut2<-cut2[-c(12,15),] 

### calculte the mean [fiber] for each species 
library(plyr)

cut3<-na.omit(cut2)

means <- ddply(cut3, c("species"), summarise,
               N    = length(thickAVG),
               mean = mean(thickAVG),
               sd   = sd(thickAVG),
               se   = sd / sqrt(N)
)
means
#       species  N      mean         sd          se
#1  engelmannii  9 1.812272 0.21200008 0.07066669
#2 ficus-indica  8 1.638542 0.10865497 0.03841533
#3   macrorhiza  8 1.773750 0.05521782 0.01952245
#4      stricta 10 1.578889 0.09561009 0.03023457

# ANOVA 
c<-aov(thickAVG~species,data=cut2)
summary(c)
#              Df Sum Sq Mean Sq F value  Pr(>F)   
#  species      3 0.3336 0.11118   6.315 0.00181 **
# Residuals   31 0.5458 0.01761   

### GLM 
c1=glm(thickAVG~species,
       data=cut2,family="gaussian")
summary(c1)
#                     Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          1.81227    0.04423  40.974  < 2e-16 ***
# speciesficus-indica -0.17373    0.06448  -2.694 0.011280 *  
# speciesmacrorhiza   -0.03852    0.06448  -0.597 0.554542    
# speciesstricta      -0.23338    0.06097  -3.828 0.000588 ***

#         Null deviance: 0.87936  on 34  degrees of freedom
#         Residual deviance: 0.54581  on 31  degrees of freedom
#         0.54581 / 31
library(multcomp)
summary(glht(c1, linfct=mcp(species="Tukey")))
#                                   Estimate Std. Error z value Pr(>|z|)
# ficus-indica - engelmannii == 0 -0.17373    0.06448  -2.694   0.0355 *  
# macrorhiza - engelmannii == 0   -0.03852    0.06448  -0.597   0.9328    
# stricta - engelmannii == 0      -0.23338    0.06097  -3.828   <0.001 ***
# macrorhiza - ficus-indica == 0   0.13521    0.06635   2.038   0.1740    
# stricta - ficus-indica == 0     -0.05965    0.06294  -0.948   0.7788    
# stricta - macrorhiza == 0       -0.19486    0.06294  -3.096   0.0105 *  

### CHI-SQUARED TEST 
# first make a contingency table for test
ttable <- table(cut2$species,cut2$thickAVG)
chisq.test(ttable)
# X-squared = 89.137, df = 63, p-value = 0.0168
# X-squared / df =  89.137/63 = 1.414873

### DEVIANCE
sum(residuals(c1, type = "pearson")^2)
# Residual Deviance: 0.5458083
# Red. Deviance / df = 0.5458083/31 = 0.01760672

### LOG-LIKELIHOOD
logLik(c1)
# 'log Lik.' 23.15177 (df=5)


### ------------------------------------------------------------------------------------
### ------------------------------------------------------------------------------------
### Species C:P Content Analysis (FIGURE 3A)

# Nutritional geometry studies tend to follow a sequential model building
# approach for comparing nutrional ratios among treatments (e.g. species)
browseURL('https://www.datanovia.com/en/lessons/ancova-in-r/')
### Basic ANCOVA Syntax: lm(y ~ x + g, data=df)  OR  aov(y ~ x + g, data=df)

dats<-load(file='geometry.data_6-24-20_RData')
dats

# calculate carb:protein ratio for analysis 
ratio<-pc4$carbohydrate/pc4$protein 
pc5<-cbind.data.frame(pc4,ratio)
pc5

# summary stats 
stats <- ddply(pc5, c("species"), summarise,
                N    = length(ratio),
                mean = mean(ratio),
                sd   = sd(ratio),
                se   = sd / sqrt(N)
)
stats
#        species  N     mean        sd        se
# 1  engelmannii 37 4.020155 2.5582104 0.4205672
# 2 ficus-indica  7 1.298012 0.7075578 0.2674317
# 3   macrorhiza 14 3.541676 1.8159982 0.4853459
# 4      stricta  9 4.986831 1.5637683 0.5212561

shapiro.test(pc5$ratio)
#W = 0.82911, p-value = 2.486e-07   # NOT normally distributed 
logratio<-log(pc5$ratio) # log transform ratio to avoid violating ANCOVA assumptions

# ANCOVA
spp<-aov(macronutrient~species, data=pc5)
summary(spp)
#            Df Sum Sq Mean Sq F value   Pr(>F)    
# species      3  9.351  3.1169   11.62 3.72e-06 ***
# Residuals   63 16.903  0.2683 
TukeyHSD(spp)
#                               diff        lwr        upr     p adj
# ficus-indica-engelmannii -1.1317257 -1.6951239 -0.5683276 0.0000092 ***
# macrorhiza-engelmannii   -0.1157844 -0.5446874  0.3131187 0.8918197
# stricta-engelmannii       0.3117769 -0.1962606  0.8198143 0.3752342
# macrorhiza-ficus-indica   1.0159414  0.3831864  1.6486963 0.0004288 ***
# stricta-ficus-indica      1.4435026  0.7546463  2.1323590 0.0000039 ***
# stricta-macrorhiza        0.4275613 -0.1564451  1.0115676 0.2252723
#


### ------------------------------------------------------------------------------------
### ------------------------------------------------------------------------------------
### O. engelmannii population C:P Composition Analysis (FIGURE 3B)

dats<-load(file='geometry.data_6-24-20_RData')
dats
head(pops3) 
rbind.data.frame(travis)

# calculate carb:protein ratio for analysis 
POPratio<-pops3$carbohydrate/pops3$protein 
pops4<-cbind.data.frame(pops3,POPratio)
# pops5<-pops4[-c(28),] # outlier with carb:protein = 16, omission didn't change results

var.test(MATA$macronutrient,TRAV$macronutrient)
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
#  0.1801183 1.4028337
hist(pops4$macronutrient)
shapiro.test(pops4$POPratio)
# W = 0.72485, p-value = 5.481e-07  # NOT normally distributed 
logratio<-log(pops4$POPratio) # log transform ratio to avoid violating ANCOVA assumptions

# ANCOVA
POPS<-aov(logratio~county, data=pops4) # log transformed b/c data not normally distributed
summary(POPS) 
#             Df Sum Sq Mean Sq F value Pr(>F)  
# county       3  2.221  0.7404   3.116 0.0393 *
# Residuals   33  7.843  0.2377  
TukeyHSD(POPS)
#                                         diff        lwr       upr     p adj
# Travis Co. Mel-Travis Co.          0.1067694 -0.4334524 0.6469912 0.9499813
# Matagorda Co.-Travis Co.           0.3921585 -0.2260412 1.0103583 0.3318503
# Matagorda Co. Cacto-Travis Co.     0.6754069  0.0245822 1.3262316 0.0395629
# Matagorda Co.-Travis Co. Mel       0.2853891 -0.3521772 0.9229555 0.6245245
# Matagorda Co. Cacto-Travis Co. Mel 0.5686375 -0.1006104 1.2378854 0.1190458
# Matagorda Co. Cacto-Matagorda Co.  0.2832483 -0.4503892 1.0168859 0.7249272



### ------------------------------------------------------------------------------------
### ------------------------------------------------------------------------------------
### Species Macronutrient Comparison Analysis (FIGURE 3C)

dats<-load(file='geometry.data_6-24-20_RData')
dats

### 
m=glm(macronutrient ~ species,
      data = pc4,family="gaussian")
summary(m)
#                     Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          43.4799     1.8313  23.743   <2e-16 ***
# speciesficus-indica  -8.1683     4.5913  -1.779   0.0801 .  
# speciesmacrorhiza     0.2956     3.4952   0.085   0.9329    
# speciesstricta       -6.0304     4.1401  -1.457   0.1502 

library(multcomp)
summary(glht(m, linfct=mcp(species="Tukey")))
# ficus-indica - engelmannii == 0  -8.1683     4.5913  -1.779    0.275
# macrorhiza - engelmannii == 0     0.2956     3.4952   0.085    1.000
# stricta - engelmannii == 0       -6.0304     4.1401  -1.457    0.454
# macrorhiza - ficus-indica == 0    8.4639     5.1565   1.641    0.346
# stricta - ficus-indica == 0       2.1379     5.6137   0.381    0.980
# stricta - macrorhiza == 0        -6.3260     4.7592  -1.329    0.535
# 


### ------------------------------------------------------------------------------------
### ------------------------------------------------------------------------------------
### Opuntia engelmannii Population Macronutriet Comparison  (FIGURE 3D)

### 
MATA<-pops4[25:37,]
TRAV<-pops4[1:24,]
nrow(TRAV)
# summary stats 
x<-MATA$POPratio
mean(x)
se(x)

stats2 <- ddply(TRAV, c("county"), summarise,
               N    = length(macronutrient),
               mean = mean(macronutrient),
               sd   = sd(macronutrient),
               se   = sd / sqrt(N)
)
stats2
# 
cac<-aov(macronutrient~county,data = MATA)
summary(cac)

var.test(MATA$macronutrient,TRAV$macronutrient)
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
#  0.1801183 1.4028337
nrow(pops4) # 37
t.test(MATA$macronutrient,TRAV$macronutrient, 
       var.equal = TRUE,paired = FALSE,alternative = 'two.sided')
# t = 2.087, df = 35, p-value = 0.04423


### ------------------------------------------------------------------------------------
### ------------------------------------------------------------------------------------
### Mucilage test for significance

muc=read.csv('mucilage.csv')
View(muc)
 
m1<-aov(measure1~species,data=muc)
summary(m1)



### ------------------------------------------------------------------------------------



### ------------------------------------------------------------------------------------
### ------------------------------------------------------------------------------------
### CITE R Core Team 
citation('ggplot2')

#To cite R in publications use:

#  R Core Team (2018). R: A language and environment for statistical
# computing. R Foundation for Statistical Computing, Vienna, Austria.
# URL https://www.R-project.org/.


####################################################################################################
