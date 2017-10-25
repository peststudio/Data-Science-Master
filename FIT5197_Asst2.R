# FIT9157 Assessment 1
# Student: Dong Lei Qian
# ID: 29115566

# First read the data file
data = read.csv('FIT5197_2ndSem2017_Qu1.csv')
# Check what is in the data
str(data)
# data.frame':	900 obs. of  3 variables:
# $ x: num  9.936 -0.395 4.732 -10.413 2.167 ...
# $ y: num  15.7 21.8 20.8 23.7 12 ...
# $ z: num  -1.42475 -3.70484 3.27558 -0.00465 5.15928 ...

# plot the data pairwise to have a look
plot(data[c("x","y","z")])

# can also plot 3D
library(rgl)
plot3d(data$x,data$y,data$z)

# Import library for evaluation
library(clusterCrit)

# try k = 2
kc2 = kmeans(data,2)

# plot the data
plot(data[c("x","y")], col= kc2$cluster)
points(kc2$centers[, c("x", "y")], col = 20:21, pch = 8, cex = 2)

plot(data[c("y","z")], col =kc2$cluster)
points(kc2$centers[, c("y","z")], col = 20:21, pch = 8, cex = 5)

plot(data[c("x","z")], col =kc2$cluster)
points(kc2$centers[, c("x", "z")], col = 20:21, pch = 8, cex = 2)

# and in 3D
plot3d(data[c("x","y","z")],col = kc2$cluster)
points3d(kc2$centers[, c("x", "y", "z")], col = 20:21, pch = 8, cex = 2)

# Computer Davies-Bouldin index
intEvaluations2 = intCriteria(as.matrix(data), kc2$cluster, "all")
intEvaluations2$davies_bouldin
# 1.053575

# It looks like there should be more clusters
# Let's try 3 clusters
kc3 = kmeans(data, 3)

plot(data[c("x","y")], col = kc3$cluster)
points(kc3$centers[, c("x", "y")], col = 20:21:22, pch = 8, cex = 2)

plot(data[c("y","z")], col = kc3$cluster)
points(kc3$centers[, c("y","z")], col = 20:21:22, pch = 8, cex = 2)

plot(data[c("x","z")], col = kc3$cluster)
points(kc3$centers[, c("x", "z")], col = 20:21:22, pch =8, cex = 2)

# and in 3D
plot3d(data[c("x","y","z")], col = kc3$cluster)
points3d(kc3$centers[, c("x","y","z")], col = 20:21:22, pch =8, cex = 2)

intEvaluations3 = intCriteria(as.matrix(data), kc3$cluster, "all")
intEvaluations3$davies_bouldin
# 0.7606554

# What about 4
kc4 = kmeans(data, 4)

plot(data[c("x","y")], col = kc4$cluster)
points(kc4$centers[, c("x", "y")], col = 20:21:22:23, pch = 8, cex = 2)

plot(data[c("y","z")], col = kc4$cluster)
points(kc4$centers[, c("y","z")], col = 20:21:22:23, pch = 8, cex = 2)

plot(data[c("x","z")], col = kc4$cluster)
points(kc4$centers[, c("x", "z")], col = 20:21:22:23, pch =8, cex = 2)

# and in 3D
plot3d(data[c("x","y","z")], col = kc4$cluster)
points3d(kc4$centers[, c("x", "y", "z")], col = 20:21:22:23, pch =8, cex = 2)

intEvaluations4 = intCriteria(as.matrix(data), kc4$cluster, "all")
intEvaluations4$davies_bouldin
# 1.314163

# The best one is 3 clusters

# We can also run run 2:10 and compare all davis_bouldin index
# use k-means for 2-10 clusters
range = 2:10
davies_bouldin_vector = integer(9) # initialise empty vector
for (i in range){
  kc =  kmeans(data, centers = i)
  intEvaluations = intCriteria(as.matrix(data), kc$cluster, "all")
  davies_bouldin_vector[i-1] = intEvaluations$davies_bouldin
}
# plot davis_bouldin index against k
plot(range,davies_bouldin_vector,xlab='k',ylab='Davies-bouldin index')
# 3 is the lowest

# Another criteria is using within group sum of squares measure and look for 'elbow' point
withinness = integer(9) # initialise empty vector
for (i in range){
  kc =  kmeans(data, centers = i)
  withinness[i-1] = kc$tot.withinss
}
plot(range,withinness,type='b',xlab='k',ylab = 'within group sum of squares')

# Tune the parameters of k-means algorithm and see if the results improves, also run algorithm 100 times to get best result
tries = 100
daives_bouldin = 10 # initialise min value
for (i in 1:tries){
  kc3 = kmeans(data, 3,iter.max = 100,nstart = 10)
  intEvaluations3 = intCriteria(as.matrix(data), kc3$cluster, "all")
  if  (intEvaluations3$davies_bouldin < davies_bouldin) {
    davies_bouldin = intEvaluations3$davies_bouldin
    kc = kc3
  }
}
davies_bouldin
# 0.7606554
#Running multiple times gives different results, turning the parameters themselves is not improving results
kc
# K-means clustering with 3 clusters of sizes 248, 140, 512
# Cluster means:
#  x         y          z
# 1  13.525871 -6.127266 17.8445450
# 2   2.075944 18.887183  0.4222016
# 3 -15.818306  7.927049 27.5040425
# Within cluster sum of squares by cluster:
#  [1] 48983.28 29753.72 98154.56
# (between_SS / total_SS =  62.7 %)

# Add cluster classifier to data as an extra column, use this factor for F test
data$cluster = kc3$cluster
data$cluster = factor(data$cluster)

# first look at boxplot to see if means are same
boxplot(x ~ cluster, data=data)
# The means are quite different
# Next setup analyis of variance test, because data is multidimensional, use manova
res = manova(cbind(x, y, z) ~ cluster, data = data)
summary(res)
# Df Pillai approx F num Df den Df    Pr(>F)    
# cluster     2 1.3918   683.51      6   1792 < 2.2e-16 ***
# Residuals 897                                            
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# p value is very small so we reject the null hypothesis that all the means are the same

# manova can not work together with Tukey test, so use anova instead
resx = aov(x ~ cluster, data=data)
summary(resx)
# Df Sum Sq Mean Sq F value Pr(>F)    
# cluster       2 152045   76023    1021 <2e-16 ***
#  Residuals   897  66775      74                   
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# p value is very small so we reject the null hypothesis that all the means are the same
# Next look at Tukey test to see pairwise comparison
tukx = TukeyHSD(res)
tukx
# Tukey multiple comparisons of means
# 95% family-wise confidence level
# Fit: aov(formula = x ~ cluster, data = data)
# $cluster
# diff       lwr        upr p adj
# 2-1 -11.44993 -13.59115  -9.308705     0
# 3-1 -29.34418 -30.91122 -27.777131     0
# 3-2 -17.89425 -19.82604 -15.962458     0

# p value is 0 for all pairs, we can reject the null hypothesis any pairs have the same mean
# plot Tukey
plot(tukx)

# Do the same for y and z
boxplot(y ~ cluster, data=data)
resy = aov(y ~ cluster, data=data)
summary(resy)
# Df Sum Sq Mean Sq F value Pr(>F)    
# cluster       2  61574   30787   527.1 <2e-16 ***
# Residuals   897  52390      58                   
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# p is so small we reject the null hypothesis that the means are the same
tuky = TukeyHSD(resy)
tuky
# Tukey multiple comparisons of means
# 95% family-wise confidence level
# Fit: aov(formula = y ~ cluster, data = data)
# $cluster
# diff       lwr       upr p adj
# 2-1  25.01445  23.11784 26.911062     0
# 3-1  14.05432  12.66629 15.442345     0
# 3-2 -10.96013 -12.67124 -9.249026     0
# p value is 0 for all pairs, we can reject the null hypothesis any pairs have the same mean
plot(tuky)

boxplot(z ~ cluster, data=data)
resz = aov(z ~ cluster, data=data)
summary(resz)
# Df Sum Sq Mean Sq F value Pr(>F)    
# cluster       2  83287   41644   647.1 <2e-16 ***
# Residuals   897  57727      64                   
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# p is so small we reject the null hypothesis that the means are the same
tukz = TukeyHSD(resz)
tukz
# Tukey multiple comparisons of means
# 95% family-wise confidence level
# Fit: aov(formula = z ~ cluster, data = data)
# $cluster
# diff        lwr       upr p adj
# 2-1 -17.422343 -19.413214 -15.43147     0
# 3-1   9.659498   8.202486  11.11651     0
# 3-2  27.081841  25.285695  28.87799     0
# p value is 0 for all pairs, we can reject the null hypothesis any pairs have the same mean
plot(tukz)

# remove the extra column in data
data = data[c("x","y","z")]
data

# Q2
# use mclust library for Gaussian mixture modelling
library(mclust)
mc = Mclust(data)
mc$G
# 3

# plot the clusters for each of x y and z
plot(data[c("x", "y")], col = mc$classification)
points(t(mc$parameters$mean)[, c("x", "y")], col = 20:21:22, pch = 8, cex = 2)
plot(data[c("y", "z")], col = mc$classification)
points(t(mc$parameters$mean)[, c("y", "z")], col = 20:21:22, pch = 8, cex = 2)
plot(data[c("x", "z")], col = mc$classification)
points(t(mc$parameters$mean)[, c("x", "z")], col = 20:21:22, pch = 8, cex = 2)

# and in 3d
plot3d(data[c("x","y","z")], col = mc$classification)
points(t(mc$parameters$mean)[, c("x","y","z")], col = 20:21:22, pch = 8, cex = 2)

# calculate davies_bouldin index
intEvaluations = intCriteria(as.matrix(data), as.integer(mc$classification), "all")
intEvaluations$davies_bouldin
# 0.7498929, better than k-means

# Tune the parameters to find best model
mc = Mclust(data, G=c(2:10), modelNames=mclust.options("emModelNames"))
print(mc)
# 'Mclust' model object:
#  best model: diagonal, equal volume, varying shape (EVI) with 3 components

# look at plot
plot(mc)

# Now use parameters found
mc = Mclust(data,G=3,modelNames = 'EVI')
intEvaluations = intCriteria(as.matrix(data), as.integer(mc$classification), "all")
intEvaluations$davies_bouldin
# This is same as before
summary(mc)
#  log.likelihood   n df       BIC       ICL
# -10196.12 900 18 -20514.68 -20597.43
# Clustering table:
#      1   2   3 
# 124 549 227 
mc$parameters
# aov test on mclust, first add cluster column
data$cluster = mc$classification
data$cluster = factor(data$cluster)

# look at boxplot to see if means are same
boxplot(x ~ cluster, data=data)
# The means are quite different

# Next setup analyis of variance test
resx = aov(x ~ cluster, data=data)
summary(resx)
# Df Sum Sq Mean Sq F value Pr(>F)    
# cluster       2 142778   71389   842.1 <2e-16 ***
# Residuals   897  76042      85                   
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# p value is small so we reject the null hypothesis the means are all the same for each cluster

tukx = TukeyHSD(resx)
tukx
# Tukey multiple comparisons of means
# 95% family-wise confidence level
# Fit: aov(formula = x ~ cluster, data = data)
# $cluster
# diff       lwr       upr p adj
# 2-1 -16.56542 -18.71457 -14.41627     0
# 3-1  12.51756  10.10385  14.93128     0
# 3-2  29.08298  27.37734  30.78862     0

# p value is 0 for all pairs, we can reject the null hypothesis any pairs have the same mean

# plot Tukey
plot(tukx)

# Next do this for y
boxplot(y ~ cluster, data=data)
# The means are quite different

# Next setup analyis of variance test
resy = aov(y ~ cluster, data=data)
summary(resy)
# Df Sum Sq Mean Sq F value Pr(>F)    
# cluster       2  63119   31560   556.8 <2e-16 ***
# Residuals   897  50844      57                   
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# p value is small so we reject the null hypothesis the means are all the same for each cluster

tuky = TukeyHSD(resy)
tuky
# Tukey multiple comparisons of means
# 95% family-wise confidence level
# Fit: aov(formula = y ~ cluster, data = data)
# $cluster
# diff       lwr       upr p adj
# 2-1 -12.34395 -14.10131 -10.58659     0
# 3-1 -26.83400 -28.80770 -24.86031     0
# 3-2 -14.49005 -15.88476 -13.09535     0

# p value is 0 for all pairs, we can reject the null hypothesis any pairs have the same mean

# plot Tukey
plot(tuky)

# and z
boxplot(z ~ cluster, data=data)
# The means are quite different

# Next setup analyis of variance test
resz = aov(z ~ cluster, data=data)
summary(resz)
# Df Sum Sq Mean Sq F value Pr(>F)    
# cluster       2  85394   42697   688.6 <2e-16 ***
# Residuals   897  55620      62                   
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# p value is small so we reject the null hypothesis the means are all the same for each cluster

tuky = TukeyHSD(resz)
tukz
# Tukey multiple comparisons of means
# 95% family-wise confidence level
# Fit: aov(formula = z ~ cluster, data = data)
# $cluster
# diff        lwr       upr p adj
# 2-1 -17.422343 -19.413214 -15.43147     0
# 3-1   9.659498   8.202486  11.11651     0
# 3-2  27.081841  25.285695  28.87799     0

# p value is 0 for all pairs, we can reject the null hypothesis any pairs have the same mean

# plot Tukey
plot(tukz)

# manova test for overall
res = manova(cbind(x,y,z) ~ cluster, data=data)
summary(res)
#           Df Pillai approx F num Df den Df    Pr(>F)    
# cluster     2 1.3752   657.34      6   1792 < 2.2e-16 ***
# Residuals 897                                            
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# p value is small so we reject null hypothesis all means are the same for 3 dimentional data

# Exercise 3
# Read Data
disease = read.csv('chronic_kidney_disease.csv')
str(disease)
# There are 400 obs. and 25 variables including 1 response variable

# Change response variable to number 1/0
disease$class = as.numeric(disease$class)
disease$class = disease$class %% 2

# Look at missing values
sapply(disease,function(x) sum(is.na(x)))
# age    bp    sg    al    su   rbc    pc   pcc    ba   bgr    bu    sc   sod   pot  hemo   pcv  wbcc  rbcc   htn    dm   cad 
# 9    12    47    46    49   152    65     4     4    44    19    17    87    88    52    71   106   131     2     2     2 
# appet    pe   ane class 
# 1     1     1     0 

# Drop columns with over 100 mising values
disease1 <- subset(disease, select = -c(rbc,rbcc,wbcc))

# change all variable to numeric so we can do correlation analysis
disease1$ane = as.numeric(disease1$ane)
disease1$pe = as.numeric(disease1$pe)
disease1$appet = as.numeric(disease1$appet)
disease1$cad = as.numeric(disease1$cad)
disease1$dm = as.numeric(disease1$dm)
disease1$htn = as.numeric(disease1$htn)
disease1$ba = as.numeric(disease1$ba)
disease1$pcc = as.numeric(disease1$pcc)
disease1$pc = as.numeric(disease1$pc)

# Calculate correlation between variables
library('psych')
corr <- cor.ci(disease1,method = 'spearman')
# drop columns that have high correlation with other columns
disease1 <- subset(disease1, select = -c(pcv,sc))

# impute for missing values using median
library('Hmisc')
disease2 = disease1
disease2$age <- as.numeric(impute(disease2$age,median))
disease2$bp <- as.numeric(impute(disease2$bp,median))
disease2$sg <- as.numeric(impute(disease2$sg,median))
disease2$al <- as.numeric(impute(disease2$al,median))
disease2$su <- as.numeric(impute(disease2$su,median))
disease2$pc <- as.numeric(impute(disease2$pc,median))
disease2$pcc <- as.numeric(impute(disease2$pcc,median))
disease2$ba <- as.numeric(impute(disease2$ba,median))
disease2$bgr <- as.numeric(impute(disease2$bgr,median))
disease2$bu <- as.numeric(impute(disease2$bu,median))
disease2$sod <- as.numeric(impute(disease2$sod,median))
disease2$pot <- as.numeric(impute(disease2$pot,median))
disease2$htn <- as.numeric(impute(disease2$htn,median))
disease2$dm <- as.numeric(impute(disease2$dm,median))
disease2$cad <- as.numeric(impute(disease2$cad,median))
disease2$appet <- as.numeric(impute(disease2$appet,median))
disease2$pe <- as.numeric(impute(disease2$pe,median))
disease2$ane <- as.numeric(impute(disease2$ane,median))
disease2$hemo <- as.numeric(impute(disease2$hemo,median))

# check correlation again
corr <- cor.ci(disease2,method = 'spearman')

#split into train and test
set.seed(100)
testIndices = sample(1:nrow(disease2), nrow(disease2)*0.1)

# set training and testing data
disease.training = disease2[-testIndices,] 
disease.test = disease2[testIndices,] 

# try to fit the model
model <- glm(class ~ ., disease.training, family = binomial)
# Warning messages:
# 1: glm.fit: algorithm did not converge 
# 2: glm.fit: fitted probabilities numerically 0 or 1 occurred 
summary(model)
# Call:
# glm(formula = class ~ ., family = binomial, data = disease.training)
# Deviance Residuals: 
# Min          1Q      Median          3Q         Max  
# -2.183e-04  -2.100e-08   2.100e-08   2.100e-08   1.944e-04  
# Coefficients:
# Estimate Std. Error z value Pr(>|z|)
# (Intercept)  1.722e+04  4.497e+06   0.004    0.997
# age         -6.082e-02  6.810e+02   0.000    1.000
# bp          -2.133e-02  1.456e+03   0.000    1.000
# sg          -1.652e+04  4.374e+06  -0.004    0.997
# al           5.410e+01  5.270e+04   0.001    0.999
# su          -1.664e+01  2.668e+04  -0.001    1.000
# pc          -3.788e+01  1.337e+05   0.000    1.000
# pcc         -1.232e+02  1.159e+05  -0.001    0.999
# ba          -6.433e+01  1.848e+05   0.000    1.000
# bgr          3.348e-01  2.834e+02   0.001    0.999
# bu           1.138e+00  5.374e+02   0.002    0.998
# sod         -2.383e-01  1.191e+03   0.000    1.000
# pot          1.630e+01  2.357e+04   0.001    0.999
# hemo        -3.023e+01  6.650e+03  -0.005    0.996
# htn         -1.700e+01  6.559e+04   0.000    1.000
# dm           7.154e+01  9.929e+04   0.001    0.999
# cad          7.070e+01  7.124e+04   0.001    0.999
# appet        4.056e+01  3.824e+04   0.001    0.999
# pe           4.664e+01  6.449e+04   0.001    0.999
# ane         -5.561e+01  1.106e+05  -0.001    1.000
# The algorithm didn't converge, z values are small and p values are large, meaning the varaibles do not explain the model well
# (Dispersion parameter for binomial family taken to be 1)
# Null deviance: 4.7633e+02  on 359  degrees of freedom
# Residual deviance: 1.4545e-07  on 340  degrees of freedom
# AIC: 40
# Number of Fisher Scoring iterations: 25

# Pick just the top 3 varaibles that have high correlation with the response variable
model <- glm(class~ al+sg+hemo,data=disease.training, family = 'binomial') 
summary(model)
# Call:
# glm(formula = class ~ al + sg + hemo, family = "binomial", data = disease.training)
# Deviance Residuals: 
# Min       1Q   Median       3Q      Max  
# -1.7144  -0.0032   0.0000   0.0000   3.4643  
# Coefficients:
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  1478.6951   442.2163   3.344 0.000826 ***
# al             24.0472  2329.4716   0.010 0.991764    
# sg          -1411.1479   426.4835  -3.309 0.000937 ***
# hemo           -3.0131     0.7105  -4.241 2.23e-05 ***
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# (Dispersion parameter for binomial family taken to be 1)
# Null deviance: 476.33  on 359  degrees of freedom
# Residual deviance:  42.60  on 356  degrees of freedom
# AIC: 50.6
# Number of Fisher Scoring iterations: 22
# From the p value we can see that al does not explain the variable well, but sg and hemo are good candidates

model <- glm(class~ sg+hemo,data=disease.training, family = 'binomial') 
summary(model)
# Call:
# glm(formula = class ~ sg + hemo, family = "binomial", data = disease.training)
# Deviance Residuals: 
# Min        1Q    Median        3Q       Max  
# -1.89452  -0.06919   0.00274   0.04983   2.73969  
# Coefficients:
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  654.8470   123.9812   5.282 1.28e-07 ***
# sg          -616.3340   119.8916  -5.141 2.74e-07 ***
# hemo          -1.9426     0.2983  -6.511 7.45e-11 ***
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# (Dispersion parameter for binomial family taken to be 1)
# Null deviance: 476.326  on 359  degrees of freedom
# Residual deviance:  85.991  on 357  degrees of freedom
# AIC: 91.991
# Number of Fisher Scoring iterations: 8

# To predict 
predicted <- round(predict(model,disease.test,type='response'))

# Get confusion matrix
confusion <- as.matrix(table('Actual'= disease.test$class, 'Prediction'=predicted))
confusion
# Prediction
# Actual  0  1
# 0 15  0
# 1  2 23

# Calculate accuray, precision, recall, and f values
diag <- diag(confusion)
accuray <- sum(diag) / 40
accuray
# 0.95
rowsums = apply(confusion, 1, sum)	# number of observations per class
colsums = apply(confusion, 2, sum)
Precision = diag / colsums
Recall = diag / rowsums
F1 = 2 * Precision * Recall / (Precision + Recall) 
Precision
# 0         1 
# 0.8823529 1.0000000 
Recall
# 0    1 
# 1.00 0.92 
F1
# 0         1 
# 0.9375000 0.9583333 

# plot ROC curve and calculate AUD
library(ROCR)
pr <- prediction(predicted, disease.test$class)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
# 0.88

# 10 fold average
library(caret)
set.seed(100)
folds <- createFolds(disease2$class, k=10)
accuracy = double(10)
precision = double(10)
recall = double(10)
f1 = double(10)
auc = double(10)
aic = double(10)
for (i in 1:10){
  model <- glm(class ~ hemo+sg, data=disease2[-folds[[i]],], family = 'binomial')
  aic[i] = model$aic
  predicted <- round(predict(model,disease2[folds[[i]],],type = 'response'))
  confusion <- as.matrix(table('Actual'= disease2[folds[[i]],]$class, 'Prediction'=predicted))
  diag <- diag(confusion)
  accuracy[i] = sum(diag) / length(predicted)
  rowsums = apply(confusion, 1, sum)	# number of observations per class
  colsums = apply(confusion, 2, sum)
  precision[i] =mean(diag / colsums)
  recall[i] = mean(diag / rowsums)
  f1[i] = mean(2 * Precision * Recall / (Precision + Recall))
  pr <- prediction(predicted, disease2[folds[[i]],]$class)
  Auc <- performance(pr, measure = "auc")
  auc[i] <- Auc@y.values[[1]]
}
mean(precision)
# 0.9378618
mean(recall)
# 0.9407591
mean(accuracy)
# 0.9425
mean(f1)
# 0.8901099
mean(auc)
# 0.9407591
mean(aic)
# 98.28435

# Try a different method for implementing missing values
# use KNN
library('VIM')
disease2 <- kNN(disease1)

# same steps as before
disease.training = disease2[-testIndices,] 
disease.test = disease2[testIndices,] 
model <- glm(class~ sg+hemo,data=disease.training, family = 'binomial') 
summary(model)
# Call:
# glm(formula = class ~ sg + hemo, family = "binomial", data = disease.training)
# Deviance Residuals: 
# Min        1Q    Median        3Q       Max  
# -1.43750  -0.04220   0.00058   0.01895   2.88737  
# Coefficients:
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  790.7246   164.0909   4.819 1.44e-06 ***
# sg          -746.7195   157.4517  -4.743 2.11e-06 ***
# hemo          -2.1574     0.4117  -5.240 1.61e-07 ***
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# (Dispersion parameter for binomial family taken to be 1)
# Null deviance: 476.326  on 359  degrees of freedom
# Residual deviance:  54.201  on 357  degrees of freedom
# AIC: 60.201
# Number of Fisher Scoring iterations: 9

predicted <- round(predict(model,disease.test,type='response'))
confusion <- as.matrix(table('Actual'= disease.test$class, 'Prediction'=predicted))
confusion
# Prediction
# Actual  0  1
# 0 14  1
# 1  1 24

# Calculate accuray, precision, recall, and f values
diag <- diag(confusion)
accuray <- sum(diag) / 40
accuray
# 0.95
rowsums = apply(confusion, 1, sum)	# number of observations per class
colsums = apply(confusion, 2, sum)
Precision = diag / colsums
Recall = diag / rowsums
F1 = 2 * Precision * Recall / (Precision + Recall) 
Precision
# 0         1 
# 0.9333333 0.9600000 
Recall
# 0         1 
# 0.9333333 0.9600000 
F1
# 0         1 
# 0.9333333 0.9600000 

pr <- prediction(predicted, disease.test$class)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
# 0.94

# 10 fold average
accuracy = double(10)
precision = double(10)
recall = double(10)
f1 = double(10)
auc = double(10)
aic = double(10)
for (i in 1:10){
  model <- glm(class ~ hemo+sg, data=disease2[-folds[[i]],], family = 'binomial')
  aic[i] = model$aic
  predicted <- round(predict(model,disease2[folds[[i]],],type = 'response'))
  confusion <- as.matrix(table('Actual'= disease2[folds[[i]],]$class, 'Prediction'=predicted))
  diag <- diag(confusion)
  accuracy[i] = sum(diag) / length(predicted)
  rowsums = apply(confusion, 1, sum)	# number of observations per class
  colsums = apply(confusion, 2, sum)
  precision[i] =mean(diag / colsums)
  recall[i] = mean(diag / rowsums)
  f1[i] = mean(2 * Precision * Recall / (Precision + Recall))
  pr <- prediction(predicted, disease2[folds[[i]],]$class)
  Auc <- performance(pr, measure = "auc")
  auc[i] <- Auc@y.values[[1]]
}
mean(precision)
# 0.9594111
mean(recall)
# 0.9661476
mean(accuracy)
# 0.965
mean(f1)
# 0.8901099
mean(auc)
# 0.9661476
mean(aic)
# 63.22274

# 3b
# use rpart for tree model
library('rpart')
# read data again
disease = read.csv('chronic_kidney_disease.csv')

disease1 = kNN(disease)
# split to train/test
disease.training = disease1[-testIndices,] 
disease.test = disease1[testIndices,] 
#build tree model
tree = rpart(formula = class ~ .,data=disease.training,method = "class")
printcp(tree)
# Classification tree:
# rpart(formula = class ~ ., data = disease.training, method = "class")
# Variables actually used in tree construction:
# [1] hemo sg  
# Root node error: 135/360 = 0.375
# n= 360 
#       CP nsplit rel error   xerror     xstd
# 1 0.86667      0   1.00000 1.000000 0.068041
# 2 0.10370      1   0.13333 0.155556 0.032940
# 3 0.01000      2   0.02963 0.051852 0.019407

# tree model actaully tells us which are the significant prediction variables, hemo and sg
tree = rpart(formula = class ~ hemo+sg,data=disease.training,method = "class")
summary(tree)
# Call:
# rpart(formula = class ~ hemo + sg, data = disease.training, method = "class")
# n= 360 
# CP nsplit  rel error     xerror       xstd
# 1 0.8666667      0 1.00000000 1.00000000 0.06804138
# 2 0.1037037      1 0.13333333 0.15555556 0.03294007
# 3 0.0100000      2 0.02962963 0.05185185 0.01940668
# Variable importance
# hemo   sg 
# 56   44 
# Node number 1: 360 observations,    complexity param=0.8666667
# predicted class=ckd     expected loss=0.375  P(node) =1
# class counts:   225   135
# probabilities: 0.625 0.375 
# left son=2 (213 obs) right son=3 (147 obs)
# Primary splits:
# hemo < 13.05  to the left,  improve=135.8957, (0 missing)
# sg   < 1.0175 to the left,  improve=105.8523, (0 missing)
# Surrogate splits:
# sg < 1.0175 to the left,  agree=0.842, adj=0.612, (0 split)
# Node number 2: 213 observations
# predicted class=ckd     expected loss=0.01408451  P(node) =0.5916667
# class counts:   210     3
# probabilities: 0.986 0.014 
# Node number 3: 147 observations,    complexity param=0.1037037
# predicted class=notckd  expected loss=0.1020408  P(node) =0.4083333
# class counts:    15   132
# probabilities: 0.102 0.898 
# left son=6 (14 obs) right son=7 (133 obs)
# Primary splits:
# sg   < 1.0175 to the left,  improve=24.953810, (0 missing)
# hemo < 13.35  to the left,  improve= 1.260718, (0 missing)
# Node number 6: 14 observations
# predicted class=ckd     expected loss=0  P(node) =0.03888889
# class counts:    14     0
# probabilities: 1.000 0.000 
# Node number 7: 133 observations
# predicted class=notckd  expected loss=0.007518797  P(node) =0.3694444
# class counts:     1   132
# probabilities: 0.008 0.992 

plotcp(tree)

plot(tree)
text(tree,use.n=TRUE, all=TRUE, cex=.8)

disease.test$class = as.numeric(disease.test$class)
disease.test$class = disease.test$class %% 2
# predict
predicted <- round(predict(tree,newdata = disease.test, method='class')[,1])

# confusion matrix
confusion <- as.matrix(table('Actual'= disease.test$class, 'Prediction'=predicted))
confusion
#          Prediction
# Actual  0  1
#          0 12  3
#          1  1 24

# Calculate accuray, precision, recall, and f values
diag <- diag(confusion)
accuray <- sum(diag) / 40
accuray
# 0.9
rowsums = apply(confusion, 1, sum)	# number of observations per class
colsums = apply(confusion, 2, sum)
Precision = diag / colsums
Recall = diag / rowsums
F1 = 2 * Precision * Recall / (Precision + Recall) 
Precision
# 0         1 
# 0.9230769 0.8888889 
Recall
# 0         1 
# 0.80 0.96 
F1
# 0         1 
# 0.8571429 0.9230769 

# ROC
pr <- prediction(predicted, disease.test$class)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
# 0.88

# 10 fold average
accuracy = double(10)
precision = double(10)
recall = double(10)
f1 = double(10)
auc = double(10)

for (i in 1:10){
  tree = rpart(formula = class ~ hemo+sg,data = disease1[-folds[[i]],],method = "class")
  disease.test = disease1[folds[[i]],]
  disease.test$class = as.numeric(disease.test$class)
  disease.test$class = disease.test$class %% 2 
  predicted <- round(predict(tree,newdata = disease.test, method='class')[,1])
  confusion <- as.matrix(table('Actual'= disease.test$class, 'Prediction'=predicted))
  diag <- diag(confusion) 
  accuracy[i] = sum(diag) / length(predicted)
  rowsums = apply(confusion, 1, sum)	# number of observations per class
  colsums = apply(confusion, 2, sum)
  precision[i] =mean(diag / colsums)
  recall[i] = mean(diag / rowsums)
  f1[i] = mean(2 * Precision * Recall / (Precision + Recall))
  pr <- prediction(predicted, disease2[folds[[i]],]$class)
  Auc <- performance(pr, measure = "auc")
  auc[i] <- Auc@y.values[[1]]
}
mean(precision)
# 0.9745008
mean(recall)
# 0.966853
mean(accuracy)
# 0.9725
mean(f1)
# 0.8901099
mean(auc)
# 0.966853
