# Paul GOUJON
# SY19 - UTC
# TP2 ex1

# Linear regression

# install the regression package
install.packages("FNN");
library("FNN")

# package for calculating mean square error
install.packages("hydroGOF")
library("hydroGOF")

# clear workspace
rm(list=ls())

# read dataset
data = read.table("/Users/Polo/Google Drive/SY19/TP3/data/x.data");

# checked if the 32nd individual is still wrong, OK.

# x is data
x = data[,1:8]

# z is labels 
z = data[,9:10]

# sx is scaled data (mean 0) and variance 96 ?
sx = as.data.frame(scale(x, T, T))

# train / test separating
Xtrain = sx[which(z$train == T),]
Xtst = sx[which(z$train == F),]
ztrain = z[which(z$train == T),]

# Q1 - training linear regression model
model = lm(z$lpsa ~ sx$lcavol + sx$lweight + sx$age + sx$lbph + sx$svi + sx$lcp + sx$gleason + sx$pgg45)
# 
# > summary(model)
# 
# Call:
#     lm(formula = z$lpsa ~ sx$lcavol + sx$lweight + sx$age + sx$lbph + 
#            sx$svi + sx$lcp + sx$gleason + sx$pgg45)
# 
# Residuals:
#     Min       1Q   Median       3Q      Max 
# -1.76644 -0.35510 -0.00328  0.38087  1.55770 
# 
# Coefficients:
#     Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  2.47839    0.07102  34.895  < 2e-16 ***
#     sx$lcavol    0.66515    0.10352   6.425 6.55e-09 ***
#     sx$lweight   0.26648    0.08607   3.096  0.00263 ** 
#     sx$age      -0.15820    0.08252  -1.917  0.05848 .  
# sx$lbph      0.14031    0.08402   1.670  0.09848 .  
# sx$svi       0.31533    0.09985   3.158  0.00218 ** 
#     sx$lcp      -0.14829    0.12566  -1.180  0.24115    
# sx$gleason   0.03555    0.11218   0.317  0.75207    
# sx$pgg45     0.12572    0.12312   1.021  0.31000    
# ---
#     Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.6995 on 88 degrees of freedom
# Multiple R-squared:  0.6634,	Adjusted R-squared:  0.6328 
# F-statistic: 21.68 on 8 and 88 DF,  p-value: < 2.2e-16
# 


# TO DO : is the regression significative ? => Answer


# Q2 - confidence intervals, level 95%

ic = confint(level = 0.95, model)
# > summary(ic)
# 2.5 %              97.5 %        
# Min.   :-0.39800   Min.   :0.005801  
# 1st Qu.:-0.18738   1st Qu.:0.258477  
# Median :-0.02666   Median :0.370395  
# Mean   : 0.21731   Mean   :0.609452  
# 3rd Qu.: 0.11691   3rd Qu.:0.513751  
# Max.   : 2.33724   Max.   :2.619531  

# => Confidence interval on lpsa value

# Q3 - Plotting 

# When using the whole dataset as train example
# and using the training set to make the prediction
pred = predict.lm(model)

pdf("./plots/plot1.pdf")
plot(z$lpsa, pred, main="Prediction in function of actual value (Every sample as train data)", xlab="Prediction value", ylab="Actual value")
abline(0,1)
dev.off()


# When using only the train samples for training
# And plotting the test sample

# model2 = lm(ztrain$lpsa ~ Xtrain$lcavol + Xtrain$lweight + Xtrain$age + Xtrain$lbph + Xtrain$svi + Xtrain$lcp + Xtrain$gleason + Xtrain$pgg45)
datareg= cbind(ztrain$lpsa, Xtrain)
names(datareg)[1] = "lpsa"
model2=lm(lpsa~. , data=datareg)
# note : lpsa~.    <=>   "lpsa against all"
# in order for this to work, give a dataframe with the right structure
pred2 = predict.lm(model2,newdata= Xtst)


pdf("./plots/plot2.pdf")
plot(as.vector(ztst$lpsa), as.vector(pred2), main="Prediction in function of actual value (Train for train, test for pred)", xlab="Prediction value", ylab="Actual value")
abline(0,1)
dev.off()

# Q3 - Residuals study
# Notes a propos de la non linéarité des résidus : 
# On peut tracer Y en fonction de chaque Xj, pour voir s'il y a correlation entre les résidus 
# et certaines variables => possibilité de réfléchir à partir de là à un meilleur modèle

# CORRECTION
plot(prostate$lpsa, fitted(reg))

plot residus rstandard(reg) en fonction de prostate$lpsa

dans formula :
lpsa ~ . -train (on enleve la variable train)

"On a des données qui sont pas mal ici, c'est juste qu'on a des résidus plutot négatifs sur les premières valeurs, plutot positifs sur les dernieres mais blablabla c'est pas trop degueux"


qqnorm(resid(reg)) : diagramme de normalité
qqline(resid(reg)) : ligne de normalité

analyse du diagramme : au centre de la distribution globalement ça suit bien la loi normale, mais
aux extremités c'est pas ouf

=> possibilité de revoir les hypothèses, les préciser pour mieux coller au dataset
=> necessité d'etre bien attentif aux intervalles de confiance etc   
=> possibilité d'utiliser le bootstrapping 

Q6
hist(hatvalues(reg))
hist(cooks.distance(reg))

Q7
prend un exemple de model different, en prenant un nombre de variables different
prochain chapitre du cours on va voir comment choisir les variables de manière scientifique
On compare les Adjusted R-squared pour comparer les differents models

Q8 
reg2 = lm (lpsa ~lcavol +lweight + I(lweight^2) + svi , data = prostate)
=> ici on étudie de la transformation de variable, on a squared l'attribut lweight




