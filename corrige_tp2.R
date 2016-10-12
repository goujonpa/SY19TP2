# Corrigé du TD 2


# Exercice 1

prostate<-read.table('/Users/Thierry/Documents/R/Data/SY19/prostate.data',header = TRUE)  
# changer le chemin d'accès aux données

summary(prostate)

reg<- lm(lpsa ~. - train ,data=prostate)

# Q1
summary(reg)

# Q2
confint(reg)

# Q3
plot(prostate$lpsa,fitted(reg))

# Q4

plot(prostate$lpsa, resid(reg))
plot(prostate$lpsa, rstandard(reg))
plot(prostate$lpsa, rstudent(reg))

plot(prostate$lcavol,rstandard(reg))
# etc.

reg1<-lm(lpsa ~. - train -lweight,data=prostate)
plot(prostate$lpsa, rstandard(reg1))
# Q5

qqnorm(resid(reg))
qqline(resid(reg))

# Q6

hist(hatvalues(reg))
hist(cooks.distance(reg))

#Q7


reg1<- lm(lpsa ~ lcavol+lweight+svi  ,data=prostate)
# etc.

# Q8
reg2<- lm(lpsa ~ lcavol+lweight+I(lweight^2) +svi  ,data=prostate)
# maintenant, les coefficients de lweight et lweight2 ne sont plus significatifs !


# Exercice 2

n<-20
x1<-rnorm(n,0,1)
x2<-rnorm(n,0,1)
X<-cbind(rep(1,n),x1,x2)
beta<-c(1,2,3)
Ey<- X %*% beta
sig<-0.5
N<-5000 # nombre d'exemples d'apprentissage
I<-matrix(0,N,3)
for(i in 1:N){
    y<-Ey+rnorm(n,0,sig) 
    reg<-lm(y ~ x1+x2)
    CI<-confint(reg)
    I[i,] <- (CI[,1] <= beta) & (CI[,2] >= beta)
}

colMeans(I) # niveaux de confiance des 3 intervalles de conf 
mean(apply(I,1,min)) # probabilité pour le produit cartésien des 3 intervalles de confiance 
# (ce n'est pas une région de confiance au niveau 95 %)


# Intervalles de confiance et de prédiction

x0<-c(0.9,0.9)
Ey0=beta[1]+0.9*beta[2]+0.9*beta[3]
N<-5000
IC<-rep(0,N)
IP<-IC

for(i in 1:N){
    y<-Ey+rnorm(n,0,sig) 
    y0<-Ey0+rnorm(1,0,sig)
    reg<-lm(y ~ x1+x2)
    int<-predict(reg,int="c",newdata=data.frame(x1=0.9,x2=0.9))
    IC[i]<-(int[,2] <= Ey0) & (int[,3] >= Ey0)
    int<-predict(reg,int="p",newdata=data.frame(x1=0.9,x2=0.9))
    IP[i]<-(int[,2] <= y0) & (int[,3] >= y0)
}

mean(IP)
mean(IC)
