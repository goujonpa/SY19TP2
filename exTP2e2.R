# Paul GOUJON
# SY19 - UTC
# TP2 ex2

# Confidence intervals & prediction
rm(list=ls())

beta = c(1, 5, 3)

# y esperance, (mean value)
Ey = X %*% beta

# std of error
sigma = 0.5

N = 5000
train = as.data.frame(matrix(nrow=100, ncol=100))
I = matrix(0,N,3)

for (i in 1:N) {
    y = Ey + rnorm(n, 0, sigma)
    reg = lm(y~x1+x2)
    Ci = confint(reg)
    I[,i] = (CI[,1] <= beta) & (CI[,2] >= beta)
}

colMeans(I) # niveau de conf des 3 intervalles de conf
mean(apply(I,1,min)) # probabilité pour que le produit cartésien des 3 intervales de confiance (ce n'est pas une région de confiance au niveau 95%)

"
Pour X fixé, on tire 5000 ensemble d'apprentissage
Et on vérifie que blabla



prediction

commence idem que tout à lheure
"

for (i in 1:N) {
    y = Ey + rnorm(n,0, sigma)
    y0 = Ey0 + rnorm(1,0,sigma)
    reg = lm(y ~x1 + x2)
    int = predict(...)
    ...
    IC [i] <-(int[,2] <= Ey0) & (int[,3] >= Ey0)
    int <- predict(reg, int="p", newdata=data.frame(x1=0.9, x2=0.9))
    IP[i] = (int[,2] <=y0) & (int[,3] >= y0)
}

mean(IP)
mean(IC)

