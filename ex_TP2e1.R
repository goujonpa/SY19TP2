# Paul GOUJON
# UTC - SY19
# TP2 Exercise 1


rm(list=ls())
data = read.table("./");

# checked if the 32nd individual is still wrong, OK.

# x is data
x = data[,1:9]
# z is labels 
z = data[,9:10]
# sx is scaled data (mean 0) and variance 96 ?
sx = as.data.frame(scale(x))


