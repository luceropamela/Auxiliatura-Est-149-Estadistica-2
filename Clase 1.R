setwd("C:\\Users\\ALAVE\\Documents\\PAMELA\\AUXILIATURA ESTADISTICA 2")
getwd()
rm(list = ls())
#FUNCION DE DISTRIBUCION CONJUNTA
integrate(function(x) exp(-x), lower = 0, upper = Inf)
integrate(function(x) 1 / sqrt(3 - x), lower = 0, upper = 3)



# PARA EL CASO CONTINUO

#Integrales dobles con el paquete pracma


#Ejercicio 1 
install.packages("pracma")
library(pracma)
fun<-function(x,y){
  x+y
}
xmin<-0;xmax<-1
ymin<-,50;ymax<-1
integral2(fun,xmin,xmax,ymin,ymax)

# Otra forma de hallar integrales dobles en R

f <- function(x) {
  integrate(function(y) {x+y},0, 1)$value
}
integrate(Vectorize(f), 0, 1)$value

# Hallamos las probabilidades
Fxy<-function(u,v){
  f <- function(x) {
    integrate(function(y) {x+y},0, u)$value
  }
  integrate(Vectorize(f), 0, v)$value
}
Fxy((1/2),(3/4))


#Ejercicio 2
f <- function(x) {
  integrate(function(y) {(3*x)*(1-(x*y))},0, 1)$value
}
integrate(Vectorize(f), 0, 1)$value











