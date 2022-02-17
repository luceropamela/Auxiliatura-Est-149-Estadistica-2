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
ymin<-50;ymax<-1
integral2(fun,xmin,xmax,ymin,ymax)

# Otra forma de hallar integrales dobles en R

f <- function(x) {
  integrate(function(y) {x+y},0, 1)$value
}
integrate(Vectorize(f), 0, 1)$value

# Hallamos las probabilidades
Fxy<-function(u,v){
  f <- function(x) {
    integrate(function(y) {x+y},0, v)$value
  }
  integrate(Vectorize(f), 0, u)$value
}
Fxy((1/2),(3/4))


# Hallamos la probabilidad de P(x>1/3, y<3/4)

Fxy<-function(u,v){
  f <- function(x) {
    integrate(function(y) {x+y},0, v)$value
  }
  integrate(Vectorize(f), u, 1)$value
}
Fxy((1/3),(3/4))


#Ejercicio 2
f <- function(x) {
  integrate(function(y) {(3*x)*(1-(x*y))},0, 1)$value
}
integrate(Vectorize(f), 0, 1)$value




# Ejercicio 3

#a)
# Comprobando que sea una funcion de densidad
library(pracma)
fun3<-function(x,y){
  (3*x-y)/5 
}
xmin<-1;xmax<-2
ymin<-1;ymax<-3
integral2(fun3,xmin,xmax,ymin,ymax)

# su otra forma 
f <- function(x) {
  integrate(function(y) {(3*x-y)/5},1, 3)$value
}
integrate(Vectorize(f), 1, 2)$value

# b)
Fxy<-function(u,v){
  f <- function(x) {
    integrate(function(y) {((3*x)-y)/5},1, v)$value
  }
  integrate(Vectorize(f), 1, u)$value
}
Fxy(1.5,2)



library(pracma)
fun<-function(x,y){
  ((3*x)-y)/5
}
xmin<-1;xmax<-1.5
ymin<-1;ymax<-2
integral2(fun,xmin,xmax,ymin,ymax)
