library(openxlsx)
library(splines)

# Lectura archivo
Datos = read.xlsx("D://Documentos//Trabajo Utal//SAVIA//Sap flow viña 2018-2019.xlsx",sheet = 1)

# Extraccion informacion
# Tiempo, Tz1  y distancia para spline
x = Datos[,-c(1,2,4,5,7:27)]

# Reemplazo datos
# Noche = 480
# eliminar 480 de dia y datos muy altos
x$Tz3=replace(x$Tz3,x$`hh:mm`>= 0 & x$`hh:mm`<= 800,480)
x$Tz3=replace(x$Tz3,x$`hh:mm`>= 2100 & x$`hh:mm`<= 2330,480)
x$Tz3=replace(x$Tz3,x$`hh:mm`>800 & x$`hh:mm`<2100 & x$Tz3==480,NA)
# plot(x$Tz1)

#x$Tz1=replace(x$Tz1,x$`hh:mm`>=1100 & x$`hh:mm`<=1830 & x$Tz1>=mean(Datos$Tz1),NA)
x$Tz3=replace(x$Tz3,x$`hh:mm`>=1030 & x$`hh:mm`<=1930 & x$Tz3>=150,NA)

# spline simple de relleno
# montes y valles fuera del rango esperado
splineout<-spline(x=x$x
                  ,y=x$Tz3
                  ,n=4688)
write.xlsx(splineout$y,"D://Documentos//Trabajo Utal//SAVIA//prueba2.xlsx")
# solucion Guillermo
# suaviza demasiado o muy poco
# demasiado extremista para los datos
prueba = smooth.spline(x$x,x$Tz1,w=NULL,spar=0.1)

# Otras pruebas Fallidas
# en proceso, rellenar 480 de noche
# recorrer datos con for para dejar ciertos puntos.
max(Filtrado$Tz1)
min(Filtrado$Tz1)
mean(Filtrado$Tz1)
mean(Filtrado$Tz1[Filtrado$Tz1<=100])

rowna <- apply(x, 1, function(y){any(is.na(y))})
sum(rowna)
Filtrado = x[!rowna,]
cont=0

rowna

for(i in 1:length(Datos$Tz1)){
  if(is.na(x$Tz1[i])){
    cont = cont + 1
  }else{
    if(cont<=10){
      for(j in 1:cont){
        x$Tz1[i-cont+j-1]=x$Tz1[i-cont-1+j-1]
      }
      
    }
    
    cont = 0
    
  }
  
}

is.na(x$Tz1[1])

write.xlsx(x,"D://Documentos//Trabajo Utal//SAVIA//prueba2.xlsx")

x$Tz1=replace(x$Tz1,x$`hh:mm`>800 & x$`hh:mm`<2100 & x$Tz1==480,NA)
x$Tz1=replace(x$Tz1,x$`hh:mm`>=1030 & x$`hh:mm`<=1930 & x$Tz1>=150,NA)

x$Tz1 = splineout$y
splineout<-spline(x=x$x
       ,y=x$Tz1
       ,n=4688)

# Falta probar bien
library(trajr)
lines(x$Tz1)
plot(splineout$x,splineout$y,col="blue")
points(splineout$x,splineout$y,type = 'l',col="red") 
write.xlsx(splineout$y,"D://Documentos//Trabajo Utal//SAVIA//prueba4.xlsx")

# SG ya no existe en rstudio
prueba = sgolayfilt(x, p = 3, n = p + 3 - p%%2, m = 0, ts = 1)
lines(prueba)

###Series de Tiempo###

z = ts(x,start=c(2018,1),end=c(2019,4688),frequency = 12)
descomp1=decompose(z)
plot(descomp1)

plot(Datos$x,Datos$Tz1)
lines(smooth.spline(Datos$x,Datos$Tz1))

descomp2 = stl(z,s.window="periodic")
plot(x)

mean(Datos$Tz1)

filter(x$Tz1)

summary(x$Tz1)

# resto de division
10%%3

# Falta probar paquetes:
# Signal
# trajr

# hsdar
# Para datos espectrales (generalmente valores de reflectancia)
# no sirve
derivative.speclib(x, m = 1, method = "sgolay", ...)
noiseFiltering(x$Tz1, method = "mean")
noiseFiltering(spectral_data, method="sgolay", n=25)

prueba = derivative.speclib(x$Tz1, m = 1, method = "sgolay")
library(hsdar)

prueba = noiseFiltering(x$Tz1, method = "sgolay")
