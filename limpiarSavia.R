library(openxlsx)

# Lectura archivo
Datos = read.xlsx("D://Documentos//Trabajo Utal//SAVIA//Sap flow viña 2018-2019.xlsx",sheet = 1)

# Duplicar para no utilizar la informacion original
x <- Datos

# Reemplazo datos
# Noche = 480
# eliminar 480 de dia y datos muy altos
for(i in 4:27){
  x[,i] <- replace(x[,i],x$`hh:mm`>= 0 & x$`hh:mm`<= 800,480)
  x[,i] <- replace(x[,i],x$`hh:mm`>= 2100 & x$`hh:mm`<= 2330,480)
  x[,i] <- replace(x[,i],x$`hh:mm`>800 & x$`hh:mm`<2100 & x[,i]==480,NA)
  x[,i] <- replace(x[,i],x$`hh:mm`>=1030 & x$`hh:mm`<=1930 & x[,i]>=150,NA)
}
write.xlsx(x,"D://Documentos//Trabajo Utal//SAVIA//prueba.xlsx",header =T)
remove(i)

# Primer relleno
# Primer dato de la serie no puede ser vacio
for(i in 4:27){
  if(is.na(x[1,i])){
    x[1,i] <- rowMeans(x[1,4:(i-1)])
  }
}
remove(i)
write.xlsx(x,"D://Documentos//Trabajo Utal//SAVIA//prueba1.xlsx",header =T)

# Segundo relleno
# 
count <- 0
for(i in 4:27){
  for(j in 1:length(x[,1])){
    if(is.na(x[j,i])){
      count <- count + 1
    }else{
      if(count >= 10){
        if(count%%2 == 0){
          x[j-(count/2)-1,i] <- rowMeans(x[j-(count/2)-1,4:27], na.rm = T)
          count <- 0
        }else{
          if(rowMeans(x[j-(count%/%2)-1,4:27], na.rm = T) < 100){
            x[j-(count%/%2)-1,i] <- NA
            count <- 0
          }else{
            x[j-(count%/%2)-1,i] <- rowMeans(x[j-(count%/%2)-1,4:27], na.rm = T)
            count <- 0
          }
        }
      }else{
        count <- 0
      }
    }
  }
}
write.xlsx(x,"D://Documentos//Trabajo Utal//SAVIA//prueba2.xlsx",header =T)
remove(i,j,count)

col2 <- vector()
interpol <- Datos[,1:3]

for(i in 4:27){
  col2[1] <- x[1,i]
  interpol <- cbind(interpol,INTERPOLATION(x[,i]))
  colnames(interpol)[i] = colnames(x)[i]
}
write.xlsx(interpol,"D://Documentos//Trabajo Utal//SAVIA//prueba3.xlsx",header =T)

### FUNCION INTERPOLACION ###

INTERPOLATION <- function(col){
  count <- 0
  for(i in 2:length(col)){
    if(is.na(col[i])){
      count <- count + 1
    }else{
      if(count == 0){
        col2[i] <- col[i]
      }else{
        for(j in 0:(count-1)){
          col2[i-count+j] <- col2[i-count+j-1] + (col[i]-col2[i-count+j-1])/(count-j+1)
          col2[i] <- col[i]
        }
        count <- 0
      }
    }
  }
  assign("col2", col2, envir = .GlobalEnv)
}

