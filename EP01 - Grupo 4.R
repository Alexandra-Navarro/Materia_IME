#Ejercicio practico 1

##################
#Librerias
library(dplyr)
library(ggplot2)
##################

#---------------------------------------------------------------------
#Objetivo: saber que dia los casos con síntomas fueron mayores en coquimbo entre
#las fechas 01-may-2020 y el 31-oct-2020

datos <- read.csv2("C:/Users/osswa/Downloads/EP01 Datos Covid.csv")
#leemos el archivo csv con los datos de contagios
#Coquimbo corresponde a la fila 5
coquimbo <- datos[datos$Region == 'Coquimbo',]
#Separamos la fila de la comuna que nos interesa
coquimboFecha <- coquimbo[,61:244]

#vemos que las columnas con las fechas que nos interesan van de
#la 61 a la 244, asi que las aislamos
valorMaximo <- max.col(coquimboFecha)
nombres <- names(coquimboFecha)
maximo <- nombres[valorMaximo]


#Respuesta: Se concluye que entre el rango de fechas señalados el día 25 de Julio del 2020 es el que posee mas casos de síntomas

#---------------------------------------------------------------------
#¿Cuál fue el total de casos con síntomas para cada mes de este periodo?

mayo <- rowSums(coquimboFecha[,1:31])
junioDatos <- as.integer(coquimboFecha[1,32:61])
junio <- sum(junioDatos)
julio <- rowSums(coquimboFecha[,62:92])
agosto <- rowSums(coquimboFecha[,93:123])
septiembre <- rowSums(coquimboFecha[,124:153])
octubre <- rowSums(coquimboFecha[,154:184])

sumas <- data.frame(Mes = c("Mayo","Junio","Julio","Agosto","Septiembre","Octubre"), Cantidad = c(mayo,junio,julio,agosto,septiembre,octubre))



#Respuesta: al realizar las sumas de los casos en cada mes se obtuvo 414 en Mayo, 1787 en Junio, 2586 en Julio, 2780 en Agosto, 1361 en Septiembre y 499 en Octubre del 2020.

# --- Visualizacion de los meses ---
ggplot(data = sumas,
       mapping = aes(x = Mes,
                     y = Cantidad)) +
  geom_point() +
  labs(title = 'Meses')
