require(dplyr)
datos<-read.csv2("EP10 Datos.csv")

#1.-�Existe diferencia en la puntuaci�n obtenida por los envases dise�ados por 
# PackPro seg�n las evaluaciones realizadas por ni�os y adultos?
filtrado<-datos%>%filter(Diseno=="PackPro" & (Edad=="Nino" | Edad=="Adulto"))

#H0: No hay diferencia en la puntuaci�n obtenida por los envase dise�ados por
# PackPro
#HA: Hay diferencia en la puntuaci�n obtenida por los envases dise�ados por 
# PackPro

#Se pretende utilizar una prueba de rangos Wilconson, ya que las variables son 
# independientes y almenos ordinal
alfa=0.05

ninos<-filtrado%>%filter(Edad=="Nino")
adultos<-filtrado%>%filter(Edad=="Adulto")
prueba1<-wilcox.test(ninos$Puntaje,adultos$Puntaje,alternative = "two.sided",conf.level = 1-alfa)
print(prueba1)

#Con un nivel de significancia de alfa=0.05 no existe suficiente evidencia para
# rechazar la hip�tesis nula, por ende no existe una diferencia en los puntajes
# obtenidos por los envases dise�ados por PackPro entre ni�os y adultos

#2.-�Existen diferencias entre las puntuaciones obtenidas para los diferentes 
# envases de caramelos? De ser as�, �cu�l(es) envase(s) se diferencia(n) de los
# dem�s?

#H0: No existe diferencia entre las puntuaciones obtenidas para los diferentes
# envases de caramelo
#HA: Existe diferencia entre las puntuaciones obtenidas para los diferentes 
# envases de caramelo

#Se realizar� la prueba de kruskal, ya que la variable independiente tiene m�s de
# 2 niveles, la escala de la variable dependiente es almenos ordinal y las 
# observaciones son independientes entre s�.

caramelo<-datos%>%filter(Producto=="Caramelos")
prueba2<-kruskal.test(Puntaje~Diseno,data=caramelo)
print(prueba2)

#Con un nivel de significancia de alfa=0.05 no existe suficiente evidencia para
# rechazar la hip�tesis nula por ende se puede determina que no existe diferencia
# entre las puntuaciones de los envases de caramelo.

