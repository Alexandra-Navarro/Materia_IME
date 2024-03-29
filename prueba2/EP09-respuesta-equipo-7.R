require(dplyr)
require(ggpubr)
require(tidyverse)
require(ez)
require(nlme)
require(emmeans)
#En este momento, los investigadores buscan determinar si existen diferencias
#en el tiempo que tardan los usuarios en formular consultas para problemas 
#con diferente nivel de dificultad en el �rea de literatura.

#Definir hip�tesis

#H0: El tiempo de formular una consulta en el �rea de literaruta es la misma para
#las 3 dificultades
#H1: El tiempo de formular una consulta en el �rea de literatura es distinto en
# al menos una dificultad

datos <- read.csv2("EP08 Datos.csv")

literatura <- datos %>% filter(area == "Literatura")
literaturaBaja <- literatura %>% filter(dificultad == "Baja")
literaturaMedia <- literatura %>% filter(dificultad == "Media")
literaturaAlta <- literatura %>% filter(dificultad == "Alta")

t1<-shapiro.test(literaturaBaja$tiempo)

t2<-shapiro.test(literaturaMedia$tiempo)

t3<-shapiro.test(literaturaAlta$tiempo)
# Con un nivel de significancia de alfa = 0.05 al realizar la prueba de Shapiro 
# se puede determinar que el grupo de preguntas del �rea de literatura con 
# nivel de dificultad media no sigue una distribuci�n normal

literatura[["id"]] = factor(literatura[["id"]])
literatura[["dificultad"]] = factor(literatura[["dificultad"]])

# Se utiliza una prueba ANOVA de una v�a para muestras correlacionadas
prueba <-ezANOVA(data=literatura,dv=tiempo,within=dificultad,wid=id,return_aov=TRUE)
resumen_prueba<-summary(prueba$aov)
#Con un nivel de significancia de alfa = 0.05 entonces se puede determinar que 
#los datos cumplen con la condici�n de esfericidad


#Con un nivel de confianza del 95% existe suficiente evidencia para rechazar la
#hipotesis nula en favor de la alternativa. Por ende, si existen diferencias
#en el tiempo que tardan los usuarios en formular consultas para problemas 
#con diferente nivel de dificultad en el �rea de literatura.


#Como existe una diferencia en los tiempos promedio de consulta se realiza una 
#prueba de post-hoc de HSD Tukey para determinar en donde se genera.
mixto<- lme(tiempo~dificultad,data=literatura,random =~1|id)
medias<-emmeans(mixto,"dificultad")
tukey<-pairs(medias,adjust="tukey")

#Teniendo un nivel de confianza de 0.05 se puede concluir que existe diferencia
#en los tiempos de realizar consultas en el �rea de literatura para todas las
#dificultades.

