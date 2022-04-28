
################ 
#Ep 04 grupo 4# 
############### 


# ------- Para visualizar de mejor manera correr el script con Source -------- 

#Librerias 

library(ggplot2) 
library(ggpubr) 
library(dplyr) 

# Datos (Cambiar directorio según corresponda) 

datos<- read.csv2("C:/Users/alena/Downloads/EP04-datos.csv")
# ----------------------------------------------------------------------------------------------- # 
cat("\n\nGrupo 04 - Ejercicio Práctico 04\n") 
cat("\n-------------------------------------------------------------------------\n") 
cat("\nPregunta 1) El Comité Olímpico cree que el mejor tiempo medio de los atletas orientales antes de ingresar al programa de entrenamiento es superior a 18,83 segundos. ¿Soportan los datos esta afirmación?\n") 
cat("-- Datos a trabajar: Tiempos previos de las personas de raza oriental\n\nPara su estudio se sugiera utilizar la prueba T de Student para una muestras, por lo que se procede a revisar si la muestra cumple con las condiciones para la prueba.\n") 
cat("\n********\n") 
cat("Verificación:\n1.- ¿Datos independientes? : Se asume independencia debido a que los datos utilizados provienen de distintos atletas escogidos aleatoriamente. 
\n2.- ¿Distribución normal? : Se aplica un estudio sobre el gráfico qq de la muestra:") 

orientales tiempoPrevioOrientales 
qq1 color = "red",) 
print(qq1) 

cat("Resultado qq (Gráfico 1): No se observan datos atípicos, por lo que se asume que los datos de la población provienen de una distribución cercana a la normal.\n") 
cat("\nConclusión: Como los datos cumplen con las condiciones de la prueba T para una muestra, se procede a generar las hipótesis a estudiar.") 
cat("\n********\n") 
cat("\n\nPrueba T de Student para una muestra\n") 
cat("--- Hipotesis ---\n 
H0 : El tiempo medio de los atletas previo a ingresar al programa de entrenamiento es igual a 18,83 segundos.\n 
HA : El tiempo medio de los atletas previo a ingresar al programa de entrenamiento es mayor a 18,83 segundos.") 
cat("\n\n**Valores a utilizar en la prueba**\n") 
cat("\n-El estadístico de esta prueba corresponde a la media y su valor nulo es de 18.83.\n-Se decide evaluar el estudio con 95% de confianza (a = 0.05).\n") 

#Datos para probar hipótesis 

mu = 18.83 
a = 0.05 

prueba1 print(prueba1) 

cat("El t.test arrojo un p-valor < a, por lo que con los datos obtenidos se puede concluir con un 95% de confianza que se rechaza la hipótesis nula en favor de la alternativa.\n") 
cat("En consecuencia, el tiempo medio de los atletas orientales previo al programa de entrenamiento es mayor a 18.83 segundos.") 


cat("\n-------------------------------------------------------------------------\n") 




cat("\nPregunta 2) ¿Sugieren los datos que la mejor marca de los atletas negros se reduce en 5,82 segundos tras el entrenamiento?\n") 
cat("-- Datos a trabajar: Tiempos previos y posteriores de las personas de raza negra.\n\nPara su estudio se sugiere utilizar la prueba T de Student para datos pareados, sin embargo se trabajará con las diferencias de estos valores, utilizando en consecuencia, la prueba para una sola muestra. Entonces, se procede a revisar si la muestra cumple con las condiciones para la prueba.\n") 
cat("\n********\n") 
cat("Verificación:\n1.- ¿Datos independientes? : Se asume independencia debido a que los datos utilizados provienen de distintos atletas escogidos aleatoriamente. 
\n2.- ¿Distribución normal? : Se aplica un estudio sobre el gráfico qq de la muestra:") 

negros diferenciaTiempos 

qq2 color = "blue", 
ylab = "Diferencia") 
print(qq2) 

cat("Resultado qq (Gráfico 2): Analizando el grafico qq se observan datos atípicos, por lo que se procede a realizar la prueba de Shapiro-Wilk para confirmar o desestimar normalidad.\n\n") 

shapiro1 print(shapiro1) 

cat("Resultado shapiro.test: al aplicar el test de Shapiro-Wilk se puede llegar a concluir una supuesta normalidad de los datos, ya que p-value>0.05.") 
cat("\nConclusión: Como los datos cumplen con las condiciones de la prueba T para una muestra, se procede a generar las hipótesis a estudiar.") 
cat("\n********\n") 


cat("\n\nPrueba T de Student para una muestra\n") 
cat("--- Hipotesis ---\n 
H0 : La diferencia de los tiempos de los atletas negros es de 5.82 segundos.\n 
HA : La diferencia de los tiempos de los atletas negros es menor a 5.82 segundos.") 

cat("\n\n**Valores a utilizar en la prueba**\n") 
cat("\n-El estadístico de esta prueba corresponde a la media y su valor nulo es de 5.82.\n-Se decide evaluar el estudio con 90% de confianza por la inseguridad de asumir la normalidad. (a = 0.1).\n") 

mu = 5.82 
a = 0.1 
prueba2 print(prueba2) 

cat("El t.test arrojo un p-valor < a, por lo que con los datos obtenidos se puede concluir con un 90% de confianza que se rechaza la hipótesis nula en favor de la alternativa.\n") 
cat("En consecuencia, la media de mejora en los atletas negros es menor a 5.82 segundos.") 








cat("\n-------------------------------------------------------------------------\n") 








cat("\nPregunta 3) ¿Es posible afirmar que, en promedio, los atletas negros superan a los orientales por menos de 3,64 segundos después del entrenamiento?\n") 
cat("-- Datos a trabajar: Tiempos posteriores de las personas de raza negra y orientales.\n\nPara su estudio se sugiere utilizar la prueba T de Student para datos independientes. Entonces, se procede a revisar si las muestras cumplen con las condiciones para la prueba.\n") 
cat("\n********\n") 

cat("Verificación: datos PosteriorNegro\n1.- ¿Datos independientes? : Se asume independencia debido a que los datos utilizados provienen de distintos atletas escogidos aleatoriamente. 
\n2.- ¿Distribución normal? : Se aplica un estudio sobre el gráfico qq de la muestra:") 


#Datos a trabajar 
PosteriorNegro PosteriorOrientales 
#Gráfica de normalidad 
qq3 color = "green") 
print(qq3) 

cat("Resultado qq (Gráfico 3): Analizando el grafico qq se observan datos atípicos, por lo que se procede a realizar la prueba de Shapiro-Wilk para confirmar o desestimar normalidad.\n\n") 

shapiro2 print(shapiro2) 

cat("\nResultado shapiro.test: al aplicar el test de Shapiro-Wilk se puede llegar a concluir una supuesta normalidad de los datos, ya que p-value>0.05. Sin embargo no es un valor muy alto.") 
cat("\nConclusión: Como los datos cumplen con las condiciones de la prueba T, se puede aplicar dicha prueba. De todas maneras se debe aplicar mayor exigencia al revisar los resultados.") 
cat("\n********\n") 


cat("Verificación: datos PosteriorOrientales\n1.- ¿Datos independientes? : Se asume independencia debido a que los datos utilizados provienen de distintos atletas escogidos aleatoriamente. 
\n2.- ¿Distribución normal? : Se aplica un estudio sobre el gráfico qq de la muestra:") 

qq4 color = "orange") 
print(qq4) 


cat("Resultado qq (Gráfico 4): Analizando el grafico qq se observan solo un dato atípico, por lo que se procede a realizar la prueba de Shapiro-Wilk para confirmar o desestimar normalidad.\n\n") 


shapiro3 print(shapiro3) 

cat("\nResultado shapiro.test: al aplicar el test de Shapiro-Wilk se puede llegar a concluir una supuesta normalidad de los datos, ya que p-value>0.05. Sin embargo no es un valor muy alto.") 
cat("\nConclusión: Como los datos cumplen con las condiciones de la prueba T, se puede aplicar dicha prueba. De todas maneras se debe aplicar mayor exigencia al revisar los resultados.") 
cat("\n\nConclusion de ambas muestras: Ambos conjuntos de datos cumplen las condiciones para realizar la prueba T para muestras independientes, por lo que se procede a generar las hipótesis a estudiar.\n") 
cat("\n********\n") 



cat("\n\nPrueba T de Student para dos muestras independientes\n") 
cat("--- Hipótesis ---\n 
H0 : La media de los tiempos previos de los atletas negros es igual a la de los orientales.\n 
HA : La media de los tiempos previos de los atletas negros es menor a la de los orientales en al menos 3.64 segundos.") 

cat("\n\n**Valores a utilizar en la prueba**\n") 
cat("\n-El estadístico de esta prueba corresponde a la media y su valor nulo es de 3,64.\n-Se decide evaluar el estudio con 90% de confianza por la inseguridad de asumir la normalidad. (a = 0.1).\n") 

mu = 3.64 
a = 0.1 
prueba3 print(prueba3) 

cat("\nEl t.test arrojó un p-valor < a, por lo que con los datos obtenidos se puede concluir con un 90% de confianza que se rechaza la hipótesis nula en favor de la alternativa.\n") 
cat("Por lo tanto, la media de los tiempos previos de los atletas negros es menor a 3.64 en comparación a los orientales, es decir, tuvieron menos mejoras.") 


