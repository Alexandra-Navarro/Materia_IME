# EP 05 - Grupo 4

library(dplyr)
library(ggpubr)


set.seed(123)

cat("Ejercicio Practico 05 - Grupo 4\n")



"
Problema: 
    Se sabe que una máquina que envasa detergentes industriales llena bidones 
    con un volumen de producto que sigue una distribución normal con desviación estándar 
    de 1 litro. Usando una muestra aleatoria de 100 botellas, el ingeniero a cargo de la 
    planta requiere determinar si la máquina está llenando los bidones con una media de 10 litros.
"

"
---------------------------------------------------------------------------------------------------
1) Si el ingeniero piensa rechazar la hipótesis nula cuando la muestra presente una media menor a 
9,5 litros o mayor a 10,5 litros, ¿cuál es la probabilidad de que cometa un error de tipo I?

"

#Para el desarrollo de esta pregunta se opta por comenzar en la visualización de la posibilidad
#de cometer un error de tipo I, por lo que simula una distribución normal con los datos entregados en el 
#enunciado

#Valores conocidos
sd <- 1             #Desviación estándar
n  <- 100           #Tamaño muestra
SE <- sd/sqrt(n)    #Error estándar
media_nula <- 10    #Media nula que se estará evaluando


#---Grafico
#Se genera una secuencia para simular los datos
x <- seq(media_nula - (8 * SE), media_nula + (8 * SE), length.out = 100)
y <- dnorm(x, mean = media_nula, sd = SE) 
df <- data.frame(x, y)

#Se grafica la distribución obtenida
g <- ggplot(data = df, aes(x))+ 
  stat_function(fun = dnorm ,
                args = list(mean = media_nula, sd = SE),
                colour = "blue",
                size = 0.25) + 
  ylab("") + 
  scale_y_continuous(breaks = NULL) + 
  scale_x_continuous(name = "Litros en bidón", breaks = seq(7, 13, 0.5)) +
  ggtitle("1) Distribución de volumen en bidones") +
  theme_pubr()



#Coloreando las zonas críticas
g <- g + geom_area(data = subset(df, x < 9.5), 
                   aes(y = y),
                   colour = "red", 
                   fill = "red", 
                   alpha = 1)

g <- g + geom_area(data = subset(df, x > 10.5), 
                   aes(y = y),
                   colour = "red", 
                   fill = "red", 
                   alpha = 1)

print(g)


#Observación: Como se puede apreciar en el gráfico las zonas críticas (color rojo) casi no poseen área
#             a colorear, por lo que a simple vista se puede asumir una probabilidad muy baja de rechazar
#             la hipótesis nula.


#A continuación se calcula el valor numérico de la probabilidad de cometer error de tipo I, el cual corresponde
#al valor del área de rechazo

alpha <- pnorm(9.5,10,0.1, lower.tail = TRUE) + pnorm(10.5,10,0.1, lower.tail = FALSE)


#Se obtuvo un valor muy ínfimo pero consecuente con lo que se observa del gráfico.


#Conclusion
cat("***R: La probabilidad de que el ingeniero cometa un error de tipo I es de ",alpha*100,"%. Por lo tanto, el ingeniero posee una probabilidad casi nula de determinar que la media de los bidones es distinta de 10L.")










cat("\n-------------------------------------------\n")
cat("2) Si el verdadero volumen medio de los bidones fuera de 10,3 litros, ¿cuál sería la probabilidad de que el ingeniero, que obviamente no conoce este dato, cometa un error de tipo II?\n")

#Para el estudio del poder en este caso, se conservan los límites críticos dados en la pregunta anterior,
#pero se modifica la distribución a evaluar, pasando de una media nula de 10 a una real de 10.3

media_real <- 10.3
#Se vuelve a generar una distribución para observar su gráfica
x1 <- seq(media_real - (8 * SE), media_real + (8 * SE), length.out = 100)
y1 <- dnorm(x1, mean = media_real, sd = SE) 
df1 <- data.frame(x1, y1)

#---Gráfico
g2 <- ggplot(data = df1, aes(x1)) + 
  stat_function(fun = dnorm ,
                args = list(mean = media_real, sd = SE),
                colour = "red", 
                size = 0.5) + 
  ylab("") + 
  scale_y_continuous(breaks = NULL) + 
  scale_x_continuous(name = "litros en bidón", breaks = seq(7, 15, 0.5)) + 
  ggtitle("2) Distribución de volumen en bidones") +
  theme_pubr()



#Se colorean las zonas críticas en donde se rechazaría la hipótesis nula
g2 <- g2 + geom_area(data = subset(df1, x1 < 9.5), 
                   aes(y = y1),
                   colour = "red", 
                   fill = "red", 
                   alpha = 1) +

           geom_area(data = subset(df1, x1 > 10.5), 
                   aes(y = y1),
                   colour = "red", 
                   fill = "red", 
                   alpha = 0.5)

print(g2)
#Observación: En comparación a la gráfica 1 en este caso se observa una probabilidad de cometer error de 
#             tipo I mucho más grande, abarcando un área (en esta oportunidad) mucho más visible al ojo



#A continuación se grafican las distribuciones de diferencia de medias

y2 <- dnorm(x1, mean = 10.5, sd = SE) 
df2 <- data.frame(x1, y=y2)

y3 <- dnorm(x1, mean =9.5, sd = SE) 
df3 <- data.frame(x1, y=y3)


g2 <- g2 + stat_function(fun = dnorm ,
                       args = list(mean = 10.5, sd = SE), 
                       colour = "blue", 
                       size = 0.5) + 
  stat_function(fun = dnorm ,
                args = list(mean = 9.5, sd = SE),
                colour = "blue",
                size = 0.5)


# Coloreando las zonas Beta 

g2 <- g2 + geom_area(data = subset(df2, x1 > 10.5), 
                   aes(y=y),
                   colour = "blue", 
                   fill = "blue", 
                   alpha = 0.5)  + 
  geom_area(data = subset(df3, x1 < 9.5),
            aes(y=y),
            colour = "blue", 
            fill = "blue", 
            alpha = 0.5)


print(g2)

#Observación: Como se ve en la gráfica, debido al aumento de la zona crítica alpha, se visualiza con facilidad
#             una zona Beta que podría significar un porcentaje significativo al momento de cometer un error de tipo II.




# Se procede a corroborar si la información de la gráfica es consecuente con la teoría : Prueba de poder

poder2 <- power.t.test(n=100,
               delta=0.2,
               sd = 1,
               sig.level= 0.1,
               type="one.sample",
               alternative = "one.sided")$power
  
#Para el cálculo de poder se consideró:
#-Diferencia entre la media real y el límite superior (10.3 - 10.5), ya que como se observa en la gráfica, 
#la zona beta del límite inferior no existe, por lo que la unica posibilidad
#de cometer un error de tipo II para una media de 10.3 está inclinada hacía el extremo superior.
#-Significación del 90% : Se opta por subir la exigencia de la prueba debido a que al existir una zona
#crítica tan pequeña resulta más confiable una prueba más exigente de poder.

b <- 1 - poder2  

#Se obtuvo una probabilidad baja pero no insignificante, y que termina de ser consecuente con su respectivo
#gráfico, por lo que se conluye:


#Conclusion
cat("***R: La probabilidad de cometer un error de tipo II es de ",b*100,"%. Por lo tanto, con un 90% de confianza se concluye que el ingeniero podría no rechazar la hipótesis nula erroneamente un 24% de las veces.")







cat("\n-------------------------------------------\n")
cat("3) Como no se conoce el verdadero volumen medio, genere un gráfico del poder estadístico con las condiciones anteriores, pero suponiendo que el verdadero volumen medio podría variar de 9,3 a 10,7 litros.\n")
datos <- rnorm(1000, mean = 10,sd = sd)
#muestra <- sample(datos,8000)

#Generamos un vector con un rango para el tamaño de la muestra
tamañoefecto <- 0.3
prom.muestral <- mean(datos)
prom2 <- 10
delta3 <- 0.7
dCohen <- (prom2 - prom.muestral)/sd
a <- 0.05

#Se procede a calcular el poder
#poderEjemplo <- pwr.t.test(n = 1000,
#                           d =dCohen,
#                           sig.level =a,
#                           type = "two.sample",
#                           alternative ="two.sided")



poder <- power.t.test(n= datos,
                      sd = sd,
                      sig.level = a,
                      delta = delta3,
                      type = "two.sample",
                      alternative = "two.sided")$power


#Se crea un data frame
datos2 <- data.frame(datos, poder)

#Aquí se procede a graficar la curva de poder 
g3 <- ggplot(datos2, aes(datos,poder))
g3 <- g3 + geom_line(colour = "blue")

print(g3) 




cat("\n-------------------------------------------\n")
cat("4) Considerando un volumen medio de 10 litros, ¿cuántos bidones deberían revisarse para conseguir un poder estadístico de 0,8 y un nivel de significación de 0,05?\n")
poder.estadistico = 0.8
vol.medio4 = 10
poder4 <- power.t.test(n=datos,
                       delta = 0.1,
                       sd = sd,
                       sig.level = a,
                       type = "two.sample",
                       alternative = "two.sided")$power


#Se crea un data frame
datos4 <- data.frame(datos, poder4)

#Aquí se procede a graficar la curva de poder 
g4 <- ggplot(datos4, aes(datos,poder4))
g4 <- g4 + geom_line(colour = "blue")

print(g4) 



cat("\n-------------------------------------------\n")
cat("5) ¿Y si el ingeniero fuese muy exigente y quisiera reducir la probabilidad de cometer un error de tipo I a un 1% solamente?\n")
sig.level5 <- 0.01
poder5 <- power.t.test(n=datos,
                       delta = 1,
                       sd = sd,
                       sig.level = sig.level5,
                       type = "two.sample",
                       alternative = "two.sided")$power


#Se crea un data frame
datos5 <- data.frame(datos, poder5)

#Aquí se procede a graficar la curva de poder 
g5 <- ggplot(datos5, aes(datos,poder5))
g5 <- g5+ geom_line(colour = "blue")

print(g5) 
