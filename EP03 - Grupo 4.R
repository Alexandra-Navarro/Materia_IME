library(ggpubr)
library(dplyr)
library(patchwork)


# ----------- PARTE 1 ----------


población <- read.csv2("C:/Users/alena/Downloads/EP02 Datos Casen 2017.csv")
tamaño <- nrow(población)
ingreso <- as.numeric(población[["ytot"]])
poda <- 0.2
q20 <- quantile(ingreso, poda)
q80 <- quantile(ingreso, 1 - poda)
ingreso.podado <- ingreso[ingreso > q20 & ingreso < q80]
tamaño.podado <- length(ingreso.podado)
media.ingreso <- mean(ingreso.podado)
sd.ingreso <- sqrt (sum((ingreso.podado - media.ingreso)^2) / tamaño.podado )




#--Pregunta 1

set.seed(201)

mat <- matrix(c(1, 2), nrow = 1, ncol = 2, byrow = TRUE)
layout(mat = mat)


ingreso.normal <- rnorm(5000, media.ingreso, sd.ingreso)
grafico.normal <- ggqqplot(ingreso.normal, 
                  title = "Ingreso Normalizado",
                  color = "red",
                  ylab = "Ingresos")

#--Pregunta 2
ingreso.estandarizado <- (ingreso.normal - media.ingreso)/sd.ingreso
grafico.estandarizado <- ggqqplot(ingreso.estandarizado, 
                          title = "Ingreso Estandarizado",
                          color = "blue",
                          ylab = "Ingresos")




#Graficas

grafico.normal + grafico.estandarizado








# ----- Para visualizar los tres graficos que vienen a continuación

mat2 <- matrix(c(1:3, 3), nrow=2, byrow=T)
layout(mat = mat2)

#--Pregunta 3


# Grados de libertad : 5 y 10

distribucion.chi1 <- function(x) { resultado <- sum(sample(ingreso.estandarizado, 5)^2)
                                   return(resultado) }
distribucion.chi2 <- function(x) { resultado <- sum(sample(ingreso.estandarizado, 10)^2) 
                                   return(resultado)}

chi1 <- sapply(1:1000, distribucion.chi1)
chi2 <- sapply(1:1000, distribucion.chi2)

#Para graficar con ggqqplot se deben transformar los vectores en dataframe
df1 <- data.frame(chi1)
df2 <- data.frame(chi2)


grafico.chi1 <- ggplot(df1, aes(x = chi1)) +
                geom_density() +
                ggtitle("Distribución Chi 1") +
                xlab("Chi 1") + ylab("Densidad")

grafico.chi2 <- ggplot(df2, aes(x = chi2)) +
                geom_density() +
                ggtitle("Distribucion Chi 2") +
                xlab("Chi 2") + ylab("Densidad")



#--Pregunta 4 

ingreso.f <- (chi1/5)/(chi2/10)

df3 <- data.frame(ingreso.f)

grafico.f <- ggplot(df3, aes(x = ingreso.f)) +
             geom_density() + 
             ggtitle("Distribucion F") +
             xlab("Valores F") + ylab("Densidad")



#Tres graficas, 2 chi y 1 f
grafico.chi1 + grafico.chi2 + grafico.f












# ----------- PARTE 2 ----------

mat3 <- matrix(c(1), nrow=1, byrow=T)
layout(mat = mat3)


n.repeticiones <- 25

ensayo <- function(x)
  ifelse(sample(población[["sexo"]], 1) == "Mujer", 1, 0)





prueba <- function(x){
  repeticiones <- sapply(1:n.repeticiones, ensayo)
  resultado <- length(which(repeticiones == 1))
  return(resultado)
}

datos <- sapply(1:5000, prueba)


# --- Pregunta 1

hist(datos,freq=FALSE,col="lightcyan",
     xlim = range(0:25), main = "Distribución Binomial",
     xlab = "Cantidad de mujeres en una muestra de 25 personas", ylab = "Densidad")

# -- Pregunta 2

  #Distribución geométrica



obtenerDatos<- function(x){
  repeticiones <- sapply(1:n.repeticiones, ensayo)
  resultado <- which(repeticiones == 1)[1]
  return(resultado)
}

datosGeo <- sapply(1:1000, obtenerDatos)

hist(datosGeo, freq=FALSE, main = "Distribución Geométrica",
     xlab = "Cantidad de intentos para obtener una mujer", ylab = "Densidad")




# -- Pregunta 3

obtener <- function(x){
  repeticiones <- sapply(1:n.repeticiones, ensayo)
  cantidadMujeres <- length(which(repeticiones == 1))
  posiciones <- which(repeticiones == 1)
  maximo <- length(posiciones)
  posicion <- posiciones[maximo]
  return(posicion)
}

datosBinomialNegativa <- sapply(1:1000, obtener)

hist(datosBinomialNegativa, freq=FALSE, main = "Distribución Binomial Negativa",
     xlab = "Cantidad de intentos para obtener 10 mujeres", ylab = "Densidad")











