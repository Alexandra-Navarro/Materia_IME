library(ggpubr)
library(dplyr)
library(pwr)
library(Hmisc)

nombres <- c("Pediatr�a", "Obstetricia", "Dermatolog�a", "Psiquiatr�a", "Medicina Interna", "Oncolog�a", "Neurolog�a", "Anestesiolog�a", "Radiolog�a")     
mujeres <- c(54,71,35,30,45,44,56,21,17)
hombres <- c(52,66,41,42,65,62,88,40,45)

datos <- data.frame(nombres,mujeres,hombres)

"
------------------------------------------------------------------------------------------------------------------------------
1) Estudios previos hab�an determinado que la proporci�n de autoras en la especialidad de pediatr�a era de 35%.
�Respaldan estos datos tal estimaci�n?


Para evaluar el problema se utilizar� una prueba de proporciones, ya que se necesita corroborar si con
la muestra cumple con el valor nulo que se solicita.

---Evaluando condiciones:

    -Independencia: Los datos fueron escogidos de manera aleatoria y evidentemente representa a menos del 10%
                    de la poblacion de autores y autoras cient�ficas.
                    
    -Condici�n �xito-fracaso: Suponiendo que se cumple el valor nulo de proporcion 
    
            0.35*373      = 130.55  >= 10
            (1 -0.35)*373 = 242.45  >= 10
            
            Como presenta mas de 10 observaciones en cada caso de �xito o fracaso, se concluye que se cumple
            con la condici�n de �xito-fracaso.

Entonces, se procede a realizar las hip�tesis a evaluar:

---Hip�tesis
-Lenguaje natural:
H0 : La proporci�n de autoras en la especialidad de pediatr�a es igual a 35%.
HA : La proporci�n de autoras en la especialidad de pediatr�a es distinta a 35%.

-Lenguaje matem�tico:
Sea el valor nulo p0 = 0.35
H0 : p  = p0
HA : p != p0


-------
Observaciones:

-Se determina utilizar una prueba de proporciones de Wilson (funcion prop.test()) para una muestra bilateral,
ya que puede entregar un p-valor que permita corroborar si la proporci�n cumple con la hip�tesis nula.


-Se determin� utilizar un nivel de confianza del 90%, ya que la muestra resulta peque�a en comparaci�n a la
posible suma de las profesionales del �rea de pediatr�a, por lo que se necesita ser mas estrictos al momento
de revisar la prueba de proporci�n de Wilson.
"

alpha <- 0.1                                  #nivel de confianza
n <- datos$mujeres[nombres == "Pediatr�a"]    #cantidad de mujeres
p <- 0.35                                     #valor nulo
x <- n / sum(mujeres)                         #proporci�n en la muestra

cat("------------------------------------------------\n")
cat("Prueba de proporciones para el problema 1.")
print(prop.test(x = x,n = n,p =p,alternative="two.sided",conf.level = alpha))
cat("------------------------------------------------\n")

"
Conclusi�n: 
Se obtuvo un p-valor < a, por lo que con un 90% de seguridad se concluye que se rechaza la hip�tesis nula a 
favor de la alternativa. Por lo tanto, la proporci�n de las autoras en el �rea de pediatr�a es 
distinta a 35%.
------------------------------------------------------------------------------------------------------------------------------
"
"
2) Seg�n estos datos, �es igual la proporci�n de autoras en las �reas de anestesiolog�a y pediatr�a?"
"Se extiende del ejercicio anterior que las condiciones se cumplen para esta prueba,
por lo que se procede a plantear las hipotesis"
"en lenguaje humano:
  
H0: La proporci�n de autoras de Anesteiologia y pediatria son iguales
HA: La proporion de autoras de Anestesiologia y pediatria no son iguales

en lenguaje matematico:
  sea #beta# la probabilidad de obtener un error de tipo 2, se debe conocer el tama�o de
efect y realizar una prueba de poder de 2 proporciones para determinar este beta, que 
concluira si efectivamente podemos afirmar igualdada(si es muy peque�o) o no igualdad
(si es muy grande)"

mujeresAnestesiologia <- datos$mujeres[nombres == "Anestesiolog�a"]/sum(mujeres)#proporcion de anestesiologas
mujeresPediatria <- datos$mujeres[nombres == "Pediatr�a"]/sum(mujeres)#proporcion de pediatras

tama�o_del_efecto <- ES.h(mujeresPediatria,mujeresAnestesiologia)#se calcula el tama�o de efecto para este caso
n <- 9 #cantidad de especialidades

"finalmente se calcula Beta restando a 1 el nivel de significaci�n que resulta de la prueba de poder para 2 proporciones"
beta <- (1 - pwr.2p.test(h = tama�o_del_efecto, n = n, power = NULL, alternative = "two.sided")$power) *100
"*multiplicando por 100 para que quede expresado en porcentaje."
"Respuesta: La probabilidad de cometer un error de tipo 2 es del 90,2%, por lo que con un 95% de confianza 
se concluye que las proporciones de las autoras del ar�a de anestesiolog�a y pediatr�a no son iguales.
------------------------------------------------------------------------------------------------------------------------------
"
"
------------------------------------------------------------------------------------------------------------------------------
3) Suponiendo que la diferencia en la proporci�n de autoras en la especialidad de medicina interna 
y la de dermatolog�a es de 0,15. �A cu�ntos autores deber�amos monitorear para obtener un intervalo 
de confianza del 97,5% y poder estad�stico de 80%, si se intenta mantener aproximadamente la misma 
proporci�n de gente estudiada en cada caso?


Para este problema se necesita obtener el tama�o de la muestra en las �reas de dermatolog�a y medicina
interna para ambos casos (hombres y mujeres) a modo de mantener una diferencia de 0.15 entre las proporciones
de mujeres de dermatolog�a y medicina interna.
"
#Se obtienen los datos globales de cada especialidad
autores_D  <- datos$mujeres[nombres == "Dermatolog�a"] +  datos$hombres[nombres == "Dermatolog�a"]
autores_MI <-  datos$mujeres[nombres == "Medicina Interna"] +  datos$hombres[nombres == "Medicina Interna"]

#Cantidad de personas que deber�an existir en cada �rea para obtener la diferencia de proporciones solicitada
#en el enunciado en la muestra
N_D  <- datos$mujeres[nombres == "Dermatolog�a"] + 2
N_MI <- datos$mujeres[nombres == "Medicina Interna"] - 8

prop_mujeres_D  <- (N_D)/autores_D
prop_mujeres_MI <- (N_MI)/autores_MI

dif1 <- prop_mujeres_D - prop_mujeres_MI

#Se corrobora que la diferencia es de 0.15
cat(dif1)

#Se obtienen los valores para aplicar la prueba de Wilson para dos proporciones, ya que esta
#calcula la cantidad de personas que deben existir en cada grupo

fraccion = N_D/(N_D + N_MI)

poder_deseado <- 0.8
intervalo <- 0.025

bsamsize(p1=prop_mujeres_D,
         p2 = prop_mujeres_MI,
         fraction = fraccion,
         alpha = intervalo,
         power=poder_deseado)


"
Respuesta: Se deber�an estudiar a 202 autores para mantener una diferencia de proporciones aproximada de 0.15 entre las mujeres
del �rea de dermatolog�a y medicina interna.
------------------------------------------------------------------------------------------------------------------------------


"
  