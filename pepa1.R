## ----setup, include=FALSE---------------------------------------------------------------------------------------------
library(learnr)
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)


## ----two-plus-two, exercise=TRUE--------------------------------------------------------------------------------------



## ----edad, exercise=TRUE, exercise.lines = 9--------------------------------------------------------------------------
# Poné el año en el que naciste
anio_nacimiento <- ____
anio_actual <- 2021

# Calculá tu edad
edad <- _______ - ________

# Visualizá los resultados
edad


## ----edad-hint-1------------------------------------------------------------------------------------------------------
# Reemplazá las líneas ____ por el año en el que naciste
# Para calcular tu edad restá el año actual menos tu año de nacimiento


## ----edad-hint-2------------------------------------------------------------------------------------------------------
# Poné el año en el que naciste
anio_nacimiento <- 1979
anio_actual <- 2021

# Calculá tu edad
edad <- anio_actual - anio_nacimiento

# Visualizá los resultados
edad


## ----carga-datos, echo=TRUE-------------------------------------------------------------------------------------------
# Cargamos los datos
kiwi_ar <- readRDS("kiwi_ar.RDS")


## ----carga-datos2-----------------------------------------------------------------------------------------------------
library(readr)
library(dplyr)

kiwi_ar <- read_delim("kiwi_ar.csv", delim = ";")

# Ordena los puestos por jerarquía
kiwi_ar <- kiwi_ar %>% 
  mutate(puesto = factor(puesto, levels = c("Administrativo", "Analista", "HRBP",
                                            "Responsable", "Jefe", "Gerente")))

kiwi_ar <- kiwi_ar %>% 
  mutate(rangos_aumentos = case_when(
    ajuste_porcentaje == 0  ~ "Sin aumentos",
    ajuste_porcentaje <= 10 ~ "Entre 1 y 10",
    ajuste_porcentaje <= 20 ~ "Entre 11 y 20",
    ajuste_porcentaje > 30  ~ "Entre 21 y 30",
    ajuste_porcentaje = TRUE ~ "Más de 30"
  ),
  rangos_aumentos = factor(rangos_aumentos, levels = c("Sin aumentos", "Entre 1 y 10", "Entre 11 y 20", "Entre 21 y 30", "Más de 30")))



## ----str, exercise=TRUE-----------------------------------------------------------------------------------------------
# Ver la estructura del data frame
str(kiwi_ar)


## ----str-q------------------------------------------------------------------------------------------------------------
quiz(
  question("¿Cuántas filas y columnas tiene el dataframe 'salarios'?",
  answer("548 columnas y 44 variables", message = "Las observaciones son las filas y las variables son las columnas. Columna es sinónimo de variable en este contexto."),
  answer("548 variables y 44 filas", message = "Mirá de nuevo la primera fila del resultado de la estructura del dataframe kiwi_ar. Primero indica las observaciones, y luego las variables"),
  answer("548 filas y 44 columnas", correct = TRUE, message = "Muy bien!"),
  submit_button = "Enviar respuesta",
  try_again_button = "Intenta de nuevo",
  allow_retry = TRUE
))


## ----librerias--------------------------------------------------------------------------------------------------------
library(tidyverse)
library(funModeling)
library(summarytools)
library(scales)


## ----cate-1, exercise=TRUE--------------------------------------------------------------------------------------------
library(tidyverse) # Limpieza y manipulación de datos

kiwi_ar %>% 
  count(puesto) # Cuenta la cantidad de casos por puesto


## ----cate-2, exercise=TRUE--------------------------------------------------------------------------------------------
# Tidyverse ya está cargado
library(scales) # Es para usar la función percent() a continuación  

# Hace click en "Run Code"
kiwi_ar %>% 
  count(puesto) %>% 
  mutate(porcentaje = percent(n / sum(n))) # Toma el valor de la celda en n y lo divide por el total de la columna n


## ----cate-3, exercise = TRUE------------------------------------------------------------------------------------------

kiwi_ar %>% 
  select(puesto) %>% 
  freq(report.nas = FALSE)


## ----cate-4-----------------------------------------------------------------------------------------------------------
quiz(
  question("¿Qué porcentaje de las respuestas son de personas en posiciones no jerárquicas (considerando Administrativos, Analistas y HRBP)?",
           answer("10.40%", message = "Ese es el porcentaje de los HRBP"),
           answer("56.39%", message = "Excelente!", correct = T),
           answer("45.99%", message = "Esa es el porcentaje acumulado de Administrativo y Analista. Falta sumar el porcentaje de los HRBP."),
           submit_button = "Enviar respuesta",
           try_again_button = "Intenta de nuevo",
           allow_retry = TRUE
))


## ----cate-5, exercise=TRUE--------------------------------------------------------------------------------------------
kiwi_ar %>% 
  count(_____)


## ----cate-5-hint-1----------------------------------------------------------------------------------------------------
# Reemplazá el espacio vacío dentro de la función count() con el nombre de la variable 'mate'


## ----cate-5-hint-2----------------------------------------------------------------------------------------------------
kiwi_ar %>% 
  count(mate)


## ----cate-6, exercise=TRUE, exercise.lines = 5------------------------------------------------------------------------
kiwi_ar %>% 
  ______(____) %>% # Completá con la función y la variable que corresponde
  mutate(porcentaje = round(n/sum(n),2)) # Agrega una columna con los porcentajes redondeando a 2 decimales.


## ----cate-6-hint-1----------------------------------------------------------------------------------------------------
# Usá la función count() para contar la cantidad de respuestas por cada caso
# La variable que queremos analizar se llama 'mate'


## ----cate-6-hint-2----------------------------------------------------------------------------------------------------
kiwi_ar %>% 
  count(mate) %>% # Completá con la función y la variable que corresponde
  mutate(porcentaje = round(n/sum(n),2)) # Agrega una columna con los porcentajes redondeando a 2 decimales.


## ----cate-7, exercise=TRUE, exercise.lines = 4------------------------------------------------------------------------
kiwi_ar %>% 
  select(______) %>% 
  freq(report.nas = FALSE)


## ----cate-7-hint-1----------------------------------------------------------------------------------------------------
# Reemplazá el espacio vacío dentro de la función select() con el nombre de la variable 'region'


## ----cate-7-hint-2----------------------------------------------------------------------------------------------------
kiwi_ar %>% 
  select(region) %>% 
  freq(report.nas = FALSE)


## ----catev-1, exercise=TRUE-------------------------------------------------------------------------------------------
# Realizar un gráfico de barra
plot(kiwi_ar$puesto)


## ----catev-2, exercise=TRUE-------------------------------------------------------------------------------------------
# Realizar un gráfico de cantidad de personas a cargo
plot(kiwi_ar$sueldo_bruto)


## ----catev-3, exercise=TRUE-------------------------------------------------------------------------------------------
# Cargar el paquete con la función library()
_______(ggplot2)

# Realizar el gráfico de barras
ggplot(kiwi_ar,             # Primero indicamos el nombre del dataframe
       aes(x = puesto)) +   # Dentro de aes() mapeamos la variable que queremos graficar
  geom_bar()                # Indico el tipo de gráfico


## ----catev-3-hint-----------------------------------------------------------------------------------------------------
# Cargar el paquete con la función library()
library(ggplot2)

# Realizar el gráfico de barras
ggplot(kiwi_ar,             # Primero indicamos el nombre del dataframe
       aes(x = puesto)) +   # Dentro de aes() mapeamos la variable que queremos graficar
  geom_bar()                # Indico el tipo de gráfico


## ----catev-4----------------------------------------------------------------------------------------------------------
quiz(
  question("¿Qué pasa si asignamos la variable puesto al eje y?",
           answer("Nada", message = "No pasa nada grave pero algo pasa, probá reemplazando x por y en el bloque de código anterior"),
           answer("Le agarra un virus a la compu y aparezco en la lista de los más buscados por la Interpol", message = "Les dije 1 millón de veces que no sean exagerados"),
           answer("El gráfico 'se da vuelta'", correct = TRUE, message = "Correcto! Los nombres de los puestos aparecen en el eje y (más legibles de hecho) y las barras están apaisadas"),
         submit_button = "Enviar respuesta",
         try_again_button = "Intenta de nuevo",
         allow_retry = TRUE  
  )
)


## ----categ-5----------------------------------------------------------------------------------------------------------
# Preparación de los datos
puestos <- kiwi_ar %>% 
  select(puesto) %>%    # Seleccionamos la columna tipo universidad
  group_by(puesto) %>%  # Agrupamos los resultados
  summarise (n = n()) %>%         # Contamos la cantidad de casos para cada grupo
  mutate(freq = n/sum(n)) %>%     # Creamos una columna nueva calculando la frecuencia relativa
  arrange(-n)                     # Ordenamos los resultados de mayor a menor

# Calcular los porcentajes acumulados (tope de cada rectángulo)
puestos$ymax <- cumsum(puestos$freq)

# Calcula el límite inferior
puestos$ymin <- c(0, head(puestos$ymax, n=-1))

ggplot(puestos, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=puesto)) +
  geom_rect()


## ----pie-chart, echo=TRUE---------------------------------------------------------------------------------------------
# Preparación de los datos
puestos <- kiwi_ar %>% 
  select(puesto) %>%    # Seleccionamos la columna tipo universidad
  group_by(puesto) %>%  # Agrupamos los resultados
  summarise (n = n()) %>%         # Contamos la cantidad de casos para cada grupo
  mutate(freq = n/sum(n)) %>%     # Creamos una columna nueva calculando la frecuencia relativa
  arrange(-n)                     # Ordenamos los resultados de mayor a menor

# Calcular los porcentajes acumulados (tope de cada rectángulo)
puestos$ymax <- cumsum(puestos$freq)

# Calcula el límite inferior
puestos$ymin <- c(0, head(puestos$ymax, n=-1))

# Hacemos el gráfico
ggplot(puestos, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=puesto)) +
  geom_rect() +
  coord_polar(theta="y") + # Elimina esta línea de código para entender lo que hace R.
  xlim(c(2, 4)) + # Prueben eliminando esta línea de código para hacer un gráfico de torta
  theme_void() +                             # Cambia estética del gráfico
  theme(legend.position = "top",            # Cambia la posición de la leyenda
        plot.title.position = "plot") +      # Cambia la posición del título  
  labs(title = "Respuestas por puesto",
       fill = "Puesto")



## ----num-1, exercise = TRUE-------------------------------------------------------------------------------------------
# Análisis cuantitativo no agrupado 
kiwi_ar %>% 
  count(ajuste_porcentaje)


## ----num-2, exercise=TRUE---------------------------------------------------------------------------------------------
kiwi_ar %>% 
  select(rangos_aumentos) %>% 
  freq(report.nas = FALSE)


## ----num-3, exercise=TRUE---------------------------------------------------------------------------------------------
ggplot(kiwi_ar, aes(x = ajuste_porcentaje)) +
  geom_bar()


## ----num-4, exercise=TRUE---------------------------------------------------------------------------------------------
ggplot(kiwi_ar, aes(x = rangos_aumentos)) +
  geom_bar()


## ----hist-1, exercise = TRUE------------------------------------------------------------------------------------------
hist(kiwi_ar$ajuste_porcentaje)


## ----hist-2, exercise=TRUE--------------------------------------------------------------------------------------------
ggplot(kiwi_ar, aes(x = ajuste_porcentaje)) +
  geom_histogram(bins = 15) # Ajusta la cantidad de barras que van a aparecer en el gráfico


## ----freq-1, exercise = TRUE------------------------------------------------------------------------------------------
ggplot(kiwi_ar, aes(x = ajuste_porcentaje)) +
  geom_histogram(bins = 15) +
  geom_freqpoly(aes(ajuste_porcentaje),  # Añadimos el polígono de frecuencia
                bins = 15)


## ----carga-datos3-----------------------------------------------------------------------------------------------------
library(funModeling)

percentiles = profiling_num(kiwi_ar$sueldo_bruto)

p05 = percentiles[1,6]
p95 = percentiles[1,10]

kiwi_limpio = kiwi_ar %>% 
  filter(between(sueldo_bruto, p05, p95))


## ----hist-4, exercise=TRUE--------------------------------------------------------------------------------------------
ggplot(kiwi_ar, aes(x = _______)) +
  geom_histogram()


## ----hist-4-hint-1----------------------------------------------------------------------------------------------------
# Colocá el nombre de la variable a graficar (sueldo_bruto)


## ----hist-4-hint-2----------------------------------------------------------------------------------------------------
ggplot(kiwi_ar, aes(x = sueldo_bruto)) +
  geom_histogram()


## ----hist-5, exercise = TRUE------------------------------------------------------------------------------------------
# Cuál imaginan que es la función para calcular el valor máximo de una columna
___(kiwi_ar$____)

# Detectar el sueldo bruto más bajo
min(_____$sueldo_bruto)


## ----hist-5-hint------------------------------------------------------------------------------------------------------
# Cuál imaginan que es la función para calcular el valor máximo de una columna
max(kiwi_ar$sueldo_bruto)

# Detectar el sueldo bruto más bajo
min(kiwi_ar$sueldo_bruto)


## ----hist-6-----------------------------------------------------------------------------------------------------------
quiz(
  question(
    "¿En qué rango salarial encontramos otro outlier o valor atípico?",
    answer("Entre $ 250.000 y $ 500.000", message = "Nop, probá otra vez"),
    answer("Entre $ 500.000 y $ 1.000.000", correct = TRUE, message = "Excelente! Hay una respuesta de $ 650.000"),
    answer("Entre $ 1.000.000 y $ 1.500.000", message = "Nop, probá otra vez"),
    allow_retry = TRUE, 
    submit_button = "Enviar respuesta",
    try_again_button = "Intenta de nuevo"
  )
)


## ----hist-7, exercise=TRUE--------------------------------------------------------------------------------------------
____(______$________)


## ----hist-7-hint-1----------------------------------------------------------------------------------------------------
# La función de R base es hist
# Poné primero el nombre del nuevo data frame y luego la variable sueldo_bruto


## ----hist-7-hint-2----------------------------------------------------------------------------------------------------
# El nuevo data frame se llama kiwi_limpio
# Recordá poner nombre_dataframe$nombre_variable


## ----hist-7-hint-3----------------------------------------------------------------------------------------------------
hist(kiwi_limpio$sueldo_bruto


## ----est-1, echo=TRUE-------------------------------------------------------------------------------------------------
# Personas en posiciones de Gerente
kiwi_ar %>% 
  filter(puesto == "Gerente") %>% 
  count(puesto)

# Conjunto de mujeres en posiciones de liderazgo
kiwi_ar %>% 
  filter(es_lider == 1, genero == "Femenino") %>% 
  count(puesto)

# Personas que recibieron más de un aumento salarial
kiwi_ar %>% 
  filter(ajuste %in% c("2 ajustes", "3 o más ajustes")) %>% 
  count(ajuste)


## ----carga-datos-4----------------------------------------------------------------------------------------------------
df_edad <- read.csv("edades.csv")
edades <- df_edad %>% select(Edad) %>% pull()
edades <- sort(edades)


## ----prom-1, exercise = TRUE------------------------------------------------------------------------------------------
edades


## ----prom-2, exercise=TRUE--------------------------------------------------------------------------------------------
____(edades)


## ----prom-2-hint------------------------------------------------------------------------------------------------------
mean(edades)


## ----prom-3, exercise=TRUE--------------------------------------------------------------------------------------------
# Crear un nuevo vector con los valores de edad
edades_2 <- edad

# Modificar el primer elemento y poner 120
edades_2[1] <- ____
edades_2

# Calcular nuevo promedio del vector edades_2
____(_________)


## ----prom-3-hint------------------------------------------------------------------------------------------------------
# Crear un nuevo vector con los valores de edad
edades_2 <- edades

# Modificar el primer elemento y poner 120
edades_2[1] <- 120
edades_2

# Calcular nuevo promedio del vector edades_2
mean(edades_2)


## ----promedio-2-------------------------------------------------------------------------------------------------------
# Crear un nuevo vector con los valores de edad
edades_2 <- edades

# Modificar el primer elemento y poner 120
edades_2[1] <- 120


## ----mean-3, exercise = TRUE------------------------------------------------------------------------------------------
# Guardar el promedio edades
promedio_edad <- mean(edades)

# Grafiquemos las edades para cada persona
ggplot(df_edad, aes(x = Edad, y = Inicial)) +
  geom_point(size = 3) +
  geom_vline(xintercept = promedio_edad, size = 1, color = "red")


## ----med-1, exercise = TRUE-------------------------------------------------------------------------------------------
# Correr el script y pensar cuál será la mediana
edades

# Verificar calculando la mediana con la función median
median(______)


## ----med-1-hint-------------------------------------------------------------------------------------------------------
# Correr el script y pensar cuál será la mediana
edades

# Verificar calculando la mediana con la función median
median(edades)


## ----med-2, exercise = TRUE-------------------------------------------------------------------------------------------
#Crear un nuevo vector
edades_3 <- edades

# Agregar un nuevo valor
edades_3[12] <- 50

# Calcular la mediana
_____(edades_3)


## ----med-2-hint-------------------------------------------------------------------------------------------------------
#Crear un nuevo vector
edades_3 <- edades

# Agregar un nuevo valor
edades_3[12] <- 50

# Calcular la mediana
median(edades_3)


## ----med-3, exercise = TRUE-------------------------------------------------------------------------------------------
#Crear un nuevo vector
edades_4 <- edades

# Modificar el primer valor con un valor alto
edades_3[1] <- 150

# Calcular la mediana
median(edades_4)



## ----q1, exercise=TRUE------------------------------------------------------------------------------------------------
summary(kiwi_ar$edad)


## ----q2, exercise = TRUE----------------------------------------------------------------------------------------------
# Calcular los deciles
quantile(kiwi_ar$edad, c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8))



## ----q3, exercise = TRUE----------------------------------------------------------------------------------------------
# Calcular el primer cuartil
q1 <- quantile(kiwi_ar$edad, 0.25)

# Calcular el tercer cuartil
q3 <- ________(______$___ , ___)

# Calcular el rango intercuartil
rango_ic <- ___ - ___

rango_ic


## ----q4, exercise=TRUE------------------------------------------------------------------------------------------------
# Calcular el primer cuartil
q1 <- quantile(kiwi_ar$edad, 0.25)

# Calcular el tercer cuartil
q3 <- quantile(kiwi_ar$edad, 0.75)

# Calcular el rango intercuartil
rango_ic <- q3 - q1

# Calcular límite inferior
lim_inf <- q1 - rango_ic * 1.5

# Calcular límite superior
lim_sup <- _______________ * 1.5

# Ver ambos resultados
lim_inf
lim_sup


## ----q4-hint----------------------------------------------------------------------------------------------------------
# Calcular el primer cuartil
q1 <- quantile(kiwi_ar$edad, 0.25)

# Calcular el tercer cuartil
q3 <- quantile(kiwi_ar$edad, 0.75)

# Calcular el rango intercuartil
rango_ic <- q3 - q1

# Calcular límite inferior
lim_inf <- q1 - rango_ic * 1.5

# Calcular límite superior
lim_sup <- q3 - rango_ic * 1.5

# Ver ambos resultados
lim_inf
lim_sup


## ----q5, exercise = TRUE----------------------------------------------------------------------------------------------
ggplot(kiwi_ar, aes(x = edad)) +
  geom_boxplot()


## ----q6, out.width="70%"----------------------------------------------------------------------------------------------
ggplot(kiwi_ar, aes(x = edad)) +
  geom_histogram()

ggplot(kiwi_ar, aes(x = edad)) +
  geom_boxplot()



## ----q7, exercise = TRUE----------------------------------------------------------------------------------------------
# install.packages("funModeling") # Por si es necesario

# Cargar librería
library(funModeling)

# Probar la función profiling_num()
profiling_num(kiwi_ar$edad)


## ----dis-1, exercise=TRUE---------------------------------------------------------------------------------------------
# Calcular la varianza
___(kiwi_ar$edad)

# Calcular el desvío estándar
__(kiwi_ar$edad)


## ----dis-1-hint-1-----------------------------------------------------------------------------------------------------
# Busquen en google. Aprender a buscar soluciones en google también es parte del aprendizaje para programar


## ----dis-1-hint-2-----------------------------------------------------------------------------------------------------
# Calcular la varianza
___(kiwi_ar$edad)

# Calcular el desvío estándar
__(kiwi_ar$edad)


## ----dis-2, exercise=TRUE---------------------------------------------------------------------------------------------
# Calcular el desvío estándar de la variable edad
desvio <- sd(kiwi_ar$edad)

# Calcular el promedio de la variable edad
promedio <- mean(kiwi_ar$edad)

# Calcular el coeficiente de variación
coef_var <- desvio / promedio

coef_var


## ----z1, exercise=TRUE------------------------------------------------------------------------------------------------
# Correr el script
edades

# Hacer un summary de edades
summary(edades)


## ----z2, exercise=TRUE------------------------------------------------------------------------------------------------
# Correr el script
scale(edades, center= TRUE, scale=TRUE)


## ----z3, exercise=TRUE------------------------------------------------------------------------------------------------
# Guardar los valores estandarizados en un objeto
z_edad <- scale(edades, center= TRUE, scale=TRUE)

# Hacer un summary de este nuevo objeto
summary(z_edad)


## ----desc-1, exercise=TRUE, exercise.lines=10-------------------------------------------------------------------------
# Calcular el promedio
___(kiwi_ar$sueldo_bruto)

# Calcular la mediana
____(______$_______)

# Calcular el desvío estándar
sd(____$_____)


## ----des-1-hint-1-----------------------------------------------------------------------------------------------------
# La función para calcular el promedio es mean

# La función para calcular la mediana se llama median. Poner primero el nombre del data frame y luego el de la columna.

# Poner el nombre del data frame kiwi_ar y luego el de la columna sueldo_bruto


## ----des-2-hint-2-----------------------------------------------------------------------------------------------------
# Calcular el promedio
mean(kiwi_ar$sueldo_bruto)

# Calcular la mediana
median(kiwi_ar$sueldo_bruto)

# Calcular el desvío estándar
sd(kiwi_ar$sueldo_bruto)


## ----des-3, exercise=TRUE---------------------------------------------------------------------------------------------
kiwi_ar %>% 
  select(sueldo_bruto) %>% 
  summarise(promedio = mean(sueldo_bruto),
            mediana = median(sueldo_bruto),
            desvio = sd(sueldo_bruto))


## ----des-4, exercise = TRUE-------------------------------------------------------------------------------------------
# Realizar un histograma de sueldo_bruto
ggplot(_____, aes(x  = sueldo_bruto)) +
  geom_______(bins = 20)


## ----des-4-hint-1-----------------------------------------------------------------------------------------------------
# Poner el nombre del data frame kiwi_ar

# Para hacer un histograma necesitamos usar geom_histogram()


## ----des-4-hint-2-----------------------------------------------------------------------------------------------------
# Realizar un histograma de sueldo_bruto
ggplot(kiwi_ar, aes(x  = sueldo_bruto)) +
  geom_histogram(bins = 20)


## ----des-5, exercise = TRUE, exercise.lines = 22----------------------------------------------------------------------
# Calcular el valor para el percentil 5
p5 <- quantile(kiwi_ar$sueldo_bruto, 0.05)
p5

# Calcular el valor para el percentil 95
p95 <- _______(_______$__________, 0.95)
p95

# Filtrar los sueldos por debajo y por encima de los percentiles 5 y 95 guardándolos en un nuevo data frame
kiwi_limpio <- kiwi_ar %>% 
  filter(between(            # Filtra los casos que estén dentro de los límites
    sueldo_bruto,            # Columna para filtrar
    p5,                      # Límite inferior
    p95                      # Límite superior
  ))

# Realizar un histograma de sueldo_bruto
ggplot(kiwi_limpio, aes(x  = __________)) +
  geom_histogram(bins = 20)


## ----des-5-hint-1-----------------------------------------------------------------------------------------------------
# Utilizar la fórmula quantile y luego indicar el nombre del data frame y de la columna

# Asignar al eje x la variable sueldo_bruto


## ----des-5-hint-2-----------------------------------------------------------------------------------------------------
# Calcular el valor para el percentil 5
p5 <- quantile(kiwi_ar$sueldo_bruto, 0.05)
p5

# Calcular el valor para el percentil 95
p95 <- quantile(kiwi_ar$sueldo_bruto, 0.95)
p95

# Filtrar los sueldos por debajo y por encima de los percentiles 5 y 95 guardándolos en un nuevo data frame
kiwi_limpio <- kiwi_ar %>% 
  filter(between(            # Filtra los casos que estén dentro de los límites
    sueldo_bruto,            # Columna para filtrar
    p5,                      # Límite inferior
    p95                      # Límite superior
  ))

# Realizar un histograma de sueldo_bruto
ggplot(kiwi_limpio, aes(x  = sueldo_bruto)) +
  geom_histogram(bins = 20)


## ----des-6, exercise = TRUE-------------------------------------------------------------------------------------------
# Realizar un boxplot para el data frame kiwi_ar
ggplot(_____, aes(x = sueldo_bruto)) +
  geom_______()

# Realizar un boxplot para el data frame kiwi_limpio
ggplot(kiwi_limpio, __________)) +
    geom_boxplot()


## ----des-6-hint-1-----------------------------------------------------------------------------------------------------
# Reemplazar el primer espacio vacío por el nombre del data frame
# Para hacer un boxplot necesitás usar geom_boxplot()

# Hay que asignar columna sueldo_bruto al eje x dentro de aes()
# Tené cuidado con los paréntisis al final de la primera línea del código


## ----des-6-solution---------------------------------------------------------------------------------------------------
# Realizar un boxplot para el data frame kiwi_ar
ggplot(kiwi_ar, aes(x = sueldo_bruto)) +
  geom_boxplot()

# Realizar un boxplot para el data frame kiwi_limpio
ggplot(kiwi_limpio, aes(x = sueldo_bruto)) +
    geom_boxplot()


## ----des-7, exercise=TRUE---------------------------------------------------------------------------------------------
ggplot(kiwi_limpio, aes(x = puesto, y = sueldo_bruto)) +
    geom_boxplot()

