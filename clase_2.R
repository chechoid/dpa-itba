

kiwi_ar <- readRDS("PEPA/kiwi_ar.RDS")

View(kiwi_ar)


# Ver la estructura del data frame
str(kiwi_ar)


library(tidyverse) # Limpieza y manipulación de datos

kiwi_ar %>% 
  count(puesto) # Cuenta la cantidad de casos por puesto

# Tidyverse ya está cargado
library(scales) # Es para usar la función percent() a continuación  

# Hace click en "Run Code"
kiwi_ar %>% 
  count(puesto) %>% 
  mutate(porcentaje = percent(n / sum(n))) # Toma el valor de la celda en n y lo divide por el total de la columna n


library(summarytools)
kiwi_ar %>% 
  select(puesto) %>% 
  freq(report.nas = FALSE)


# Gráficos de variables categóricas ---------

#Gráficos de barras
plot(kiwi_ar$puesto)
plot(kiwi_ar$ajuste_porcentaje)


# Cargar el paquete con la función library()
library(ggplot2)

# Realizar el gráfico de barras
ggplot(kiwi_ar,             # Primero indicamos el nombre del dataframe
       aes(y = puesto)) +   # Dentro de aes() mapeamos la variable que queremos graficar
  geom_bar()                # Indico el tipo de gráfico



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






hist(kiwi_ar$ajuste_porcentaje)


summary(kiwi_ar$sueldo_bruto)
