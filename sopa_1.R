# Librerías ----------------
library(readxl)
library(tidyverse)
library(funModeling)
library(gt)
library(lubridate)
library(summarytools)


options(scipen = 999)

azul <- "#344D7E"
verde <-  "#4A9FC7"
rosa1 <- "#B95192"
rosa2 <- "#EE5777"
naranja <- "#FF764C"
amarillo <- "#FFA600"
gris <- "#75838F"
lila <- "#755395"
rojo <- "#943126"

# Carga de datos --------------------

# Un listado de empleados
plantel <- read_xlsx("datos/plantel.xlsx")

# Encuesta de Sueldos de RH 2020
kiwi <- read_delim("datos/kiwi_rh.csv", delim = ",") %>% 
  filter(pais == "Argentina")

# kiwi = Key Investigation of Wages and Incomes 

# Análisis exploratorio -------------

# Frecuencias

# Contar la cantidad de empleados por nivel de contribución

plantel$nivel_contribucion

plantel %>% 
  count(nivel_contribucion, sort = T)

# Contar las empresas donde se podía tomar mate en la oficina
kiwi %>% 
  filter(!is.na(mate)) %>% 
  count(mate)


# Creamos una columna para extraer el año de las columnas de fechas

str(plantel)

plantel <- plantel %>% 
  mutate(anio_ingreso = year(floor_date(fecha_ingreso, unit = "year")),
         q_ingreso = quarter(fecha_ingreso))

plantel$anioq_ingreso <- paste(plantel$anio_ingreso, plantel$q_ingreso, sep = "-")


# Creamos una columna con las generaciones a la que pertenecen los empleados
# Fuente: https://www.mckinsey.com/industries/consumer-packaged-goods/our-insights/true-gen-generation-z-and-its-implications-for-companies#
plantel <- plantel %>% 
  mutate(anio_nacimiento = year(fecha_nacimiento),
         generacion = case_when(
           anio_nacimiento <= 1959 ~ "Baby Boomer",
           anio_nacimiento <= 1979 ~ "Gen X",
           anio_nacimiento <= 1994 ~ "Gen Y",
           anio_nacimiento = TRUE  ~ "Gen Z"
  )) %>% 
  select(-anio_nacimiento)

# Distribución de frecuencias
plantel %>% 
  filter(anio_ingreso > 2015) %>% 
  count(anio_ingreso, q_ingreso) %>% 
  arrange()


plantel %>% 
  filter(anio_ingreso > 2015) %>% 
  select(anioq_ingreso) %>% 
  freq(report.nas = FALSE)


plantel %>% 
  filter(anio_ingreso > 2015) %>% 
  select(anioq_ingreso) %>% 
  freq(report.nas = FALSE,)


# Datos cuantitativos agrupados -----------

contactos <- kiwi %>% 
  select(contactos_linkedin) %>% 
  filter(!is.na(contactos_linkedin)) 


contactos$contactos_linkedin

ggplot(contactos, aes(x = contactos_linkedin)) + 
  geom_histogram()

# Creamos una columna nueva para asignar los grupos
contactos$grupo <- cut(contactos$contactos_linkedin, 
                       breaks = 5) # Crea 5 grupos

summary(contactos)

freq(contactos$grupo,
     report.nas = FALSE)


# Gráfico de barras
idioma <- kiwi %>% 
  filter(!is.na(idioma_porcentaje),
         idioma_exigencia == "Si") %>% 
  count(idioma_porcentaje) %>% 
  mutate(idioma_porcentaje = idioma_porcentaje)

ggplot(idioma, aes(x = factor(idioma_porcentaje), y = n)) +
  geom_col() +
  labs(title = "Porcentaje del tiempo usando idioma extranjero",
       subtitle = "Personas a las que les exigieron saber un idioma para ser contratadas")


limpieza <- profiling_num(kiwi$sueldo_bruto)
limpieza

p05 <- limpieza[1,6]
p95 <- limpieza[1,10]

sueldos <- kiwi %>% 
  filter(tipo_contratacion != "Part time",
         between(sueldo_bruto,
                 p05,
                 p95))
# Histograma

ggplot(sueldos, aes(x = sueldo_bruto)) +
  geom_histogram() +
  ggtitle("Histograma de Sueldo Bruto")

# Histograma con Polígono de Frecuencias
ggplot(sueldos, aes(x = sueldo_bruto)) +
  geom_histogram(bins = 20) +
  labs(title = "Histograma de Sueldo Bruto",
       subtitle = "bins = 20") +
  geom_freqpoly(aes(sueldo_bruto),
                bins = 20)



# Serie de tiempo 

ingresos <- plantel %>% 
  filter(year(fecha_ingreso) > 2010)

ingresos <- ingresos %>% 
  count(anio_ingreso)

gt(ingresos)

ggplot(ingresos, aes(x = anio_ingreso, y = n)) +
  geom_line() 


# Gráfico de torta

educ <- kiwi %>% 
  select(tipo_universidad) %>%  
  group_by(tipo_universidad) %>% 
  summarise (n = n()) %>% 
  mutate(freq = n/sum(n)) %>% 
  arrange(-n)

# Compute the cumulative percentages (top of each rectangle)
educ$ymax <- cumsum(educ$freq)

# Compute the bottom of each rectangle
educ$ymin <- c(0, head(educ$ymax, n=-1))

# Compute label position
educ$labelPosition <- (educ$ymax + educ$ymin) / 2

# Compute a good label
educ$label <- paste0(educ$tipo_universidad, "\n Cant: ", educ$n)

# Make the plot
ggplot(educ, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=tipo_universidad)) +
  geom_rect() +
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  xlim(c(2, 4)) +# Try to remove that to see how to make a pie chart
  scale_fill_manual(values = c(gris, verde, azul)) +
  theme_void() +
  theme(legend.position = "right",
        panel.background = element_blank(),
        plot.title.position = "plot") +
  labs(title = "Tipo de Universidad",
       fill = "Tipo de Universidad")
