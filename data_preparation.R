library(tidyverse)

kiwi <- read_delim("datos/kiwi_rh.csv", delim = ",")

glimpse(kiwi)

kiwi_ar <- kiwi %>% 
  filter(pais == "Argentina", 
         genero %in% c("Masculino", "Femenino")) %>% 
  select(-pais, -sueldo_dolar, -multiplicador, -sueldo_ft, -tipo_cambio, -cuenta, -trabajo) %>% 
  filter(!tipo_contratacion %in% c("Part time", "Pasante"),
         puesto != "Pasante") 


glimpse(kiwi_ar)

kiwi_ar %>% 
  count(tipo_contratacion)

kiwi_ar %>% 
  count(puesto)

kiwi_ar %>% 
  count(rubro, sort = T) %>% 
  print(n = Inf)

kiwi_ar <- kiwi_ar %>% 
  mutate(rubro = str_replace(rubro, 
                             "Tecnologías de Información, Sistemas, y afines", 
                             "IT"),
         rubro = str_replace(rubro, 
                             "Terminales automotrices, fábricas autopartistas, y afines",
                             "Industria autopartista"),
         rubro = str_replace(rubro,
                             "Transporte (incluyendo aviación civil; ferrocarriles por carretera)",
                             "Transporte"),
         rubro = str_replace(rubro,
                             "Petróleo y producción de gas; refinación de petróleo",
                             "Oil & gas"),
         rubro = str_replace(rubro,
                             "Agricultura; plantaciones,otros sectores rurales",
                             "Agro"))

glimpse(kiwi_ar)

summary(kiwi_ar$ajuste_porcentaje)

kiwi_ar <- kiwi_ar %>% 
  filter(ajuste_porcentaje < 100,
         puesto != "Director") %>% 
  select(-Marca.temporal)

kiwi_ar$id <- rep(1:nrow(kiwi_ar))

kiwi_ar <- kiwi_ar %>% 
  select(id, everything())


kiwi_ar %>% 
  count(puesto)

kiwi_ar <- kiwi_ar %>% 
  mutate(puesto = factor(puesto, levels = c("Administrativo", "Analista", "HRBP",
                                            "Responsable", "Jefe", "Gerente")))

summary(kiwi_ar)

kiwi_ar %>% 
  count(provincia, sort = T)

unique(kiwi_ar$provincia)

kiwi_ar <- kiwi_ar %>% 
  mutate(region = if_else(provincia %in% c("Ciudad Autónoma de Buenos Aires","Córdoba", "Buenos Aires", "La Pampa", "Sante Fe"),
                          "Centro", 
                          if_else(
                            provincia %in% c("Misiones", "Corrientes", "Chaco", "Formosa", "Entre Ríos"),
                            "NEA", 
                            if_else(provincia %in% c("Jujuy", "Salta", "Tucumán", "Catamarca", "Santiago del Estero"),
                                    "NOA",
                                    if_else(provincia %in% c("Mendoza", "San Juan", "San Luis", "La Rioja"),
                                            "Cuyo",
                                            "Patagonia"))))) 

glimpse(kiwi_ar)
kiwi_ar <- kiwi_ar %>% 
  select(genero, genero_diverso, edad, discapacidad, nivel_formacion, carrera_grado, tipo_universidad, provincia, region, everything())

kiwi_ar <- kiwi_ar %>% 
  mutate(es_lider = if_else(puesto %in% c("Responsable", "Jefe", "Gerente"),1,0))

kiwi_ar <- kiwi_ar %>% 
  mutate(rangos_aumentos = case_when(
    ajuste_porcentaje == 0  ~ "Sin aumentos",
    ajuste_porcentaje <= 10 ~ "Entre 1 y 10",
    ajuste_porcentaje <= 20 ~ "Entre 11 y 20",
    ajuste_porcentaje <= 30 ~ "Entre 21 y 30",
    ajuste_porcentaje > 30  ~ "Más de 30"
  ),
  rangos_aumentos = factor(rangos_aumentos, levels = c("Sin aumentos", "Entre 1 y 10", "Entre 11 y 20", "Entre 21 y 30", "Más de 30")))

satisfaccion <- kiwi_ar %>% 
  select(id, genero, puesto, rubro, satisfaccion) %>% 
  filter(rubro != "Otros", 
         !is.na(satisfaccion))

analistas <- kiwi_ar %>% 
  filter(puesto == "Analista")

# Guardar archivos
saveRDS(kiwi_ar, file = "PEPA/kiwi_ar.RDS")
write_delim(kiwi_ar, file = "PEPA/kiwi_ar.csv", delim = ";")
saveRDS(satisfaccion, file = "PEPA/satisfaccion.RDS")
saveRDS(analistas, file = "PEPA/analistas.RDS")


# Testeando pie charts
table(kiwi_ar$puesto)

summary(kiwi_ar$ajuste_porcentaje)
summary(kiwi_ar$rangos_aumentos)
