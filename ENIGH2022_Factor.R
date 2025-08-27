setwd("/Users/gerardoruiz/Desktop/SEMESTRES/Servicio Social")
library(tidyverse) 
library(foreign) 
library(srvyr) 
library(readxl)
library(writexl)

viv <- read.csv("viviendas.csv")

# Nuevas columnas para identificar el Estado al que corresponde cada vivienda
viv <- mutate(viv,
              folioviv=str_pad(folioviv, 10, "left", pad = "0"),
              ent=as.numeric(str_sub(folioviv, 1,2)), 
              entidad=case_when(ent==1  ~ "Aguascalientes",
                                ent==2  ~ "Baja California",
                                ent==3  ~ "Baja California Sur",
                                ent==4  ~ "Campeche",
                                ent==5  ~ "Coahuila",
                                ent==6  ~ "Colima",
                                ent==7  ~ "Chiapas",
                                ent==8  ~ "Chihuahua",
                                ent==9  ~ "Ciudad de México",
                                ent==10 ~	"Durango",
                                ent==11 ~	"Guanajuato",
                                ent==12 ~	"Guerrero",
                                ent==13 ~	"Hidalgo",
                                ent==14 ~	"Jalisco",
                                ent==15 ~	"México",
                                ent==16 ~	"Michoacán",
                                ent==17 ~	"Morelos",
                                ent==18 ~	"Nayarit",
                                ent==19 ~	"Nuevo León",
                                ent==20 ~	"Oaxaca",
                                ent==21 ~	"Puebla",
                                ent==22 ~	"Querétaro",
                                ent==23 ~	"Quintana Roo",
                                ent==24 ~	"San Luis Potosí",
                                ent==25 ~	"Sinaloa",
                                ent==26 ~	"Sonora",
                                ent==27 ~	"Tabasco",
                                ent==28 ~	"Tamaulipas",
                                ent==29 ~	"Tlaxcala",
                                ent==30 ~	"Veracruz",
                                ent==31 ~	"Yucatán",
                                ent==32 ~	"Zacatecas"))

# Seleccionamos únicamente las variables de interés
viv2 <- viv %>% 
  select(folioviv, factor, ent, entidad, disp_agua, dotac_agua, excusado, 
         uso_compar, sanit_agua, bano_comp, bano_excus, bano_regad, drenaje)


# DISPONIBILIDAD DE AGUA

# Tomamos en cuenta el Factor de Expansión
viv3 <- viv2 %>%
  group_by(entidad, disp_agua) %>%
  summarise(hogares = sum(factor)) %>%
  ungroup()

# Creamos la tabla para después exportarla
tabla <- viv3 %>%
  pivot_wider(names_from = disp_agua, values_from = hogares)

write_xlsx(tabla, 
           "Mapa.xlsx")


# DRENAJE 

# Tomamos en cuenta el Factor de Expansión
viv4 <- viv2 %>%
  group_by(entidad, drenaje) %>%
  summarise(hogares = sum(factor)) %>%
  ungroup()

# Creamos la tabla para después exportarla
tabla2 <- viv4 %>%
  pivot_wider(names_from = drenaje, values_from = hogares)

write_xlsx(tabla2, 
           "Mapa2.xlsx")
