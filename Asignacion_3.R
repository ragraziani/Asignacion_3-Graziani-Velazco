# Carga de librerías

library(tidyverse)
library(readxl)
library(haven)
library(data.table)
library(sjlabelled)
library(sjPlot)
library(mice)
library(survey)

# Base de datos a trabajar
personas <- read_sav("encovi_personas2017_ds.sav")

# Ver todas las etiquetas
view_df(personas)

# Columnas de la tablas personas
cols_personas <- c("ENNUMC", "LIN", "CMHP17", "CMHP18", "CMHP19",
                   "CMHP22", "EMHP28N", "EMHP28A", "EMHP28S",
                   "EMHP32", "TMHP36", "TMHP41", "TMHP43",
                   "TMHP44", "TMHP44BS", "TMHP48", "TMHP45BS",
                   "PMHP60BS", 
                   "PESOPERSONA", "GRPEDAD", "AESTUDIO", "Tciudad_max")

# Nueva tabla para manejar los datos sin modificar la base de datos original
personas_imputar <- personas %>%
  select(all_of(cols_personas))

#Arreglando los nombres de las columnas


new_names_pers <- c("id_hogar", "id_per", "parentesco", "edad", "sexo", 
                    "sit_conyu", "nivel_edu", "edu_ano_aprobado", "edu_sem_aprobado",
                    "tipo_edu", "sit_econo", "sector_eco", "cat_ocupa",
                    "trab_remun", "ing_laboral", "horas_trab", "ing_otro",
                    "ing_pension",
                    "pesop", "grp_edad", "anos_edu", "tipo_ciudad")

# Renombrar
personas_imputar <- personas %>%
  setnames(old = colnames(.), new = new_names_pers) %>%
  
  # Convierte los identificadores a caracteres
  mutate(id_hogar = str_pad(id_hogar, width = 4, "left", "0"),
         id_per = str_pad(id_per, width = 2, "left", "0"))


# RECORDATORIO: NAs 98 y 99
personas_imputar[personas_imputar == 98 | personas_imputar == 99] <- NA
personas[personas == 98 | personas == 99] <- NA

personas_imputar <- personas_imputar %>% 
  filter(sit_econo %in% c(1,2), trab_remun == 1, is.na(ing_laboral) | ing_laboral <= 0)

#Agrupando donantes

donantes <- personas %>%
  filter(sit_econo %in% c(1,2), trab_remun == 1, !is.na(ing_laboral)) %>% 
  select(sexo, grp_edad, nivel_edu,  ing_laboral) %>% 
  group_by(sexo, grp_edad, nivel_edu) %>% 
  summarise(n = n())


