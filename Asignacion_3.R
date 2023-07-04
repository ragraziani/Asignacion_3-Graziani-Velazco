#Carga de librerías

library(tidyverse)
library(readxl)
library(haven)
library(data.table)
library(sjlabelled)
library(sjPlot)
library(mice)
library(survey)

#Base de datos a trabajar
personas <- read_sav("encovi_personas2017_ds.sav")

# Ver todas las etiquetas
view_df(personas)

#columnas de la tablas personas
cols_personas <- c("ENNUMC", "LIN", "CMHP17", "CMHP18", "CMHP19",
                   "CMHP22", "EMHP28N", "EMHP28A", "EMHP28S",
                   "EMHP32", "TMHP36", "TMHP41", "TMHP43",
                   "TMHP44", "TMHP44BS", "TMHP48", "TMHP45BS",
                   "PMHP60BS", 
                   "PESOPERSONA", "GRPEDAD", "AESTUDIO", "Tciudad_max")

#Nueva tabla para manejar los datos sin modificar la base de datos original
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
  group_by(sexo, grp_edad, nivel_edu) 

resumen <- donantes %>% 
  summarise(n = n())

#Unimos los dataframes

personas_imputar <- personas_imputar %>%
  left_join(resumen, by = c("sexo", "grp_edad", "nivel_edu"))

# Rellena los NAs con muestras aleatorias del grupo respectivo

#personas_imputar <- personas_imputar %>% 
#  mutate(ing_laboral = ifelse(is.na(ing_laboral) | ing_laboral <= 0,
#                           sample(donantes$ing_laboral, size = n(), replace = TRUE),
 #                          ing_laboral))


#donantes_filtrados <- donantes %>%
 # filter(sexo == 1, grp_edad == 5, nivel_edu == 4)

#personas_imputar <- personas_imputar %>%
 # mutate(ing_laboral = ifelse(is.na(ing_laboral) | ing_laboral <= 0 &
  #                            grp_edad == 5,
   #                           sample(donantes_filtrados$ing_laboral, size = n(), 
    #                                 replace = TRUE),
    #                          ing_laboral))


set.seed(123) # Para reproducibilidad

#personas_imputar$ing_laboral_imputado <- personas_imputar %>%
 # left_join(donantes, by = c("sexo", "grp_edad", "nivel_edu")) %>%
  #group_by(sexo, grp_edad, nivel_edu) %>%
  #mutate(ing_laboral_imputado = sample(ing_laboral, 1)) %>%
#  pull(ing_laboral_imputado)


set.seed(123) # Para reproducibilidad

#personas_imputar$ing_laboral_imputado <- personas_imputar %>%
#  left_join(donantes, by = c("sexo", "grp_edad", "nivel_edu"), relationship = "many-to-many") %>%
 # group_by(sexo, grp_edad, nivel_edu) %>%
#  mutate(ing_laboral_imputado = ifelse(is.na(ing_laboral.y), NA, sample(ing_laboral.y, size = n(), replace = TRUE))) %>%
 # pull(ing_laboral_imputado)


set.seed(123) # Para reproducibilidad

#personas_imputar$ing_laboral_imputado <- personas_imputar %>%
#  left_join(donantes, by = c("sexo", "grp_edad", "nivel_edu"), relationship = "many-to-many") %>%
#  group_by(sexo, grp_edad, nivel_edu) %>%
#  mutate(ing_laboral_imputado = ifelse(is.na(ing_laboral.y), NA, sample(ing_laboral.y, size = n(), replace = TRUE))) %>%
 # distinct(across(everything())) %>%
  #pull(ing_laboral_imputado)

# Asegurarse de que las columnas tengan el mismo tipo en ambos dataframes
personas_imputar$sexo <- as.factor(personas_imputar$sexo)
donantes$sexo <- as.factor(donantes$sexo)

personas_imputar$grp_edad <- as.numeric(personas_imputar$grp_edad)
donantes$grp_edad <- as.numeric(donantes$grp_edad)

personas_imputar$nivel_edu <- as.numeric(personas_imputar$nivel_edu)
donantes$nivel_edu <- as.numeric(donantes$nivel_edu)

# Imputación

set.seed(123) # Para reproducibilidad

personas_imputar$ing_laboral_imputado <- NA
#Bucle para imputar ingresos 
for (i in seq_len(nrow(personas_imputar))) {
  condiciones <- personas_imputar[i, c("sexo", "grp_edad", "nivel_edu")]
  donantes_filtrados <- donantes %>%
    filter(sexo == condiciones$sexo & grp_edad == condiciones$grp_edad & nivel_edu == condiciones$nivel_edu)
  if (nrow(donantes_filtrados) > 0) {
    personas_imputar$ing_laboral_imputado[i] <- sample(donantes_filtrados$ing_laboral, 1)
  }
}
#Imputar NAs con otra agrupación
set.seed(123) # Para reproducibilidad

for (i in seq_len(nrow(personas_imputar))) {
  if (is.na(personas_imputar$ing_laboral_imputado[i])) {
    condiciones <- personas_imputar[i, c("sexo", "grp_edad", "sector_eco")]
    donantes_filtrados <- donantes %>%
      filter(sexo == condiciones$sexo & grp_edad == condiciones$grp_edad & sector_eco == condiciones$sector_eco)
    if (nrow(donantes_filtrados) > 0) {
      personas_imputar$ing_laboral_imputado[i] <- sample(donantes_filtrados$ing_laboral, 1)
    }
  }
}

