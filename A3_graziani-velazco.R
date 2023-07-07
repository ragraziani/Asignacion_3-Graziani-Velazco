# Miembros del grupo:

# Rafael Graziani y Anselmo Velazco

# Carga de librerías

library(tidyverse)
library(readxl)
library(haven)
library(data.table)
library(sjlabelled)
library(sjPlot)
library(mice)
library(survey)
library(dplyr)

# ----------------------------------------------------------------

# Se abre la base de datos a trabajar:

personas <- read_sav("encovi_personas2017_ds.sav")

# Se muestran todas las etiquetas para visualizar mejor:

view_df(personas)

# ---------------------------------------------------------------- 

# Se escriben las columnas de la tabla "personas" para luego renombrar:

cols_personas <- c("ENNUMC", "LIN", "CMHP17", "CMHP18", "CMHP19",
                   "CMHP22", "EMHP28N", "EMHP28A", "EMHP28S",
                   "EMHP32", "TMHP36", "TMHP41", "TMHP43",
                   "TMHP44", "TMHP44BS", "TMHP48", "TMHP45BS",
                   "PMHP60BS", 
                   "PESOPERSONA", "GRPEDAD", "AESTUDIO", "Tciudad_max")

# Se crea nueva tabla para manejar los datos sin modificar la base original:

personas_imputar <- personas %>%
  select(all_of(cols_personas))

# Se arreglan los nombres de las columnas:

new_names_pers <- c("id_hogar", "id_per", "parentesco", "edad", "sexo", 
                    "sit_conyu", "nivel_edu", "edu_ano_aprobado",
                    "edu_sem_aprobado", "tipo_edu", "sit_econo", 
                    "sector_eco", "cat_ocupa","trab_remun", "ing_laboral", 
                    "horas_trab", "ing_otro","ing_pension",
                    "pesop", "grp_edad", "anos_edu", "tipo_ciudad")

# Se renombran las columnas con los nombres nuevos:

personas_imputar <- personas %>%
  setnames(old = colnames(.),
           new = new_names_pers) %>%
  
  # Se convierten los identificadores a caracteres:
  
  mutate(id_hogar = str_pad(id_hogar,
                            width = 4, "left", "0"),
         id_per = str_pad(id_per, 
                          width = 2, "left", "0"))

  # Se realiza lo mismo con la base de datos original de personas:

  personas <- personas %>% 
    mutate(id_hogar = str_pad(id_hogar,
                              width = 4, "left", "0"),
           id_per = str_pad(id_per,
                            width = 2, "left", "0"))

# ----------------------------------------------------------------

# Se convierten en NAs aquellas respuestas 98 y 99.

personas_imputar[personas_imputar == 98 | personas_imputar == 99] <- NA
personas[personas == 98 | personas == 99] <- NA

# Se filtran a las personas que cumplan con las condiciones para ser imputadas:

# Condiciones: 1. Declara estar trabajando de forma remunerada.
#              2. No declara ingresos recientes, pero tiene trabajo.

personas_imputar <- personas_imputar %>% 
  filter(sit_econo %in% c(1,2),
         trab_remun == 1,
         is.na(ing_laboral) | ing_laboral <= 0)

# Se filtran los donantes que cumplan con las condiciones para serlo:

# Condición: Declara tener trabajo, y manifestó generación de ingresos.

donantes <- personas %>%
  filter(sit_econo %in% c(1,2),
         trab_remun == 1,
         !is.na(ing_laboral))


# Se evalúa si vale la pena ampliar los grupos de edad, revisando sus
# promedios de ingreso laboral.

resumen_grp_edad <- donantes %>%
  group_by(grp_edad) %>%
  summarise(mean_ing = mean(ing_laboral),
            w_mean = weighted.mean(ing_laboral,
                                   w = pesop, 
                                   na.rm = T),
            n = n())

# Tras revisar los promedios, notamos que vale la pena ampliar los
# grupos de edad, pues existe similitud entre varios. De esta forma
# se podrá trabajar con grupos más representativos, garantizando que
# cada recipiente tenga suficientes potenciales donantes.

# -------------------------------------------------------------------

# Se cambian las agrupaciones de grupos de edad a todos los dataframes
# que contenían los grupos de edad previos. Estos nuevos grupos de edad
# van de 15 en 15 años.

donantes <- donantes %>%
  mutate(grp_edad = case_when(
    grp_edad %in% c(2,3,4) ~ 1,
    grp_edad %in% c(5,6,7) ~ 2,
    grp_edad %in% c(8,9,10) ~ 3,
    grp_edad %in% c(11,12,13) ~ 4,
    grp_edad %in% c(14,15) ~ 5,
  ))

  # Se forman los grupos de donantes, utilizando los siguientes criterios:
  
  # 1. Sexo, ya que es un criterio obligatorio.
  # 2. Grupos de edad, ya que es un criterio obligatorio.
  # 3. Nivel de educación, se seleccionó este criterio, debido a la creencia
  # que se tiene de que aquellas personas con mejor nivel de estudios alcanzan 
  # mejor capacidad de generación de ingresos, lo cual nos permite diferenciar
  # bien los grupos de donantes.

    grupos_donantes <- donantes %>% 
      group_by(sexo, grp_edad, nivel_edu) %>% 
      summarise(n=n()) %>% 
      ungroup()

# Continuamos cambiando los grupos de edad a los dataframes:    

personas_imputar <- personas_imputar %>%
  mutate(grp_edad = case_when(
    grp_edad %in% c(2,3,4) ~ 1,
    grp_edad %in% c(5,6,7) ~ 2,
    grp_edad %in% c(8,9,10) ~ 3,
    grp_edad %in% c(11,12,13) ~ 4,
    grp_edad %in% c(14,15) ~ 5,
  ))

personas <- personas %>%
  mutate(grp_edad = case_when(
    grp_edad %in% c(0,1) ~ 0,
    grp_edad %in% c(2,3,4) ~ 1,
    grp_edad %in% c(5,6,7) ~ 2,
    grp_edad %in% c(8,9,10) ~ 3,
    grp_edad %in% c(11,12,13) ~ 4,
    grp_edad %in% c(14,15) ~ 5,
  ))

# **NOTA**: Al modificar el dataframe de personas se agrega el "c(0,1) ~ 0", y 
# en los demás dataframes no, porque no hay personas para imputar que 
# pertenezcan originalmente al grupo de edad 0 o 1.

# -------------------------------------------------------------------

# Se unen los dataframes de personas a imputar y de grupos de donantes.

personas_imputar <- personas_imputar %>%
  left_join(grupos_donantes, by = c("sexo",
                                    "grp_edad", 
                                    "nivel_edu")) %>% 
  rename(n_imp = n)

# -------------------------------------------------------------------

# Asegurarse de que las columnas tengan el mismo tipo en ambos dataframes:

personas_imputar$sexo <- as.factor(personas_imputar$sexo)
donantes$sexo <- as.factor(donantes$sexo)

personas_imputar$grp_edad <- as.numeric(personas_imputar$grp_edad)
donantes$grp_edad <- as.numeric(donantes$grp_edad)

personas_imputar$nivel_edu <- as.numeric(personas_imputar$nivel_edu)
donantes$nivel_edu <- as.numeric(donantes$nivel_edu)

# Se procede a realizar la imputación de datos a aquellas personas calificadas,
# siguiendo la metodología Random Hot Deck, y se tomó en cuenta el peso muestral
# para aquellas personas que tenían 5 o menos potenciales donantes:

set.seed(123) # Se crea la semilla para reproducibilidad.

personas_imputar$ing_laboral_imp <- personas_imputar %>%
  rowwise() %>%
  mutate(ing_laboral_imp = {
    donantes_filtrados <- donantes %>%
      filter(sexo == .env$sexo,
             grp_edad == .env$grp_edad,
             (nivel_edu == .env$nivel_edu) | (is.na(nivel_edu) & is.na(.env$nivel_edu)))
    if (nrow(donantes_filtrados) > 0) {
      ifelse(
        nrow(donantes_filtrados) > 5,
        donantes_filtrados %>%
          sample_n(1) %>%
          pull(ing_laboral),
        weighted.mean(donantes_filtrados$ing_laboral,
                      w = donantes_filtrados$pesop)
      )
    } else {
      NA
    }
  }) %>%
  pull(ing_laboral_imp)


# Se procede a realizar la imputación de los datos a aquellas personas que 
# quedaron con NAs, debido a que no coincidieron con ningún grupo de donantes.

# Para hacer esta imputación se realizó un proceso de revisión de los donantes
# para que los recipientes que no pudieron recibir datos tras el proceso de RHD,
# recibieran datos de algún donante, igualmente aleatorio, que fuera lo más 
# similar posible al recipiente:

set.seed(123)

personas_imputar <- personas_imputar %>%
  rowwise() %>%
  mutate(ing_laboral_imp = if (is.na(n_imp)) {
    donantes_filtrados <- donantes %>%
      filter(
        sexo == .env$sexo,
        grp_edad == .env$grp_edad,
        if (is.na(.env$nivel_edu)) {
          nivel_edu %in% c(1,2,3)
        } else {
          nivel_edu %in% (.env$nivel_edu + c(-1, 0, 1))
        }
      )
    if (nrow(donantes_filtrados) > 0) {
      donantes_filtrados %>%
        sample_n(1) %>%
        pull(ing_laboral)
    } else {
      NA
    }
  } else {
    ing_laboral_imp
  })

# Se calcula el porcentaje de imputación exitoso:

# 1. ¿Cúantas imputaciones se lograron por el método de Random Hot Deck?:

sum(!is.na(personas_imputar$n_imp) & 
      ifelse(is.na(personas_imputar$n_imp),
                                            0, personas_imputar$n_imp) > 5)

# 2. Porcentaje exitoso de imputaciones mediante el Random Hot Deck:

(sum(!is.na(personas_imputar$n_imp) & 
       ifelse(is.na(personas_imputar$n_imp), 
              0, personas_imputar$n_imp) > 5))/1236 * 100

# 11 observaciones se imputaron mediante un promedio ponderado, 
# y las dos restantes fueron imputadas mediante el proceso de revisión explicado
# anteriormente: "Para hacer esta imputación se realizó un proceso de revisión de los donantes
# para que los recipientes que no pudieron recibir datos tras el proceso de RHD,
# recibieran datos de algún donante, igualmente aleatorio, que fuera lo más 
# similar posible al recipiente".

# ----------------------------------------------------------------------

# Se unen las personas_imputar a la base de datos original de personas


personas <- personas %>% 
  left_join(personas_imputar[, c("id_hogar", "id_per", "ing_laboral_imp", "n_imp")],
            by = c("id_hogar", "id_per"))


# Se Verifica que en ing_laboral_imp existan 1236 observaciones con los 
# ingresos imputados.

sum(!is.na(personas$ing_laboral_imp))