library(dplyr)
library(tidyverse)
library(stringr)

#remotes::install_github("Nicolas-Schmidt/puy")

library(puy)

data("politicos")
data("legislaturas")

# LIMPIO APELLIDOS COMPUESTOS (PROCESO DE ESTANDARIZACIÓN)

nrow(politicos %>% distinct())

df <- politicos %>% distinct(politico, partido, legislatura)

df_limpio <- df %>%
  # 1. Separar por coma en apellido / nombre
  separate(politico, into = c("apellidos", "nombres"), sep = ",") %>%
  
  # 2. Separar apellidos por espacio
  separate(apellidos, into = c("primer_apellido", "segundo_apellido"), sep = " ", extra = "merge")

df_limpio$nombres <- str_squish(df_limpio$nombres)

df_limpio <- df_limpio %>%
  separate(nombres, into = c("primer_nombre", "segundo_nombre"), sep = " ", fill = "right")

df_limpio$primer_apellido <- toupper(df_limpio$primer_apellido)
df_limpio$segundo_apellido <- toupper(df_limpio$segundo_apellido)
df_limpio$primer_nombre <- toupper(df_limpio$primer_nombre)
df_limpio$segundo_nombre <- toupper(df_limpio$segundo_nombre)

apellidos_compuestos <- c("LEON", "LOS SANTOS", "LA SERNA", "LA SIERRA", "ACHA", "AMEZAGA", "ROSA", "LUZ","SILVEIRA",
                          "CASTRO", "MATTOS", "MATO", "MATTO", "HERRERA", "ARTEGA", "ARZADUM", "BETHENCOURT",
                          "BARROS", "FREITAS", "LA BANDERA", "LA CRUZ", "LA FUENTE", "LA SOTA", "LA SIERRA",
                          "LAS CARRERA", "LAS CASAS", "LAS CARRERAS", "LATORRE", "LOS CAMPOS", "LOS REYES",
                          "MARCO", "PABLO", "MELLO", "MARIA", "LUCIA", "PAULA", "POSADAS", "SALTERAIN", "	VIANA",
                          "TEZANOS", "VIANA", "BUSTOS", "CASTILLO", "NAVA", "LUIS", "PAUL", "MARTIN","ZAS", "RIO",
                          "RIOS", "TORRES WILSON", "SIERRA", "CARLO", "SILVA", "DOVITTIS", "GREGORIO", "HAEDO",
                          "CAMPO", "MARTINI", "VALLE", "CARLOS", "FUENTES", "AMORES", "ARMAS", "MEDINA", "BOISMENU",
                          "LA HOZ", "ALCANTARA", "LIMA", "ARRASCAETA", "ENRIQUEZ", "TORO", "CANDIA", "TRANO", 
                          "COLL", "EACHEN", "ALLISTER", "VICAR", "SOUZA", "COSTA", "ANGELIS", "LA PEA", "LA SOVERA",
                          "LARROBLA", "VEDIA", "BRUM","LA HANTY", "MULA")
  
df_limpio <- df_limpio %>%
  mutate(
    primer_apellido = case_when(
      primer_apellido %in% c("DE", "DEL", "DI", "DA", "MAC") & segundo_apellido %in% apellidos_compuestos ~ 
        str_c(primer_apellido, segundo_apellido, sep = " "),
      TRUE ~ primer_apellido
    ),
    segundo_apellido = case_when(
      primer_apellido %in% c("DE", "DEL", "DI", "DA", "MAC") & segundo_apellido %in% apellidos_compuestos ~ 
        NA_character_,
      TRUE ~ segundo_apellido
    )
  )


# Agregar segundos nombres y segundos apellidos manualmente a los politicos (asi luego)
# pegan mejor con los datos de fechas de nacimiento

df_limpio <- df_limpio %>% arrange((primer_apellido))

write.csv(df_limpio, 'C:/Users/PC/Desktop/pasantia_CP/pasantia_UMAD/df_limpio.csv')


politicos %>% filter(status == 'Suplente' & is.na(fecha_inicio) & is.na(fecha_fin)) 

-------------------------------------------------------------------------------------------------------
# PEGADO DE FECHAS DE NACIMIENTO  
  
# 1.689.972
base_fecha_nac <- base %>% filter(!is.na(fecha_nacimiento))

base_fecha_nac <- base_fecha_nac %>% group_by_all() %>% count()



nrow(base_fecha_nac %>% distinct()) # 1.689.749
nrow(df_corregido) # 5.437

df_1 <- df_corregido %>% filter(!is.na(primer_nombre) & !is.na(segundo_nombre) & !is.na(primer_apellido)
                        & !is.na(segundo_apellido))


df_2 <- base_fecha_nac %>% filter(!is.na(primer_nombre) & !is.na(segundo_nombre) & !is.na(primer_apellido)
                                & !is.na(segundo_apellido))

df_1_2 <- df_1 %>% left_join(df_2, by =c("primer_nombre","primer_apellido", "segundo_apellido"))

nrow(df_1_2 %>% filter(!is.na(fecha_nacimiento))) # solamente 8 pude pegar fecha

---------------------------------------------------------------------------------------------------------------------

df_2 <- df_corregido %>% filter(!is.na(primer_nombre) & !is.na(segundo_nombre) & !is.na(primer_apellido)
                                  & !is.na(segundo_apellido))
