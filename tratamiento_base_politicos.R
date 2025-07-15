library(dplyr)
library(tidyverse)
library(stringr)
library(fastLink)
#remotes::install_github("Nicolas-Schmidt/puy")
library(puy)

data("politicos")
data("legislaturas")

# CORTE TEMPORAL: PRIMERA LEGISLATURA QUE COMIENZA EN EL SIGLO XX: LEGISLATURA NÚMERO 21

politicos <- politicos %>% filter(legislatura >= 21)

# LIMPIO APELLIDOS COMPUESTOS (PROCESO DE ESTANDARIZACIÓN)

nrow(politicos %>% distinct())

df <- politicos %>% distinct(politico, partido, legislatura)

df_limpio <- df %>%
  # 1. Separar por coma en apellido / nombre
  separate(politico, into = c("apellidos", "nombres"), sep = ",") %>%
  
  # 2. Separar apellidos por espacio
  separate(apellidos, into = c("primer_apellido", "segundo_apellido"), sep = " ", extra = "merge")

df_limpio$nombres <- str_squish(df_limpio$nombres)
df_limpio$partido <- str_squish(df_limpio$partido)
df_limpio$legislatura <- str_squish(df_limpio$legislatura)

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


df_limpio <- df_limpio %>% arrange((primer_apellido))

# ESTANDARIZACION DE PARTIDOS

df_limpio$partido <- gsub('Partido Frente Amplio', 'Frente Amplio', df_limpio$partido)
df_limpio$partido <- gsub('Frente Ampilio', 'Frente Amplio', df_limpio$partido)
df_limpio$partido <- gsub('Partido Ecologista Radical Intransigene', 'Partido Ecologista Radical Intransigente', df_limpio$partido)
df_limpio$partido <- gsub('Partido Frente Izquierda de Liberacion', 'Frente Izquierda de Liberacion', df_limpio$partido)

partidos_unicos <- df_limpio %>% group_by(partido) %>% count()

#write.csv(df_limpio, 'C:/Users/PC/Desktop/pasantia_CP/pasantia_UMAD/df_limpio.csv')

# Agregar segundos nombres y segundos apellidos manualmente a los politicos (asi luego)
# pegan mejor con los datos de fechas de nacimiento


-------------------------------------------------------------------------------------------------------

# GENERACION DEL ID PARA CADA POLITICO

# Comienzo con opcion robusta, para ir deduplicando

df_1 <- df_limpio %>%
  group_by(primer_apellido, segundo_apellido, primer_nombre, segundo_nombre) %>%
  mutate(id = cur_group_id())

-------------------------------------------------------------------------------------------------------

# MATCHING PROBABILISTICO (DEDUPLICACION)

library(fastLink)
library(RecordLinkage)

df_limpio <- df_limpio %>%
  mutate(across(everything(), ~ifelse(is.na(.), "", .)))


pairs <- compare.dedup(
  df_limpio,
  blockfld = list(c(1, 3,5)),  # Bloquear por primer_nombre (col 1) y primer_apellido (col 3)
  strcmp = c(TRUE, TRUE, TRUE, TRUE),  # Usar comparación de strings para todos los campos
  exclude = c("legislatura")  # Excluir columnas no relevantes para el matching
)

# Calcular pesos (probabilidades)
pairs <- epiWeights(pairs)

# Obtener pares coincidentes
results <- epiClassify(pairs, threshold.upper = 0.85)


# Paso 5: Extraer y asignar clusters CORRECTAMENTE
# Necesitamos usar la función getPairs() para obtener los matches
matched_pairs <- getPairs(results, single.rows = TRUE)

# Crear un mapeo de IDs
cluster_map <- data.frame(
  id = seq_len(nrow(df_limpio)),
  cluster_id = seq_len(nrow(df_limpio))  # Inicialmente cada registro es su propio cluster
)

# Actualizar clusters basado en los matches
for(i in seq_len(nrow(matched_pairs))) {
  id1 <- matched_pairs$id1[i]
  id2 <- matched_pairs$id2[i]
  current_cluster <- min(cluster_map$cluster_id[id1], cluster_map$cluster_id[id2])
  cluster_map$cluster_id[cluster_map$id %in% c(id1, id2)] <- current_cluster
}

# Paso 6: Asignar los clusters al dataframe original
df_con_ids <- df_limpio %>%
  mutate(cluster_id = cluster_map$cluster_id)


# Busqueda de falsos negativos

falsos_negativos <- df_con_ids %>% group_by(primer_nombre, primer_apellido) %>%
  filter(n_distinct(cluster_id) > 1)

df_con_ids <- df_con_ids %>% group_by(primer_nombre, primer_apellido) %>%
  filter(n_distinct(cluster_id) <= 1)

falsos_negativos <- falsos_negativos %>%
  group_by(primer_nombre, primer_apellido) %>%
  mutate(id_unificado = min(cluster_id, na.rm = TRUE)) %>%
  ungroup()

falsos_negativos_1 <- falsos_negativos %>% filter(partido %in%c('Frente Amplio', 'Partido Socialista', 'Partido Comunista
                                          del Uruguay', 'Frente Izquierda de Liberacion','Nuevo Espacio', 'Coalicion Liberal',
                                          'Union Popular'))

falsos_negativos_1 %>% select(-cluster_id) %>% rename(cluster_id=id_unificado)


write.csv(falsos_negativos, 'C:/Users/PC/Desktop/pasantia_CP/pasantia_UMAD/falsos_negativos.csv', row.names = FALSE)

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
