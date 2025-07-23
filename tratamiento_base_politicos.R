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
                          "LARROBLA", "VEDIA", "BRUM","LA HANTY", "MULA", 'ARTEAGA')
  
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


####################################################################################################

# GENERACION DEL ID PARA CADA POLITICO

# Comienzo con opcion robusta, para ir deduplicando

#df_1 <- df_limpio %>%
#  group_by(primer_apellido, segundo_apellido, primer_nombre, segundo_nombre) %>%
#  mutate(id = cur_group_id())

###################################################################################################
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

# ESPECTRO IZQUIERDA
falsos_negativos_1 <- falsos_negativos %>% filter(partido %in%c('Frente Amplio', 'Partido Socialista', 'Partido Comunista
                                          del Uruguay', 'Frente Izquierda de Liberacion','Nuevo Espacio', 'Coalicion Liberal',
                                          'Union Popular'))

falsos_negativos_1 %>% select(-cluster_id) %>% rename(cluster_id=id_unificado)


# ESPECTRO NO IZQUIERDA
# parece razonable quedarme con el id unificado, borrando el cluster id

falsos_negativos_2 <- falsos_negativos %>% filter(!partido %in%c('Frente Amplio', 'Partido Socialista', 'Partido Comunista
                                          del Uruguay', 'Frente Izquierda de Liberacion','Nuevo Espacio', 'Coalicion Liberal',
                                                                'Union Popular'))

## PARTIDO NACIONAL/ PARTIDO NACIONAL INDEPENDIENTE

falsos_negativos_2_1 <- falsos_negativos_2 %>% filter(partido %in%c('Partido Nacional', 'Partido Nacional Independiente',
                                                                    'Partido General Aparicio Saravia', 'Partido Blanco'))

# parece razonable quedarme con el id unificado, borrando el cluster id
falsos_negativos_2_1 %>% select(-cluster_id) %>% rename(cluster_id=id_unificado)

## PARTIDO COLORADO

falsos_negativos_2_2 <- falsos_negativos_2 %>% filter(!partido %in%c('Partido Nacional', 'Partido Nacional Independiente',
                                                                     'Partido General Aparicio Saravia', 'Partido Blanco'))

falsos_negativos_1


# chequeo a mano izquierda
#write.csv(falsos_negativos_1, 'C:/Users/PC/Desktop/pasantia_CP/pasantia_UMAD/falsos_negativos_1.csv', row.names = FALSE)

# chequeo a mano derecha (partido nacional)
#write.csv(falsos_negativos_2_1, 'C:/Users/PC/Desktop/pasantia_CP/pasantia_UMAD/falsos_negativos_2_1.csv', row.names = FALSE)

# chequeo a mano derecha (partido nacional)
#write.csv(falsos_negativos_2_2, 'C:/Users/PC/Desktop/pasantia_CP/pasantia_UMAD/falsos_negativos_2_2.csv', row.names = FALSE)


---------------------------------------------------- # PEGADO DE BASES CORREGIDAS MANUALMENTE ------

# HAGO LOS CAMBIOS CON CODIGO (DADO QUE POR ALGUNA RAZON NO SE GUARDAN)

## FALSOS NEGATIVOS 1

falsos_negativos_1[falsos_negativos_1$primer_nombre == 'JOSE' &
                     falsos_negativos_1$segundo_apellido == 'LANGORTA'
                     & falsos_negativos_1$primer_apellido == 'LUIS', "primer_apellido"] <- 'LANGORTA'

falsos_negativos_1[falsos_negativos_1$primer_nombre == 'JOSE' &
                   falsos_negativos_1$primer_apellido == 'LANGORTA', "segundo_nombre"] <- 'LUIS'

falsos_negativos_1[falsos_negativos_1$primer_nombre == 'JUAN'
                   & falsos_negativos_1$primer_apellido == 'JOSE', "primer_apellido"] <- 'FIGUEROA'

falsos_negativos_1[falsos_negativos_1$primer_nombre == 'JUAN'
                   & falsos_negativos_1$primer_apellido == 'FIGUEROA', "segundo_nombre"] <- 'JOSE'

falsos_negativos_1[falsos_negativos_1$primer_nombre == 'JUAN'
                   & falsos_negativos_1$primer_apellido == 'FIGUEROA', "segundo_apellido"] <- ''


## FALSOS NEGATIVOS 2_1

falsos_negativos_2_1[falsos_negativos_2_1$primer_nombre == 'WASHINGTON' &
                       falsos_negativos_2_1$primer_apellido == 'BELTRAN'
                     & falsos_negativos_2_1$legislatura %in%c(25,26,27), "segundo_apellido"] <- 'MULLIN'

falsos_negativos_2_1[falsos_negativos_2_1$primer_nombre == 'WASHINGTON' &
                       falsos_negativos_2_1$primer_apellido == 'BELTRAN'
                     & falsos_negativos_2_1$legislatura %in%c(25,26,27), "id_unificado"] <- 965

falsos_negativos_2_1[falsos_negativos_2_1$primer_nombre == 'JUAN' &
                       falsos_negativos_2_1$segundo_nombre == 'JOSE'
                     & falsos_negativos_2_1$primer_apellido == 'LOPEZ', "id_unificado"] <- 8072

falsos_negativos_2_1[falsos_negativos_2_1$primer_nombre == 'JUAN' &
                       falsos_negativos_2_1$primer_apellido == 'LOPEZ' &
                      falsos_negativos_2_1$legislatura == 37, "id_unificado"] <- 8073

falsos_negativos_2_1[falsos_negativos_2_1$primer_nombre == 'ANGEL' &
                       falsos_negativos_2_1$primer_apellido == 'NUÑEZ'
                     & falsos_negativos_2_1$segundo_apellido == 'COLL', "id_unificado"] <- 8077

falsos_negativos_2_1[falsos_negativos_2_1$primer_nombre == 'JUAN' &
                       falsos_negativos_2_1$segundo_nombre == 'MARTIN'
                     & falsos_negativos_2_1$primer_apellido == 'RODRIGUEZ', "id_unificado"] <- 8078

falsos_negativos_2_1[falsos_negativos_2_1$primer_nombre == 'JUAN' &
                       falsos_negativos_2_1$segundo_nombre == 'C.'
                     & falsos_negativos_2_1$primer_apellido == 'RODRIGUEZ', "id_unificado"] <- 8079

falsos_negativos_2_1[falsos_negativos_2_1$primer_nombre == 'HUGO' &
                       falsos_negativos_2_1$primer_apellido == 'RODRIGUEZ'
                     & falsos_negativos_2_1$legislatura == 48, "id_unificado"] <- 8080

falsos_negativos_2_1[falsos_negativos_2_1$primer_nombre == 'HUGO' &
                       falsos_negativos_2_1$primer_apellido == 'RODRIGUEZ'
                     & falsos_negativos_2_1$legislatura == 47, "id_unificado"] <- 8080

falsos_negativos_2_1[falsos_negativos_2_1$primer_nombre == 'PEDRO' &
                       falsos_negativos_2_1$primer_apellido == 'SUAREZ'
                     & falsos_negativos_2_1$legislatura == 47, "id_unificado"] <- 8081

falsos_negativos_2_1[falsos_negativos_2_1$primer_nombre == 'JUAN' &
                       falsos_negativos_2_1$primer_apellido == 'SUAREZ'
                     & falsos_negativos_2_1$legislatura == 48, "id_unificado"] <- 8082

falsos_negativos_2_1[falsos_negativos_2_1$primer_nombre == 'RUBEN' &
                       falsos_negativos_2_1$primer_apellido == 'MARTINEZ'
                     & falsos_negativos_2_1$legislatura == 48, "id_unificado"] <- 8083

falsos_negativos_2_1[falsos_negativos_2_1$primer_nombre == 'JAVIER' &
                       falsos_negativos_2_1$primer_apellido == 'BARRIOS'
                     & falsos_negativos_2_1$segundo_apellido == 'ANZA', "id_unificado"] <- 8891

falsos_negativos_2_1[falsos_negativos_2_1$primer_nombre == 'JOSE' &
                       falsos_negativos_2_1$primer_apellido == 'LUIS'
                     & falsos_negativos_2_1$segundo_apellido == 'REDES', "primer_apellido"] <- 'REDES'

falsos_negativos_2_1[falsos_negativos_2_1$primer_nombre == 'JOSE' &
                       falsos_negativos_2_1$primer_apellido == 'REDES',
                       "segundo_nombre"] <- 'LUIS'

falsos_negativos_2_1[falsos_negativos_2_1$primer_nombre == 'JOSE' &
                       falsos_negativos_2_1$primer_apellido == 'REDES',
                     "id_unificado"] <- 8074

falsos_negativos_2_1[falsos_negativos_2_1$primer_nombre == 'JOSE' &
                       falsos_negativos_2_1$primer_apellido == 'REDES',
                     "segundo_apellido"] <- ''

falsos_negativos_2_1[falsos_negativos_2_1$primer_nombre == 'JOSE' &
                       falsos_negativos_2_1$primer_apellido == 'LUIS'
                     & falsos_negativos_2_1$segundo_apellido == 'CUELLO', "primer_apellido"] <- 'CUELLO'

falsos_negativos_2_1[falsos_negativos_2_1$primer_nombre == 'JOSE' &
                       falsos_negativos_2_1$primer_apellido == 'CUELLO',
                     "segundo_nombre"] <- 'LUIS'

falsos_negativos_2_1[falsos_negativos_2_1$primer_nombre == 'JOSE' &
                       falsos_negativos_2_1$primer_apellido == 'CUELLO',
                     "id_unificado"] <- 8075

falsos_negativos_2_1[falsos_negativos_2_1$primer_nombre == 'JOSE' &
                       falsos_negativos_2_1$primer_apellido == 'CUELLO',
                     "segundo_apellido"] <- ''

falsos_negativos_2_1[falsos_negativos_2_1$primer_nombre == 'JOSE' &
                       falsos_negativos_2_1$primer_apellido == 'LUIS'
                     & falsos_negativos_2_1$segundo_apellido == 'NUÑEZ', "primer_apellido"] <- 'NUÑEZ'

falsos_negativos_2_1[falsos_negativos_2_1$primer_nombre == 'JOSE' &
                       falsos_negativos_2_1$primer_apellido == 'NUÑEZ',
                     "segundo_nombre"] <- 'LUIS'

falsos_negativos_2_1[falsos_negativos_2_1$primer_nombre == 'JOSE' &
                       falsos_negativos_2_1$primer_apellido == 'NUÑEZ',
                     "id_unificado"] <- 8076

falsos_negativos_2_1[falsos_negativos_2_1$primer_nombre == 'JOSE' &
                       falsos_negativos_2_1$primer_apellido == 'NUÑEZ',
                     "segundo_apellido"] <- ''

falsos_negativos_2_1[falsos_negativos_2_1$primer_nombre == 'HORACIO' &
                       falsos_negativos_2_1$primer_apellido == 'ABADIE' &
                     falsos_negativos_2_1$segundo_apellido == 'PEREZ',
                     "id_unificado"] <- 8893

falsos_negativos_2_1[falsos_negativos_2_1$primer_nombre == 'HORACIO' &
                       falsos_negativos_2_1$primer_apellido == 'ABADIE' &
                       falsos_negativos_2_1$legislatura == 43,
                     "id_unificado"] <- 8893

## FALSOS NEGATIVOS 2_2

falsos_negativos_2_2[falsos_negativos_2_2$primer_nombre == 'JOSE' &
                       falsos_negativos_2_2$primer_apellido == 'AROCENA'
                     & falsos_negativos_2_2$partido == 'Partido Independiente', "id_unificado"] <- 557

falsos_negativos_2_2[falsos_negativos_2_2$primer_nombre == 'HUGO' &
                       falsos_negativos_2_2$primer_apellido == 'FERNANDEZ'
                     & falsos_negativos_2_2$segundo_apellido == 'BRITOS', "id_unificado"] <- 8084

falsos_negativos_2_2[falsos_negativos_2_2$primer_nombre == 'JOSE' &
                       falsos_negativos_2_2$primer_apellido == 'LOPEZ'
                     & falsos_negativos_2_2$segundo_apellido == 'RAMOS', "id_unificado"] <- 8085

falsos_negativos_2_2[falsos_negativos_2_2$primer_nombre == 'JOSE' &
                       falsos_negativos_2_2$primer_apellido == 'LOPEZ'
                     & falsos_negativos_2_2$segundo_apellido == 'LAPHITZ', "id_unificado"] <- 8086

falsos_negativos_2_2[falsos_negativos_2_2$primer_nombre == 'JOSE' &
                       falsos_negativos_2_2$segundo_nombre == 'PEDRO'
                     & falsos_negativos_2_2$primer_apellido == 'MASSERA', "id_unificado"] <- 8087

falsos_negativos_2_2[falsos_negativos_2_2$primer_nombre == 'ANTONIO' &
                       falsos_negativos_2_2$primer_apellido == 'RODRIGUEZ'
                     & falsos_negativos_2_2$segundo_apellido == 'CORREA', "id_unificado"] <- 8088

falsos_negativos_2_2[falsos_negativos_2_2$primer_nombre == 'JORGE' &
                       falsos_negativos_2_2$primer_apellido == 'SILVA'
                     & falsos_negativos_2_2$segundo_apellido == 'ROTTA', "id_unificado"] <- 8889

falsos_negativos_2_2[falsos_negativos_2_2$primer_nombre == 'JORGE' &
                       falsos_negativos_2_2$primer_apellido == 'SILVA'
                     & falsos_negativos_2_2$partido == 'Partido Democratico Unido',"id_unificado"] <- 8890

falsos_negativos_2_2[falsos_negativos_2_2$primer_nombre == 'ENRIQUE' &
                       falsos_negativos_2_2$primer_apellido == 'RODRIGUEZ'
                     & falsos_negativos_2_2$partido == 'Partido Comunista del Uruguay',"id_unificado"] <- 8891

falsos_negativos_2_2[falsos_negativos_2_2$primer_nombre == 'HORACIO' &
                       falsos_negativos_2_2$primer_apellido == 'ABADIE' &
                     falsos_negativos_2_2$segundo_apellido == 'SANTOS' 
                     & falsos_negativos_2_2$legislatura == 40,"id_unificado"] <- 8892

# ------------------------------------ # union de los DF con los ids corregidos #  ----------------------------


df_con_ids_1 <- rbind(falsos_negativos_1, falsos_negativos_2_1, falsos_negativos_2_2)

df_con_ids_1 <- df_con_ids_1 %>% arrange(primer_apellido)

df_con_ids_1[df_con_ids_1$primer_nombre == 'CARLOS' &
               df_con_ids_1$primer_apellido == 'ABDALA' &
               df_con_ids_1$partido == 'Partido Nacional' 
                     ,"id_unificado"] <- 13

df_con_ids_1[df_con_ids_1$primer_nombre == 'CARLOS' &
               df_con_ids_1$primer_apellido == 'ABDALA' &
               df_con_ids_1$partido == 'Partido Colorado' 
                     ,"id_unificado"] <- 18

df_con_ids_1[df_con_ids_1$primer_nombre == 'CARLOS' &
               df_con_ids_1$primer_apellido == 'ABDALA' &
               df_con_ids_1$partido == 'Partido Colorado' 
                     ,"id_unificado"] <- 18

df_con_ids_1[df_con_ids_1$primer_nombre == 'EDUARDO' &
               df_con_ids_1$primer_apellido == 'ACEVEDO' &
               df_con_ids_1$segundo_apellido == 'DIAZ' &
               df_con_ids_1$partido == 'Partido Nacional' 
             ,"id_unificado"] <- 62

df_con_ids_1[df_con_ids_1$primer_nombre == 'JUAN' &
               df_con_ids_1$primer_apellido == 'ALVAREZ' &
               df_con_ids_1$segundo_nombre == 'C.' &
               df_con_ids_1$legislatura == 39 
             ,"id_unificado"] <- 265

df_con_ids_1[df_con_ids_1$primer_nombre == 'JOSE' &
               df_con_ids_1$primer_apellido == 'ARIAS' &
               df_con_ids_1$partido == 'Partido Nacional' &
               df_con_ids_1$legislatura == 40 
             ,"id_unificado"] <- 522

df_con_ids_1[df_con_ids_1$primer_nombre == 'JOSE' &
               df_con_ids_1$primer_apellido == 'ARRILLAGA' &
               df_con_ids_1$segundo_nombre == 'SERVANDO'
             ,"id_unificado"] <- 608

df_con_ids_1[df_con_ids_1$primer_nombre == 'JUAN' &
             df_con_ids_1$primer_apellido == 'BENTANCOUR' &
             df_con_ids_1$segundo_nombre == 'JOSE' &
             df_con_ids_1$legislatura == 45 
             ,"id_unificado"] <- 8894

df_con_ids_1[df_con_ids_1$primer_nombre == 'JUAN' &
               df_con_ids_1$primer_apellido == 'BENTANCOUR' &
               df_con_ids_1$segundo_apellido == 'FERREIRA' &
               df_con_ids_1$legislatura == 42 
             ,"id_unificado"] <- 8895

df_con_ids_1[df_con_ids_1$primer_nombre == 'LUIS' &
               df_con_ids_1$primer_apellido == 'BIANCHI' &
               df_con_ids_1$segundo_nombre == 'A.' &
               df_con_ids_1$legislatura == 48
             ,"id_unificado"] <- 1148

df_con_ids_1[df_con_ids_1$primer_nombre == 'RAUL' &
               df_con_ids_1$primer_apellido == 'BOGLIACCINI' &
               df_con_ids_1$segundo_nombre == 'F.' &
               df_con_ids_1$legislatura == 29
             ,"id_unificado"] <- 1203

df_con_ids_1[df_con_ids_1$primer_nombre == 'SERGIO' &
               df_con_ids_1$primer_apellido == 'BOTANA',
             "partido"] <- 'Partido Nacional'

df_con_ids_1[df_con_ids_1$primer_nombre == 'CARMELO' &
               df_con_ids_1$primer_apellido == 'CABRERA' &
               df_con_ids_1$legislatura == 41,
              "id_unificado"] <- 1462

df_con_ids_1[df_con_ids_1$primer_nombre == 'JOSE' &
               df_con_ids_1$primer_apellido == 'CARDOSO' &
               df_con_ids_1$segundo_nombre == 'CARLOS' &
               df_con_ids_1$legislatura %in%c(47,48),
               "id_unificado"] <- 1663

df_con_ids_1[df_con_ids_1$primer_nombre == 'HUMBERTO' &
               df_con_ids_1$primer_apellido == 'CASTELLI' &
               df_con_ids_1$legislatura %in%c(31,29),
               "id_unificado"] <- 1776

df_con_ids_1[df_con_ids_1$primer_nombre == 'JUAN' &
               df_con_ids_1$primer_apellido == 'CASTRO' &
               df_con_ids_1$partido == 'Frente Amplio' &
               df_con_ids_1$legislatura %in%c(48),
             "id_unificado"] <- 1806

df_con_ids_1[df_con_ids_1$primer_nombre == 'JOSE' &
               df_con_ids_1$primer_apellido == 'DIAZ' &
               df_con_ids_1$segundo_nombre == 'RAMON' &
               df_con_ids_1$legislatura %in%c(38),
             "id_unificado"] <- 2466

df_con_ids_1[df_con_ids_1$primer_nombre == 'JOSE' &
               df_con_ids_1$primer_apellido == 'DIAZ' &
               df_con_ids_1$segundo_apellido == '(H)',
             "id_unificado"] <- 2493

df_con_ids_1[df_con_ids_1$primer_nombre == 'RAMON' &
               df_con_ids_1$primer_apellido == 'DIAZ' &
               df_con_ids_1$partido == 'Partido Liberal',
             "id_unificado"] <- 2489

df_con_ids_1[df_con_ids_1$primer_nombre == 'OMAR' &
               df_con_ids_1$primer_apellido == 'CASTRO' &
               df_con_ids_1$segundo_apellido == 'BIANCHINO',
              "id_unificado"] <- 1809

df_con_ids_1[df_con_ids_1$primer_nombre == 'EDUARDO' &
               df_con_ids_1$primer_apellido == 'CASTRO' &
               df_con_ids_1$legislatura %in%c(47,48),
               "id_unificado"] <- 1804

df_con_ids_1[df_con_ids_1$primer_nombre == 'ALEJANDRO' &
               df_con_ids_1$primer_apellido == 'FERNANDEZ' &
               df_con_ids_1$partido == 'Frente Amplio' &
               df_con_ids_1$legislatura %in%c(48),
                "id_unificado"] <- 2935

df_con_ids_1[df_con_ids_1$primer_nombre == 'JOSE' &
               df_con_ids_1$primer_apellido == 'FERNANDEZ' &
               df_con_ids_1$partido == 'Frente Amplio' &
               df_con_ids_1$legislatura %in%c(45),
             "id_unificado"] <- 2914

df_con_ids_1[df_con_ids_1$primer_nombre == 'PEDRO' &
               df_con_ids_1$primer_apellido == 'ETCHEGARAY' &
               df_con_ids_1$partido == 'Partido Colorado' &
               df_con_ids_1$legislatura %in%c(46,48),
             "id_unificado"] <- 2739

df_con_ids_1[df_con_ids_1$primer_nombre == 'JUAN' &
               df_con_ids_1$primer_apellido == 'ETCHEVERRY' &
               df_con_ids_1$partido == 'Frente Amplio' &
               df_con_ids_1$legislatura %in%c(43),
             "id_unificado"] <- 2763

df_con_ids_1[df_con_ids_1$primer_nombre == 'JOSE' &
               df_con_ids_1$primer_apellido == 'FERNANDEZ' &
               df_con_ids_1$segundo_apellido == 'SALDAÑA' &
               df_con_ids_1$legislatura %in%c(23),
             "id_unificado"] <- 2880

df_con_ids_1[df_con_ids_1$primer_nombre == 'DANIEL' &
               df_con_ids_1$primer_apellido == 'FERNANDEZ' &
               df_con_ids_1$segundo_apellido == 'FRANCA' &
               df_con_ids_1$legislatura %in%c(42),
             "id_unificado"] <- 2889

df_con_ids_1[df_con_ids_1$primer_nombre == 'WASHINGTON' &
               df_con_ids_1$primer_apellido == 'FERNANDEZ' &
               df_con_ids_1$segundo_apellido == 'MARIN' &
               df_con_ids_1$legislatura %in%c(38),
             "id_unificado"] <- 2954

df_con_ids_1[df_con_ids_1$primer_nombre == 'JULIO' &
               df_con_ids_1$primer_apellido == 'FERNANDEZ' &
               df_con_ids_1$partido == 'Partido Colorado' &
               df_con_ids_1$legislatura %in%c(47),
             "id_unificado"] <- 2916

df_con_ids_1[df_con_ids_1$primer_nombre == 'GUILLERMO' &
               df_con_ids_1$primer_apellido == 'GARCIA' &
               df_con_ids_1$segundo_nombre == 'L.',
             "id_unificado"] <- 3340

df_con_ids_1[df_con_ids_1$primer_nombre == 'ALFREDO' &
               df_con_ids_1$primer_apellido == 'GARCIA' &
               df_con_ids_1$partido == 'Frente Amplio',
             "id_unificado"] <- 3371

df_con_ids_1[df_con_ids_1$primer_nombre == 'ORLANDO' &
               df_con_ids_1$primer_apellido == 'GIL' &
               df_con_ids_1$partido == 'Frente Amplio',
             "id_unificado"] <- 3486

df_con_ids_1[df_con_ids_1$primer_nombre == 'FRANCISCO' &
               df_con_ids_1$primer_apellido == 'GOMEZ' &
               df_con_ids_1$segundo_nombre == 'LARRIERA',
             "id_unificado"] <- 3561

df_con_ids_1[df_con_ids_1$primer_nombre == 'HUGO' &
               df_con_ids_1$primer_apellido == 'GONZALEZ' &
               df_con_ids_1$partido == 'Partido Nacional',
             "id_unificado"] <- 3644

df_con_ids_1[df_con_ids_1$primer_nombre == 'CARLOS' &
               df_con_ids_1$primer_apellido == 'GONZALEZ' &
               df_con_ids_1$partido == 'Partido Colorado' &
               df_con_ids_1$legislatura == 35,
             "id_unificado"] <- 3651

df_con_ids_1[df_con_ids_1$primer_nombre == 'ALBERTO' &
               df_con_ids_1$primer_apellido == 'GONZALEZ' &
               df_con_ids_1$partido == 'Frente Amplio',
             "id_unificado"] <- 3636

df_con_ids_1[df_con_ids_1$primer_nombre == 'CESAR' &
               df_con_ids_1$primer_apellido == 'GUTIERREZ' &
               df_con_ids_1$segundo_nombre == 'G.',
             "id_unificado"] <- 3811

df_con_ids_1[df_con_ids_1$primer_nombre == 'ALBERTO' &
               df_con_ids_1$primer_apellido == 'IGLESIAS' &
               df_con_ids_1$segundo_apellido == 'PUIG',
             "id_unificado"] <- 3998

df_con_ids_1[df_con_ids_1$primer_nombre == 'JUAN' &
               df_con_ids_1$primer_apellido == 'LOPEZ' &
               df_con_ids_1$segundo_apellido == 'GUTIERREZ',
             "id_unificado"] <- 4420

df_con_ids_1[df_con_ids_1$primer_nombre == 'JUAN' &
               df_con_ids_1$primer_apellido == 'MARTINEZ' &
               df_con_ids_1$legislatura %in%c (47,41),
             "id_unificado"] <- 4709

df_con_ids_1[df_con_ids_1$primer_nombre == 'JORGE' &
               df_con_ids_1$primer_apellido == 'MARTINEZ' &
               df_con_ids_1$legislatura %in%c (47),
             "id_unificado"] <- 4753

df_con_ids_1[df_con_ids_1$primer_nombre == 'CARLOS' &
               df_con_ids_1$primer_apellido == 'MORENO' &
               df_con_ids_1$segundo_nombre == 'B.' &
               df_con_ids_1$legislatura %in%c (35, 36,37),
             "id_unificado"] <- 5172

df_con_ids_1[df_con_ids_1$primer_nombre == 'MARIA' &
               df_con_ids_1$primer_apellido == 'MUNIZ' &
               df_con_ids_1$partido == 'Partido Nacional',
             "id_unificado"] <- 5206

df_con_ids_1[df_con_ids_1$primer_nombre == 'ADOLFO' &
               df_con_ids_1$primer_apellido == 'PEREZ' &
               df_con_ids_1$segundo_apellido == 'PIERA',
             "id_unificado"] <- 5875

df_con_ids_1[df_con_ids_1$primer_nombre == 'ADOLFO' &
               df_con_ids_1$primer_apellido == 'PEREZ' &
               df_con_ids_1$segundo_apellido == 'SANCHEZ' &
               df_con_ids_1$legislatura %in%c (32),
             "id_unificado"] <- 8894

df_con_ids_1[df_con_ids_1$primer_nombre == 'ADOLFO' &
               df_con_ids_1$primer_apellido == 'PEREZ' &
               df_con_ids_1$segundo_apellido == 'SANDE',
             "id_unificado"] <- 8895

df_con_ids_1[df_con_ids_1$primer_nombre == 'LUIS' &
               df_con_ids_1$primer_apellido == 'PONCE' &
               df_con_ids_1$legislatura == 37,
             "id_unificado"] <- 6060

df_con_ids_1[df_con_ids_1$primer_nombre == 'CARLOS' &
               df_con_ids_1$primer_apellido == 'RODRIGUEZ' &
               df_con_ids_1$segundo_apellido == 'ALVEZ',
             "id_unificado"] <- 6550

df_con_ids_1[df_con_ids_1$primer_nombre == 'CARLOS' &
             df_con_ids_1$primer_apellido == 'RODRIGUEZ' &
             df_con_ids_1$segundo_apellido == 'LABRUNA' &
             df_con_ids_1$legislatura %in%c (40,41,42),
             "id_unificado"] <- 8896

df_con_ids_1[df_con_ids_1$primer_nombre == 'CARLOS' &
               df_con_ids_1$primer_apellido == 'RODRIGUEZ' &
               df_con_ids_1$segundo_apellido == 'CAL' &
               df_con_ids_1$legislatura %in%c (39,41,40),
             "id_unificado"] <- 8897

df_con_ids_1[df_con_ids_1$primer_nombre == 'HUGO' &
               df_con_ids_1$primer_apellido == 'RODRIGUEZ' &
               df_con_ids_1$segundo_apellido == 'CARRASCO' &
               df_con_ids_1$legislatura %in%c (39,41,39),
             "id_unificado"] <- 6535

df_con_ids_1[df_con_ids_1$primer_nombre == 'JUAN' &
               df_con_ids_1$primer_apellido == 'RODRIGUEZ' &
               df_con_ids_1$segundo_apellido == 'CORREA' &
               df_con_ids_1$legislatura %in%c (36,37,38,35),
             "id_unificado"] <- 6457

df_con_ids_1[df_con_ids_1$primer_nombre == 'RAMON' &
               df_con_ids_1$primer_apellido == 'RODRIGUEZ' &
               df_con_ids_1$partido == 'Partido Colorado' &
               df_con_ids_1$legislatura %in%c (48),
             "id_unificado"] <- 6595

df_con_ids_1[df_con_ids_1$primer_nombre == 'ENRIQUE' &
               df_con_ids_1$primer_apellido == 'RODRIGUEZ' &
               df_con_ids_1$partido %in%c ('Frente Izquierda de Liberacion','Frente Amplio'),
             "id_unificado"] <- 8891

df_con_ids_1[df_con_ids_1$primer_nombre == 'JOSE' &
               df_con_ids_1$segundo_nombre == 'LUIS' &
               df_con_ids_1$primer_apellido == 'RODRIGUEZ' &
               df_con_ids_1$legislatura == 41 &
               df_con_ids_1$partido %in%c ('Partido Nacional'),
             "id_unificado"] <- 6544

df_con_ids_1[df_con_ids_1$primer_nombre == 'JUAN' &
               df_con_ids_1$segundo_nombre == 'CARLOS' &
               df_con_ids_1$primer_apellido == 'RODRIGUEZ' &
               df_con_ids_1$legislatura %in%c (43,44) &
               df_con_ids_1$partido %in%c ('Partido Colorado'),
             "id_unificado"] <- 8898

df_con_ids_1[df_con_ids_1$primer_nombre == 'ANTONIO' &
               df_con_ids_1$segundo_nombre == 'J.' &
               df_con_ids_1$primer_apellido == 'RODRIGUEZ' &
               df_con_ids_1$legislatura %in%c (29) &
               df_con_ids_1$partido %in%c ('Partido Nacional'),
             "id_unificado"] <- 6499

df_con_ids_1[df_con_ids_1$primer_nombre == 'MARIO' &
               df_con_ids_1$primer_apellido == 'ROSSI' &
               df_con_ids_1$partido %in%c ('Partido Comuna'),
             "id_unificado"] <- 6713

df_con_ids_1[df_con_ids_1$primer_nombre == 'ALEJANDRO' &
               df_con_ids_1$primer_apellido == 'SANCHEZ' &
               df_con_ids_1$partido %in%c ('Partido cuatro puntos cardinales'),
             "id_unificado"] <- 6879

df_con_ids_1[df_con_ids_1$primer_nombre == 'JUAN' &
               df_con_ids_1$segundo_nombre == 'PEDRO' &
               df_con_ids_1$primer_apellido == 'SUAREZ' &
               df_con_ids_1$legislatura %in%c (29,30,31,32) &
               df_con_ids_1$partido %in%c ('Partido Nacional'),
             "id_unificado"] <- 7363

df_con_ids_1[df_con_ids_1$primer_nombre == 'CARLOS' &
               df_con_ids_1$segundo_nombre == 'MARIA' &
               df_con_ids_1$primer_apellido == 'URIARTE' &
               df_con_ids_1$partido %in%c ('Partido Colorado'),
             "id_unificado"] <- 7632

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
