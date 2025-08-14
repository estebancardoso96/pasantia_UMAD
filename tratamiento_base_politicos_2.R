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


# GENERACION DEL ID PRIMER PASO: TOMANDO EL NOMBRE COMPLETO

library(fastLink)
library(RecordLinkage)

rpairs <- compare.dedup(
  politicos["politico"],
  strcmp = TRUE,
  blockfld = 1  # Bloquea por la columna 1 del data frame (en este caso, 'politico')
)


# Paso 2: Calcular los pesos (similaridad)
rpairs <- epiWeights(rpairs)

# Paso 3: Clasificar pares con umbral más permisivo
results <- epiClassify(rpairs, threshold.upper = 0.85)

# Paso 4: Extraer los matches
matched_pairs <- getPairs(results, single.rows = TRUE)

# Paso 5: Generar cluster_ids probabilísticos basados en nombres
cluster_map <- data.frame(
  id = seq_len(nrow(politicos)),
  cluster_id = seq_len(nrow(politicos))
)

# Asignar mismo cluster a registros similares
for(i in seq_len(nrow(matched_pairs))) {
  id1 <- matched_pairs$id1[i]
  id2 <- matched_pairs$id2[i]
  min_id <- min(cluster_map$cluster_id[id1], cluster_map$cluster_id[id2])
  cluster_map$cluster_id[cluster_map$id %in% c(id1, id2)] <- min_id
}

# Paso 6: Agregar `cluster_id` al data frame original
politicos <- politicos %>%
  mutate(cluster_id = cluster_map$cluster_id)

# LIMPIO APELLIDOS COMPUESTOS (PROCESO DE ESTANDARIZACIÓN)

politicos <- politicos %>%
  select(1, id_match, everything()) 

df_limpio <- politicos %>%
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
                          "COLL", "EACHEN", "ALLISTER GREEN", "VICAR", "SOUZA", "COSTA", "ANGELIS", "LA PEA", "LA SOVERA",
                          "LARROBLA", "VEDIA", "BRUM","LA HANTY", "MULA", 'ARTEAGA','AMORES', 'PAZOS','HAEDO HARLEY',
                          'BRUM CARBAJAL','CARLOS PINTOS','CASTRO PEREZ','POSADAS BELGRANO','VEGA')

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

df_limpio[which(df_limpio$primer_nombre == 'JOSE' &
                  df_limpio$segundo_apellido == 'LANGORTA'), 
          "primer_apellido"] <- 'LANGORTA'

df_limpio[which(df_limpio$primer_nombre == 'JOSE' &
          df_limpio$primer_apellido == 'LANGORTA'), "segundo_nombre"] <- 'LUIS'

df_limpio[which(df_limpio$primer_nombre == 'JUAN'
          & df_limpio$primer_apellido == 'JOSE'), "primer_apellido"] <- 'FIGUEROA'

df_limpio[which(df_limpio$primer_nombre == 'JUAN'
                   & df_limpio$primer_apellido == 'FIGUEROA'), "segundo_nombre"] <- 'JOSE'

df_limpio[which(df_limpio$primer_nombre == 'JUAN'
                   & df_limpio$primer_apellido == 'FIGUEROA'), "segundo_apellido"] <- ''

df_limpio[df_limpio$primer_nombre == 'WASHINGTON' &
          df_limpio$primer_apellido == 'BELTRAN'
          & df_limpio$legislatura %in%c(25,26,27), "segundo_apellido"] <- 'BARBAT'

df_limpio[df_limpio$primer_nombre == 'WASHINGTON' &
            df_limpio$primer_apellido == 'BELTRAN'
          & df_limpio$legislatura %in%c(35,36,37,38,39,40,41), "segundo_apellido"] <- 'MULLIN'

df_limpio[df_limpio$primer_nombre == 'WASHINGTON' &
            df_limpio$primer_apellido == 'BELTRAN'
          & df_limpio$legislatura %in%c(35,36,37,38,39,40,41), "segundo_apellido"] <- 'MULLIN'

df_limpio[df_limpio$primer_nombre == 'JOSE'
          & df_limpio$primer_apellido == 'LUIS'
          & df_limpio$cluster_id == 6503, "primer_apellido"] <- 'CUELLO'

df_limpio[df_limpio$primer_nombre == 'JOSE'
          & df_limpio$primer_apellido == 'CUELLO'
          & df_limpio$cluster_id == 6503, "segundo_nombre"] <- 'LUIS'

df_limpio[df_limpio$primer_nombre == 'JOSE'
          & df_limpio$primer_apellido == 'LUIS'
          & df_limpio$cluster_id == 6333, "primer_apellido"] <- 'REDES'

df_limpio[df_limpio$primer_nombre == 'JOSE'
          & df_limpio$primer_apellido == 'REDES'
          & df_limpio$cluster_id == 6333, "segundo_nombre"] <- 'LUIS'

df_limpio[df_limpio$primer_nombre == 'JOSE'
          & df_limpio$primer_apellido == 'LUIS'
          & df_limpio$cluster_id == 7191, "primer_apellido"] <- 'NUÑEZ'

df_limpio[df_limpio$primer_nombre == 'JOSE'
          & df_limpio$primer_apellido == 'NUÑEZ'
          & df_limpio$cluster_id == 7191, "primer_apellido"] <- 'LUIS'

partidos_unicos <- df_limpio %>% group_by(partido) %>% count()

####################################################################################################

# SEGUNDO PASO PARA ID (comparar primer nombre, primer apellido y segundo apellido) 

# Busqueda de falsos negativos

falsos_negativos <- df_limpio %>% group_by(primer_nombre, primer_apellido) %>%
  filter(n_distinct(cluster_id) > 1) %>% ungroup()

df_limpio_1 <- df_limpio %>% group_by(primer_nombre, primer_apellido) %>%
  filter(n_distinct(cluster_id) <= 1) %>% ungroup()

falsos_negativos <- falsos_negativos %>% 
  mutate(across(where(is.character), ~ ifelse(is.na(.), "", .)))

campos_comparar <- falsos_negativos[c("primer_nombre", "primer_apellido", "segundo_apellido",
"partido")]

rpairs <- compare.dedup(
  campos_comparar,
  strcmp = c(TRUE, TRUE, TRUE,TRUE),  # compara strings para los 3 campos
  blockfld = c("primer_apellido","primer_nombre","partido")   # bloquear por apellido
)

# Paso 2: Calcular los pesos (similaridad)
rpairs <- epiWeights(rpairs)

# Paso 3: Clasificar pares con umbral más permisivo
results <- epiClassify(rpairs, threshold.upper = 0.70)

# Paso 4: Extraer los matches
matched_pairs <- getPairs(results, single.rows = TRUE)

# Paso 5: Generar cluster_ids probabilísticos basados en nombres
n <- nrow(campos_comparar)
cluster_map <- data.frame(
  id = seq_len(n),
  cluster_id = seq_len(n)
)

# Asignar mismo cluster a registros similares
for(i in seq_len(nrow(matched_pairs))) {
  id1 <- matched_pairs$id1[i]
  id2 <- matched_pairs$id2[i]
  min_id <- min(cluster_map$cluster_id[id1], cluster_map$cluster_id[id2])
  cluster_map$cluster_id[cluster_map$id %in% c(id1, id2)] <- min_id
}

campos_comparar$cluster_id <- cluster_map$cluster_id

# Paso 6: Agregar `cluster_id` al data frame original
falsos_negativos <- falsos_negativos %>%
  bind_cols(cluster_id_nuevo = campos_comparar$cluster_id + 8677 - 1)

falsos_negativos <- falsos_negativos %>% select(5, cluster_id_nuevo, everything())

## CORRECIONES EN DF LIMPIO Y EN LOS FALSOS NEGATIVOS

df_limpio_1[which(df_limpio_1$primer_apellido == 'BELTRAN' &
            df_limpio_1$segundo_apellido == 'MULLIN'),
            "cluster_id"] <- 474

df_limpio_1[which(df_limpio_1$primer_apellido == 'BELTRAN' &
            df_limpio_1$segundo_apellido == 'BARBAT'),
            "cluster_id"] <- 8679

df_limpio_1[which(df_limpio_1$primer_nombre == 'ALEJANDRO' &
                  df_limpio_1$primer_apellido == 'SANCHEZ' &
                  df_limpio_1$partido == 'Partido cuatro puntos cardinales'),
                  "cluster_id"] <- 8680

df_limpio_1[which(df_limpio_1$primer_nombre == 'MARIO' &
                  df_limpio_1$primer_apellido == 'ROSSI' &
                  df_limpio_1$partido == 'Partido Comuna'),
                  "cluster_id"] <- 8681

falsos_negativos[falsos_negativos$primer_nombre == 'HORACIO' &
                 falsos_negativos$primer_apellido == 'ABADIE' &
                 falsos_negativos$segundo_apellido == 'SANTOS' &
                 falsos_negativos$legislatura == 40,"cluster_id_nuevo"] <- 8678

falsos_negativos[falsos_negativos$primer_nombre == 'HORACIO' &
                 falsos_negativos$primer_apellido == 'ABADIE' &
                 falsos_negativos$segundo_apellido == 'SANTOS' &
                 falsos_negativos$legislatura == 40,"cluster_id_nuevo"] <- 8678

falsos_negativos[falsos_negativos$primer_nombre == 'ADOLFO' &
                 falsos_negativos$primer_apellido == 'PEREZ' &
                 falsos_negativos$segundo_apellido == 'SANDE'
                 ,"cluster_id_nuevo"] <- 10281

###### UNION DE LAS BASES ######

falsos_negativos_1 <- falsos_negativos %>% filter(primer_apellido == "LACALLE" | (primer_apellido == "FLORES"
                      & primer_nombre == 'MANUEL' | primer_nombre == 'LUIS' & primer_apellido == 'HIERRO')) %>%
                      select(-c(cluster_id_nuevo))

falsos_negativos <- falsos_negativos %>% filter(primer_apellido != "LACALLE" & (primer_apellido != "FLORES"
                                                | primer_nombre != 'MANUEL') & (primer_apellido != "HIERRO"
                                                                                | primer_nombre != 'LUIS'))

falsos_negativos_1 <- falsos_negativos_1 %>%
  mutate(across(where(is.character), ~na_if(., "")))

falsos_negativos <- falsos_negativos %>%
  mutate(across(where(is.character), ~na_if(., ""))) %>% select(-cluster_id) %>%
  rename(cluster_id=cluster_id_nuevo)

## uno este df con df_limpio_1

politicos_id <- rbind(df_limpio_1, falsos_negativos_1, falsos_negativos) %>%
  rename(id_politico=cluster_id)

politicos_id[politicos_id$primer_nombre == 'ADOLFO' &
             politicos_id$primer_apellido == 'PEREZ' &
             politicos_id$segundo_apellido == 'SANCHEZ'
            ,"id_politico"] <- 10282

politicos_id[politicos_id$primer_nombre == 'JULIO' &
             politicos_id$primer_apellido == 'BESOZZI'
            ,"id_politico"] <- 5556

politicos_id[which(politicos_id$primer_nombre == 'JOSE' &
             politicos_id$primer_apellido == 'LOPEZ' &
             politicos_id$segundo_apellido == 'LAPHITZ')
             ,"id_politico"] <- 10283

politicos_id[which(politicos_id$primer_nombre == 'PEDRO' &
             politicos_id$primer_apellido == 'ETCHEGARAY' &
             politicos_id$partido == 'Partido Colorado')
             ,"id_politico"] <- 10284

politicos_id[which(politicos_id$primer_nombre == 'LUIS' &
             politicos_id$primer_apellido == 'BOTANA')
             ,"id_politico"] <- 590

politicos_id[which(politicos_id$primer_nombre == 'RENAN' &
             politicos_id$primer_apellido == 'RODRIGUEZ' &
             politicos_id$segundo_apellido == 'SANTURIO')
             ,"id_politico"] <- 10285

politicos_id[which(politicos_id$primer_nombre == 'HUMBERTO' &
             politicos_id$primer_apellido == 'CASTELLI' &
             politicos_id$legislatura == 31)
             ,"id_politico"] <- 9021

politicos_id[which(politicos_id$primer_nombre == 'JUAN' &
             politicos_id$primer_apellido == 'BORDABERRY')
             ,"id_politico"] <- 8931

politicos_id %>% distinct(id_politico) %>% count() 

politicos_id <-  politicos_id %>%
  mutate(id_politico = dense_rank(id_politico))


###################################### FECHA DE NACIMIENTO ####################################

library(arrow)

df_nac <- read_parquet("df_nac.parquet")

prueba <- apellidos_1 %>% inner_join(df_nac, by =c('primer_nombre','primer_apellido'))

prueba <- ids %>%
  inner_join(df_nac, by =c('primer_nombre','primer_apellido','segundo_nombre','segundo_apellido'))


######################################################
apellidos <- politicos_id %>% distinct(id_politico, primer_apellido) %>%
group_by(primer_apellido) %>% count() %>% arrange(desc(n))


apellidos_1 <- apellidos %>% filter(n == 1)

apellidos_1 <- politicos_id %>%
  filter(primer_apellido %in% apellidos_1$primer_apellido)



ids <-apellidos_1 %>%
  select(primer_nombre, segundo_nombre, primer_apellido, segundo_apellido, id_politico, fecha_inicio) %>%
  distinct() %>%
  group_by(primer_nombre, segundo_nombre, primer_apellido, segundo_apellido, id_politico) %>%
  slice_max(order_by = fecha_inicio, n = 1, with_ties = FALSE) %>%
  ungroup()

write.csv(ids,row.names = FALSE,"base_fecha_nac.csv")

base <- read.csv("base_fecha_nac.csv")

