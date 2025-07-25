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
  select(1, cluster_id, everything()) 

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
  filter(n_distinct(cluster_id) > 1)

df_limpio_1 <- df_limpio %>% group_by(primer_nombre, primer_apellido) %>%
  filter(n_distinct(cluster_id) <= 1)

falsos_negativos <- falsos_negativos %>% select(-c(fecha_inicio, fecha_fin)) %>% 
  mutate(across(everything(), ~ifelse(is.na(.), "", .)))

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
results <- epiClassify(rpairs, threshold.upper = 0.95)

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
  bind_cols(cluster_id_nuevo = campos_comparar$cluster_id)

falsos_negativos <- falsos_negativos %>% select(5, cluster_id_nuevo, everything())


## CORRECIONES EN LOS FALSOS NEGATIVOS


