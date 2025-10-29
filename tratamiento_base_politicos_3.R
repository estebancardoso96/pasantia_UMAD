library(dplyr)
library(tidyverse)
library(stringr)
library(stringi)
library(puy)
library(lubridate)

data("politicos")
data("legislaturas")

# CORTE TEMPORAL: PRIMERA LEGISLATURA QUE COMIENZA EN EL SIGLO XX: LEGISLATURA NÚMERO 21

politicos <- politicos %>% filter(legislatura >= 21)

# GENERACION DEL ID PRIMER PASO: TOMANDO EL NOMBRE COMPLETO

politicos$politico_norm <- politicos$politico %>%
  stri_trans_general("Latin-ASCII") %>%   # quitar tildes
  toupper() %>%                           # pasar a mayúsculas
  gsub("[[:punct:]]", "", .) %>%          # quitar puntuación (, . etc)
  gsub("\\s+", " ", .) %>%                # quitar espacios dobles
  trimws()    

library(stringdist)
dist_mat <- stringdistmatrix(politicos$politico_norm, politicos$politico_norm, method = "lv")

# Clustering jerárquico (agrupa nombres parecidos)
hc <- hclust(as.dist(dist_mat), method = "average")

# Cortamos el árbol en un umbral de distancia máxima aceptable
clusters <- cutree(hc, h = 3)  # 3 caracteres de diferencia como máximo

# Asignamos el ID de grupo
politicos$id_match <- clusters

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
                          'BRUM CARBAJAL','CARLOS PINTOS','CASTRO PEREZ','POSADAS BELGRANO','VEGA', 'ROSA VAZQUEZ', 'ROZA')

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
          & df_limpio$id_match == 3130, "primer_apellido"] <- 'CUELLO'

df_limpio[df_limpio$primer_nombre == 'JOSE'
          & df_limpio$primer_apellido == 'CUELLO'
          & df_limpio$id_match == 3130, "segundo_nombre"] <- 'LUIS'

df_limpio[df_limpio$primer_nombre == 'JOSE'
          & df_limpio$primer_apellido == 'LUIS'
          & df_limpio$id_match == 2995, "primer_apellido"] <- 'REDES'

df_limpio[df_limpio$primer_nombre == 'JOSE'
          & df_limpio$primer_apellido == 'REDES'
          & df_limpio$id_match == 2995, "segundo_nombre"] <- 'LUIS'

df_limpio[df_limpio$primer_nombre == 'JOSE'
          & df_limpio$primer_apellido == 'LUIS'
          & df_limpio$id_match == 3604, "primer_apellido"] <- 'NUÑEZ'

df_limpio[df_limpio$primer_nombre == 'JOSE'
          & df_limpio$primer_apellido == 'NUÑEZ'
          & df_limpio$id_match == 3604, "primer_apellido"] <- 'LUIS'


falsos_negativos <- df_limpio %>% group_by(primer_nombre, primer_apellido) %>%
  filter(n_distinct(id_match) > 1) %>% ungroup()

df_limpio_1 <- df_limpio %>% group_by(primer_nombre, primer_apellido) %>%
  filter(n_distinct(id_match) <= 1) %>% ungroup()

falsos_negativos <- falsos_negativos %>% 
  mutate(across(where(is.character), ~ ifelse(is.na(.), "", .)))

campos_comparar <- falsos_negativos[c("primer_nombre", "primer_apellido", "segundo_apellido",
                                      "partido")]
library(fastLink)
library(RecordLinkage)

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
                  "id_match"] <- 8699

df_limpio_1[which(df_limpio_1$primer_apellido == 'BELTRAN' &
                  df_limpio_1$segundo_apellido == 'BARBAT'),
                 "id_match"] <- 8679

df_limpio_1[which(df_limpio_1$primer_nombre == 'ALEJANDRO' &
                  df_limpio_1$primer_apellido == 'SANCHEZ' &
                  df_limpio_1$partido == 'Partido cuatro puntos cardinales'),
                  "id_match"] <- 8680

df_limpio_1[which(df_limpio_1$primer_nombre == 'MARIO' &
                  df_limpio_1$primer_apellido == 'ROSSI' &
                  df_limpio_1$partido == 'Partido Comuna'),
                 "id_match"] <- 8681

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

falsos_negativos_1 <- falsos_negativos %>%
  filter(primer_apellido == "LACALLE" | (primer_apellido == "FLORES"
                                                                                  & primer_nombre == 'MANUEL' | primer_nombre == 'LUIS' & primer_apellido == 'HIERRO')) %>%
  select(-c(cluster_id_nuevo))

falsos_negativos <- falsos_negativos %>%
  filter(primer_apellido != "LACALLE" & (primer_apellido != "FLORES"
                                                                                | primer_nombre != 'MANUEL') & (primer_apellido != "HIERRO"
                                                                                                                | primer_nombre != 'LUIS'))

falsos_negativos_1 <- falsos_negativos_1 %>%
  mutate(across(where(is.character), ~na_if(., "")))

falsos_negativos <- falsos_negativos %>%
  mutate(across(where(is.character), ~na_if(., ""))) %>% select(-id_match) %>%
  rename(id_match=cluster_id_nuevo)

politicos_id <- rbind(df_limpio_1, falsos_negativos_1, falsos_negativos) %>%
  rename(id_politico=id_match)


politicos_id[politicos_id$primer_nombre == 'ADOLFO' &
             politicos_id$primer_apellido == 'PEREZ' &
             politicos_id$segundo_apellido == 'SANCHEZ'
             ,"id_politico"] <- 10282

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

politicos_id[which(politicos_id$primer_nombre == 'HUMBERTO' &
                   politicos_id$primer_apellido == 'CASTELLI' &
                   politicos_id$legislatura == 29)
                   ,"id_politico"] <- 9021

politicos_id[which(politicos_id$primer_nombre == 'JUAN' &
                   politicos_id$primer_apellido == 'BORDABERRY')
                   ,"id_politico"] <- 8931

politicos_id %>% distinct(id_politico) %>% count() 

politicos_id <-  politicos_id %>%
  mutate(id_politico = dense_rank(id_politico))

politicos_id %>% distinct(id_politico) %>% count() # 4130 ids


###################################### FECHA DE NACIMIENTO ####################################

library(arrow)

df_nac <- read_parquet("df_nac.parquet")

base <- read.csv("base_fecha_nac.csv")
base %>% distinct(id_politico) %>% count()

base2 <- read.csv("base_fecha_nac_2.csv")
base2 %>% distinct(id_politico) %>% count()

base_final <- rbind(base, base2)
# 3268
base_final %>% distinct(id_politico) %>% count()
# por que no tengo todos los id politico?? 4130
politicos_id %>% distinct(id_politico) %>% count()


### Comenzamos con el pegado de las fechas de nacimiento con la base por los nombres y apellidos
### que no se repiten

base[base == ""] <- NA
base <- base %>% select(-(id_politico))
base <- base %>% filter(!is.na(fecha_nac))

politicos_id <- politicos_id %>% select(-(politico_norm))
politicos_id[politicos_id == '']<- NA

politicos_id_1 <- politicos_id %>% left_join(base, by=c("primer_apellido",'segundo_nombre','segundo_apellido',
                                                         "primer_nombre"))%>%select(-ends_with(".y"))  
politicos_id_1 <- politicos_id_1 %>%
  mutate(
    fecha_nac = if_else(
      !is.na(fecha_nac) & nchar(as.character(fecha_nac)) == 4,  # Si es solo año
      as.Date(paste0(fecha_nac, "-06-01")),
      as.Date(fecha_nac)
    )
  )

politicos_id_1$fecha_nac <- as.Date(politicos_id_1$fecha_nac)


### segundo pegado con base2 (lo hago con los nombres y la fecha de inicio de la legislatura)

base2[base2 == '']<- NA
base2 <- base2 %>% filter(!is.na(fecha_nac))

base2 <- base2 %>%
  mutate(
    fecha_nac = if_else(
      !is.na(fecha_nac) & nchar(as.character(fecha_nac)) == 4,  # Si es solo año
      as.Date(paste0(fecha_nac, "-06-01")),
      as.Date(fecha_nac)
    )
  )

base2$fecha_nac <- as.Date(base2$fecha_nac)


base2_00<- base2 %>% distinct(primer_nombre, segundo_nombre, primer_nombre, primer_apellido,segundo_apellido,fecha_inicio,
                              fecha_nac, fuente)

base2_00$fecha_inicio <-as.Date(base2_00$fecha_inicio)

politicos_id_1 <- politicos_id_1 %>% rename('fecha_inicio'='fecha_inicio.x')

politicos_id_1 <- politicos_id_1 %>% left_join(base2_00, by =c('primer_nombre', 'segundo_nombre',
                                                               'primer_apellido', 'segundo_apellido',
                                                               'fecha_inicio')) # revisar por que pega mal

politicos_id_1 <- politicos_id_1 %>% mutate(fuente.y = as.character(fuente.y))

politicos_id_1 <- politicos_id_1 %>%
  mutate(
    fecha_nac = coalesce(fecha_nac.x, fecha_nac.y)
  ) %>% select(-c(fecha_nac.x, fecha_nac.y)) %>% mutate(
    fuente = coalesce(fuente.x, fuente.y)
  ) %>% select(-c(fuente.y, fuente.x)) 

ids_cambio <- politicos_id_1 %>% group_by(id_politico) %>% 
  filter(n_distinct(fecha_nac, na.rm = TRUE) > 1) %>% ungroup()
  
politicos_id_1 <- politicos_id_1 %>% anti_join(ids_cambio, by = 'id_politico')

### Matcheo probabilistico para este conjunto de casos

nombres_completos <- paste(ids_cambio$primer_nombre, ids_cambio$primer_apellido, ids_cambio$segundo_apellido)
dist_mat <- stringdistmatrix(nombres_completos, nombres_completos, method = "lv")

# Clustering jerárquico (agrupa nombres parecidos)
hc <- hclust(as.dist(dist_mat), method = "average")

# Cortamos el árbol en un umbral de distancia máxima aceptable
clusters <- cutree(hc, h = 1)  # 3 caracteres de diferencia como máximo

# Asignamos el ID de grupo
ids_cambio$id_match <- clusters

ids_cambio <- ids_cambio %>%
  select(1, id_match, everything()) 

ids_cambio <- ids_cambio %>% select(-id_politico) 
ids_cambio <- ids_cambio %>% rename(id_politico=id_match)
ids_cambio <- ids_cambio %>%
  mutate(id_politico = dense_rank(id_politico) + 4132 - 1)

# correcciones ad hoc en ids cambio

ids_cambio[ids_cambio$primer_nombre == 'JULIO' &
           ids_cambio$segundo_nombre == 'LUIS' &
           ids_cambio$primer_apellido == 'SANGUINETTI'
           ,"id_politico"] <- 4316

ids_cambio[ids_cambio$primer_nombre == 'FRANCISCO' &
           ids_cambio$primer_apellido == 'FORTEZA'&
           ids_cambio$legislatura %in%c(42, 41)
           ,"id_politico"] <- 4317

politicos_id_1 <- rbind(politicos_id_1, ids_cambio)

### Genero nuevos ids para politicos_id_1 (4131 el id mas alto)

#### FALSOS NEGATIVOS

politicos_id_1[politicos_id_1$primer_nombre == 'JULIO' &
               politicos_id_1$primer_apellido == 'BESOZZI'
               ,"id_politico"] <- 2417

politicos_id_1[politicos_id_1$primer_nombre == 'ANIBAL' &
               politicos_id_1$primer_apellido == 'PEREIRA'
               ,"id_politico"] <- 4020

politicos_id_1[politicos_id_1$primer_nombre == 'ALTIVO' &
               politicos_id_1$primer_apellido == 'ESTEVEZ'
               ,"id_politico"] <- 505

politicos_id_1[politicos_id_1$primer_nombre == 'ALFREDO' &
               politicos_id_1$primer_apellido == 'GARCIA' &
               politicos_id_1$partido == 'Partido Nacional Independiente'    
               ,"id_politico"] <- 3915

politicos_id_1[politicos_id_1$primer_nombre == 'CHRISTIAN' &
               politicos_id_1$primer_apellido == 'MOREL'
               ,"id_politico"] <- 2429

politicos_id_1[politicos_id_1$primer_nombre == 'DANIEL' &
               politicos_id_1$primer_apellido == 'PEÑA'  &
               politicos_id_1$partido == 'Partido de la Gente'
               ,"id_politico"] <- 4028

politicos_id_1[politicos_id_1$primer_nombre == 'EBER' &
               politicos_id_1$primer_apellido == 'DA ROSA'  &
               politicos_id_1$partido == 'Partido Nacional'
               ,"id_politico"] <- 398

politicos_id_1[politicos_id_1$primer_nombre == 'EBER' &
               politicos_id_1$primer_apellido == 'DA ROSA'  &
               politicos_id_1$partido == 'Partido Nacional'
               ,"id_politico"] <- 398

politicos_id_1[politicos_id_1$primer_nombre == 'ENRIQUE' &
               politicos_id_1$primer_apellido == 'RODRIGUEZ'  &
               politicos_id_1$partido == 'Frente Izquierda de Liberacion'
               ,"id_politico"] <- 4062

politicos_id_1[politicos_id_1$primer_nombre == 'HERIBERTO' &
               politicos_id_1$id_politico == 4096 &
               politicos_id_1$partido == 'Partido por el Gobierno del Pueblo'   
               ,"id_politico"] <- 4095

politicos_id_1[politicos_id_1$primer_apellido == 'CARDOSO' &
               politicos_id_1$id_politico == 3855 &
               politicos_id_1$partido == 'Frente Amplio'   
               ,"id_politico"] <- 3854

politicos_id_1[politicos_id_1$primer_apellido == 'ZABALZA' &
               politicos_id_1$id_politico == 4125 &
               politicos_id_1$partido == 'Partido Blanco'   
               ,"id_politico"] <- 4124

politicos_id_1[politicos_id_1$primer_apellido == 'GARDIOL' &
               politicos_id_1$id_politico == 3775 &
               politicos_id_1$primer_nombre == 'NAPOLEON'   
               ,"id_politico"] <- 1804

#### FALSOS POSITIVOS

ids_a_filtrar <- c(
  2516, 2688, 1077, 379, 1786, 3369,1133,1174,2830,4360,
  1350, 2400, 2319, 2078, 3962, 2737, 1783, 10, 3959,4317,
  1196, 3796, 3890, 1384, 4057, 2629, 3825, 3826, 776,4133,
  4010, 3841, 3819, 3820, 2519, 4037, 1748, 3985,927,4050,4041,
  258, 513, 3900, 762, 3908, 3827, 4097, 4099,793,879,4056,3801,
  3935, 1190, 3902, 997, 3281, 960, 2343, 464, 2681,4090,4060,
  3235, 671, 667, 704, 731, 765, 768, 775, 779, 790,955, 4028
)

# Filtrar el data frame excluyendo esos IDs
politicos_id_filtrado <- politicos_id_1 %>%
  filter(id_politico %in% ids_a_filtrar)

# quitar esos id del original
politicos_id_2 <- politicos_id_1 %>%
  filter(!id_politico %in% ids_a_filtrar)

### a partir de 4256

nombres_completos <- paste(politicos_id_filtrado$primer_nombre, politicos_id_filtrado$primer_apellido,
                           politicos_id_filtrado$partido)

dist_mat <- stringdistmatrix(nombres_completos, nombres_completos, method = "lv")

# Clustering jerárquico (agrupa nombres parecidos)
hc <- hclust(as.dist(dist_mat), method = "average")

# Cortamos el árbol en un umbral de distancia máxima aceptable
clusters <- cutree(hc, h = 1)  # 3 caracteres de diferencia como máximo

# Asignamos el ID de grupo
politicos_id_filtrado$id_match <- clusters

politicos_id_filtrado <- politicos_id_filtrado %>%
  select(1, id_match, everything()) 

politicos_id_filtrado <- politicos_id_filtrado %>% select(-id_politico) 
politicos_id_filtrado <- politicos_id_filtrado %>% rename(id_politico=id_match)
politicos_id_filtrado <- politicos_id_filtrado %>%
  mutate(id_politico = dense_rank(id_politico) + 4256 - 1)

# asigno a jose espalter el id anterior

politicos_id_filtrado[politicos_id_filtrado$primer_nombre == 'JOSE' &
                        politicos_id_filtrado$primer_apellido == 'ESPALTER'
                      ,"id_politico"] <- 3889

max(politicos_id_filtrado$id_politico)

df_final <- rbind(politicos_id_2, politicos_id_filtrado)

df_final <- df_final %>%
  group_by(id_politico) %>%
  mutate(
    n_fechas_distintas = n_distinct(fecha_nac[!is.na(fecha_nac)]),
    fecha_nac_final = case_when(
      n_fechas_distintas > 1 ~ "1",  # conflicto
      n_fechas_distintas == 1 ~ as.character(first(na.omit(fecha_nac))), # única fecha
      TRUE ~ NA_character_
    )
  ) %>%
  ungroup() %>%
  select(-n_fechas_distintas)


# ULTIMAS CORRECCIONES

df_final[df_final$primer_nombre == 'ANTONIO' &
         df_final$primer_apellido == 'CHIESA' &
         df_final$segundo_apellido == 'BRUNO' & 
         df_final$legislatura %in%c(47)  
         ,"id_politico"] <- 4371

df_final[df_final$primer_nombre == 'MIGUEL' &
         df_final$primer_apellido == 'DUBRA' &
         df_final$legislatura %in%c(43)  
         ,"id_politico"] <- 4372

df_final[df_final$primer_apellido == 'MOREIRA' &
         df_final$primer_nombre == 'CARLOS' & 
         df_final$legislatura %in%c (41,42)
         ,"id_politico"] <- 4373

df_final[df_final$primer_apellido == 'ACCINELLI' &
         df_final$segundo_apellido == 'GALVEZ'  &
         df_final$legislatura == 39  
         ,"id_politico"] <- 4374

df_final[df_final$primer_apellido == 'ACCINELLI' &
         df_final$primer_nombre == 'GALVEZ' &
         df_final$legislatura %in%c(39)
         ,"feha_nac"] <- NA

df_final[df_final$primer_apellido == 'GALLO' &
         df_final$primer_nombre == 'LUIS'  &
         df_final$legislatura == 48  
         ,"id_politico"] <- 4375

df_final[df_final$primer_apellido == 'COSIO' &
         df_final$primer_nombre == 'VALENTIN'  &
         df_final$legislatura == 39 
         ,"fecha_nac"] <- as.Date("1900-02-14")

df_final[which(df_final$primer_apellido == "BERRO" &
         df_final$primer_nombre == "CARLOS" &
         df_final$segundo_nombre == "MARIA"),
         "id_politico"] <- 4376

df_final[which(df_final$primer_apellido == "RAMIREZ" &
         df_final$primer_nombre == "JOSE" &
         df_final$segundo_nombre == "ANTONIO"),
         "id_politico"] <- 4377

df_final[which(df_final$primer_apellido == "KORZENIAK" &
         df_final$primer_nombre == "JOSE" &
         df_final$segundo_nombre == "IGNACIO"),
         "id_politico"] <- 4378

df_final[which(df_final$primer_apellido == "FABINI" &
         df_final$primer_nombre == "JUAN" &
         df_final$segundo_nombre == "E."),
         "id_politico"] <- 4379

df_final[which(df_final$primer_apellido == "JIMENEZ" &
         df_final$primer_nombre == "JUSTINO" &
         df_final$cargo == "Candidato Presidente"),
         "id_politico"] <- 4380

df_final[which(df_final$id_politico == 4390
         ),
         "fecha_nac_final"] <- "1910-06-01"

df_final[which(df_final$id_politico == 4245
),
"fecha_nac_final"] <- "1902-02-24"

df_final[which(df_final$id_politico == 4213
),
"fecha_nac_final"] <- "1898-06-16"

df_final[which(df_final$id_politico == 4198
),
"fecha_nac_final"] <- "1910-06-03"

df_final[which(df_final$id_politico == 4171
),
"fecha_nac_final"] <- "1883-06-01"

df_final[which(df_final$primer_apellido == "FORTEZA" &
         df_final$segundo_nombre == "(H)"),
         "id_politico"] <- 4391

df_final[which(df_final$id_politico == 4391
),
"fecha_nac_final"] <- "1928-06-01"

df_final[which(df_final$id_politico == 4370
),
"fecha_nac_final"] <- "1892-06-01"

df_final[which(df_final$primer_apellido == "MESA" &
         df_final$primer_nombre == "NICOLAS"),
         "id_politico"] <- 4392

df_final[which(df_final$primer_apellido == "LARRAÑAGA" &
         df_final$primer_nombre == "JORGE" &       
         df_final$legislatura == 40),
         "id_politico"] <- 4393

df_final[which(df_final$primer_apellido == "BOTANA" &
         df_final$segundo_nombre == "SERGIO" &       
         df_final$primer_nombre == "LUIS" &       
         df_final$legislatura == 40),
         "id_politico"] <- 243

max(df_final$id_politico)

write.csv(df_final, 'df_final.csv', row.names = FALSE)

names(df_final)

df_final <- df_final %>% select(-c(feha_nac, fecha_nac)) %>% rename(fecha_nac = fecha_nac_final)
################################################# LEVANTO #################################################

df_final <- df_final %>% mutate(fecha_nac = as.Date(fecha_nac))

# correcciones de las edades (edades negativas)

df_final[which(
  df_final$cargo == "Intendente Interventor" &
    df_final$id_politico == 3724),
  "fecha_inicio"] <- as.Date("1973-06-27")

df_final[which(
         df_final$cargo == "Intendente" &
         df_final$legislatura == 22 &   
         df_final$id_politico == 3750),
         "fecha_inicio"] <- as.Date("1995-06-17")

df_final[which(
  df_final$legislatura == 31 &   
  df_final$id_politico == 91),
  "fecha_inicio"] <- as.Date("1932-02-15")

df_final[which(
  df_final$id_politico == 4340),
  "fecha_nac"] <- as.Date("1880-06-30")

df_final[which(
  df_final$id_politico == 13),
  "fecha_nac"] <- as.Date("1915-06-30")

df_final[which(
  df_final$id_politico == 536),
  "fecha_nac"] <- as.Date("1883-02-19")

df_final[which(
  df_final$id_politico == 13),
  "fecha_nac"] <- NA

df_final[which(
  df_final$id_politico == 13),
  "fecha_nac"] <- NA

df_final[which(
  df_final$id_politico == 1224 &
  df_final$primer_apellido == 'SOUTO'),
  "fecha_nac"] <- NA

df_final[which(
  df_final$id_politico == 1224 &
  df_final$primer_apellido == 'SOUTO'),
  "id_politico"] <- 4394

df_final[which(
  df_final$id_politico == 2461 &
  df_final$primer_apellido == 'GARCIA'),
  "fecha_nac"] <- NA

df_final[which(
  df_final$id_politico == 2461 &
  df_final$primer_apellido == 'SOUTO'),
  "id_politico"] <- 4395

df_final[which(
  df_final$id_politico == 1122 &
  df_final$primer_apellido == 'PONS'),
  "fecha_nac"] <- NA

df_final[which(
  df_final$id_politico == 1122 &
  df_final$primer_apellido == 'PONS'),
  "id_politico"] <- 4396

df_final[which(
  df_final$id_politico == 1122 &
  df_final$primer_apellido == 'SOSA' &  
  df_final$legislatura == 41),
  "fecha_nac"] <- NA

df_final[which(
  df_final$id_politico == 1122 &
  df_final$primer_apellido == 'SOSA' &  
  df_final$legislatura == 41),
  "id_politico"] <- 4397

df_final[which(
  df_final$id_politico == 1122 &
  df_final$primer_apellido == 'ROS'),
  "id_politico"] <- 4398

df_final[which(
  df_final$id_politico == 375 &
  df_final$primer_apellido == 'BONINO'),
  "id_politico"] <- 4399

df_final[which(
  df_final$id_politico == 4399 &
  df_final$primer_apellido == 'BONINO'),
  "fecha_nac"] <-  as.Date('1937-09-25')

df_final[which(
  df_final$id_politico == 376 &
  df_final$primer_apellido == 'ROSA'),
  "id_politico"] <- 4400

df_final[which(
  df_final$id_politico == 4400 &
  df_final$primer_apellido == 'ROSA'),
  "fecha_nac"] <-  NA

df_final[which(
  df_final$id_politico == 3830 &
  df_final$primer_apellido == 'BATLLE'),
  "fecha_nac"] <-  as.Date('1943-04-02')

df_final[which(
  df_final$id_politico == 3830 &
  df_final$segundo_apellido == 'BERTOLINI'),
  "id_politico"] <-  4403

df_final[which(
  df_final$id_politico == 3830 &
  df_final$primer_apellido == 'BATLLE'),
  "fecha_nac"] <-  as.Date('1897-11-26')

df_final[which(
  df_final$id_politico == 536 &
  df_final$primer_apellido == 'BOTANA'),
  "fecha_nac"] <-  as.Date('1964-11-08')

df_final[which(
  df_final$id_politico == 2268 &
  df_final$primer_apellido == 'WILLIMAN' &
  df_final$legislatura == 42),  
  "id_politico"] <-  4401

df_final[which(
  df_final$id_politico == 4401 &
  df_final$primer_apellido == 'WILLIMAN'),
  "fecha_nac"] <-  as.Date('1925-01-05')

df_final[which(
  df_final$id_politico == 2268 &
  df_final$primer_apellido == 'WILLIMAN' &
  df_final$legislatura == 42),  
  "id_politico"] <-  4401

df_final[which(
  df_final$id_politico == 3828 &
  df_final$segundo_apellido == 'ANZA'),  
  "id_politico"] <-  4402

df_final[which(
  df_final$id_politico == 4402 &
  df_final$segundo_apellido == 'ANZA'),
  "fecha_nac"] <-  as.Date('1932-01-29')

df_final[which(
  df_final$primer_apellido == 'FORTEZA' &
  df_final$legislatura >= 39),
  "id_politico"] <-  4391

df_final[which(
  df_final$id_politico == 4391 &
  df_final$primer_apellido == 'FORTEZA'),
  "fecha_nac"] <-  as.Date('1928-06-01')

df_final[which(
  df_final$primer_apellido == 'BARRERA' &
  df_final$legislatura == 45),
  "id_politico"] <-  4404

df_final[which(
  df_final$primer_apellido == 'CABRERA' &
  df_final$primer_nombre == 'JORGE' &
  df_final$legislatura == 48),
  "id_politico"] <-  4405

df_final[which(
  df_final$primer_apellido == 'ARENA' &
  df_final$primer_nombre == 'DANIEL' &
  df_final$partido == 'Partido Colorado'),
  "id_politico"] <-  4406

df_final[which(
  df_final$primer_apellido == 'MESA' &
  df_final$primer_nombre == 'DANIEL' &
  df_final$legislatura == 45),
  "id_politico"] <-  4407

df_final[which(
  df_final$primer_apellido == 'BOSCH' &
  df_final$primer_nombre == 'FEDERICO' &
  df_final$partido == 'Partido Nacional'),
  "id_politico"] <-  4408

df_final[which(
  df_final$primer_apellido == 'CHIESA' &
  df_final$primer_nombre == 'EDUARDO' &
  df_final$legislatura == 45),
  "id_politico"] <-  4409

df_final[which(
  df_final$primer_apellido == 'CAIRO' &
  df_final$primer_nombre == 'DANIEL' &
  df_final$legislatura == 44),
  "id_politico"] <-  4410

df_final[which(
  df_final$primer_apellido == 'CORES' &
  df_final$primer_nombre == 'HUGO' &
  df_final$partido == 'Frente Amplio'),
  "id_politico"] <-  4411

df_final[which(
  df_final$primer_apellido == 'PITA' &
  df_final$primer_nombre == 'JUAN' &
  df_final$segundo_nombre == 'CARLOS' &  
  df_final$partido == 'Frente Amplio'),
  "id_politico"] <-  4412

df_final[which(
  df_final$primer_apellido == 'PITA' &
  df_final$primer_nombre == 'JUAN' &
  df_final$segundo_nombre == 'CARLOS' &  
  df_final$partido == 'Partido Nacional'),
  "id_politico"] <-  4412

df_final[which(
  df_final$primer_apellido == 'CRUZ' &
  df_final$primer_nombre == 'PEDRO' &
  df_final$legislatura == 48 &  
  df_final$partido == 'Frente Amplio'),
  "id_politico"] <-  4413

df_final[which(
  df_final$primer_apellido == 'YAEZ' &
  df_final$primer_nombre == 'RUBEN'),
  "id_politico"] <-  4414

df_final[which(
  df_final$primer_apellido == 'MONTES' &
  df_final$primer_nombre == 'HECTOR' &
  df_final$partido == 'Partido Colorado'),
  "id_politico"] <-  4415

df_final[which(
  df_final$primer_apellido == 'FREY' &
  df_final$primer_nombre == 'RUBEN' &
  df_final$partido == 'Frente Amplio'),
  "id_politico"] <-  4416

df_final[which(
  df_final$primer_apellido == 'FURLATI' &
  df_final$primer_nombre == 'JUAN' &
  df_final$partido == 'Partido Colorado'),
  "id_politico"] <-  4417

df_final[which(
  df_final$primer_apellido == 'GARCIA' &
  df_final$primer_nombre == 'EMILIO' &
  df_final$legislatura == 48),
  "id_politico"] <-  4418

df_final[which(
  df_final$primer_apellido == 'LAGO' &
  df_final$primer_nombre == 'RAUL' &
  df_final$partido == 'Partido Colorado'),
  "id_politico"] <-  4419

df_final[which(
  df_final$primer_apellido == 'LONGO' &
  df_final$primer_nombre == 'FERNANDO' &
  df_final$partido == 'Frente Amplio'),
  "id_politico"] <-  4420

df_final[which(
  df_final$primer_apellido == 'MENENDEZ' &
  df_final$primer_nombre == 'JORGE' &
  df_final$partido == 'Frente Amplio'),
  "id_politico"] <-  4421

df_final[which(
  df_final$primer_apellido == 'TOLEDO' &
  df_final$primer_nombre == 'PEDRO' &
  df_final$partido == 'Frente Amplio'),
  "id_politico"] <-  4422

df_final[which(
  df_final$primer_apellido == 'ARBIO' &
  df_final$primer_nombre == 'PEDRO' &
  df_final$partido == 'Partido Colorado'),
  "id_politico"] <-  4424

df_final[which(
  df_final$primer_apellido == 'BARRIOS' &
  df_final$primer_nombre == 'HEBEL'),
  "id_politico"] <-  4425

df_final[which(
  df_final$primer_apellido == 'BORJAS' &
  df_final$primer_nombre == 'LUIS' &
  df_final$partido == 'Partido Nacional'),
  "id_politico"] <-  4426

df_final[which(
  df_final$primer_apellido == 'CASTRO' &
  df_final$primer_nombre == 'TOMAS' &
  df_final$partido == 'Partido Nacional'),
  "id_politico"] <-  4427

df_final[which(
  df_final$primer_apellido == 'CID' &
  df_final$primer_nombre == 'ALBERTO' &
  df_final$partido == 'Frente Amplio'),
  "id_politico"] <-  4428

df_final[which(
  df_final$primer_apellido == 'CORONEL' &
  df_final$primer_nombre == 'JOSE' &
  df_final$partido == 'Frente Amplio'),
  "id_politico"] <-  4429

df_final[which(
  df_final$primer_apellido == 'CORREA' &
  df_final$primer_nombre == 'WALTER' &
  df_final$partido == 'Frente Amplio'),
  "id_politico"] <-  4430

df_final[which(
  df_final$primer_apellido == 'CORREA' &
  df_final$primer_nombre == 'DANIEL' &
  df_final$partido == 'Partido Nacional'),
  "id_politico"] <-  4431

df_final[which(
  df_final$primer_apellido == 'DINI' &
  df_final$primer_nombre == 'LUIS' &
  df_final$partido == 'Partido Colorado'),
  "id_politico"] <-  4432

df_final[which(
  df_final$primer_apellido == 'FALCO' &
  df_final$primer_nombre == 'CARLOS' &
  df_final$partido == 'Frente Amplio'),
  "id_politico"] <-  4433

df_final[which(
  df_final$primer_apellido == 'FERNANDEZ' &
  df_final$primer_nombre == 'DINA' &
  df_final$partido == 'Partido Nacional'),
  "id_politico"] <-  4434

df_final[which(
  df_final$primer_apellido == 'FERNANDEZ' &
  df_final$primer_nombre == 'DIVER'),
  "id_politico"] <-  4435

df_final[which(
  df_final$primer_apellido == 'CENANDES' &
  df_final$primer_nombre == 'JESUS'),
  "id_politico"] <-  4436

df_final[which(
  df_final$primer_apellido == 'FERRARI' &
  df_final$primer_nombre == 'HUGO' &
  df_final$partido == 'Partido Colorado'),
  "id_politico"] <- 4437

df_final[which(
  df_final$primer_apellido == 'FERREIRA' &
  df_final$primer_nombre == 'SILVIA' &
  df_final$partido == 'Partido Nacional'),
  "id_politico"] <- 4438

df_final[which(
  df_final$primer_apellido == 'GUSSONI' &
  df_final$primer_nombre == 'HUGO' &
  df_final$partido == 'Partido Colorado'),
  "id_politico"] <- 4439

df_final[which(
  df_final$primer_apellido == 'GONZALEZ' &
  df_final$primer_nombre == 'BERNARDO' &
  df_final$partido == 'Partido Nacional'),
  "id_politico"] <- 4440

df_final[which(
  df_final$primer_apellido == 'GONZALEZ' &
  df_final$primer_nombre == 'MABEL' &
  df_final$partido == 'Nuevo Espacio'),
  "id_politico"] <- 4441

df_final[which(
  df_final$primer_apellido == 'GONZALEZ' &
  df_final$primer_nombre == 'MABEL' &
  df_final$partido == 'Partido Nacional'),
  "id_politico"] <- 4442

df_final[which(
  df_final$primer_apellido == 'GONZALEZ' &
  df_final$primer_nombre == 'MILTON' &
  df_final$partido == 'Partido Nacional'),
  "id_politico"] <- 4443

df_final[which(
  df_final$primer_apellido == 'GONZALEZ' &
  df_final$primer_nombre == 'NELSON' &
  df_final$partido == 'Frente Amplio'),
  "id_politico"] <- 4444

df_final[which(
  df_final$primer_apellido == 'ITUO' &
  df_final$primer_nombre == 'JOSE' &
  df_final$partido == 'Partido Nacional'),
  "id_politico"] <- 4445

df_final[which(
  df_final$primer_apellido == 'LONG' &
  df_final$primer_nombre == 'MARIO' &
  df_final$partido == 'Partido Nacional'),
  "id_politico"] <- 4446

df_final[which(
  df_final$primer_apellido == 'MESA' &
  df_final$primer_nombre == 'MAXIMO' &
  df_final$partido == 'Partido Nacional'),
  "id_politico"] <- 4447

df_final[which(
  df_final$primer_apellido == 'PERDOMO' &
  df_final$primer_nombre == 'DARO'),
  "id_politico"] <- 4448

df_final[which(
  df_final$primer_apellido == 'PEREIRA' &
  df_final$primer_nombre == 'ARIEL' &
  df_final$partido == 'Partido Nacional'),
  "id_politico"] <- 4449

df_final[which(
  df_final$primer_apellido == 'GOMEZ' &
  df_final$primer_nombre == 'ALVARO' &
  df_final$partido == 'Frente Amplio' &
  df_final$legislatura == 48),
  "id_politico"] <- 4450

df_final[which(
  df_final$primer_apellido == 'PEREZ' &
  df_final$primer_nombre == 'ESTEBAN' &
  df_final$partido == 'Frente Amplio'),
  "id_politico"] <- 4451

df_final[which(
  df_final$primer_apellido == 'RAMOS' &
  df_final$primer_nombre == 'ROQUE' &
  df_final$partido == 'Nuevo Espacio'),
  "id_politico"] <- 4452

df_final[which(
  df_final$primer_apellido == 'RODRIGUEZ' &
  df_final$primer_nombre == 'LIRIO' &
  df_final$partido == 'Frente Amplio'),
  "id_politico"] <- 4453

df_final[which(
  df_final$primer_apellido == 'RODRIGUEZ' &
  df_final$primer_nombre == 'MARTIN' &
  df_final$partido == 'Partido Colorado'),
  "id_politico"] <- 4454

df_final[which(
  df_final$primer_apellido == 'SCAIOLA' &
  df_final$primer_nombre == 'GUSTAVO'),
  "id_politico"] <- 4455

df_final[which(
  df_final$primer_apellido == 'SOLIS' &
  df_final$primer_nombre == 'MARISA'),
  "id_politico"] <- 4456

df_final[which(
  df_final$primer_apellido == 'SASSO' &
  df_final$primer_nombre == 'WALTER'),
  "id_politico"] <- 4457

df_final[which(
  df_final$primer_apellido == 'VARELA' &
  df_final$primer_nombre == 'GUSTAVO' &
  df_final$partido == 'Partido Nacional'),
  "id_politico"] <- 4458

df_final[which(
  df_final$primer_apellido == 'BORGES' &
  df_final$primer_nombre == 'CARLOS' &
  df_final$partido == 'Frente Amplio'),
  "id_politico"] <- 4459

df_final[which(
  df_final$primer_apellido == 'UMPIERREZ' &
  df_final$primer_nombre == 'JAVIER' &
  df_final$partido == 'Frente Amplio'),
  "id_politico"] <- 4460

df_final[which(
  df_final$primer_apellido == 'OLIVERA' &
  df_final$primer_nombre == 'RAUL' &
  df_final$partido == 'Frente Amplio'),
  "id_politico"] <- 4461

df_final[which(
  df_final$primer_apellido == 'ETCHEVERRY' &
  df_final$primer_nombre == 'LUCIA' &
  df_final$partido == 'Frente Amplio'),
  "id_politico"] <- 4462

df_final[which(
  df_final$primer_apellido == 'ALVEAR' &
  df_final$primer_nombre == 'JORGE' &
  df_final$partido == 'Partido Colorado'),
  "id_politico"] <- 4463

df_final[which(
  df_final$primer_apellido == 'ESTEVEZ' &
  df_final$primer_nombre == 'OMAR' &
  df_final$partido == 'Partido Colorado'),
  "id_politico"] <- 4464

df_final[which(
  df_final$primer_apellido == 'VIANA' &
  df_final$primer_nombre == 'PABLO' &
  df_final$partido == 'Partido Nacional'),
  "id_politico"] <- 4465

df_final[which(
  df_final$primer_apellido == 'GONZALEZ' &
  df_final$primer_nombre == 'CONRADO' &
  df_final$partido == 'Partido Colorado'),
  "id_politico"] <- 4466

df_final[which(
  df_final$primer_apellido == 'BASTOS' &
  df_final$primer_nombre == 'JORGE' &
  df_final$partido == 'Partido Colorado'),
  "id_politico"] <- 4467

df_final[which(
  df_final$primer_apellido == 'SANTOS' &
  df_final$primer_nombre == 'JORGE' &
  df_final$partido == 'Frente Amplio'),
  "id_politico"] <- 4468

df_final[which(
  df_final$primer_apellido == 'SILVEIRA' &
  df_final$primer_nombre == 'PABLO' &
  df_final$partido == 'Frente Amplio'),
  "id_politico"] <- 4469

df_final[which(
  df_final$primer_apellido == 'RODRIGUEZ' &
  df_final$primer_nombre == 'MARIA' &
  df_final$segundo_nombre == 'V.' & 
  df_final$legislatura == 48 & 
  df_final$partido == 'Partido Nacional'),
  "id_politico"] <- 4470

df_final[which(
  df_final$primer_apellido == 'RODRIGUEZ' &
  df_final$primer_nombre == 'RICHARD' &
  df_final$partido == 'Partido Nacional'),
  "id_politico"] <- 4471

df_final[which(
  df_final$primer_apellido == 'CARDOZO' &
  df_final$primer_nombre == 'ADEMAR'),
  "id_politico"] <- 4472

df_final[which(
  df_final$primer_apellido == 'ALVEZ' &
  df_final$primer_nombre == 'RICARDO' &
  df_final$partido == 'Partido Colorado'),
  "id_politico"] <- 4473

df_final[which(
  df_final$primer_apellido == 'VIVIAN' &
  df_final$primer_nombre == 'CHRISTIAN' &
  df_final$partido == 'Frente Amplio'),
  "id_politico"] <- 4474

df_final[which(
  df_final$primer_apellido == 'RODRIGUEZ' &
  df_final$primer_nombre == 'HYARA' &
  df_final$partido == 'Frente Amplio'),
  "id_politico"] <- 4475

df_final[which(
  df_final$primer_apellido == 'SOCA' &
  df_final$primer_nombre == 'ANGEL' &
  df_final$partido == 'Frente Amplio'),
  "id_politico"] <- 4476

df_final[which(
  df_final$primer_apellido == 'LOPEZ' &
  df_final$primer_nombre == 'JULIO' &
  df_final$partido == 'Frente Amplio'),
  "id_politico"] <- 4477

df_final[which(
  df_final$primer_apellido == 'CABRERA' &
  df_final$primer_nombre == 'ARMANDO' &
  df_final$partido == 'Frente Amplio'),
  "id_politico"] <- 4478

df_final[which(
  df_final$primer_apellido == 'TOLEDO' &
  df_final$primer_nombre == 'DARIO' &
  df_final$partido == 'Frente Amplio'),
  "id_politico"] <- 4479

df_final[which(
  df_final$primer_apellido == 'PERDOMO' &
  df_final$primer_nombre == 'DANIEL' &
  df_final$partido == 'Frente Amplio'),
  "id_politico"] <- 4480

df_final[which(
  df_final$primer_apellido == 'BARBOZA' &
  df_final$primer_nombre == 'DANY' &
  df_final$partido == 'Partido Nacional'),
  "id_politico"] <- 4481

df_final[which(
  df_final$primer_apellido == 'ACOSTA' &
  df_final$primer_nombre == 'MATIAS' &
  df_final$partido == 'Partido Nacional'),
  "id_politico"] <- 4482

df_final[which(
  df_final$primer_apellido == 'SILVEIRA' &
  df_final$primer_nombre == 'ELIO' &
  df_final$partido == 'Frente Amplio'),
  "id_politico"] <- 4483

df_final[which(
  df_final$primer_apellido == 'SILVEIRA' &
  df_final$primer_nombre == 'ELIO' &
  df_final$partido == 'Frente Amplio'),
  "id_politico"] <- 4483

df_final[which(
  df_final$primer_apellido == 'MORAES' &
  df_final$primer_nombre == 'MARIA' &
  df_final$partido == 'Frente Amplio'),
  "id_politico"] <- 4484

df_final[which(
  df_final$primer_apellido == 'ALVAREZ' &
  df_final$primer_nombre == 'NESTOR' &
  df_final$partido == 'Partido Nacional'),
  "id_politico"] <- 4485

df_final[which(
  df_final$primer_apellido == 'ALFARO' &
  df_final$primer_nombre == 'MARIO' &
  df_final$partido == 'Frente Amplio'),
  "id_politico"] <- 4486

df_final[which(
  df_final$primer_apellido == 'BERNANDO' &
  df_final$primer_nombre == 'JORGE' &
  df_final$partido == 'Frente Amplio'),
  "id_politico"] <- 4487

df_final[which(
  df_final$primer_apellido == 'SUAREZ' &
  df_final$primer_nombre == 'JAVIER' &
  df_final$partido == 'Partido Nacional'),
  "id_politico"] <- 4488

df_final[which(
  df_final$primer_apellido == 'CORDANO' &
  df_final$primer_nombre == 'ANA' &
  df_final$partido == 'Partido Verde Animalista'),
  "id_politico"] <- 4489

df_final[which(
  df_final$primer_apellido == 'MARTINEZ' &
  df_final$primer_nombre == 'DARIO' &
  df_final$partido == 'Partido Nacional'),
  "id_politico"] <- 4490

df_final[which(
  df_final$primer_apellido == 'DELGADO' &
  df_final$primer_nombre == 'JUAN' &
  df_final$partido == 'Partido Nacional'),
  "id_politico"] <- 4491

df_final[which(
  df_final$primer_apellido == 'FERNANDEZ' &
  df_final$primer_nombre == 'ARACIEL' &
  df_final$partido == 'Partido Colorado'),
  "id_politico"] <- 4492

df_final[which(
  df_final$primer_apellido == 'ALEGRE' &
  df_final$primer_nombre == 'MIGUEL' &
  df_final$partido == 'Partido Nacional'),
  "id_politico"] <- 4493

df_final[which(
  df_final$primer_apellido == 'IZA' &
  df_final$primer_nombre == 'RAFAEL' &
  df_final$partido == 'Partido Nacional'),
  "id_politico"] <- 4494

df_final[which(
  df_final$primer_apellido == 'RIVERA' &
  df_final$primer_nombre == 'JULIO' &
  df_final$partido == 'Partido Nacional'),
  "id_politico"] <- 4495

df_final[which(
  df_final$primer_apellido == 'BENTANCOUR' &
  df_final$segundo_apellido == 'FERREIRA' &
  df_final$partido == 'Partido Colorado'),
  "id_politico"] <- 4496

df_final[which(
  df_final$primer_apellido == 'DEL' &
  df_final$primer_nombre == 'MARIA' &
  df_final$segundo_apellido == 'VALLE SILVERA'),
  "id_politico"] <- 4497

df_final[which(
  df_final$id_politico == 4497),
  "primer_apellido"] <- 'DEL VALLE'

df_final[which(
  df_final$id_politico == 3880),
  "primer_apellido"] <- 'DEL C. SUAREZ'

df_final[which(
  df_final$id_politico == 4020 &
  df_final$primer_apellido == 'PEREIRA'),
  "id_politico"] <- 4022

df_final[which(
  df_final$id_politico == 4022 &
  df_final$circunscripcion == 'Montevideo'),
  "eliminada"] <- 1

df_final[which(
  df_final$primer_nombre == 'MARIA' &
  df_final$primer_apellido == 'PEREIRA'),
  "id_politico"] <- 4498

df_final[which(
  df_final$primer_apellido == 'AMARAL' &
  df_final$primer_nombre == 'GERARDO' &
  df_final$partido == 'Frente Amplio'),
  "id_politico"] <- 4499

df_final[which(
  df_final$id_politico == 4309),
  "circunscripcion"] <- 'Maldonado'

df_final[which(
  df_final$primer_apellido == 'RODRIGUEZ' &
    df_final$segundo_apellido == 'GROLERO' &
    df_final$partido == 'Partido Nacional'),
  "id_politico"] <- 4500

df_final[which(
  df_final$primer_nombre == 'JORGE' &
  df_final$primer_apellido == 'BARREIRO' &
  df_final$partido == 'Partido Colorado'),
  "id_politico"] <- 4501

max(df_final$id_politico)
df_final %>% filter(!is.na(fecha_nac)) %>% count()

df_final <- read_csv("df_final.csv")

library(DBI)
library(RPostgres)
library(RPostgreSQL)

readRenviron("~/.Renviron")

usuario  <- Sys.getenv("DB_USER")
password <- Sys.getenv("DB_PASS")
host     <- Sys.getenv("DB_HOST")
dbname   <- Sys.getenv("DB_NAME")

# Conectar
con <- tryCatch({
  dbConnect(
    Postgres(),
    dbname   = Sys.getenv("DB_NAME"),
    host     = Sys.getenv("DB_HOST"),
    user     = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASS"),
    port     = 5432
  )
}, error = function(e) {
  message("❌ No se pudo conectar a la base de datos: ", e$message)
  NULL
})

if (!is.null(con)) {
  message("✅ Conexión exitosa a la base de datos")
}

#dbWriteTable(con, "fact_politicos_PASANTIA_23_09", df_final, overwrite = TRUE, row.names = FALSE)

################################################################################################################
politicos <- dbGetQuery(con, 'SELECT * FROM "public"."fact_politicos_PASANTIA_23_09"')

df_final <- dbGetQuery(con, 'SELECT * FROM "public"."fact_politicos_PASANTIA_23_09"')

df_final$ed_asumir <-trunc((df_final$fecha_nac %--% df_final$fecha_inicio) / years(1))

### colocar na a las fechas de nacimiento para las personas tienen menos de 21 años al asumir el cargo
df_final <- df_final %>% mutate(fecha_nac = if_else(is.na(ed_asumir) | ed_asumir > 21, fecha_nac, as.Date(NA)))
df_final$ed_asumir <-trunc((df_final$fecha_nac %--% df_final$fecha_inicio) / years(1))


legislaturas <- legislaturas %>% select(legislatura, fecha_inicio, fecha_fin)
legislaturas <- legislaturas %>% mutate(legislatura = as.double(legislatura))
legislaturas <- legislaturas %>% rename(fecha_inicio_l = fecha_inicio, fecha_fin_l = fecha_fin)

df_final <- df_final %>% left_join(legislaturas, by = "legislatura")

df_final <- df_final %>%
  mutate(fecha_intermedia = fecha_inicio_l + (fecha_fin_l - fecha_inicio_l) / 2)

df_final$ed_asumir_1 <-trunc((df_final$fecha_nac %--% df_final$fecha_intermedia) / years(1))

df_final <- df_final %>% mutate(ed_asumir_1 = ifelse(is.na(fecha_inicio), ed_asumir_1, NA))

df_final <- df_final %>%
  mutate(ed_asumir_1 = ifelse(ed_asumir_1 < 22 | ed_asumir_1 > 85, NA, ed_asumir_1))

df_final <- df_final %>% mutate(edad_asumir = ifelse(is.na(ed_asumir), ed_asumir_1, ed_asumir))

# periodos de legislaturas

df_final <- df_final %>% mutate(legislaturas_agrupadas = case_when(legislatura <= 31 ~ "1902-1933",
                                                                     legislatura > 31 & legislatura <= 41 ~ "1934-1973",
                                                                     legislatura > 41 ~ "1985-2020"))


################### CORRECCIONES EN LOS LEGISLADORES DE LAS LEGISLATURAS ###############################
######## Correción de la legislatura 47 (que marca a todos los legisladores como titulares)

politicos <- politicos %>% mutate(eliminada = 0)

leg47_biblio <- read.csv("leg47_biblio.csv", encoding = "utf-8")
leg47_biblio <- leg47_biblio %>% filter(Cargo %in%c("DIPUTADO", "SENADOR","SENADORA", "DIPUTADA"))

# --- Lógica para apellidos ---
# Detectar si empieza con "DE " y en ese caso tomar "DE + siguiente palabra" como Apellido1
leg47_biblio <- leg47_biblio %>%
  mutate(
    Apellido1 = ifelse(str_detect(Apellidos, "^(DE|DA)"),
                       word(Apellidos, 1, 2),     # DE + segunda palabra
                       word(Apellidos, 1)),       # si no empieza con DE, tomar solo la primera
    Apellido2 = ifelse(str_detect(Apellidos, "^(DE|DA)"),
                       word(Apellidos, 3),        # lo que venga después de "DE X"
                       word(Apellidos, 2))        # segundo apellido normal
  )


leg47_biblio <- leg47_biblio %>% select(-c(primer_apellido, segundo_apellido)) %>% rename('primer_apellido'='Apellido1',
                                                                          'segundo_apellido' ='Apellido2')
leg47_biblio <- leg47_biblio %>% select(-c(Nombre, Nombres, Apellidos))

## SENADORES
# analisis de calidad del dato de legisladores
leg47_biblio_sen_tit <- leg47_biblio %>% filter(Condición == 'Titular' & Cargo %in%c('SENADOR','SENADORA'))

# conteo (tengo mas senadores titulares que bancas)
leg47_biblio_sen_tit %>% distinct(X) %>% count()
leg47_senador %>% count()

leg47_biblio_sen_tit %>% filter(Condición == 'Titular') %>% count() # sigue habiendo mas senadores titulares

## marco a todos los senadores como suplentes (base UMAD)
leg47_senador <- leg47_senador %>% mutate(status = "Suplente")

################### OBTENGO DEL DW TODOS LOS ID POLITICOS Y NOMBRES Y APELLIDOS #########################

ids_politicos <- dbGetQuery(
  con,
  "select distinct id_politico,
                  primer_nombre,
                  primer_apellido,
                  partido,
                  fecha_nac
   from public.\"fact_politicos_PASANTIA_23_09\"
   where legislaturas_agrupadas like '%1985-2020%' and eliminada = 0"
)


leg47_biblio$Fecha.nacimiento <- str_squish(leg47_biblio$Fecha.nacimiento)

leg47_biblio <- leg47_biblio %>% mutate(
    Fecha.nacimiento = na_if(Fecha.nacimiento, ""))

leg47_biblio <- leg47_biblio %>%
  mutate(fecha_nac = dmy(Fecha.nacimiento))

pegado <- leg47_biblio %>% left_join(ids_politicos, by = c("primer_apellido", "primer_nombre","fecha_nac"))

pegado_2 <-pegado %>% select(Partido,primer_nombre, segundo_nombre,primer_apellido,segundo_apellido,Fecha.nacimiento,
                             id_politico, no_esta) %>%   distinct()

pegado_2 <- read.csv('pegado2.csv')
pegado_2 <- pegado_2[-(179),]
pegado_2 <- pegado_2 %>% filter(id_politico != "4252" | is.na(id_politico))
pegado_2 <- pegado_2 %>% filter(id_politico != "3816" | is.na(id_politico))
pegado_2 <- pegado_2 %>% filter(id_politico != "3812" | is.na(id_politico))
pegado_2 <- pegado_2 %>% filter(id_politico != "3979" | is.na(id_politico))
pegado_final <- leg47_biblio %>% left_join(pegado_2, by = c("primer_apellido", "primer_nombre","Fecha.nacimiento")) %>%
  select(-ends_with(".y"))

pegado_final$Partido.x <- gsub('FRENTE AMPLIO', 'Frente Amplio',pegado_final$Partido.x)
pegado_final$Partido.x <- gsub('PARTIDO NACIONAL', 'Partido Nacional',pegado_final$Partido.x)
pegado_final$Partido.x <- gsub('PARTIDO COLORADO', 'Partido Colorado',pegado_final$Partido.x)
pegado_final$Partido.x <- gsub('PARTIDO INDEPENDIENTE', 'Partido Independiente',pegado_final$Partido.x)
pegado_final <- pegado_final %>% mutate(sexo = ifelse(Cargo %in%c('SENADOR', 'DIPUTADO'), 1, 0))
pegado_final <- pegado_final %>%
  mutate(sexo = 1L - as.integer(str_ends(primer_nombre, regex("a$", ignore_case = TRUE))))
pegado_final$Cargo <- gsub('DIPUTADO', 'Diputado',pegado_final$Cargo)
pegado_final$Cargo <- gsub('DIPUTADA', 'Diputado',pegado_final$Cargo)
pegado_final$Cargo <- gsub('SENADOR', 'Senador',pegado_final$Cargo)
pegado_final$Cargo <- gsub('SENADORA', 'Senador',pegado_final$Cargo)
pegado_final <- pegado_final %>% rename(status=Condición, segundo_nombre = segundo_nombre.x,
                        segundo_apellido = segundo_apellido.x, partido = Partido.x)
pegado_final <- pegado_final %>% select(-c(Fecha.nacimiento, X))
pegado_final[pegado_final$primer_apellido == 'ABT' &
             pegado_final$primer_nombre == 'ANDRES',
             "id_politico"] <- 2970
pegado_final <- pegado_final %>% mutate(no_esta = ifelse(!is.na(id_politico), 0, 1))
pegado_final <- pegado_final %>% mutate(
  segundo_nombre = na_if(segundo_nombre, ""),
  segundo_apellido = na_if(segundo_apellido, ""),
  primer_apellido = na_if(primer_apellido, ""),
  primer_nombre = na_if(primer_nombre, ""))
pegado_final <- pegado_final %>% filter(!is.na(primer_apellido))
pegado_final <- pegado_final %>% mutate(legislatura = 47)
pegado_final <- pegado_final %>% rename(periodo = Periodo, cargo = Cargo, inicio = Inicio, fin = Fin)
circun47 <- leg47 %>% select(id_politico, cargo, circunscripcion) %>% distinct()
pegado_final <- pegado_final %>% left_join(circun47, by = c("id_politico", "cargo"))
pegado_final <- pegado_final %>% mutate(fecha_inicio_l = '2010-02-15',
                                        fecha_fin_l = '2015-02-14', legislaturas_agrupadas = '1985-2020')
pegado_final <- pegado_final %>% select(primer_apellido, segundo_apellido, primer_nombre, segundo_nombre,
                        id_politico, partido, inicio, fin, legislatura, cargo, status,
                        circunscripcion, sexo, fecha_nac, fecha_inicio_l, fecha_fin_l, legislaturas_agrupadas)

# MARCO SUPLENTE SI COMENZO LUEGO DEL 2010
pegado_final <- pegado_final %>%
  mutate(status = if_else(inicio > 2010 & inicio < 2015, 'Suplente', status))

# correccion a mano SENADORES (colocar los suplentes que por error en la tabla del parlamento aparecen como titulares)

pegado_final[pegado_final$primer_nombre == 'ALFREDO'&
             pegado_final$primer_apellido == 'SOLARI',
                     "status"] <- 'Suplente'

pegado_final[pegado_final$primer_nombre == 'WILSON'&
             pegado_final$primer_apellido == 'SANABRIA',
                     "status"] <- 'Suplente'

pegado_final[pegado_final$primer_nombre == 'ENRIQUE'&
             pegado_final$primer_apellido == 'PINTADO',
                     "status"] <- 'Suplente'

pegado_final[pegado_final$primer_nombre == 'SUSANA'&
             pegado_final$segundo_nombre == 'ELIDA',
                     "primer_apellido"] <- 'DALMAS'

pegado_final[pegado_final$primer_nombre == 'EBER'&
             pegado_final$segundo_apellido == 'VAZQUEZ',
                     "primer_apellido"] <- 'DA ROSA VAZQUEZ'

# Generacion de id

pegado_final <- pegado_final %>% mutate(agregado = ifelse(is.na(id_politico), 1, 0))

sin_id <- pegado_final %>% filter(is.na(id_politico))
con_id <- pegado_final %>% filter(!is.na(id_politico))  

sin_id <- sin_id %>%
  mutate(
key = paste(primer_nombre, primer_apellido, fecha_nac, sep = "|"),
id_politico = dense_rank(key) + 4516L
) %>%
  select(-key)   

pegado_final <- rbind(con_id, sin_id)
  
# correccion a mano DIPUTADOS (colocar los suplentes que por error en la tabla del
# parlamento aparecen como titulares)

dip_titu <- pegado_final %>% filter(cargo == "Diputado" & status == 'Titular')
pegado_final <- pegado_final %>% filter(cargo == "Senador" | (cargo == 'Diputado' & status == 'Suplente'))

# correcciones a mano diputados titulares (corrijo)

dip_titu[dip_titu$primer_nombre == 'LIDIA'&
         dip_titu$primer_apellido == 'VILLALBA',
         "status"] <- 'Suplente'

dip_titu[dip_titu$primer_nombre == 'FABRICIO'&
         dip_titu$primer_apellido == 'MARIONE',
         "status"] <- 'Suplente'

dip_titu[dip_titu$primer_nombre == 'ANA'&
         dip_titu$primer_apellido == 'LIMA',
         "status"] <- 'Suplente'

dip_titu[dip_titu$primer_nombre == 'MARIO'&
         dip_titu$primer_apellido == 'GUERRERO',
         "status"] <- 'Suplente'

dip_titu[dip_titu$primer_nombre == 'DANIEL'&
         dip_titu$primer_apellido == 'GONZALEZ',
         "status"] <- 'Suplente'

dip_titu[dip_titu$primer_nombre == 'MARCELO'&
         dip_titu$primer_apellido == 'BISTOLFI',
         "status"] <- 'Suplente'

dip_titu[dip_titu$primer_nombre == 'JOSE'&
         dip_titu$primer_apellido == 'AROCENA',
         "status"] <- 'Suplente'

dip_titu[dip_titu$primer_nombre == 'MIGUEL'&
         dip_titu$primer_apellido == 'OTEGUI',
         "status"] <- 'Suplente'

dip_titu[dip_titu$primer_nombre == 'RICHARD'&
         dip_titu$primer_apellido == 'SANDER',
         "status"] <- 'Suplente'

dip_titu[dip_titu$primer_nombre == 'BERTA'&
         dip_titu$primer_apellido == 'SANSEVERINO',
         "status"] <- 'Suplente'

dip_titu[dip_titu$primer_nombre == 'JUAN'&
         dip_titu$primer_apellido == 'VAZQUEZ',
         "status"] <- 'Suplente'

dip_titu[dip_titu$primer_nombre == 'MARIA'&
         dip_titu$primer_apellido == 'EGUILUZ',
         "status"] <- 'Suplente'

dip_titu[dip_titu$primer_nombre == 'LUIS'&
         dip_titu$primer_apellido == 'BOTANA',
         "status"] <- 'Suplente'

dip_titu[dip_titu$primer_nombre == 'GUILLERMO'&
         dip_titu$primer_apellido == 'BESOZZI',
         "status"] <- 'Suplente'

dip_titu[dip_titu$primer_nombre == 'OSCAR'&
         dip_titu$primer_apellido == 'ANDRADE',
         "status"] <- 'Suplente'

pegado_final <- rbind(dip_titu, pegado_final)

pegado_final <- pegado_final %>%
  mutate(fecha_inicio_l = as.Date(fecha_inicio_l), fecha_fin_l = as.Date(fecha_fin_l),
         fecha_intermedia = fecha_inicio_l + (fecha_fin_l - fecha_inicio_l) / 2)

titular <- pegado_final %>% filter(status == "Titular")
no_titular <- pegado_final %>% filter(status != "Titular")

titular$ed_asumir <- trunc((titular$fecha_nac %--% titular$fecha_inicio_l) / years(1))
no_titular$ed_asumir_1 <- trunc((no_titular$fecha_nac %--% no_titular$fecha_intermedia) / years(1))

titular <- titular %>% mutate(ed_asumir_1 = NA)
no_titular <- no_titular %>% mutate(ed_asumir = NA)
pegado_final <- rbind(titular, no_titular)
pegado_final <- pegado_final %>% mutate(edad_asumir = ifelse(is.na(ed_asumir), ed_asumir_1, ed_asumir))

leg47_biblio[leg47_biblio$primer_nombre == 'MARCELO'&
             leg47_biblio$primer_apellido == 'BISTOLFI',
             "status"] <- 'Titular'

leg47_biblio[leg47_biblio$primer_nombre == 'JODAMI'&
             leg47_biblio$primer_apellido == 'MARTINEZ',
             "status"] <- 'Suplente'

leg47_biblio[leg47_biblio$primer_nombre == 'LUIS'&
               leg47_biblio$primer_apellido == 'BOTANA',
             "status"] <- 'Suplente'

leg47_biblio[leg47_biblio$primer_nombre == 'CARLOS'&
             leg47_biblio$primer_apellido == 'ENCISO',
             "status"] <- 'Suplente'

leg47_biblio %>% filter(status == 'Titular' & cargo == 'Senador') %>% 
  group_by(partido) %>% distinct(id_politico) %>%  count()

leg47_biblio <- leg47_biblio %>%
  select(
    primer_apellido, segundo_apellido, primer_nombre, segundo_nombre,
    id_politico, partido, inicio, fin, legislatura, cargo, status,
    circunscripcion, sexo, fecha_nac, fecha_inicio_l, fecha_fin_l,
    fecha_intermedia, ed_asumir_1, ed_asumir, edad_asumir, agregado,
    legislaturas_agrupadas
  )

# Subir respetando el orden
#dbWriteTable(
#  con,
#  Id(schema = "leg_biblioteca_parlamento", table = "fact_legisladores_biblio_parla"),
#  leg47_biblio,
#  overwrite = TRUE,
#  row.names = FALSE
#)


# subo un hub politicos
hub_politicos <- pegado_final %>% select(primer_apellido, segundo_apellido, primer_nombre,
                        segundo_nombre, fecha_nac, id_politico, agregado) %>% distinct()

hub_politicos <- hub_politicos %>% mutate(id_fuente = 2)

dim_fuente <- data.frame(id_fuente = c(1, 2),
           desc = c('UMAD', 'Biblioteca Parlamento'))

#dbWriteTable(con, Id(schema = "public", table = "hub_politicos")
#             , hub_politicos, overwrite = TRUE, row.names = FALSE)


################# levanto legislatura 48

leg48_biblio <- read.csv('leg48_biblioteca.csv')

leg48_biblio <- leg48_biblio %>% mutate(
  segundo_nombre = na_if(segundo_nombre, ""),
  segundo_apellido = na_if(segundo_apellido, ""),
  primer_apellido = na_if(primer_apellido, ""),
  primer_nombre = na_if(primer_nombre, ""))



# pegado del id politico con hub de biblio

### PRIMER PEGADO: nombre, apellido y fecha de nacimiento (base biblioteca)
ids_politicos <- dbGetQuery(
  con,
  "select distinct id_politico,
                  primer_nombre,
                  primer_apellido,
                  fecha_nac
   from public.\"hub_politicos\"
"
)

leg48_biblio <- leg48_biblio %>% mutate(fecha_nac = as.Date(fecha_nac))

leg48_biblio <- leg48_biblio %>% left_join(ids_politicos, by =c('primer_nombre',
                                                                'primer_apellido', 'fecha_nac'))

### SEGUNDO PEGADO: nombre, apellido y fecha de nacimiento
ids_politicos <- dbGetQuery(
  con,
  "select distinct id_politico,
                  primer_nombre,
                  primer_apellido,
                  fecha_nac
   from public.\"fact_politicos_PASANTIA_23_09\"
   where legislaturas_agrupadas like '%1985-2020%' and eliminada = 0"
)

leg48_biblio_id <- leg48_biblio %>% filter(!is.na(id_politico))
leg48_biblio_sin_id <- leg48_biblio %>% filter(is.na(id_politico)) %>% select(-id_politico)
ids_politicos <- ids_politicos %>% filter(!id_politico %in%c(4730, 4053))

leg48_biblio_sin_id <- leg48_biblio_sin_id %>% left_join(ids_politicos, by =c('primer_nombre',
                                                        'primer_apellido', 'fecha_nac'))
  
leg48_biblio_id2 <- leg48_biblio_sin_id %>% filter(!is.na(id_politico))
leg48_biblio_sin_id <- leg48_biblio_sin_id %>% filter(is.na(id_politico)) %>% select(-id_politico)
leg48_biblio_sin_id <- leg48_biblio_sin_id %>% filter(X != 259)


### TERCER PEGADO: apellido, nombre y partido
ids_politicos <- dbGetQuery(
  con,
  "select distinct id_politico,
                  primer_nombre,
                  primer_apellido,
                  partido,
                  fecha_nac
   from public.\"fact_politicos_PASANTIA_23_09\"
   where legislaturas_agrupadas like '%1985-2020%' and eliminada = 0"
)

leg48_biblio_sin_id <- leg48_biblio_sin_id %>% left_join(ids_politicos, by =c('primer_nombre','partido',
                                                       'primer_apellido')) %>% select(-ends_with(".y")) %>% 
  rename(fecha_nac = fecha_nac.x)

leg48_biblio_sin_id[leg48_biblio_sin_id$primer_nombre == 'MARIA'&
                    leg48_biblio_sin_id$primer_apellido == 'LUSTEMBERG',
                    "id_politico"] <- 2344

leg48_biblio_id3 <- leg48_biblio_sin_id %>%
  filter(!is.na(id_politico))

leg48_biblio_sin_id <- leg48_biblio_sin_id %>%
  filter(is.na(id_politico))

leg48_biblio_sin_id <- leg48_biblio_sin_id %>%
  mutate(
    key = paste(primer_nombre, primer_apellido, fecha_nac, sep = "|"),
    id_politico = dense_rank(key) + 4729L
  ) %>%
  select(-key)   

leg48_biblio_sin_id <- leg48_biblio_sin_id %>% mutate(agregado = 1)
leg48_biblio_ag <- rbind(leg48_biblio_id, leg48_biblio_id2, leg48_biblio_id3)
leg48_biblio_ag <- leg48_biblio_ag %>% mutate(agregado = 0)
pegado_final <- rbind(leg48_biblio_ag, leg48_biblio_sin_id)
pegado_final <- pegado_final %>% mutate(id_fuente = 2)

# subo un hub politicos
hub_politicos <- pegado_final %>% select(primer_apellido, segundo_apellido, primer_nombre,
                                         segundo_nombre, fecha_nac, id_politico, agregado,                                         id_fuente) %>% distinct()

ids_politicos <- dbGetQuery(
  con,
  "select distinct
                  primer_nombre,
                  primer_apellido,
                  fecha_nac
   from public.\"hub_politicos\"
"
)

nuevos <- anti_join(hub_politicos, ids_politicos,
                    by=c("primer_nombre", "primer_apellido",
                         "fecha_nac"))

#dbWriteTable(con, Id(schema = "public", table = "hub_politicos")
#             , nuevos, append = TRUE,row.names = FALSE)


## legislatura 48

pegado_final %>% filter(cargo == 'Senador' & status == 'Titular') %>% distinct(id_politico) %>% count()

# correccion a mano DIPUTADOS (colocar los suplentes que por error en la tabla del
# parlamento aparecen como titulares)

dip_titu <- pegado_final %>% filter(cargo == "Diputado" & status == 'Titular')
pegado_final <- pegado_final %>% filter(cargo == "Senador" | (cargo == 'Diputado' & status == 'Suplente'))

pegado_final[pegado_final$primer_nombre == 'CARLOS'&
             pegado_final$primer_apellido == 'CAMY',
             "status"] <- 'Suplente'

pegado_final[pegado_final$primer_nombre == 'DANIEL'&
             pegado_final$primer_apellido == 'GARIN',
             "status"] <- 'Suplente'

pegado_final[pegado_final$primer_nombre == 'JOSE'&
             pegado_final$primer_apellido == 'FALERO',
             "status"] <- 'Suplente'

pegado_final[pegado_final$primer_nombre == 'JUAN'&
            pegado_final$primer_apellido == 'CASTILLO',
             "status"] <- 'Suplente'

pegado_final[pegado_final$primer_nombre == 'NELSON'&
             pegado_final$primer_apellido == 'PENA',
             "status"] <- 'Suplente'

pegado_final[pegado_final$primer_nombre == 'JUAN'&
             pegado_final$primer_apellido == 'CASTILLO',
             "status"] <- 'Suplente'

pegado_final[pegado_final$primer_nombre == 'ENRIQUE'&
             pegado_final$primer_apellido == 'PINTADO',
             "status"] <- 'Suplente'

pegado_final[pegado_final$primer_nombre == 'MICHELLE'&
             pegado_final$primer_apellido == 'SUAREZ',
             "status"] <- 'Suplente'

pegado_final[pegado_final$primer_nombre == 'IVONNE'&
             pegado_final$primer_apellido == 'PASSADA',
             "status"] <- 'Suplente'

pegado_final[pegado_final$primer_nombre == 'CHARLES'&
             pegado_final$primer_apellido == 'CARRERA',
             "status"] <- 'Suplente'

pegado_final[pegado_final$primer_nombre == 'JOSE'&
             pegado_final$primer_apellido == 'CARDOSO',
             "status"] <- 'Suplente'

pegado_final %>% filter(cargo == 'Senador' & status == 'Titular') %>% distinct(id_politico) %>% count()
dip_titu %>% filter(cargo == 'Diputado' & status == 'Titular') %>% distinct(id_politico) %>% count()

## diputados

dip_titu[dip_titu$primer_nombre == 'JOSE'&
         dip_titu$primer_apellido == 'OLANO',
         "status"] <- 'Suplente'

dip_titu[dip_titu$primer_nombre == 'MABEL'&
         dip_titu$primer_apellido == 'QUINTELA',
         "status"] <- 'Suplente'

dip_titu[dip_titu$primer_nombre == 'OSCAR'&
         dip_titu$primer_apellido == 'VIERA',
         "status"] <- 'Suplente'

dip_titu[dip_titu$primer_nombre == 'CATALINA'&
         dip_titu$primer_apellido == 'CORREA',
         "status"] <- 'Suplente'

dip_titu[dip_titu$primer_nombre == 'ALEJO'&
         dip_titu$primer_apellido == 'UMPIERREZ',
         "status"] <- 'Suplente'

dip_titu[dip_titu$primer_nombre == 'NIBIA'&
         dip_titu$primer_apellido == 'REISCH',
         "status"] <- 'Suplente'

dip_titu[dip_titu$primer_nombre == 'GONZALO'&
         dip_titu$primer_apellido == 'NOVALES',
         "status"] <- 'Suplente'

dip_titu[dip_titu$primer_nombre == 'GERARDO'&
         dip_titu$primer_apellido == 'NUNEZ',
         "status"] <- 'Titular'

dip_titu[dip_titu$primer_nombre == 'BETTIANA'&
         dip_titu$primer_apellido == 'DIAZ',
         "status"] <- 'Suplente'

dip_titu[dip_titu$primer_nombre == 'JORGE'&
         dip_titu$primer_apellido == 'MERONI',
         "status"] <- 'Suplente'

dip_titu[dip_titu$primer_nombre == 'RICHARD'&
         dip_titu$primer_apellido == 'CHARAMELO',
         "status"] <- 'Suplente'

dip_titu[dip_titu$primer_nombre == 'DARCY'&
         dip_titu$primer_apellido == 'DE LOS SANTOS',
         "status"] <- 'Suplente'

dip_titu[dip_titu$primer_nombre == 'HECTOR'&
         dip_titu$primer_apellido == 'GIANOLI',
         "status"] <- 'Suplente'

dip_titu[dip_titu$primer_nombre == 'BENJAMIN'&
         dip_titu$primer_apellido == 'IRAZABAL',
         "status"] <- 'Suplente'

dip_titu[dip_titu$primer_nombre == 'CLAUDIA'&
         dip_titu$primer_apellido == 'HUGO',
         "status"] <- 'Suplente'

dip_titu[dip_titu$primer_nombre == 'WILSON'&
         dip_titu$primer_apellido == 'BENTANCOR',
         "status"] <- 'Suplente'

dip_titu[dip_titu$primer_nombre == 'ELISABETH'&
         dip_titu$primer_apellido == 'ARRIETA',
         "status"] <- 'Suplente'

dip_titu[dip_titu$primer_nombre == 'FELIPE'&
         dip_titu$primer_apellido == 'MICHELINI',
         "status"] <- 'Suplente'

dip_titu[dip_titu$primer_nombre == 'MARIA'&
         dip_titu$primer_apellido == 'SANTALLA',
         "status"] <- 'Suplente'

dip_titu[dip_titu$primer_nombre == 'OSCAR'&
         dip_titu$primer_apellido == 'VIERA',
         "status"] <- 'Suplente'

dip_titu[dip_titu$primer_nombre == 'LUIS'&
         dip_titu$primer_apellido == 'BOTANA',
         "status"] <- 'Suplente'

dip_titu[dip_titu$primer_nombre == 'JAVIER'&
         dip_titu$primer_apellido == 'GARCIA',
         "status"] <- 'Suplente'

dip_titu[dip_titu$primer_nombre == 'CARLOS'&
         dip_titu$primer_apellido == 'ENCISO',
         "status"] <- 'Suplente'

dip_titu[dip_titu$primer_nombre == 'JAIME'&
         dip_titu$primer_apellido == 'TROBO',
         "status"] <- 'Suplente'

dip_titu[dip_titu$primer_nombre == 'ALVARO'&
         dip_titu$primer_apellido == 'DELGADO',
         "status"] <- 'Suplente'

dip_titu %>% filter(cargo == 'Diputado' & status == 'Titular') %>% group_by(partido) %>%  distinct(id_politico) %>% count()
dip_titu %>% filter(cargo == 'Diputado' & status == 'Titular') %>%  distinct(id_politico) %>% count()
# MIRAR TITULARES POR PARTIDO
dip_titu %>% filter(cargo == 'Diputado' & status == 'Titular' & partido == 'Partido Colorado') %>%
  distinct(id_politico, primer_nombre, primer_apellido)

pegado_final <- rbind(dip_titu, pegado_final)

pegado_final <- pegado_final %>%
  mutate(fecha_inicio_l = as.Date(fecha_inicio_l), fecha_fin_l = as.Date(fecha_fin_l),
         fecha_intermedia = fecha_inicio_l + (fecha_fin_l - fecha_inicio_l) / 2)

titular <- pegado_final %>% filter(status == 'Titular')
titular$ed_asumir <- trunc((titular$fecha_nac %--% titular$fecha_inicio_l) / years(1))
no_titular <- pegado_final %>% filter(status != 'Titular')
no_titular$ed_asumir_1 <- trunc((no_titular$fecha_nac %--% no_titular$fecha_intermedia) / years(1))

titular <- titular %>% mutate(ed_asumir_1 = NA)
no_titular <- no_titular %>% mutate(ed_asumir = NA)
pegado_final <- rbind(titular, no_titular)
pegado_final <- pegado_final %>% mutate(edad_asumir = ifelse(is.na(ed_asumir), ed_asumir_1, ed_asumir))
pegado_final <- pegado_final %>% mutate(agregado = ifelse(id_politico > 4730, 1, 0))
pegado_final <- pegado_final %>% select(-c(nombre, id_fuente))

pegado_final[pegado_final$primer_nombre == 'GUILLERMO'&
             pegado_final$primer_apellido == 'BESOZZI',
            "status"] <- 'Suplente'

pegado_final %>% filter(status == 'Titular' & cargo == 'Senador') %>% group_by(partido) %>% 
  distinct(id_politico) %>% count()

# PEGAR circunscripcion

ids_politicos <- dbGetQuery(
  con,
  "
  SELECT fpp.id_politico,
         fpp.circunscripcion, fpp.cargo
  FROM public.\"fact_politicos_PASANTIA_23_09\" fpp
  WHERE fpp.eliminada = 0
    AND fpp.legislatura = 48
    AND fpp.cargo = 'Diputado'
    AND fpp.status = 'Titular'
    AND fpp.circunscripcion IS NOT NULL
  "
)

pegado_final <- pegado_final %>% left_join(ids_politicos, by = c("id_politico", "cargo"))

pegado_final <- pegado_final %>% select(-circunscripcion.x) %>% rename(circunscripcion = circunscripcion.y)

#dbWriteTable(con, Id(schema = "leg_biblioteca_parlamento", table = "fact_legisladores_biblio_parla")
#             , pegado_final, append = TRUE, row.names = FALSE)

################# levanto legislatura 49

leg49_biblio <- read.csv('leg49_biblioteca.csv')

leg49_biblio <- leg49_biblio %>% mutate(
  segundo_nombre = na_if(segundo_nombre, ""),
  segundo_apellido = na_if(segundo_apellido, ""),
  primer_apellido = na_if(primer_apellido, ""),
  primer_nombre = na_if(primer_nombre, ""))

# pegado del id politico con hub de biblio

### PRIMER PEGADO: nombre, apellido y fecha de nacimiento (base biblioteca)
ids_politicos <- dbGetQuery(
  con,
  "select distinct id_politico,
                  primer_nombre,
                  primer_apellido,
                  fecha_nac
   from public.\"hub_politicos\"
"
)

leg49_biblio <- leg49_biblio %>% mutate(fecha_nac = as.Date(fecha_nac))

leg49_biblio <- leg49_biblio %>% left_join(ids_politicos, by =c('primer_nombre',
                                                                'primer_apellido', 'fecha_nac'))

### SEGUNDO PEGADO: nombre, apellido y fecha de nacimiento
ids_politicos <- dbGetQuery(
  con,
  "select distinct id_politico,
                  primer_nombre,
                  primer_apellido,
                  fecha_nac
   from public.\"fact_politicos_PASANTIA_23_09\"
   where legislaturas_agrupadas like '%1985-2020%' and eliminada = 0"
)

leg49_biblio_id <- leg49_biblio %>% filter(!is.na(id_politico))
leg49_biblio_sin_id <- leg49_biblio %>% filter(is.na(id_politico)) %>% select(-id_politico)

leg49_biblio_sin_id <- leg49_biblio_sin_id %>% left_join(ids_politicos, by =c('primer_nombre',
                                                                              'primer_apellido', 'fecha_nac'))

leg49_biblio_id2 <- leg49_biblio_sin_id %>% filter(!is.na(id_politico))
leg49_biblio_sin_id <- leg49_biblio_sin_id %>% filter(is.na(id_politico)) %>% select(-id_politico)

### TERCER PEGADO: apellido, nombre y partido
ids_politicos <- dbGetQuery(
  con,
  "select distinct id_politico,
                  primer_nombre,
                  primer_apellido,
                  partido,
                  fecha_nac
   from public.\"fact_politicos_PASANTIA_23_09\"
   where legislaturas_agrupadas like '%1985-2020%' and eliminada = 0"
)

leg49_biblio_sin_id <- leg49_biblio_sin_id %>% left_join(ids_politicos, by =c('primer_nombre','partido',
                                                                              'primer_apellido')) %>% select(-ends_with(".y")) %>% 
  rename(fecha_nac = fecha_nac.x)


leg49_biblio_sin_id[leg49_biblio_sin_id$primer_nombre == 'NORMA'&
                    leg49_biblio_sin_id$primer_apellido == 'KECHICHIAN',
                    "id_politico"] <- 2370

leg49_biblio_sin_id[leg49_biblio_sin_id$primer_nombre == 'ANA'&
                    leg49_biblio_sin_id$primer_apellido == 'COSSE',
                    "id_politico"] <- 2350

leg49_biblio_sin_id[leg49_biblio_sin_id$primer_nombre == 'ANA'&
                    leg49_biblio_sin_id$primer_apellido == 'ARISMENDI',
                    "id_politico"] <- 99

leg49_biblio_sin_id[leg49_biblio_sin_id$primer_nombre == 'ANA'&
                    leg49_biblio_sin_id$primer_apellido == 'CAIRO',
                    "id_politico"] <- 2345

leg49_biblio_id3 <- leg49_biblio_sin_id %>%
  filter(!is.na(id_politico))

leg49_biblio_sin_id <- leg49_biblio_sin_id %>%
  filter(is.na(id_politico))

leg49_biblio_sin_id <- leg49_biblio_sin_id %>%
  mutate(
    key = paste(primer_nombre, primer_apellido, fecha_nac, sep = "|"),
    id_politico = dense_rank(key) + 4871L
  ) %>%
  select(-key)   

leg49_biblio_sin_id <- leg49_biblio_sin_id %>% mutate(agregado = 1)
leg49_biblio_ag <- rbind(leg49_biblio_id, leg49_biblio_id2, leg49_biblio_id3)
leg49_biblio_ag <- leg49_biblio_ag %>% mutate(agregado = 0)
pegado_final <- rbind(leg49_biblio_ag, leg49_biblio_sin_id)
pegado_final <- pegado_final %>% mutate(id_fuente = 2)

# subo un hub politicos
hub_politicos <- pegado_final %>% select(primer_apellido, segundo_apellido, primer_nombre,
                                         segundo_nombre, fecha_nac, id_politico, agregado,id_fuente) %>% distinct()
ids_politicos <- dbGetQuery(
  con,
  "select distinct
                  primer_nombre,
                  primer_apellido,
                  fecha_nac
   from public.\"hub_politicos\"
"
)

nuevos <- anti_join(hub_politicos, ids_politicos,
                    by=c("primer_nombre", "primer_apellido",
                         "fecha_nac"))

nuevos[nuevos$segundo_apellido == 'VIDAL'&
       nuevos$primer_apellido == 'LARRANAGA',
       "id_politico"] <- 5087

#dbWriteTable(con, Id(schema = "public", table = "hub_politicos")
#             , nuevos, append = TRUE,row.names = FALSE)

## legislatura 49

pegado_final %>% filter(cargo == 'Senador' & status == 'Titular') %>% distinct(id_politico) %>% count()

# correccion a mano DIPUTADOS (colocar los suplentes que por error en la tabla del
# parlamento aparecen como titulares)

dip_titu <- pegado_final %>% filter(cargo == "Diputado" & status == 'Titular')
pegado_final <- pegado_final %>% filter(cargo == "Senador" | (cargo == 'Diputado' & status == 'Suplente'))


pegado_final[pegado_final$primer_nombre == 'SEBASTIAN'&
             pegado_final$primer_apellido == 'SABINI',
             "status"] <- 'Suplente'

pegado_final[pegado_final$primer_nombre == 'ALEJANDRO'&
             pegado_final$primer_apellido == 'SANCHEZ',
             "status"] <- 'Suplente'

pegado_final[pegado_final$primer_nombre == 'DANIEL'&
             pegado_final$primer_apellido == 'CAGGIANI',
             "status"] <- 'Suplente'

pegado_final[pegado_final$primer_nombre == 'GRACIELA'&
             pegado_final$primer_apellido == 'GARCIA',
             "status"] <- 'Suplente'

pegado_final[pegado_final$primer_nombre == 'SILVIA'&
             pegado_final$primer_apellido == 'NANE',
             "status"] <- 'Suplente'

pegado_final[pegado_final$primer_nombre == 'JOSE'&
             pegado_final$primer_apellido == 'NUNES',
             "status"] <- 'Suplente'

pegado_final[pegado_final$primer_nombre == 'JOSE'&
             pegado_final$primer_apellido == 'MAHIA',
             "status"] <- 'Suplente'

pegado_final[pegado_final$primer_nombre == 'AMIN'&
             pegado_final$primer_apellido == 'NIFFOURI',
             "status"] <- 'Suplente'

pegado_final[pegado_final$primer_nombre == 'GUSTAVO'&
             pegado_final$primer_apellido == 'PENADES',
             "status"] <- 'Suplente'

pegado_final[pegado_final$primer_nombre == 'RODRIGO'&
             pegado_final$primer_apellido == 'BLAS',
             "status"] <- 'Suplente'

pegado_final[pegado_final$primer_nombre == 'SERGIO'&
             pegado_final$primer_apellido == 'ABREU',
             "status"] <- 'Suplente'

pegado_final[pegado_final$primer_nombre == 'SEBASTIAN'&
             pegado_final$primer_apellido == 'DA SILVA',
             "status"] <- 'Suplente'

pegado_final[pegado_final$primer_nombre == 'CARLOS'&
             pegado_final$primer_apellido == 'CAMY',
             "status"] <- 'Suplente'

pegado_final[pegado_final$primer_nombre == 'CARMEN'&
             pegado_final$primer_apellido == 'SANGUINETTI',
             "status"] <- 'Suplente'

pegado_final[pegado_final$primer_nombre == 'TABARE'&
             pegado_final$primer_apellido == 'VIERA',
             "status"] <- 'Suplente'

pegado_final[pegado_final$primer_nombre == 'PEDRO'&
             pegado_final$primer_apellido == 'IRIGOIN',
             "status"] <- 'Suplente'

pegado_final[pegado_final$primer_nombre == 'GONZALO'&
             pegado_final$primer_apellido == 'CIVILA',
             "status"] <- 'Suplente'

pegado_final[pegado_final$primer_nombre == 'MARIA'&
             pegado_final$primer_apellido == 'LUSTEMBERG',
             "status"] <- 'Suplente'

pegado_final[pegado_final$primer_nombre == 'MARIA'&
             pegado_final$primer_apellido == 'FAJARDO',
             "status"] <- 'Suplente'

pegado_final[pegado_final$primer_nombre == 'LUCIA'&
             pegado_final$primer_apellido == 'ETCHEVERRY',
             "status"] <- 'Suplente'

pegado_final[pegado_final$primer_nombre == 'LUCIA'&
             pegado_final$primer_apellido == 'ETCHEVERRY',
             "status"] <- 'Suplente'

pegado_final[pegado_final$primer_nombre == 'LUIS'&
             pegado_final$primer_apellido == 'FRATTI',
             "status"] <- 'Suplente'

pegado_final[pegado_final$primer_nombre == 'AMANDA'&
             pegado_final$primer_apellido == 'DELLA',
             "status"] <- 'Titular'

pegado_final[pegado_final$primer_nombre == 'GERMAN'&
             pegado_final$primer_apellido == 'COUTINHO',
             "status"] <- 'Titular'

pegado_final[pegado_final$primer_nombre == 'NICOLAS'&
             pegado_final$primer_apellido == 'OLIVERA',
             "status"] <- 'Suplente'

pegado_final[pegado_final$primer_nombre == 'ALEJO'&
             pegado_final$primer_apellido == 'UMPIERREZ',
             "status"] <- 'Suplente'

pegado_final %>% filter(cargo == 'Senador' & status == 'Titular') %>%
  distinct(primer_apellido, primer_nombre, partido) %>% count()

## diputados

pegado_final[pegado_final$primer_nombre == 'ANDRES'&
             pegado_final$primer_apellido == 'ABT',
             "status"] <- 'Titular'

dip_titu[dip_titu$primer_nombre == 'OSCAR'&
         dip_titu$primer_apellido == 'AMIGO',
         "status"] <- 'Suplente'

dip_titu[dip_titu$primer_nombre == 'MARIA'&
         dip_titu$primer_apellido == 'FAJARDO',
         "status"] <- 'Suplente'

dip_titu[dip_titu$primer_nombre == 'MIGUEL'&
         dip_titu$primer_apellido == 'IRRAZABAL',
         "status"] <- 'Suplente'

dip_titu[dip_titu$primer_nombre == 'EDUARDO'&
         dip_titu$primer_apellido == 'LORENZO',
         "status"] <- 'Suplente'

dip_titu[dip_titu$primer_nombre == 'JOSE'&
         dip_titu$primer_apellido == 'MAHIA',
         "status"] <- 'Suplente'

dip_titu[dip_titu$primer_nombre == 'SYLVIA'&
         dip_titu$primer_apellido == 'IBARGUREN',
         "status"] <- 'Suplente'

dip_titu[dip_titu$primer_nombre == 'JUAN'&
         dip_titu$primer_apellido == 'LERETE',
         "status"] <- 'Suplente'

dip_titu[dip_titu$primer_nombre == 'JUAN'&
         dip_titu$primer_apellido == 'OLAIZOLA',
         "status"] <- 'Suplente'

dip_titu[dip_titu$primer_nombre == 'PEDRO'&
         dip_titu$primer_apellido == 'JISDONIAN',
         "status"] <- 'Suplente'

dip_titu[dip_titu$primer_nombre == 'NANCY'&
         dip_titu$primer_apellido == 'NUNEZ',
         "status"] <- 'Suplente'

dip_titu[dip_titu$primer_nombre == 'DANIEL'&
         dip_titu$primer_apellido == 'MARTINEZ',
         "status"] <- 'Suplente'

dip_titu[dip_titu$primer_nombre == 'AGUSTIN'&
         dip_titu$primer_apellido == 'MAZZINI',
         "status"] <- 'Suplente'

dip_titu[dip_titu$primer_nombre == 'AGUSTIN'&
         dip_titu$primer_apellido == 'MAZZINI',
         "status"] <- 'Suplente'

dip_titu[dip_titu$primer_nombre == 'MILTON'&
         dip_titu$primer_apellido == 'CORBO',
         "status"] <- 'Suplente'

dip_titu[dip_titu$primer_nombre == 'JORGE'&
         dip_titu$primer_apellido == 'VIVIANO',
         "status"] <- 'Suplente'

dip_titu[dip_titu$primer_nombre == 'GRACIELA'&
         dip_titu$primer_apellido == 'BIANCHI',
         "status"] <- 'Suplente'

dip_titu[dip_titu$primer_nombre == 'ALVARO'&
         dip_titu$primer_apellido == 'DELGADO',
         "status"] <- 'Suplente'

dip_titu[dip_titu$primer_nombre == 'JAVIER'&
         dip_titu$primer_apellido == 'GARCIA',
         "status"] <- 'Suplente'

dip_titu[dip_titu$primer_nombre == 'DANIEL'&
         dip_titu$primer_apellido == 'GERHARD',
         "status"] <- 'Suplente'

dip_titu[dip_titu$primer_nombre == 'ESTELA'&
         dip_titu$primer_apellido == 'PEREYRA',
         "status"] <- 'Suplente'

dip_titu[dip_titu$primer_nombre == 'HECTOR'&
         dip_titu$primer_apellido == 'PEREYRA',
         "status"] <- 'Suplente'

dip_titu[dip_titu$primer_nombre == 'LUIS'&
         dip_titu$primer_apellido == 'BOTANA',
         "status"] <- 'Suplente'

pegado_final[pegado_final$primer_nombre == 'NELSON'&
             pegado_final$primer_apellido == 'TIERNO',
             "status"] <- 'Suplente'

pegado_final[pegado_final$primer_nombre == 'NELSON'&
             pegado_final$primer_apellido == 'TIERNO',
             "status"] <- 'Suplente'

pegado_final[pegado_final$primer_nombre == 'JORGE'&
             pegado_final$primer_apellido == 'SCHUSMAN',
             "status"] <- 'Titular'

pegado_final[pegado_final$primer_nombre == 'ELSA'&
             pegado_final$primer_apellido == 'CAPILLERA',
             "partido"] <- 'Cabildo Abierto'

pegado_final[pegado_final$primer_nombre == 'ELSA'&
             pegado_final$primer_apellido == 'CAPILLERA',
             "status"] <- 'Titular'

pegado_final[pegado_final$primer_nombre == 'MARIA'&
             pegado_final$primer_apellido == 'CORTES',
             "status"] <- 'Suplente'

pegado_final[pegado_final$primer_nombre == 'GRACIELA'&
             pegado_final$primer_apellido == 'ECHENIQUE',
             "status"] <- 'Suplente'

diputados <- pegado_final %>% filter(cargo == 'Diputado' & status == 'Titular') %>%
  distinct(id_politico, primer_nombre, primer_apellido, partido)

pegado_final %>% filter(cargo == 'Senador' & status == 'Titular') %>%
  distinct(id_politico, primer_nombre, primer_apellido, partido) %>% count()

pegado_final %>% filter(cargo == 'Diputado' & status == 'Titular') %>%
  distinct(id_politico, primer_nombre, primer_apellido, partido) %>% count()

pegado_final <- rbind(pegado_final, dip_titu)

pegado_final <- pegado_final %>%
  mutate(fecha_inicio_l = as.Date(fecha_inicio_l), fecha_fin_l = as.Date(fecha_fin_l),
         fecha_intermedia = fecha_inicio_l + (fecha_fin_l - fecha_inicio_l) / 2)

titular <- pegado_final %>% filter(status == 'Titular')
titular$ed_asumir <- trunc((titular$fecha_nac %--% titular$fecha_inicio_l) / years(1))
no_titular <- pegado_final %>% filter(status != 'Titular')
no_titular$ed_asumir_1 <- trunc((no_titular$fecha_nac %--% no_titular$fecha_intermedia) / years(1))

titular <- titular %>% mutate(ed_asumir_1 = NA)
no_titular <- no_titular %>% mutate(ed_asumir = NA)
pegado_final <- rbind(titular, no_titular)
pegado_final <- pegado_final %>% mutate(edad_asumir = ifelse(is.na(ed_asumir), ed_asumir_1, ed_asumir))
pegado_final <- pegado_final %>% mutate(agregado = ifelse(id_politico > 4730, 1, 0))
pegado_final <- pegado_final %>% select(-c(X, nombre, id_fuente))

# PEGAR circunscripcion

ids_politicos <- dbGetQuery(
  con,
  "
  SELECT fpp.id_politico,
         fpp.circunscripcion, fpp.cargo
  FROM public.\"fact_politicos_PASANTIA_23_09\" fpp
  WHERE fpp.eliminada = 0
    AND fpp.legislatura = 49
    AND fpp.cargo = 'Diputado'
    AND fpp.status = 'Titular'
    AND fpp.circunscripcion IS NOT NULL
  "
)

pegado_final <- pegado_final %>% left_join(ids_politicos, by = c("id_politico", "cargo"))

# subo al DW
#dbWriteTable(con, Id(schema = "leg_biblioteca_parlamento", table = "fact_legisladores_biblio_parla")
#             , pegado_final, append = TRUE, row.names = FALSE)


###### legislatura 46 #####

leg46_biblio <- read.csv('leg46_biblioteca.csv')

leg46_biblio <- leg46_biblio %>% mutate(
  segundo_nombre = na_if(segundo_nombre, ""),
  segundo_apellido = na_if(segundo_apellido, ""),
  primer_apellido = na_if(primer_apellido, ""),
  primer_nombre = na_if(primer_nombre, ""))

# pegado del id politico con hub de biblio

### PRIMER PEGADO: nombre, apellido y fecha de nacimiento (base biblioteca)
ids_politicos <- dbGetQuery(
  con,
  "select distinct id_politico,
                  primer_nombre,
                  primer_apellido,
                  fecha_nac
   from public.\"hub_politicos\"
"
)

leg46_biblio <- leg46_biblio %>% mutate(fecha_nac = as.Date(fecha_nac))

leg46_biblio <- leg46_biblio %>% left_join(ids_politicos, by =c('primer_nombre',
                                                                'primer_apellido', 'fecha_nac'))

### SEGUNDO PEGADO: nombre, apellido y fecha de nacimiento
ids_politicos <- dbGetQuery(
  con,
  "select distinct id_politico,
                  primer_nombre,
                  primer_apellido,
                  fecha_nac
   from public.\"fact_politicos_PASANTIA_23_09\"
   where legislaturas_agrupadas like '%1985-2020%' and eliminada = 0"
)

leg46_biblio_id <- leg46_biblio %>% filter(!is.na(id_politico))
leg46_biblio_sin_id <- leg46_biblio %>% filter(is.na(id_politico)) %>% select(-id_politico)

leg46_biblio_sin_id <- leg46_biblio_sin_id %>% left_join(ids_politicos, by =c('primer_nombre',
                                                                              'primer_apellido', 'fecha_nac'))

leg46_biblio_id2 <- leg46_biblio_sin_id %>% filter(!is.na(id_politico))
leg46_biblio_sin_id <- leg46_biblio_sin_id %>% filter(is.na(id_politico)) %>% select(-id_politico)

### TERCER PEGADO: apellido, nombre y partido
ids_politicos <- dbGetQuery(
  con,
  "select distinct id_politico,
                  primer_nombre,
                  primer_apellido,
                  partido,
                  fecha_nac
   from public.\"fact_politicos_PASANTIA_23_09\"
   where legislaturas_agrupadas like '%1985-2020%' and eliminada = 0"
)

leg46_biblio_sin_id <- leg46_biblio_sin_id %>% left_join(ids_politicos, by =c('primer_nombre','partido',
                                                                              'primer_apellido')) %>% select(-ends_with(".y")) %>% 
  rename(fecha_nac = fecha_nac.x)

leg46_biblio_sin_id[leg46_biblio_sin_id$primer_nombre == 'LEON'&
                   leg46_biblio_sin_id$primer_apellido == 'LEV',
                   "id_politico"] <- 1889

leg46_biblio_id3 <- leg46_biblio_sin_id %>%
  filter(!is.na(id_politico))

leg46_biblio_id3 <- leg46_biblio_id3 %>% filter(id_politico != 4378)

leg46_biblio_sin_id <- leg46_biblio_sin_id %>%
  filter(is.na(id_politico))

leg46_biblio_sin_id <- leg46_biblio_sin_id %>%
  mutate(
    key = paste(primer_nombre, primer_apellido, fecha_nac, sep = "|"),
    id_politico = dense_rank(key) + 5087L
  ) %>%
  select(-key)   

leg46_biblio_sin_id <- leg46_biblio_sin_id %>% mutate(agregado = 1)
leg46_biblio_ag <- rbind(leg46_biblio_id, leg46_biblio_id2, leg46_biblio_id3)
leg46_biblio_ag <- leg46_biblio_ag %>% mutate(agregado = 0)
pegado_final <- rbind(leg46_biblio_ag, leg46_biblio_sin_id)
pegado_final <- pegado_final %>% mutate(id_fuente = 2)

# subo un hub politicos
hub_politicos <- pegado_final %>% select(primer_apellido, segundo_apellido, primer_nombre,
                                         segundo_nombre, fecha_nac, id_politico, agregado,id_fuente) %>% distinct()

ids_politicos <- dbGetQuery(
  con,
  "select distinct
                  primer_nombre,
                  primer_apellido,
                  fecha_nac
   from public.\"hub_politicos\"
"
)

nuevos <- anti_join(hub_politicos, ids_politicos,
                    by=c("primer_nombre", "primer_apellido",
                         "fecha_nac"))

#dbWriteTable(con, Id(schema = "public", table = "hub_politicos")
#             , nuevos, append = TRUE,row.names = FALSE)


########## legislatura 46 ##############

# correccion a mano DIPUTADOS (colocar los suplentes que por error en la tabla del
# parlamento aparecen como titulares)

dip_titu <- pegado_final %>% filter(cargo == "Diputado" & status == 'Titular')

dip_titu[dip_titu$primer_apellido == 'MUJICA' &
         dip_titu$primer_nombre == 'JOSE', "partido"]<- "Frente Amplio"

pegado_final <- pegado_final %>% filter(cargo == "Senador" | (cargo == 'Diputado' & status == 'Suplente'))
diputados <- dip_titu %>% distinct(id_politico, primer_apellido, primer_nombre, partido) %>%
  arrange(primer_apellido)

dip_titu[dip_titu$primer_apellido == 'AGAZZI' &
         dip_titu$primer_nombre == 'ERNESTO', "status"]<- "Suplente"

dip_titu[dip_titu$primer_apellido == 'CANEPA' &
         dip_titu$primer_nombre == 'DIEGO', "status"]<- "Suplente"

dip_titu[dip_titu$primer_apellido == 'BERNINI' &
         dip_titu$primer_nombre == 'GUSTAVO', "status"]<- "Suplente"

dip_titu[dip_titu$primer_apellido == 'COCCO' &
         dip_titu$primer_nombre == 'ALBA', "status"]<- "Suplente"

dip_titu[dip_titu$primer_apellido == 'PEREZ' &
         dip_titu$primer_nombre == 'PABLO', "status"]<- "Suplente"

dip_titu[dip_titu$primer_apellido == 'ALVAREZ' &
         dip_titu$primer_nombre == 'PABLO', "status"]<- "Suplente"

dip_titu[dip_titu$primer_apellido == 'MANANA' &
         dip_titu$primer_nombre == 'DANIEL', "status"]<- "Titular"

dip_titu[dip_titu$primer_apellido == 'CARAM' &
         dip_titu$primer_nombre == 'RODOLFO', "status"]<- "Titular"

dip_titu[dip_titu$primer_apellido == 'CHARAMELO' &
         dip_titu$primer_nombre == 'RICHARD', "status"]<- "Suplente"

dip_titu[dip_titu$primer_apellido == 'CARDOZO' &
           dip_titu$primer_nombre == 'JULIO', "status"]<- "Titular"

dip_titu[dip_titu$primer_apellido == 'ASTI' &
         dip_titu$primer_nombre == 'ALFREDO', "status"]<- "Suplente"

dip_titu[dip_titu$primer_apellido == 'MAGURNO' &
         dip_titu$primer_nombre == 'OSCAR', "status"]<- "Suplente"

dip_titu[dip_titu$primer_apellido == 'FERNANDEZ' &
         dip_titu$segundo_apellido == 'HUIDOBRO', "status"]<- "Suplente"

dip_titu[dip_titu$primer_apellido == 'PEREZ' &
         dip_titu$primer_nombre == 'ESTEBAN', "status"]<- "Suplente"

pegado_final[pegado_final$primer_apellido == 'CARLOS' &
             pegado_final$primer_nombre == 'MAZZULO', "status"]<- "Titular"

#### SENADORES

senadores <- pegado_final %>% filter(cargo == 'Senador' & status == 'Titular') %>%
  distinct(primer_apellido, primer_nombre, partido, id_politico)

pegado_final[pegado_final$primer_apellido == 'KORZENIAK' &
             pegado_final$primer_nombre == 'JOSE' &
             pegado_final$cargo == 'Senador', "status"]<- "Suplente"

pegado_final[pegado_final$primer_apellido == 'PERCOVICH' &
             pegado_final$primer_nombre == 'MARIA' &
             pegado_final$cargo == 'Senador', "status"]<- "Suplente"

pegado_final[pegado_final$primer_apellido == 'PINTADO' &
             pegado_final$primer_nombre == 'ENRIQUE' &
             pegado_final$cargo == 'Senador', "status"]<- "Titular"

pegado_final[pegado_final$primer_apellido == 'NICOLINI' &
             pegado_final$primer_nombre == 'LEONARDO' &
             pegado_final$cargo == 'Senador', "status"]<- "Suplente"

pegado_final[pegado_final$primer_apellido == 'LAPAZ' &
             pegado_final$primer_nombre == 'GUSTAVO' &
             pegado_final$cargo == 'Senador', "status"]<- "Suplente"

pegado_final[pegado_final$primer_apellido == 'XAVIER' &
             pegado_final$primer_nombre == 'MONICA' &
             pegado_final$cargo == 'Senador', "status"]<- "Titular"

pegado_final <- pegado_final %>% filter(primer_apellido != 'NIN')

pegado_final <- rbind(pegado_final, dip_titu)

pegado_final <- pegado_final %>%
  mutate(fecha_inicio_l = as.Date(fecha_inicio_l), fecha_fin_l = as.Date(fecha_fin_l),
         fecha_intermedia = fecha_inicio_l + (fecha_fin_l - fecha_inicio_l) / 2)

titular <- pegado_final %>% filter(status == 'Titular')
titular$ed_asumir <- trunc((titular$fecha_nac %--% titular$fecha_inicio_l) / years(1))
no_titular <- pegado_final %>% filter(status != 'Titular')
no_titular$ed_asumir_1 <- trunc((no_titular$fecha_nac %--% no_titular$fecha_intermedia) / years(1))

titular <- titular %>% mutate(ed_asumir_1 = NA)
no_titular <- no_titular %>% mutate(ed_asumir = NA)
pegado_final <- rbind(titular, no_titular)
pegado_final <- pegado_final %>% mutate(edad_asumir = ifelse(is.na(ed_asumir), ed_asumir_1, ed_asumir))
pegado_final <- pegado_final %>% mutate(agregado = ifelse(id_politico > 4730, 1, 0))
pegado_final <- pegado_final %>% select(-c(X, nombre, id_fuente))

# PEGAR circunscripcion

ids_politicos <- dbGetQuery(
  con,
  "
  SELECT fpp.id_politico,
         fpp.circunscripcion, fpp.cargo
  FROM public.\"fact_politicos_PASANTIA_23_09\" fpp
  WHERE fpp.eliminada = 0
    AND fpp.legislatura = 46
    AND fpp.cargo = 'Diputado'
    AND fpp.status = 'Titular'
    AND fpp.circunscripcion IS NOT NULL
  "
)

ids_politicos <- ids_politicos %>% mutate(id_politico = as.character(id_politico)) 
pegado_final <- pegado_final %>% left_join(ids_politicos, by = c("id_politico", "cargo"))
pegado_final <- pegado_final %>% mutate(id_politico = as.integer(id_politico)) 
pegado_final[pegado_final$primer_apellido == 'PEREYRA' &
             pegado_final$segundo_apellido == 'HUELMO',"id_politico"] <- 4022

# subo al DW
#dbWriteTable(con, Id(schema = "leg_biblioteca_parlamento", table = "fact_legisladores_biblio_parla")
#             , pegado_final, append = TRUE, row.names = FALSE)

# SUBIR AL HUB POLITICOS EL RESTO DE LOS POLITICOS DE LA BASE UMADA

df_final <- dbGetQuery(con, 'select distinct fpp.primer_apellido,fpp.segundo_apellido, fpp.primer_nombre, fpp.segundo_nombre, fpp.fecha_nac, fpp.id_politico    
from "public"."fact_politicos_PASANTIA_23_09" fpp')

hub <- dbGetQuery(con, 'SELECT * FROM "public"."hub_politicos"')

nuevos <- anti_join(df_final, hub, by = c('id_politico'))

nuevos <- nuevos %>% mutate(agregado = 0, id_fuente = 1)

repes <- nuevos %>% group_by(id_politico) %>% filter(n()>1) %>% arrange(id_politico) %>% ungroup()
unicos <- nuevos %>% group_by(id_politico) %>% filter(n()<=1) %>% arrange(id_politico) %>% ungroup()

unicos <- unicos %>%
  mutate(
    fecha_desde = Sys.time(),                        # fecha y hora actual
    fecha_hasta = as.POSIXct("2100-12-31 23:59:59")  # fecha fija
  )

##### REPES

repes_2 <- repes %>% distinct(primer_apellido, primer_nombre, id_politico)
repes_2 <- repes_2 %>% group_by(id_politico) %>% filter(n()>1) %>% arrange(id_politico) %>% ungroup()

repes_final <- repes_2 %>% filter(!id_politico %in%c(13,84, 117, 146, 200, 344, 432, 505, 861, 869,
                                      916, 1091, 1146, 1286, 1310, 1312, 1330, 1629, 1650,
                                      1680, 1690, 1719, 1741, 1779, 1991, 1997, 2003, 2238, 
                                      2457, 2523, 2674, 2692, 2698, 2738, 2742, 2818, 2834, 
                                      2864, 2908, 2914, 2916, 3538, 3593, 3804, 4152, 4213, 4186, 
                                      4506, 515, 887, 2609,2690))

max(df_final$id_politico) # 5228
max(hub$id_politico) # 5211

repes$segundo_nombre <- gsub("\\.", "", repes$segundo_nombre)
repes <- repes %>% arrange(primer_apellido, segundo_apellido, primer_nombre, fecha_nac, desc(segundo_nombre))

repes_subida <- repes %>% distinct(id_politico, agregado, id_fuente, .keep_all = TRUE)
repes_subida$segundo_nombre <- ifelse(
  nchar(repes_subida$segundo_nombre) == 1 & repes_subida$segundo_nombre != "",
  paste0(repes_subida$segundo_nombre, "."),
  repes_subida$segundo_nombre
)

repes_subida <- repes_subida %>%
  mutate(
    fecha_desde = Sys.time(),                        # fecha y hora actual
    fecha_hasta = as.POSIXct("2100-12-31 23:59:59")  # fecha fija
  )

repes_subida <- repes_subida %>% mutate(formas_de_escribir = 1)
repes_subida <- repes_subida %>% filter(id_politico != 3973)

#dbWriteTable(con, Id(schema = "public", table = "aux_hub_politicos")
#             , repes, append = TRUE, row.names = FALSE)

###### UNION DE LAS DOS TABLAS UMAD + PARLAMENTO (LEGISLATURAS 46, 47, 48 Y 49) ########

df_fact_parlamento_biblioteca <- dbGetQuery(con, 'SELECT * FROM leg_biblioteca_parlamento."fact_legisladores_biblio_parla"')
df_umad <- dbGetQuery(con, 'SELECT * FROM public."fact_politicos_PASANTIA_23_09"')

df_umad <- df_umad %>% filter(eliminada != 1)
df_umad <- df_umad %>% select(-c(eliminada, modificada)) %>% rename(id_fuente = fuente) %>% 
  mutate(id_fuente = 1)

df_fact_parlamento_biblioteca <- df_fact_parlamento_biblioteca %>%
  select(-c(agregado)) %>% mutate(id_fuente = 2)

# elimino senadores y diputados de las legislaturas 46, 47, 48 y 49
df_umad <- df_umad %>% filter((cargo != 'Senador' & cargo != 'Diputado') | !legislatura %in%c(46, 47, 48, 49))

df_umad <- df_umad %>% mutate(inicio = as.integer(NA), fin = as.integer(NA))
df_fact_parlamento_biblioteca <- df_fact_parlamento_biblioteca %>% mutate(fecha_inicio = as.Date(NA), fecha_fin = as.Date(NA))

df_union <- rbind(df_umad, df_fact_parlamento_biblioteca)

df_union[df_union$legislaturas_agrupadas == "1985-2020", "legislaturas_agrupadas"] <- "1985-2025"

### UNION DE FECHAS

politicos <- politicos %>% mutate(fecha_inicio = as.character(fecha_inicio),
                                  fecha_fin = as.character(fecha_fin),
                                  inicio = as.character(inicio),fin = as.character(fin))

politicos <- politicos %>% mutate(fecha_inicio = ifelse(is.na(fecha_inicio), inicio, fecha_inicio),
                     fecha_fin = ifelse(is.na(fecha_fin), fin, fecha_fin))



### ESTANDARIZACION DE CARGOS ###

ministerios <- politicos %>% filter(grepl("Ministro", cargo)) %>% group_by(cargo) %>% count()


#dbWriteTable(con, Id(schema = "public", table = "fact_politicos_final")
#             , df_union, append = TRUE, row.names = FALSE)

############################################################################################################
hub <- dbGetQuery(con, 'SELECT * FROM public."hub_politicos"')
df_fact_parlamento_biblioteca <- dbGetQuery(con, 'SELECT * FROM leg_biblioteca_parlamento."fact_legisladores_biblio_parla"')
hub <- dbGetQuery(con, 'SELECT * FROM public."hub_politicos"')


# identificar errores en el id politico
ids <- ids_politicos %>% group_by(id_politico) %>% count() %>% filter(n > 1)
ids <- ids %>% left_join(ids_politicos, by = 'id_politico')


#write.csv(df_final, 'df_final.csv', row.names = FALSE)
#write.csv(df_fact_parlamento_biblioteca, 'df_fact_parlamento_biblioteca.csv', row.names = FALSE)
#write.csv(hub, 'hub_politicos.csv', row.names = FALSE)
#write.csv(final, 'fact_politicos_final.csv', row.names = FALSE)

