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

max(df_final$id_politico)
df_final %>% filter(!is.na(fecha_nac)) %>% count()

df_final <- read_csv("df_final.csv")

library(DBI)
library(RPostgres)
library(RPostgreSQL)

readRenviron("~/.Renviron")

usuario <- Sys.getenv("DB_USER")
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

dbWriteTable(con, "fact_politicos_PASANTIA_02_09", df_final, overwrite = TRUE, row.names = FALSE)

################################################################################################################

df_final <- dbGetQuery(connec, "SELECT * FROM fact_politicos")

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


######## Correción de la legislatura 47 (que marca a todos los legisladores como titulares)

leg47_biblio <- read.csv("leg47_biblioteca.csv", encoding = "utf-8")
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

leg47_diputado <- leg47 %>% filter(cargo %in%c("Diputado"))
leg47_senador <- leg47 %>% filter(cargo %in%c("Senador"))

leg47_biblio_diputado <- leg47_biblio %>% filter(Cargo %in%c("DIPUTADO", "DIPUTADA"))

# Me quedo con las filas unicas

leg47_biblio_diputado_unicos <- leg47_biblio_diputado %>% group_by(X) %>% filter(n()<=1) %>% ungroup()


write.csv(df_final, 'df_final.csv', row.names = FALSE)

