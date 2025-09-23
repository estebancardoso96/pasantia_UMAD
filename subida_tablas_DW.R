# SUBIR LAS TABLAS AL DW

library(tidyverse)
library(dplyr)
library(RPostgres)
library(puy)
library(lubridate)
library(stringi)
library(DBI)

dsn_database = "postgres"
dsn_hostname = "localhost"
dsn_port = "5432"
dsn_uid = "postgres"
dsn_pwd = "bolsilludo2020"

tryCatch({
  drv <- dbDriver("Postgres")
  print("Connecting to Database…")
  connec <- dbConnect(drv,
                      dbname = dsn_database,
                      host = dsn_hostname,
                      port = dsn_port,
                      user = dsn_uid,
                      password = dsn_pwd)
  print("Database Connected!")
},
error=function(cond) {
  print("Unable to connect to Database.")
})


## CIRCUNSCRIPCION
df_final$circunscripcion <- str_squish(df_final$circunscripcion)
df_final <- df_final %>% mutate(circunscripcion = stri_trans_general(circunscripcion, "Latin-ASCII"))

circunscripcion <- df_final %>% group_by(circunscripcion) %>% count()
circunscripcion <- circunscripcion %>% select(-n)
circunscripcion <- circunscripcion %>% ungroup() %>% mutate(id_circu = row_number(circunscripcion)) 
dbWriteTable(connec, "dim_circunscripcion", circunscripcion, overwrite = TRUE, row.names = FALSE)


## PARTIDOS
partidos <- df_final %>% group_by(partido) %>% count()
partidos <- partidos %>% select(-n)
partidos <- partidos %>% ungroup() %>% mutate(id_partido = row_number(partido)) 
partidos <- partidos %>% filter(!is.na(partido))
dbWriteTable(connec, "dim_partidos", partidos, overwrite = TRUE, row.names = FALSE)

## CARGOS
cargos <- df_final %>% group_by(cargo) %>% count()
cargos <- cargos %>% select(-n)
cargos <- cargos %>% ungroup() %>% mutate(id_cargo = row_number(cargo)) 
cargos <- cargos %>% filter(!is.na(cargo))
dbWriteTable(connec, "dim_cargos",cargos, overwrite = TRUE, row.names = FALSE)

## ESTATUS
status <- df_final_1 %>% group_by(status) %>% count() %>% na.omit() %>% ungroup()
status <- status %>%
  ungroup() %>%
  arrange(desc(n)) %>%          
  mutate(id_status = row_number())
status <- status %>% select(-n)
dbWriteTable(connec,Id(schema = "politicos_uy", table = "dim_status"), status, overwrite = TRUE,
             row.names = FALSE)


df_final_1 <- df_final %>% select(-(ed_asumir))

## SUBIDA TABLA
dbWriteTable(connec, "fact_politicos", df_final_1, overwrite = TRUE, row.names = FALSE)

df_final_1 <- df_final_1 %>% select(-c(primer_nombre, segundo_nombre, primer_apellido, segundo_apellido))
df_final_1 <- df_final_1 %>% left_join(partidos, by=c('partido')) %>% select(-(partido))  
df_final_1 <- df_final_1 %>% left_join(cargos, by=c('cargo')) %>% select(-(cargo))
df_final_1 <- df_final_1 %>% left_join(circunscripcion, by=c('circunscripcion')) %>% select(-(circunscripcion))
df_final_1 <- df_final_1 %>% left_join(status, by=c('status')) %>% select(-(status))

df_final_1 <- df_final_1 %>% select(id_politico, id_partido, id_cargo, id_status, id_circu, legislatura,fecha_inicio,
                      fecha_fin, sexo)

dbWriteTable(connec,Id(schema = "politicos_uy", table = "fact_politicos"), df_final_1, overwrite = TRUE,
             row.names = FALSE)




