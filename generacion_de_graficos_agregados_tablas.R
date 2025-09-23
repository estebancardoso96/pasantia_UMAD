library(DBI)
library(RPostgres)
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)

options(scipen = 2)
options(digits = 1)

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

politicos <- dbGetQuery(con, 'SELECT * FROM "public"."fact_politicos_PASANTIA_02_09"')

politicos <- politicos %>% select(-c(fecha_inicio_l, fecha_fin_l))


# ----------------------------
# METRICAS 
# ----------------------------

## Edad asumir por cargo

#### Construir agrupaciones de legislaturas

politicos <- politicos %>% mutate(legislaturas_agrupadas = case_when(legislatura <= 31 ~ "1902-1933",
                                                        legislatura > 31 & legislatura <= 41 ~ "1934-1973",
                                                        legislatura > 41 ~ "1985-2020"))

tabla_ed_asumir_cargo <- politicos %>% group_by(cargo, legislaturas_agrupadas) %>% 
  summarise(promedio = mean(ed_asumir, na.rm = TRUE), desvio_estandar = sd(ed_asumir, na.rm = TRUE)) %>% 
  ungroup()


## Edad_1 asumir por cargo (datos unicamente de diputados, senadores e intendentes)

tabla_ed_asumir_cargo <- politicos %>% group_by(cargo) %>%
  summarise(promedio = mean(ed_asumir_1, na.rm = TRUE))

tabla_ed_asumir_cargo <- politicos %>% group_by(cargo, legislatura) %>%
  summarise(promedio = mean(ed_asumir, na.rm = TRUE))

# ----------------------------
# TABLAS 
# ----------------------------

### Dividir tablas por cargos ###

tabla_cargos <- politicos %>% group_by(cargo) %>% count() %>% arrange(desc(n)) %>% ungroup() %>%
  mutate(porc=n/sum(n)*100)

parlamentario <- politicos %>% filter(cargo %in%c("Diputado", "Senador"))

### Cantidad de cargos por partido

parlamentario %>% group_by(cargo, partido) %>% count() %>% arrange(partido,desc(n),)

parlamentario %>% group_by(id_politico) %>% count() %>% arrange(desc(n))

### Cantidad de hombres y mujeres parlamentarias

parlamentario %>% group_by(sexo) %>% count()

parlamentario %>% group_by(partido,sexo) %>% count()




# ----------------------------
# ANALISIS DE LEGISLADORES 
# ----------------------------

#  ggplot(aes(x=))

parlamentario %>% filter(cargo %in%c('Senador', 'Diputado') & status == 'Titular') %>% group_by(legislatura, sexo) %>% count() %>% rename(legisladores=n) %>% 
  mutate(sexo = as.factor(sexo)) %>% ggplot(aes(x = legislatura, y = legisladores, fill = sexo)) +
  geom_col()


# analizando la cantidad de senadores y diputados por legislatura

tab <- parlamentario %>% filter(cargo %in%c('Senador', 'Diputado')) %>% group_by(legislatura, sexo) %>% count()

leg43 <- parlamentario %>% filter(cargo %in%c('Senador', 'Diputado') & legislatura %in%c(43))
leg47 <- politicos %>% filter(cargo %in%c('Senador', 'Diputado') & legislatura %in%c(47))

leg43 %>% group_by(status) %>% count()
leg47 %>% group_by(status) %>% count()

write.csv(leg47, "leg47.csv",row.names = FALSE)



