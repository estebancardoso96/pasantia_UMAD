library(shiny)
library(DBI)
library(RPostgres)
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(DBI)
library(RPostgres)
library(RPostgreSQL)
library(DT) # tablas interactivas
library(plotly)

options(scipen = 2)
options(digits = 1)

library(shiny)
library(shinydashboard)

# guardar la base nuevamente
#saveRDS(politicos,'C:/Users/PC/Desktop/pasantia_CP/pasantia_UMAD/trayectorias_politicos/politicos.rds')

politicos <- readRDS('politicos.rds')


##### Generacion de tablas

### Cantidad de mujeres por partido
m <- politicos %>% filter(partido %in%c('Partido Nacional', 'Frente Amplio', 'Partido Colorado',
                                        'Cabildo Abierto', 'Partido Independiente') & sexo == 0) %>%
  group_by(sexo, partido) %>% distinct(id_politico) %>% count() %>% rename('Cantidad de mujeres'=n) %>%
  ungroup() %>%  select(-sexo)

h <- politicos %>% filter(partido %in%c('Partido Nacional', 'Frente Amplio', 'Partido Colorado',
                                        'Cabildo Abierto', 'Partido Independiente') & sexo == 1) %>%
  group_by(sexo, partido) %>% distinct(id_politico) %>% count() %>% rename('Cantidad de hombres'=n) %>%
  ungroup() %>% select(-sexo)

tabla_sexos <- inner_join(h,m,by =('partido'))

tabla_sexos <- tabla_sexos %>% mutate('Hombres por cada mujer' = `Cantidad de hombres`/`Cantidad de mujeres`) %>% 
   mutate('Hombres por cada mujer' = round(`Hombres por cada mujer`,0))

### Cantidad de mujeres legisladoras por partido y legislatura

### Cantidad de hombres y mujeres legisladores por partido

m <- politicos %>% filter(sexo == 0 & cargo %in%c('Diputado', 'Senador')) %>%
  group_by(sexo, partido, cargo,legislatura) %>% distinct(id_politico) %>% count() %>% rename('Cantidad de mujeres'=n) %>%
  ungroup() %>%  select(-sexo)

h <- politicos %>% filter(sexo == 1 & cargo %in%c('Diputado', 'Senador')) %>%
  group_by(sexo, partido, cargo,legislatura) %>% distinct(id_politico) %>% count() %>% rename('Cantidad de hombres'=n) %>%
  ungroup() %>% select(-sexo)

tabla_sexos_leg <- left_join(h,m,by =c('partido', 'cargo', 'legislatura'))
tabla_sexos_leg <- tabla_sexos_leg %>%
  mutate(`Cantidad de mujeres` = ifelse(is.na((`Cantidad de mujeres`)), 0, `Cantidad de mujeres`))

options(digits = 2)
tabla_sexos_leg <- tabla_sexos_leg %>% mutate('Hombres por cada mujer' = `Cantidad de hombres`/`Cantidad de mujeres`)
tabla_sexos_leg <- tabla_sexos_leg %>%
  mutate(`Hombres por cada mujer` = 
           ifelse(is.infinite(`Hombres por cada mujer`), 
                  NA, 
                  `Hombres por cada mujer`))

tabla_sexos_leg <- tabla_sexos_leg %>% mutate('Hombres por cada mujer' = `Cantidad de hombres`/`Cantidad de mujeres`)%>% 
  mutate('Hombres por cada mujer' = round(`Hombres por cada mujer`,2))

### Cantidad de mujeres por cargo (incluye suplencias)
m_1 <- politicos %>% filter(partido %in%c('Partido Nacional', 'Frente Amplio', 'Partido Colorado',
                                          'Cabildo Abierto', 'Partido Independiente') & sexo == 0 ) %>%
  group_by(sexo, cargo) %>% distinct(id_politico, cargo) %>% count() %>% rename('Cantidad de mujeres'=n) %>%
  ungroup() %>%  select(-sexo)

h_1 <- politicos %>% filter(partido %in%c('Partido Nacional', 'Frente Amplio', 'Partido Colorado',
                                          'Cabildo Abierto', 'Partido Independiente') & sexo == 1) %>%
  group_by(sexo, cargo) %>% distinct(id_politico, cargo) %>% count() %>% rename('Cantidad de hombres'=n) %>%
  ungroup() %>% select(-sexo)

tabla_sexos_cargos <- left_join(h_1,m_1,by =('cargo'))
tabla_sexos_cargos <- tabla_sexos_cargos %>% mutate('Hombres por cada mujer' = `Cantidad de hombres`/`Cantidad de mujeres`)%>% 
  mutate('Hombres por cada mujer' = round(`Hombres por cada mujer`,0))

### Cantidad de mujeres por cargo (titulares del cargo)
#### responde preguntas: en que cargos tiene la mujer menor disparidad respecto al hombre?

m_2 <- politicos %>% filter(sexo == 0 & status == 'Titular') %>%
  group_by(sexo, cargo) %>% distinct(id_politico) %>% count() %>% rename('Cantidad de mujeres'=n) %>%
  ungroup() %>%  select(-sexo)

h_2 <- politicos %>% filter(sexo == 1 & status == 'Titular') %>%
  group_by(sexo, cargo) %>% distinct(id_politico) %>% count() %>% rename('Cantidad de hombres'=n) %>%
  ungroup() %>% select(-sexo)

tabla_sexos_cargos_2 <- left_join(h_2,m_2,by =('cargo'))

tabla_sexos_cargos_2 <- tabla_sexos_cargos_2 %>% mutate('Hombres por cada mujer' = `Cantidad de hombres`/`Cantidad de mujeres`)%>% 
  mutate('Hombres por cada mujer' = round(`Hombres por cada mujer`,0))


### Promedio general de edades por cargo ###
tabla_edad  <- politicos %>% filter(!is.na(edad_asumir)) %>% group_by(cargo, legislaturas_agrupadas) %>% 
  reframe('Edad promedio al asumir el cargo' = round(mean(edad_asumir),1),
          'Desvío estándar de la edad' = round(sd(edad_asumir),1),
          'Casos analizados' = n()) %>%
  ungroup()

### edades por partido por legislatura
tabla_edad_legislatura <- politicos %>% filter(cargo %in%c("Senador", "Diputado") & !is.na(edad_asumir)) %>%
  group_by(partido, cargo,legislatura) %>% reframe("Edad promedio" = round(mean(edad_asumir),1),
                                                   'Casos analizados' = n()) %>% ungroup()

## METRICAS INDIVIDUALES
### politicos mas jovenes y mas viejos por cargo

edad_max <- politicos %>% filter(!is.na(edad_asumir)) %>% group_by(cargo, primer_apellido, primer_nombre, fecha_inicio,partido, circunscripcion) %>%
  summarise(edad_max = max(edad_asumir)) %>% arrange(desc(edad_max)) %>% group_by(cargo) %>%
  slice_head(n = 1) %>% ungroup()

edad_min <- politicos %>% filter(!is.na(edad_asumir)) %>% group_by(cargo, primer_apellido, primer_nombre, fecha_inicio,partido, circunscripcion) %>%
  summarise(edad_min = min(edad_asumir)) %>% arrange((edad_min)) %>% group_by(cargo) %>%
  slice_head(n = 1) %>% ungroup()

### politicos mas exitosos

tabla_politicos_exitosos <- politicos %>% group_by(primer_apellido, primer_nombre, partido, id_politico, cargo) %>% 
                                   distinct(legislatura)%>% count() %>% rename(cantidad = n)  %>% arrange(desc(cantidad)) %>% 
  filter(cantidad >= 6)


## GRAFICOS
### Distribucion por sexo de los legisladores

s_1 <- politicos %>%
  filter(cargo %in% c("Senador", "Diputado") & status == "Titular") %>%
  select(id_politico, sexo, legislatura) %>%
  mutate(sexo = ifelse(sexo == 1, "Hombre", "Mujer")) %>%
  group_by(legislatura, sexo) %>%
  distinct(id_politico) %>%  # cuenta cada político una sola vez por legislatura y sexo
  summarise(cantidad = n(), .groups = "drop_last") %>%
  mutate(porcentaje = round(cantidad / sum(cantidad) * 100, 2)) %>%
  ungroup() %>% mutate(status = 'Titular')

s_2 <- politicos %>%
  filter(cargo %in% c("Senador", "Diputado") & status == "Suplente") %>%
  select(id_politico, sexo, legislatura) %>%
  mutate(sexo = ifelse(sexo == 1, "Hombre", "Mujer")) %>%
  group_by(legislatura, sexo) %>%
  distinct(id_politico) %>%  # cuenta cada político una sola vez por legislatura y sexo
  summarise(cantidad = n(), .groups = "drop_last") %>%
  mutate(porcentaje = round(cantidad / sum(cantidad) * 100, 2)) %>%
  ungroup() %>% mutate(status = 'Suplente')

grafico_sexo_leg <- rbind(s_1, s_2)

## Distribucion del sexo por cargos

grafico_sexo_cargos <- politicos %>% mutate(sexo = ifelse(sexo == 1, "Hombre", "Mujer")) %>% 
  distinct(cargo, sexo, id_politico) %>% group_by(cargo, sexo) %>% summarise(cantidad = n(), .groups = "drop_last") %>%
  mutate(porcentaje = round(cantidad / sum(cantidad) * 100, 2)) %>%
  ungroup() %>% arrange(sexo, porcentaje)

orden_cargos <- grafico_sexo_cargos %>%
  filter(sexo == "Mujer") %>% 
  select(cargo, porcentaje) %>%
  rename(pct_mujeres = porcentaje)

grafico_sexo_cargos_ordenado <- grafico_sexo_cargos %>%
  left_join(orden_cargos, by = "cargo") %>%
  arrange(desc(pct_mujeres)) %>%              # ordenar filas
  mutate(cargo = factor(cargo, unique(cargo)))

# cambio de nombres a cargo (para acortar)

diccio <- grafico_sexo_cargos_ordenado %>% select(cargo)

diccio <- diccio %>% 
  distinct(cargo) %>% 
  mutate(
    desc = case_when(
      cargo == "Ministro Desarrollo Social" ~ "MIDES",
      cargo == "Secretario Corte Electoral" ~ "Secretario Corte Electoral",
      cargo == "Ministro Vivienda, Ordenamiento Territorial y Medio Ambiente" ~ "MVOTMA",
      cargo == "Concejal" ~ "Concejal",
      cargo == "Alcalde" ~ "Alcalde",
      cargo == "Candidato Vicepresidente" ~ "Candidato Vice",
      cargo == "Ministro Corte Electoral" ~ "Ministro CE",
      cargo == "Vicepresidente de la República" ~ "Vicepresidente",
      cargo == "Candidato Vicepresidente" ~ "Candidato Vice",
      cargo == "Diputado" ~ "Diputado",
      cargo == "Ministro Turismo" ~ "Ministro Turismo",
      cargo == "Senador" ~ "Senador",
      cargo == "Ministro Salud Pública" ~ "MSP",
      cargo == "Candidato Precandidato Presidente" ~ "CPP",
      cargo == "Ministro Educación y Cultura" ~ "MEC",
      cargo == "Intendente" ~ "Intendente",
      cargo == "Candidato Presidente" ~ "CP",
      cargo == "Ministro Economía y Finanzas" ~ "MEF",
      cargo == "Candidato Consejo Nacional de Gobierno" ~ "CCNG",
      cargo == "Ministro Trabajo y Seguridad Social" ~ "MTSS",
      cargo == "Ministro Interior" ~ "MI",
      cargo == "Concejal Departamental" ~ "Conc Departamental",
      cargo == "Consejero Departamental" ~ "Cons Departamental",
      cargo == "Intendente Interventor" ~ "II",
      cargo == "Miembro del Consejo Nacional de Gobierno" ~ "MCNG",
      cargo == "Ministro Ambiente" ~ "MA",
      cargo == "Ministro Defensa Nacional" ~ "MDN",
      cargo == "Ministro Ganadería, Agricultura y Pesca" ~ "MGAP",
      cargo == "Ministro Gobierno" ~ "MG",
      cargo == "Ministro Industria, Energía y Minería" ~ "MIEM",
      cargo == "Ministro Protección a la infancia" ~ "MPI",
      cargo == "Ministro Relaciones Exteriores" ~ "MRREE",
      cargo == "Ministro Transporte y Obras Públicas" ~ "MTOP",
      cargo == "Presidente Corte Electoral" ~ "PCE",
      cargo == "Presidente de la República" ~ "Presidente",
      cargo == "Presidente del Concejo Departamental" ~ "PCD",
      cargo == "Presidente del Consejo Nacional de Gobierno" ~ "PCNG",
      cargo == "Vicepresidente Corte Electoral" ~ "Vice Corte Electoral",
      TRUE ~ NA_character_
    )
  )

grafico_sexo_cargos_ordenado <- grafico_sexo_cargos_ordenado %>% mutate(etiqueta = cargo) %>% select(-cargo)
grafico_sexo_cargos_ordenado <- grafico_sexo_cargos_ordenado %>% left_join(diccio, by=c('etiqueta'='cargo'))
grafico_sexo_cargos_ordenado <- grafico_sexo_cargos_ordenado %>% rename(cargo = desc)

## promedio de edad del legislador

grafico_edad <- politicos %>% filter(cargo %in%c('Diputado', 'Senador') & !is.na(edad_asumir)) %>%
  group_by(legislatura, cargo) %>% distinct(id_politico, legislatura, edad_asumir,cargo) %>% 
  mutate(promedio_edad = mean(edad_asumir)) %>% distinct(legislatura, promedio_edad, cargo) %>% ungroup()

## Calculos de renovacion de la legislatura, tasa de rotacion TITULARES

leg_por_leg  <- politicos %>% filter(cargo %in%c('Senador', 'Diputado') & status=='Titular') %>% 
  group_by(legislatura) %>%
  summarise(lista = list(unique(id_politico)))

leg_join <- leg_por_leg %>%
  arrange(legislatura) %>%
  mutate(lista_anterior = lag(lista))


renovacion_titulares <- leg_join %>%
  rowwise() %>%
  mutate(
    nuevos = if (is.null(lista_anterior)) NA else sum(!lista %in% lista_anterior),
    total = length(lista),
    tasa_de_rotacion = (nuevos / total) * 100
  ) %>%
  ungroup()

df_plot <- renovacion_titulares %>%
  mutate(custom = Map(c, total, nuevos))

# Etiquetas para la pagina

library(labelled)

politicos <- politicos %>%
  set_variable_labels(
    primer_apellido = "Primer apellido",
    primer_nombre   = "Primer nombre",
    id_politico     = "ID político",
    partido         = "Partido político",
    cargo           = "Cargo",
    fecha_inicio    = "Inicio",
    fecha_fin       = "Fin",
    status          = "Estatus",
    legislatura     = "Legislatura",
    circunscripcion = "Circunscripción"
  )

politicos <- politicos %>% arrange(primer_apellido)

tabla_sexos_cargos <- tabla_sexos_cargos %>%
  set_variable_labels(
    'Cantidad de hombres' = "Cantidad de hombres",
    'Cantidad de mujeres' = "Cantidad de mujeres",
    'Hombres por cada mujer' = "Hombres por cada mujer",
     cargo = 'Cargo'
  )

tabla_sexos <- tabla_sexos %>%
  set_variable_labels(
    'Cantidad de hombres' = "Cantidad de hombres",
    'Cantidad de mujeres' = "Cantidad de mujeres",
    'Hombres por cada mujer' = "Hombres por cada mujer",
     partido = 'Partido'
  )

tabla_sexos_leg <- tabla_sexos_leg %>%
  set_variable_labels(
    'Cantidad de hombres' = "Cantidad de hombres",
    'Cantidad de mujeres' = "Cantidad de mujeres",
    'Hombres por cada mujer' = "Hombres por cada mujer",
    cargo = 'Cargo',
    legislatura = 'Legislatura',
    partido = 'Partido'
  )

tabla_sexos_cargos_2 <- tabla_sexos_cargos_2 %>%
  set_variable_labels(
    'Cantidad de hombres' = "Cantidad de hombres",
    'Cantidad de mujeres' = "Cantidad de mujeres",
    'Hombres por cada mujer' = "Hombres por cada mujer",
    cargo = 'Cargo'
  )

tabla_edad  <- tabla_edad %>%
  set_variable_labels(
    'Edad promedio al asumir el cargo' = 'Edad promedio al asumir el cargo',
    'Desvío estándar de la edad' = 'Desvío estándar de la edad',
    'Casos analizados' = 'Casos analizados',
    "cargo" = "Cargo",
    "legislaturas_agrupadas" = "Legislaturas agrupadas")

tabla_edad_legislatura <- tabla_edad_legislatura %>%
  set_variable_labels(
    'Edad promedio' = 'Edad promedio al asumir el cargo',
    'cargo' = 'Cargo',
    "partido" = "Partido",
    "legislatura" = "Legislatura",
    "Casos analizados" = "Casos analizados")

edad_min <- edad_min %>%
  set_variable_labels(
    'edad_min' = 'Edad al asumir el cargo',
    'cargo' = 'Cargo',
    'fecha_inicio' = 'Fecha inicio',
    'primer_apellido' = 'Primer apellido',
    'primer_nombre' = 'Primer nombre',
    'partido' = 'Partido',
    'circunscripcion' = 'Circunscripción')
    
edad_max <- edad_max %>%
  set_variable_labels(
    'edad_max' = 'Edad al asumir el cargo',
    'cargo' = 'Cargo',
    'fecha_inicio' = 'Fecha inicio',
    'primer_apellido' = 'Primer apellido',
    'primer_nombre' = 'Primer nombre',
    'partido' = 'Partido político',
    'circunscripcion' = 'Circunscripción')

grafico_sexo_leg <- grafico_sexo_leg %>%
  set_variable_labels(
    'sexo' = 'Sexo',
    'cantidad' = 'Cantidad',
    'porcentaje' = 'Porcentaje',
    'status' = 'Status')

grafico_edad <- grafico_edad %>%
  set_variable_labels(
    'cargo' = 'Cargo',
    'legislatura' = 'Legislatura',
    'promedio_edad' = 'Promedio de edad')

grafico_sexo_cargos <- grafico_sexo_cargos %>%
  set_variable_labels(
    'cargo' = 'Cargo',
    'sexo' = 'Sexo',
    'cantidad' = 'Cantidad',
    'porcentaje' = 'Porcentaje')

  renovacion_titulares <- renovacion_titulares %>%
  set_variable_labels(
    'legislatura' = 'Legislatura',
    'nuevos' = 'Nuevos legisladores',
    'total' = 'Total de legisladores',
    'tasa_de_rotacion' = 'Tasa de rotación')

tabla_politicos_exitosos <- tabla_politicos_exitosos %>%
  set_variable_labels(
    'cantidad' = 'Veces que ocupó el cargo en diferentes legislaturas',
    'primer_apellido' = 'Primer apellido',
    'primer_nombre' = 'Primer nombre',
    'partido' = 'Partido',
    'id_politico' = 'Id político',
    'cargo' = 'Cargo')

aplicar_etiquetas <- function(df) {
  names(df) <- var_label(df)
  df
}

library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)

### UI ###

ui <- dashboardPage(
  dashboardHeader(
    title = span(
      img(
        # src = "logo_fcs_umad.png",
        height = "40px",
        style = "margin-right:10px;"
      ),
      "Visualizador"
    )
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Búsqueda en la base", tabName = "busqueda", icon = icon("search")),
      menuItem("Tablas", tabName = "tablas", icon = icon("table")),
      menuItem("Métricas etarias", tabName = "metricas", icon = icon("tachometer-alt")),
      menuItem("Métricas individuales", tabName = "metricas2", icon = icon("user")),
      menuItem("Gráficos", tabName = "graficos", icon = icon("chart-bar")),
      menuItem("Nota metodológica", tabName = "nota", icon = icon("file-alt"))
    )
  ),
  
  dashboardBody(
    
    tags$head(tags$style(HTML("
      h2.dashboard-title {
        text-align: center;
        font-weight: bold;
        font-size: 28px;
        color: #2c3e50;
        margin-bottom: 25px;
        margin-top: 10px;
      }
    "))),
    
    h2("Visualizador de políticos y políticas del Uruguay (1902-2025)", class = "dashboard-title"),
    
    tabItems(
      
      # -------------------
      # 1. BÚSQUEDA
      # -------------------
      tabItem(tabName = "busqueda",
              fluidRow(
                box(
                  width = 12,
                  title = tagList(icon("search"), "Búsqueda en la base"),
                  solidHeader = TRUE,
                  status = "primary",
                  tabsetPanel(
                    
                    tabPanel("Búsqueda en toda la base",
                             DTOutput("tabla_base")),
                    
                    tabPanel("Filtro por Legislatura",
                             selectInput("legislatura", "Seleccionar Legislatura",
                                         choices = sort(unique(politicos$legislatura)),
                                         selected = max(politicos$legislatura, na.rm = TRUE)),
                             DTOutput("tabla_leg"),
                             div(style = "margin-top: 20px; text-align: center;",
                                 downloadButton("descargar_tabla_leg", "Descargar CSV"))),
                    
                    tabPanel("Filtro por Partido",
                             selectInput("partido", "Seleccionar Partido Político",
                                         choices = sort(unique(politicos$partido))),
                             DTOutput("tabla_part"),
                             div(style = "margin-top: 20px; text-align: center;",
                                 downloadButton("descargar_tabla_partido", "Descargar CSV"))),
                    
                    tabPanel("Filtro por Cargo",
                             selectInput("cargo", "Seleccionar cargo",
                                         choices = sort(unique(politicos$cargo))),
                             DTOutput("tabla_cargos"),
                             div(style = "margin-top: 20px; text-align: center;",
                                 downloadButton("descargar_tabla_cargos", "Descargar CSV")))
                  )
                )
              )
      ),
      
      # -------------------
      # 2. TABLAS
      # -------------------
      tabItem(tabName = "tablas",
              fluidRow(
                box(
                  width = 12,
                  title = tagList(icon("table"), "Distribución por sexo"),
                  solidHeader = TRUE,
                  status = "primary",
                  tabsetPanel(
                    
                    tabPanel("Cantidad de legisladores y legisladoras por partido",
                             selectInput("legislaturas", "Seleccionar Legislatura",
                                         choices = sort(unique(tabla_sexos_leg$legislatura))),
                             DTOutput("tabla_mujeres_partido_leg"),
                             div(style = "margin-top: 20px; text-align: center;",
                                 downloadButton("descargar_tabla_mujeres_1_1", "Descargar CSV"))),
                    
                    tabPanel("Cantidad de mujeres y hombres por cargo",
                             DTOutput("tabla_mujeres_cargo"),
                             div(style = "margin-top: 20px; text-align: center;",
                                 downloadButton("descargar_tabla_mujeres_2", "Descargar CSV"))),
                    
                    tabPanel("Cantidad de mujeres y hombres por cargo (solo titulares)",
                             DTOutput("tabla_mujeres_cargo_2"),
                             div(style = "margin-top: 20px; text-align: center;",
                                 downloadButton("descargar_tabla_mujeres_3", "Descargar CSV")))
                  )
                )
              )
      ),
      
      # -------------------
      # 3. MÉTRICAS ETARIAS
      # -------------------
      tabItem(tabName = "metricas",
              fluidRow(
                box(
                  width = 12,
                  title = tagList(icon("tachometer-alt"), "Indicadores etarios"),
                  solidHeader = TRUE,
                  status = "primary",
                  tabsetPanel(
                    
                    tabPanel("Edad promedio de asunción por cargo",
                             selectInput("legislaturas_agrupadas",
                                         "Seleccionar Legislaturas agrupadas",
                                         choices = sort(unique(tabla_edad$legislaturas_agrupadas))),
                             DTOutput("edades_cargo"),
                             div(style = "margin-top: 20px; text-align: center;",
                                 downloadButton("descargar_tabla_edades", "Descargar CSV"))),
                    
                    tabPanel("Edad promedio por partido de los legisladores",
                             selectInput("legislatura_partido",
                                         "Seleccionar Legislatura",
                                         choices = sort(unique(tabla_edad_legislatura$legislatura)),
                                         selected = max(tabla_edad_legislatura$legislatura, na.rm = TRUE)),
                             DTOutput("legislatura_part"),
                             div(style = "margin-top: 20px; text-align: center;",
                                 downloadButton("descargar_tabla_legislatura_part", "Descargar CSV")))
                  )
                )
              )
      ),
      
      # -------------------
      # 4. MÉTRICAS INDIVIDUALES
      # -------------------
      tabItem(tabName = "metricas2",
              fluidRow(
                box(
                  width = 12,
                  title = tagList(icon("user"), "Indicadores individuales"),
                  solidHeader = TRUE,
                  status = "primary",
                  tabsetPanel(
                    
                    tabPanel("Los políticos más jóvenes en asumir cada cargo",
                             DTOutput("edad_minima"),
                             div(style = "margin-top: 20px; text-align: center;",
                                 downloadButton("descargar_tabla_edad_minima", "Descargar CSV"))),
                    
                    tabPanel("Los políticos más viejos en asumir cada cargo",
                             DTOutput("edad_maxima"),
                             div(style = "margin-top: 20px; text-align: center;",
                                 downloadButton("descargar_tabla_edad_maxima", "Descargar CSV"))),
                    
                    tabPanel("Los legisladores más exitosos",
                             DTOutput("politicos_exitosos"),
                             div(style = "margin-top: 20px; text-align: center;",
                                 downloadButton("descargar_tabla_pol_exitoso", "Descargar CSV")))
                  )
                )
              )
      ),
      
      # -------------------
      # 5. GRAFICOS
      # -------------------
      tabItem(tabName = "graficos",
              fluidRow(
                box(
                  width = 12,
                  title = tagList(icon("chart-bar"), "Gráficos interactivos"),
                  solidHeader = TRUE,
                  status = "primary",
                  tabsetPanel(
                    
                    tabPanel("Distribución por sexo de los legisladores",
                             selectInput("leg_status",
                                         "Seleccionar status",
                                         choices = sort(unique(grafico_sexo_leg$status)),
                                         selected = max(grafico_sexo_leg$status)),
                             plotlyOutput("grafico_sexo_leg_out"),
                             div(style = "margin-top: 20px; text-align: center;",
                                 downloadButton("descargar_grafico_sexo_leg", "Descargar CSV"))),
                    
                    tabPanel("Distribución por sexo de los cargos",
                             plotlyOutput("grafico_sexo_cargo"),
                             div(style = "margin-top: 20px; text-align: center;",
                                 downloadButton("descargar_grafico_edad_cargo", "Descargar CSV"))),
                    
                    tabPanel("Rotación parlamentaria por legislatura",
                             plotlyOutput("grafico_renovacion_titulares"),
                             div(style = "margin-top: 20px; text-align: center;",
                                 downloadButton("descargar_grafico_edad_cargo", "Descargar CSV"))),
                    
                    tabPanel("Promedio de edad por legislatura",
                             plotlyOutput("grafico_edad_leg"),
                             div(style = "margin-top: 20px; text-align: center;",
                                 downloadButton("descargar_grafico_edad_cargo", "Descargar CSV")))
                  )
                )
              )
      ),
      
      # -------------------
      # 6. NOTA METODOLÓGICA
      # -------------------
      tabItem(
        tabName = "nota",
        fluidRow(
          box(
            width = 12,
            title = tagList(icon("file-alt"), "Nota metodológica"),
            solidHeader = TRUE,
            status = "primary",
            tabsetPanel(
              
              # 1. ACLARACIONES
              tabPanel(
                "Aclaraciones metodológicas",
                div(
                  style = "text-align: justify;",
                  p("Este visualizador se alimenta de dos fuentes: la base de datos del paquete de R puy (mantenido por la Unidad de Métodos y Acceso a Datos de la Facultad de Ciencias Sociales) y de datos obtenidos mediante web scraping de la página de la biblioteca del parlamento https://biblioteca.parlamento.gub.uy:8008/biografias/busqueda
Cada fila de la tabla alojada en la pestaña de búsqueda representa un cargo o una candidatura de un político con una fecha de inicio y una fecha de fin, teniendo información específica sobre el cargo, el partido por el cual asume (o se postula), la edad al asumir del político, el sexo y la circunscripción en caso de que corresponda. A su vez cada político se identifica con un id político único que permite seguir su trayectoria a lo largo de la base.")
                )
              ),
              
              # 2. DICCIONARIO (completo)
              tabPanel(
                "Diccionario de variables",
                div(
                  style = "padding: 15px;",
                  
                  div(
                    style = "text-align: center; margin-bottom: 12px;",
                    downloadButton("descargar_diccionario", "Descargar diccionario CSV")
                  ),
                  tags$table(
                    class = "table table-striped table-bordered",
                    tags$thead(
                      tags$tr(
                        tags$th("Variable"),
                        tags$th("Etiqueta"),
                        tags$th("Descripción")
                      )
                    ),
                    tags$tbody(
                      tags$tr(tags$td("primer_apellido"), tags$td("Primer apellido"), tags$td("Primer apellido del político/a")),
                      tags$tr(tags$td("segundo_apellido"), tags$td("Segundo apellido"), tags$td("Segundo apellido del político/a")),
                      tags$tr(tags$td("primer_nombre"), tags$td("Primer nombre"), tags$td("Primer nombre del político/a")),
                      tags$tr(tags$td("segundo_nombre"), tags$td("Segundo nombre"), tags$td("Segundo nombre del político/a")),
                      tags$tr(tags$td("id_politico"), tags$td("Id político"), tags$td("Identificador único del político")),
                      tags$tr(tags$td("partido"), tags$td("Partido"), tags$td("Partido político por el que asume o se postula")),
                      tags$tr(tags$td("fecha_inicio"), tags$td("Inicio"), tags$td("Fecha (formato año-mes-día) en la que el político asume su cargo o postulación")),
                      tags$tr(tags$td("fecha_fin"), tags$td("Fin"), tags$td("Fecha (formato año-mes-día) en la que el político finaliza el ejercicio en su cargo")),
                      tags$tr(tags$td("status"), tags$td("Estatus"), tags$td("Titular o suplente en el cargo asumido")),
                      tags$tr(tags$td("circunscripcion"), tags$td("Circunscripción"), tags$td("Circunscripción del cargo (en caso de que corresponda)")),
                      tags$tr(tags$td("sexo"), tags$td("Sexo"), tags$td("Sexo del político, 1 si es hombre y 0 si es mujer")),
                      tags$tr(tags$td("id_fuente"), tags$td("id_fuente"), tags$td("Fuente de donde proviene el dato, 1 = paquete R puy ; 2 = web scraping de la biblioteca del parlamento")),
                      tags$tr(tags$td("fecha_nac"), tags$td("fecha_nac"), tags$td("Fecha de nacimiento del político/a")),
                      tags$tr(tags$td("fecha_inicio_l"), tags$td("fecha_inicio_l"), tags$td("Fecha de inicio de la Legislatura que actúa el político")),
                      tags$tr(tags$td("fecha_fin_l"), tags$td("fecha_fin_l"), tags$td("Fecha de fin de la Legislatura que actúa el político")),
                      tags$tr(tags$td("fecha_intermedia"), tags$td("fecha_intermedia"), tags$td("Fecha intermedia (en la mitad entre la fecha de fin y la fecha de inicio de la Legislatura)")),
                      tags$tr(tags$td("ed_asumir"), tags$td("ed_asumir"), tags$td("Edad en la que asume el cargo el político calculada con la fecha de inicio del cargo")),
                      tags$tr(tags$td("ed_asumir_1"), tags$td("ed_asumir_1"), tags$td("Edad en la que asume el cargo calculada con la fecha intermedia cuando la fecha de inicio del cargo está missing")),
                      tags$tr(tags$td("edad_asumir"), tags$td("edad_asumir"), tags$td("Integración de ed_asumir con ed_asumir_1")),
                      tags$tr(tags$td("legislaturas_agrupadas"), tags$td("legislaturas_agrupadas"), tags$td("Asume 3 valores que agrupan legislaturas: 1902-1933, 1934-1973 y 1985-2025")),
                      tags$tr(tags$td("inicio"), tags$td("inicio"), tags$td("Año de inicio del cargo (datos obtenidos de la página del parlamento)")),
                      tags$tr(tags$td("fin"), tags$td("fin"), tags$td("Año de fin del cargo (datos obtenidos de la página del parlamento)")),
                      tags$tr(tags$td("cargo"), tags$td("Cargo"), tags$td("Cargo o candidatura")),
                      tags$tr(tags$td("cargo_vigente"), tags$td("cargo_vigente"), tags$td("Asume SI si el cargo está vigente en la actualidad y NO si el cargo no está vigente en la actualidad")),
                      tags$tr(tags$td("legislatura"), tags$td("Legislatura"), tags$td("Legislatura en que se asume el cargo o se postula la candidatura"))
                    )
                  )
                )
              ),
              # 3. ESTANDARIZACIÓN DE MINISTERIOS
              tabPanel(
                "Estandarización de Ministerios",
                div(
                  style = "padding: 15px;",
                  tags$table(
                    class = "table table-striped table-bordered",
                    tags$thead(
                      tags$tr(
                        tags$th("Cargo"),
                        tags$th("Cantidad"),
                        tags$th("Cargo estandarizado"),
                        tags$th("Ministerio vigente")
                      )
                    ),
                    tags$tbody(
                      tags$tr(tags$td("Ministro Ambiente"), tags$td("1"), tags$td("Ministro Ambiente"), tags$td("SI")),
                      tags$tr(tags$td("Ministro Corte Electoral"), tags$td("35"), tags$td("Ministro Corte Electoral"), tags$td("SI")),
                      tags$tr(tags$td("Ministro Defensa Nacional"), tags$td("46"), tags$td("Ministro Defensa Nacional"), tags$td("SI")),
                      tags$tr(tags$td("Ministro Deporte y Juventud"), tags$td("1"), tags$td("Ministro Turismo"), tags$td("NO")),
                      tags$tr(tags$td("Ministro Desarrollo Social"), tags$td("5"), tags$td("Ministro Desarrollo Social"), tags$td("SI")),
                      tags$tr(tags$td("Ministro Economia y Finanzas"), tags$td("22"), tags$td("Ministro Economia y Finanzas"), tags$td("SI")),
                      tags$tr(tags$td("Ministro Educación y Cultura"), tags$td("24"), tags$td("Ministro Educación y Cultura"), tags$td("SI")),
                      tags$tr(tags$td("Ministro Fomento"), tags$td("2"), tags$td("Ministro Transporte y Obras Públicas"), tags$td("NO")),
                      tags$tr(tags$td("Ministro Ganadería y Agricultura"), tags$td("16"), tags$td("Ministro Ganadería, Agricultura y Pesca"), tags$td("NO")),
                      tags$tr(tags$td("Ministro Ganadería, Agricultura y Pesca"), tags$td("26"), tags$td("Ministro Ganadería, Agricultura y Pesca"), tags$td("SI")),
                      tags$tr(tags$td("Ministro Gobierno"), tags$td("2"), tags$td(""), tags$td("NO")),
                      tags$tr(tags$td("Ministro Guerra y Marina"), tags$td("17"), tags$td("Ministro Defensa Nacional"), tags$td("NO")),
                      tags$tr(tags$td("Ministro Hacienda"), tags$td("36"), tags$td("Ministro Economia y Finanzas"), tags$td("NO")),
                      tags$tr(tags$td("Ministro Industria y Comercio"), tags$td("8"), tags$td("Ministro Industria, Energia y Mineria"), tags$td("NO")),
                      tags$tr(tags$td("Ministro Industria y Energía"), tags$td("3"), tags$td("Ministro Industria, Energia y Mineria"), tags$td("NO")),
                      tags$tr(tags$td("Ministro Industria y Trabajo"), tags$td("19"), tags$td("Ministro Industria, Energia y Mineria"), tags$td("NO")),
                      tags$tr(tags$td("Ministro Industria, Energia y Mineria"), tags$td("18"), tags$td("Ministro Industria, Energia y Mineria"), tags$td("SI")),
                      tags$tr(tags$td("Ministro Industria, Trabajo e Instrucción Pública"), tags$td("4"), tags$td("Ministro Trabajo y Seguridad Social"), tags$td("NO")),
                      tags$tr(tags$td("Ministro Industrias, Trabajo y Comunicación"), tags$td("14"), tags$td("Ministro Industria, Energia y Mineria"), tags$td("NO")),
                      tags$tr(tags$td("Ministro Instruccion Publica y Prevision Social"), tags$td("18"), tags$td("Ministro Trabajo y Seguridad Social"), tags$td("NO")),
                      tags$tr(tags$td("Ministro Interior"), tags$td("75"), tags$td("Ministro Interior"), tags$td("SI")),
                      tags$tr(tags$td("Ministro Justicia e Instrucción Pública"), tags$td("16"), tags$td("Ministro Educación y Cultura"), tags$td("NO")),
                      tags$tr(tags$td("Ministro Obras Públicas"), tags$td("37"), tags$td("Ministro Transporte y Obras Públicas"), tags$td("NO")),
                      tags$tr(tags$td("Ministro Protección a la infancia"), tags$td("1"), tags$td("Ministro Protección a la infancia"), tags$td("NO")),
                      tags$tr(tags$td("Ministro Relaciones Exteriores"), tags$td("53"), tags$td("Ministro Relaciones Exteriores"), tags$td("SI")),
                      tags$tr(tags$td("Ministro Salud Pública"), tags$td("38"), tags$td("Ministro Salud Pública"), tags$td("SI")),
                      tags$tr(tags$td("Ministro Trabajo y Previsión Social"), tags$td("3"), tags$td("Ministro Trabajo y Seguridad Social"), tags$td("NO")),
                      tags$tr(tags$td("Ministro Trabajo y Seguridad Social"), tags$td("23"), tags$td("Ministro Trabajo y Seguridad Social"), tags$td("SI")),
                      tags$tr(tags$td("Ministro Transporte y Obras Públicas"), tags$td("14"), tags$td("Ministro Transporte y Obras Públicas"), tags$td("SI")),
                      tags$tr(tags$td("Ministro Transporte, Comunicación y Turismo"), tags$td("9"), tags$td("Ministro Transporte y Obras Públicas"), tags$td("NO")),
                      tags$tr(tags$td("Ministro Turismo"), tags$td("7"), tags$td("Ministro Turismo"), tags$td("SI")),
                      tags$tr(tags$td("Ministro Turismo y Deportes"), tags$td("5"), tags$td("Ministro Turismo"), tags$td("NO")),
                      tags$tr(tags$td("Ministro Vivienda, Ordenamiento Territorial y Medio Ambiente"), tags$td("13"), tags$td("Ministro Vivienda, Ordenamiento Territorial y Medio Ambiente"), tags$td("SI"))
                    )
                  )
                )
              )
            ) 
          )
        )
      )
    )
  )
)



# --- SERVER ---

server <- function(input, output, session) {
  
  # --- 1. Tabla base ---
  output$tabla_base <- renderDT({
    df <- politicos %>%
      select(primer_apellido, primer_nombre, id_politico, partido, cargo, status, circunscripcion,
             fecha_inicio, fecha_fin)
    datatable(aplicar_etiquetas(df), filter = "top", rownames = FALSE)    
  })
  
  # --- 2. Reactivos principales ---
  df_leg <- reactive({
    politicos %>%
      filter(legislatura == input$legislatura) %>%
      select(primer_apellido, primer_nombre, id_politico, partido, cargo, fecha_inicio, fecha_fin)
  })
  
  df_part <- reactive({
    politicos %>%
      filter(partido == input$partido) %>%
      select(primer_apellido, primer_nombre, id_politico, legislatura, cargo, fecha_inicio, fecha_fin)
  })
  
  df_cargo <- reactive({
    politicos %>%
      filter(cargo == input$cargo) %>%
      select(primer_apellido, primer_nombre, id_politico, partido, status, fecha_inicio, fecha_fin)
  })
  
  df_tabla_mujeres_leg <- reactive({
    tabla_sexos_leg %>%
      filter(legislatura == input$legislaturas) %>%
      arrange(desc(`Hombres por cada mujer`))
  })
  
  df_edades_cargo <- reactive({
    tabla_edad %>%
      filter(legislaturas_agrupadas == input$legislaturas_agrupadas)
  })
  
  df_legislatura_partido <- reactive({
    tabla_edad_legislatura %>%
      filter(legislatura == input$legislatura_partido)
  })
  
  df_grafico_sexo_leg <- reactive({
    grafico_sexo_leg %>% 
      filter(status == input$leg_status)%>% 
      mutate(sexo = factor(sexo, levels = c("Mujer", "Hombre")))
  })
  
  # --- 3. Render de tablas ---
  output$tabla_leg <- renderDT({
    datatable(aplicar_etiquetas(df_leg()), filter = "top", rownames = FALSE)
  })
  
  output$tabla_part <- renderDT({
    datatable(aplicar_etiquetas(df_part()), filter = "top", rownames = FALSE)
  })
  
  output$tabla_cargos <- renderDT({
    datatable(aplicar_etiquetas(df_cargo()), filter = "top", rownames = FALSE)
  })
  
  output$tabla_mujeres_partido_leg <- renderDT({
    datatable(aplicar_etiquetas(df_tabla_mujeres_leg()), rownames = FALSE)
  })
  
  output$tabla_mujeres_cargo <- renderDT({
    df <- tabla_sexos_cargos %>% arrange(desc(`Hombres por cada mujer`))
    datatable(aplicar_etiquetas(df), rownames = FALSE)
  })
  
  output$tabla_mujeres_cargo_2 <- renderDT({
    df <- tabla_sexos_cargos_2 %>% arrange(desc(`Hombres por cada mujer`))
    datatable(aplicar_etiquetas(df), rownames = FALSE)
  })
  
  output$edades_cargo <- renderDT({
    datatable(aplicar_etiquetas(df_edades_cargo()), rownames = FALSE)
  })
  
  output$legislatura_part <- renderDT({
    datatable(aplicar_etiquetas(df_legislatura_partido()), rownames = FALSE)
  })
  
  output$edad_maxima <- renderDT({
    df <- edad_max %>% arrange(desc(edad_max))
    datatable(aplicar_etiquetas(df), rownames = FALSE)
  })
  
  output$edad_minima <- renderDT({
    df <- edad_min %>% arrange((edad_min))
    datatable(aplicar_etiquetas(df), rownames = FALSE)
  })
  
  output$politicos_exitosos <- renderDT({
    df <- tabla_politicos_exitosos %>% arrange(desc(cantidad))
    datatable(aplicar_etiquetas(df), rownames = FALSE)
  })
  
  # grafico sexo legisladores
  output$grafico_sexo_leg_out <- renderPlotly({
    
    plot_ly(
      data = df_grafico_sexo_leg(),
      x = ~legislatura,
      y = ~porcentaje,
      color = ~sexo,
      type = "bar"
    ) %>%
      layout(
        title = "Distribución por sexo de los legisladores por legislatura",
        barmode = "stack",
        xaxis = list(
          title = "Legislatura",
          tickangle = 90
        ),
        yaxis = list(title = "Porcentaje")
      )
  })
  # edad legisladores
  output$grafico_edad_leg <- renderPlotly({
    
    plot_ly(
      data = grafico_edad,
      x = ~legislatura,
      y = ~promedio_edad,
      color = ~cargo,
      type = "scatter",
      mode = 'markers',
      marker = list(size = 10)
    ) %>%
      layout(
        title = "Promedio de edad del legislador",
        xaxis = list(
          title = "Legislatura",
          tickangle = 90
        ),
        yaxis = list(title = "Promedio de edad")
      )
  })
  # graficos sexo cargos
  output$grafico_sexo_cargo <- renderPlotly({
    plot_ly(
      data = grafico_sexo_cargos_ordenado,
      x = ~cargo,
      y = ~porcentaje,
      color = ~sexo,
      type = "bar",
      colors = c("Hombre" = "#9AA9D9",
                 "Mujer"  = "#6CC8B9"), 
      hovertemplate = paste(
        "<b>Cargo:</b> %{x}<br>",
        "<b>Sexo:</b> %{color}<br>",
        "<b>Porcentaje:</b> %{y:.2f}%<br>",
        "<b>Cantidad:</b> %{customdata}<extra></extra>"
      ),
      customdata = ~cantidad  # etiqueta adicional
    ) %>%
      layout(
        title = "Distribución por sexo de los cargos",
        barmode = 'stack',
        xaxis = list(title = "Cargo", tickangle = 90),
        yaxis = list(title = "Porcentaje")
      )
  })
  output$grafico_renovacion_titulares <- renderPlotly({
    plot_ly(
      data = df_plot,
      x = ~legislatura,
      y = ~tasa_de_rotacion,
      type = "bar",
      customdata = ~custom,
      hovertemplate = paste(
        "<b>Legislatura:</b> %{x}<br>",
        "<b>Tasa de rotación:</b> %{y:.2f}%<br>",
        "<b>Total legisladores:</b> %{customdata[0]}<br>",
        "<b>Nuevos legisladores:</b> %{customdata[1]}<extra></extra>"
      )
    ) %>%   
      layout(
        title = "Tasa de rotación parlamentaria (titulares)",
        xaxis = list(title = "Legislatura", tickangle = 90),
        yaxis = list(title = "Tasa de rotación")
      )
  })  
  # --- 4. Descargas ---
  output$descargar_tabla_leg <- downloadHandler(
    filename = function() paste0("politicos_legislatura_", input$legislatura, ".csv"),
    content = function(file) write.csv(df_leg(), file, row.names = FALSE, fileEncoding = "UTF-8")
  )
  
  output$descargar_tabla_partido <- downloadHandler(
    filename = function() paste0("politicos_partido_", input$partido, ".csv"),
    content = function(file) write.csv(df_part(), file, row.names = FALSE, fileEncoding = "UTF-8")
  )
  
  output$descargar_tabla_cargos <- downloadHandler(
    filename = function() paste0("politicos_cargo_", input$cargo, ".csv"),
    content = function(file) write.csv(df_cargo(), file, row.names = FALSE, fileEncoding = "UTF-8")
  )
  
  output$descargar_tabla_mujeres_1 <- downloadHandler(
    filename = function() "tabla_mujeres_por_partido.csv",
    content = function(file) write.csv(tabla_sexos, file, row.names = FALSE, fileEncoding = "UTF-8")
  )
  
  output$descargar_tabla_mujeres_1_1 <- downloadHandler(
    filename = function() paste0("tabla_mujeres_legislatura_", input$legislaturas, ".csv"),
    content = function(file) write.csv(df_tabla_mujeres_leg(), file, row.names = FALSE, fileEncoding = "UTF-8")
  )
  
  output$descargar_tabla_mujeres_2 <- downloadHandler(
    filename = function() "tabla_mujeres_por_cargo.csv",
    content = function(file) write.csv(tabla_sexos_cargos, file, row.names = FALSE, fileEncoding = "UTF-8")
  )
  
  output$descargar_tabla_mujeres_3 <- downloadHandler(
    filename = function() "tabla_mujeres_por_cargo_titulares.csv",
    content = function(file) write.csv(tabla_sexos_cargos_2, file, row.names = FALSE, fileEncoding = "UTF-8")
  )
  
  output$descargar_tabla_edades <- downloadHandler(
    filename = function() "tabla_edades_por_cargo.csv",
    content = function(file) write.csv(df_edades_cargo(), file, row.names = FALSE, fileEncoding = "UTF-8")
  )
  
  output$descargar_tabla_legislatura_part <- downloadHandler(
    filename = function() paste0("tabla_edades_partido_legislatura_", input$legislatura_partido, ".csv"),
    content = function(file) write.csv(df_legislatura_partido(), file, row.names = FALSE, fileEncoding = "UTF-8")
  )
  
  # Descargar tabla de los más jóvenes
  output$descargar_tabla_edad_minima <- downloadHandler(
    filename = function() "edad_minima_por_cargo.csv",
    content = function(file) {
      write.csv(edad_min, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  # Descargar tabla de los más viejos
  output$descargar_tabla_edad_maxima <- downloadHandler(
    filename = function() "edad_maxima_por_cargo.csv",
    content = function(file) {
      write.csv(edad_max, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
}

shinyApp(ui, server)
