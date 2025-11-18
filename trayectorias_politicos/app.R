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

politicos <- dbGetQuery(con, 'SELECT * FROM "public"."fact_politicos_final"')
legislaturas <- dbGetQuery(con, 'SELECT legislatura, periodo FROM "politicos_uy"."legislaturas"')

politicos <- politicos %>% left_join(legislaturas, by=("legislatura")) %>% select(-legislatura, -cargo) %>%
  rename(legislatura = periodo, cargo = cargo_estandarizado)

library(shiny)
library(shinydashboard)

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

edad_max <- politicos %>% filter(!is.na(edad_asumir)) %>% group_by(cargo, primer_apellido, primer_nombre, partido, circunscripcion) %>%
  summarise(edad_max = max(edad_asumir)) %>% arrange(desc(edad_max)) %>% group_by(cargo) %>%
  slice_head(n = 1) %>% ungroup()

edad_min <- politicos %>% filter(!is.na(edad_asumir)) %>% group_by(cargo, primer_apellido, primer_nombre, partido, circunscripcion) %>%
  summarise(edad_min = min(edad_asumir)) %>% arrange((edad_min)) %>% group_by(cargo) %>%
  slice_head(n = 1) %>% ungroup()

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
  ungroup()

## promedio de edad del legislador

grafico_edad <- politicos %>% filter(cargo %in%c('Diputado', 'Senador') & !is.na(edad_asumir)) %>%
  group_by(legislatura, cargo) %>% distinct(id_politico, legislatura, edad_asumir,cargo) %>% 
  mutate(promedio_edad = mean(edad_asumir)) %>% distinct(legislatura, promedio_edad, cargo) %>% ungroup()

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
    'edad_min' = 'Edad al asumir el cargo')
    
edad_max <- edad_max %>%
  set_variable_labels(
    'edad_max' = 'Edad al asumir el cargo')

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
        # src = "logo_fcs_umad.png",  # (descomentar cuando funcione el logo)
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
      menuItem("Métricas", tabName = "metricas", icon = icon("tachometer-alt")),
      menuItem("Métricas individuales", tabName = "metricas2", icon = icon("user")),
      menuItem("Gráficos", tabName = "graficos", icon = icon("chart-bar"))
    )
  ),
  
  dashboardBody(
    # ---- estilos generales ----
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
      
      # --- 1. BÚSQUEDA ---
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
      
      # --- 2. TABLAS ---
      tabItem(tabName = "tablas",
              fluidRow(
                box(
                  width = 12,
                  title = tagList(icon("table"), "Distribución por sexo"),
                  solidHeader = TRUE,
                  status = "primary",
                  tabsetPanel(
                    tabPanel("Cantidad de mujeres y hombres por partido (histórico)",
                             DTOutput("tabla_mujeres_partido"),
                             div(style = "margin-top: 20px; text-align: center;",
                                 downloadButton("descargar_tabla_mujeres_1", "Descargar CSV"))),
                    
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
      
      # --- 3. MÉTRICAS ---
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
      
      # --- 4. MÉTRICAS INDIVIDUALES ---
      tabItem(tabName = "metricas2",
              fluidRow(
                box(
                  width = 12,
                  title = tagList(icon("user"), "Indicadores etarios individuales"),
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
                                 downloadButton("descargar_tabla_edad_maxima", "Descargar CSV"))
                    )
                  )
                )
              )
      ),
      # --- 5. GRAFICOS ---
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
                    
                    tabPanel("Promedio de edad de los legisladores por legislatura",
                             plotlyOutput("grafico_edad_leg"),
                             div(style = "margin-top: 20px; text-align: center;",
                                 downloadButton("descargar_grafico_edad_cargo", "Descargar CSV"))
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
  
  output$tabla_mujeres_partido <- renderDT({
    df <- tabla_sexos %>% arrange(desc(`Hombres por cada mujer`))
    datatable(aplicar_etiquetas(df), rownames = FALSE)
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
      data = grafico_sexo_cargos,
      x = ~cargo,
      y = ~porcentaje,
      color = ~sexo,
      type = "bar",
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

