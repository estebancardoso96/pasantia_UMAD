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

politicos <- politicos %>% left_join(legislaturas, by=("legislatura")) %>% select(-legislatura) %>%
  rename(legislatura = periodo)

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

m_2 <- politicos %>% filter(partido %in%c('Partido Nacional', 'Frente Amplio', 'Partido Colorado',
                                          'Cabildo Abierto', 'Partido Independiente') & sexo == 0 & status == 'Titular') %>%
  group_by(sexo, cargo) %>% distinct(id_politico) %>% count() %>% rename('Cantidad de mujeres'=n) %>%
  ungroup() %>%  select(-sexo)

h_2 <- politicos %>% filter(partido %in%c('Partido Nacional', 'Frente Amplio', 'Partido Colorado',
                                          'Cabildo Abierto', 'Partido Independiente') & sexo == 1 & status == 'Titular') %>%
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
tabla_edad_legislatura <- politicos %>% filter(cargo %in%c("Senador", "Diputado") & !is.na(edad_asumir) & partido
                     %in%c("Partido Nacional", "Partido Colorado", "Frente Amplio", "Cabildo Abierto",
                           "Partido Independiente", "Asamblea Popular")) %>%
  group_by(partido, cargo,legislatura) %>% reframe("Edad promedio" = round(mean(edad_asumir),1),
                                                   'Casos analizados' = n()) %>% ungroup()

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
    legislatura     = "Legislatura"
  )

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

aplicar_etiquetas <- function(df) {
  names(df) <- var_label(df)
  df
}


ui <- dashboardPage(
  dashboardHeader(
    title = span(
      img(src = "logo_fcs_umad.png", height = "40px", style = "margin-right:10px;"),
      "Visualizador de políticos y políticas del Uruguay"
    )
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Búsqueda en la base", tabName = "busqueda", icon = icon("search")),
      menuItem("Tablas", tabName = "tablas", icon = icon("table")),
      menuItem("Métricas", tabName = "metricas", icon = icon("tachometer-alt")),
      menuItem("Gráficos", tabName = "graficos", icon = icon("chart-bar"))
    )
  ),
  # -- pestana 1: busqueda -- 
  dashboardBody(
    tabItems(
      tabItem(tabName = "busqueda",
              
              fluidRow(
                box(
                  width = 12,
                  title = tagList(icon("search"),"Búsqueda en la base"),
                  solidHeader = TRUE,
                  status = "primary",
                  
                  tabsetPanel(
                    tabPanel("Filtro por Legislatura",
                             selectInput("legislatura",
                                         "Seleccionar Legislatura",
                                         choices = sort(unique(politicos$legislatura))),
                             DTOutput("tabla_leg"),
                             div(
                               style = "margin-top: 20px; text-align: center;",
                               downloadButton("descargar_tabla_leg", "Descargar CSV"))
                    ),
                    
                    tabPanel("Filtro por Partido",
                             selectInput("partido",
                                         "Seleccionar Partido Político",
                                         choices = sort(unique(politicos$partido))),
                             DTOutput("tabla_part"),
                             div(
                               style = "margin-top: 20px; text-align: center;",
                               downloadButton("descargar_tabla_partido", "Descargar CSV"))
                    ),         
                    
                    tabPanel("Filtro por Cargo",
                             selectInput("cargo",
                                         "Seleccionar cargo",
                                         choices = sort(unique(politicos$cargo))),
                             DTOutput("tabla_cargos")),
                             div(style = "margin-top: 20px; text-align: center;",
                                 downloadButton("descargar_tabla_cargos", "Descargar CSV"))  
                  )
                )
              )
      ),
      
      # --- Pestaña 2: Tablas ---
      tabItem(tabName = "tablas",
              fluidRow(
                box(
                  width = 12,
                  title = tagList(icon("table"),"Distribución por sexo"),
                  solidHeader = TRUE,
                  status = "primary",
                  
                  tabsetPanel(
                    tabPanel("Cantidad de mujeres y hombres por partido",
                             DTOutput("tabla_mujeres_partido"),
                             div(style = "margin-top: 20px; text-align: center;",
                                 downloadButton("descargar_tabla_mujeres_1", "Descargar CSV"))
                    ),
                    tabPanel("Cantidad de mujeres y hombres por cargo (se incluyen suplentes)",
                             DTOutput("tabla_mujeres_cargo"),
                             div(style = "margin-top: 20px; text-align: center;",
                                 downloadButton("descargar_tabla_mujeres_2", "Descargar CSV"))
                    ),
                    tabPanel("Cantidad de mujeres y hombres por cargo (titulares)",
                             DTOutput("tabla_mujeres_cargo_2"),
                             div(style = "margin-top: 20px; text-align: center;",
                                 downloadButton("descargar_tabla_mujeres_3", "Descargar CSV"))
                    )
                  )
                )
              )
            ),
      # --- Pestaña 3: Métricas ---
      tabItem(tabName = "metricas",
              fluidRow(
                box(
                  width = 12,
                  title = tagList(icon("tachometer-alt"), "Indicadores generales"),
                  solidHeader = TRUE,
                  status = "primary",
                  
                  tabsetPanel(
                    tabPanel("Edad promedio de asunción por cargo",
                             selectInput("legislaturas_agrupadas",
                                         "Seleccionar Legislaturas agrupadas",
                                         choices = sort(unique(tabla_edad$legislaturas_agrupadas))),
                             DTOutput("edades_cargo"),
                             div(style = "margin-top: 20px; text-align: center;",
                                 downloadButton("descargar_tabla_edades", "Descargar CSV"))
                    ),
                    
                    tabPanel("Edad promedio por partido de los legisladores",
                             selectInput("legislatura_partido",
                                         "Seleccionar Legislatura",
                                         choices = sort(unique(tabla_edad_legislatura$legislatura))),
                             DTOutput("legislatura_part"),
                             div(style = "margin-top: 20px; text-align: center;",
                                 downloadButton("descargar_tabla_legislatura_part", "Descargar CSV"))
                                             
                    )
                )
              )
      )
    )
  )
  ))
                                           
                                         

server <- function(input, output, session) {
  # --- Data reactiva filtrada por legislatura ---
  df_leg <- reactive({
    politicos %>%
      filter(legislatura == input$legislatura) %>%
      select(primer_apellido, primer_nombre, id_politico, partido, cargo, fecha_inicio, fecha_fin)
  })
  
  ## se muestra la tabla
  output$tabla_leg <- renderDT({
    df <- politicos %>%
      filter(legislatura == input$legislatura) %>%
      select(primer_apellido, primer_nombre, id_politico, partido, cargo, fecha_inicio, fecha_fin)
    datatable(aplicar_etiquetas(df), filter = "top", rownames = FALSE)    
  })
  
  output$descargar_tabla_leg <- downloadHandler(
    filename = function() {
      paste0("politicos_legislatura_", input$legislatura, ".csv")
    },
    content = function(file) {
      write.csv(df_leg(), file, row.names = FALSE, fileEncoding = "UTF-8")
      
    })
  
  df_part <- reactive({
    politicos %>%
      filter(partido == input$partido) %>%
      select(primer_apellido, primer_nombre, id_politico, legislatura, cargo, fecha_inicio, fecha_fin)
  })
  
  ## Mostrar tabla por partido
  output$tabla_part <- renderDT({
    datatable(aplicar_etiquetas(df_part()), filter = "top", rownames = FALSE)
  })
  
  ## Descargar CSV por partido
  output$descargar_tabla_partido <- downloadHandler(
    filename = function() {
      paste0("politicos_partido_", input$partido, ".csv")
    },
    content = function(file) {
      write.csv(df_part(), file, row.names = FALSE, fileEncoding = "UTF-8")
    })
  
  output$tabla_cargos <- renderDT({
    df <- politicos %>%
      filter(cargo == input$cargo) %>%
      select(primer_apellido, primer_nombre, id_politico,partido,status, fecha_inicio, fecha_fin)
    datatable(aplicar_etiquetas(df), filter = "top", rownames = FALSE)
  })
  
  output$tabla_mujeres_partido <- renderDT({
    df <- tabla_sexos
    datatable(aplicar_etiquetas(df), rownames = FALSE)
  })
  ### permite al usuario descargar los csv
  output$descargar_tabla_mujeres_1 <- downloadHandler(
    filename = function() {
      "tabla_mujeres_por_partido.csv"
    },
    content = function(file) {
      write.csv(tabla_sexos, file, row.names = FALSE, fileEncoding = "UTF-8")
  
  })    
  output$tabla_mujeres_cargo <- renderDT({
    df <- tabla_sexos_cargos
    datatable(aplicar_etiquetas(df), rownames = FALSE)
  })
  
  ### permite al usuario descargar los csv
  output$descargar_tabla_mujeres_2 <- downloadHandler(
    filename = function() {
      "tabla_mujeres_por_cargo.csv"
    },
    content = function(file) {
      write.csv(tabla_sexos_cargos, file, row.names = FALSE, fileEncoding = "UTF-8")
  })
  output$tabla_mujeres_cargo_2 <- renderDT({
    df <- tabla_sexos_cargos_2
    datatable(aplicar_etiquetas(df), rownames = FALSE)
    
  })
  output$descargar_tabla_mujeres_3 <- downloadHandler(
    filename = function() {
      "tabla_mujeres_por_cargo_titulares.csv"
    },
    content = function(file) {
      write.csv(tabla_sexos_cargos_2, file, row.names = FALSE, fileEncoding = "UTF-8")
  })
  output$edades_cargo <- renderDT({
    df <- tabla_edad %>%
    filter(legislaturas_agrupadas == input$legislaturas_agrupadas)
    datatable(aplicar_etiquetas(df), rownames = FALSE)
    
  })
  output$descargar_tabla_edades <- downloadHandler(
    filename = function() {
      "tabla_edades_por_cargo.csv"
    },
    content = function(file) {
      write.csv(tabla_edad, file, row.names = FALSE, fileEncoding = "UTF-8")
    
  })  
  output$legislatura_part <- renderDT({
    df <- tabla_edad_legislatura %>%
      filter(legislatura == input$legislatura_partido)
    datatable(aplicar_etiquetas(df), rownames = FALSE)
    
  })
  output$descargar_tabla_legislatura_part <- downloadHandler(
    filename = function() {
      paste0("tabla_edades_partido_legislatura_", input$legislatura_partido, ".csv")
    },
    content = function(file) {
      df <- tabla_edad_legislatura %>%
        filter(legislatura == input$legislatura_partido)
      write.csv(df, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
}

shinyApp(ui = ui, server = server)







