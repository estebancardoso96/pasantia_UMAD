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

library(shiny)
library(shinydashboard)

##### Generacion de tablas

### Cantidad de mujeres por partido
m <- politicos %>% filter(partido %in%c('Partido Nacional', 'Frente Amplio', 'Partido Colorado',
                                        'Cabildo Abierto') & sexo == 0) %>%
  group_by(sexo, partido) %>% distinct(id_politico) %>% count() %>% rename('Cantidad de mujeres'=n) %>%
  ungroup() %>%  select(-sexo)

h <- politicos %>% filter(partido %in%c('Partido Nacional', 'Frente Amplio', 'Partido Colorado',
                                        'Cabildo Abierto') & sexo == 1) %>%
  group_by(sexo, partido) %>% distinct(id_politico) %>% count() %>% rename('Cantidad de hombres'=n) %>%
  ungroup() %>% select(-sexo)

tabla_sexos <- inner_join(h,m,by =('partido'))

tabla_sexos <- tabla_sexos %>% mutate('Hombres por cada mujer' = `Cantidad de hombres`/`Cantidad de mujeres`) %>% 
   mutate('Hombres por cada mujer' = round(`Hombres por cada mujer`,0))

### Cantidad de mujeres por cargo (incluye suplencias)
m_1 <- politicos %>% filter(partido %in%c('Partido Nacional', 'Frente Amplio', 'Partido Colorado',
                                          'Cabildo Abierto') & sexo == 0 ) %>%
  group_by(sexo, cargo) %>% distinct(id_politico, cargo) %>% count() %>% rename('Cantidad de mujeres'=n) %>%
  ungroup() %>%  select(-sexo)

h_1 <- politicos %>% filter(partido %in%c('Partido Nacional', 'Frente Amplio', 'Partido Colorado',
                                          'Cabildo Abierto') & sexo == 1) %>%
  group_by(sexo, cargo) %>% distinct(id_politico, cargo) %>% count() %>% rename('Cantidad de hombres'=n) %>%
  ungroup() %>% select(-sexo)

tabla_sexos_cargos <- left_join(h_1,m_1,by =('cargo'))
tabla_sexos_cargos <- tabla_sexos_cargos %>% mutate('Hombres por cada mujer' = `Cantidad de hombres`/`Cantidad de mujeres`)%>% 
  mutate('Hombres por cada mujer' = round(`Hombres por cada mujer`,0))


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
    status          = "Estatus"
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

aplicar_etiquetas <- function(df) {
  names(df) <- var_label(df)
  df
}




ui <- dashboardPage(
  dashboardHeader(title = "Visualizador de políticos y políticas del Uruguay"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Búsqueda en la base", tabName = "busqueda", icon = icon("search")),
      menuItem("Tablas", tabName = "tablas", icon = icon("table")),
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
                  title = "Búsqueda en la base",
                  solidHeader = TRUE,
                  status = "primary",
                  
                  tabsetPanel(
                    tabPanel("Filtro por Legislatura",
                             selectInput("legislatura",
                                         "Seleccionar Legislatura",
                                         choices = sort(unique(politicos$legislatura))),
                             DTOutput("tabla_leg")
                    ),
                    
                    tabPanel("Filtro por Partido",
                             selectInput("partido",
                                         "Seleccionar Partido Político",
                                         choices = sort(unique(politicos$partido))),
                             DTOutput("tabla_part")
                    ),         
                    
                    tabPanel("Filtro por Cargo",
                             selectInput("cargo",
                                         "Seleccionar cargo",
                                         choices = sort(unique(politicos$cargo))),
                             DTOutput("tabla_cargos"))         
                  )
                )
              )
      ),
      
      # --- pestana 2: Tablas ---
      tabItem(tabName = "tablas",
              fluidRow(
                box(
                  width = 12,
                  title = "Tablas",
                  solidHeader = TRUE,
                  status = "primary",
                  
                  tabsetPanel(
                    tabPanel("Cantidad de mujeres por partido",
                             DTOutput("tabla_mujeres_partido")
                    ),
                    tabPanel("Cantidad de mujeres por cargo",
                             DTOutput("tabla_mujeres_cargo")
                    ),
                    tabPanel("Tabla de Cargos",
                             DTOutput("tabla_cargos_detalle")
                    )
                  )
                )
              )
      )
    )
  )
)



server <- function(input, output, session) {
  
  output$tabla_leg <- renderDT({
    df <- politicos %>%
      filter(legislatura == input$legislatura) %>%
      select(primer_apellido, primer_nombre, id_politico, partido, cargo, fecha_inicio, fecha_fin)
    datatable(aplicar_etiquetas(df), filter = "top", rownames = FALSE)    
  })
  
  output$tabla_part <- renderDT({
    df <- politicos %>%
      filter(partido == input$partido) %>%
      select(primer_apellido, primer_nombre, id_politico, cargo, fecha_inicio, fecha_fin)
    datatable(aplicar_etiquetas(df), filter = "top", rownames = FALSE)
    
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
  
  output$tabla_mujeres_cargo <- renderDT({
    df <- tabla_sexos_cargos
    datatable(aplicar_etiquetas(df), rownames = FALSE)
  })
  
}

shinyApp(ui = ui, server = server)






library(bslib)

# Definir la interfaz de usuario
ui <- page_sidebar(
  title = "Datos Políticos de Uruguay",
  
  # Barra lateral con enlaces
  sidebar = sidebar(
    navlistPanel(
      "Secciones",
      tabPanel("Inicio", icon = icon("home")),
      tabPanel("Elecciones", icon = icon("vote-yea")),
      tabPanel("Opinión Pública", icon = icon("comments"))
    )
  ),
  
  # Contenido principal
  main = div(
    h2("Bienvenida/o a la app de datos políticos de Uruguay"),
    p("Esta aplicación permite explorar datos de elecciones y opinión pública desde 1989 hasta la actualidad."),
    p("Utiliza los menús laterales para navegar entre las diferentes secciones.")
  )
)

# Definir la lógica del servidor
server <- function(input, output, session) {
  # Aquí se puede agregar la lógica para renderizar gráficos, tablas, etc.
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)
