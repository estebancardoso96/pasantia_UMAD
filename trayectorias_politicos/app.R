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

ui <- dashboardPage(
  dashboardHeader(title = "Visualizador de políticos y políticas del Uruguay"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Búsqueda en la base", tabName = "busqueda", icon = icon("search"))
    )
  ),
  
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
              )
      )
    )
  )


server <- function(input, output, session) {
  
  output$tabla_leg <- renderDT({
    politicos %>%
      filter(legislatura == input$legislatura) %>%
      select(primer_apellido, primer_nombre, id_politico, partido, cargo, fecha_inicio, fecha_fin) %>%
      datatable(filter = "top", rownames = FALSE)
  })
  
  output$tabla_part <- renderDT({
    politicos %>%
      filter(partido == input$partido) %>%
      select(primer_apellido, primer_nombre, id_politico, cargo, fecha_inicio, fecha_fin) %>%
      datatable(filter = "top", rownames = FALSE)
  
  })
  
  output$tabla_cargos <- renderDT({
    politicos %>%
      filter(cargo == input$cargo) %>%
      select(primer_apellido, primer_nombre, id_politico,partido,status, fecha_inicio, fecha_fin) %>%
      datatable(filter = "top", rownames = FALSE)
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
