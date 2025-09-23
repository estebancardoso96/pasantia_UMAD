library(shiny)
library(dplyr)
library(ggplot2)

# ----------------------------
# Datos 
# ----------------------------

df_final = politicos


library(shiny)
library(dplyr)
library(ggplot2)

# ----------------------------
# UI (Interfaz)
# ----------------------------
ui <- fluidPage(
  
  # CSS personalizado para fondo celeste
  tags$head(
    tags$style(HTML("
      body {
        background-color: #E6F2FA; /* celeste claro */
        color: #003366; /* azul oscuro para contraste */
      }
      .well {
        background-color: #ffffffcc; 
        border: 1px solid #b3d9ff;
        border-radius: 10px;
      }
      .tab-pane, .tab-content {
        background-color: #ffffffdd;
        padding: 15px;
        border-radius: 10px;
      }
      h2, h3, h4 {
        color: #004080;
      }
    "))
  ),
  
  titlePanel("Visualizador de trayectorias de políticos y políticas del Uruguay"),
  
  sidebarLayout(
    sidebarPanel(width = 3,
                 
                 # Mostrar filtros distintos según pestaña activa
                 conditionalPanel(
                   condition = "input.tabs == 'Buscador'",
                   selectInput("legislaturaInput", "Selecciona una legislatura:",
                               choices = sort(unique(df_final$legislatura)),  
                               selected = min(df_final$legislatura)),
                   selectInput("partidoInput", "Selecciona un partido:",
                               choices = unique(df_final$partido),
                               selected = "Partido Colorado"),
                   selectInput("cargoInput", "Selecciona un cargo:",
                               choices = unique(df_final$cargo),
                               selected = "Diputado")
                 ),
                 
                 conditionalPanel(
                   condition = "input.tabs == 'Tablas'",
                   selectInput("legislaturas_agrupadasTabla", "Filtrar período:",
                               choices = unique(df_final$legislaturas_agrupadas),
                               selected = "1902-1933")
                 )
    ),
    
    mainPanel(
      tabsetPanel(
        id = "tabs",  # ID para detectar pestaña activa
        tabPanel("Buscador", dataTableOutput("buscador")),
        tabPanel("Tablas", dataTableOutput("tabla"))
      )
    )
  )
)

# ----------------------------
# SERVER (Lógica)
# ----------------------------
server <- function(input, output, session) {options(digits = 2)
  
  # --- Datos para el buscador ---
  datos_tabla <- reactive({
    df_final %>% 
      select(-c(segundo_nombre,segundo_apellido,fuente, fecha_intermedia,sexo,
                fecha_inicio_l, fecha_fin_l)) %>% 
      filter(partido == input$partidoInput,
             cargo == input$cargoInput,
             legislatura == input$legislaturaInput)
  })
  
  # Buscador
  output$buscador <- renderDataTable({
    datos_tabla() %>%
      select(-legislatura, -partido, -cargo, -fecha_nac, -ed_asumir_1, -ed_asumir, -legislaturas_agrupadas)
  })
  
  # --- Datos para la tabla ---
  output$tabla <- renderDataTable({
    df_final %>%
      filter(legislaturas_agrupadas == input$legislaturas_agrupadasTabla) %>% 
      group_by(cargo) %>% 
      summarise(
        promedio = round(mean(ed_asumir, na.rm = TRUE),2),
        desvio_estandar = round(sd(ed_asumir, na.rm = TRUE),2)
      ) %>% 
      ungroup() 
  })
  
}

# ----------------------------
# Lanzar la App
# ----------------------------
shinyApp(ui = ui, server = server)







