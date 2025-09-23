library(shiny)
library(dplyr)
library(ggplot2)

# ----------------------------
# Datos de ejemplo (inventados)
# ----------------------------
set.seed(123)
df <- data.frame(
  id = 1:100,
  partido = sample(c("Partido A", "Partido B", "Partido C"), 100, replace = TRUE),
  cargo = sample(c("Diputado", "Senador", "Intendente"), 100, replace = TRUE),
  edad = sample(25:75, 100, replace = TRUE),
  votos = sample(1000:10000, 100, replace = TRUE)
)

# ----------------------------
# UI (Interfaz)
# ----------------------------
ui <- fluidPage(
  titlePanel("Ejemplo de Shiny App con datos políticos"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("partidoInput", "Selecciona un partido:",
                  choices = unique(df$partido),
                  selected = "Partido A"),
      selectInput("cargoInput", "Selecciona un cargo:",
                  choices = unique(df$cargo),
                  selected = "Diputado")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Tabla", dataTableOutput("tabla")),
        tabPanel("Gráfico", plotOutput("grafico"))
      )
    )
  )
)

# ----------------------------
# SERVER (Lógica)
# ----------------------------
server <- function(input, output) {
  
  # Filtrado reactivo según inputs
  datos_filtrados <- reactive({
    df %>%
      filter(partido == input$partidoInput,
             cargo == input$cargoInput)
  })
  
  # Tabla
  output$tabla <- renderDataTable({
    datos_filtrados()
  })
  
  # Gráfico
  output$grafico <- renderPlot({
    datos_filtrados() %>%
      ggplot(aes(x = edad, y = votos)) +
      geom_point(color = "blue") +
      geom_smooth(method = "lm", se = FALSE, color = "red") +
      labs(title = paste("Relación edad-votos en", input$partidoInput),
           x = "Edad", y = "Votos")
  })
}

# ----------------------------
# Lanzar la App
# ----------------------------
shinyApp(ui = ui, server = server)

