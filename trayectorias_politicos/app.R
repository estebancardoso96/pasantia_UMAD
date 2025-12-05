ui <- dashboardPage(
  dashboardHeader(
    title = span(
      img(
        # src = "logo_fcs_umad.png",
        height = "40px",
        style = "margin-right:10px;"
      ),
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
    
    # --- Estilos para el tooltip ---
    tags$style(HTML("
    .tooltip-box {
      position: relative;
      display: inline-block;
      cursor: pointer;
      font-size: 20px;
      color: #1e88e5;
      margin-left: 10px;
    }

    .tooltip-box .tooltip-content {
      visibility: hidden;
      width: 420px;
      background-color: white;
      color: #2c3e50;
      text-align: left;
      padding: 15px;
      border-radius: 10px;
      position: absolute;
      z-index: 999;
      top: 28px;
      left: 0;
      box-shadow: 0px 4px 20px rgba(0,0,0,0.15);
      font-size: 15px;
      line-height: 1.4;
    }

    .tooltip-box:hover .tooltip-content {
      visibility: visible;
    }
  ")),  
    
    # --- Estilos para el título ---
    tags$head(
      # Título del documento (lo que aparece en la pestaña del navegador)
      tags$title("Visualizador de políticos y políticas del Uruguay"),
      
      # Por si algún fragmento de HTML estaba sobreescribiendo el title: refuerza con JS
      tags$script(HTML("
    document.title = 'Visualizador de políticos y políticas del Uruguay';
  ")),
      
      # (Opcional) estilos globales para tu h2 si los quieres mantener en el head
      tags$style(HTML("
    h2.dashboard-title {
      text-align: center;
      font-weight: bold;
      font-size: 28px;
      color: #2c3e50;
      margin-bottom: 25px;
      margin-top: 10px;
    }
  "))
    ),
    
    # --- Título principal ---
    h2("Visualizador interactivo de políticos y políticas del Uruguay (1902-2025)", 
       class = "dashboard-title"),
    
    
    # ----------------------------------------------------
    #  TAB ITEMS 
    # ----------------------------------------------------
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
                             DTOutput("tabla_base"),
                             div(style = "margin-top: 20px; text-align: center;",
                                 downloadButton("descargar_base_completa", "Descargar base completa CSV"))),
                    
                    tabPanel("Filtro por Legislatura",
                             selectInput("legislatura", "Seleccionar Legislatura",
                                         choices = sort(unique(politicos$legislatura)),
                                         selected = max(politicos$legislatura, na.rm = TRUE),
                                         width = "180px"),
                             DTOutput("tabla_leg"),
                             div(style = "margin-top: 20px; text-align: center;",
                                 downloadButton("descargar_tabla_leg", "Descargar CSV"))),
                    
                    tabPanel("Filtro por Partido",
                             selectInput("partido", "Seleccionar Partido Político",
                                         choices = sort(unique(politicos$partido)),
                                         width = "180px"),
                             DTOutput("tabla_part"),
                             div(style = "margin-top: 20px; text-align: center;",
                                 downloadButton("descargar_tabla_partido", "Descargar CSV"))),
                    
                    tabPanel("Filtro por Cargo",
                             selectInput("cargo", "Seleccionar cargo",
                                         choices = sort(unique(politicos$cargo)),
                                         width = "200px"),
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
                                         choices = sort(unique(tabla_sexos_leg$legislatura)),
                                         width = "180px"),
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
                                         choices = sort(unique(tabla_edad$legislaturas_agrupadas)),
                                         width = "180px"),
                             DTOutput("edades_cargo"),
                             div(style = "margin-top: 20px; text-align: center;",
                                 downloadButton("descargar_tabla_edades", "Descargar CSV"))),
                    
                    tabPanel("Edad promedio por partido de los legisladores",
                             selectInput("legislatura_partido",
                                         "Seleccionar Legislatura",
                                         choices = sort(unique(tabla_edad_legislatura$legislatura)),
                                         width = "180px",
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
                             # Tooltip
                             div(
                               class = "tooltip-box",
                               icon("circle-info"),
                               div(
                                 class = "tooltip-content",
                                 HTML("
                            <b>Nota:</b><br>
                            Los legisladores exitosos son diputados o senadores electos en seis o más legislaturas diferentes.
                          ")
                               )
                             ),
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
                             # --- ESTILO PARA EL SELECT INPUT (GRISES) ---
                             tags$head(
                               tags$style(HTML("
                               /* selectInput estándar */
select {
  background-color: #4A90E2 !important;    /* azul suave */
  color: #ffffff !important;               /* texto blanco */
  border: 1px solid #3b78ba !important;    /* borde ligeramente más oscuro */
}

option {
  background-color: #ffffff !important;
  color: #333333 !important;
}

/* Estilos para selectizeInput (Shiny por defecto) */
.selectize-input {
  background-color: #4A90E2 !important;    /* azul suave */
  color: #ffffff !important;               /* texto blanco */
  border: 1px solid #3b78ba !important;
}

.selectize-input input {
  color: #ffffff !important;               /* texto blanco dentro del input */
}

.selectize-dropdown {
  background-color: #f0f6ff !important;    /* azul muy claro para dropdown */
  color: #333333 !important;
  border: 1px solid #3b78ba !important;
}

.selectize-dropdown .option {
  background-color: #ffffff !important;
  color: #333333 !important;
}

.selectize-dropdown .active {
  background-color: #4A90E2 !important;    /* mismo azul suave para hover */
  color: #ffffff !important;
}

 
    "))
                             ),
                             selectInput("leg_status",
                                         "Seleccionar status",
                                         choices = sort(unique(grafico_sexo_leg$status)),
                                         width = "180px",
                                         selected = max(grafico_sexo_leg$status)),
                             plotlyOutput("grafico_sexo_leg_out"),
                             div(style = "margin-top: 20px; text-align: center;",
                                 downloadButton("descargar_grafico_sexo_leg", "Descargar CSV"))),
                    
                    tabPanel("Porcentaje de mujeres legisladoras por partido y legislatura",
                             div(
                               class = "tooltip-box",
                               icon("circle-info"),
                               div(
                                 class = "tooltip-content",
                                 HTML("
                            <b>Nota:</b><br>
                            Se decidió quitar a una senadora del Partido Comunista y a una diputada de la Unión Popular para una mejor visualización de los datos dado que distorsionaban el eje.
                          ")
                               )
                             ),
                             selectInput(
                               "cargo",
                               "Seleccionar cargo",
                               choices = c("Senador", "Diputado"),
                               selected = "Diputado",
                               width = "180px"),
                             plotlyOutput("grafico_porcent_mujeres_leg"),
                             div(style = "margin-top: 20px; text-align: center;",
                                 downloadButton("descargar_grafico_sexo_leg", "Descargar CSV"))),
                    
                    tabPanel("Distribución por sexo de los cargos",
                             plotlyOutput("grafico_sexo_cargo"),
                             div(style = "margin-top: 20px; text-align: center;",
                                 downloadButton("descargar_grafico_edad_cargo", "Descargar CSV"))),
                    
                    tabPanel(
                      "Rotación parlamentaria por legislatura",
                      
                      # Título + tooltip
                      div(
                        style = "display: flex; align-items: center; gap: 10px; margin-bottom: 10px;",
                        
                        h3("Tasa de rotación parlamentaria (titulares)",
                           style = "margin: 0; padding: 0;"),
                        
                        # Tooltip
                        div(
                          class = "tooltip-box",
                          icon("book"),
                          div(
                            class = "tooltip-content",
                            HTML("
                            <b>Metodología:</b><br>
                            La tasa de rotación se calcula como el porcentaje de legisladores titulares nuevos (que no estuvieron en la legislatura n-1)
                            respecto al total de titulares que integran cada legislatura.
                          ")
                          )
                        )
                      ),
                      plotlyOutput("grafico_renovacion_titulares")
                    ),
                    
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
                  br(),
                  # Párrafo original
                  HTML("<b>Interpretación de la tabla y fuentes:</b>"),
                  br(),
                  p("Este visualizador se alimenta de dos fuentes: la base de datos del paquete de R puy (mantenido por la Unidad de Métodos y Acceso a Datos de la Facultad de Ciencias Sociales) y de datos obtenidos mediante web scraping de la página de la biblioteca del parlamento https://biblioteca.parlamento.gub.uy:8008/biografias/busqueda ",
strong("Cada fila de la tabla alojada en la pestaña de búsqueda representa un cargo o una candidatura de un político con una fecha de inicio y una fecha de fin,"),
 " teniendo información específica sobre el cargo, el partido por el cual asume (o se postula), la edad al asumir del político, el sexo y la circunscripción en caso de que corresponda. A su vez cada político se identifica con un id político único que permite seguir su trayectoria a lo largo de la base."),
                  
                  br(),
                  
                  # Nuevo bloque solicitado
                  HTML("<b>Creación y mantenimiento del visualizador:</b>"),
                  br(),
                  HTML('Esteban Cardoso y <a href="https://umad.cienciassociales.edu.uy/" target="_blank">UMAD</a>'),
                  
                  br(), br(),
                  
                  HTML("<b>Acceso del código (GitHub):</b>"),
                  br(),
                  HTML('<a href=\"https://github.com/estebancardoso96/pasantia_UMAD\" target=\"_blank\">https://github.com/estebancardoso96/pasantia_UMAD</a>'),
                  
                  br(), br(),
                  
                  HTML("<b>Contacto (consultas y notificación de errores):</b>"),
                  br(),
                  "esteban.cardoso96@gmail.com"
                )
              ),
              # 2. DICCIONARIO (completo)
              tabPanel(
                "Diccionario de variables",
                div(
                  style = "padding: 15px;",
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
                      tags$tr(tags$td("id_fuente"), tags$td("Sin etiqueta"), tags$td("Fuente de donde proviene el dato, 1 = paquete R puy ; 2 = web scraping de la biblioteca del parlamento")),
                      tags$tr(tags$td("fecha_nac"), tags$td("Sin etiqueta"), tags$td("Fecha de nacimiento del político/a")),
                      tags$tr(tags$td("fecha_inicio_l"), tags$td("Sin etiqueta"), tags$td("Fecha de inicio de la Legislatura que actúa el político")),
                      tags$tr(tags$td("fecha_fin_l"), tags$td("Sin etiqueta"), tags$td("Fecha de fin de la Legislatura que actúa el político")),
                      tags$tr(tags$td("fecha_intermedia"), tags$td("Sin etiqueta"), tags$td("Fecha intermedia (en la mitad entre la fecha de fin y la fecha de inicio de la Legislatura)")),
                      tags$tr(tags$td("ed_asumir"), tags$td("Sin etiqueta"), tags$td("Edad en la que asume el cargo el político calculada con la fecha de inicio del cargo")),
                      tags$tr(tags$td("ed_asumir_1"), tags$td("Sin etiqueta"), tags$td("Edad en la que asume el cargo calculada con la fecha intermedia cuando la fecha de inicio del cargo está missing")),
                      tags$tr(tags$td("edad_asumir"), tags$td("Sin etiqueta"), tags$td("Integración de ed_asumir con ed_asumir_1")),
                      tags$tr(tags$td("legislaturas_agrupadas"), tags$td("Sin etiqueta"), tags$td("Asume 3 valores que agrupan legislaturas: 1902-1933, 1934-1973 y 1985-2025")),
                      tags$tr(tags$td("inicio"), tags$td("Sin etiqueta"), tags$td("Año de inicio del cargo (datos obtenidos de la página del parlamento)")),
                      tags$tr(tags$td("fin"), tags$td("Sin etiqueta"), tags$td("Año de fin del cargo (datos obtenidos de la página del parlamento)")),
                      tags$tr(tags$td("cargo"), tags$td("Cargo"), tags$td("Cargo o candidatura")),
                      tags$tr(tags$td("cargo_vigente"), tags$td("Sin etiqueta"), tags$td("Asume SI si el cargo está vigente en la actualidad y NO si el cargo no está vigente en la actualidad")),
                      tags$tr(tags$td("legislatura"), tags$td("Legislatura"), tags$td("Legislatura en que se asume el cargo o se postula la candidatura"))
                    )
                  ),
                  div(
                    style = "text-align: center; margin-top: 20px;",
                    downloadButton("descargar_diccionario", "Descargar diccionario CSV")
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
             fecha_inicio, fecha_fin, legislatura) %>% distinct()
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
  
  grafico_porcent_mujeres <- reactive({
    politicos %>% 
      filter(cargo %in% c("Senador", "Diputado"),
             status == "Titular") %>% 
      group_by(partido, cargo, legislatura, sexo) %>% 
      summarise(n = n(), .groups = "drop_last") %>% 
      mutate(
        total_partido = sum(n),
        porcentaje = ifelse(sexo == 0, n / total_partido * 100, NA)
      ) %>% 
      filter(partido %in% c('Cabildo Abierto', 'Frente Amplio', 'Partido Colorado', 
                            'Partido Nacional') &
               !legislatura %in% c('1902-1905', '1905-1908', '1908-1911', '1911-1914', 
                                   '1914-1917', '1917-1920', '1920-1923', '1923-1926', 
                                   '1926-1929', '1929-1932', '1932-1933', '1934-1938',
                                   '1938-1942')) %>% 
      mutate(porcentaje = ifelse(is.na(porcentaje), 0, porcentaje)) %>% 
      select(partido, legislatura, cargo, porcentaje, n, sexo) %>% 
      mutate(n = ifelse(porcentaje == 0, 0, n)) %>% 
      
      # Paso 1: Identificar dónde hay mujeres con porcentaje > 0
      group_by(partido, legislatura, cargo) %>%
      mutate(
        # Verificar si existe fila de mujeres (sexo == 0) con porcentaje > 0
        hay_mujeres_con_porcentaje = any(sexo == 0 & porcentaje > 0)
      ) %>%
      ungroup() %>%
      
      # Paso 2: Filtrar - mantener mujeres siempre, hombres solo si no hay mujeres con porcentaje > 0
      filter(
        # Mantener todas las filas de mujeres
        sexo == 0 |
          # Mantener hombres solo si NO hay mujeres con porcentaje > 0
          (sexo == 1 & !hay_mujeres_con_porcentaje)
      ) %>%
      select(-hay_mujeres_con_porcentaje) %>% mutate(sexo = 0)
  })
  
  df_plot_2 <- reactive({
    req(input$cargo)
    grafico_porcent_mujeres() %>% 
      filter(cargo == input$cargo)
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
  
  # Porcentaje de mujeres legisladoras titualres
  output$grafico_porcent_mujeres_leg <- renderPlotly({
    
    df <- df_plot_2()
    
    partidos_presentes <- unique(df$partido)
    
    colores_usados <- colores_partidos[
      names(colores_partidos) %in% partidos_presentes
    ]
    plot_ly(
      data = df,
      x = ~legislatura,
      y = ~porcentaje,
      split = ~partido,
      color = ~partido,
      colors = colores_usados,
      type = "scatter",
      mode = "lines+markers",                
      marker = list(
        opacity = 0.5,                      
        size = 8
      ),
      line = list(
        width = 1,                           
        shape = "linear"                    
      ),
      text = ~paste0(
        "Partido: ", partido,
        "<br>Legislatura: ", legislatura,
        "<br>Porcentaje: ", round(porcentaje, 1), "%",
        "<br>Cantidad (n): ", n
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        title = "Porcentaje de mujeres legisladoras titulares sobre el total de \
legisladores por partido y legislatura",
        xaxis = list(
          title = "Legislatura",
          tickangle = 90
        ),
        yaxis = list(title = "% de mujeres sobre el total")
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
  # Descargar diccionario
  output$descargar_diccionario <- downloadHandler(
    filename = function() "diccionario_de_variables.csv",
    content = function(file) {
      write.csv(diccio_variables, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  # Descargar base completa
  output$descargar_base_completa <- downloadHandler(
    filename = function() "base_completa.csv",
    content = function(file) {
      write.csv(politicos, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
}

shinyApp(ui, server)
