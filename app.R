# Librerías
libraries <- c("shiny", "shinydashboard", "shinycssloaders", "shinyWidgets", "FactoMineR", "FactoClass", "DataExplorer","tidyr",
               "openxlsx", "dplyr", "RColorBrewer", "sparkline","DT","htmlwidgets","ggplot2","GGally")

# Instala los paquetes si no están instalados
install.packages(setdiff(libraries, rownames(installed.packages())), dependencies = TRUE)

# Cargar librerías
lapply(libraries, library, character.only = TRUE)

# Define la interfaz de usuario de Shiny

  # Encabezado --------------------------------------------------------------
  header <- dashboardHeader( title="DesCo Descripción de Variables Continuas" )

  # Sidebar -----------------------------------------------------------------
  sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem("Tabla de Datos Original", tabName = "datos_ori", icon = icon("table")),
      menuItem("Descripción - Validación", tabName = "desc", icon = icon("cog", lib = "glyphicon"))
    )
  )
  
  # Cuerpo ------------------------------------------------------------------
body <- dashboardBody(
  tabItems(
    # Primer tabItem
    tabItem(
      tabName = "datos_ori",
      h2("Carga de fichero CSV"), 
      box(
        width = 12,
        h5("La tabla CSV DEBE tener en 1º coluna: etiqueta de observaciones ID"), 
        materialSwitch(inputId = "header", label = "El archivo tiene encabezado", value = TRUE),
        materialSwitch(inputId = "nominales", label = "La tabla tiene Variables Categóricas Nominales", value = FALSE),
        radioButtons("sep", "Separador de campos:", choices = c(Coma = ",", Punto_y_Coma = ";", Tabulador = "\t"), selected = ";"),
        radioButtons("dec", "Separador decimal:", choices = c(Coma = ",", Punto = "."), selected = '.'),
        fileInput("file", "Selecciona un archivo CSV:", accept = ".csv")
      ),
      h2("Datos Originales"),
      # Visualización de estadísticas y datos originales
      box(width = 12,
          h3("Estadísticas de las Variables Cuantitativas"),
          verbatimTextOutput("estadi"),
          box(width = 12, 
              plotOutput("corre"),
              conditionalPanel(
                condition = "input.nominales == true",
                selectInput("catVar", "Seleccione la variable categórica para Colorear Grafico:", choices = NULL)
              )
          ),
          conditionalPanel(
            condition = "input.nominales == true",
            h3("Estadísticas de las Variables Cualitativas"),
            plotOutput("bar_char")
          )
      ),
      h3("Tabla de Datos"),
      DTOutput("originalTable", height = 700)
    ),
    # Segundo tabItem
    tabItem(
      tabName = "desc",
      h3("Caracterización de Variables Cuantitativas"),
      selectInput("variable", "Selección Variable a describir:", choices = NULL),
      box(
        width = 12,
        pickerInput(
          inputId = "continua", 
          label = "Selección Variable descriptiva continua:", 
          choices = NULL, 
          options = pickerOptions(
            actionsBox = TRUE, 
            size = 5,
            selectedTextFormat = "count > 3"
          ), 
          multiple = TRUE
        ),
        conditionalPanel(
          condition = "input.nominales == true", 
          pickerInput(
            inputId = "nominal", 
            label = "Selección Variable descriptiva nominal:", 
            choices = NULL, 
            options = pickerOptions(
              actionsBox = TRUE, 
              size = 5,
              selectedTextFormat = "count > 3"
            ), 
            multiple = TRUE
          )
        ),
        box(
          width = 6,
          actionButton("desco", "Describir")
        ), 
        h3(" "),
        box(
          width = 12,
          h3("Caracterización"), 
          textOutput("desco_co")
        )
      )
    )
  )
)


## App completo ----------------------------------------------------------------
ui <- dashboardPage(
  skin = "green",  # Establece el tema de color del dashboard a verde
  header,  # Define el encabezado del dashboard
  sidebar,  # Define la barra lateral del dashboard
  body  # Define el cuerpo del dashboard
)

 server <- function(input, output) {
   datos <- reactiveVal(NULL)
   factores <- reactiveVal(NULL)
   df <- reactiveVal(NULL)
   resultados <- reactiveVal()
   seleccion <- reactiveVal()  # Define la variable reactiva al principio del server
   
   # Cargar datos
   # Observa el evento de carga de un archivo (input$file)
   observeEvent(input$file, {
     tryCatch({
       # Carga el archivo CSV y actualiza el dataframe reactivo 'df'
       df( read.csv(input$file$datapath, 
                    header = input$header, 
                    dec = input$dec, 
                    sep = input$sep, 
                    stringsAsFactors = TRUE, 
                    row.names = 1 ))
       # Extrae columnas que son factores y numéricas separadamente, 
       fact <- as.data.frame(df()[, sapply(df(), is.factor), drop=FALSE])
       n <-as.data.frame(df()[, sapply(df(), function(x) is.numeric(x)  && !is.factor(x)), drop = FALSE])
       
       # Actualiza la interfaz las listas de seleccion de variables
       updateSelectInput(getDefaultReactiveDomain(), inputId = "variable", choices = names(n), selected = NULL)
       updatePickerInput(getDefaultReactiveDomain(), inputId = "continua", choices = names(n), selected = NULL)
       updatePickerInput(getDefaultReactiveDomain(), inputId = "nominal", choices = names(fact), selected = NULL) 
       updateSelectInput(getDefaultReactiveDomain(), inputId = "catVar", choices = c(names(fact),"Ninguna"), selected = "Ninguna")
       # Guarda los factores y datos separados en sus respectivas variables reactivas
       factores(fact)
       datos(n)
     }, error = function(e) {
       # Maneja errores en la carga del archivo mostrando un diálogo modal con el mensaje de error
       showModal(modalDialog(
         title = "Error en la Carga del fichero CSV ",
         paste("Se ha producido un error al cargar y preprocesamiento del fichero:  ", e$message),
         easyClose = TRUE
       ))
     })
   })
   
   # Mostrar la tabla de datos original
   output$originalTable <- DT::renderDataTable({
     req(input$file)  # Requiere que se haya cargado un archivo
     
     # Asegura que ambos data frames están disponibles y tienen el mismo número de filas
     if (is.null(datos()) || is.null(factores())) {
       stop("Datos o factores están vacíos.")
     }
     if (nrow(datos()) != nrow(factores())) {
       stop("Datos y factores no tienen el mismo número de filas.")
     }
     
     # Asegura que ambos sean data frames
     if (!is.data.frame(datos()) || !is.data.frame(factores())) {
       stop("Datos o factores no son data frames.")
     }
     combined_data <- cbind(datos(), factores())  # Combina las columnas cuantitativas y cualitativas
     
     DT::datatable(
       combined_data,
       options = list(scrollX = TRUE)  # Habilita el desplazamiento horizontal
     )
   })
   
   # Mostrar Estadísticas de la tabla numérica
   output$estadi <- renderPrint({
     # Requiere que se haya cargado un archivo (input$file)
     req(input$file)
     # Genera un resumen estadístico de las columnas cuantitativas
     summary(datos())
   })
   
   # Genera y muestra una matriz de correlación de variables cuantitativas
   output$corre <- renderPlot({
     req(input$file)
     tryCatch({
       # Ajustes según la entrada de la variable categórica
       if (input$nominales != FALSE && input$catVar !="Ninguna") {
         colorVar <- input$catVar
         # Creación del gráfico Diagrama de pares con ggpairs
         ggpairs(datos(),
                 title = "Diagrama de pares", axisLabels = "show",
                 aes(color = factores()[[colorVar]], alpha = 0.5),
                 lower = list(continuous = "smooth") )
       } else {
         # Creación del gráfico Diagrama de pares con ggpairs
         ggpairs(datos(),
                 title = "Diagrama de pares", axisLabels = "show",
                 lower = list(continuous = "smooth") )
       }
     }, error = function(e) {
       showModal(modalDialog(
         title = "Error en cálculo de matriz de correlación ",
         paste("Se ha producido un error al realizar el cálculo y grafico de las correlaciones entre variables numéricas: ", e$message),
         easyClose = TRUE
       ))
     })
   }) 
   
   # Renderiza gráficos de barras para las distribuciones de las variables categóricas.
   output$bar_char <- renderPlot({
     req(input$nominales)
     if(input$nominales){
       # Gráfico de barras con xray::distributions(data frame factores)
       # xray::distributions(datos()$cuali)
       # Calcular frecuencias para cada factor y almacenar en una lista de tablas
       list_of_tables <- lapply(factores(), table)
       # Convertir la lista de tablas a un dataframe
       frequency_data <- bind_rows(lapply(names(list_of_tables), function(x) {
         data.frame(Factor = x,
                    Level = names(list_of_tables[[x]]),
                    Frequency = as.vector(list_of_tables[[x]]),
                    stringsAsFactors = FALSE)
       }), .id = "Variable")
       # Graficar las frecuencias usando ggplot2
       ggplot(frequency_data, aes(x = Level, y = Frequency, fill = Level)) +
         geom_bar(stat = "identity") +
         facet_wrap(~ Factor, scales = "free_x") +
         theme_minimal() +
         labs(title = "Frecuencia ocurrencia de cada Variables Nominales",
              x = "Modalidades o Categorias",
              y = "Frecuencia") +
         theme(axis.text.x = element_text(angle = 45, hjust = 1))
     }
   })
   observeEvent(input$desco, {
     # Evitar acción si la variable principal está sin selección
     if (!is.null(input$variable)) {
       # Preparar selección de data frames según condiciones
       # Asegúrate de que todos los componentes son data frames no vacíos antes de combinarlos
       df_a_desc <- df()[, input$variable, drop = FALSE]
       # Para 'continua'
       df_cont <- if (!is.null(input$continua) && length(input$continua) > 0) {
         selected_cols <- intersect(names(df()), input$continua)  # Asegura que las columnas existan en df()
         if (length(selected_cols) > 0) {
           df()[, selected_cols, drop = FALSE]
         } else {
           NULL  # No hay columnas válidas seleccionadas
         }
       } else {
         NULL  # No hay selección
       }
       # Para 'nominal'
       df_nom <- if (!is.null(input$nominal) && length(input$nominal) > 0) {
         selected_cols = intersect(names(df()), input$nominal)  # Asegura que las columnas existan en df()
         if (length(selected_cols) > 0) {
           df()[, selected_cols, drop = FALSE]
         } else {
           NULL  # No hay columnas válidas seleccionadas
         }
       } else {
         NULL  # No hay selección
       }
       # Inicializa 'seleccion' con 'df_a_desc' si no es NULL
       if (!is.null(df_a_desc)) {
         seleccion(df_a_desc)  # Establece el valor inicial de 'seleccion'
       }
       # Actualización condicional con 'df_cont'
       if (!is.null(df_cont)) {
         seleccion(cbind(seleccion(), df_cont))  # Combina y actualiza 'seleccion'
       }
       # Actualización condicional con 'df_nom'
       if (!is.null(df_nom)) {
         seleccion(cbind(seleccion(), df_nom))  # Combina y actualiza 'seleccion'
       }
       # Función para describir las variables, asegurarse de que seleccion tiene mmas de una var
       if (ncol(seleccion()) > 1) {
         resDesco <- condes(seleccion(), num.var = 1)
         resultados(resDesco)
       }
     }
     # Resultados Vtest en cuadro de texto
     output$desco_co <- renderText({
       req(resultados())
       capture.output(print(resultados()))
     })
   })

  # Detiene la aplicación Shiny cuando se presiona el botón de salida
      observeEvent(input$exit_btn, {
        stopApp()  
      })
}

shinyApp(ui, server)