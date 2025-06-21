# -----------------------------------------------------------------------------
# |                        Aplicación Shiny para Mapa de Centros Educativos   |
# |                          (Versión 5.9 - Tabla Avanzada)                   |
# -----------------------------------------------------------------------------

# --- 1. Carga de Librerías ---
# install.packages(c("shiny", "bslib", "leaflet", "dplyr", "readxl", "htmltools", "DT", "tidyr"))

library(shiny)
library(bslib)
library(leaflet)
library(dplyr)
library(readxl)
library(htmltools)
library(DT)
library(tidyr)

# --- 2. Lectura de Datos desde Archivo Excel ---
tryCatch({
  mapa_data_raw <- readxl::read_excel("mapa_mmm_muestra_v2.xlsx")
}, error = function(e) {
  stop("Error al leer el archivo Excel. Asegúrate de que exista en el directorio de la app. Error original: ", e$message)
})


# --- 3. Limpieza y Preparación de los Datos ---
mapa <- mapa_data_raw %>%
  mutate(
    latitud = suppressWarnings(as.numeric(latitud)),
    longitud = suppressWarnings(as.numeric(longitud)),
    departamento = tidyr::replace_na(departamento, "No especificado"),
    distrito = tidyr::replace_na(distrito, "No especificado"),
    cluster = tidyr::replace_na(as.character(cluster), "No especificado")
  ) %>%
  filter(!is.na(latitud) & !is.na(longitud)) %>%
  filter(sede_codigo != 86027)


# --- 4. Definición de la Interfaz de Usuario (UI) ---
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "cerulean"),
  fluid = TRUE,
  
  h1("Mapa Interactivo de Centros Educativos", align = "center"),
  
  div(style = "text-align: center; margin-bottom: 10px;",
      a(img(src = "https://i.postimg.cc/Sxm8Xdx1/Logos.png", height = 50)),hr(),
      helpText("Elaborado por EUROLATINA con datos del MINEDUCYT. Nota: Los filtros son responsivos a la selección.")
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 2,
      h4("Filtros"),
      hr(),
      selectInput(
        inputId = "filtro_muestra",
        label = "¿En muestra?",
        choices = c("Todos" = "todos", "Sí" = "1", "No" = "0"),
        selected = "todos"
      ),
      selectInput(
        inputId = "filtro_depto",
        label = "Depto:",
        choices = c("Todos" = "todos", sort(unique(mapa$departamento)))
      ),
      selectInput(inputId = "filtro_distrito", label = "Distrito:", choices = NULL),
      selectInput(inputId = "filtro_cluster", label = "Clúster:", choices = NULL)
    
     
      ),
    mainPanel(
      leafletOutput("mapa_centros", height = "80vh"),
      hr(),
      h4("Datos de los Centros Educativos Filtrados"),
      DT::dataTableOutput("tabla_datos")
    )
  )
)


# --- 5. Definición de la Lógica del Servidor (Server) ---
server <- function(input, output, session) {
  
  # --- Lógica de los Filtros ---
  observeEvent(input$filtro_depto, {
    if (input$filtro_depto == "todos") {
      opciones_distrito <- c("Todos" = "todos", sort(unique(mapa$distrito)))
    } else {
      opciones_distrito <- mapa %>%
        filter(departamento == input$filtro_depto) %>%
        pull(distrito) %>% unique() %>% sort()
      opciones_distrito <- c("Todos" = "todos", opciones_distrito)
    }
    updateSelectInput(session, "filtro_distrito", choices = opciones_distrito)
  })
  
  observe({
    data_filtrada_cluster <- mapa
    if (input$filtro_depto != "todos") {
      data_filtrada_cluster <- data_filtrada_cluster %>% filter(departamento == input$filtro_depto)
    }
    if (!is.null(input$filtro_distrito) && input$filtro_distrito != "todos") {
      data_filtrada_cluster <- data_filtrada_cluster %>% filter(distrito == input$filtro_distrito)
    }
    opciones_cluster <- c("Todos" = "todos", sort(unique(data_filtrada_cluster$cluster)))
    updateSelectInput(session, "filtro_cluster", choices = opciones_cluster)
  })
  
  datos_filtrados <- reactive({
    data_a_filtrar <- mapa
    if (input$filtro_muestra != "todos") {
      data_a_filtrar <- data_a_filtrar %>% filter(EntraMuestreo == as.numeric(input$filtro_muestra))
    }
    if (input$filtro_depto != "todos") {
      data_a_filtrar <- data_a_filtrar %>% filter(departamento == input$filtro_depto)
    }
    if (!is.null(input$filtro_distrito) && input$filtro_distrito != "todos") {
      data_a_filtrar <- data_a_filtrar %>% filter(distrito == input$filtro_distrito)
    }
    if (!is.null(input$filtro_cluster) && input$filtro_cluster != "todos") {
      data_a_filtrar <- data_a_filtrar %>% filter(cluster == input$filtro_cluster)
    }
    return(data_a_filtrar)
  })
  
  # --- Salidas (Mapa y Tabla) ---
  output$mapa_centros <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap, group = "Mapa") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satélite") %>%
      setView(lng = -88.89653, lat = 13.794185, zoom = 9) %>%
      addLayersControl(
        baseGroups = c("Mapa", "Satélite"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  observe({
    df <- datos_filtrados()
    popup_content <- paste0(
      "<b>Código:</b> ", htmlEscape(df$sede_codigo), "<br>",
      "<b>Centro Educativo:</b> ", htmlEscape(df$nombre_del_centro_educativo), "<br>",
      "<b>Departamento:</b> ", htmlEscape(df$departamento), "<br>",
      "<b>Distrito:</b> ", htmlEscape(df$distrito), "<br>",
      "<b>Dirección:</b> ", htmlEscape(df$sede_direccion), "<br>",
      "<b>Director:</b> ", htmlEscape(df$Director), "<br>",
      "<b>Email:</b> ", htmlEscape(df$Correo_CE), "<br>",
      "<b>Telf.:</b> ", htmlEscape(df$TEL.), "<br>",
      "<b>Clúster:</b> ", htmlEscape(df$cluster), "<br>",
      "<b>Cohorte:</b> ", htmlEscape(df$GrupoN), "<br>",
      "<b>Grupo Tratamiento: </b> ", htmlEscape(df$GruposT), "<br>",
      "<b>Docentes PI:</b> ", htmlEscape(df$total_CuentaProfesor_recod), "<br>",
      "<b>Matrícula PI:</b> ", htmlEscape(df$MatriculaP4_PrimerG), "<br>",
      "<b>¿Entra a Muestra? Sí = 1:</b> ", htmlEscape(df$EntraMuestreo), "<br>",
      "<b>¿Sobremuestreo?  Sí = 2;(1 muestra):</b> ", htmlEscape(df$SobreMuestreo), "<br>",
      "<b>Cuota de Muestreo:</b> ", htmlEscape(df$Cuota_Muestreo_Docentes)
    )
    leafletProxy("mapa_centros", data = df) %>%
      clearMarkers() %>%
      clearMarkerClusters() %>%
      addMarkers(
        lng = ~longitud, lat = ~latitud,
        popup = ~popup_content,
        clusterOptions = markerClusterOptions()
      )
  })
  
  output$tabla_datos <- DT::renderDataTable({
    df_tabla <- datos_filtrados() %>%
      select(
        `Código` = sede_codigo,
        `Nombre del Centro` = nombre_del_centro_educativo,
        Departamento = departamento,
        Distrito = distrito,
        Clúster = cluster,
        `En Muestra` = EntraMuestreo,
        `Sobre Muestreo` = SobreMuestreo,
        `Profesores PI` = total_CuentaProfesor_recod,
        `Matrícula Total` = total_matricula
      )
    
    # ## MODIFICADO ##: Se integran las nuevas características a la tabla
    DT::datatable(
      df_tabla,
      class = 'cell-border stripe',
      rownames = FALSE,
      filter = "top", # <-- Filtros por columna
      options = list(
        pageLength = 5,
        #scrollX = TRUE, # <-- Scroll horizontal para que los filtros funcionen bien
        searching = TRUE, # <-- Búsqueda global
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
      )
    )
  })
}

# --- 6. Ejecución de la Aplicación ---
shinyApp(ui = ui, server = server)
