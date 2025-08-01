library(shiny)
library(shinydashboard)
library(DT)
library(openxlsx)

rds_file <- "fichas_modelos.rds"
model_store <- reactiveVal(if (file.exists(rds_file)) readRDS(rds_file) else data.frame())

ui <- dashboardPage(
  dashboardHeader(title = "Ficha Técnica"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Nueva Ficha", tabName = "nueva", icon = icon("edit")),
      menuItem("Ver Fichas", tabName = "ver", icon = icon("table"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML(".dataTables_wrapper .dataTables_scroll { overflow: auto; }"))
    ),
    tabItems(
      tabItem(tabName = "nueva",
              fluidRow(
                box(title = "Complete la Ficha Técnica", width = 12, status = "primary", solidHeader = TRUE,
                    textInput("codigo", "Código Interno"),
                    textInput("nombre", "Nombre del Modelo"),
                    textInput("area", "Área Propietaria"),
                    textAreaInput("proposito", "Propósito", "", rows = 2),
                    selectInput("tipo_modelo", "Tipo de Modelo", c("Predictivo", "Descriptivo", "Clasificación", "Segmentación", "Otro")),
                    selectInput("algoritmo", "Algoritmo o Técnica", c("XGBoost", "Random Forest", "Regresión", "Red Neuronal", "Otro")),
                    textInput("predictoras", "Variables Predictoras"),
                    textInput("objetivo", "Variable Objetivo"),
                    textInput("lenguaje", "Lenguaje y Entorno"),
                    textInput("version", "Versión del Modelo"),
                    dateInput("fecha_desarrollo", "Fecha de Desarrollo"),
                    textInput("validador", "Validador"),
                    textInput("desarrollador", "Desarrollador(es)"),
                    textInput("metricas", "Métricas de Evaluación"),
                    textInput("dataset", "Dataset de Entrenamiento"),
                    textInput("segmento", "Segmento de Aplicación"),
                    textInput("uso", "Uso en procesos clave"),
                    selectInput("reentrenamiento", "Frecuencia de Reentrenamiento", c("Mensual", "Trimestral", "Anual", "Según desempeño")),
                    selectInput("estado", "Estado Actual", c("En desarrollo", "Validado", "En producción", "Retirado")),
                    textInput("ubicacion", "Ubicación del Código Fuente y Dataset"),
                    textInput("documentacion", "Documentación Complementaria"),
                    actionButton("guardar", "Guardar Ficha", class = "btn-success")
                )
              )
      ),
      tabItem(tabName = "ver",
              fluidRow(
                box(title = "Fichas Técnicas Registradas", width = 12, status = "info",
                    DTOutput("tabla_fichas"),
                    br(),
                    actionButton("eliminar", "Eliminar ficha seleccionada", class = "btn-danger"),
                    br(), br(),
                    downloadButton("descargar", "Descargar Excel")
                )
              )
      )
    )
  )
)

# ------------------------------------------------------------------------------------------------
#                                            SERVIDOR 
# ------------------------------------------------------------------------------------------------

server <- function(input, output, session) {
  observeEvent(input$guardar, {
    req(input$codigo, input$nombre, input$algoritmo)
    
    nueva_ficha <- data.frame(
      Codigo = input$codigo,
      Nombre = input$nombre,
      Area = input$area,
      Proposito = input$proposito,
      Tipo_Modelo = input$tipo_modelo,
      Algoritmo = input$algoritmo,
      Predictoras = input$predictoras,
      Objetivo = input$objetivo,
      Lenguaje = input$lenguaje,
      Version = input$version,
      Fecha_Desarrollo = as.character(input$fecha_desarrollo),
      Validador = input$validador,
      Desarrollador = input$desarrollador,
      Metricas = input$metricas,
      Dataset = input$dataset,
      Segmento = input$segmento,
      Uso = input$uso,
      Reentrenamiento = input$reentrenamiento,
      Estado = input$estado,
      Ubicacion = input$ubicacion,
      Documentacion = input$documentacion,
      stringsAsFactors = FALSE
    )
    
    registros <- model_store()
    registros <- rbind(registros, nueva_ficha)
    model_store(registros)
    saveRDS(registros, rds_file)
    
    showModal(modalDialog("La ficha técnica se ha registrado correctamente.", title = "Ficha guardada"))
    
    updateTextInput(session, "codigo", value = "")
    updateTextInput(session, "nombre", value = "")
    updateTextInput(session, "area", value = "")
    updateTextInput(session, "proposito", value = "")
    updateSelectInput(session, "tipo_modelo", selected = "Predictivo")
    updateSelectInput(session, "algoritmo", selected = "XGBoost")
    updateTextInput(session, "predictoras", value = "")
    updateTextInput(session, "objetivo", value = "")
    updateTextInput(session, "lenguaje", value = "")
    updateTextInput(session, "version", value = "")
    updateDateInput(session, "fecha_desarrollo", value = Sys.Date())
    updateTextInput(session, "validador", value = "")
    updateTextInput(session, "desarrollador", value = "")
    updateTextInput(session, "metricas", value = "")
    updateTextInput(session, "dataset", value = "")
    updateTextInput(session, "segmento", value = "")
    updateTextInput(session, "uso", value = "")
    updateSelectInput(session, "reentrenamiento", selected = "Anual")
    updateSelectInput(session, "estado", selected = "En desarrollo")
    updateTextInput(session, "ubicacion", value = "")
    updateTextInput(session, "documentacion", value = "")
  })
  
  output$tabla_fichas <- renderDT({
    datatable(
      model_store(),
      options = list(
        pageLength = 10,
        autoWidth = TRUE,
        scrollX = TRUE,
        columnDefs = list(
          list(targets = "_all", className = "dt-left", width = '200px')
        ),
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      ),
      escape = FALSE,
      rownames = FALSE,
      selection = "single"
    ) %>%
      formatStyle(
        columns = names(model_store()),
        `white-space` = "normal",
        `word-wrap` = "break-word"
      )
  })
  
  output$descargar <- downloadHandler(
    filename = function() paste0("fichas_modelos_", Sys.Date(), ".xlsx"),
    content = function(file) {
      write.xlsx(model_store(), file)
    }
  )
  
  observeEvent(input$eliminar, {
    selected <- input$tabla_fichas_rows_selected
    if (length(selected) == 0) {
      showModal(modalDialog(
        title = "Ninguna fila seleccionada",
        "Por favor selecciona una fila antes de intentar eliminar.",
        easyClose = TRUE
      ))
    } else {
      showModal(modalDialog(
        title = "¿Estás seguro?",
        "¿Deseas eliminar esta ficha técnica?",
        footer = tagList(
          modalButton("Cancelar"),
          actionButton("confirmar_eliminar", "Sí, eliminar", class = "btn-danger")
        )
      ))
    }
  })
  
  observeEvent(input$confirmar_eliminar, {
    registros <- model_store()
    selected <- input$tabla_fichas_rows_selected
    registros <- registros[-selected, ]
    model_store(registros)
    saveRDS(registros, rds_file)
    removeModal()
  })
}

shinyApp(ui, server)
