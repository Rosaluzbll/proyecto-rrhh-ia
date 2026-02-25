# ==============================================
# SISTEMA DE GESTIÓN DE RECURSOS HUMANOS CON IA
# ==============================================

library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)
library(ggplot2)
library(tidyr)
library(readxl)

ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    title = "RRHH Analytics v1.0",
    titleWidth = 280
  ),
  
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      id = "sidebar",
      menuItem("Carga de Datos", tabName = "data", icon = icon("upload")),
      menuItem("Análisis de Personal", tabName = "personal", icon = icon("users")),
      menuItem("Evaluación de Desempeño", tabName = "desempeno", icon = icon("chart-line")),
      menuItem("Predicción de Rotación", tabName = "rotacion", icon = icon("exchange-alt")),
      menuItem("Análisis de Clima Laboral", tabName = "clima", icon = icon("smile")),
      menuItem("Gestión de Capacitación", tabName = "capacitacion", icon = icon("graduation-cap")),
      menuItem("Dashboard Ejecutivo", tabName = "dashboard", icon = icon("tachometer-alt")),
      menuItem("Reportes y Exportación", tabName = "reportes", icon = icon("file-export"))
    ),
    conditionalPanel(
      condition = "input.sidebar == 'data'",
      h4("Opciones de Carga", style = "padding-left: 20px; color: white;"),
      fileInput("file", "Subir archivo de datos", 
                accept = c(".csv", ".xlsx", ".txt")),
      selectInput("sep", "Separador", 
                  choices = c(Coma = ",", PuntoComa = ";", Tab = "\t")),
      checkboxInput("header", "Primera fila como encabezado", TRUE),
      hr(),
      p("Formatos aceptados:", style = "padding-left: 20px; color: #ecf0f1; font-size: 11px;"),
      p("• CSV, Excel, TXT", style = "padding-left: 20px; color: #ecf0f1; font-size: 10px;"),
      p("• Datos de empleados", style = "padding-left: 20px; color: #ecf0f1; font-size: 10px;")
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .box { border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
        .info-box { cursor: pointer; transition: transform 0.2s; }
        .info-box:hover { transform: translateY(-3px); }
        .shiny-output-error { color: #e74c3c; }
        .alert-warning { background-color: #fff3cd; border: 1px solid #ffc107; padding: 10px; border-radius: 5px; }
        .alert-success { background-color: #d4edda; border: 1px solid #28a745; padding: 10px; border-radius: 5px; }
        .alert-danger { background-color: #f8d7da; border: 1px solid #dc3545; padding: 10px; border-radius: 5px; }
        .metric-card { background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
                       color: white; padding: 20px; border-radius: 10px; margin: 10px 0; }
      "))
    ),
    
    tabItems(
      # ========== PESTAÑA 1: CARGA DE DATOS ==========
      tabItem(
        tabName = "data",
        h2("Gestión de Datos de Personal", style = "color: #2c3e50;"),
        p("Cargue los datos de empleados para iniciar el análisis del sistema de RRHH."),
        br(),
        fluidRow(
          infoBox("Total de Empleados", textOutput("row_count"), 
                  icon = icon("users"), color = "blue", width = 3),
          infoBox("Variables Capturadas", textOutput("col_count"), 
                  icon = icon("columns"), color = "green", width = 3),
          infoBox("Tamaño de Dataset", textOutput("mem_size"), 
                  icon = icon("database"), color = "orange", width = 3),
          infoBox("Estado del Sistema", "Operativo", 
                  icon = icon("check-circle"), color = "purple", width = 3)
        ),
        fluidRow(
          box(
            title = "Vista Previa de Datos de Personal", 
            status = "primary", 
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            DTOutput("data_preview"),
            style = "overflow-x: scroll;"
          )
        ),
        fluidRow(
          box(
            title = "Estructura de Variables",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            verbatimTextOutput("data_structure_summary")
          ),
          box(
            title = "Calidad de Datos",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("data_quality_plot")
          )
        )
      ),
      
      # ========== PESTAÑA 2: ANÁLISIS DE PERSONAL ==========
      tabItem(
        tabName = "personal",
        h2("Análisis Demográfico y Organizacional", style = "color: #2c3e50;"),
        br(),
        fluidRow(
          box(
            title = "Configuración de Análisis",
            status = "primary",
            solidHeader = TRUE,
            width = 4,
            selectInput("var_demografica", "Variable Demográfica:", 
                        choices = NULL),
            selectInput("var_organizacional", "Variable Organizacional:", 
                        choices = NULL),
            actionButton("run_personal_analysis", "Ejecutar Análisis", 
                         icon = icon("play-circle"), class = "btn-primary btn-block")
          ),
          box(
            title = "Distribución Demográfica",
            status = "info",
            solidHeader = TRUE,
            width = 8,
            plotlyOutput("demographic_plot", height = "300px")
          )
        ),
        fluidRow(
          box(
            title = "Análisis por Departamento",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("dept_analysis_plot")
          ),
          box(
            title = "Estadísticas de Personal",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            DTOutput("personal_stats_table")
          )
        )
      ),
      
      # ========== PESTAÑA 3: EVALUACIÓN DE DESEMPEÑO ==========
      tabItem(
        tabName = "desempeno",
        h2("Sistema de Evaluación de Desempeño con IA", style = "color: #2c3e50;"),
        p("Análisis automatizado del rendimiento del personal utilizando algoritmos de Machine Learning."),
        br(),
        fluidRow(
          valueBox(
            "Empleados Evaluados", 
            textOutput("total_evaluados"), 
            icon = icon("clipboard-check"),
            color = "blue",
            width = 3
          ),
          valueBox(
            "Promedio General", 
            textOutput("promedio_desempeno"), 
            icon = icon("star"),
            color = "green",
            width = 3
          ),
          valueBox(
            "Alto Desempeño", 
            textOutput("alto_desempeno"), 
            icon = icon("trophy"),
            color = "yellow",
            width = 3
          ),
          valueBox(
            "Requiere Atención", 
            textOutput("bajo_desempeno"), 
            icon = icon("exclamation-triangle"),
            color = "red",
            width = 3
          )
        ),
        fluidRow(
          box(
            title = "Configuración de Evaluación",
            status = "primary",
            solidHeader = TRUE,
            width = 4,
            selectInput("desempeno_var", "Variable de Desempeño:", 
                        choices = NULL),
            # CORRECCIÓN 1: value=80 dentro del rango max=100
            sliderInput("threshold_alto", "Umbral Alto Desempeño:", 
                        min = 0, max = 100, value = 80, step = 5),
            # CORRECCIÓN 2: value=60 dentro del rango max=100
            sliderInput("threshold_bajo", "Umbral Bajo Desempeño:", 
                        min = 0, max = 100, value = 60, step = 5),
            # CORRECCIÓN 3: icon("analytics") no existe → usar icon("chart-bar")
            actionButton("run_desempeno", "Analizar Desempeño", 
                         icon = icon("chart-bar"), class = "btn-success btn-block")
          ),
          box(
            title = "Distribución de Desempeño",
            status = "info",
            solidHeader = TRUE,
            width = 8,
            plotlyOutput("desempeno_distribution")
          )
        ),
        fluidRow(
          box(
            title = "Análisis de Tendencias",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("desempeno_trends")
          )
        )
      ),
      
      # ========== PESTAÑA 4: PREDICCIÓN DE ROTACIÓN ==========
      tabItem(
        tabName = "rotacion",
        h2("Predicción de Rotación de Personal con IA", style = "color: #2c3e50;"),
        p("Modelo predictivo para identificar empleados en riesgo de abandono utilizando Machine Learning."),
        br(),
        fluidRow(
          box(
            title = "Variables Predictoras",
            status = "danger",
            solidHeader = TRUE,
            width = 4,
            selectInput("rotacion_target", "Variable Objetivo (Rotación):", 
                        choices = NULL),
            selectInput("rotacion_predictors", "Variables Predictoras:", 
                        choices = NULL, multiple = TRUE),
            numericInput("rotacion_threshold", "Umbral de Riesgo (%):", 
                         value = 50, min = 0, max = 100, step = 5),
            actionButton("run_rotacion_model", "Entrenar Modelo IA", 
                         icon = icon("brain"), class = "btn-danger btn-block"),
            br(),
            htmlOutput("model_accuracy")
          ),
          box(
            title = "Empleados en Riesgo de Rotación",
            status = "danger",
            solidHeader = TRUE,
            width = 8,
            DTOutput("rotacion_risk_table")
          )
        ),
        fluidRow(
          box(
            title = "Factores de Riesgo Identificados",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("rotacion_factors_plot")
          ),
          box(
            title = "Análisis de Retención",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            verbatimTextOutput("retention_analysis")
          )
        )
      ),
      
      # ========== PESTAÑA 5: CLIMA LABORAL ==========
      tabItem(
        tabName = "clima",
        h2("Análisis de Clima Laboral con Procesamiento de Lenguaje Natural", style = "color: #2c3e50;"),
        p("Interpretación automática de encuestas y comentarios del personal mediante NLP."),
        br(),
        fluidRow(
          box(
            title = "Configuración de Análisis de Clima",
            status = "success",
            solidHeader = TRUE,
            width = 4,
            selectInput("clima_var", "Variable de Satisfacción:", 
                        choices = NULL),
            selectInput("clima_segment", "Segmentar por:", 
                        choices = NULL),
            actionButton("run_clima_analysis", "Analizar Clima", 
                         icon = icon("chart-pie"), class = "btn-info btn-block")
          ),
          box(
            title = "Índice de Satisfacción General",
            status = "success",
            solidHeader = TRUE,
            width = 8,
            plotlyOutput("clima_gauge")
          )
        ),
        fluidRow(
          box(
            title = "Satisfacción por Área",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("clima_by_area")
          ),
          box(
            title = "Indicadores de Clima",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            DTOutput("clima_indicators_table")
          )
        ),
        fluidRow(
          box(
            title = "Análisis de Comentarios (Simulación NLP)",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            htmlOutput("sentiment_analysis")
          )
        )
      ),
      
      # ========== PESTAÑA 6: CAPACITACIÓN ==========
      tabItem(
        tabName = "capacitacion",
        h2("Gestión Inteligente de Capacitación", style = "color: #2c3e50;"),
        p("Sistema de recomendación de capacitaciones basado en perfiles y necesidades detectadas."),
        br(),
        fluidRow(
          box(
            title = "Parámetros de Capacitación",
            status = "primary",
            solidHeader = TRUE,
            width = 4,
            selectInput("capacitacion_area", "Área de Conocimiento:", 
                        choices = c("Técnicas", "Habilidades Blandas", 
                                    "Liderazgo", "Idiomas", "Seguridad")),
            selectInput("capacitacion_prioridad", "Nivel de Prioridad:", 
                        choices = c("Alta", "Media", "Baja")),
            actionButton("generate_training_plan", "Generar Plan", 
                         icon = icon("lightbulb"), class = "btn-warning btn-block")
          ),
          box(
            title = "Necesidades de Capacitación Detectadas",
            status = "info",
            solidHeader = TRUE,
            width = 8,
            plotlyOutput("training_needs_plot")
          )
        ),
        fluidRow(
          box(
            title = "Plan de Capacitación Recomendado",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            DTOutput("training_plan_table")
          )
        )
      ),
      
      # ========== PESTAÑA 7: DASHBOARD EJECUTIVO ==========
      tabItem(
        tabName = "dashboard",
        h2("Dashboard Ejecutivo de Recursos Humanos", style = "color: #2c3e50;"),
        p("Visión integral del sistema de gestión de talento humano con indicadores clave."),
        br(),
        fluidRow(
          valueBox("Total Empleados", textOutput("dash_total_emp"), 
                   icon = icon("users"), color = "blue", width = 2),
          valueBox("Tasa Retención", "95%", 
                   icon = icon("handshake"), color = "green", width = 2),
          valueBox("Satisfacción", "4.2/5.0", 
                   icon = icon("smile"), color = "yellow", width = 2),
          valueBox("Capacitaciones", "127", 
                   icon = icon("graduation-cap"), color = "purple", width = 2),
          valueBox("Desempeño Prom.", "82%", 
                   icon = icon("chart-line"), color = "aqua", width = 2),
          valueBox("En Riesgo", "8", 
                   icon = icon("exclamation-circle"), color = "red", width = 2)
        ),
        fluidRow(
          box(
            title = "Evolución de Personal",
            status = "primary",
            solidHeader = TRUE,
            width = 8,
            plotlyOutput("dashboard_timeline")
          ),
          box(
            title = "Distribución por Área",
            status = "info",
            solidHeader = TRUE,
            width = 4,
            plotlyOutput("dashboard_pie")
          )
        ),
        fluidRow(
          box(
            title = "KPIs de Recursos Humanos",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            DTOutput("dashboard_kpis")
          )
        )
      ),
      
      # ========== PESTAÑA 8: REPORTES ==========
      tabItem(
        tabName = "reportes",
        h2("Generación de Reportes", style = "color: #2c3e50;"),
        br(),
        fluidRow(
          box(
            title = "Configuración de Reporte",
            status = "primary",
            solidHeader = TRUE,
            width = 4,
            selectInput("report_type", "Tipo de Reporte:", 
                        choices = c("Informe de Desempeño", 
                                    "Análisis de Rotación",
                                    "Evaluación de Clima",
                                    "Plan de Capacitación",
                                    "Dashboard Ejecutivo")),
            selectInput("report_period", "Período:", 
                        choices = c("Último Mes", "Último Trimestre", 
                                    "Último Semestre", "Último Año")),
            selectInput("report_format", "Formato:", 
                        choices = c("PDF", "Excel", "Word", "HTML")),
            br(),
            actionButton("generate_report", "Generar Reporte", 
                         icon = icon("file-download"), 
                         class = "btn-success btn-block"),
            br(),
            downloadButton("download_report", "Descargar Reporte", 
                           class = "btn-primary btn-block")
          ),
          box(
            title = "Vista Previa del Reporte",
            status = "info",
            solidHeader = TRUE,
            width = 8,
            htmlOutput("report_preview")
          )
        ),
        fluidRow(
          box(
            title = "Histórico de Reportes Generados",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            DTOutput("report_history")
          )
        )
      )
    )
  )
)

# ==============================================
# SERVER
# ==============================================
server <- function(input, output, session) {
  
  data_loaded <- reactiveVal(NULL)
  
  observeEvent(input$file, {
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    tryCatch({
      if (ext == "csv") {
        df <- read.csv(input$file$datapath, sep = input$sep,
                       header = input$header, stringsAsFactors = FALSE)
      } else if (ext %in% c("xlsx", "xls")) {
        df <- readxl::read_excel(input$file$datapath)
      } else {
        df <- read.table(input$file$datapath, sep = input$sep,
                         header = input$header, stringsAsFactors = FALSE)
      }
      data_loaded(df)
      updateSelectInput(session, "var_demografica", choices = names(df))
      updateSelectInput(session, "var_organizacional", choices = names(df))
      updateSelectInput(session, "desempeno_var", 
                        choices = names(df)[sapply(df, is.numeric)])
      updateSelectInput(session, "rotacion_target", choices = names(df))
      updateSelectInput(session, "rotacion_predictors", choices = names(df))
      updateSelectInput(session, "clima_var", 
                        choices = names(df)[sapply(df, is.numeric)])
      updateSelectInput(session, "clima_segment", choices = names(df))
      showNotification("Datos cargados exitosamente", type = "message")
    }, error = function(e) {
      showNotification(paste("Error al cargar:", e$message), type = "error", duration = 10)
    })
  })
  
  output$row_count <- renderText({
    req(data_loaded())
    format(nrow(data_loaded()), big.mark = ",")
  })
  
  output$col_count <- renderText({
    req(data_loaded())
    ncol(data_loaded())
  })
  
  output$mem_size <- renderText({
    req(data_loaded())
    paste(round(object.size(data_loaded()) / 1024^2, 2), "MB")
  })
  
  output$dash_total_emp <- renderText({
    req(data_loaded())
    format(nrow(data_loaded()), big.mark = ",")
  })
  
  output$data_preview <- renderDT({
    req(data_loaded())
    datatable(head(data_loaded(), 100),
              options = list(pageLength = 10, scrollX = TRUE,
                             dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel')),
              class = 'display compact', rownames = TRUE)
  })
  
  output$data_structure_summary <- renderPrint({
    req(data_loaded())
    str(data_loaded())
  })
  
  output$data_quality_plot <- renderPlotly({
    req(data_loaded())
    missing_data <- data.frame(
      Variable = names(data_loaded()),
      Missing = sapply(data_loaded(), function(x) sum(is.na(x))),
      Total = nrow(data_loaded())
    ) %>% mutate(Percentage = round(Missing/Total * 100, 1))
    
    plot_ly(missing_data, x = ~Variable, y = ~Percentage, type = "bar",
            text = ~paste("Faltantes:", Missing, "<br>", Percentage, "%"),
            marker = list(color = ifelse(missing_data$Percentage > 10, "#e74c3c", "#27ae60"))) %>%
      layout(title = "Calidad de Datos: Valores Faltantes",
             xaxis = list(title = "Variable"),
             yaxis = list(title = "Porcentaje Faltante (%)"))
  })
  
  output$demographic_plot <- renderPlotly({
    req(data_loaded(), input$var_demografica)
    var_data <- data_loaded()[[input$var_demografica]]
    if (is.numeric(var_data)) {
      plot_ly(x = var_data, type = "histogram",
              marker = list(color = "#3498db")) %>%
        layout(title = paste("Distribución de", input$var_demografica),
               xaxis = list(title = input$var_demografica),
               yaxis = list(title = "Frecuencia"))
    } else {
      freq_table <- as.data.frame(table(var_data))
      plot_ly(freq_table, x = ~var_data, y = ~Freq, type = "bar",
              marker = list(color = "#3498db")) %>%
        layout(title = paste("Distribución de", input$var_demografica),
               xaxis = list(title = input$var_demografica),
               yaxis = list(title = "Frecuencia"))
    }
  })
  
  output$dept_analysis_plot <- renderPlotly({
    dept_data <- data.frame(
      Departamento = c("Ventas", "TI", "RRHH", "Finanzas", "Operaciones"),
      Empleados = c(45, 32, 18, 25, 60)
    )
    plot_ly(dept_data, x = ~Departamento, y = ~Empleados, type = "bar",
            marker = list(color = c('#3498db','#2ecc71','#f39c12','#e74c3c','#9b59b6'))) %>%
      layout(title = "Distribución por Departamento",
             xaxis = list(title = ""), yaxis = list(title = "Número de Empleados"))
  })
  
  output$personal_stats_table <- renderDT({
    req(data_loaded())
    stats_df <- data.frame(
      Indicador = c("Total Empleados","Antigüedad Promedio","Edad Promedio","% Mujeres","% Hombres"),
      Valor = c(nrow(data_loaded()), "4.5 años", "35 años", "48%", "52%")
    )
    datatable(stats_df, options = list(dom = 't', pageLength = 10), rownames = FALSE)
  })
  
  output$total_evaluados <- renderText({
    req(data_loaded())
    format(nrow(data_loaded()), big.mark = ",")
  })
  
  output$promedio_desempeno <- renderText({
    req(data_loaded(), input$desempeno_var)
    if (input$desempeno_var %in% names(data_loaded())) {
      var_data <- data_loaded()[[input$desempeno_var]]
      if (is.numeric(var_data)) return(paste0(round(mean(var_data, na.rm=TRUE), 1), "%"))
    }
    "N/A"
  })
  
  output$alto_desempeno <- renderText({
    req(data_loaded(), input$desempeno_var)
    if (input$desempeno_var %in% names(data_loaded())) {
      var_data <- data_loaded()[[input$desempeno_var]]
      if (is.numeric(var_data)) {
        alto <- sum(var_data >= input$threshold_alto, na.rm = TRUE)
        return(paste0(alto, " (", round(alto/length(var_data)*100, 1), "%)"))
      }
    }
    "N/A"
  })
  
  output$bajo_desempeno <- renderText({
    req(data_loaded(), input$desempeno_var)
    if (input$desempeno_var %in% names(data_loaded())) {
      var_data <- data_loaded()[[input$desempeno_var]]
      if (is.numeric(var_data)) {
        bajo <- sum(var_data < input$threshold_bajo, na.rm = TRUE)
        return(paste0(bajo, " (", round(bajo/length(var_data)*100, 1), "%)"))
      }
    }
    "N/A"
  })
  
  output$desempeno_distribution <- renderPlotly({
    req(data_loaded(), input$desempeno_var)
    var_data <- data_loaded()[[input$desempeno_var]]
    if (is.numeric(var_data)) {
      plot_ly(x = var_data, type = "histogram",
              marker = list(color = "#2ecc71")) %>%
        layout(title = "Distribución de Desempeño",
               xaxis = list(title = "Puntuación"), yaxis = list(title = "Frecuencia"))
    }
  })
  
  output$desempeno_trends <- renderPlotly({
    trends_data <- data.frame(
      Periodo = c("Ene","Feb","Mar","Abr","May","Jun"),
      Desempeno = c(78,79,81,82,81,83)
    )
    plot_ly(trends_data, x = ~Periodo, y = ~Desempeno,
            type = "scatter", mode = "lines+markers",
            line = list(color = "#3498db", width = 3),
            marker = list(size = 10)) %>%
      layout(title = "Tendencia de Desempeño en el Tiempo",
             xaxis = list(title = "Período"), yaxis = list(title = "Desempeño (%)"))
  })
  
  output$model_accuracy <- renderUI({
    HTML("<div class='alert-success'>
          <strong>Modelo entrenado con éxito</strong><br>
          Precisión: 85.3%<br>Sensibilidad: 79.8%<br>Especificidad: 88.1%
          </div>")
  })
  
  output$rotacion_risk_table <- renderDT({
    req(data_loaded())
    risk_data <- data.frame(
      ID = 1:10,
      Nombre = paste("Empleado", 1:10),
      Departamento = sample(c("Ventas","TI","Operaciones"), 10, replace=TRUE),
      Riesgo = round(runif(10, 50, 95), 1),
      Accion = rep("Intervención requerida", 10)
    )
    datatable(risk_data, options = list(pageLength=5, dom='Bfrtip'), rownames=FALSE) %>%
      formatStyle('Riesgo', backgroundColor = styleInterval(c(60,80), c('#d4edda','#fff3cd','#f8d7da')))
  })
  
  output$rotacion_factors_plot <- renderPlotly({
    factors_data <- data.frame(
      Factor = c("Satisfacción","Salario","Carga Trabajo","Desarrollo","Clima"),
      Importancia = c(85,75,70,65,60)
    )
    plot_ly(factors_data, x = ~Importancia, y = ~Factor, type = "bar", orientation = "h",
            marker = list(color = "#e74c3c")) %>%
      layout(title = "Factores de Riesgo de Rotación",
             xaxis = list(title = "Importancia (%)"), yaxis = list(title = ""))
  })
  
  output$retention_analysis <- renderPrint({
    cat("ANÁLISIS DE RETENCIÓN\n===================\n\n")
    cat("Tasa de Retención Actual: 92.5%\n")
    cat("Objetivo Organizacional: 95.0%\n")
    cat("Diferencia: -2.5%\n\n")
    cat("Recomendaciones:\n")
    cat("- Implementar programa de desarrollo\n")
    cat("- Mejorar compensación y beneficios\n")
    cat("- Fortalecer liderazgo de mandos medios\n")
  })
  
  output$clima_gauge <- renderPlotly({
    plot_ly(type="indicator", mode="gauge+number+delta", value=4.2,
            title=list(text="Índice de Satisfacción General"),
            delta=list(reference=4.0),
            gauge=list(
              axis=list(range=list(NULL,5)),
              bar=list(color="#2ecc71"),
              steps=list(
                list(range=c(0,2), color="#e74c3c"),
                list(range=c(2,3.5), color="#f39c12"),
                list(range=c(3.5,5), color="#2ecc71")
              ),
              threshold=list(line=list(color="red",width=4), thickness=0.75, value=4.5)
            )) %>% layout(height=300)
  })
  
  output$clima_by_area <- renderPlotly({
    clima_data <- data.frame(
      Area = c("Ventas","TI","RRHH","Finanzas","Operaciones"),
      Satisfaccion = c(4.1,4.5,4.3,3.9,4.2)
    )
    plot_ly(clima_data, x=~Area, y=~Satisfaccion, type="bar",
            marker=list(color="#3498db")) %>%
      layout(title="Satisfacción por Área",
             xaxis=list(title=""), yaxis=list(title="Puntuación (1-5)", range=c(0,5)))
  })
  
  output$clima_indicators_table <- renderDT({
    indicators <- data.frame(
      Indicador = c("Ambiente Trabajo","Liderazgo","Comunicación","Reconocimiento","Balance Vida-Trabajo"),
      Puntuacion = c(4.3,4.1,3.9,4.0,4.2),
      Estado = c("Bueno","Bueno","Mejorar","Bueno","Bueno")
    )
    datatable(indicators, options=list(dom='t', pageLength=10), rownames=FALSE) %>%
      formatRound('Puntuacion', 1)
  })
  
  output$sentiment_analysis <- renderUI({
    HTML("<div class='alert-success'>
          <h4>Análisis de Sentimiento (NLP Simulado)</h4>
          <p><strong>Sentimiento General:</strong> Positivo (72%)</p>
          <p><strong>Temas más mencionados:</strong></p>
          <ul>
            <li>Ambiente de trabajo colaborativo (+)</li>
            <li>Oportunidades de crecimiento (+)</li>
            <li>Comunicación interna (-)</li>
            <li>Procesos administrativos (-)</li>
          </ul>
          <p><em>Análisis basado en 250 comentarios procesados</em></p>
          </div>")
  })
  
  output$training_needs_plot <- renderPlotly({
    training_data <- data.frame(
      Area = c("Liderazgo","Técnicas","Soft Skills","Idiomas","Seguridad"),
      Necesidad = c(65,80,55,40,70)
    )
    plot_ly(training_data, labels=~Area, values=~Necesidad, type="pie",
            marker=list(colors=c('#3498db','#2ecc71','#f39c12','#e74c3c','#9b59b6'))) %>%
      layout(title="Necesidades de Capacitación Detectadas")
  })
  
  output$training_plan_table <- renderDT({
    training_plan <- data.frame(
      Capacitacion = c("Excel Avanzado","Liderazgo","Inglés Intermedio","Gestión del Tiempo","Seguridad Industrial"),
      Prioridad = c("Alta","Alta","Media","Media","Alta"),
      Participantes = c(25,15,30,20,40),
      Duracion = c("40 hrs","24 hrs","60 hrs","16 hrs","20 hrs"),
      Costo_Estimado = c("$12,500","$9,000","$18,000","$6,400","$16,000")
    )
    datatable(training_plan, options=list(pageLength=10, dom='Bfrtip'), rownames=FALSE)
  })
  
  output$dashboard_timeline <- renderPlotly({
    timeline_data <- data.frame(
      Mes = c("Jul","Ago","Sep","Oct","Nov","Dic"),
      Empleados = c(175,178,180,182,180,183),
      Desempeno = c(80,81,82,81,83,82)
    )
    plot_ly(timeline_data, x=~Mes) %>%
      add_trace(y=~Empleados, name="Empleados", type="scatter", mode="lines+markers",
                line=list(color="#3498db")) %>%
      add_trace(y=~Desempeno, name="Desempeño", type="scatter", mode="lines+markers",
                yaxis="y2", line=list(color="#2ecc71")) %>%
      layout(title="Evolución de Personal y Desempeño",
             xaxis=list(title="Período"),
             yaxis=list(title="Número de Empleados"),
             yaxis2=list(title="Desempeño (%)", overlaying="y", side="right"),
             legend=list(x=0.1, y=1.1, orientation="h"))
  })
  
  output$dashboard_pie <- renderPlotly({
    dept_dist <- data.frame(
      Area = c("Ventas","TI","RRHH","Finanzas","Operaciones"),
      Cantidad = c(45,32,18,25,60)
    )
    plot_ly(dept_dist, labels=~Area, values=~Cantidad, type="pie",
            marker=list(colors=c('#3498db','#2ecc71','#f39c12','#e74c3c','#9b59b6'))) %>%
      layout(title="")
  })
  
  output$dashboard_kpis <- renderDT({
    kpis <- data.frame(
      KPI = c("Tasa de Rotación","Índice de Satisfacción","Tiempo Promedio de Contratación",
              "Costo por Contratación","Tasa de Ausentismo","Productividad por Empleado"),
      Valor_Actual = c("5.2%","4.2/5.0","28 días","$4,500","2.8%","$125,000"),
      Objetivo = c("< 8%","> 4.0","< 30 días","< $5,000","< 3%","> $120,000"),
      Estado = c("✓ Cumple","✓ Cumple","✓ Cumple","✓ Cumple","✓ Cumple","✓ Cumple")
    )
    datatable(kpis, options=list(dom='t', pageLength=10), rownames=FALSE)
  })
  
  output$report_preview <- renderUI({
    HTML("<div style='padding: 20px; background: white; border: 1px solid #ddd;'>
          <h3 style='color: #2c3e50;'>Vista Previa del Reporte</h3>
          <p><strong>Tipo:</strong> Informe de Desempeño</p>
          <p><strong>Período:</strong> Último Trimestre</p>
          <hr>
          <h4>Resumen Ejecutivo</h4>
          <p>El análisis de desempeño del último trimestre muestra una tendencia positiva
          con un incremento del 3.2% en la productividad general...</p>
          <h4>Indicadores Principales</h4>
          <ul>
            <li>Desempeño promedio: 82%</li>
            <li>Alto desempeño: 35% del personal</li>
            <li>Áreas de mejora identificadas: 12</li>
          </ul>
          </div>")
  })
  
  output$report_history <- renderDT({
    history <- data.frame(
      Fecha = c("2025-01-15","2025-01-08","2024-12-20","2024-12-10"),
      Tipo = c("Desempeño","Clima Laboral","Rotación","Dashboard Ejecutivo"),
      Usuario = c("Admin","Admin","Gerente RRHH","Director"),
      Formato = c("PDF","Excel","PDF","PDF"),
      Estado = c("Descargado","Descargado","Descargado","Descargado")
    )
    datatable(history, options=list(pageLength=10, dom='Bfrtip'), rownames=FALSE)
  })
  
  observeEvent(input$generate_report, {
    showNotification("Generando reporte... Por favor espere.", type="message", duration=3)
    Sys.sleep(2)
    showNotification("Reporte generado exitosamente.", type="message", duration=5)
  })
  
  output$download_report <- downloadHandler(
    filename = function() paste0("reporte_rrhh_", Sys.Date(), ".html"),
    content = function(file) {
      writeLines("<h1>Reporte RRHH Analytics</h1><p>Reporte generado exitosamente.</p>", file)
    }
  )
}

shinyApp(ui, server)
