# =============================================================================
# PsyML Report Generator - Shiny App
# =============================================================================
# Aplicacion Shiny para generar reportes cientificos desde JSON de PsyML
# usando ChatGPT API para redaccion academica
#
# Uso:
#   1. Ejecutar analisis con PsyML:
#      resultado <- easy_ml(data, target = "variable", task = "classification")
#
#   2. Capturar y exportar verbose:
#      resultado <- easy_ml_capture(data, target = "variable", task = "classification")
#      export_verbose_txt(resultado, "mi_analisis.txt")
#      export_verbose_json(resultado, "mi_analisis.json")
#
#   3. Abrir esta app y subir el JSON generado
#
# =============================================================================

library(shiny)
library(bslib)
library(shinydashboard)
library(jsonlite)
library(rmarkdown)
library(officer)
library(httr2)
library(commonmark)
library(flextable)

# =============================================================================
# UI
# =============================================================================

ui <- dashboardPage(
  skin = "purple",

  dashboardHeader(
    title = span(icon("chart-line"), " PsyML Report Generator"),
    titleWidth = 320
  ),

  dashboardSidebar(
    width = 320,

    # Seccion 1: Cargar JSON
    div(style = "padding: 20px;",

      # Header con estilo
      div(
        style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); padding: 15px; border-radius: 10px; margin-bottom: 20px; color: white;",
        h4(icon("file-code"), "Paso 1: Cargar JSON", style = "margin: 0;"),
        tags$small("Generado con export_verbose_json()")
      ),

      fileInput(
        "json_file",
        NULL,
        accept = c(".json"),
        buttonLabel = icon("folder-open"),
        placeholder = "Seleccionar archivo..."
      ),

      # Seccion 2: API Key
      div(
        style = "background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%); padding: 15px; border-radius: 10px; margin-bottom: 20px; color: white;",
        h4(icon("key"), "Paso 2: API Key", style = "margin: 0;"),
        tags$small("Tu clave de OpenAI")
      ),

      passwordInput(
        "api_key",
        NULL,
        placeholder = "sk-proj-...",
        width = "100%"
      ),
      div(
        style = "text-align: center; margin-bottom: 15px;",
        tags$a(
          href = "https://platform.openai.com/api-keys",
          target = "_blank",
          style = "color: #667eea; font-size: 12px;",
          icon("external-link-alt"), " Obtener API Key"
        )
      ),

      # Seccion 3: Configuracion
      div(
        style = "background: linear-gradient(135deg, #4facfe 0%, #00f2fe 100%); padding: 15px; border-radius: 10px; margin-bottom: 20px; color: white;",
        h4(icon("sliders-h"), "Paso 3: Configurar", style = "margin: 0;"),
        tags$small("Personaliza tu reporte")
      ),

      textInput(
        "report_title",
        "Titulo:",
        value = "Analisis de Machine Learning",
        width = "100%"
      ),

      textInput(
        "author_name",
        "Autor(es):",
        value = "",
        placeholder = "Nombre del autor",
        width = "100%"
      ),

      fluidRow(
        column(6,
          selectInput(
            "language",
            "Idioma:",
            choices = c("Espanol" = "es", "English" = "en"),
            selected = "es",
            width = "100%"
          )
        ),
        column(6,
          selectInput(
            "model_choice",
            "Modelo IA:",
            choices = c(
              "GPT-4.1-mini" = "gpt-4.1-mini",
              "GPT-4o-mini" = "gpt-4o-mini",
              "GPT-4o" = "gpt-4o",
              "GPT-4.1" = "gpt-4.1"
            ),
            selected = "gpt-4.1-mini",
            width = "100%"
          )
        )
      ),

      hr(),

      # Boton de generar
      actionButton(
        "generate_btn",
        label = tagList(icon("magic"), " Generar Reporte"),
        class = "btn-success btn-lg",
        style = "width: 100%; margin-bottom: 15px; font-weight: bold; border-radius: 25px;"
      ),

      # Botones de descarga
      div(
        style = "display: flex; gap: 10px;",
        downloadButton(
          "download_docx",
          tagList(icon("file-word"), " DOCX"),
          class = "btn-primary",
          style = "flex: 1; border-radius: 20px;"
        ),
        downloadButton(
          "download_md",
          tagList(icon("file-alt"), " MD"),
          class = "btn-default",
          style = "flex: 1; border-radius: 20px;"
        )
      )
    )
  ),

  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper { background-color: #f8f9fa; }
        .box-header { font-weight: bold; }
        .skin-purple .main-header .logo { background-color: #667eea; }
        .skin-purple .main-header .navbar { background-color: #667eea; }
        .skin-purple .main-sidebar { background-color: #2c3e50; }
        .report-preview {
          background: white;
          padding: 20px;
          border-radius: 5px;
          max-height: 600px;
          overflow-y: auto;
        }
        .metric-card {
          text-align: center;
          padding: 15px;
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          color: white;
          border-radius: 10px;
          margin: 5px;
        }
        .metric-value { font-size: 28px; font-weight: bold; }
        .metric-label { font-size: 12px; opacity: 0.9; }
      "))
    ),

    fluidRow(
      # Panel de informacion del JSON
      column(4,
        box(
          title = tagList(icon("info-circle"), "Informacion del Analisis"),
          status = "primary",
          solidHeader = TRUE,
          width = NULL,
          uiOutput("json_info")
        ),

        box(
          title = tagList(icon("chart-bar"), "Metricas del Modelo"),
          status = "success",
          solidHeader = TRUE,
          width = NULL,
          uiOutput("metrics_cards")
        ),

        box(
          title = tagList(icon("list-ol"), "Top Variables"),
          status = "info",
          solidHeader = TRUE,
          width = NULL,
          collapsible = TRUE,
          tableOutput("var_table")
        )
      ),

      # Panel del reporte
      column(8,
        box(
          title = tagList(icon("file-alt"), "Vista Previa del Reporte"),
          status = "success",
          solidHeader = TRUE,
          width = NULL,
          div(
            class = "report-preview",
            htmlOutput("report_preview")
          )
        ),

        box(
          title = tagList(icon("bug"), "Debug Info"),
          status = "warning",
          solidHeader = TRUE,
          width = NULL,
          collapsible = TRUE,
          collapsed = TRUE,
          verbatimTextOutput("debug_info")
        )
      )
    )
  )
)


# =============================================================================
# Server
# =============================================================================

server <- function(input, output, session) {

  # Reactive values para almacenar el reporte
  rv <- reactiveValues(
    report_md = NULL,
    report_html = NULL,
    last_generation = NULL
  )

  # ---- Cargar JSON ----
  json_data <- reactive({
    req(input$json_file)
    tryCatch({
      jsonlite::fromJSON(input$json_file$datapath)
    }, error = function(e) {
      showNotification(paste("Error al leer JSON:", e$message), type = "error")
      NULL
    })
  })

  # ---- Mostrar info del JSON ----
  output$json_info <- renderUI({
    req(json_data())
    data <- json_data()
    meta <- data$metadata
    model <- data$model_info

    tagList(
      tags$p(tags$strong("Tarea: "),
             ifelse(meta$task == "classification", "Clasificacion", "Regresion")),
      tags$p(tags$strong("Variable objetivo: "), meta$target),
      tags$p(tags$strong("Observaciones: "), format(meta$n_observations, big.mark = ",")),
      tags$p(tags$strong("Variables: "),
             if (!is.null(meta$n_features)) meta$n_features else "N/A"),
      tags$p(tags$strong("Train/Test: "),
             paste0(meta$n_train, " / ", meta$n_test)),
      if (!is.null(meta$cv_folds)) {
        tags$p(tags$strong("CV Folds: "), meta$cv_folds)
      },
      if (!is.null(meta$models_trained)) {
        tags$p(tags$strong("Modelos evaluados: "),
               paste(meta$models_trained, collapse = ", "))
      },
      hr(),
      tags$p(tags$strong("Modelo: "), model$label),
      tags$p(tags$strong("Tuneado: "), ifelse(model$tuned, "Si", "No")),
      if (!is.null(meta$tune_method) && !is.na(meta$tune_method)) {
        tags$p(tags$strong("Metodo tuning: "), meta$tune_method)
      },
      if (model$tuned && !is.null(model$hyperparameters)) {
        tags$p(tags$strong("Hiperparametros: "),
               paste(names(model$hyperparameters), "=",
                     unlist(model$hyperparameters), collapse = ", "))
      },
      if (!is.null(meta$elapsed_time)) {
        tags$p(tags$strong("Tiempo: "), paste0(meta$elapsed_time, " seg"))
      },
      if (!is.null(meta$easyml_version)) {
        tags$p(tags$small(tags$em(paste0("PsyML v", meta$easyml_version))))
      }
    )
  })

  # ---- Mostrar metricas ----
  output$metrics_cards <- renderUI({
    req(json_data())
    data <- json_data()
    metrics <- data$metrics

    if (is.null(metrics)) {
      return(tags$p("No hay metricas disponibles"))
    }

    # Crear cards para metricas principales
    metric_cards <- lapply(names(metrics), function(m) {
      value <- metrics[[m]]
      # Formatear segun tipo
      if (m %in% c("accuracy", "sensitivity", "specificity")) {
        display_val <- paste0(round(value * 100, 1), "%")
      } else {
        display_val <- round(value, 4)
      }

      div(
        class = "metric-card",
        style = "display: inline-block; width: 45%; margin: 2%;",
        div(class = "metric-value", display_val),
        div(class = "metric-label", toupper(m))
      )
    })

    div(style = "text-align: center;", metric_cards)
  })

  # ---- Tabla de variables ----
  output$var_table <- renderTable({
    req(json_data())
    data <- json_data()

    if (is.null(data$variable_importance)) return(NULL)

    var_df <- data$variable_importance
    var_df$importance <- round(var_df$importance, 4)
    names(var_df) <- c("Variable", "Importancia", "Rango")
    var_df <- var_df[, c("Rango", "Variable", "Importancia")]

    head(var_df, 10)
  }, striped = TRUE, hover = TRUE, width = "100%")

  # ---- Debug info ----
  output$debug_info <- renderText({
    data_loaded <- !is.null(json_data())
    api_key_set <- nzchar(input$api_key)
    api_key_preview <- if (api_key_set) paste0(substr(input$api_key, 1, 7), "...") else "No configurada"

    paste0(
      "Estado de generacion de reporte:\n",
      "- JSON cargado: ", toupper(as.character(data_loaded)), "\n",
      "- API Key: ", api_key_preview, "\n",
      "- Modelo OpenAI: ", input$model_choice, "\n",
      "- Ultima generacion: ",
        if(!is.null(rv$last_generation)) format(rv$last_generation, "%Y-%m-%d %H:%M:%S") else "Ninguna",
      "\n- Hora actual: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    )
  })

  # ---- Generar reporte con ChatGPT ----
  observeEvent(input$generate_btn, {

    # Validaciones
    if (is.null(json_data())) {
      showNotification("Primero cargue un archivo JSON", type = "warning")
      return()
    }

    if (!nzchar(input$api_key)) {
      showNotification("Ingrese su API Key de OpenAI", type = "error")
      return()
    }

    withProgress(message = "Generando reporte con IA...", value = 0, {

      incProgress(0.1, detail = "Preparando contexto...")

      data <- json_data()
      meta <- data$metadata
      metrics <- data$metrics
      model <- data$model_info
      var_imp <- data$variable_importance
      refs <- data$references

      # ---- Construir prompt del sistema ----
      system_prompt <- if (input$language == "es") {
        paste(
          "Eres un experto en redaccion cientifica especializado en Machine Learning y analisis predictivo.",
          "Tu tarea es generar un reporte academico COMPLETO Y DETALLADO en ESTILO DE ARTICULO CIENTIFICO.",
          "",
          "IMPORTANTE: Se te proporcionara el OUTPUT COMPLETO (verbose) del analisis de Machine Learning.",
          "Debes usar TODA esa informacion para escribir un reporte exhaustivo que incluya:",
          "- Todos los pasos del preprocesamiento mencionados",
          "- Detalles de la validacion cruzada (folds, estratificacion)",
          "- Comparacion de modelos si hay multiples",
          "- Todas las metricas con sus interpretaciones",
          "- Matriz de confusion y su interpretacion",
          "- Calibracion del modelo si esta disponible",
          "- Optimizacion de threshold si aplica",
          "- Deteccion de data leakage si se menciona",
          "- Importancia de variables",
          "- Las referencias bibliograficas que aparecen en el verbose",
          "",
          "ESTILO DE ESCRITURA OBLIGATORIO:",
          "1. PARRAFOS EXTENSOS: Cada parrafo debe tener minimo 150-200 palabras",
          "2. FLUJO NARRATIVO: Las ideas deben conectarse con transiciones suaves",
          "3. NO LISTAS: Nunca uses bullets (-), asteriscos (*) ni numeracion para listas",
          "4. MUCHAS CITAS: Integra las citas mencionadas en el verbose + citas adicionales",
          "5. ESTADISTICOS EN LINEA: Reporta valores dentro de oraciones narrativas",
          "6. VOZ IMPERSONAL: 'se observo', 'se estimo', 'se empleo'",
          "",
          "ESTRUCTURA OBLIGATORIA:",
          "",
          "## Metodo",
          "### Participantes",
          "Describe la muestra, division train/test, y caracteristicas del dataset.",
          "",
          "### Preprocesamiento de Datos",
          "Detalla TODOS los pasos de preprocesamiento mencionados en el verbose:",
          "imputacion, normalizacion, dummy encoding, etc.",
          "",
          "### Procedimiento de Modelado",
          "Explica la validacion cruzada, modelos evaluados, y criterios de seleccion.",
          "",
          "## Resultados",
          "### Comparacion de Modelos",
          "Si hay multiples modelos, compara su rendimiento en CV.",
          "",
          "### Rendimiento del Modelo Final",
          "Reporta TODAS las metricas del test set con interpretaciones.",
          "Tabla 1. Metricas de rendimiento del modelo en el conjunto de prueba.",
          "[Insertar Tabla 1 aqui]",
          "",
          "### Curva ROC y Discriminacion",
          "Interpreta el AUC y la capacidad discriminativa.",
          "Figura 1. Curva ROC del modelo.",
          "[Insertar Figura 1 aqui]",
          "",
          "### Matriz de Confusion",
          "Analiza verdaderos positivos/negativos y errores de clasificacion.",
          "Figura 2. Matriz de confusion.",
          "[Insertar Figura 2 aqui]",
          "",
          "### Calibracion del Modelo",
          "Si hay datos de calibracion, interpreta la confiabilidad de las probabilidades.",
          "",
          "### Optimizacion del Umbral de Decision",
          "Si se optimizo el threshold, explica el metodo y resultados.",
          "",
          "### Importancia de Variables",
          "Interpreta las variables mas importantes y su relevancia teorica.",
          "Tabla 2. Importancia de las variables predictoras.",
          "[Insertar Tabla 2 aqui]",
          "Figura 3. Grafico de importancia de variables.",
          "[Insertar Figura 3 aqui]",
          "",
          "### Analisis de Data Leakage",
          "Si se detecto o analizo, menciona los resultados.",
          "",
          "## Discusion",
          "Interpreta los hallazgos, limitaciones y direcciones futuras.",
          "",
          "## Referencias",
          "Incluye TODAS las referencias del verbose + adicionales relevantes.",
          "Formato APA 7, minimo 15 referencias.",
          "",
          "MINIMO 2500 palabras de texto narrativo fluido y detallado.",
          sep = "\n"
        )
      } else {
        paste(
          "You are an expert scientific writer specialized in Machine Learning and predictive analytics.",
          "Your task is to generate a COMPLETE AND DETAILED academic report in SCIENTIFIC ARTICLE STYLE.",
          "",
          "IMPORTANT: You will receive the COMPLETE OUTPUT (verbose) from the Machine Learning analysis.",
          "You must use ALL that information to write an exhaustive report including:",
          "- All preprocessing steps mentioned",
          "- Cross-validation details (folds, stratification)",
          "- Model comparison if multiple models",
          "- All metrics with their interpretations",
          "- Confusion matrix and its interpretation",
          "- Model calibration if available",
          "- Threshold optimization if applicable",
          "- Data leakage detection if mentioned",
          "- Variable importance",
          "- The bibliographic references from the verbose",
          "",
          "MANDATORY WRITING STYLE:",
          "1. EXTENSIVE PARAGRAPHS: Each paragraph must have minimum 150-200 words",
          "2. NARRATIVE FLOW: Ideas must connect with smooth transitions",
          "3. NO LISTS: Never use bullets (-), asterisks (*) or numbering for lists",
          "4. MANY CITATIONS: Integrate citations from verbose + additional relevant ones",
          "5. INLINE STATISTICS: Report values within narrative sentences",
          "6. IMPERSONAL VOICE: 'was observed', 'was estimated', 'results indicate'",
          "",
          "MANDATORY STRUCTURE:",
          "",
          "## Method",
          "### Participants",
          "### Data Preprocessing",
          "### Modeling Procedure",
          "",
          "## Results",
          "### Model Comparison",
          "### Final Model Performance",
          "Table 1. Model performance metrics.",
          "[Insert Table 1 here]",
          "",
          "### ROC Curve and Discrimination",
          "Figure 1. ROC curve.",
          "[Insert Figure 1 here]",
          "",
          "### Confusion Matrix",
          "Figure 2. Confusion matrix.",
          "[Insert Figure 2 here]",
          "",
          "### Model Calibration",
          "### Decision Threshold Optimization",
          "### Variable Importance",
          "Table 2. Variable importance.",
          "[Insert Table 2 here]",
          "Figure 3. Variable importance plot.",
          "[Insert Figure 3 here]",
          "",
          "### Data Leakage Analysis",
          "",
          "## Discussion",
          "",
          "## References (APA 7, minimum 15 references)",
          "",
          "MINIMUM 2500 words of fluid, detailed narrative text.",
          sep = "\n"
        )
      }

      incProgress(0.2, detail = "Construyendo datos...")

      # ---- Construir mensaje del usuario ----

      # Formatear metricas
      metrics_text <- paste(sapply(names(metrics), function(m) {
        val <- metrics[[m]]
        if (m %in% c("accuracy", "sensitivity", "specificity")) {
          sprintf("%s: %.2f%%", toupper(m), val * 100)
        } else {
          sprintf("%s: %.4f", toupper(m), val)
        }
      }), collapse = "\n")

      # Formatear importancia de variables
      var_text <- if (!is.null(var_imp) && nrow(var_imp) > 0) {
        paste(sapply(1:min(10, nrow(var_imp)), function(i) {
          sprintf("%d. %s (%.4f)", var_imp$rank[i], var_imp$variable[i], var_imp$importance[i])
        }), collapse = "\n")
      } else {
        "No disponible"
      }

      # Hiperparametros
      hyperparam_text <- if (model$tuned && !is.null(model$hyperparameters)) {
        paste(names(model$hyperparameters), "=", unlist(model$hyperparameters), collapse = ", ")
      } else {
        "No se realizo tuning"
      }

      # Obtener verbose_text completo si existe
      verbose_section <- ""
      if (!is.null(data$verbose_text) && nzchar(data$verbose_text)) {
        verbose_section <- paste(
          "",
          "## OUTPUT COMPLETO DEL ANALISIS (VERBOSE)",
          "=========================================",
          "A continuacion se presenta el output COMPLETO del analisis de Machine Learning.",
          "Usa TODA esta informacion para escribir un reporte MUY DETALLADO y COMPLETO.",
          "Incluye TODOS los detalles tecnicos, interpretaciones y referencias mencionadas.",
          "",
          "```",
          data$verbose_text,
          "```",
          "",
          sep = "\n"
        )
      }

      user_message <- paste(
        "# DATOS COMPLETOS PARA REPORTE ACADEMICO DE MACHINE LEARNING",
        "",
        "## RESUMEN DEL ESTUDIO",
        sprintf("Tipo de tarea: %s", ifelse(meta$task == "classification", "Clasificacion binaria", "Regresion")),
        sprintf("Variable objetivo: %s", meta$target),
        sprintf("Total de observaciones: N = %d", meta$n_observations),
        sprintf("Conjunto de entrenamiento: n = %d (%.1f%%)", meta$n_train, meta$n_train/meta$n_observations*100),
        sprintf("Conjunto de prueba: n = %d (%.1f%%)", meta$n_test, meta$n_test/meta$n_observations*100),
        "",
        "## MODELO SELECCIONADO",
        sprintf("Algoritmo: %s", model$label),
        sprintf("Tuning: %s", ifelse(model$tuned, "Si", "No")),
        sprintf("Hiperparametros: %s", hyperparam_text),
        "",
        "## METRICAS DE RENDIMIENTO (en test set)",
        metrics_text,
        "",
        if (!is.null(data$threshold)) {
          sprintf("Threshold optimizado: %.4f (default: 0.50)", data$threshold$optimized)
        } else {
          ""
        },
        "",
        "## IMPORTANCIA DE VARIABLES (Top 10)",
        var_text,
        verbose_section,
        "",
        sprintf("IDIOMA DEL REPORTE: %s", ifelse(input$language == "es", "ESPANOL", "ENGLISH")),
        "",
        "INSTRUCCIONES FINALES:",
        "- Usa TODA la informacion del verbose para escribir el reporte",
        "- Incluye detalles sobre preprocesamiento, validacion cruzada, metricas",
        "- Menciona las interpretaciones de cada metrica que aparecen en el verbose",
        "- Incluye las referencias bibliograficas mencionadas en el analisis",
        "- El reporte debe ser MUY COMPLETO y DETALLADO (minimo 2500 palabras)",
        "- Escribe en PARRAFOS EXTENSOS y fluidos (estilo articulo cientifico)",
        "- NO uses listas con bullets ni numeracion",
        "- Menciona 'vease Tabla 1', 'vease Figura 1' etc. donde corresponda",
        "",
        "GENERA EL REPORTE AHORA.",
        sep = "\n"
      )

      incProgress(0.3, detail = "Contactando ChatGPT...")

      # ---- Llamada a OpenAI ----
      selected_model <- input$model_choice
      clean_api_key <- trimws(input$api_key)

      req <- request("https://api.openai.com/v1/chat/completions") |>
        req_headers(
          Authorization = paste("Bearer", clean_api_key),
          "Content-Type" = "application/json"
        ) |>
        req_body_json(list(
          model = selected_model,
          temperature = 0.4,
          max_tokens = 16000,
          messages = list(
            list(role = "system", content = system_prompt),
            list(role = "user", content = user_message)
          )
        )) |>
        req_timeout(300)

      resp <- tryCatch({
        req_perform(req)
      }, error = function(e) {
        # Extraer mensaje mas detallado
        msg <- e$message
        # Si es error HTTP, intentar obtener el body
        if (grepl("HTTP", msg)) {
          list(error = TRUE, message = msg, type = "http")
        } else {
          list(error = TRUE, message = msg, type = "connection")
        }
      })

      if (is.list(resp) && !is.null(resp$error) && resp$error == TRUE) {
        showNotification(
          paste("Error de OpenAI:", resp$message),
          type = "error",
          duration = 10
        )
        return()
      }

      # Verificar status code
      if (inherits(resp, "httr2_response")) {
        status <- resp_status(resp)
        if (status == 401) {
          showNotification(
            "Error 401: API Key invalida. Verifica que:\n1. La key este completa\n2. No tenga espacios\n3. Tu cuenta tenga creditos",
            type = "error",
            duration = 15
          )
          return()
        } else if (status != 200) {
          body_text <- tryCatch(resp_body_string(resp), error = function(e) "")
          showNotification(
            paste("Error HTTP", status, ":", body_text),
            type = "error",
            duration = 10
          )
          return()
        }
      }

      incProgress(0.7, detail = "Procesando respuesta...")

      js <- tryCatch(resp_body_json(resp, simplifyVector = FALSE), error = function(e) NULL)

      if (is.null(js)) {
        showNotification("Respuesta JSON invalida", type = "error")
        return()
      }

      if (!is.null(js$error)) {
        showNotification(paste("Error API:", js$error$message), type = "error", duration = 10)
        return()
      }

      # Extraer texto
      txt <- ""
      if (!is.null(js$choices) && length(js$choices) > 0) {
        if (!is.null(js$choices[[1]]$message$content)) {
          txt <- js$choices[[1]]$message$content
        }
      }

      if (!nzchar(txt)) {
        showNotification("Respuesta vacia de la API", type = "error")
        return()
      }

      incProgress(0.9, detail = "Finalizando...")

      # Guardar resultado
      rv$report_md <- txt
      rv$last_generation <- Sys.time()

      # Convertir a HTML para preview
      html <- tryCatch(
        commonmark::markdown_html(txt),
        error = function(e) paste0("<pre>", htmltools::htmlEscape(txt), "</pre>")
      )
      rv$report_html <- html

      showNotification("Reporte generado exitosamente!", type = "message", duration = 5)
    })
  })

  # ---- Preview del reporte ----
  output$report_preview <- renderUI({
    if (is.null(rv$report_html)) {
      return(div(
        style = "text-align: center; padding: 50px; color: #999;",
        icon("file-alt", style = "font-size: 48px;"),
        tags$br(), tags$br(),
        tags$p("Cargue un archivo JSON y haga clic en"),
        tags$p(tags$strong("'Generar Reporte con IA'"))
      ))
    }

    HTML(rv$report_html)
  })

  # ---- Descargar DOCX ----
  output$download_docx <- downloadHandler(
    filename = function() {
      paste0("PsyML_Report_", format(Sys.Date(), "%Y%m%d"), ".docx")
    },
    content = function(file) {
      req(rv$report_md)
      req(json_data())

      tryCatch({
        data <- json_data()

        # Crear documento Word con tablas reales
        doc <- create_word_with_tables(
          report_text = rv$report_md,
          title = input$report_title,
          author = input$author_name,
          json_data = data
        )

        print(doc, target = file)

      }, error = function(e) {
        showNotification(paste("Error al crear DOCX:", e$message), type = "error")
      })
    }
  )

  # ---- Descargar Markdown ----
  output$download_md <- downloadHandler(
    filename = function() {
      paste0("PsyML_Report_", format(Sys.Date(), "%Y%m%d"), ".md")
    },
    content = function(file) {
      req(rv$report_md)

      # Agregar header
      header <- paste0(
        "# ", input$report_title, "\n",
        if (nzchar(input$author_name)) paste0("**Autor:** ", input$author_name, "\n") else "",
        "**Fecha:** ", format(Sys.Date(), "%Y-%m-%d"), "\n",
        "\n---\n\n"
      )

      writeLines(paste0(header, rv$report_md), file)
    }
  )
}


# =============================================================================
# Funciones Auxiliares para Word
# =============================================================================

#' Crear documento Word con tablas reales
create_word_with_tables <- function(report_text, title, author, json_data) {

  doc <- read_docx()

  # Titulo
  doc <- body_add_par(doc, title, style = "heading 1")

  if (!is.null(author) && nzchar(author)) {
    doc <- body_add_par(doc, author, style = "Normal")
  }

  doc <- body_add_par(doc, format(Sys.Date(), "%Y-%m-%d"), style = "Normal")
  doc <- body_add_par(doc, "", style = "Normal")

  # Procesar Markdown linea por linea
  md_lines <- unlist(strsplit(report_text, "\n", fixed = TRUE))
  i <- 1

  while (i <= length(md_lines)) {
    ln <- md_lines[i]

    # Lineas vacias
    if (nchar(trimws(ln)) == 0) {
      i <- i + 1
      next
    }

    # Separadores
    if (grepl("^---+$", ln)) {
      i <- i + 1
      next
    }

    # Headers nivel 1
    if (grepl("^#\\s+", ln) && !grepl("^##", ln)) {
      texto <- gsub("^#\\s+", "", ln)
      texto <- gsub("\\*\\*", "", texto)
      doc <- body_add_par(doc, texto, style = "heading 1")
      i <- i + 1
      next
    }

    # Headers nivel 2
    if (grepl("^##\\s+", ln) && !grepl("^###", ln)) {
      texto <- gsub("^##\\s+", "", ln)
      texto <- gsub("\\*\\*", "", texto)
      doc <- body_add_par(doc, texto, style = "heading 2")
      i <- i + 1
      next
    }

    # Headers nivel 3
    if (grepl("^###\\s+", ln)) {
      texto <- gsub("^###\\s+", "", ln)
      texto <- gsub("\\*\\*", "", texto)
      doc <- body_add_par(doc, texto, style = "heading 3")
      i <- i + 1
      next
    }

    # Placeholder de Tabla 1 - Insertar tabla de metricas real
    if (grepl("\\[Insertar Tabla 1", ln, ignore.case = TRUE) ||
        grepl("\\[Insert Table 1", ln, ignore.case = TRUE)) {
      doc <- add_metrics_table(doc, json_data)
      i <- i + 1
      next
    }

    # Placeholder de Tabla 2 - Insertar tabla de importancia de variables
    if (grepl("\\[Insertar Tabla 2", ln, ignore.case = TRUE) ||
        grepl("\\[Insert Table 2", ln, ignore.case = TRUE)) {
      doc <- add_importance_table(doc, json_data)
      i <- i + 1
      next
    }

    # Otros placeholders de figuras/tablas
    if (grepl("^\\[Insertar", ln, ignore.case = TRUE) ||
        grepl("^\\[Insert", ln, ignore.case = TRUE)) {
      doc <- body_add_par(doc, ln, style = "Normal")
      i <- i + 1
      next
    }

    # Titulo de Tabla o Figura
    if (grepl("^Tabla\\s+\\d+", ln) || grepl("^Table\\s+\\d+", ln) ||
        grepl("^Figura\\s+\\d+", ln) || grepl("^Figure\\s+\\d+", ln)) {
      texto <- gsub("\\*\\*", "", ln)
      texto <- gsub("\\*", "", texto)
      doc <- body_add_par(doc, texto, style = "Normal")
      i <- i + 1
      next
    }

    # Detectar tabla markdown (linea con |)
    if (grepl("^\\|", ln) && grepl("\\|$", ln)) {
      table_lines <- c()
      while (i <= length(md_lines) && grepl("^\\|", md_lines[i])) {
        table_lines <- c(table_lines, md_lines[i])
        i <- i + 1
      }
      doc <- add_markdown_table(doc, table_lines)
      next
    }

    # Parrafos normales
    texto <- ln
    texto <- gsub("\\*\\*([^*]+)\\*\\*", "\\1", texto)
    texto <- gsub("\\*([^*]+)\\*", "\\1", texto)
    texto <- gsub("_([^_]+)_", "\\1", texto)
    doc <- body_add_par(doc, texto, style = "Normal")
    i <- i + 1
  }

  return(doc)
}


#' Agregar tabla de metricas
add_metrics_table <- function(doc, json_data) {

  metrics <- json_data$metrics
  if (is.null(metrics)) {
    doc <- body_add_par(doc, "[Metricas no disponibles]", style = "Normal")
    return(doc)
  }

  metric_names <- names(metrics)
  metric_values <- unlist(metrics)

  formatted_names <- sapply(metric_names, function(m) {
    switch(m,
      "roc_auc" = "ROC-AUC",
      "accuracy" = "Accuracy",
      "sensitivity" = "Sensitivity",
      "specificity" = "Specificity",
      "f_meas" = "F1-Score",
      "pr_auc" = "PR-AUC",
      "bal_accuracy" = "Balanced Accuracy",
      "rmse" = "RMSE",
      "rsq" = "R-squared",
      "mae" = "MAE",
      toupper(m)
    )
  })

  formatted_values <- sapply(seq_along(metric_values), function(i) {
    m <- metric_names[i]
    val <- metric_values[i]
    if (m %in% c("accuracy", "sensitivity", "specificity", "bal_accuracy")) {
      sprintf("%.2f%%", val * 100)
    } else {
      sprintf("%.4f", val)
    }
  })

  interpretations <- sapply(seq_along(metric_values), function(i) {
    m <- metric_names[i]
    val <- metric_values[i]
    if (m == "roc_auc") {
      if (val >= 0.9) "Excelente"
      else if (val >= 0.8) "Bueno"
      else if (val >= 0.7) "Aceptable"
      else "Bajo"
    } else if (m %in% c("accuracy", "sensitivity", "specificity")) {
      if (val >= 0.9) "Excelente"
      else if (val >= 0.8) "Bueno"
      else if (val >= 0.7) "Aceptable"
      else "Bajo"
    } else {
      ""
    }
  })

  df <- data.frame(
    Metrica = formatted_names,
    Valor = formatted_values,
    Interpretacion = interpretations,
    stringsAsFactors = FALSE
  )

  ft <- flextable(df)
  ft <- set_header_labels(ft, Metrica = "Metrica", Valor = "Valor", Interpretacion = "Interpretacion")
  ft <- theme_booktabs(ft)
  ft <- autofit(ft)
  ft <- align(ft, align = "center", part = "all")
  ft <- bold(ft, part = "header")
  ft <- fontsize(ft, size = 10, part = "all")

  doc <- body_add_flextable(doc, ft)
  doc <- body_add_par(doc, "", style = "Normal")
  return(doc)
}


#' Agregar tabla de importancia de variables
add_importance_table <- function(doc, json_data) {

  var_imp <- json_data$variable_importance
  if (is.null(var_imp)) {
    doc <- body_add_par(doc, "[Importancia de variables no disponible]", style = "Normal")
    return(doc)
  }

  if (is.list(var_imp) && !is.data.frame(var_imp)) {
    df <- do.call(rbind, lapply(var_imp, function(v) {
      data.frame(
        Rango = v$rank,
        Variable = v$variable,
        Importancia = v$importance,
        stringsAsFactors = FALSE
      )
    }))
  } else {
    df <- data.frame(
      Rango = var_imp$rank,
      Variable = var_imp$variable,
      Importancia = var_imp$importance,
      stringsAsFactors = FALSE
    )
  }

  df$Importancia <- sprintf("%.4f", df$Importancia)
  df <- head(df, 10)

  ft <- flextable(df)
  ft <- set_header_labels(ft, Rango = "Rango", Variable = "Variable", Importancia = "Importancia")
  ft <- theme_booktabs(ft)
  ft <- autofit(ft)
  ft <- align(ft, j = 1, align = "center", part = "all")
  ft <- align(ft, j = 2, align = "left", part = "all")
  ft <- align(ft, j = 3, align = "center", part = "all")
  ft <- bold(ft, part = "header")
  ft <- fontsize(ft, size = 10, part = "all")

  doc <- body_add_flextable(doc, ft)
  doc <- body_add_par(doc, "", style = "Normal")
  return(doc)
}


#' Agregar tabla markdown al Word
add_markdown_table <- function(doc, table_lines) {

  if (length(table_lines) < 2) {
    return(doc)
  }

  parse_row <- function(line) {
    cells <- strsplit(line, "\\|")[[1]]
    cells <- cells[nzchar(trimws(cells))]
    trimws(cells)
  }

  header <- parse_row(table_lines[1])

  data_rows <- list()
  for (k in 3:length(table_lines)) {
    if (k <= length(table_lines)) {
      row <- parse_row(table_lines[k])
      if (length(row) > 0) {
        data_rows[[length(data_rows) + 1]] <- row
      }
    }
  }

  if (length(data_rows) == 0) {
    return(doc)
  }

  df <- do.call(rbind, lapply(data_rows, function(r) {
    if (length(r) < length(header)) {
      r <- c(r, rep("", length(header) - length(r)))
    } else if (length(r) > length(header)) {
      r <- r[1:length(header)]
    }
    r
  }))
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  names(df) <- header

  ft <- flextable(df)
  ft <- theme_booktabs(ft)
  ft <- autofit(ft)
  ft <- align(ft, align = "center", part = "all")
  ft <- bold(ft, part = "header")
  ft <- fontsize(ft, size = 10, part = "all")

  doc <- body_add_flextable(doc, ft)
  doc <- body_add_par(doc, "", style = "Normal")
  return(doc)
}


# =============================================================================
# Run App
# =============================================================================

shinyApp(ui = ui, server = server)
