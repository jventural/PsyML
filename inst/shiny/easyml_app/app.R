# =============================================================================
# PsyML Integrated App - Pipeline ML + Sample Size + AI Report
# =============================================================================
# Aplicacion Shiny integrada que combina las 3 funcionalidades principales
# de PsyML en una sola interfaz con shinydashboard.
#
# Uso:
#   library(PsyML)
#   launch_easyml_app()
#
# =============================================================================

library(shiny)
library(shinydashboard)
library(jsonlite)
library(officer)
library(httr)
library(httr2)
library(commonmark)
library(flextable)
library(DT)

# =============================================================================
# UI
# =============================================================================

ui <- dashboardPage(
  skin = "black",

  dashboardHeader(
    title = span(icon("brain"), " PsyML"),
    titleWidth = 250
  ),

  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "tabs",
      menuItem("Data", tabName = "tab_data",
               icon = icon("database")),
      menuItem("Sample Size", tabName = "tab_ss",
               icon = icon("chart-area")),
      menuItem("Pipeline ML", tabName = "tab_ml",
               icon = icon("cogs")),
      menuItem("AI Report", tabName = "tab_report",
               icon = icon("file-alt"))
    ),
    hr(),
    div(style = "padding: 10px; color: #d0d5e0; font-size: 11px;",
      p("PsyML v1.0.0")
    )
  ),

  dashboardBody(
    tags$head(
      tags$link(
        href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap",
        rel = "stylesheet"
      ),
      tags$style(HTML("
        /* ============================================================
           PsyML — Oxford Ink Theme
           Flat, academic, print-ready. No gradients.
           ============================================================ */

        /* --- Typography --- */
        body, .content-wrapper, .main-sidebar, .box {
          font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
        }

        /* --- Content area --- */
        .content-wrapper {
          background-color: #f2f3f5;
        }

        /* --- Header --- */
        .skin-black .main-header .logo {
          background-color: #1b2a4a;
          font-weight: 700;
          letter-spacing: 0.5px;
          color: #ffffff;
        }
        .skin-black .main-header .logo:hover {
          background-color: #243b6a;
        }
        .skin-black .main-header .navbar {
          background-color: #243b6a;
        }
        .skin-black .main-header .navbar .sidebar-toggle {
          color: #ffffff;
        }
        .skin-black .main-header .navbar .sidebar-toggle:hover {
          background-color: rgba(255,255,255,0.1);
        }

        /* --- Sidebar --- */
        .skin-black .main-sidebar,
        .skin-black .left-side {
          background-color: #1b2a4a;
        }
        .skin-black .sidebar-menu > li > a {
          color: #d0d5e0;
          border-left: 3px solid transparent;
          transition: all 0.15s ease;
        }
        .skin-black .sidebar-menu > li > a:hover {
          background-color: rgba(255, 255, 255, 0.07);
          border-left: 3px solid #d0d5e0;
          color: #ffffff;
        }
        .skin-black .sidebar-menu > li.active > a {
          background-color: rgba(255, 255, 255, 0.1);
          border-left: 3px solid #ffffff;
          color: #ffffff;
          font-weight: 600;
        }
        .skin-black .sidebar-menu > li > .treeview-menu {
          background-color: #162240;
        }
        .skin-black .sidebar a { color: #d0d5e0; }

        /* --- Box overrides (uniform neutral style) --- */
        .box {
          border-radius: 6px;
          box-shadow: 0 1px 3px rgba(0,0,0,0.06);
          border-top: 2px solid #d0d4dc;
          background-color: #ffffff;
        }
        .box.box-solid.box-primary,
        .box.box-solid.box-success,
        .box.box-solid.box-info,
        .box.box-solid.box-warning {
          border-top: 2px solid #d0d4dc;
        }
        .box.box-solid.box-primary > .box-header,
        .box.box-solid.box-success > .box-header,
        .box.box-solid.box-info > .box-header,
        .box.box-solid.box-warning > .box-header {
          background-color: #f7f8fa;
          border-radius: 6px 6px 0 0;
          color: #1b2a4a;
          border-bottom: 1px solid #e8e9ed;
        }
        .box.box-solid.box-primary > .box-body,
        .box.box-solid.box-success > .box-body,
        .box.box-solid.box-info > .box-body,
        .box.box-solid.box-warning > .box-body {
          background-color: #ffffff;
        }
        .box-header {
          font-weight: 600;
        }
        .box-header .box-title {
          font-weight: 600;
          color: #1b2a4a;
        }
        .box.box-solid > .box-header > .box-tools .btn {
          color: #6b7280;
        }
        .box-body {
          padding: 15px 20px;
          color: #333333;
        }

        /* --- Run buttons --- */
        .btn-run {
          width: 100%; font-weight: 700; border-radius: 6px;
          margin-top: 10px; margin-bottom: 10px; font-size: 15px; padding: 12px;
          border: none;
          background-color: #1b2a4a;
          color: #ffffff;
          transition: all 0.15s ease;
          letter-spacing: 0.3px;
        }
        .btn-run:hover {
          background-color: #243b6a;
          color: #ffffff;
        }
        .btn-run:active {
          background-color: #162240;
        }

        /* --- General buttons --- */
        .btn-primary {
          background-color: #1b2a4a;
          border: none;
          color: #ffffff;
        }
        .btn-primary:hover {
          background-color: #243b6a;
          color: #ffffff;
        }
        .btn-warning {
          background-color: #1b2a4a;
          border: none;
          color: #ffffff;
        }
        .btn-warning:hover {
          background-color: #243b6a;
          color: #ffffff;
        }
        .btn-success {
          background-color: #1b2a4a;
          border: none;
          color: #ffffff;
        }
        .btn-success:hover {
          background-color: #243b6a;
          color: #ffffff;
        }
        .btn-default {
          border-radius: 6px;
          border: 1px solid #d0d4dc;
          color: #1b2a4a;
          transition: all 0.15s ease;
        }
        .btn-default:hover {
          background-color: #f7f8fa;
          border-color: #1b2a4a;
        }

        /* --- Verbose terminal --- */
        .verbose-output {
          max-height: 500px; overflow-y: auto; font-size: 12px;
          background: #fafafa; color: #333333; padding: 18px; border-radius: 6px;
          font-family: 'JetBrains Mono', 'Fira Code', 'Consolas', monospace;
          white-space: pre-wrap;
          border: 1px solid #e0e0e0;
        }

        /* --- Metric cards --- */
        .metric-card {
          text-align: center; padding: 16px 12px;
          background-color: #1b2a4a;
          color: white; border-radius: 6px; margin: 5px; display: inline-block;
          width: 45%;
        }
        .metric-value { font-size: 26px; font-weight: 700; color: #ffffff; }
        .metric-label { font-size: 11px; color: #d0d5e0; letter-spacing: 0.3px; }

        /* --- Report preview --- */
        .report-preview {
          background: #ffffff; padding: 24px; border-radius: 6px;
          max-height: 600px; overflow-y: auto;
          border: 1px solid #e0e0e0;
        }

        /* --- Info text --- */
        .info-text { color: #6b7280; font-size: 12px; margin-top: 5px; }

        /* --- Section headers --- */
        .section-header {
          background-color: #1b2a4a;
          padding: 10px 15px; border-radius: 6px; color: #ffffff; margin-bottom: 15px;
        }
        .section-header h4 { margin: 0; font-weight: 600; }

        /* --- Recommendation card --- */
        .ss-recommendation {
          background: #f7f8fa; border-left: 4px solid #1b2a4a;
          padding: 15px; border-radius: 0 6px 6px 0; margin: 10px 0;
        }

        /* --- AI response card --- */
        .ai-response {
          background: #f7f8fa; border-left: 4px solid #1b2a4a;
          padding: 15px; border-radius: 0 6px 6px 0; margin: 10px 0;
          font-size: 14px; line-height: 1.7; white-space: pre-wrap;
        }

        /* --- Form inputs --- */
        .form-control {
          border-radius: 4px;
          border: 1px solid #d0d4dc;
          transition: border-color 0.15s ease, box-shadow 0.15s ease;
        }
        .form-control:focus {
          border-color: #1b2a4a;
          box-shadow: 0 0 0 2px rgba(27, 42, 74, 0.1);
        }

        /* --- Select inputs --- */
        .selectize-input {
          border-radius: 4px !important;
          border: 1px solid #d0d4dc !important;
        }
        .selectize-input.focus {
          border-color: #1b2a4a !important;
          box-shadow: 0 0 0 2px rgba(27, 42, 74, 0.1) !important;
        }

        /* --- Slider accent --- */
        .irs--shiny .irs-bar {
          background: #1b2a4a;
          border-top: 1px solid #1b2a4a;
          border-bottom: 1px solid #1b2a4a;
        }
        .irs--shiny .irs-from, .irs--shiny .irs-to,
        .irs--shiny .irs-single {
          background-color: #1b2a4a;
        }
        .irs--shiny .irs-handle {
          border: 2px solid #1b2a4a;
          background: #ffffff;
        }

        /* --- Checkbox accent --- */
        .checkbox input[type='checkbox']:checked + span::before,
        .icheckbox_square-blue.checked {
          border-color: #1b2a4a;
        }

        /* --- DataTables --- */
        .dataTables_wrapper .dataTables_paginate .paginate_button.current {
          background: #1b2a4a !important;
          color: #ffffff !important;
          border: none !important;
          border-radius: 4px;
        }
        .dataTables_wrapper .dataTables_paginate .paginate_button:hover {
          background: #f7f8fa !important;
          color: #1b2a4a !important;
          border: none !important;
        }
        table.dataTable thead th {
          background-color: #f7f8fa;
          font-weight: 600;
          color: #1b2a4a;
          border-bottom: 2px solid #d0d4dc;
        }

        /* --- Notification badges --- */
        .skin-black .main-header .navbar .nav > li > a {
          color: #d0d5e0;
        }

        /* --- Scrollbar styling --- */
        .verbose-output::-webkit-scrollbar,
        .report-preview::-webkit-scrollbar {
          width: 6px;
        }
        .verbose-output::-webkit-scrollbar-track,
        .report-preview::-webkit-scrollbar-track {
          background: transparent;
        }
        .verbose-output::-webkit-scrollbar-thumb {
          background: #d0d4dc;
          border-radius: 3px;
        }
        .report-preview::-webkit-scrollbar-thumb {
          background: #d0d4dc;
          border-radius: 3px;
        }

        /* --- Sub-section headers in Advanced Options --- */
        .box-body h5 {
          color: #1b2a4a;
          margin-top: 5px;
        }
        .box-body hr {
          border-top: 1px solid #e8e9ed;
        }

        /* --- Download buttons row --- */
        .btn-primary[id*='download'] {
          border-radius: 6px;
        }
      "))
    ),

    tabItems(

      # =====================================================================
      # TAB 0: Data Upload (Shared)
      # =====================================================================
      tabItem(tabName = "tab_data",
        fluidRow(
          column(4,
            box(
              title = tagList(icon("upload"), "Upload Data"),
              status = "primary", solidHeader = TRUE, width = NULL,
              fileInput("data_file", "Upload CSV or Excel:",
                        accept = c(".csv", ".xlsx", ".xls")),
              uiOutput("data_info_ui")
            ),
            box(
              title = tagList(icon("crosshairs"), "Target Variable"),
              status = "info", solidHeader = TRUE, width = NULL,
              uiOutput("data_target_ui")
            ),
            box(
              title = tagList(icon("check-square"), "Predictors"),
              status = "warning", solidHeader = TRUE, width = NULL,
              collapsible = TRUE, collapsed = TRUE,
              uiOutput("data_predictors_ui"),
              div(class = "info-text",
                "Uncheck variables to exclude from all analyses."
              )
            )
          ),
          column(8,
            box(
              title = tagList(icon("table"), "Data Preview"),
              status = "success", solidHeader = TRUE, width = NULL,
              DT::dataTableOutput("data_preview_table")
            ),
            box(
              title = tagList(icon("info-circle"), "Data Summary"),
              status = "info", solidHeader = TRUE, width = NULL,
              collapsible = TRUE, collapsed = TRUE,
              verbatimTextOutput("data_summary")
            )
          )
        )
      ),

      # =====================================================================
      # TAB 1: Pipeline ML
      # =====================================================================
      tabItem(tabName = "tab_ml",
        fluidRow(
          # --- Panel izquierdo: configuracion ---
          column(4,
            box(
              title = tagList(icon("sliders-h"), "Configuration"),
              status = "info", solidHeader = TRUE, width = NULL,
              collapsible = TRUE,

              selectInput("ml_task", "Task type:",
                          choices = c("Auto-detect" = "auto",
                                      "Classification" = "classification",
                                      "Regression" = "regression"),
                          selected = "auto"),

              checkboxGroupInput("ml_models", "Algorithms:",
                                 choices = c("Random Forest" = "rf",
                                             "XGBoost" = "xgboost",
                                             "SVM" = "svm",
                                             "Neural Network" = "nnet",
                                             "GLM" = "glm",
                                             "Decision Tree" = "tree"),
                                 selected = c("rf", "xgboost", "svm",
                                              "nnet", "glm", "tree")),

              uiOutput("ml_metric_ui"),

              sliderInput("ml_test_split", "Test proportion:",
                          min = 0.10, max = 0.40, value = 0.20, step = 0.05),

              sliderInput("ml_cv_folds", "CV Folds:",
                          min = 3, max = 20, value = 10, step = 1)
            ),

            box(
              title = tagList(icon("filter"), "Preprocessing"),
              status = "info", solidHeader = TRUE, width = NULL,
              collapsible = TRUE, collapsed = TRUE,

              checkboxInput("ml_impute", "Impute missing values", value = TRUE),
              conditionalPanel(
                condition = "input.ml_impute == true",
                selectInput("ml_impute_method", "Imputation method:",
                            choices = c("KNN" = "knn", "Median" = "median",
                                        "Mean" = "mean"),
                            selected = "knn")
              ),

              checkboxInput("ml_normalize", "Normalize variables", value = TRUE),
              conditionalPanel(
                condition = "input.ml_normalize == true",
                selectInput("ml_normalize_method", "Normalization method:",
                            choices = c("Z-score" = "zscore", "Min-Max" = "minmax"),
                            selected = "zscore")
              ),

              checkboxInput("ml_treat_outliers", "Treat outliers (winsorize)",
                            value = TRUE),
              conditionalPanel(
                condition = "input.ml_treat_outliers == true",
                numericInput("ml_outlier_percentile", "Outlier percentile:",
                             value = 0.05, min = 0.01, max = 0.10, step = 0.01)
              ),

              checkboxInput("ml_remove_high_cor", "Remove high correlation",
                            value = TRUE),
              conditionalPanel(
                condition = "input.ml_remove_high_cor == true",
                numericInput("ml_cor_threshold", "Correlation threshold:",
                             value = 0.90, min = 0.50, max = 0.99, step = 0.05)
              ),

              checkboxInput("ml_remove_high_vif", "Remove high VIF",
                            value = TRUE),
              conditionalPanel(
                condition = "input.ml_remove_high_vif == true",
                numericInput("ml_vif_threshold", "VIF threshold:",
                             value = 5, min = 2, max = 20, step = 1)
              ),

              checkboxInput("ml_use_pca", "Apply PCA", value = FALSE),
              conditionalPanel(
                condition = "input.ml_use_pca == true",
                numericInput("ml_pca_threshold", "PCA variance retention:",
                             value = 0.95, min = 0.80, max = 0.99, step = 0.01)
              )
            ),

            box(
              title = tagList(icon("wrench"), "Advanced Options"),
              status = "warning", solidHeader = TRUE, width = NULL,
              collapsible = TRUE, collapsed = TRUE,

              tags$h5(tags$strong(icon("magic"), " Tuning")),
              checkboxInput("ml_tune", "Tune best model", value = TRUE),
              conditionalPanel(
                condition = "input.ml_tune == true",
                selectInput("ml_tune_method", "Tune method:",
                            choices = c("Random Search" = "random",
                                        "Grid Search" = "grid",
                                        "Bayesian" = "bayes",
                                        "Racing ANOVA" = "racing"),
                            selected = "random"),
                numericInput("ml_tune_grid", "Grid size:", value = 20,
                             min = 5, max = 100),
                conditionalPanel(
                  condition = "input.ml_tune_method == 'bayes'",
                  numericInput("ml_tune_iter", "Bayesian iterations:",
                               value = 30, min = 10, max = 200)
                )
              ),

              hr(),
              tags$h5(tags$strong(icon("cogs"), " Feature Engineering")),
              checkboxInput("ml_feature_eng", "Auto feature engineering",
                            value = FALSE),
              checkboxInput("ml_feature_selection",
                            "Feature selection (Boruta)", value = FALSE),

              hr(),
              tags$h5(tags$strong(icon("balance-scale"), " Class Balancing")),
              checkboxInput("ml_balance_classes", "Balance classes",
                            value = FALSE),
              conditionalPanel(
                condition = "input.ml_balance_classes == true",
                selectInput("ml_balance_method", "Balance method:",
                            choices = c("SMOTE" = "smote",
                                        "ADASYN" = "adasyn",
                                        "ROSE" = "rose",
                                        "Upsample" = "up",
                                        "Downsample" = "down"),
                            selected = "smote")
              ),

              hr(),
              tags$h5(tags$strong(icon("stethoscope"), " Analysis & Diagnostics")),
              checkboxInput("ml_run_eda", "Run EDA", value = TRUE),
              checkboxInput("ml_run_shap", "Calculate SHAP", value = TRUE),
              conditionalPanel(
                condition = "input.ml_run_shap == true",
                numericInput("ml_n_shap", "SHAP observations:", value = 100,
                             min = 10, max = 500, step = 10)
              ),
              checkboxInput("ml_optimize_threshold", "Optimize threshold",
                            value = TRUE),
              conditionalPanel(
                condition = "input.ml_optimize_threshold == true",
                selectInput("ml_threshold_method", "Threshold method:",
                            choices = c("Youden" = "youden", "F1" = "f1",
                                        "Balanced" = "balanced"),
                            selected = "youden")
              ),
              checkboxInput("ml_calibrate_probs", "Calibrate probabilities",
                            value = FALSE),
              conditionalPanel(
                condition = "input.ml_calibrate_probs == true",
                selectInput("ml_calibration_method", "Calibration method:",
                            choices = c("Platt" = "platt",
                                        "Isotonic" = "isotonic"),
                            selected = "platt")
              ),
              checkboxInput("ml_check_leakage", "Check data leakage",
                            value = TRUE),
              checkboxInput("ml_nested_cv", "Nested CV", value = FALSE),
              checkboxInput("ml_analyze_interactions",
                            "Analyze interactions", value = TRUE),

              hr(),
              tags$h5(tags$strong(icon("sync"), " Execution")),
              numericInput("ml_seed", "Seed:", value = 2024,
                           min = 1, max = 99999),
              checkboxInput("ml_verbose", "Verbose output", value = TRUE)
            ),

            actionButton("ml_run_btn",
                         label = tagList(icon("play"), " Run Pipeline"),
                         class = "btn-success btn-run")
          ),

          # --- Panel derecho: resultados ---
          column(8,
            # Status
            uiOutput("ml_status_box"),

            # Metricas
            box(
              title = tagList(icon("chart-bar"), "Model Metrics"),
              status = "success", solidHeader = TRUE, width = NULL,
              collapsible = TRUE,
              uiOutput("ml_metrics_cards")
            ),

            # Verbose output
            box(
              title = tagList(icon("terminal"), "Verbose Output"),
              status = "primary", solidHeader = TRUE, width = NULL,
              collapsible = TRUE, collapsed = TRUE,
              div(class = "verbose-output",
                  verbatimTextOutput("ml_verbose")),
              div(style = "margin-top: 10px;",
                downloadButton("ml_download_txt",
                               tagList(icon("file-alt"), " Download TXT"),
                               class = "btn-default",
                               style = "width: 100%; border-radius: 20px;")
              )
            ),

            # Plots
            box(
              title = tagList(icon("images"), "Plots"),
              status = "info", solidHeader = TRUE, width = NULL,
              collapsible = TRUE,
              uiOutput("ml_plots_ui")
            ),

            # AI Interpretation
            box(
              title = tagList(icon("robot"), "AI Interpretation"),
              status = "warning", solidHeader = TRUE, width = NULL,
              collapsible = TRUE, collapsed = TRUE,
              div(style = "margin-bottom: 10px;",
                selectInput("ml_ai_language", "Language:",
                            choices = c("Spanish" = "es", "English" = "en"),
                            selected = "es", width = "200px")
              ),
              actionButton("ml_ai_btn",
                           label = tagList(icon("brain"), " Ask AI"),
                           class = "btn-warning",
                           style = "width: 100%; font-weight: bold; border-radius: 20px; margin-bottom: 15px;"),
              div(class = "info-text", style = "margin-bottom: 10px;",
                "Free: Groq Llama 3.3 70B. No API key needed."
              ),
              uiOutput("ml_ai_response")
            )
          )
        )
      ),

      # =====================================================================
      # TAB 2: Sample Size Estimation
      # =====================================================================
      tabItem(tabName = "tab_ss",
        fluidRow(
          # --- Panel izquierdo: configuracion ---
          column(4,
            box(
              title = tagList(icon("flask"), "Simulation Settings"),
              status = "primary", solidHeader = TRUE, width = NULL,

              selectInput("ss_task", "Task type:",
                          choices = c("Classification" = "classification",
                                      "Regression" = "regression"),
                          selected = "classification"),

              selectInput("ss_model", "Algorithm(s):",
                          choices = c("Random Forest" = "rf",
                                      "XGBoost" = "xgboost",
                                      "SVM" = "svm",
                                      "GLM" = "glm"),
                          selected = "rf", multiple = TRUE),

              uiOutput("ss_metric_ui"),

              numericInput("ss_target", "Target metric value:",
                           value = 0.80, min = 0, max = 1, step = 0.05),

              sliderInput("ss_reps", "Monte Carlo repetitions:",
                          min = 10, max = 200, value = 50, step = 10)
            ),

            box(
              title = tagList(icon("ruler-combined"), "Sample Size Grid"),
              status = "info", solidHeader = TRUE, width = NULL,
              collapsible = TRUE,

              textInput("ss_n_grid", "Sample sizes (comma-separated):",
                        value = "50,100,150,200,300,400,600,800,1000"),

              numericInput("ss_n_test", "Test set size:", value = 5000,
                           min = 100, max = 50000, step = 100)
            ),

            box(
              title = tagList(icon("cog"), "Advanced"),
              status = "warning", solidHeader = TRUE, width = NULL,
              collapsible = TRUE, collapsed = TRUE,

              div(class = "section-header",
                h4("Data Source")
              ),

              radioButtons("ss_data_source", NULL,
                           choices = c("Use uploaded data (Data tab)" = "from_data",
                                       "Simulate data" = "simulate"),
                           selected = "from_data"),

              conditionalPanel(
                condition = "input.ss_data_source == 'simulate'",
                numericInput("ss_sim_p", "Number of predictors:", value = 20,
                             min = 2, max = 100),
                sliderInput("ss_sim_signal", "Signal strength:",
                            min = 0.5, max = 5, value = 2.5, step = 0.5),
                numericInput("ss_sim_prevalence", "Prevalence (classif.):",
                             value = 0.20, min = 0.05, max = 0.50, step = 0.05)
              ),

              hr(),
              numericInput("ss_n_outer", "Outer splits (robustness):",
                           value = 1, min = 1, max = 10),
              checkboxInput("ss_bootstrap", "Bootstrap CI", value = FALSE),
              numericInput("ss_seed", "Seed:", value = 123,
                           min = 1, max = 99999)
            ),

            actionButton("ss_run_btn",
                         label = tagList(icon("play"), " Estimate Sample Size"),
                         class = "btn-success btn-run")
          ),

          # --- Panel derecho: resultados ---
          column(8,
            uiOutput("ss_status_box"),

            box(
              title = tagList(icon("chart-line"), "Learning Curve"),
              status = "success", solidHeader = TRUE, width = NULL,
              plotOutput("ss_plot", height = "450px")
            ),

            box(
              title = tagList(icon("table"), "Summary Table"),
              status = "primary", solidHeader = TRUE, width = NULL,
              collapsible = TRUE,
              DT::dataTableOutput("ss_summary_table")
            ),

            box(
              title = tagList(icon("check-circle"), "Recommendation"),
              status = "info", solidHeader = TRUE, width = NULL,
              collapsible = TRUE,
              uiOutput("ss_recommendation")
            )
          )
        )
      ),

      # =====================================================================
      # TAB 3: AI Report Generator
      # =====================================================================
      tabItem(tabName = "tab_report",
        fluidRow(
          # --- Panel izquierdo: configuracion ---
          column(4,
            box(
              title = tagList(icon("key"), "OpenAI API"),
              status = "primary", solidHeader = TRUE, width = NULL,
              passwordInput("report_api_key", "API Key:",
                            placeholder = "sk-proj-..."),
              div(style = "text-align: center; margin-bottom: 10px;",
                tags$a(href = "https://platform.openai.com/api-keys",
                       target = "_blank",
                       style = "color: #1b2a4a; font-size: 12px;",
                       icon("external-link-alt"), " Get API Key")
              ),
              selectInput("report_model", "Model:",
                          choices = c("GPT-4.1-mini" = "gpt-4.1-mini",
                                      "GPT-4o-mini" = "gpt-4o-mini",
                                      "GPT-4o" = "gpt-4o",
                                      "GPT-4.1" = "gpt-4.1"),
                          selected = "gpt-4.1-mini")
            ),

            box(
              title = tagList(icon("database"), "Data Source"),
              status = "info", solidHeader = TRUE, width = NULL,

              radioButtons("report_source", "Source:",
                           choices = c("From Pipeline (Tab 1)" = "pipeline",
                                       "Upload JSON" = "json_upload",
                                       "Upload TXT" = "txt_upload"),
                           selected = "pipeline"),

              conditionalPanel(
                condition = "input.report_source == 'json_upload'",
                fileInput("report_json_file", "Upload JSON:",
                          accept = ".json")
              ),

              conditionalPanel(
                condition = "input.report_source == 'txt_upload'",
                fileInput("report_txt_file", "Upload TXT:",
                          accept = c(".txt", ".text"))
              ),

              uiOutput("report_source_status")
            ),

            box(
              title = tagList(icon("sliders-h"), "Report Settings"),
              status = "warning", solidHeader = TRUE, width = NULL,

              textInput("report_title", "Title:",
                        value = "Machine Learning Analysis",
                        width = "100%"),
              textInput("report_author", "Author(s):",
                        placeholder = "Author name", width = "100%"),

              selectInput("report_language", "Language:",
                          choices = c("Spanish" = "es", "English" = "en"),
                          selected = "es"),

              textAreaInput("report_context", "Additional context (optional):",
                            placeholder = "E.g.: study objective, clinical context...",
                            rows = 3, width = "100%")
            ),

            actionButton("report_gen_btn",
                         label = tagList(icon("magic"), " Generate Report"),
                         class = "btn-success btn-run"),

            div(style = "display: flex; gap: 10px; margin-top: 10px;",
              downloadButton("report_download_docx",
                             tagList(icon("file-word"), " DOCX"),
                             class = "btn-primary",
                             style = "flex: 1; border-radius: 20px;"),
              downloadButton("report_download_md",
                             tagList(icon("file-alt"), " MD"),
                             class = "btn-default",
                             style = "flex: 1; border-radius: 20px;")
            )
          ),

          # --- Panel derecho: resultados ---
          column(8,
            box(
              title = tagList(icon("info-circle"), "Analysis Info"),
              status = "primary", solidHeader = TRUE, width = NULL,
              collapsible = TRUE, collapsed = TRUE,
              fluidRow(
                column(6, uiOutput("report_json_info")),
                column(6,
                  uiOutput("report_metrics_cards"),
                  tableOutput("report_var_table")
                )
              )
            ),

            box(
              title = tagList(icon("file-alt"), "Report Preview"),
              status = "success", solidHeader = TRUE, width = NULL,
              div(class = "report-preview",
                  htmlOutput("report_preview"))
            )
          )
        )
      )
    )
  )
)


# =============================================================================
# Server
# =============================================================================

server <- function(input, output, session) {

  # --- Shared reactive values ---
  rv <- reactiveValues(
    raw_data     = NULL,   # data.frame loaded in Data tab (shared)
    ml_result    = NULL,   # easyml object from Pipeline ML
    ml_verbose   = NULL,   # verbose text
    ml_json_data = NULL,   # JSON structure from result
    ss_result    = NULL,   # ml_sample_size result
    report_md    = NULL,   # markdown report text
    report_html  = NULL,   # HTML rendered report
    report_json  = NULL    # JSON for report generation
  )


  # ===================================================================
  # TAB 0: Shared Data Upload - Server Logic
  # ===================================================================

  # ---- Load data (shared across all tabs) ----
  observeEvent(input$data_file, {
    req(input$data_file)
    ext <- tools::file_ext(input$data_file$name)
    tryCatch({
      if (ext == "csv") {
        rv$raw_data <- read.csv(input$data_file$datapath, stringsAsFactors = FALSE)
      } else if (ext %in% c("xlsx", "xls")) {
        if (!requireNamespace("readxl", quietly = TRUE)) {
          showNotification("Package 'readxl' required for Excel files.",
                           type = "error")
          return()
        }
        rv$raw_data <- readxl::read_excel(input$data_file$datapath)
        rv$raw_data <- as.data.frame(rv$raw_data)
      } else {
        showNotification("Unsupported file format. Use CSV or Excel.",
                         type = "error")
        return()
      }
      showNotification(
        paste0("Data loaded: ", nrow(rv$raw_data), " rows x ",
               ncol(rv$raw_data), " columns"),
        type = "message", duration = 4)
    }, error = function(e) {
      showNotification(paste("Error loading file:", e$message), type = "error")
    })
  })

  # ---- Data tab UI outputs ----
  output$data_info_ui <- renderUI({
    if (!is.null(rv$raw_data)) {
      div(style = "color: #1b2a4a; font-size: 13px; font-weight: 600;",
        icon("check-circle"),
        paste0(" ", nrow(rv$raw_data), " rows x ", ncol(rv$raw_data), " columns")
      )
    } else {
      div(style = "color: #9ca3af; font-size: 12px;",
        "Upload a CSV or Excel file to begin."
      )
    }
  })

  output$data_target_ui <- renderUI({
    req(rv$raw_data)
    selectInput("data_target", "Target variable:",
                choices = names(rv$raw_data),
                selected = NULL)
  })

  output$data_predictors_ui <- renderUI({
    req(rv$raw_data, input$data_target)
    all_vars <- setdiff(names(rv$raw_data), input$data_target)
    checkboxGroupInput("data_predictors", "Predictors (uncheck to exclude):",
                       choices = all_vars,
                       selected = all_vars)
  })

  output$data_preview_table <- DT::renderDataTable({
    req(rv$raw_data)
    DT::datatable(utils::head(rv$raw_data, 100),
                  options = list(scrollX = TRUE, pageLength = 10),
                  rownames = FALSE)
  })

  output$data_summary <- renderPrint({
    req(rv$raw_data)
    str(rv$raw_data)
  })


  # ===================================================================
  # TAB 1: Pipeline ML - Server Logic
  # ===================================================================

  # ---- Dynamic metric selector based on task type ----
  output$ml_metric_ui <- renderUI({
    task <- input$ml_task
    if (task == "regression") {
      choices <- c("Auto (RMSE)" = "auto", "RMSE" = "rmse",
                   "R-squared" = "rsq", "MAE" = "mae")
    } else {
      choices <- c("Auto (ROC-AUC)" = "auto", "ROC-AUC" = "roc_auc",
                   "F1-Score" = "f_meas", "F2-Score" = "f2_meas",
                   "Accuracy" = "accuracy", "Sensitivity" = "sensitivity",
                   "Specificity" = "specificity",
                   "Balanced Accuracy" = "bal_accuracy",
                   "PR-AUC" = "pr_auc", "MCC" = "mcc")
    }
    selectInput("ml_select_metric", "Selection metric:",
                choices = choices, selected = "auto")
  })

  # ---- Run easy_ml ----
  observeEvent(input$ml_run_btn, {

    # Validations
    if (is.null(rv$raw_data)) {
      showNotification("Please upload a data file first.", type = "warning")
      return()
    }
    if (is.null(input$data_target) || !nzchar(input$data_target)) {
      showNotification("Please select a target variable.", type = "warning")
      return()
    }
    if (is.null(input$data_predictors) || length(input$data_predictors) == 0) {
      showNotification("Please select at least one predictor.", type = "warning")
      return()
    }
    if (is.null(input$ml_models) || length(input$ml_models) == 0) {
      showNotification("Please select at least one algorithm.", type = "warning")
      return()
    }

    # Subset data to selected predictors + target
    sel_cols <- c(input$data_predictors, input$data_target)
    data_subset <- rv$raw_data[, sel_cols, drop = FALSE]

    # --- Pre-clean: remove high-cardinality & ID-like columns ---
    target_col <- input$data_target
    predictor_cols <- setdiff(names(data_subset), target_col)
    dropped_cols <- c()

    for (col in predictor_cols) {
      vals <- data_subset[[col]]
      n_unique <- length(unique(na.omit(vals)))
      n_rows <- nrow(data_subset)

      # Drop columns with too many unique values (likely IDs or free text)
      if (is.character(vals) || is.factor(vals)) {
        if (n_unique > 50 || n_unique > 0.5 * n_rows) {
          dropped_cols <- c(dropped_cols, col)
        }
      }
      # Drop near-constant columns (only 1 unique value)
      if (n_unique <= 1) {
        dropped_cols <- c(dropped_cols, col)
      }
    }

    if (length(dropped_cols) > 0) {
      data_subset <- data_subset[, !names(data_subset) %in% dropped_cols, drop = FALSE]
      showNotification(
        paste0("Auto-removed high-cardinality/constant columns: ",
               paste(dropped_cols, collapse = ", ")),
        type = "warning", duration = 8)
    }

    # Ensure target is factor for classification
    if (input$ml_task == "classification" ||
        (input$ml_task == "auto" && length(unique(na.omit(data_subset[[target_col]]))) <= 10)) {
      data_subset[[target_col]] <- as.factor(data_subset[[target_col]])
    }

    # Build parameters
    select_metric_val <- if (!is.null(input$ml_select_metric) &&
                             input$ml_select_metric != "auto") {
      input$ml_select_metric
    } else NULL

    withProgress(message = "Running PsyML pipeline...", value = 0, {

      incProgress(0.05, detail = "Preparing data...")

      tryCatch({
        result <- PsyML::easy_ml(
            data                = data_subset,
            target              = input$data_target,
            task                = input$ml_task,
            models              = input$ml_models,
            test_split          = input$ml_test_split,
            cv_folds            = as.integer(input$ml_cv_folds),
            select_metric       = select_metric_val,
            tune_best           = input$ml_tune,
            tune_method         = if (input$ml_tune) input$ml_tune_method else "random",
            tune_grid           = if (input$ml_tune) as.integer(input$ml_tune_grid) else 20L,
            tune_iter           = if (input$ml_tune && !is.null(input$ml_tune_iter))
                                    as.integer(input$ml_tune_iter) else 30L,
            feature_engineering = input$ml_feature_eng,
            feature_selection   = input$ml_feature_selection,
            balance_classes     = input$ml_balance_classes,
            balance_method      = if (input$ml_balance_classes)
                                    input$ml_balance_method else "smote",
            impute              = input$ml_impute,
            impute_method       = if (input$ml_impute)
                                    input$ml_impute_method else "knn",
            normalize           = input$ml_normalize,
            normalize_method    = if (input$ml_normalize)
                                    input$ml_normalize_method else "zscore",
            use_pca             = input$ml_use_pca,
            pca_threshold       = if (input$ml_use_pca && !is.null(input$ml_pca_threshold))
                                    input$ml_pca_threshold else 0.95,
            treat_outliers      = input$ml_treat_outliers,
            outlier_percentile  = if (input$ml_treat_outliers && !is.null(input$ml_outlier_percentile))
                                    input$ml_outlier_percentile else 0.05,
            remove_high_cor     = input$ml_remove_high_cor,
            cor_threshold       = if (input$ml_remove_high_cor && !is.null(input$ml_cor_threshold))
                                    input$ml_cor_threshold else 0.90,
            remove_high_vif     = input$ml_remove_high_vif,
            vif_threshold       = if (input$ml_remove_high_vif && !is.null(input$ml_vif_threshold))
                                    input$ml_vif_threshold else 5,
            run_eda             = input$ml_run_eda,
            run_shap            = input$ml_run_shap,
            n_shap              = if (input$ml_run_shap && !is.null(input$ml_n_shap))
                                    as.integer(input$ml_n_shap) else 100L,
            optimize_threshold  = input$ml_optimize_threshold,
            threshold_method    = if (input$ml_optimize_threshold && !is.null(input$ml_threshold_method))
                                    input$ml_threshold_method else "youden",
            calibrate_probs     = input$ml_calibrate_probs,
            calibration_method  = if (input$ml_calibrate_probs && !is.null(input$ml_calibration_method))
                                    input$ml_calibration_method else "platt",
            check_leakage       = input$ml_check_leakage,
            nested_cv           = input$ml_nested_cv,
            analyze_interactions = input$ml_analyze_interactions,
            seed                = as.integer(input$ml_seed),
            verbose             = input$ml_verbose
          )

        incProgress(0.8, detail = "Processing results...")

        rv$ml_result <- result
        rv$ml_verbose <- if (!is.null(result$verbose_text)) {
          result$verbose_text
        } else {
          "Verbose output not captured."
        }

        # Build JSON structure for report use via export_verbose_json
        tryCatch({
          tmp_json <- tempfile(fileext = ".json")
          PsyML::export_verbose_json(result, tmp_json)
          rv$ml_json_data <- jsonlite::fromJSON(tmp_json)
          unlink(tmp_json)
        }, error = function(e) {
          # Fallback: build minimal JSON
          rv$ml_json_data <- list(
            metadata = result$metadata,
            metrics = if (!is.null(result$test_metrics)) {
              as.list(result$test_metrics)
            } else NULL,
            model_info = list(
              name = if (!is.null(result$best_model)) result$best_model else "unknown",
              label = if (!is.null(result$best_model)) result$best_model else "unknown",
              tuned = !is.null(result$best_params),
              hyperparameters = result$best_params
            ),
            verbose_text = rv$ml_verbose
          )
        })

        incProgress(0.95, detail = "Done!")

        showNotification("Pipeline completed successfully!",
                         type = "message", duration = 5)

      }, error = function(e) {
        # Show full error detail for debugging
        err_msg <- e$message
        if (grepl("All models failed", err_msg)) {
          err_msg <- paste0(err_msg,
            "\n\nCommon causes:",
            "\n- Multiclass target with binary-only metrics (use 2-class target)",
            "\n- Too few observations for the number of CV folds",
            "\n- All predictors removed during preprocessing",
            "\n\nTry: fewer models, fewer CV folds, or a simpler dataset.")
        }
        showNotification(err_msg, type = "error", duration = 15)
      })
    })
  })

  # ---- ML Status Box ----
  output$ml_status_box <- renderUI({
    if (is.null(rv$ml_result)) {
      box(
        width = NULL, status = "info",
        div(style = "text-align: center; padding: 20px; color: #9ca3af;",
          icon("upload", style = "font-size: 36px;"),
          tags$br(), tags$br(),
          tags$p("Upload a dataset, configure parameters, and click"),
          tags$p(tags$strong("'Run Pipeline'"), "to start the analysis.")
        )
      )
    } else {
      result <- rv$ml_result
      task_label <- if (!is.null(result$task)) result$task else "N/A"
      model_label <- if (!is.null(result$best_model)) result$best_model else "N/A"
      elapsed <- if (!is.null(result$metadata$elapsed_time)) {
        paste0(round(result$metadata$elapsed_time, 1), "s")
      } else "N/A"

      box(
        width = NULL, status = "success",
        div(style = "padding: 5px;",
          fluidRow(
            column(3, tags$p(tags$strong("Task: "), task_label)),
            column(3, tags$p(tags$strong("Best model: "), model_label)),
            column(3, tags$p(tags$strong("Target: "), result$target)),
            column(3, tags$p(tags$strong("Time: "), elapsed))
          )
        )
      )
    }
  })

  # ---- Helper: convert test_metrics tibble to named list ----
  ml_metrics_list <- reactive({
    req(rv$ml_result)
    metrics_raw <- rv$ml_result$test_metrics
    if (is.null(metrics_raw)) return(NULL)

    # test_metrics is a tidymodels tibble with .metric, .estimator, .estimate
    if (is.data.frame(metrics_raw) && ".metric" %in% names(metrics_raw)) {
      vals <- stats::setNames(metrics_raw$.estimate, metrics_raw$.metric)
      as.list(vals)
    } else if (is.list(metrics_raw) && !is.data.frame(metrics_raw)) {
      metrics_raw
    } else {
      NULL
    }
  })

  # ---- ML Metrics Cards ----
  output$ml_metrics_cards <- renderUI({
    metrics <- ml_metrics_list()
    if (is.null(metrics) || length(metrics) == 0) {
      return(tags$p("No metrics available."))
    }

    # Show up to 6 key metrics as cards
    metric_names <- names(metrics)
    cards <- lapply(metric_names[1:min(6, length(metric_names))], function(m) {
      val <- metrics[[m]]
      if (is.null(val) || !is.numeric(val) || is.na(val)) return(NULL)
      display_val <- if (m %in% c("accuracy", "sensitivity", "specificity",
                                   "bal_accuracy")) {
        paste0(round(val * 100, 1), "%")
      } else {
        round(val, 4)
      }
      div(class = "metric-card",
        div(class = "metric-value", display_val),
        div(class = "metric-label", toupper(gsub("_", " ", m)))
      )
    })
    div(style = "text-align: center;", cards)
  })

  # ---- ML Metrics Table ----
  output$ml_metrics_table <- DT::renderDataTable({
    metrics <- ml_metrics_list()
    if (is.null(metrics) || length(metrics) == 0) return(NULL)

    df <- data.frame(
      Metric = names(metrics),
      Value = round(as.numeric(unlist(metrics)), 4),
      stringsAsFactors = FALSE
    )
    rownames(df) <- NULL
    DT::datatable(df, options = list(dom = 't', pageLength = 20),
                  rownames = FALSE)
  })

  # ---- ML Verbose Output ----
  output$ml_verbose <- renderText({
    req(rv$ml_verbose)
    rv$ml_verbose
  })

  # ---- Download Verbose TXT ----
  output$ml_download_txt <- downloadHandler(
    filename = function() {
      paste0("PsyML_Verbose_", format(Sys.Date(), "%Y%m%d"), ".txt")
    },
    content = function(file) {
      req(rv$ml_verbose)
      writeLines(rv$ml_verbose, file)
    }
  )

  # ---- ML Plots ----
  # Plots are in result$figures (shortcut to result$plots$plots)
  # Each is a ggplot object keyed by name: importance, model_comparison,
  # roc_curve, confusion_matrix, calibration, threshold_optimization,
  # pred_vs_obs, residuals, tuning, shap_summary, etc.

  output$ml_plots_ui <- renderUI({
    req(rv$ml_result)
    result <- rv$ml_result

    # result$figures is the shortcut to the actual ggplot list
    figs <- result$figures
    if (is.null(figs) || length(figs) == 0) {
      return(tags$p(style = "color: #9ca3af; text-align: center;",
                    "No plots available."))
    }

    # Build friendly labels from plot names
    plot_labels <- sapply(names(figs), function(nm) {
      gsub("_", " ", tools::toTitleCase(gsub("_", " ", nm)))
    })
    choices <- stats::setNames(names(figs), plot_labels)

    tagList(
      selectInput("ml_plot_select", "Select plot:",
                  choices = choices,
                  selected = names(figs)[1]),
      plotOutput("ml_plot_display", height = "500px")
    )
  })

  output$ml_plot_display <- renderPlot({
    req(rv$ml_result, input$ml_plot_select)
    figs <- rv$ml_result$figures
    req(figs)

    p <- figs[[input$ml_plot_select]]
    if (!is.null(p)) {
      if (inherits(p, "gg") || inherits(p, "ggplot")) {
        print(p)
      } else if (inherits(p, "recordedplot")) {
        replayPlot(p)
      }
    }
  })

  # ---- AI Interpretation ----
  observeEvent(input$ml_ai_btn, {

    if (is.null(rv$ml_result)) {
      showNotification("Run the pipeline first before asking AI.",
                       type = "warning")
      return()
    }

    withProgress(message = "Consulting AI...", value = 0.3, {

      ai_result <- tryCatch({
        # Capture cat() output since explain_with_ai prints to console
        captured <- capture.output({
          res <- PsyML::explain_with_ai(
            result   = rv$ml_result,
            language = input$ml_ai_language
          )
        }, type = "output")
        res
      }, error = function(e) {
        list(success = FALSE, text = paste("Error:", e$message))
      })

      incProgress(0.9, detail = "Done!")

      output$ml_ai_response <- renderUI({
        if (isTRUE(ai_result$success)) {
          div(class = "ai-response", ai_result$text)
        } else {
          div(style = "color: #c0392b; padding: 10px;",
            icon("exclamation-triangle"),
            ai_result$text
          )
        }
      })
    })
  })


  # ===================================================================
  # TAB 2: Sample Size Estimation - Server Logic
  # ===================================================================

  # ---- Dynamic metric selector ----
  output$ss_metric_ui <- renderUI({
    if (input$ss_task == "classification") {
      selectInput("ss_metric", "Metric:",
                  choices = c("AUC" = "auc",
                              "Accuracy" = "accuracy",
                              "F1" = "f1"),
                  selected = "auc")
    } else {
      selectInput("ss_metric", "Metric:",
                  choices = c("RMSE" = "rmse",
                              "MAE" = "mae",
                              "R-squared" = "r2"),
                  selected = "rmse")
    }
  })

  # ---- Run ml_sample_size ----
  observeEvent(input$ss_run_btn, {

    # Parse n_grid
    n_grid <- tryCatch({
      vals <- as.integer(trimws(unlist(strsplit(input$ss_n_grid, ","))))
      vals[!is.na(vals) & vals > 0]
    }, error = function(e) {
      showNotification("Invalid sample size grid format.", type = "error")
      return(NULL)
    })

    if (is.null(n_grid) || length(n_grid) < 2) {
      showNotification("Provide at least 2 sample sizes in the grid.",
                       type = "warning")
      return()
    }

    withProgress(message = "Running Monte Carlo simulation...", value = 0, {

      incProgress(0.05, detail = "Setting up...")

      tryCatch({
        # Determine data source
        use_data <- NULL
        use_formula <- NULL

        if (input$ss_data_source == "from_data" && !is.null(rv$raw_data)) {
          use_data <- rv$raw_data
          target_var <- if (!is.null(input$data_target)) {
            input$data_target
          } else {
            names(rv$raw_data)[ncol(rv$raw_data)]
          }

          # Pre-clean: remove ID-like, high-cardinality, and constant columns
          pred_cols <- setdiff(names(use_data), target_var)
          drop_cols <- c()
          for (col in pred_cols) {
            vals <- use_data[[col]]
            n_uniq <- length(unique(na.omit(vals)))
            n_rows <- nrow(use_data)
            # Drop ID-like or high-cardinality text columns
            if (is.character(vals) || is.factor(vals)) {
              if (n_uniq > 50 || n_uniq > 0.5 * n_rows) {
                drop_cols <- c(drop_cols, col)
              }
            }
            # Drop constant columns
            if (n_uniq <= 1) {
              drop_cols <- c(drop_cols, col)
            }
          }
          if (length(drop_cols) > 0) {
            use_data <- use_data[, !names(use_data) %in% drop_cols, drop = FALSE]
            showNotification(
              paste0("Auto-removed columns: ", paste(drop_cols, collapse = ", ")),
              type = "warning", duration = 8)
          }

          # Ensure target is factor for classification
          if (input$ss_task == "classification") {
            use_data[[target_var]] <- as.factor(use_data[[target_var]])
          }

          use_formula <- as.formula(paste(target_var, "~ ."))
        }

        # Build sim_params for simulation mode
        sim_params <- list(
          p = input$ss_sim_p,
          signal = input$ss_sim_signal,
          prevalence = input$ss_sim_prevalence
        )

        incProgress(0.1, detail = "Running simulations (this may take a while)...")

        result <- PsyML::ml_sample_size(
          data         = use_data,
          formula      = use_formula,
          task         = input$ss_task,
          model        = input$ss_model,
          metric       = input$ss_metric,
          n_grid       = n_grid,
          reps         = input$ss_reps,
          target       = input$ss_target,
          n_test       = input$ss_n_test,
          n_outer_splits = input$ss_n_outer,
          bootstrap_ci = input$ss_bootstrap,
          sim_params   = sim_params,
          seed         = input$ss_seed,
          verbose      = FALSE
        )

        incProgress(0.9, detail = "Processing results...")

        rv$ss_result <- result

        showNotification("Sample size estimation complete!",
                         type = "message", duration = 5)

      }, error = function(e) {
        showNotification(paste("Error:", e$message),
                         type = "error", duration = 10)
      })
    })
  })

  # ---- SS Status Box ----
  output$ss_status_box <- renderUI({
    if (is.null(rv$ss_result)) {
      box(
        width = NULL, status = "info",
        div(style = "text-align: center; padding: 20px; color: #9ca3af;",
          icon("chart-area", style = "font-size: 36px;"),
          tags$br(), tags$br(),
          tags$p("Configure simulation parameters and click"),
          tags$p(tags$strong("'Estimate Sample Size'"), "to begin.")
        )
      )
    } else {
      rec <- rv$ss_result$recommend_n
      rec_label <- if (is.list(rec)) {
        paste(names(rec), "=", unlist(rec), collapse = ", ")
      } else {
        as.character(rec)
      }

      box(
        width = NULL, status = "success",
        div(class = "ss-recommendation",
          tags$h4(icon("check-circle"),
                  paste0(" Recommended n* = ", rec_label))
        )
      )
    }
  })

  # ---- SS Learning Curve Plot ----
  output$ss_plot <- renderPlot({
    req(rv$ss_result)
    tryCatch({
      PsyML::plot_learning_curve(rv$ss_result)
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Plot error:", e$message), cex = 1.2, col = "red")
    })
  })

  # ---- SS Summary Table ----
  output$ss_summary_table <- DT::renderDataTable({
    req(rv$ss_result)
    smry <- rv$ss_result$summary
    if (is.null(smry)) return(NULL)

    # Round numeric columns
    num_cols <- sapply(smry, is.numeric)
    smry[num_cols] <- lapply(smry[num_cols], function(x) round(x, 4))

    DT::datatable(smry, options = list(pageLength = 20, scrollX = TRUE),
                  rownames = FALSE)
  })

  # ---- SS Recommendation ----
  output$ss_recommendation <- renderUI({
    req(rv$ss_result)
    result <- rv$ss_result

    rec <- result$recommend_n
    settings <- result$settings

    # Build recommendation text
    parts <- list()

    if (!is.null(rec)) {
      rec_text <- if (is.list(rec)) {
        paste(sapply(names(rec), function(m) {
          paste0(m, ": n* = ", rec[[m]])
        }), collapse = "; ")
      } else {
        paste0("n* = ", rec)
      }
      parts <- c(parts, list(tags$p(tags$strong("Recommended sample size: "), rec_text)))
    }

    if (!is.null(result$recommend_n_ci)) {
      ci <- result$recommend_n_ci
      parts <- c(parts, list(
        tags$p(tags$strong("Bootstrap 95% CI: "),
               paste0("[", ci$ci_lower, ", ", ci$ci_upper, "]"))
      ))
    }

    if (!is.null(result$curve_fit) && !is.null(result$curve_diagnostics)) {
      diag <- result$curve_diagnostics
      parts <- c(parts, list(
        tags$p(tags$strong("Curve fit: "),
               paste0("Pseudo-R2 = ", round(diag$pseudo_r2, 3),
                      ", Quality: ", diag$fit_quality))
      ))
    }

    parts <- c(parts, list(
      hr(),
      tags$p(class = "info-text",
        paste0("Task: ", settings$task,
               " | Metric: ", settings$metric,
               " | Target: ", settings$target,
               " | Reps: ", settings$reps))
    ))

    tagList(parts)
  })


  # ===================================================================
  # TAB 3: AI Report Generator - Server Logic
  # ===================================================================

  # ---- Report data source reactive ----
  report_data <- reactive({
    src <- input$report_source

    if (src == "pipeline") {
      # Use data from Tab 1
      if (!is.null(rv$ml_json_data)) {
        return(rv$ml_json_data)
      }
      return(NULL)

    } else if (src == "json_upload") {
      req(input$report_json_file)
      tryCatch({
        jsonlite::fromJSON(input$report_json_file$datapath)
      }, error = function(e) {
        showNotification(paste("Error reading JSON:", e$message), type = "error")
        NULL
      })

    } else if (src == "txt_upload") {
      req(input$report_txt_file)
      tryCatch({
        txt <- paste(readLines(input$report_txt_file$datapath, warn = FALSE),
                     collapse = "\n")
        # Return minimal structure with verbose_text
        list(
          metadata = list(task = "unknown", target = "unknown"),
          verbose_text = txt
        )
      }, error = function(e) {
        showNotification(paste("Error reading TXT:", e$message), type = "error")
        NULL
      })
    }
  })

  # ---- Source status indicator ----
  output$report_source_status <- renderUI({
    data <- report_data()
    if (is.null(data)) {
      div(style = "color: #c0392b; font-size: 12px; margin-top: 5px;",
        icon("times-circle"), " No data available"
      )
    } else {
      task <- if (!is.null(data$metadata$task)) data$metadata$task else "N/A"
      div(style = "color: #1b2a4a; font-size: 12px; margin-top: 5px;",
        icon("check-circle"), paste(" Data ready | Task:", task)
      )
    }
  })

  # ---- Report JSON Info ----
  output$report_json_info <- renderUI({
    data <- report_data()
    if (is.null(data)) {
      return(tags$p(style = "color: #9ca3af;", "No data loaded."))
    }

    meta <- data$metadata
    model <- data$model_info

    info_items <- list()

    if (!is.null(meta$task))
      info_items <- c(info_items, list(tags$p(tags$strong("Task: "), meta$task)))
    if (!is.null(meta$target))
      info_items <- c(info_items, list(tags$p(tags$strong("Target: "), meta$target)))
    if (!is.null(meta$n_observations))
      info_items <- c(info_items, list(
        tags$p(tags$strong("N: "), format(meta$n_observations, big.mark = ","))))
    if (!is.null(meta$n_train))
      info_items <- c(info_items, list(
        tags$p(tags$strong("Train/Test: "),
               paste0(meta$n_train, " / ", meta$n_test))))
    if (!is.null(model$label))
      info_items <- c(info_items, list(
        tags$p(tags$strong("Model: "), model$label)))

    tagList(info_items)
  })

  # ---- Report Metrics Cards ----
  output$report_metrics_cards <- renderUI({
    data <- report_data()
    if (is.null(data) || is.null(data$metrics)) return(NULL)

    metrics <- data$metrics
    cards <- lapply(names(metrics)[1:min(4, length(metrics))], function(m) {
      val <- metrics[[m]]
      if (is.null(val) || !is.numeric(val)) return(NULL)
      display_val <- if (m %in% c("accuracy", "sensitivity", "specificity")) {
        paste0(round(val * 100, 1), "%")
      } else {
        round(val, 4)
      }
      div(class = "metric-card", style = "width: 42%;",
        div(class = "metric-value", display_val),
        div(class = "metric-label", toupper(m))
      )
    })
    div(style = "text-align: center;", cards)
  })

  # ---- Report Variable Importance Table ----
  output$report_var_table <- renderTable({
    data <- report_data()
    if (is.null(data) || is.null(data$variable_importance)) return(NULL)

    var_df <- data$variable_importance
    if (is.list(var_df) && !is.data.frame(var_df)) {
      var_df <- do.call(rbind, lapply(var_df, as.data.frame))
    }
    var_df$importance <- round(var_df$importance, 4)
    names(var_df) <- c("Variable", "Importance", "Rank")
    var_df <- var_df[, c("Rank", "Variable", "Importance")]
    head(var_df, 10)
  }, striped = TRUE, hover = TRUE, width = "100%")

  # ---- Generate Report ----
  observeEvent(input$report_gen_btn, {

    data <- report_data()
    if (is.null(data)) {
      showNotification("No data available. Load data first.", type = "warning")
      return()
    }

    if (!nzchar(input$report_api_key)) {
      showNotification("Please enter your OpenAI API Key.", type = "error")
      return()
    }

    withProgress(message = "Generating AI report...", value = 0, {

      incProgress(0.1, detail = "Building prompt...")

      # ---- Build system prompt ----
      system_prompt <- if (input$report_language == "es") {
        paste(
          "Eres un experto en redaccion cientifica especializado en Machine Learning y analisis predictivo.",
          "Tu tarea es generar un reporte academico COMPLETO Y DETALLADO en ESTILO DE ARTICULO CIENTIFICO.",
          "",
          "IMPORTANTE: Se te proporcionara el OUTPUT COMPLETO (verbose) del analisis de Machine Learning.",
          "Debes usar TODA esa informacion para escribir un reporte exhaustivo.",
          "",
          "SEPARACION ESTRICTA METODO vs RESULTADOS:",
          "- METODO = procedimiento, pasos, configuracion (SIN valores de metricas)",
          "- RESULTADOS = hallazgos numericos, metricas, rendimiento",
          "",
          "ESTILO DE ESCRITURA OBLIGATORIO:",
          "1. PARRAFOS EXTENSOS: Cada parrafo debe tener minimo 150-200 palabras",
          "2. FLUJO NARRATIVO: Las ideas deben conectarse con transiciones suaves",
          "3. NO LISTAS: Nunca uses bullets (-), asteriscos (*) ni numeracion para listas",
          "4. MUCHAS CITAS: Integra citas mencionadas en el verbose + adicionales",
          "5. ESTADISTICOS EN LINEA: Reporta valores dentro de oraciones narrativas",
          "6. VOZ IMPERSONAL: 'se observo', 'se estimo', 'se empleo'",
          "",
          "ESTRUCTURA OBLIGATORIA:",
          "## Metodo",
          "### Participantes",
          "### Preprocesamiento de Datos",
          "### Procedimiento de Modelado",
          "",
          "## Resultados",
          "### Comparacion de Modelos",
          "### Rendimiento del Modelo Final",
          "[Insertar Tabla 1 aqui]",
          "### Importancia de Variables",
          "[Insertar Tabla 2 aqui]",
          "",
          "## Discusion",
          "## Referencias (APA 7, minimo 15 referencias)",
          "",
          "MINIMO 2500 palabras de texto narrativo fluido y detallado.",
          sep = "\n"
        )
      } else {
        paste(
          "You are an expert scientific writer specialized in Machine Learning and predictive analytics.",
          "Your task is to generate a COMPLETE AND DETAILED academic report in SCIENTIFIC ARTICLE STYLE.",
          "",
          "IMPORTANT: You will receive the COMPLETE OUTPUT (verbose) from the ML analysis.",
          "You must use ALL that information to write an exhaustive report.",
          "",
          "STRICT SEPARATION METHOD vs RESULTS:",
          "- METHOD = procedure, steps, configuration (NO metric values)",
          "- RESULTS = numerical findings, metrics, performance",
          "",
          "MANDATORY WRITING STYLE:",
          "1. EXTENSIVE PARAGRAPHS: Each paragraph minimum 150-200 words",
          "2. NARRATIVE FLOW: Ideas connect with smooth transitions",
          "3. NO LISTS: Never use bullets (-), asterisks (*) or numbering",
          "4. MANY CITATIONS: Integrate references from verbose + additional ones",
          "5. INLINE STATISTICS: Report values within narrative sentences",
          "6. IMPERSONAL VOICE: 'was observed', 'was estimated', 'results indicate'",
          "",
          "MANDATORY STRUCTURE:",
          "## Method",
          "### Participants",
          "### Data Preprocessing",
          "### Modeling Procedure",
          "",
          "## Results",
          "### Model Comparison",
          "### Final Model Performance",
          "[Insert Table 1 here]",
          "### Variable Importance",
          "[Insert Table 2 here]",
          "",
          "## Discussion",
          "## References (APA 7, minimum 15 references)",
          "",
          "MINIMUM 2500 words of fluid, detailed narrative text.",
          sep = "\n"
        )
      }

      incProgress(0.2, detail = "Building message...")

      # ---- Build user message ----
      meta <- data$metadata
      metrics <- data$metrics
      model_info <- data$model_info
      var_imp <- data$variable_importance

      # Format metrics
      metrics_text <- if (!is.null(metrics)) {
        paste(sapply(names(metrics), function(m) {
          val <- metrics[[m]]
          if (is.numeric(val)) {
            if (m %in% c("accuracy", "sensitivity", "specificity")) {
              sprintf("%s: %.2f%%", toupper(m), val * 100)
            } else {
              sprintf("%s: %.4f", toupper(m), val)
            }
          } else {
            paste0(toupper(m), ": ", val)
          }
        }), collapse = "\n")
      } else "Not available"

      # Format variable importance
      var_text <- if (!is.null(var_imp)) {
        var_df <- if (is.data.frame(var_imp)) var_imp else {
          tryCatch(do.call(rbind, lapply(var_imp, as.data.frame)),
                   error = function(e) NULL)
        }
        if (!is.null(var_df) && nrow(var_df) > 0) {
          paste(sapply(1:min(10, nrow(var_df)), function(i) {
            sprintf("%d. %s (%.4f)", var_df$rank[i], var_df$variable[i],
                    var_df$importance[i])
          }), collapse = "\n")
        } else "Not available"
      } else "Not available"

      # Verbose section
      verbose_section <- ""
      verbose_txt <- data$verbose_text
      if (is.null(verbose_txt) && !is.null(rv$ml_verbose)) {
        verbose_txt <- rv$ml_verbose
      }
      if (!is.null(verbose_txt) && nzchar(verbose_txt)) {
        verbose_section <- paste(
          "",
          "## COMPLETE ANALYSIS OUTPUT (VERBOSE)",
          "=========================================",
          "```",
          verbose_txt,
          "```",
          sep = "\n"
        )
      }

      # Additional context from user
      context_section <- ""
      if (!is.null(input$report_context) && nzchar(input$report_context)) {
        context_section <- paste(
          "",
          "## ADDITIONAL CONTEXT FROM THE RESEARCHER",
          input$report_context,
          sep = "\n"
        )
      }

      user_message <- paste(
        "# COMPLETE DATA FOR ACADEMIC MACHINE LEARNING REPORT",
        "",
        "## STUDY SUMMARY",
        sprintf("Task type: %s", if (!is.null(meta$task)) meta$task else "N/A"),
        sprintf("Target variable: %s", if (!is.null(meta$target)) meta$target else "N/A"),
        if (!is.null(meta$n_observations)) {
          sprintf("Total observations: N = %d", meta$n_observations)
        } else "",
        if (!is.null(meta$n_train)) {
          sprintf("Training set: n = %d", meta$n_train)
        } else "",
        if (!is.null(meta$n_test)) {
          sprintf("Test set: n = %d", meta$n_test)
        } else "",
        "",
        "## SELECTED MODEL",
        if (!is.null(model_info$label)) {
          sprintf("Algorithm: %s", model_info$label)
        } else "",
        if (!is.null(model_info$tuned)) {
          sprintf("Tuned: %s", ifelse(model_info$tuned, "Yes", "No"))
        } else "",
        "",
        "## PERFORMANCE METRICS (test set)",
        metrics_text,
        "",
        "## VARIABLE IMPORTANCE (Top 10)",
        var_text,
        verbose_section,
        context_section,
        "",
        sprintf("REPORT LANGUAGE: %s",
                ifelse(input$report_language == "es", "SPANISH", "ENGLISH")),
        "",
        "GENERATE THE REPORT NOW.",
        sep = "\n"
      )

      incProgress(0.3, detail = "Contacting OpenAI...")

      # ---- API Call ----
      clean_key <- trimws(input$report_api_key)

      req_obj <- request("https://api.openai.com/v1/chat/completions") |>
        req_headers(
          Authorization = paste("Bearer", clean_key),
          "Content-Type" = "application/json"
        ) |>
        req_body_json(list(
          model = input$report_model,
          temperature = 0.4,
          max_tokens = 16000,
          messages = list(
            list(role = "system", content = system_prompt),
            list(role = "user", content = user_message)
          )
        )) |>
        req_timeout(300)

      resp <- tryCatch({
        req_perform(req_obj)
      }, error = function(e) {
        list(error = TRUE, message = e$message)
      })

      if (is.list(resp) && !is.null(resp$error) && isTRUE(resp$error)) {
        showNotification(paste("OpenAI error:", resp$message),
                         type = "error", duration = 10)
        return()
      }

      if (inherits(resp, "httr2_response")) {
        status <- resp_status(resp)
        if (status == 401) {
          showNotification(
            "Error 401: Invalid API Key. Check your key and credits.",
            type = "error", duration = 10)
          return()
        } else if (status != 200) {
          body_text <- tryCatch(resp_body_string(resp), error = function(e) "")
          showNotification(paste("HTTP Error", status, ":", body_text),
                           type = "error", duration = 10)
          return()
        }
      }

      incProgress(0.8, detail = "Processing response...")

      js <- tryCatch(resp_body_json(resp, simplifyVector = FALSE),
                     error = function(e) NULL)

      if (is.null(js) || !is.null(js$error)) {
        msg <- if (!is.null(js$error$message)) js$error$message else "Invalid response"
        showNotification(paste("API Error:", msg), type = "error", duration = 10)
        return()
      }

      txt <- ""
      if (!is.null(js$choices) && length(js$choices) > 0) {
        txt <- js$choices[[1]]$message$content
      }

      if (!nzchar(txt)) {
        showNotification("Empty response from API.", type = "error")
        return()
      }

      incProgress(0.95, detail = "Rendering...")

      rv$report_md <- txt
      rv$report_json <- report_data()
      rv$report_html <- tryCatch(
        commonmark::markdown_html(txt),
        error = function(e) paste0("<pre>", htmltools::htmlEscape(txt), "</pre>")
      )

      showNotification("Report generated successfully!",
                       type = "message", duration = 5)
    })
  })

  # ---- Report Preview ----
  output$report_preview <- renderUI({
    if (is.null(rv$report_html)) {
      return(div(
        style = "text-align: center; padding: 50px; color: #9ca3af;",
        icon("file-alt", style = "font-size: 48px;"),
        tags$br(), tags$br(),
        tags$p("Select a data source, enter your API key, and click"),
        tags$p(tags$strong("'Generate Report'"))
      ))
    }
    HTML(rv$report_html)
  })

  # ---- Download DOCX ----
  output$report_download_docx <- downloadHandler(
    filename = function() {
      paste0("PsyML_Report_", format(Sys.Date(), "%Y%m%d"), ".docx")
    },
    content = function(file) {
      req(rv$report_md)

      tryCatch({
        json_data <- rv$report_json
        doc <- create_word_with_tables(
          report_text = rv$report_md,
          title = input$report_title,
          author = input$report_author,
          json_data = json_data
        )
        print(doc, target = file)
      }, error = function(e) {
        showNotification(paste("Error creating DOCX:", e$message),
                         type = "error")
      })
    }
  )

  # ---- Download MD ----
  output$report_download_md <- downloadHandler(
    filename = function() {
      paste0("PsyML_Report_", format(Sys.Date(), "%Y%m%d"), ".md")
    },
    content = function(file) {
      req(rv$report_md)
      header <- paste0(
        "# ", input$report_title, "\n",
        if (nzchar(input$report_author))
          paste0("**Author:** ", input$report_author, "\n") else "",
        "**Date:** ", format(Sys.Date(), "%Y-%m-%d"), "\n",
        "\n---\n\n"
      )
      writeLines(paste0(header, rv$report_md), file)
    }
  )
}


# =============================================================================
# Helper Functions for Word Document Creation
# =============================================================================

#' Create Word document with real tables
create_word_with_tables <- function(report_text, title, author, json_data) {

  doc <- read_docx()

  # Title
  doc <- body_add_par(doc, title, style = "heading 1")
  if (!is.null(author) && nzchar(author)) {
    doc <- body_add_par(doc, author, style = "Normal")
  }
  doc <- body_add_par(doc, format(Sys.Date(), "%Y-%m-%d"), style = "Normal")
  doc <- body_add_par(doc, "", style = "Normal")

  # Process Markdown line by line
  md_lines <- unlist(strsplit(report_text, "\n", fixed = TRUE))
  i <- 1

  while (i <= length(md_lines)) {
    ln <- md_lines[i]

    # Empty lines
    if (nchar(trimws(ln)) == 0) { i <- i + 1; next }

    # Separators
    if (grepl("^---+$", ln)) { i <- i + 1; next }

    # Headers level 1
    if (grepl("^#\\s+", ln) && !grepl("^##", ln)) {
      texto <- gsub("\\*\\*", "", gsub("^#\\s+", "", ln))
      doc <- body_add_par(doc, texto, style = "heading 1")
      i <- i + 1; next
    }

    # Headers level 2
    if (grepl("^##\\s+", ln) && !grepl("^###", ln)) {
      texto <- gsub("\\*\\*", "", gsub("^##\\s+", "", ln))
      doc <- body_add_par(doc, texto, style = "heading 2")
      i <- i + 1; next
    }

    # Headers level 3
    if (grepl("^###\\s+", ln)) {
      texto <- gsub("\\*\\*", "", gsub("^###\\s+", "", ln))
      doc <- body_add_par(doc, texto, style = "heading 3")
      i <- i + 1; next
    }

    # Table 1 placeholder - metrics
    if (grepl("\\[Insertar Tabla 1", ln, ignore.case = TRUE) ||
        grepl("\\[Insert Table 1", ln, ignore.case = TRUE)) {
      doc <- add_metrics_table(doc, json_data)
      i <- i + 1; next
    }

    # Table 2 placeholder - variable importance
    if (grepl("\\[Insertar Tabla 2", ln, ignore.case = TRUE) ||
        grepl("\\[Insert Table 2", ln, ignore.case = TRUE)) {
      doc <- add_importance_table(doc, json_data)
      i <- i + 1; next
    }

    # Other placeholders
    if (grepl("^\\[Insertar", ln, ignore.case = TRUE) ||
        grepl("^\\[Insert", ln, ignore.case = TRUE)) {
      doc <- body_add_par(doc, ln, style = "Normal")
      i <- i + 1; next
    }

    # Table/Figure captions
    if (grepl("^Tabla\\s+\\d+", ln) || grepl("^Table\\s+\\d+", ln) ||
        grepl("^Figura\\s+\\d+", ln) || grepl("^Figure\\s+\\d+", ln)) {
      texto <- gsub("\\*", "", gsub("\\*\\*", "", ln))
      doc <- body_add_par(doc, texto, style = "Normal")
      i <- i + 1; next
    }

    # Markdown tables
    if (grepl("^\\|", ln) && grepl("\\|$", ln)) {
      table_lines <- c()
      while (i <= length(md_lines) && grepl("^\\|", md_lines[i])) {
        table_lines <- c(table_lines, md_lines[i])
        i <- i + 1
      }
      doc <- add_markdown_table(doc, table_lines)
      next
    }

    # Normal paragraphs
    texto <- ln
    texto <- gsub("\\*\\*([^*]+)\\*\\*", "\\1", texto)
    texto <- gsub("\\*([^*]+)\\*", "\\1", texto)
    texto <- gsub("_([^_]+)_", "\\1", texto)
    doc <- body_add_par(doc, texto, style = "Normal")
    i <- i + 1
  }

  return(doc)
}


#' Add metrics table to Word document
add_metrics_table <- function(doc, json_data) {

  if (is.null(json_data) || is.null(json_data$metrics)) {
    doc <- body_add_par(doc, "[Metrics not available]", style = "Normal")
    return(doc)
  }

  metrics <- json_data$metrics
  metric_names <- names(metrics)
  metric_values <- unlist(metrics)

  formatted_names <- sapply(metric_names, function(m) {
    switch(m,
      "roc_auc" = "ROC-AUC", "accuracy" = "Accuracy",
      "sensitivity" = "Sensitivity", "specificity" = "Specificity",
      "f_meas" = "F1-Score", "pr_auc" = "PR-AUC",
      "bal_accuracy" = "Balanced Accuracy",
      "rmse" = "RMSE", "rsq" = "R-squared", "mae" = "MAE",
      toupper(m))
  })

  formatted_values <- sapply(seq_along(metric_values), function(i) {
    m <- metric_names[i]; val <- metric_values[i]
    if (m %in% c("accuracy", "sensitivity", "specificity", "bal_accuracy")) {
      sprintf("%.2f%%", val * 100)
    } else {
      sprintf("%.4f", val)
    }
  })

  interpretations <- sapply(seq_along(metric_values), function(i) {
    m <- metric_names[i]; val <- metric_values[i]
    if (m %in% c("roc_auc", "accuracy", "sensitivity", "specificity")) {
      if (val >= 0.9) "Excellent" else if (val >= 0.8) "Good"
      else if (val >= 0.7) "Acceptable" else "Low"
    } else ""
  })

  df <- data.frame(Metric = formatted_names, Value = formatted_values,
                   Interpretation = interpretations, stringsAsFactors = FALSE)
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


#' Add variable importance table to Word document
add_importance_table <- function(doc, json_data) {

  if (is.null(json_data) || is.null(json_data$variable_importance)) {
    doc <- body_add_par(doc, "[Variable importance not available]", style = "Normal")
    return(doc)
  }

  var_imp <- json_data$variable_importance
  if (is.list(var_imp) && !is.data.frame(var_imp)) {
    df <- do.call(rbind, lapply(var_imp, function(v) {
      data.frame(Rank = v$rank, Variable = v$variable,
                 Importance = v$importance, stringsAsFactors = FALSE)
    }))
  } else {
    df <- data.frame(Rank = var_imp$rank, Variable = var_imp$variable,
                     Importance = var_imp$importance, stringsAsFactors = FALSE)
  }

  df$Importance <- sprintf("%.4f", df$Importance)
  df <- head(df, 10)

  ft <- flextable(df)
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


#' Add markdown table to Word document
add_markdown_table <- function(doc, table_lines) {

  if (length(table_lines) < 2) return(doc)

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
      if (length(row) > 0) data_rows[[length(data_rows) + 1]] <- row
    }
  }

  if (length(data_rows) == 0) return(doc)

  df <- do.call(rbind, lapply(data_rows, function(r) {
    if (length(r) < length(header)) r <- c(r, rep("", length(header) - length(r)))
    else if (length(r) > length(header)) r <- r[1:length(header)]
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
