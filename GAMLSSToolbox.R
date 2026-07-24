#**************************************************************
# by Puneet Talwar
# Dec 2025

# R 4.1.3
#**************************************************************

options(width = 200)
options(shiny.maxRequestSize=30*1024^2) # Maximum upload file size 30 MB

#*****************************
# Load libraries
#*****************************
 
library(shiny)
library(gamlss)
library(readr)
library(readxl)
library(DT)
library(modelsummary)
library(shinyjs)
library(gamlss.ggplots)
library(mctest)
library(gamlss.add)    # if used elsewhere

#*****************************
# UI 
#*****************************

ui <- fluidPage(
  useShinyjs(),  # Include shinyjs
  
  # ---- Custom color theme ----
  
  tags$head(
    tags$style(HTML("
        body {
          background-color: #f4f7fb;
        }
        .title-panel-custom {
          background: linear-gradient(90deg, #2c3e6b 0%, #3f6fb5 100%);
          color: #ffffff;
          padding: 18px 24px;
          border-radius: 8px;
          margin-bottom: 18px;
          box-shadow: 0 2px 6px rgba(0,0,0,0.15);
        }
        .well {
          background-color: #eaf1fb;
          border: 1px solid #c7d9f0;
        }
        .nav-tabs > li > a {
          color: #2c3e6b;
          font-weight: 600;
        }
        .nav-tabs > li.active > a,
        .nav-tabs > li.active > a:focus,
        .nav-tabs > li.active > a:hover {
          background-color: #3f6fb5 !important;
          color: #ffffff !important;
          border-color: #3f6fb5;
        }
        hr {
          border-top: 1px solid #b7c9e2;
        }
        .btn, .btn-default {
          background-color: #3f6fb5;
          color: #ffffff;
          border: none;
        }
        .btn:hover, .btn-default:hover {
          background-color: #2c3e6b;
          color: #ffffff;
        }
        h4 {
          color: #2c3e6b;
        }
      "))
  ),
  
  div(class = "title-panel-custom",
      titlePanel("GAMLSSToolbox")
      # tags$img(src = "FlexibleGLMM_logo.png", 
      #          height = "80px",
      #          #width = "50%",
      #          style="position:fixed;right:30px;top:10px;")
  ),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("data_file","Upload Data Frame (Excel or csv files)", accept = c(".csv", ".xlsx")),
      #actionButton("view_data", "View Data"),
      p("Select columns to include or exclude from analysis:"),
      uiOutput("select_columns_ui"),
      actionButton("apply_column_selection", "Apply Column Selection"),
      hr(),
      uiOutput("factor_vars_ui"),
      uiOutput("numeric_vars_ui"),
      actionButton("apply_var_types", "Apply Variable Types"),
      hr(),
      radioButtons("missing_value", "Missing Value Treatment (Empty/Blank cells)", choices = c("Empty/Blank cells"), selected = "NA"),
      actionButton("remove_missing", "Remove Missing Values"),
      verbatimTextOutput("dimBefore"),
      verbatimTextOutput("dimAfter"),
      verbatimTextOutput("missingDataCheck"),
      hr(),
      selectInput("outlier_method", "Outlier Detection Method",
                  choices = c("None",
                              "Z-score (SD cutoff)",
                              "Mahalanobis Distance"
                              )
      ),
      numericInput("n_sigmas", "SD cutoff (Z-score)", value = NA, min = 1),
      numericInput("mahal_cutoff", "Mahalanobis cutoff (Chi-square quantile, e.g. 0.99)", value = 0.99, min = 0.5, max = 0.999),
      actionButton("remove_outliers", "Remove Outliers"),
      verbatimTextOutput("dimAfterOutlierRemoval"),
      hr(),
      downloadButton("download_no_outliers", "Download Cleaned CSV"),
      hr(),
      uiOutput("standardize_vars_ui"),
      uiOutput("standardize_vars"),
      radioButtons("center_scale_mode", "Numeric preprocessing",
                   choices = c("Center and scale" = "center_scale",
                               "Center only" = "center_only",
                               "Scale only" = "scale_only",
                               "None" = "none"),
                   selected = "center_scale"),
      actionButton("apply_standardization", "Apply data standardization"),
      actionButton("open_data_view", "View Processed Data"),
      hr(),
      p("Multiple y and x inputs are allowed"),
      uiOutput("yInput"),
      selectInput(
        inputId = "dist_type",
        label = "Select Distribution Type",
        choices = c(
          "realline" = "realline",
          "realplus" = "realplus",
          "real0to1" = "real0to1",
          "counts"   = "counts",
          "binom"    = "binom"
        ),
        selected = "realplus"
      ),
      actionButton("fit_distribution", "Fit Distribution"),
      verbatimTextOutput("fitStatus"),
      uiOutput("xInput"),
      uiOutput("interactInput"),
      uiOutput("interactInput2"),   # second interaction selector
      uiOutput("familyInput"),
      p("Select atleast one covariate"),
      uiOutput("additionalVarsInput"),
      p("If vcov fails, models automatically switch to option qr"),
      radioButtons("summary_type", "Summary Type", choices = c("vcov", "qr"), selected = "vcov"),
      p("Distributional parameter formulas (optional):"),
      uiOutput("sigmaFormulaInput"),
      uiOutput("nuFormulaInput"),
      uiOutput("tauFormulaInput"),
      uiOutput("customFamilyInput"),
      uiOutput("customEquationInput"),
      p("custom Equation option gives only model summary outputs"),
      actionButton("run", "Run Models")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Instructions", 
                 
                 div(style="text-align:left; font-size: 16px",br(),
                     br(),
                     tags$b("The current version of GAMLSS toolbox can be used to"),br(),
                     br(),
                     "- fit basic GAMLSS models",br(),
                     "- identify fit distribution family for the dependent variables" ,br(),
                     "- run analysis for a multiple dependent and independent variables simultaneously",br(),
                     br(),
                     tags$b("Usage:"),br(),
                     br(),
                     "- Data input format: xlsx or csv file with header row containing variable names.",br(),
                     "- By default first sheet will be used as the input. Ex. mtcars, sleepstudy (lme4)",br(),
                     "- Missing values are blank/empty cells in the data",br(),
                     
                     "- For outlier removal specify the standard deviation value (ex. 3)",br(),
                     
                     "- Multiple covariates can be selected from the input data (age sex bmi)",br(), 
                     
                     "- Multiple fit families can be selected for good-of-fit comparison.",br(),
                     
                     "- For family of distributions Refer- https://www.gamlss.com/wp-content/uploads/2023/06/gamlssreferencecard.pdf",br(),
                     
                     "- Custom equation format : y ~ x1 + x2",br(),
                     br(),
                     
                     tags$b("Note:"),br(),
                     br(),
                     "- The summary output can use either 'vcov' or 'qr' for summary type; 'vcov' is the default.",br(),
                     
                     "- For using splines in a GAMLSS model, custom equation option should be used.",br(),
                     
                     "- Currently, it does not support Random effects.",br(),
                     
                     " - For advanced R script with options to download results (tables and plots), please send an email to
                         ptalwar@uliege.be; talwar.puneet@gmail.com."
                     
                 )
        ),
        
        tabPanel("Data", DTOutput("dataPreviewTable")
        ),

        tabPanel("FitDist Output",
                 verbatimTextOutput("fitDistLogs") # Logs displayed here
        ),
        
        tabPanel("Model Summary", 
                 verbatimTextOutput("modelOutput"),
                 tableOutput("summaryTable"),
                 DTOutput("dataTable")
        ),
        
        tabPanel("Model Tables",
                 uiOutput("modelTablesUI")
        ),
    
        tabPanel("Diagnostics",
                uiOutput("diagnostics_ui")
        ),
        
        tabPanel("Plots", 
                 uiOutput("plotTabs")  # Dynamically generated tabs for plots
        ),
      )
    )
  )
)

#*****************************
# SERVER 
#*****************************

server <- function(input, output, session) {
  
  # Reactive values to store data
  rv <- reactiveValues(data = NULL, cleaned_data = NULL, selected_data = NULL, data_no_outliers = NULL, models = NULL, model_tables = NULL)
  

  #----------------------
  # Data Upload
  #----------------------  
  
  # Reactive expression to read the uploaded data
  observe({
    req(input$data_file)
    file_ext <- tools::file_ext(input$data_file$name)
    if (file_ext == "csv") {
      rv$data <- read_csv(input$data_file$datapath)
    } else if (file_ext == "xlsx") {
      rv$data <- read_excel(input$data_file$datapath)
    } else {
      stop("Invalid file type. Please upload a .csv or .xlsx file.")
    }
  })
  
  #----------------------
  # Column selector
  #----------------------
  
  output$select_columns_ui <- renderUI({
    req(rv$data)
    checkboxGroupInput(
      "selected_columns", 
      "Select Columns to Keep:",
      choices = names(rv$data), 
      selected = names(rv$data)
    )
  })
  
  observeEvent(input$apply_column_selection, {
    req(rv$data, input$selected_columns)
    rv$selected_data <- rv$data[, input$selected_columns, drop = FALSE]
    showNotification("Column selection applied", type = "message")
  })
  
  #----------------------
  # Variable types
  #----------------------
  
  # after selecting columns, present variable-type selectors
  output$factor_vars_ui <- renderUI({
    req(rv$selected_data)
    selectInput('factor_vars', 'Variables to treat as Categorical / Factor', choices = names(rv$selected_data), multiple = TRUE)
  })
  output$numeric_vars_ui <- renderUI({
    req(rv$selected_data)
    selectInput('numeric_vars', 'Variables to treat as Numeric', choices = names(rv$selected_data), multiple = TRUE)
  })
  
  # Apply variable types
  observeEvent(input$apply_var_types, {
    req(rv$selected_data)
    df <- rv$selected_data
    
    # Coerce factors
    if (!is.null(input$factor_vars) && length(input$factor_vars) > 0) {
      for (v in input$factor_vars) {
        if (v %in% names(df)) {
          df[[v]] <- as.factor(df[[v]])
        }
      }
    }
    # Coerce numeric (numeric choice takes precedence if variable chosen in both)
    if (!is.null(input$numeric_vars) && length(input$numeric_vars) > 0) {
      for (v in input$numeric_vars) {
        if (v %in% names(df)) {
          df[[v]] <- suppressWarnings(as.numeric(df[[v]]))
        }
      }
    }
    rv$selected_data <- df
    showNotification('Variable types applied', type = 'message')
  })
  
  #----------------------
  # Remove missing values
  #----------------------
  
  # Remove missing values
  observeEvent(input$remove_missing, {
    df <- rv$selected_data
    output$dimBefore <- renderText({
      paste("Dimensions before removing missing values: ", paste(dim(df), collapse = " x "))
    })
    output$missingDataCheck <- renderText({
      paste("Missing data check: ", paste(names(df), sapply(df, function(x) sum(is.na(x))), sep = ": ", collapse = ", "))
    })
    
    df_cleaned <- na.omit(df)
    rv$cleaned_data <- df_cleaned
    
    output$dimAfter <- renderText({
      paste("Dimensions after removing missing values: ", paste(dim(df_cleaned), collapse = " x "))
    })
  })
  
  #----------------------
  # Outlier removal
  #----------------------
  
  observeEvent(input$remove_outliers, {
    
    df <- if (is.null(rv$cleaned_data)) rv$selected_data else rv$cleaned_data
    req(df)
    
    method <- input$outlier_method
    df_num <- df[sapply(df, is.numeric)]
    
    keep <- rep(TRUE, nrow(df))  # default: keep all
    
    # --- 1. Z-score method ---
    if (method == "Z-score (SD cutoff)" && !is.na(input$n_sigmas)) {
      z <- scale(df_num)
      keep <- apply(abs(z) < input$n_sigmas, 1, all)
    }
    

    # --- 2. Mahalanobis Distance ---
    if (method == "Mahalanobis Distance") {
      try({
        center <- colMeans(df_num, na.rm = TRUE)
        cov_mat <- cov(df_num, use = "complete.obs")
        
        md <- mahalanobis(df_num, center, cov_mat)
        
        cutoff <- qchisq(input$mahal_cutoff, df = ncol(df_num))
        keep <- md < cutoff
      }, silent = TRUE)
    }
    
    # Apply filtering
    rv$data_no_outliers <- df[keep, , drop = FALSE]
    
    output$dimAfterOutlierRemoval <- renderText({
      paste("After outlier removal:",
            paste(dim(rv$data_no_outliers), collapse = " x "),
            "| Removed:", sum(!keep))
    })
  })
  
  
  output$download_no_outliers <- downloadHandler(
    filename = function() {
      paste0("cleaned_no_outliers_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(rv$data_no_outliers)   # ensure data exists
      write.csv(rv$data_no_outliers, file, row.names = FALSE)
    }
  )
  
  
  #----------------------
  # Data standardization
  #----------------------
  
  output$standardize_vars_ui <- renderUI({
    req(rv$selected_data)
    
    numeric_cols <- names(rv$selected_data)[
      sapply(rv$selected_data, is.numeric)
    ]
    
    selectInput(
      "standardize_vars",
      "Select Variables to Standardize",
      choices = numeric_cols,
      multiple = TRUE
    )
  })
  
  # Apply preprocessing
  observeEvent(input$apply_standardization, {
    req(rv$selected_data)
    
    #df <- rv$selected_data
    df <- if (!is.null(rv$data_no_outliers)) rv$data_no_outliers else 
      if (!is.null(rv$cleaned_data)) rv$cleaned_data else rv$selected_data
    req(df)
    
    
    if (!is.null(input$standardize_vars)) {
      
      for (v in input$standardize_vars) {
        
        if (input$center_scale_mode == "center_scale") {
          
          df[[paste0(v, "_cs")]] <-
            as.numeric(scale(df[[v]],
                             center = TRUE,
                             scale = TRUE))
          
        } else if (input$center_scale_mode == "center_only") {
          
          df[[paste0(v, "_c")]] <-
            df[[v]] - mean(df[[v]], na.rm = TRUE)
          
        } else if (input$center_scale_mode == "scale_only") {
          
          df[[paste0(v, "_s")]] <-
            df[[v]] / sd(df[[v]], na.rm = TRUE)
        }
      }
    }
    
    rv$selected_data <- df
    rv$processed_data <- df
    
    showNotification(
      "Data standardization applied successfully",
      type = "message"
    )
  })
  
  #----------------------
  # Inputs
  #----------------------
  
  # Dynamically generate inputs for y, x, family, and additional variables based on the uploaded dataset
  observe({
    req(rv$data)  # Ensure data is available
    
    # Determine the latest version of the data to use for variable selection
    df <- if (!is.null(rv$processed_data)) {
      rv$processed_data
    } else if (!is.null(rv$data_no_outliers)) {
      rv$data_no_outliers
    } else if (!is.null(rv$cleaned_data)) {
      rv$cleaned_data
    } else if (!is.null(rv$selected_data)) {
      rv$selected_data
    } else {
      rv$data
    }
    
    # Ensure `df` is valid and not NULL
    req(df)
    
    # Dynamically generate dependent variable (y) selection
    output$yInput <- renderUI({
      selectInput("y", "Dependent Variables (y)", choices = names(df), multiple = TRUE)
    })
    
    # Dynamically generate independent variable (x) selection
    output$xInput <- renderUI({
      selectInput("x", "Independent Variables (x)", choices = names(df), multiple = TRUE)
    })
    
    # Dynamically generate interaction variable selection (first)
    output$interactInput <- renderUI({
      selectInput("interact_var", "First Interaction Variable (optional)", choices = c("", names(df)), selected = "")
    })
    
    # Dynamically generate second interaction variable selection
    output$interactInput2 <- renderUI({
      selectInput("interact_var2", "Second Interaction Variable (optional)", choices = c("", names(df)), selected = "")
    })
    
    # Dynamically generate family input selection
    output$familyInput <- renderUI({
      selectInput("family", "Family of Distribution", 
                  choices = c("NO", "GA", "GG", "BE", "BB", "BNB", "BEOI", "BEZI", "BEINF", "BI", "BCCG", "BCCGo", "BCPE", "BCPEo", "BCT","BCTo", 
                              "DEL", "DBURR12", "DPO", "DBI", "EXP", "exGAUS", "EGB2", "GA", "GB1", "GB2", "GG", "GIG", "GT", "GEOM", "GEOMo", 
                              "GU", "IGAMMA", "IG", "JSU", "LG", "LO", "LOGITNO", "LOGNO", "LNO", "NBI", "NBII", "NBF", "NET", "NOF", "LQNO", 
                              "PARETO2", "PARETO2o", "PE", "PE2", "PO", "PIG", "RGE", "RG", "SEP1", "SEP2", "SEP3", "SEP4", "SHASH", "SHASHo", 
                              "SHASH", "SI", "SICHEL", "SIMPLEX", "ST1", "ST2", "ST3", "ST4", "ST5", "TF", "WARING", "WEI", "WEI2", "WEI3", 
                              "YULE", "ZABI", "ZABNB", "ZAIG", "ZALG", "ZANBI", "ZAP", "ZASICHEL", "ZAZIPF", "ZIBI", "ZIBNB", "ZINBI", "ZIP", 
                              "ZIP2", "ZIPIG", "ZISICHEL", "ZIPF", "ZAGA", "ZAIG"), 
                  multiple = TRUE)
    })
    
    # Additional covariates input
    output$additionalVarsInput <- renderUI({
      selectInput("additional_vars", "Covariates", choices = names(df), multiple = TRUE)
    })
    
    # Sigma formula input
    output$sigmaFormulaInput <- renderUI({
      selectInput("sigma_vars", "Variables for Sigma Formula", choices = names(df), multiple = TRUE)
    })
    
    # Nu formula input
    output$nuFormulaInput <- renderUI({
      selectInput("nu_vars", "Variables for Nu Formula", choices = names(df), multiple = TRUE)
    })
    
    # Tau formula input
    output$tauFormulaInput <- renderUI({
      selectInput("tau_vars", "Variables for Tau Formula", choices = names(df), multiple = TRUE)
    })
    
    # Custom family input
    output$customFamilyInput <- renderUI({
      textInput("custom_family", "Custom Family of Distribution supported by the gamlss package (required for custom equation)")
    })
    
    # Custom equation input
    output$customEquationInput <- renderUI({
      textInput("custom_equation", "Custom Equation (optional)")
    })
  })
  
  #----------------------
  # Run models
  #----------------------
  
  # Run models based on user input
  runModels <- eventReactive(input$run, {
    req(rv$data)
    #df <- isolate(if (is.null(rv$data_no_outliers)) if (is.null(rv$cleaned_data)) rv$selected_data else rv$cleaned_data else rv$data_no_outliers)
    df <- isolate({
      
      if (!is.null(rv$processed_data)) {
        
        rv$processed_data
        
      } else if (!is.null(rv$data_no_outliers)) {
        
        rv$data_no_outliers
        
      } else if (!is.null(rv$cleaned_data)) {
        
        rv$cleaned_data
        
      } else {
        
        rv$selected_data
        
      }
      
    })
    
    custom_equation <- input$custom_equation
    custom_family <- input$custom_family
    model_tables <- list()
    results <- list()
    
    # Use input$summary_type within this reactive
    summary_type <- isolate(input$summary_type)
    
    if (!is.null(custom_equation) && custom_equation != "") {
      if (is.null(custom_family) || custom_family == "") {
        return("Custom family of distribution is required for custom equation.")
      }
      
      tryCatch({
        model <- gamlss(as.formula(custom_equation), family = custom_family, data = df,
                        control = gamlss.control(n.cyc = 2000, trace = FALSE))
        results[[custom_equation]] <- model
        # use selected summary type
        summary(model, type = summary_type)
      }, error = function(e) {
        results[[custom_equation]] <- as.character(e)
      })
    } else {
      y <- input$y
      x <- input$x
      interact_var <- input$interact_var
      interact_var2 <- input$interact_var2
      family <- if (!is.null(input$custom_family) && input$custom_family != "") c(input$family, input$custom_family) else input$family
      additional_vars <- input$additional_vars
      sigma_vars <- input$sigma_vars
      nu_vars <- input$nu_vars
      tau_vars <- input$tau_vars
      results <- list()
      
      # iterate combinations (y,x,family)
      for (dv in y) {
        for (iv in x) {
          for (fam in family) {
            # base RHS for additional covariates
            add_rhs <- if (!is.null(additional_vars) && length(additional_vars) > 0) paste("+", paste(additional_vars, collapse = " + ")) else ""
            
            # Formula without interaction
            formula_no_interact <- as.formula(paste(dv, "~", iv, add_rhs))
            
            # Build interaction formula if one or both interaction variables provided
            int_terms <- c()
            if (!is.null(interact_var) && interact_var != "") int_terms <- c(int_terms, interact_var)
            if (!is.null(interact_var2) && interact_var2 != "") int_terms <- c(int_terms, interact_var2)
            int_terms <- unique(int_terms)
            
            if (length(int_terms) > 0) {
              # build chained interaction: iv * A or iv * A * B
              interact_formula_part <- paste(iv, paste(int_terms, collapse = " * "), sep = " * ")
              formula_with_interact <- as.formula(paste(dv, "~", interact_formula_part, add_rhs))
            } else {
              formula_with_interact <- NULL
            }
            
            # Handle optional distributional formulas
            sigma_formula <- if (!is.null(sigma_vars) && length(sigma_vars) > 0) {
              as.formula(paste("~", paste(sigma_vars, collapse = " + ")))
            } else ~1
            
            nu_formula <- if (!is.null(nu_vars) && length(nu_vars) > 0) {
              as.formula(paste("~", paste(nu_vars, collapse = " + ")))
            } else ~1
            
            tau_formula <- if (!is.null(tau_vars) && length(tau_vars) > 0) {
              as.formula(paste("~", paste(tau_vars, collapse = " + ")))
            } else ~1
            
            print(paste("Dependent Variable =",dv))
            
            # Model without interaction
            tryCatch({
              model_no_interact <- gamlss(
                formula = formula_no_interact, 
                sigma.formula = sigma_formula,
                nu.formula = nu_formula,
                tau.formula = tau_formula,
                family = fam,
                data = df,
                control = gamlss.control(n.cyc = 2000, trace = FALSE)
              )
              # Use user-selected summary type
              summary_out <- tryCatch(summary(model_no_interact, type = summary_type), error = function(e) summary(model_no_interact, type = "vcov"))
              Rsq_no_interact <- Rsq(model_no_interact)
              est_tbl <- get_estimates(model_no_interact,digits=4,quick = FALSE, conf.int = TRUE, conf.level = 0.95,what="mu",exponentiate=TRUE)
              est_tbl2 <- get_estimates(model_no_interact,digits=4,quick = FALSE, conf.int = TRUE, conf.level = 0.95)
              gof_list <- list(
                AIC = tryCatch(AIC(model_no_interact), error = function(e) NA),
                BIC = tryCatch(BIC(model_no_interact), error = function(e) NA),
                GOF = tryCatch(gamlss::gof(model_no_interact), error = function(e) NA)
              )
              #results[[paste(dv, iv, fam, "No Interaction")]] <- list(model = model_no_interact, summary = summary_out, est_exponentiated = est_tbl, Rsq = Rsq_no_interact)#, gof = gof_list)
              results[[paste(dv, iv, fam, "No Interaction")]] <- list(
                model = model_no_interact,
                summary = summary_out,
                est_exponentiated = est_tbl,
                Rsq = Rsq_no_interact,
                convergence = list(
                  converged = tryCatch(model_no_interact$converged, error = function(e) NA),
                  iterations = tryCatch(model_no_interact$iter, error = function(e) NA),
                  cycles = tryCatch(model_no_interact$no.cyc, error = function(e) NA)
                )
              )
              model_tables[[paste(dv, iv, fam, "No Interaction")]] <- list(table = est_tbl2, dv = dv, gof = gof_list)
            }, error = function(e) {
              results[[paste(dv, iv, fam, "No Interaction")]] <- paste("Error:", e$message)
            })
            
            # Model with interaction (only if formula_with_interact is non-NULL)
            if (!is.null(formula_with_interact)) {
              tryCatch({
                model_with_interact <- gamlss(
                  formula = formula_with_interact, 
                  sigma.formula = sigma_formula,
                  nu.formula = nu_formula,
                  tau.formula = tau_formula,
                  family = fam,
                  data = df,
                  control = gamlss.control(n.cyc = 2000, trace = FALSE)
                )
                summary_out <- tryCatch(summary(model_with_interact, type = summary_type), error = function(e) summary(model_with_interact, type = "vcov"))
                Rsq_with_interact <- Rsq(model_with_interact)
                est_interact <- get_estimates(model_with_interact,digits=4,quick = FALSE, conf.int = TRUE, conf.level = 0.95,what="mu",exponentiate=TRUE)
                est_interact2 <- get_estimates(model_with_interact,digits=4,quick = FALSE, conf.int = TRUE, conf.level = 0.95)
                gof_list2 <- list(AIC = tryCatch(AIC(model_with_interact), error = function(e) NA), BIC = tryCatch(BIC(model_with_interact), error = function(e) NA), GOF = tryCatch(gamlss::gof(model_with_interact), error = function(e) NA))
                #results[[paste(dv, iv, fam, "With Interaction")]] <- list(model = model_with_interact, summary = summary_out, est_exponentiated =est_interact, Rsq = Rsq_with_interact)#, gof = gof_list2)
                results[[paste(dv, iv, fam, "With Interaction")]] <- list(
                  model = model_with_interact,
                  summary = summary_out,
                  est_exponentiated = est_interact,
                  Rsq = Rsq_with_interact,
                  convergence = list(
                    converged = tryCatch(model_with_interact$converged, error = function(e) NA),
                    iterations = tryCatch(model_with_interact$iter, error = function(e) NA),
                    cycles = tryCatch(model_with_interact$no.cyc, error = function(e) NA)
                  )
                )
                model_tables[[paste(dv, iv, fam, "With Interaction")]] <- list(table = est_interact2, dv = dv, gof = gof_list2)
              }, error = function(e) {
                results[[paste(dv, iv, fam, "With Interaction")]] <- paste("Error:", e$message)
              })
            }
          }
        }
      }
    }
    rv$models <- results
    rv$model_tables <- model_tables
    results
  })
  
  #----------------------
  # Outputs
  #----------------------
  
  # Outputs
  output$dataPreviewTable <- renderDT({
    req(rv$data)
    datatable(rv$data, options = list(scrollX = TRUE, pageLength = 25))
  })
  
  
  observeEvent(input$open_data_view, {
    req(rv$selected_data)
    rv$processed_data    
    
    showModal(modalDialog(
      title = "Processed Data",
      size = "l",
      easyClose = TRUE,
      DT::DTOutput("processed_table"),
      footer = modalButton("Close")
    ))
  })
  
  
  output$processed_table <- DT::renderDT({
    DT::datatable(
      rv$processed_data,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        scrollY = "500px",
        lengthMenu = c(10,25,50,100),
        paging = TRUE
      ),
      class = "display nowrap"
    )
  })
  
  
  observeEvent(input$fit_distribution, {
    
    # df <- isolate(
    #   if (is.null(rv$data_no_outliers)) {
    #     if (is.null(rv$cleaned_data)) rv$selected_data else rv$cleaned_data
    #   } else {
    #     rv$data_no_outliers
    #   }
    # )
    
    df <- if (!is.null(rv$processed_data))
      rv$processed_data
    else if (!is.null(rv$data_no_outliers))
      rv$data_no_outliers
    else if (!is.null(rv$cleaned_data))
      rv$cleaned_data
    else
      rv$selected_data
    
    y <- input$y
    selected_type <- input$dist_type   # <-- user-selected type
    
    fit_logs <- vector("list", length(y))
    
    for (i in seq_along(y)) {
      
      fit_logs[[i]] <- tryCatch({
        
        fit <- fitDist(
          df[[y[i]]],
          k = 2,
          type = selected_type,   # <-- dynamic type
          trace = FALSE,
          try.gamlss = TRUE
        )
        
        if (!is.null(fit)) {
          paste(
            "Dependent Variable =", y[i], "\n",
            "Type =", selected_type, "\n",
            paste(capture.output(summary(fit)), collapse = "\n")
          )
        } else {
          paste("Dependent Variable =", y[i], ": Fit failed or returned NULL.")
        }
        
      }, error = function(e) {
        paste("Error in fitting distribution for", y[i], ":", e$message)
      })
    }
    
    all_logs <- paste(
      paste("Selected Type:", selected_type),
      "\n\n",
      paste(fit_logs, collapse = "\n\n"),
      collapse = "\n"
    )
    
    # Render logs
    output$fitDistLogs <- renderText({
      all_logs
    })
    
    # Status message
    output$fitStatus <- renderText({
      paste("Fit distribution completed using type:", selected_type,
            ". Check the 'FitDist Output' tab for details.")
    })
  }) 
  
  observe({
    req(rv$models)
    models <- rv$models
    
    # Generate dynamic tabs for model plots with model names
    output$plotTabs <- renderUI({
      do.call(
        tabsetPanel,
        lapply(seq_along(models), function(i) {
          tabPanel(
            title = names(models)[i],  # Use model names as tab titles
            fluidRow(
              column(6, plotOutput(outputId = paste0("plot_", i, "_1"))),  # First plot
              column(6, plotOutput(outputId = paste0("plot_", i, "_2"))),  # Second plot
              column(6, plotOutput(outputId = paste0("plot_", i, "_3"))),  # Third plot
              column(6, plotOutput(outputId = paste0("plot_", i, "_4")))   # Fourth plot
            )
          )
        })
      )
    })
    
    # Generate multiple plots for each model
    for (i in seq_along(models)) {
      local({
        model_entry <- models[[i]]
        # Some entries may be error messages instead of lists
        model <- if (is.list(model_entry) && "model" %in% names(model_entry)) model_entry$model else NULL
        model_name <- names(models)[i]  # Get the model name
        
        # Helper: safe fallback plotting function (residuals vs fitted)
        safe_resid_fallback <- function(mod) {
          stopifnot(!is.null(mod))
          fitted_vals <- tryCatch(fitted(mod, what = "mu"), error = function(e) NULL)
          resid_vals <- tryCatch(resid(mod), error = function(e) NULL)
          if (is.null(fitted_vals) || is.null(resid_vals)) {
            # simple residual plot using response
            y_obs <- tryCatch(model.frame(mod)[,1], error = function(e) NULL)
            if (!is.null(y_obs)) {
              plot(y_obs, pch = 20, main = "Observed (fallback plot)", xlab = "Index", ylab = "Observed")
            } else {
              plot(1,1, type = "n", main = "No plot available")
            }
          } else {
            ylim_range <- range(resid_vals, na.rm = TRUE)
            if (any(is.infinite(ylim_range))) ylim_range <- range(resid_vals[is.finite(resid_vals)], na.rm = TRUE)
            # expand limits slightly
            expand_factor <- 0.1
            yrange <- ylim_range + c(-1,1) * abs(diff(ylim_range)) * expand_factor
            if (is.na(yrange[1]) || is.na(yrange[2]) || yrange[1] == yrange[2]) {
              yrange <- ylim_range
            }
            plot(fitted_vals, resid_vals, pch = 20, xlab = "Fitted", ylab = "Residuals", main = "Residuals vs Fitted (fallback)", ylim = yrange)
            abline(h = 0, lty = 2)
          }
        }
        
        # Render the first plot
        output[[paste0("plot_", i, "_1")]] <- renderPlot({
          req(model_entry)
          if (inherits(model, "gamlss")) {
            # try original plotting function, fallback to safe plot on error
            tryCatch({
              resid_plots(model)
              
            }, error = function(e) {
              try(safe_resid_fallback(model), silent = TRUE)
            })
          } else {
            plot.new(); title("No model to plot")
          }
        })
        
        # Render the second plot
        output[[paste0("plot_", i, "_2")]] <- renderPlot({
          req(model_entry)
          if (inherits(model, "gamlss")) {
            tryCatch({
              resid_wp(model)
              
            }, error = function(e) {
              try(safe_resid_fallback(model), silent = TRUE)
            })
          } else {
            plot.new(); title("No model to plot")
          }
        })
        
        # Render the third plot
        output[[paste0("plot_", i, "_3")]] <- renderPlot({
          req(model_entry)
          if (inherits(model, "gamlss")) {
            tryCatch({
              mc.plot(model) #mctest::mc.plot
              
            }, error = function(e) {
              try(safe_resid_fallback(model), silent = TRUE)
            })
          } else {
            plot.new(); title("No model to plot")
          }
        })
        
        # Render the fourth plot
        output[[paste0("plot_", i, "_4")]] <- renderPlot({
          req(model_entry)
          if (inherits(model, "gamlss")) {
            tryCatch({
              moment_bucket(model)
              
            }, error = function(e) {
              try(safe_resid_fallback(model), silent = TRUE)
            })
          } else {
            plot.new(); title("No model to plot")
          }
        })
      })
    }
  })
  
  #----------------------
  # Model summary output
  #----------------------
  
  output$modelOutput <- renderPrint({
    results <- runModels()
    
    for (nm in names(results)) {
      cat("\n============================\n")
      cat("Model:", nm, "\n")
      cat("============================\n")
      
      res <- results[[nm]]
      
      if (is.list(res) && !is.null(res$model)) {
        
        conv <- res$convergence
        
        cat("Converged:", conv$converged, "\n")
        cat("Iterations:", conv$iterations, "\n")
        cat("Cycles:", conv$cycles, "\n\n")
        
        #print(res$summary)
        #results
        summary(res$model)
        
        
      } else {
        print(res)
      }
    }
  })
  
  #----------------------
  # Model Table output
  #----------------------
  
  # Render model tables UI (one DT and download button per model table)
  output$modelTablesUI <- renderUI({
    req(rv$models)
    # Skip tables for custom equation
    if (!is.null(input$custom_equation) && nzchar(input$custom_equation)) {
      return(div("Model tables are skipped for custom equation fits."))
    }
    tables <- rv$model_tables
    if (is.null(tables) || length(tables) == 0) return(div("No model tables generated (check logs or try running models)."))
    
    # Build UI elements
    tabs <- lapply(seq_along(tables), function(i) {
      nm <- names(tables)[i]
      tbl_info <- tables[[i]]
      df_tbl <- tbl_info$table
      dvname <- tbl_info$dv
      gof <- tbl_info$gof
      # Create unique IDs
      dt_id <- paste0("dt_", i)
      dl_id <- paste0("dl_", i)
      box <- tagList(
        h4(paste0("Model: ", nm)),
        h5(paste0("Dependent variable: ", dvname)),
        downloadButton(dl_id, "Download table (CSV)"),
        br(), br(),
        DTOutput(dt_id),
        br(),
        div(strong("AIC:"), formatC(gof$AIC, digits = 4, format = "f"), "  ",
            strong("BIC:"), formatC(gof$BIC, digits = 4, format = "f")),
        hr()
      )
      # Render DT and download handlers via server-side observers using local closure
      local({
        my_i <- i
        my_dt_id <- dt_id
        my_dl_id <- dl_id
        my_tbl <- df_tbl
        # Create output renderers
        output[[my_dt_id]] <- renderDT({
          # round any numeric columns to 4 decimals
          df2 <- my_tbl
          numcols <- sapply(df2, is.numeric)
          df2[numcols] <- lapply(df2[numcols], function(x) round(x, 4))
          datatable(df2, options = list(scrollX = TRUE, pageLength = 20))
        })
        output[[my_dl_id]] <- downloadHandler(
          filename = function() {
            paste0(gsub("[^A-Za-z0-9_\\-]", "_", nm), "_table.csv")
          },
          content = function(file) {
            df2 <- my_tbl
            numcols <- sapply(df2, is.numeric)
            df2[numcols] <- lapply(df2[numcols], function(x) round(x, 4))
            # Prepend dependent variable name as a top row comment
            writeLines(paste0("Dependent variable:,", tbl_info$dv), con = file, sep = "\n")
            # Then append table
            write.table(df2, file = file, sep = ",", row.names = FALSE, col.names = TRUE, append = TRUE)
          }
        )
      })
      box
    })
    do.call(tagList, tabs)
  })
  
  # ------------------------------------------------------------------
  # DIAGNOSTICS UI
  # ------------------------------------------------------------------
  
  observe({
    
    results <- runModels()
    req(results)
    
    #df <- if (is.null(rv$cleaned_data)) rv$selected_data else rv$cleaned_data
    
    df <- if (!is.null(rv$processed_data))
      rv$processed_data
    else if (!is.null(rv$data_no_outliers))
      rv$data_no_outliers
    else if (!is.null(rv$cleaned_data))
      rv$cleaned_data
    else
      rv$selected_data
    
    df_num <- df[sapply(df, is.numeric)]
    
    lapply(names(results), function(nm) {
      
      safe <- make.names(nm)
      
      # ---------------------------
      # Mahalanobis Plot
      # ---------------------------
      output[[paste0("mahal_", safe)]] <- renderPlot({
        
        try({
          
          req(ncol(df_num) > 1)
          
          df_complete <- na.omit(df_num)
          
          center <- colMeans(df_complete)
          cov_mat <- cov(df_complete)
          
          md <- mahalanobis(df_complete, center, cov_mat)
          
          cutoff <- qchisq(0.99, df = ncol(df_complete))
          
          plot(md,
               main = "Mahalanobis Distance",
               ylab = "Distance", xlab = "Observation")
          
          abline(h = cutoff, lty = 2)
          
          # highlight outliers
          outliers <- which(md > cutoff)
          points(outliers, md[outliers], pch = 19)
          
        }, silent = TRUE)
        
      })
      
    })
  })
  
  
  output$diagnostics_ui <- renderUI({
    
    results <- runModels()
    req(results)
    
    tabs <- lapply(names(results), function(nm) {
      
      safe <- make.names(nm)
      
      tabPanel(nm,
               h4("Mahalanobis Distance [Default cutoff]"),
               plotOutput(paste0("mahal_", safe), height = "300px")   
      )
    })
    
    do.call(tabsetPanel, tabs)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
