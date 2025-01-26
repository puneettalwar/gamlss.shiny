# Load necessary libraries
library(shiny)
library(gamlss)
library(readr)
library(readxl)
library(DT)
library(modelsummary)
library(shinyjs)
library(gamlss.ggplots)
library(mctest)

# Define UI
ui <- fluidPage(
  useShinyjs(),  # Include shinyjs
  titlePanel("GAMLSS Regression Toolbox"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("data_file","Upload Data Frame (Excel or csv files)", accept = c(".csv", ".xlsx")),
      actionButton("view_data", "View Data"),
      p("Select columns to include or exclude from analysis:"),
      uiOutput("select_columns_ui"),
      actionButton("apply_column_selection", "Apply Column Selection"),
      # div(
      #   id = "column_selection_div",  # Assign an ID to this div for hiding or collapsing
      #   p("Select columns to include or exclude from analysis:"),
      #   uiOutput("select_columns_ui"),
      #   actionButton("apply_column_selection", "Apply Column Selection")
      # ),
      radioButtons("missing_value", "Missing Value Treatment", choices = c("Missing (Blank)", "Dot (.)"), selected = "NA"),
      actionButton("remove_missing", "Remove Missing Values"),
      verbatimTextOutput("dimBefore"),
      verbatimTextOutput("dimAfter"),
      verbatimTextOutput("missingDataCheck"),
      numericInput("n_sigmas", "Standard deviation for Outlier Removal", value = NULL, min = 1),
      actionButton("remove_outliers", "Remove Outliers"),
      verbatimTextOutput("dimAfterOutlierRemoval"),
      p("Multiple y and x inputs are allowed"),
      uiOutput("yInput"),
      actionButton("fit_distribution", "Fit Distribution"),
      verbatimTextOutput("fitStatus"),
      uiOutput("xInput"),
      uiOutput("interactInput"),
      uiOutput("familyInput"),
      p("Select atleast one covariate"),
      uiOutput("additionalVarsInput"),
      p("Distributional parameter formulas (optional):"),
      uiOutput("sigmaFormulaInput"),
      uiOutput("nuFormulaInput"),
      uiOutput("tauFormulaInput"),
      uiOutput("customFamilyInput"),
      uiOutput("customEquationInput"),
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
                     "- Missing values are either empty cells or coded -999 in the data",br(),
                     
                     "- For outlier removal specify the standard deviation value (ex. 3)",br(),
                     
                     "- Multiple covariates can be selected from the input data (age sex bmi)",br(), 
                     
                     "- Multiple fit families can be selected for good-of-fit comparison.",br(),
                     
                     "- For family of distributions Refer- https://www.gamlss.com/wp-content/uploads/2023/06/gamlssreferencecard.pdf",br(),
                     
                     "- Custom equation format : y ~ x1 + x2",br(),
                     br(),
                     
                     tags$b("Note:"),br(),
                     br(),
                     "- The summary output uses qr method for stability and consistency.",br(),
                     
                     "- For using splines in a GAMLSS model, custom equation option should be used.",br(),
                     
                     "- Currently, it does not support Random effects.",br(),
                     
                     " - For advanced R script with options to download results (tables and plots), please send an email to
                         ptalwar@uliege.be; talwar.puneet@gmail.com."
                     
                 )
        ),
        
        tabPanel("Outlier Logs",
                 verbatimTextOutput("verboseLogs") # Logs displayed here
        ),
        
        tabPanel("FitDist Output",
                 verbatimTextOutput("fitDistLogs") # Logs displayed here
        ),
        
        tabPanel("Model Summary", 
                 verbatimTextOutput("modelOutput"),
                 tableOutput("summaryTable"),
                 DTOutput("dataTable")
        ),
        tabPanel("Plots", 
                 uiOutput("plotTabs")  # Dynamically generated tabs for plots
        )
      )
    )
  )
)



server <- function(input, output, session) {
  
  # Reactive values to store data
  rv <- reactiveValues(data = NULL, cleaned_data = NULL, selected_data = NULL, data_no_outliers = NULL, models = NULL)
  
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
  
  
  # observeEvent(input$apply_column_selection, {
  #   req(rv$data, input$selected_columns)
  #   rv$selected_data <- rv$data[, input$selected_columns, drop = FALSE]
  #   showNotification("Column selection applied", type = "message")
  #   
  #   # Hide or collapse the column selection tab
  #   hide("column_selection_div")  # This uses shinyjs to hide the div
  # })
  
  observeEvent(input$view_data, {
    showModal(modalDialog(
      title = "Data Frame",
      DTOutput("dataPreviewTable"),
      size = "l",
      easyClose = TRUE
    ))
  })
  
  output$dataPreviewTable <- renderDT({
    req(rv$data)
    df <- isolate(if (is.null(rv$data_no_outliers)) if (is.null(rv$cleaned_data)) rv$selected_data else rv$cleaned_data else rv$data_no_outliers)
    datatable(df, options = list(scrollX = TRUE, pageLength = 20))
  })
  
  
  # Remove missing values
  observeEvent(input$remove_missing, {
    df <- rv$selected_data
    output$dimBefore <- renderText({
      paste("Dimensions before removing missing values: ", paste(dim(df), collapse = " x "))
    })
    output$missingDataCheck <- renderText({
      paste("Missing data check: ", paste(names(df), sapply(df, function(x) sum(is.na(x))), sep = ": ", collapse = ", "))
    })
    
    if (input$missing_value == ".") {
      df[df == .] <- NA
    }
    df_cleaned <- na.omit(df)
    rv$cleaned_data <- df_cleaned
    
    
    output$dimAfter <- renderText({
      paste("Dimensions after removing missing values: ", paste(dim(df_cleaned), collapse = " x "))
    })
  })
  
  
  observeEvent(input$remove_outliers, {
    # Get the current dataset (cleaned or original)
    df <- isolate(if (is.null(rv$cleaned_data)) rv$selected_data else rv$cleaned_data)
    
    # Create a connection to capture verbose output
    verbose_logs <- capture.output({
      df_no_outliers <- dataPreparation::remove_sd_outlier(df,cols = "auto",n_sigmas = input$n_sigmas,verbose = TRUE)
      # Save the cleaned data
      rv$data_no_outliers <- df_no_outliers
    }, type = "output")
    
    # Concatenate verbose logs into a single string for display
    verbose_logs <- paste(verbose_logs, collapse = "\n")
    
    # Render verbose logs in the app
    output$verboseLogs <- renderText({
      verbose_logs
    })
    
    # Display dimensions after removing outliers
    output$dimAfterOutlierRemoval <- renderText({
      paste("Dimensions after removing outliers: ", paste(dim(rv$data_no_outliers), collapse = " x "))
    })  
  })
  
  
  # Data Preview Table
  observeEvent(input$view_data, {
    showModal(modalDialog(
      title = "Data Frame",
      DTOutput("dataPreviewTable"),
      size = "l",
      easyClose = TRUE
    ))
  })
  
  output$dataPreviewTable <- renderDT({
    req(rv$data)  # Ensure data is uploaded before rendering
    df <- rv$data  # Always use the raw data for preview
    datatable(df, options = list(scrollX = TRUE, pageLength = 20))
  })
  
  # Dynamically generate inputs for y, x, family, and additional variables based on the uploaded dataset
  observe({
    req(rv$data)  # Ensure data is available
    
    # Determine the latest version of the data to use for variable selection
    df <- if (!is.null(rv$data_no_outliers)) {
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
    
    # Dynamically generate interaction variable selection
    output$interactInput <- renderUI({
      selectInput("interact_var", "Interaction Variable", choices = c("", names(df)), selected = "")
    })
    
    # Dynamically generate family input selection
    output$familyInput <- renderUI({
      selectInput("family", "Family of Distribution", 
                  choices = c("NO", "GA", "GG", "BE", "BB", "BNB", "BEOI", "BEZI", "BEINF", "BI", "BCCG", "BCCGo", "BCPE", "BCPEo", "BCT", 
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
  
  # Run models based on user input
  runModels <- eventReactive(input$run, {
    req(rv$data)
    df <- isolate(if (is.null(rv$data_no_outliers)) if (is.null(rv$cleaned_data)) rv$selected_data else rv$cleaned_data else rv$data_no_outliers)
    custom_equation <- input$custom_equation
    custom_family <- input$custom_family
    results <- list()
    
    if (custom_equation != "") {
      if (custom_family == "") {
        return("Custom family of distribution is required for custom equation.")
      }
      
      tryCatch({
        model <- gamlss(as.formula(custom_equation), family = custom_family, data = df)
        results[[custom_equation]] <- model
        summary(model,type="qr")
      }, error = function(e) {
        results[[custom_equation]] <- as.character(e)
      })
    } else {
      y <- input$y
      x <- input$x
      interact_var <- input$interact_var
      family <- if (input$custom_family != "") c(input$family, input$custom_family) else input$family
      additional_vars <- input$additional_vars
      sigma_vars <- input$sigma_vars
      nu_vars <- input$nu_vars
      tau_vars <- input$tau_vars
      results <- list()
      
      for (dv in y) {
        for (iv in x) {
          for (fam in family) {
            # Formula without interaction
            formula_no_interact <- as.formula(paste(dv, "~", iv, "+", paste(additional_vars, collapse = " + ")))
            
            # Formula with interaction (if interact_var is provided)
            formula_with_interact <- if (!is.null(interact_var) && interact_var != "") {
              as.formula(paste(dv, "~", iv, "*", interact_var, "+", paste(additional_vars, collapse = " + ")))
            } else NULL
            
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
              estimates_no_interact <- get_estimates(model_no_interact,digits=4,quick = FALSE, conf.int = TRUE, conf.level = 0.95)
              summary <- summary(model_no_interact, type="qr")
              Rsq_no_interact <- Rsq(model_no_interact)
              results[[paste(dv, iv, fam, "No Interaction")]] <- list(model = model_no_interact, summary = summary, estimates = estimates_no_interact,Rsq = Rsq_no_interact)
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
                estimates_with_interact <- get_estimates(model_with_interact,digits=4,quick = FALSE, conf.int = TRUE, conf.level = 0.95)
                summary <- summary(model_with_interact, type="qr")
                Rsq_with_interact <- Rsq(model_with_interact)
                results[[paste(dv, iv, fam, "With Interaction")]] <- list(model = model_with_interact, summary = summary, estimates = estimates_with_interact,Rsq = Rsq_with_interact)
              }, error = function(e) {
                results[[paste(dv, iv, fam, "With Interaction")]] <- paste("Error:", e$message)
              })
            }
          }
        }
      }
    }
    rv$models <- results
    results
  })
  
  observeEvent(input$fit_distribution, {
    
    df <- isolate(if (is.null(rv$data_no_outliers)) if (is.null(rv$cleaned_data)) rv$selected_data else rv$cleaned_data else rv$data_no_outliers)
    y <- input$y
    
    fit_logs <- vector("list", length(y)) # Store logs for each variable
    
    for (i in seq_along(y)) {
      fit_logs[[i]] <- tryCatch({
        fit <- fitDist(df[[y[i]]], k = 2, type = "realplus", trace = FALSE, try.gamlss = TRUE)
        summary(fit)
        
        if (!is.null(fit)) {
          log <-paste(capture.output(summary(fit)),collapse = "\n")
          log
        } else {
          paste("Dependent Variable =", y[i], ": Fit failed or returned NULL.")
        }
      }, error = function(e) {
        paste("Error in fitting distribution for", y[i], ":", e$message)
      })
    }
    
    all_logs <- paste(
      paste("Dependent Variable:", y, collapse = "\n"),
      "\n\n",
      paste(fit_logs, collapse = "\n\n"),
      collapse = "\n\n"
    )
    
    # Render logs in the FitDist Output tab
    output$fitDistLogs <- renderText({
      all_logs
    })
    
    # Update status in the main panel
    output$fitStatus <- renderText({
      "Fit distribution process completed. Check the 'FitDist Output' tab for details."
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
        model <- models[[i]]$model  # Access the model using its index
        model_name <- names(models)[i]  # Get the model name
        
        # Render the first plot
        output[[paste0("plot_", i, "_1")]] <- renderPlot({
          req(model)
          if (inherits(model, "gamlss")) {
            resid_plots(model)  
          }
        })
        
        # Render the second plot
        output[[paste0("plot_", i, "_2")]] <- renderPlot({
          req(model)
          if (inherits(model, "gamlss")) {
            resid_wp(model)  
          }
        })
        
        # Render the third plot
        output[[paste0("plot_", i, "_3")]] <- renderPlot({
          req(model)
          if (inherits(model, "gamlss")) {
            mc.plot(model)  
          }
        })

        # Render the fourth plot
        output[[paste0("plot_", i, "_4")]] <- renderPlot({
          req(model)
          if (inherits(model, "gamlss")) {
            moment_bucket(model)  
          }
        })
      })
    }
  })
  
  
  output$modelOutput <- renderPrint({
    results <- runModels()
    results
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
