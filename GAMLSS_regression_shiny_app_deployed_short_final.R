# Load necessary libraries
library(shiny)
library(gamlss)
library(readr)
library(readxl)
library(DT)

# Define UI
ui <- fluidPage(
  titlePanel("GAMLSS Regression Toolbox"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("data_file", "Upload Data Frame", accept = c(".csv", ".xlsx")),
      actionButton("view_data", "View Data"),
      radioButtons("missing_value", "Missing Value Treatment", choices = c("Missing (Blank)", "-999"), selected = "NA"),
      actionButton("remove_missing", "Remove Missing Values"),
      verbatimTextOutput("dimBefore"),
      verbatimTextOutput("dimAfter"),
      verbatimTextOutput("missingDataCheck"),
      numericInput("n_sigmas", "Standard deviation for Outlier Removal", value = NULL, min = 1),
      actionButton("remove_outliers", "Remove Outliers"),
      verbatimTextOutput("dimAfterOutlierRemoval"),
      p("Multiple y and x inputs are allowed"),
      uiOutput("yInput"),
      uiOutput("xInput"),
      uiOutput("interactInput"),
      uiOutput("familyInput"),
      uiOutput("additionalVarsInput"),
      p("Distributional parameter formulas (optional):"),
      uiOutput("sigmaFormulaInput"),
      uiOutput("nuFormulaInput"),
      uiOutput("tauFormulaInput"),
      uiOutput("customFamilyInput"),
      uiOutput("customEquationInput"),
      actionButton("run", "Run Models"),
      
      div(style="text-align:left", "INSTRUCTIONS",br(),
          
          "The current version of GAMLSS toolbox can be used to",br(),
          
          "- fit basic GAMLSS models",br(),
          "- identify fit distribution family for the dependent variables" ,br(),
          "- run analysis for a multiple dependent and independent variables simultaneously",br(),
          
          "Usage:",br(), 
          "- Data input format: xlsx or csv file with header row containing variable names.",br(),
          "By default first sheet will be used as the input. Ex. mtcars, sleepstudy (lme4)",br(),
          "- Missing values are either empty cells or coded -999 in the data",br(),
          
          "- For outlier removal specify the standard deviation value (ex. 3)",br(),
          
          "- Multiple covariates can be selected from the input data (age sex bmi)",br(), 
          
          "- Run the first model with Normal (NO) distribution and check the fit distribution family recommendation output by gamlss. multiple fit families can be selected for good-of-fit comparison.",br(),
          
          "- For family of distributions Refer- https://www.gamlss.com/wp-content/uploads/2023/06/gamlssreferencecard.pdf",br(),
          
          "- Custom equation format : y ~ x1 + x2",br(),
          
          "Note:",br(),
          
          "-The summary output uses qr method for stability and consistency.",br(),
          
          "For using splines in a GAMLSS model, custom equation option should be used.",br(),
          
          "Currently, it does not support Random effects.",br(),
          
          "For advanced R script with options to download results (tables and plots), please send an email to ptalwar@uliege.be; talwar.puneet@gmail.com."
      )
    ),
    
    mainPanel(
      tabsetPanel(
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
  rv <- reactiveValues(data = NULL, cleaned_data = NULL, data_no_outliers = NULL, models = NULL)
  
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
  
  # Remove missing values
  observeEvent(input$remove_missing, {
    df <- rv$data
    output$dimBefore <- renderText({
      paste("Dimensions before removing missing values: ", paste(dim(df), collapse = " x "))
    })
    output$missingDataCheck <- renderText({
      paste("Missing data check: ", paste(names(df), sapply(df, function(x) sum(is.na(x))), sep = ": ", collapse = ", "))
    })
    
    if (input$missing_value == "-999") {
      df[df == -999] <- NA
    }
    df_cleaned <- na.omit(df)
    rv$cleaned_data <- df_cleaned
    
    output$dimAfter <- renderText({
      paste("Dimensions after removing missing values: ", paste(dim(df_cleaned), collapse = " x "))
    })
  })
  
  # Remove outliers based on standard deviation
  observeEvent(input$remove_outliers, {
    df <- isolate(if (is.null(rv$cleaned_data)) rv$data else rv$cleaned_data)
    df_no_outliers <- dataPreparation::remove_sd_outlier(df, n_sigmas = input$n_sigmas, verbose = TRUE)
    rv$data_no_outliers <- df_no_outliers
    
    output$dimAfterOutlierRemoval <- renderText({
      paste("Dimensions after removing outliers: ", paste(dim(df_no_outliers), collapse = " x "))
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
    req(rv$data)
    df <- isolate(if (is.null(rv$data_no_outliers)) if (is.null(rv$cleaned_data)) rv$data else rv$cleaned_data else rv$data_no_outliers)
    datatable(df, options = list(scrollX = TRUE, pageLength = 20))
  })
  
  # Dynamically generate inputs for y, x, family, and additional variables based on the uploaded dataset
  observe({
    req(rv$data)
    df <- isolate(if (is.null(rv$data_no_outliers)) if (is.null(rv$cleaned_data)) rv$data else rv$cleaned_data else rv$data_no_outliers)
    
    output$yInput <- renderUI({
      selectInput("y", "Dependent Variables (y)", choices = names(df), multiple = TRUE)
    })
    
    output$xInput <- renderUI({
      selectInput("x", "Independent Variables (x)", choices = names(df), multiple = TRUE)
    })
    
    output$interactInput <- renderUI({
      selectInput("interact_var", "Interaction Variable", choices = c("", names(df)), selected = "")
    })
    
    output$familyInput <- renderUI({
      selectInput("family", "Family of Distribution", choices = c("NO","GA","GG","BE","BB","BNB","BEOI",	"BEZI",	"BEINF","BI","BCCG","BCPE","BCPEo","BCT","DEL","DBURR12","DPO","DBI","EXP","exGAUS",	
                                                                  "EGB2",	"GA","GB1","GB2","GG","GIG","GT","GEOM","GEOMo","GU","IGAMMA","IG","JSU","LG","LO","LOGITNO",	
                                                                  "LOGNO","LNO","NBI","NBII","NBF","NET","NOF","LQNO","PARETO2","PARETO2o","PE","PE2","PO","PIG","RGE","RG",	
                                                                  "SEP1","SEP2","SEP3","SEP4","SHASH","SHASHo","SHASH","SI","SICHEL","SIMPLEX","ST1","ST2","ST3","ST4","ST5",	
                                                                  "TF","WARING","WEI","WEI2","WEI3","YULE","ZABI","ZABNB","ZAIG","ZALG","ZANBI","ZAP","ZASICHEL","ZAZIPF","ZIBI",
                                                                  "ZIBNB","ZINBI","ZIP","ZIP2","ZIPIG","ZISICHEL","ZIPF","ZAGA","ZAIG"), multiple = TRUE)
    })
    
    output$additionalVarsInput <- renderUI({
      selectInput("additional_vars", "Covariates", choices = names(df), multiple = TRUE)
    })
    
    output$sigmaFormulaInput <- renderUI({
      selectInput("sigma_vars", "Variables for Sigma Formula", choices = names(df), multiple = TRUE)
    })
    
    output$nuFormulaInput <- renderUI({
      selectInput("nu_vars", "Variables for Nu Formula", choices = names(df), multiple = TRUE)
    })
    
    output$tauFormulaInput <- renderUI({
      selectInput("tau_vars", "Variables for Tau Formula", choices = names(df), multiple = TRUE)
    })
    
    output$customFamilyInput <- renderUI({
      textInput("custom_family", "Custom Family of Distribution supported by the gamlss package (required for custom equation)")
    })
    
    output$customEquationInput <- renderUI({  
      textInput("custom_equation", "Custom Equation (optional)")
    })
  })
  
  # Run models based on user input
  runModels <- eventReactive(input$run, {
    req(rv$data)
    
    df <- isolate(if (is.null(rv$data_no_outliers)) if (is.null(rv$cleaned_data)) rv$data else rv$cleaned_data else rv$data_no_outliers)
    custom_equation <- input$custom_equation
    custom_family <- input$custom_family
    #results <- list()
    
    if (custom_equation != "") {
      if (custom_family == "") {
        return("Custom family of distribution is required for custom equation.")
      }
      
      tryCatch({
        model <- gamlss(as.formula(custom_equation), family = custom_family, data = df)
        results[[custom_equation]] <- model
        #summary(model,type="qr")
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
      
      #Fitdist output
      
      for (i in seq_along(y)) {
        fit <- tryCatch({
          fitDist(df[[y[i]]], k = 2, type = "realplus", trace = FALSE, try.gamlss = TRUE)
        }, error = function(e) {
          cat("Error in fitting distribution: ", e$message, "\n")
          NULL
        })
        
        if (!is.null(fit)) {
          print(paste("Dependent Variable =",y[i]))
          summary(fit)
        }
      }
      
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
                control = gamlss.control(n.cyc = 2000, trace = TRUE)
              )
              results[[paste(dv, iv, fam, "No Interaction")]] <- model_no_interact
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
                  control = gamlss.control(n.cyc = 2000, trace = TRUE)
                )
                results[[paste(dv, iv, fam, "With Interaction")]] <- model_with_interact
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
  
  # Dynamically generate tabs for plots
  output$plotTabs <- renderUI({
    req(rv$models)
    models <- rv$models
    tabs <- lapply(names(models), function(model_name) {
      tabPanel(
        model_name,
        plotOutput(paste0("plot_", gsub(" ", "_", model_name)))
      )
    })
    do.call(tabsetPanel, tabs)
  })
  
  # Generate plots for each model
  observe({
    req(rv$models)
    models <- rv$models
    for (model_name in names(models)) {
      local({
        model <- models[[model_name]]
        model_id <- paste0("plot_", gsub(" ", "_", model_name))
        if (inherits(model, "gamlss")) {
          output[[model_id]] <- renderPlot({
            plot(model)  # Diagnostic plot for the model
          })
        }
      })
    }
  })
  
  output$modelOutput <- renderPrint({
    results <- runModels()
    if (is.list(results)) {
      lapply(results, function(res) if (inherits(res, "gamlss")) summary(res,type = "qr") else res) # uses "qr" by default
    } else {
      results
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
