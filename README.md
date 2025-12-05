# gamlss.shiny

*** Warning! This Shiny App has not been validated enough! There is no warranty for the app! ***

https://puneet-talwar.shinyapps.io/GAMLSSToolbox/

The current version of GAMLSS shiny toolbox can be used to
          
          - fit basic GAMLSS models
          - identify fit distribution family for the dependent variables
          - run analysis for a multiple dependent and independent variables simultaneously
          
          Usage: 
          
          - Data input format: xlsx or csv file with header row containing variable names.
          - By default first sheet will be used as the input. Ex. mtcars, sleepstudy (lme4)"
          - Missing values are either blank/empty cells in the data
          
          - For outlier removal specify the standard deviation value (ex. 3)
          
          - Multiple covariates can be selected from the input data (age sex bmi)
          
          - Run the first model with Normal (NO) distribution and check the fit distribution family recommendation output by gamlss. Multiple fit families can be selected for good-of-fit comparison.
          
          - For family of distributions Refer- https://www.gamlss.com/wp-content/uploads/2023/06/gamlssreferencecard.pdf
          
          - Custom equation format : y ~ x1 + x2
          
          Note:
          
          - The summary output can use either 'vcov' or 'qr' for summary type; 'vcov' is the default.
          
          For using splines in a GAMLSS model, custom equation option should be used.
          
          Currently, it does not support Random effects.
