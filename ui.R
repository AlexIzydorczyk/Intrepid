# ui.R
shinyUI(pageWithSidebar(
  headerPanel("Data"),
  sidebarPanel(
    textInput("dataset", "Enter Quandl Code For What to Predict: "),
    #selectInput("names", "Choose a factor:", 
    #           choices = c(names(dataset))),
    uiOutput("column_namesY"),
    numericInput("numberOfX", "Number of Predictors", 5),
    uiOutput("x1"),
    uiOutput("column_namesX1"),
    uiOutput("x2"),
    uiOutput("column_namesX2"),
    uiOutput("x3"),
    uiOutput("column_namesX3"),
    uiOutput("x4"),
    uiOutput("column_namesX4"),
    uiOutput("x5"),
    uiOutput("column_namesX5"),
    checkboxInput("ready", "Check when ready", FALSE)

  ),
  mainPanel(
#     tabsetPanel(
#       tabPanel("Plot", htmlOutput("graph"),
#                htmlOutput("view"),
#                tableOutput("table")),
#       tabPanel("Summary", verbatimTextOutput("summary")), 
#       tabPanel("Table", tableOutput("table"))
#     )
    
    tabsetPanel(
      ########################### SEARCH ###############################
      tabPanel("Explore",
               textInput("dataquery","Search Database:"),
               #numericInput("numberOfsearch", "Number of Search Results (Max. 10)", 5, min=1, max=10),
               verbatimTextOutput("search1"),
               verbatimTextOutput("search2"),
               verbatimTextOutput("search3"),
               verbatimTextOutput("search4"),
               verbatimTextOutput("search5"),
               verbatimTextOutput("search6"),
               verbatimTextOutput("search7"),
               verbatimTextOutput("search8"),
               verbatimTextOutput("search9"),
               verbatimTextOutput("search10")
      ),
      ########################### SEARCH ###############################
      tabPanel("Data",
    htmlOutput("graph"),
    htmlOutput("view"),
    tableOutput("table")
               ),
      tabPanel("Preprocess",
    uiOutput("LagsY"),
    uiOutput("LagsX1"),
    uiOutput("LagsX2"),
    uiOutput("LagsX3"),
    uiOutput("LagsX4"),
    uiOutput("LagsX5"),
    uiOutput("cX1"),
    uiOutput("cX2"),
    uiOutput("cX3"),
    uiOutput("cX4"),
    uiOutput("cX5"),
               
    tableOutput("table_preprocess")
               
               ),
      tabPanel("Run Model",
    selectInput("choose_model", "Choose a model:", list("Linear Regression" = "lm", 
                                                        "Random Forest" = "rf", 
                                                        "Support Vector Machine" = "svm")),
               numericInput("splitperc", "Percent to Use for Training", 70),
               actionButton('action', 'Run Model')
               
               ),
      tabPanel("Results",
               
               htmlOutput("results_graph"),
               verbatimTextOutput("results_summary"),
               tableOutput("table_predictions"),
               downloadButton('downloadData', 'Download')
               
               
               )
               
               
      
      )

    
  )
))