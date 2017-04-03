shinyUI(pageWithSidebar(
  headerPanel(fileInput('file1', 'File input',
                        accept=c('text/csv', 'text/comma-separated-values,text/plain')),
              tags$hr()),
  sidebarPanel(
    uiOutput('Xcolumns'),
    uiOutput('Ycolumns'),
    checkboxGroupInput("checkGroup", label = h3("Checkbox group"), 
                       choices = list("Linear Model" = 1, "Quadratic Model" = 2),
                       selected = 0 ),
    
    hr(),
    fluidRow(column(2, verbatimTextOutput("value")))
  ),
  mainPanel(
    plotOutput("dataPlot")
  )
))

