library(shiny)
library(shinydashboard)
library(data.table)

ui <- dashboardPage(
  dashboardHeader(title = "Kinship Calculator"),
  dashboardSidebar(
    sidebarMenu(
      fileInput("file1", "Upload CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      sidebarSearchForm(textId = "searchbar", buttonId = "searchbtn", label = "Search..."),
      menuItem("Current state", tabName = "state", icon = icon("dashboard")),
      menuItem("Stats", tabName="stats", icon = icon("bar-chart")),
      menuItem("Help", tabName="help", icon = icon("question-circle"))
    )
  ),
  dashboardBody(
    mainPanel(
      tableOutput("contents")
    )
  )
)

server <- function(input, output) {
  data <- reactive({
    file <- input$file1
    if(is.null(file)){return()} 
    read.csv(file$datapath)
  })
  
  filteredData <- reactive({
    data <- data()
    if(is.null(data)){return()}
    data[grep(input$searchbar, data$column_to_search),]
  })
  
  output$contents <- renderTable({
    data <- filteredData()
    if(is.null(data)){return()}
    data
  })
}

shinyApp(ui = ui, server = server)