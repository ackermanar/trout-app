library(shiny)

ui <- fluidPage(
  
  # App title
  titlePanel("Kinship Calculator"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(
      
      # Input: Selector for choosing file to upload
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Input: Text input for search bar
      textInput("search", "Search:")
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      
      # Output: Data file summary
      tableOutput("contents")
    )
  )
)

server <- function(input, output) {
  
  # Load the uploaded file
  data <- reactive({
    file <- input$file1
    if(is.null(file)){return()} 
    read.csv(file$datapath)
  })
  
  # Filter the data based on the search input
  filteredData <- reactive({
    data <- data()
    if(is.null(data)){return()}
    data[grep(input$search, data$column_to_search),]
  })
  
  # Display the filtered data
  output$contents <- renderTable({
    data <- filteredData()
    if(is.null(data)){return()}
    data
  })
}

# Run the application 
shinyApp(ui = ui, server = server)