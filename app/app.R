library(shiny)
library(DT)
library(tools)
library(readxl)
source("../analyses/A1cAnalysis.R")
source("../summaries/A1cSummary.R")

main_page <- fluidPage(
  titlePanel("Generate change in HgA1c reports"),
  sidebarLayout(
  	sidebarPanel(
	  fileInput("file_input", "Upload a CSV or XLS file of the lab results report on MDR", accept = c(".csv",".xls"))
	),
	mainPanel(
	  tabsetPanel(
        tabPanel(
          title = "Processed data",
          DT::dataTableOutput("data")
        ),
        tabPanel(
          title = "Summary statistics",
          tableOutput("stats")
        )
      )
    )
  )
)

ui <- fluidPage(
  title="Validate Reports",
  main_page
  
)

server <- function(input, output) {
  output$data <- DT::renderDataTable({
    file <- input$file_input
    ext <- tools::file_ext(file$datapath)

    req(file)
    validate(need(ext == "csv" || ext == "xls", "Please upload a csv or xls file"))

    if(tools::file_ext(file$datapath)=="csv"){
      csv <- read.csv(file$datapath,head=TRUE)
      dataframe <- csv
    } else if(tools::file_ext(file$datapath)=="xls"){
      xl <- data.frame(readxl::read_excel(file$datapath))
      dataframe <- xl
    }
    a1c_analysis(dataframe,tools::file_ext(file$datapath),0)
  
  })
  output$stats <- renderTable({
    file <- input$file_input
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext == "csv" || ext == "xls", "Please upload a csv or xls file"))
    if(tools::file_ext(file$datapath)=="csv"){
      csv <- read.csv(file$datapath,head=TRUE)
      dataframe <- csv
    } else if(tools::file_ext(file$datapath)=="xls"){
      xl <- data.frame(readxl::read_excel(file$datapath))
      dataframe <- xl
    }
    analysis <- a1c_analysis(dataframe,tools::file_ext(file$datapath),0)
    a1c_summary_stats(analysis)
  
  })
  count <- 0
  autoInvalidate <- reactiveTimer(30000)
  observe({
    if(count <= 10){
        autoInvalidate()
        cat(".")
        count <- count+1 
    }
  })
  
}

options(shiny.port=8080)

shinyApp(ui, server)

