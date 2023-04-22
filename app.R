library(shiny)
library(DT)
source("./analyses/A1cAnalysis.R")
source("./summaries/A1cSummary.R")

main_page <- fluidPage(
  titlePanel("Generate change in HgA1c reports"),
  sidebarLayout(
  	sidebarPanel(
	  fileInput("csv_input", "Upload a CSV file of the lab results report on MDR", accept = ".csv")
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
    file <- input$csv_input
    ext <- tools::file_ext(file$datapath)

    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))

    a1c_analysis(file$datapath,0)
  
  })
  output$stats <- renderTable({
    file <- input$csv_input
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))
    analysis <- a1c_analysis(file$datapath,0)
    a1c_summary_stats(analysis)
  
  })
  autoInvalidate <- reactiveTimer(50000)
  observe({
    autoInvalidate()
    cat(".")
  })
  
}

options(shiny.port=8080)

shinyApp(ui, server)
