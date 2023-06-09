library(shiny)
library(DT)
library(tools)
library(readxl)
source("./analyses/A1cAnalysis.R")
source("./summaries/A1cSummary.R")

main_page <- fluidPage(
  titlePanel("Generate change in HgA1c reports"),
  sidebarLayout(
  	sidebarPanel(
	  fileInput("file_input", "Upload a CSV or XLSX file of the lab results report on MDR", accept = c(".csv",".xlsx")),
    textInput("a1c_thres","Enter a minimum initial A1c to include in analysis",value="0"),
    textInput("a1c_final","Enter a final A1c value to include in analysis",value="7"),
    downloadButton('downloadData', 'Download data')
	),
	mainPanel(
	  tabsetPanel(
      tabPanel(
          title = "Summary statistics",
          tableOutput("stats")
        ),
        tabPanel(
          title = "Processed data",
          DT::dataTableOutput("data")
        )
      )
    )
  )
)

ui <- fluidPage(
  title="Validate Reports",
  main_page
  
)

get_data <-function(input,file){
  file <- input$file_input
  a1c_thres <- input$a1c_thres
  a1c_final <- input$a1c_final
  ext <- tools::file_ext(file$datapath)
  req(file)
  validate(need(ext == "csv" || ext == "xlsx", "Please upload a csv or xlsx file"))
  if(tools::file_ext(file$datapath)=="csv"){
    csv <- read.csv(file$datapath,head=TRUE)
    dataframe <- csv
  } else if(tools::file_ext(file$datapath)=="xlsx"){
    xl <- data.frame(readxl::read_excel(file$datapath))
    dataframe <- xl
  }
  data <- a1c_analysis(dataframe,ext,a1c_thres,a1c_final)
  return(data)

}

server <- function(input, output) {
  output$data <- DT::renderDataTable({
      get_data(input,file)
  })
  
  output$stats <- renderTable({
    analysis <- get_data(input,file)  
    a1c_summary_stats(analysis,input$a1c_final)
  })
  autoInvalidate <- reactiveTimer(50000)

  observe({
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("HgA1c-data-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(get_data(input,file), file)
      }
    )
  })

  observe({
    autoInvalidate()
    cat(".")
  })
  
}

shinyApp(ui, server)
