library(shiny)
library(DT)
library(tools)
library(readxl)
library(shiny.router)
source("./analyses/A1cAnalysis.R")
source("./summaries/A1cSummary.R")

main_page <- div(
  fluidPage(
    titlePanel("Select an analysis from the list above.")
  )
)

a1c_report <- div(
  fluidPage(
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
)

bp_report <- div(
  titlePanel("Second"),
  p("This is a second page")
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

menu <- tags$ul(
  tags$li(a(class="item", href = route_link("/"), "Select an analysis")),
  tags$li(a(class="item", href = route_link("a1c_report"), "HgA1c reports")),
  tags$li(a(class="item", href = route_link("bp_report"), "Blood pressure reports"))
)

ui <- fluidPage(
  menu,
  tags$hr(),
  router_ui(
    route("/", main_page),
    route("a1c_report", a1c_report),
    route("bp_report", bp_report)
  )
)

server <- function(input, output, session) {
  router_server()
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
