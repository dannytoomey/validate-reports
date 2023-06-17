library(shiny)
library(DT)
library(tools)
library(readxl)
library(shiny.router)
source("./analyses/A1cAnalysis.R")
source("./analyses/BPAnalysis.R")
source("./summaries/A1cSummary.R")
source("./summaries/BPSummary.R")

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
      fileInput("a1c_file_input", "Upload a CSV or XLSX file of the HgA1c lab results report on MDR", accept = c(".csv",".xlsx")),
      textInput("a1c_thres","Enter a minimum initial A1c to include in analysis",value="0"),
      textInput("a1c_final","Enter a final A1c value to include in analysis",value="7"),
      downloadButton('download_a1c_data', 'Download data')
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
            title = "Summary statistics",
            tableOutput("a1c_stats")
          ),
          tabPanel(
            title = "Processed data",
            DT::dataTableOutput("a1c_data")
          )
        )
      )
    )
  )
)

bp_report <- div(
fluidPage(
    titlePanel("Generate change in blood pressure reports"),
    sidebarLayout(
      sidebarPanel(
      fileInput("bp_file_input", "Upload a CSV or XLSX file of the blood pressure report on MDR", accept = c(".csv",".xlsx")),
      textInput("bp_thres","Enter a minimum initial systolic BP to include in analysis",value="100"),
      textInput("bp_final","Enter a final systolic BP to include in analysis",value="130"),
      downloadButton('download_bp_data', 'Download data')
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
            title = "Summary statistics",
            tableOutput("bp_stats")
          ),
          tabPanel(
            title = "Processed data",
            DT::dataTableOutput("bp_data")
          )
        )
      )
    )
  )
)

get_data <-function(input,file_input,page){
  a1c_thres <- input$a1c_thres
  a1c_final <- input$a1c_final    
  bp_thres <- input$bp_thres
  bp_final <- input$bp_final
  ext <- tools::file_ext(file_input$datapath)
  req(file_input)
  validate(need(ext == "csv" || ext == "xlsx", "Please upload a csv or xlsx file"))
  if(tools::file_ext(file_input$datapath)=="csv"){
    csv <- read.csv(file_input$datapath,head=TRUE)
    dataframe <- csv
  } else if(tools::file_ext(file_input$datapath)=="xlsx"){
    xl <- data.frame(readxl::read_excel(file_input$datapath))
    dataframe <- xl
  }
  if(page=="a1c_report"){
      data <- a1c_analysis(dataframe,ext,a1c_thres,a1c_final)
  }
  if(page=="bp_report"){
      data <- bp_analysis(dataframe,ext,bp_thres,bp_final)
  }
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

  observe({
    page <- shiny.router::get_page()
    if(page=="a1c_report"){
      file_input <- input$a1c_file_input
      output$a1c_data <- DT::renderDataTable({
        page <- shiny.router::get_page()
        get_data(input,file_input,page)
      })
      output$a1c_stats <- renderTable({
        analysis <- get_data(input,file_input,page)
        a1c_summary_stats(analysis,input$a1c_final)
      })
    }
    if(page=="bp_report"){
      file_input <- input$bp_file_input
      output$bp_data <- DT::renderDataTable({
        page <- shiny.router::get_page()
        get_data(input,file_input,page)
      })
      output$bp_stats <- renderTable({
        analysis <- get_data(input,file_input,page)
        bp_summary_stats(analysis,input$bp_final)
      })
    }
  })
  
  observe({
    page <- shiny.router::get_page()
    if(page=="a1c_report"){
      file_input <- input$a1c_file_input
      output$download_a1c_data <- downloadHandler(
        filename = function() {
          paste("HgA1c-data-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
          write.csv(get_data(input,file_input,page), file)
        }
      )
    }
    if(page=="bp_report"){
      file_input <- input$bp_file_input
      output$download_bp_data <- downloadHandler(
        filename = function() {
          paste("BP-data-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
          write.csv(get_data(input,file_input,page), file)
        }
      )
    }
  })

  autoInvalidate <- reactiveTimer(50000)
  observe({
    autoInvalidate()
    cat(".")
  })
  
}

shinyApp(ui, server)
