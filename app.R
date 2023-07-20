library(shiny)
library(DT)
library(tools)
library(readxl)
library(metafor)
library(meta)
library(esc)
library(grid)
source("./analyses/A1cAnalysis.R")
source("./analyses/BPAnalysis.R")
source("./analyses/HLDAnalysis.R")
source("./summaries/A1cSummary.R")
source("./summaries/BPSummary.R")
source("./summaries/HLDSummary.R")

a1c_report <- tabPanel(
  title="HgA1c Analysis",
  titlePanel("Generate change in HgA1c reports"),
  sidebarLayout(
    sidebarPanel(
    fileInput("a1c_file_input", "Upload a CSV or XLSX file of the HgA1c lab results report on MDR", accept = c(".csv",".xlsx")),
    textInput("a1c_thres","Enter a minimum initial A1c to include in analysis",value="5"),
    textInput("a1c_final","Enter a final A1c value to include in analysis",value="7"),
    selectInput("a1c_group_select", "Select a variable to use for plotting", choices = c("Age","Gender")),
    downloadButton('download_a1c_data', 'Download data')
  ),
  mainPanel(
    tabsetPanel(
      tabPanel(
          title = "Summary statistics",
          tableOutput("a1c_stats")
        ),
        tabPanel(
          title = "Plots",
          plotOutput("a1c_plot")
        ),
        tabPanel(
          title = "Processed data",
          DT::dataTableOutput("a1c_data")
        )
      )
    )
  )
)

bp_report <- tabPanel(
  title="Blood Pressure Analysis",
  titlePanel("Generate change in blood pressure reports"),
  sidebarLayout(
    sidebarPanel(
    fileInput("bp_file_input", "Upload a CSV or XLSX file of the blood pressure report on MDR", accept = c(".csv",".xlsx")),
    textInput("bp_thres","Enter a minimum initial systolic BP to include in analysis",value="100"),
    textInput("bp_final","Enter a final systolic BP to include in analysis",value="130"),
    selectInput("bp_group_select", "Select a variable to use for plotting", choices = c("Age")),
    downloadButton('download_bp_data', 'Download data')
  ),
  mainPanel(
    tabsetPanel(
      tabPanel(
          title = "Summary statistics",
          tableOutput("bp_stats")
        ),
        tabPanel(
          title = "Plots",
          plotOutput("bp_plot")
        ),
        tabPanel(
          title = "Processed data",
          DT::dataTableOutput("bp_data")
        )
      )
    )
  )
)

hld_report <- tabPanel(
  title = "LDL Analysis",
  titlePanel("Generate change in cholesterol reports"),
  sidebarLayout(
    sidebarPanel(
    fileInput("hld_file_input", "Upload a CSV or XLSX file of the LDL cholesterol report on MDR", accept = c(".csv",".xlsx")),
    textInput("hld_thres","Enter a minimum initial LDL level to include in analysis",value="75"),
    textInput("hld_final","Enter a final LDL level to include in analysis",value="125"),
    selectInput("hld_group_select", "Select a variable to use for plotting", choices = c("Age","Gender")),
    downloadButton('download_hld_data', 'Download data')
  ),
  mainPanel(
    tabsetPanel(
      tabPanel(
          title = "Summary statistics",
          tableOutput("hld_stats")
        ),
        tabPanel(
          title = "Plots",
          plotOutput("hld_plot")
        ),
        tabPanel(
          title = "Processed data",
          DT::dataTableOutput("hld_data")
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
  hld_thres <- input$hld_thres
  hld_final <- input$hld_final
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
  if(page=="HgA1c Analysis"){
      data <- a1c_analysis(dataframe,ext,a1c_thres,a1c_final)
  }
  if(page=="Blood Pressure Analysis"){
      data <- bp_analysis(dataframe,ext,bp_thres,bp_final)
  }
  if(page=="LDL Analysis"){
      data <- hld_analysis(dataframe,ext,hld_thres,hld_final)
  }
  return(data)
}

ui <- navbarPage(
  title = "Select an analysis:",
  id = "navbarid",
  a1c_report,
  bp_report,
  hld_report
)

get_odds_gender <- function(data,group){
  event_group = nrow(data[data$Gender==group & data$Final_result_below_threshold=='Yes',])
  non_event_group = nrow(data[data$Gender==group & data$Final_result_below_threshold!='Yes',])
  event_non_group = nrow(data[data$Gender!=group & data$Final_result_below_threshold=='Yes',])
  non_event_non_group = nrow(data[data$Gender!=group & data$Final_result_below_threshold!='Yes',])
  return(c(event_group,non_event_group,event_non_group,non_event_non_group))
}
get_odds_age <- function(data,group){
  event_group = nrow(data[data$age_quant_num==group & data$Final_result_below_threshold=='Yes',])
  non_event_group = nrow(data[data$age_quant_num==group & data$Final_result_below_threshold!='Yes',])
  event_non_group = nrow(data[data$age_quant_num!=group & data$Final_result_below_threshold=='Yes',])
  non_event_non_group = nrow(data[data$age_quant_num!=group & data$Final_result_below_threshold!='Yes',])
  return(c(event_group,non_event_group,event_non_group,non_event_non_group))
}
get_esc <- function(data,group,var){
  if(var=="gender"){
      res <- get_odds_gender(data,group)    
  }
  if(var=="age"){
      res <- get_odds_age(data,group)    
  }
  esc::esc_2x2(grp1yes = res[1], grp1no = res[2],
          grp2yes = res[3], grp2no = res[4],
          es.type = "logit",study=group)
}
do_meta_es <- function(combined_es,title){
  meta <- meta::metagen(TE = combined_es$es,
          seTE = combined_es$se,
          studlab = combined_es$study,
          data = combined_es,
          sm = "SMD",
          fixed = FALSE,
          random = TRUE,
          method.tau = "REML",
          hakn = TRUE,
          title = title)
  return(meta)
}

server <- function(input, output, session) {
  observe({
    page <- input$navbarid 
    if(page=="HgA1c Analysis"){
      file_input <- input$a1c_file_input
      output$a1c_data <- DT::renderDataTable({
        get_data(input,file_input,page)
      })
      output$a1c_stats <- renderTable({
        analysis <- get_data(input,file_input,page)
        a1c_summary_stats(analysis,input$a1c_final)
      })
      output$a1c_plot <- renderImage({
        outfile <- tempfile(fileext = '.png')
        png(outfile, 
          width = 5500, 
          height = 3500,
          res = 50*10)
        analysis <- get_data(input,file_input,page)
        esc_array <- list()
        i<-1
        if(input$a1c_group_select=="Gender"){
          for(group in unique(analysis$Gender)){
            esc_array[[i]] <- get_esc(analysis,group,"gender")
            i <- i+1
          }
          combined_es <- esc::combine_esc(esc_array[1],esc_array[2])
          meta <- do_meta_es(combined_es,"Gender and odds of improving HgA1c")
          meta::forest.meta(meta,
                            sortvar = TE,
                            print.tau2 = FALSE,
                            leftlabs = c("Gender", "g", "SE"),
                            fontsize=16
                           )
          grid::grid.text("Odds of reducing A1c by Gender", x=0.5,y=0.85, gp=gpar(fontsize=18))
        }
        if(input$a1c_group_select=="Age"){
          age_quants <- quantile(as.numeric(analysis$Birth_Year),probs=c(0,0.25,0.5,0.75,1))
          age_quant_num <- vector()
          for(chart in analysis$Chart_num){
            this_chart <- analysis[analysis$Chart_num==chart,]
            if(age_quants[[1]] <= as.numeric(this_chart$Birth_Year) & as.numeric(this_chart$Birth_Year) < age_quants[[2]]){
              x <- 1
            }
            if(age_quants[[2]] <= as.numeric(this_chart$Birth_Year) & as.numeric(this_chart$Birth_Year) < age_quants[[3]]){
              x <- 2
            }
            if(age_quants[[3]] <= as.numeric(this_chart$Birth_Year) & as.numeric(this_chart$Birth_Year) < age_quants[[4]]){
              x <- 3
            }
            if(age_quants[[4]] <= as.numeric(this_chart$Birth_Year) & as.numeric(this_chart$Birth_Year) < age_quants[[5]]){
              x <- 4
            }
            age_quant_num <- c(age_quant_num,paste0("Age quantile ",x," - ",age_quants[[x]], " to ",age_quants[[x+1]],""))
          }
          analysis$age_quant_num <- age_quant_num
          for(group in unique(analysis$age_quant_num)){
            esc_array[[i]] <- get_esc(analysis,group,"age")
            i <- i+1
          }
          combined_es <- esc::combine_esc(esc_array[1],esc_array[2],esc_array[3],esc_array[4])
          meta <- do_meta_es(combined_es,"Age and odds of improving HgA1c")
          meta::forest.meta(meta,
                            sortvar = TE,
                            print.tau2 = FALSE,
                            leftlabs = c("Age", "g", "SE"),
                            fontsize=16
                           )
          grid::grid.text("Odds of reducing A1c by Age", x=0.5,y=0.85, gp=gpar(fontsize=18))
        }
        dev.off()
        list(src = outfile,
             width=600,
             height=400)
      },deleteFile = TRUE)
    }
    if(page=="Blood Pressure Analysis"){
      file_input <- input$bp_file_input
      output$bp_data <- DT::renderDataTable({
        get_data(input,file_input,page)
      })
      output$bp_stats <- renderTable({
        analysis <- get_data(input,file_input,page)
        bp_summary_stats(analysis,input$bp_final)
      })
      output$bp_plot <- renderImage({
        outfile <- tempfile(fileext = '.png')
        png(outfile, 
          width = 5500, 
          height = 3500,
          res = 50*10)
        analysis <- get_data(input,file_input,page)
        esc_array <- list()
        i<-1
        if(input$bp_group_select=="Age"){
          age_quants <- quantile(as.numeric(analysis$Birth_Year),probs=c(0,0.25,0.5,0.75,1))
          age_quant_num <- vector()
          for(chart in analysis$Chart_num){
            this_chart <- analysis[analysis$Chart_num==chart,]
            if(age_quants[[1]] < as.numeric(this_chart$Birth_Year) & as.numeric(this_chart$Birth_Year) <= age_quants[[2]]){
              x <- 1
            }
            if(age_quants[[2]] < as.numeric(this_chart$Birth_Year) & as.numeric(this_chart$Birth_Year) <= age_quants[[3]]){
              x <- 2
            }
            if(age_quants[[3]] < as.numeric(this_chart$Birth_Year) & as.numeric(this_chart$Birth_Year) <= age_quants[[4]]){
              x <- 3
            }
            if(age_quants[[4]] < as.numeric(this_chart$Birth_Year) & as.numeric(this_chart$Birth_Year) <= age_quants[[5]]){
              x <- 4
            }
            age_quant_num <- c(age_quant_num,paste0("Age quantile ",x," - ",age_quants[[x]], " to ",age_quants[[x+1]],""))
          }
          analysis$age_quant_num <- age_quant_num
          for(group in unique(analysis$age_quant_num)){
            esc_array[[i]] <- get_esc(analysis,group,"age")
            i <- i+1
          }
          combined_es <- esc::combine_esc(esc_array[1],esc_array[2],esc_array[3],esc_array[4])
          meta <- do_meta_es(combined_es,"Age and odds of improving Blood Pressure")
          meta::forest.meta(meta,
                            sortvar = TE,
                            print.tau2 = FALSE,
                            leftlabs = c("Age", "g", "SE"),
                            fontsize=16
                           )
          grid::grid.text("Odds of reducing Blood Pressure by Age", x=0.5,y=0.85, gp=gpar(fontsize=18))
        }
        dev.off()
        list(src = outfile,
             width=600,
             height=400)
      },deleteFile = TRUE)
    }
    if(page=="LDL Analysis"){
      file_input <- input$hld_file_input
      output$hld_data <- DT::renderDataTable({
        get_data(input,file_input,page)
      })
      output$hld_stats <- renderTable({
        analysis <- get_data(input,file_input,page)
        hld_summary_stats(analysis,input$hld_final)
      })
      output$hld_plot <- renderImage({
        outfile <- tempfile(fileext = '.png')
        png(outfile, 
          width = 5500, 
          height = 3500,
          res = 50*10)
        analysis <- get_data(input,file_input,page)
        esc_array <- list()
        i<-1
        if(input$hld_group_select=="Gender"){
          for(group in unique(analysis$Gender)){
            esc_array[[i]] <- get_esc(analysis,group,"gender")
            i <- i+1
          }
          combined_es <- esc::combine_esc(esc_array[1],esc_array[2])
          meta <- do_meta_es(combined_es,"Gender and odds of improving LDL cholesterol")
          meta::forest.meta(meta,
                            sortvar = TE,
                            print.tau2 = FALSE,
                            leftlabs = c("Gender", "g", "SE"),
                            fontsize=16
                           )
          grid::grid.text("Odds of reducing LDL cholesterol by Gender", x=0.5,y=0.85, gp=gpar(fontsize=18))
        }
        if(input$hld_group_select=="Age"){
          age_quants <- quantile(as.numeric(analysis$Birth_Year),probs=c(0,0.25,0.5,0.75,1))
          age_quant_num <- vector()
          for(chart in analysis$Chart_num){
            this_chart <- analysis[analysis$Chart_num==chart,]
            if(age_quants[[1]] < as.numeric(this_chart$Birth_Year) & as.numeric(this_chart$Birth_Year) <= age_quants[[2]]){
              x <- 1
            }
            if(age_quants[[2]] < as.numeric(this_chart$Birth_Year) & as.numeric(this_chart$Birth_Year) <= age_quants[[3]]){
              x <- 2
            }
            if(age_quants[[3]] < as.numeric(this_chart$Birth_Year) & as.numeric(this_chart$Birth_Year) <= age_quants[[4]]){
              x <- 3
            }
            if(age_quants[[4]] < as.numeric(this_chart$Birth_Year) & as.numeric(this_chart$Birth_Year) <= age_quants[[5]]){
              x <- 4
            }
            age_quant_num <- c(age_quant_num,paste0("Age quantile ",x," - ",age_quants[[x]], " to ",age_quants[[x+1]],""))
          }
          analysis$age_quant_num <- age_quant_num
          for(group in unique(analysis$age_quant_num)){
            esc_array[[i]] <- get_esc(analysis,group,"age")
            i <- i+1
          }
          combined_es <- esc::combine_esc(esc_array[1],esc_array[2],esc_array[3],esc_array[4])
          meta <- do_meta_es(combined_es,"Age and odds of improving LDL cholesterol")
          meta::forest.meta(meta,
                            sortvar = TE,
                            print.tau2 = FALSE,
                            leftlabs = c("Age", "g", "SE"),
                            fontsize=16
                           )
          grid::grid.text("Odds of reducing LDL cholesterol by Age", x=0.5,y=0.85, gp=gpar(fontsize=18))
        }
        dev.off()
        list(src = outfile,
             width=600,
             height=400)
      },deleteFile = TRUE)
    }
  })
  
  observe({
    page <- input$navbarid 
    if(page=="HgA1c Analysis"){
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
    if(page=="Blood Pressure Analysis"){
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
    if(page=="LDL Analysis"){
      file_input <- input$hld_file_input
      output$download_hld_data <- downloadHandler(
        filename = function() {
          paste("HLD-data-", Sys.Date(), ".csv", sep="")
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
